use common::{FmtChar, Pos, Span};

use crate::datetime::{Date, DateTime, DateTimeField, DateTimeField::*, Offset, Time};
use crate::lex::CharIter;
use crate::parse::{DateTimeVal, PartialValue};
use crate::Error;

// Continue parsing a date-time after the first two digits. These digits could either be part of
// the year in case of a date, or the hour in case of a time.
pub fn continue_parsing_date_time(
    chars: &mut CharIter,
    span: Span,
    two_digits: u16,
) -> Result<PartialValue, Error> {
    let y2 = match chars.next() {
        Some((_, c @ ('0'..='9'))) => c as u16 - '0' as u16,
        Some((_, ':')) => {
            let hour = two_digits as u8;
            let time = continue_parsing_local_time(chars, span, hour)?;
            return Ok(PartialValue::PartialTime(time));
        }
        Some((i, c)) => return invalid_char_error(c, span, i),
        None => return Err(Error::DateTimeIncomplete(Year, span.end)),
    };

    let y3 = match chars.next() {
        Some((_, c @ ('0'..='9'))) => c as u16 - '0' as u16,
        Some((i, c)) => return invalid_char_error(c, span, i),
        None => return Err(Error::DateTimeIncomplete(Year, span.end)),
    };
    let year = 100 * two_digits + 10 * y2 + y3;

    expect_char(chars, span, DateTimeField::Year, '-')?;

    continue_parsing_date_time_after_year(chars, span, year)
}

/// Continue parsing this date-time after the `-` separator following the year.
pub fn continue_parsing_date_time_after_year(
    chars: &mut CharIter,
    span: Span,
    year: u16,
) -> Result<PartialValue, Error> {
    let (month, _) = expect_two_digit_num(chars, span)
        .map_err(|e| e.kind(Month))?
        .check_range(0..=12)
        .map_err(|e| e.kind(Month))?;

    expect_char(chars, span, DateTimeField::Month, '-')?;

    let (day, _) = expect_two_digit_num(chars, span)
        .map_err(|e| e.kind(Day))?
        .check_range(0..=31)
        .map_err(|e| e.kind(Day))?;

    let date = Date { year, month, day };

    let (time, offset) = match chars.next() {
        Some((_, 'T')) => parse_time_and_offset(chars, span)?,
        Some((i, c)) => return invalid_char_error(c, span, i),
        None => return Ok(PartialValue::PartialDate(date)),
    };

    match offset {
        Some(offset) => {
            let val = DateTime::OffsetDateTime(date, time, offset);
            Ok(PartialValue::OffsetDateTime(val))
        }
        None => Ok(PartialValue::PartialDateTime(date, time)),
    }
}

/// Continue to parse local time *without* offset (finding one will result in an error), after the
/// `:` separtor following the hour.
pub fn continue_parsing_local_time(
    chars: &mut CharIter,
    span: Span,
    hour: u8,
) -> Result<Time, Error> {
    let hour_span = Span::from_pos_len(span.start, 2);
    (hour, hour_span)
        .check_range(0..=23)
        .map_err(|e| e.kind(Hour))?;

    let time = continue_parsing_time(chars, span, hour)?;
    error_on_offset(chars, span)?;
    Ok(time)
}

pub fn parse_time_and_offset(
    chars: &mut CharIter,
    span: Span,
) -> Result<(Time, Option<Offset>), Error> {
    let time = {
        let (hour, _) = expect_two_digit_num(chars, span)
            .map_err(|e| e.kind(Hour))?
            .check_range(0..=23)
            .map_err(|e| e.kind(Hour))?;

        expect_char(chars, span, DateTimeField::Hour, ':')?;

        continue_parsing_time(chars, span, hour)?
    };

    let offset = try_to_parse_offset(chars, span)?;

    Ok((time, offset))
}

/// NOTE: This intentionally doesn't parse offsets, and quietly returns *just* the time if one
/// is encountered. The caller is expected to check and handle any remaining offsets.
fn continue_parsing_time(chars: &mut CharIter, span: Span, hour: u8) -> Result<Time, Error> {
    let (minute, _) = expect_two_digit_num(chars, span)
        .map_err(|e| e.kind(Minute))?
        .check_range(0..=59)
        .map_err(|e| e.kind(Minute))?;

    expect_char(chars, span, DateTimeField::Minute, ':')?;

    let (second, _) = expect_two_digit_num(chars, span)
        .map_err(|e| e.kind(Second))?
        .check_range(0..=59)
        .map_err(|e| e.kind(Second))?;

    match chars.peek() {
        // ignore offset
        Some((_, 'Z' | '+' | '-')) => (),
        Some(&(i, c)) => return invalid_char_error(c, span, i),
        None => (),
    }

    Ok(Time {
        hour,
        minute,
        second,
        nanos: 0,
    })
}

pub fn parse_subsec_part<'a>(
    lit: &'a str,
    span: Span,
    subsec_lit: &str,
    subsec_span: Span,
    date: Option<Date>,
    mut time: Time,
) -> Result<DateTimeVal<'a>, Error> {
    let mut chars = subsec_lit.char_indices().peekable();
    let val = match date {
        Some(date) => {
            let (nanos, offset) = parse_subsec_and_offset(&mut chars, subsec_span)?;
            time.nanos = nanos;
            DateTime::from_optional_offset(date, time, offset)
        }
        None => {
            let nanos = parse_subsec_without_offset(&mut chars, subsec_span)?;
            time.nanos = nanos;
            DateTime::LocalTime(time)
        }
    };

    Ok(DateTimeVal::new(lit, span, val))
}

/// Parse the subsecond part up to nano seconds, truncating the rest, and an optional offset.
fn parse_subsec_and_offset(
    chars: &mut CharIter,
    span: Span,
) -> Result<(u32, Option<Offset>), Error> {
    let nanos = parse_subsec(chars, span)?;
    let offset = try_to_parse_offset(chars, span)?;
    Ok((nanos, offset))
}

/// Parse the subsecond part up to nano seconds, truncating the rest, error on finding an offset.
fn parse_subsec_without_offset(chars: &mut CharIter, span: Span) -> Result<u32, Error> {
    let nanos = parse_subsec(chars, span)?;
    error_on_offset(chars, span)?;
    Ok(nanos)
}

fn parse_subsec(chars: &mut CharIter, span: Span) -> Result<u32, Error> {
    let mut subsec_digits = 0;
    let mut subsec = 0;
    while let Some(&(i, c)) = chars.peek() {
        match c {
            '0'..='9' => {
                if subsec_digits < 9 {
                    subsec *= 10;
                    subsec += c as u32 - '0' as u32;
                    subsec_digits += 1;
                }
                chars.next();
            }
            'Z' | '+' | '-' => break,
            _ => return invalid_char_error(c, span, i),
        }
    }

    if subsec_digits == 0 {
        return Err(Error::DateTimeMissingSubsec(span.end));
    }

    let nanos = subsec * 10_u32.pow(9 - subsec_digits);
    Ok(nanos)
}

fn try_to_parse_offset(chars: &mut CharIter, span: Span) -> Result<Option<Offset>, Error> {
    match chars.next() {
        Some((_, 'Z')) => Ok(Some(Offset::Utc)),
        Some((_, '+')) => {
            let minutes = parse_offset(chars, span)?;
            Ok(Some(Offset::Custom(minutes)))
        }
        Some((_, '-')) => {
            let minutes = parse_offset(chars, span)?;
            Ok(Some(Offset::Custom(-minutes)))
        }
        Some((i, c)) => invalid_char_error(c, span, i),
        None => Ok(None),
    }
}

fn parse_offset(chars: &mut CharIter, span: Span) -> Result<i16, Error> {
    let (hour, _) = expect_two_digit_num(chars, span)
        .map_err(|e| e.kind(OffsetHour))?
        .check_range(0..=23)
        .map_err(|e| e.kind(OffsetHour))?;

    expect_char(chars, span, DateTimeField::OffsetHour, ':')?;

    let (minute, _) = expect_two_digit_num(chars, span)
        .map_err(|e| e.kind(OffsetMinute))?
        .check_range(0..=59)
        .map_err(|e| e.kind(OffsetMinute))?;

    Ok(60 * hour as i16 + minute as i16)
}

fn error_on_offset(chars: &mut CharIter, span: Span) -> Result<(), Error> {
    match chars.next() {
        Some((i, 'Z' | '+' | '-')) => {
            let pos = span.start.plus(i as u32);
            Err(Error::LocalDateTimeOffset(pos))
        }
        Some((i, c)) => invalid_char_error(c, span, i),
        None => Ok(()),
    }
}

fn invalid_char_error<T>(char: char, span: Span, offset: usize) -> Result<T, Error> {
    let pos = span.start.plus(offset as u32);
    Err(Error::InvalidCharInDateTime(FmtChar(char), pos))
}

fn expect_char(
    chars: &mut CharIter,
    span: Span,
    after: DateTimeField,
    expected: char,
) -> Result<(), Error> {
    match chars.next() {
        Some((_, c)) if c == expected => Ok(()),
        Some((i, c)) => {
            let pos = span.start.plus(i as u32);
            Err(Error::DateTimeExpectedCharFound {
                after,
                expected: FmtChar(expected),
                found: FmtChar(c),
                pos,
            })
        }
        None => Err(Error::DateTimeMissingChar(
            after,
            FmtChar(expected),
            span.end,
        )),
    }
}

struct ExpectNumError(ExpectNumErrorKind, Pos);
enum ExpectNumErrorKind {
    Incomplete,
    Missing,
    Invalid(char),
}

impl ExpectNumError {
    fn kind(self, field: DateTimeField) -> Error {
        match self.0 {
            ExpectNumErrorKind::Incomplete => Error::DateTimeIncomplete(field, self.1),
            ExpectNumErrorKind::Missing => Error::DateTimeMissing(field, self.1),
            ExpectNumErrorKind::Invalid(c) => Error::InvalidCharInDateTime(FmtChar(c), self.1),
        }
    }
}

fn expect_two_digit_num(
    chars: &mut impl Iterator<Item = (usize, char)>,
    span: Span,
) -> Result<(u8, Span), ExpectNumError> {
    use ExpectNumErrorKind::*;

    let Some((start_offset, c)) = chars.next() else {
        let pos = span.end;
        return Err(ExpectNumError(Missing, pos));
    };
    let d0 = match c {
        '0'..='9' => c as u8 - b'0',
        _ => {
            let pos = span.start.plus(start_offset as u32);
            return Err(ExpectNumError(Invalid(c), pos));
        }
    };

    let Some((_, c)) = chars.next() else {
        let pos = span.end;
        return Err(ExpectNumError(Incomplete, pos));
    };
    let d1 = match c {
        '0'..='9' => c as u8 - b'0',
        _ => {
            let pos = span.start.plus(start_offset as u32);
            return Err(ExpectNumError(Invalid(c), pos));
        }
    };

    let start = span.start.plus(start_offset as u32);
    let end = start.plus(2);
    let span = Span { start, end };
    Ok((10 * d0 + d1, span))
}

struct NumRangeError<T>(T, Span);

impl NumRangeError<u8> {
    fn kind(self, field: DateTimeField) -> Error {
        Error::DateTimeOutOfBounds(field, self.0, self.1)
    }
}

trait NumRangeCheck<T: PartialOrd<T>>: Sized {
    fn check_range(self, num_range: std::ops::RangeInclusive<T>) -> Result<Self, NumRangeError<T>>;
}

impl NumRangeCheck<u8> for (u8, Span) {
    fn check_range(
        self,
        num_range: std::ops::RangeInclusive<u8>,
    ) -> Result<Self, NumRangeError<u8>> {
        if !num_range.contains(&self.0) {
            return Err(NumRangeError(self.0, self.1));
        }
        Ok(self)
    }
}
