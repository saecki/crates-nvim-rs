use common::{FmtChar, Pos, Span};

use crate::datetime::{Date, DateTime, DateTimeField, DateTimeField::*, Offset, Time};
use crate::lex::CharIter;
use crate::parse::PartialValue;
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
            return Ok(PartialValue::DateTime(DateTime::LocalTime(time)));
        }
        Some((i, c)) => {
            let pos = span.start.plus(i as u32);
            return match c {
                '.' | '_' => Err(Error::InvalidLeadingZero(span.start)),
                '-' => Err(Error::DateTimeIncomplete(Year, pos)),
                _ => Err(Error::UnexpectedCharInDateTime(FmtChar(c), pos)),
            };
        }
        None => return Err(Error::DateTimeIncomplete(Year, span.end)),
    };

    let y3 = match chars.next() {
        Some((_, c @ ('0'..='9'))) => c as u16 - '0' as u16,
        Some((i, c)) => {
            let pos = span.start.plus(i as u32);
            return match c {
                '.' | '_' => Err(Error::InvalidLeadingZero(span.start)),
                '-' => Err(Error::DateTimeIncomplete(Year, pos)),
                _ => Err(Error::UnexpectedCharInDateTime(FmtChar(c), pos)),
            };
        }
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
    let (month, _) = expect_two_digit_num(chars, ['-'], span)
        .map_err(|e| e.kind(Month))?
        .check_range(1..=12)
        .map_err(|e| e.kind(Month))?;

    expect_char(chars, span, DateTimeField::Month, '-')?;

    let is_leap_year = (year % 4 == 0) && ((year % 100 != 0) || (year % 400 == 0));
    let max_day = match month {
        2 if is_leap_year => 29,
        2 => 28,
        4 | 6 | 9 | 11 => 30,
        _ => 31,
    };
    let (day, _) = expect_two_digit_num(chars, ['T', 't'], span)
        .map_err(|e| e.kind(Day))?
        .check_range(1..=max_day)
        .map_err(|e| e.kind(Day))?;

    let date = Date { year, month, day };

    let (time, offset) = match chars.next() {
        Some((_, 'T' | 't')) => parse_time_and_offset(chars, span)?,
        Some((i, c)) => {
            let pos = span.start.plus(i as u32);
            return match c {
                '0'..='9' => Err(Error::DateTimeMissingChar(Day, FmtChar('T'), pos)),
                _ => Err(Error::UnexpectedCharInDateTime(FmtChar(c), pos)),
            };
        }
        None => return Ok(PartialValue::PartialDate(date)),
    };

    let val = DateTime::from_optional_offset(date, time, offset);
    Ok(PartialValue::DateTime(val))
}

/// Continue to parse local time *without* offset (finding one will result in an error), after the
/// `:` separator following the hour.
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
        let (hour, _) = expect_two_digit_num(chars, [':'], span)
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
    let (minute, _) = expect_two_digit_num(chars, [':'], span)
        .map_err(|e| e.kind(Minute))?
        .check_range(0..=59)
        .map_err(|e| e.kind(Minute))?;

    expect_char(chars, span, DateTimeField::Minute, ':')?;

    let (second, _) = expect_two_digit_num(chars, ['Z', 'z', '+', '-'], span)
        .map_err(|e| e.kind(Second))?
        .check_range(0..=59)
        .map_err(|e| e.kind(Second))?;

    let mut nanos = 0;
    if let Some((_, '.')) = chars.peek() {
        chars.next();
        nanos = parse_subsec(chars, span)?;
    }

    match chars.peek() {
        // ignore offset
        Some((_, 'Z' | 'z' | '+' | '-')) => (),
        Some(&(i, c)) => return unexpected_char_error(c, span, i),
        None => (),
    }

    Ok(Time {
        hour,
        minute,
        second,
        nanos,
    })
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
            'Z' | 'z' | '+' | '-' => break,
            _ => return unexpected_char_error(c, span, i),
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
        Some((_, 'Z' | 'z')) => Ok(Some(Offset::Utc)),
        Some((_, '+')) => {
            let minutes = parse_offset(chars, span)?;
            Ok(Some(Offset::Custom(minutes)))
        }
        Some((_, '-')) => {
            let minutes = parse_offset(chars, span)?;
            Ok(Some(Offset::Custom(-minutes)))
        }
        Some((i, c)) => unexpected_char_error(c, span, i),
        None => Ok(None),
    }
}

fn parse_offset(chars: &mut CharIter, span: Span) -> Result<i16, Error> {
    let (hour, _) = expect_two_digit_num(chars, [':'], span)
        .map_err(|e| e.kind(OffsetHour))?
        .check_range(0..=23)
        .map_err(|e| e.kind(OffsetHour))?;

    expect_char(chars, span, DateTimeField::OffsetHour, ':')?;

    let (minute, _) = expect_two_digit_num(chars, [], span)
        .map_err(|e| e.kind(OffsetMinute))?
        .check_range(0..=59)
        .map_err(|e| e.kind(OffsetMinute))?;

    Ok(60 * hour as i16 + minute as i16)
}

fn error_on_offset(chars: &mut CharIter, span: Span) -> Result<(), Error> {
    match chars.next() {
        Some((i, c)) => {
            let pos = span.start.plus(i as u32);
            return match c {
                'Z' | 'z' | '+' | '-' => Err(Error::LocalDateTimeOffset(pos)),
                _ => Err(Error::UnexpectedCharInDateTime(FmtChar(c), pos)),
            };
        }
        None => Ok(()),
    }
}

fn unexpected_char_error<T>(char: char, span: Span, offset: usize) -> Result<T, Error> {
    let pos = span.start.plus(offset as u32);
    Err(Error::UnexpectedCharInDateTime(FmtChar(char), pos))
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
            ExpectNumErrorKind::Invalid(c) => Error::UnexpectedCharInDateTime(FmtChar(c), self.1),
        }
    }
}

fn expect_two_digit_num<const SIZE: usize>(
    chars: &mut impl Iterator<Item = (usize, char)>,
    next: [char; SIZE],
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
            if next.contains(&c) {
                return Err(ExpectNumError(Missing, pos));
            } else {
                return Err(ExpectNumError(Invalid(c), pos));
            }
        }
    };

    let Some((i, c)) = chars.next() else {
        let pos = span.end;
        return Err(ExpectNumError(Incomplete, pos));
    };
    let d1 = match c {
        '0'..='9' => c as u8 - b'0',
        _ => {
            let pos = span.start.plus(i as u32);
            return if next.contains(&c) {
                Err(ExpectNumError(Incomplete, pos))
            } else {
                Err(ExpectNumError(Invalid(c), pos))
            };
        }
    };

    let start = span.start.plus(start_offset as u32);
    let end = start.plus(2);
    let span = Span { start, end };
    Ok((10 * d0 + d1, span))
}

struct NumRangeError<T>(T, std::ops::RangeInclusive<T>, Span);

impl NumRangeError<u8> {
    fn kind(self, field: DateTimeField) -> Error {
        Error::DateTimeOutOfBounds(field, self.0, (*self.1.start(), *self.1.end()), self.2)
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
            return Err(NumRangeError(self.0, num_range, self.1));
        }
        Ok(self)
    }
}
