use crate::toml::parse::PartialValue;
use crate::toml::{DateTimeField, DateTimeField::*, Error, Pos, Range};

type CharIter<'a> = std::iter::Peekable<std::str::CharIndices<'a>>;

/// All variants allowed by the [toml spec](https://toml.io/en/v1.0.0#offset-date-time).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DateTime {
    OffsetDateTime(Date, Time, Offset),
    LocalDateTime(Date, Time),
    LocalDate(Date),
    LocalTime(Time),
}

impl DateTime {
    pub fn from_optional_offset(date: Date, time: Time, offset: Option<Offset>) -> Self {
        match offset {
            Some(o) => Self::OffsetDateTime(date, time, o),
            None => Self::LocalDateTime(date, time),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Date {
    pub year: u16,
    pub month: u8,
    pub day: u8,
}

impl Date {
    pub fn new(year: u16, month: u8, day: u8) -> Self {
        Self { year, month, day }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Time {
    pub hour: u8,
    pub minute: u8,
    pub second: u8,
    pub nanos: u32,
}

impl Time {
    pub fn new(hour: u8, minute: u8, second: u8, nanos: u32) -> Self {
        Self {
            hour,
            minute,
            second,
            nanos,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Offset {
    /// Z
    Utc,
    /// Minutes
    Custom(i16),
}

pub fn continue_parsing_date_time(
    mut chars: &mut CharIter,
    range: Range,
    two_digits: u16,
) -> Result<PartialValue, Error> {
    let y2 = match chars.next() {
        Some((_, c @ ('0'..='9'))) => c as u16 - '0' as u16,
        Some((_, ':')) => {
            let hour = two_digits as u8;
            let hour_range = Range {
                start: range.start,
                end: range.start.plus(2),
            };
            (hour, hour_range)
                .check_range(0..=23)
                .map_err(|e| e.kind(Hour))?;

            let time = continue_parsing_local_time(&mut chars, range, hour)?;
            return Ok(PartialValue::PartialTime(time));
        }
        Some((i, c)) => return invalid_char_error(c, range, i),
        None => return Err(Error::DateTimeIncomplete(Year, range.end)),
    };

    let y3 = match chars.next() {
        Some((_, c @ ('0'..='9'))) => c as u16 - '0' as u16,
        Some((i, c)) => return invalid_char_error(c, range, i),
        None => return Err(Error::DateTimeIncomplete(Year, range.end)),
    };
    let year = 100 * two_digits + 10 * y2 + y3;

    expect_char(chars, range, '-')?;

    continue_parsing_date_time_after_year(chars, range, year)
}

pub fn continue_parsing_date_time_after_year(
    mut chars: &mut CharIter,
    range: Range,
    year: u16,
) -> Result<PartialValue, Error> {
    let (month, _) = expect_two_digit_num(&mut chars, range)
        .map_err(|e| e.kind(Month))?
        .check_range(0..=12)
        .map_err(|e| e.kind(Month))?;

    expect_char(chars, range, '-')?;

    let (day, _) = expect_two_digit_num(&mut chars, range)
        .map_err(|e| e.kind(Day))?
        .check_range(0..=31)
        .map_err(|e| e.kind(Day))?;

    let date = Date { year, month, day };

    let (time, offset) = match chars.next() {
        Some((_, 'T')) => parse_time_and_offset(&mut chars, range)?,
        Some((i, c)) => return invalid_char_error(c, range, i),
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

/// Parse local time *without* offset, finding one will result in an error.
pub fn continue_parsing_local_time(
    chars: &mut CharIter,
    range: Range,
    hour: u8,
) -> Result<Time, Error> {
    let time = continue_parsing_time(chars, range, hour)?;
    error_on_offset(chars, range)?;
    Ok(time)
}

pub fn parse_time_and_offset(
    chars: &mut CharIter,
    range: Range,
) -> Result<(Time, Option<Offset>), Error> {
    let time = {
        let (hour, _) = expect_two_digit_num(chars, range)
            .map_err(|e| e.kind(Hour))?
            .check_range(0..=23)
            .map_err(|e| e.kind(Hour))?;

        expect_char(chars, range, ':')?;

        continue_parsing_time(chars, range, hour)?
    };

    let offset = match chars.next() {
        Some((_, 'Z')) => Some(Offset::Utc),
        Some((_, '+')) => {
            let minutes = parse_offset(chars, range)?;
            Some(Offset::Custom(minutes))
        }
        Some((_, '-')) => {
            let minutes = parse_offset(chars, range)?;
            Some(Offset::Custom(-minutes))
        }
        Some((i, c)) => return invalid_char_error(c, range, i),
        None => None,
    };

    Ok((time, offset))
}

/// NOTE: This intentionally doesn't parse offsets, and quietly returns *just* the time if one
/// is encountered. The caller is expected to check and handle any remaining offsets.
fn continue_parsing_time(chars: &mut CharIter, range: Range, hour: u8) -> Result<Time, Error> {
    let (minute, _) = expect_two_digit_num(chars, range)
        .map_err(|e| e.kind(Minute))?
        .check_range(0..=59)
        .map_err(|e| e.kind(Minute))?;

    expect_char(chars, range, ':')?;

    let (second, _) = expect_two_digit_num(chars, range)
        .map_err(|e| e.kind(Second))?
        .check_range(0..=59)
        .map_err(|e| e.kind(Second))?;

    match chars.peek() {
        // ignore offset
        Some((_, 'Z' | '+' | '-')) => (),
        Some(&(i, c)) => return invalid_char_error(c, range, i),
        None => (),
    }

    Ok(Time {
        hour,
        minute,
        second,
        nanos: 0,
    })
}

pub fn parse_subsec_and_offset(
    chars: &mut CharIter,
    range: Range,
) -> Result<(u32, Option<Offset>), Error> {
    let nanos = parse_subsec(chars, range)?;
    let offset = try_to_parse_offset(chars, range)?;
    Ok((nanos, offset))
}

pub fn parse_subsec_without_offset(chars: &mut CharIter, range: Range) -> Result<u32, Error> {
    let nanos = parse_subsec(chars, range)?;
    error_on_offset(chars, range)?;
    Ok(nanos)
}

fn parse_subsec(chars: &mut CharIter, range: Range) -> Result<u32, Error> {
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
            _ => return invalid_char_error(c, range, i),
        }
    }

    if subsec_digits == 0 {
        return Err(Error::DateTimeMissingSubsec(range.end));
    }

    let nanos = subsec * 10_u32.pow(9 - subsec_digits);
    Ok(nanos)
}

fn try_to_parse_offset(chars: &mut CharIter, range: Range) -> Result<Option<Offset>, Error> {
    match chars.next() {
        Some((_, 'Z')) => Ok(Some(Offset::Utc)),
        Some((_, '+')) => {
            let minutes = parse_offset(chars, range)?;
            Ok(Some(Offset::Custom(minutes)))
        }
        Some((_, '-')) => {
            let minutes = parse_offset(chars, range)?;
            Ok(Some(Offset::Custom(-minutes)))
        }
        Some((i, c)) => return invalid_char_error(c, range, i),
        None => Ok(None),
    }
}

fn error_on_offset(chars: &mut CharIter, range: Range) -> Result<(), Error> {
    match chars.next() {
        Some((i, 'Z' | '+' | '-')) => {
            let pos = range.start.plus(i as u32);
            return Err(Error::LocalDateTimeOffset(pos));
        }
        Some((i, c)) => return invalid_char_error(c, range, i),
        None => Ok(()),
    }
}

fn parse_offset(chars: &mut CharIter, range: Range) -> Result<i16, Error> {
    let (hour, _) = expect_two_digit_num(chars, range)
        .map_err(|e| e.kind(OffsetHour))?
        .check_range(0..=23)
        .map_err(|e| e.kind(OffsetHour))?;

    expect_char(chars, range, ':')?;

    let (minute, _) = expect_two_digit_num(chars, range)
        .map_err(|e| e.kind(OffsetMinute))?
        .check_range(0..=59)
        .map_err(|e| e.kind(OffsetMinute))?;

    Ok(60 * hour as i16 + minute as i16)
}

fn invalid_char_error<T>(char: char, range: Range, offset: usize) -> Result<T, Error> {
    let pos = range.start.plus(offset as u32);
    Err(Error::InvalidCharInDateTime(char, pos))
}

fn expect_char(chars: &mut CharIter, range: Range, expected: char) -> Result<(), Error> {
    match chars.next() {
        Some((_, c)) if c == expected => Ok(()),
        Some((i, c)) => {
            let pos = range.start.plus(i as u32);
            Err(Error::DateTimeExpectedCharFound(expected, c, pos))
        }
        None => Err(Error::DateTimeMissingChar(expected, range.end)),
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
            ExpectNumErrorKind::Invalid(c) => Error::InvalidCharInDateTime(c, self.1),
        }
    }
}

fn expect_two_digit_num(
    chars: &mut impl Iterator<Item = (usize, char)>,
    range: Range,
) -> Result<(u8, Range), ExpectNumError> {
    use ExpectNumErrorKind::*;

    let Some((start_offset, c)) = chars.next() else {
        let pos = range.end;
        return Err(ExpectNumError(Missing, pos));
    };
    let d0 = match c {
        '0'..='9' => c as u8 - '0' as u8,
        _ => {
            let pos = range.start.plus(start_offset as u32);
            return Err(ExpectNumError(Invalid(c), pos));
        }
    };

    let Some((_, c)) = chars.next() else {
        let pos = range.end;
        return Err(ExpectNumError(Incomplete, pos));
    };
    let d1 = match c {
        '0'..='9' => c as u8 - '0' as u8,
        _ => {
            let pos = range.start.plus(start_offset as u32);
            return Err(ExpectNumError(Invalid(c), pos));
        }
    };

    let start = range.start.plus(start_offset as u32);
    let end = start.plus(2);
    let range = Range { start, end };
    Ok((10 * d0 + d1, range))
}

struct NumRangeError<T>(T, Range);

impl NumRangeError<u8> {
    fn kind(self, field: DateTimeField) -> Error {
        Error::DateTimeOutOfBounds(field, self.0, self.1)
    }
}

trait NumRangeCheck<T: PartialOrd<T>>: Sized {
    fn check_range(self, num_range: std::ops::RangeInclusive<T>) -> Result<Self, NumRangeError<T>>;
}

impl NumRangeCheck<u8> for (u8, Range) {
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
