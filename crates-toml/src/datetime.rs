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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DateTimeField {
    Year,
    Month,
    Day,
    Hour,
    Minute,
    Second,
    OffsetHour,
    OffsetMinute,
}

impl std::fmt::Display for DateTimeField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DateTimeField::Year => f.write_str("year"),
            DateTimeField::Month => f.write_str("month"),
            DateTimeField::Day => f.write_str("day"),
            DateTimeField::Hour => f.write_str("hour"),
            DateTimeField::Minute => f.write_str("minute"),
            DateTimeField::Second => f.write_str("second"),
            DateTimeField::OffsetHour => f.write_str("offset-hour"),
            DateTimeField::OffsetMinute => f.write_str("offset-minute"),
        }
    }
}
