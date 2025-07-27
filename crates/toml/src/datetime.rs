/// All variants allowed by the [toml spec](https://toml.io/en/v1.0.0#offset-date-time).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DateTime {
    OffsetDateTime(Date, Time, Offset),
    LocalDateTime(Date, Time),
    LocalDate(Date),
    LocalTime(Time),
}

impl std::fmt::Display for DateTime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(date) = self.date() {
            std::fmt::Display::fmt(&date, f)?;
        }
        if let Some(time) = self.time() {
            if self.date().is_some() {
                f.write_str("T")?;
            }
            std::fmt::Display::fmt(&time, f)?;
        }
        if let Some(offset) = self.offset() {
            std::fmt::Display::fmt(&offset, f)?;
        }
        Ok(())
    }
}

impl DateTime {
    pub fn from_optional_offset(date: Date, time: Time, offset: Option<Offset>) -> Self {
        match offset {
            Some(o) => Self::OffsetDateTime(date, time, o),
            None => Self::LocalDateTime(date, time),
        }
    }

    pub fn date(self) -> Option<Date> {
        match self {
            DateTime::OffsetDateTime(date, _, _) => Some(date),
            DateTime::LocalDateTime(date, _) => Some(date),
            DateTime::LocalDate(date) => Some(date),
            DateTime::LocalTime(_) => None,
        }
    }

    pub fn time(self) -> Option<Time> {
        match self {
            DateTime::OffsetDateTime(_, time, _) => Some(time),
            DateTime::LocalDateTime(_, time) => Some(time),
            DateTime::LocalDate(_) => None,
            DateTime::LocalTime(time) => Some(time),
        }
    }

    pub fn offset(self) -> Option<Offset> {
        match self {
            DateTime::OffsetDateTime(_, _, offset) => Some(offset),
            DateTime::LocalDateTime(_, _) => None,
            DateTime::LocalDate(_) => None,
            DateTime::LocalTime(_) => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Date {
    pub year: u16,
    pub month: u8,
    pub day: u8,
}

impl std::fmt::Display for Date {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Date { year, month, day } = self;
        write!(f, "{year:04}-{month:02}-{day:02}")
    }
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

impl std::fmt::Display for Time {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Time {
            hour,
            minute,
            second,
            mut nanos,
        } = *self;
        write!(f, "{hour:02}:{minute:02}:{second:02}")?;
        nanos = nanos.min(999_999_999);
        if nanos != 0 {
            let mut width = 9;
            while nanos % 10 == 0 {
                width -= 1;
                nanos /= 10;
            }
            write!(f, ".{nanos:0>width$}")?;
        }
        Ok(())
    }
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

impl std::fmt::Display for Offset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Offset::Utc => f.write_str("Z"),
            Offset::Custom(mut minutes) => {
                let mut sign = '+';
                if minutes < 0 {
                    minutes = minutes.abs();
                    sign = '-';
                }
                let hour = minutes / 60;
                let minute = minutes % 60;
                write!(f, "{sign}{hour:02}:{minute:02}")
            }
        }
    }
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

impl DateTimeField {
    pub fn to_str(&self) -> &'static str {
        match self {
            DateTimeField::Year => "year",
            DateTimeField::Month => "month",
            DateTimeField::Day => "day",
            DateTimeField::Hour => "hour",
            DateTimeField::Minute => "minute",
            DateTimeField::Second => "second",
            DateTimeField::OffsetHour => "offset-hour",
            DateTimeField::OffsetMinute => "offset-minute",
        }
    }
}

impl std::fmt::Display for DateTimeField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_str())
    }
}
