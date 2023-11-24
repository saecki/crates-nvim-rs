use crate::toml::{Pos, Quote, Range};

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    MissingQuote(Quote, Pos),
    InvalidLiteral(String, Range),
    InvalidEscapeChar(char, Pos),
    InvalidUnicodeEscapeChar(char, Pos),
    InvalidUnicodeScalar(u32, Range),
    InvalidCharInIdentifier(char, Pos),
    UnfinishedEscapeSequence(Range),

    ExpectedEqFound(String, Range),
    ExpectedRightCurlyFound(String, Range),
    ExpectedRightSquareFound(String, Range),
    ExpectedKeyFound(String, Range),
    ExpectedValueFound(String, Range),
    ExpectedComma(Pos),
    ExpectedNewline(Pos),

    InvalidIntRadix(char, Pos),
    InvalidNumOrDateLiteralStart(char, Pos),
    InvalidCharInNumLiteral(char, Pos),
    NumLiteralStartsWithUnderscore(Pos),
    NumLiteralEndsWithUnderscore(Pos),

    MissingFloatFractionalPart(Pos),
    FloatEndsWithUnderscore(Pos),
    FloatFractEndsWithUnderscore(Pos),
    InvalidCharInFloatLiteral(char, Pos),
    FloatExponentStartsWithUnderscore(Pos),
    FloatExponentEndsWithUnderscore(Pos),
    InvalidCharInFloatExponent(char, Pos),
    FloatLiteralOverflow(Range),

    EmptyPrefixedIntValue(Pos),
    PrefixedIntValueStartsWithUnderscore(Pos),
    PrefixedIntValueEndsWithUnderscore(Pos),
    InvalidCharInPrefixedInt(char, Pos),
    IntDigitTooBig(u8, char, Pos),
    IntLiteralOverflow(Range),

    InvalidCharInDateTime(char, Pos),
    DateTimeExpectedCharFound(char, char, Pos),
    DateTimeMissingChar(char, Pos),
    DateTimeIncomplete(DateTimeField, Pos),
    DateTimeMissing(DateTimeField, Pos),
    DateTimeOutOfBounds(DateTimeField, u8, Range),
    DateTimeMissingSubsec(Pos),
    LocalDateTimeOffset(Pos),
    DateAndTimeTooFarApart(Range),
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

#[derive(Debug, PartialEq, Eq)]
pub enum Warning {}
