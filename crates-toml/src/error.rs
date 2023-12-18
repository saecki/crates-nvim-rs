use crate::{Pos, Quote, Span};

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    MissingQuote(Quote, Pos),
    InvalidLiteral(String, Span),
    InvalidEscapeChar(char, Pos),
    InvalidUnicodeEscapeChar(char, Pos),
    InvalidUnicodeScalar(u32, Span),
    InvalidCharInIdentifier(char, Pos),
    UnfinishedEscapeSequence(Span),

    ExpectedEqFound(String, Span),
    ExpectedRightCurlyFound(String, Span),
    ExpectedRightSquareFound(String, Span),
    ExpectedKeyFound(String, Span),
    ExpectedValueFound(String, Span),
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
    FloatLiteralOverflow(Span),

    EmptyPrefixedIntValue(Pos),
    PrefixedIntValueStartsWithUnderscore(Pos),
    PrefixedIntValueEndsWithUnderscore(Pos),
    InvalidCharInPrefixedInt(char, Pos),
    IntDigitTooBig(u8, char, Pos),
    IntLiteralOverflow(Span),

    InvalidCharInDateTime(char, Pos),
    DateTimeExpectedCharFound(char, char, Pos),
    DateTimeMissingChar(char, Pos),
    DateTimeIncomplete(DateTimeField, Pos),
    DateTimeMissing(DateTimeField, Pos),
    DateTimeOutOfBounds(DateTimeField, u8, Span),
    DateTimeMissingSubsec(Pos),
    LocalDateTimeOffset(Pos),
    DateAndTimeTooFarApart(Span),

    DuplicateKey(String, Span, Span),
    CannotExtendInlineArray(String, Span, Span),
    CannotExtendInlineTable(String, Span, Span),
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
