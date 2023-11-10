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
    InvalidCharInNumOrDateLiteral(char, Pos),
    NumOrDateLiteralStartsWithUnderscore(Pos),
    NumOrDateLiteralEndsWithUnderscore(Pos),

    FloatExponentStartsWithUnderscore(Pos),
    FloatExponentEndsWithUnderscore(Pos),
    InvalidCharInFloatExponent(char, Pos),

    EmptyPrefixedIntValue(Pos),
    PrefixedIntValueStartsWithUnderscore(Pos),
    PrefixedIntValueEndsWithUnderscore(Pos),
    InvalidCharInPrefixedInt(char, Pos),
    IntDigitTooBig(u8, char, Pos),
    IntLiteralOverflow(Range),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Warning {}
