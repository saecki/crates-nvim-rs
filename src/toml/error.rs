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
    ExpectedComma(Pos),
    ExpectedKey(String, Range),
    ExpectedValue(String, Range),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Warning {}
