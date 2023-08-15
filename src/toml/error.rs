use crate::toml::{Pos, Quote, Range};

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    MissingQuote(Quote, Pos),
    InvalidLiteral(String, Range),
    InvalidEscapeChar(char, Pos),
    InvalidUnicodeEscapeChar(char, Pos),
    InvalidUnicodeScalar(u32, Range),
    UnfinishedEscapeSequence(Range),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Warning {}
