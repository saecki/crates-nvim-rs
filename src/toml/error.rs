use crate::toml::{Struct, Pos, Quote, Range};

#[derive(Debug)]
pub enum Error {
    MissingQuote(Quote, Pos),
    MissingRightPar(Struct, Pos),
    InvalidRhsLiteral(String, Range),
    InvalidEscapeChar(char, Pos),
    InvalidUnicodeEscapeChar(char, Pos),
    InvalidUnicodeScalar(u32, Range),
    UnfinishedEscapeSequence(Pos),
}

#[derive(Debug)]
pub enum Warning {}
