use crate::toml::{Pos, Range};

#[derive(Debug)]
pub enum Error {
    InvalidEscapeChar(char, Pos),
    InvalidUnicodeEscapeChar(char, Pos),
    InvalidUnicodeScalar(u32, Range),
    UnfinishedEscapeSequence(Pos),
}

#[derive(Debug)]
pub enum Warning {}
