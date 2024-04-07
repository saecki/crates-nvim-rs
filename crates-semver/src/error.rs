use common::FmtStr;

use crate::{IdentField, NumField, Offset};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    TrailingCharacters(FmtStr, Offset),
    MissingField(NumField, Offset),
    LeadingZeroNum(NumField, Offset),
    InvalidIntChar(char, NumField, Offset),
    IntOverflow(NumField, Offset, u32),
    ExpectedDot(char, NumField, Offset),
    MissingDot(NumField, Offset),
    EmptyIdentifier(IdentField, Offset),
    EmptyIdentifierSegment(IdentField, Offset),
    LeadingZeroSegment(IdentField, Offset),
    InvalidOp(char, Offset),
    MissingComma(Offset),
    TrailingComma(Offset),
    WildcardNotTheSoleComparator(Offset),
    ExcessiveComparators(Offset),
    EmptyVersionReq,
    UnexpectedAfterWildcard(char, NumField, Offset),
}
