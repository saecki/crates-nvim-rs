use common::diagnostic::{Diagnostic, DiagnosticHint, Severity};
use common::{FmtChar, FmtStr, Pos, Span};

use crate::{parse, IdentField, NumField, WlChar};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    TrailingCharacters(FmtStr, Option<IdentField>, Pos),
    MissingField(NumField, Pos),
    LeadingZeroNum(NumField, Pos),
    InvalidIntChar(FmtChar, NumField, Pos),
    IntOverflow(NumField, Pos, u32),
    ExpectedDot(FmtChar, NumField, Pos),
    MissingDot(NumField, Pos),
    EmptyIdentifier(IdentField, Pos),
    EmptyIdentifierSegment(IdentField, Pos),
    LeadingZeroSegment(IdentField, Pos),
    InvalidOp(FmtChar, Pos),
    MissingComma(Pos),
    TrailingComma(Pos),
    WildcardNotTheSoleComparator(WlChar, Pos),
    ExcessiveComparators(Pos, u32),
    EmptyVersionReq(Pos),
    UnexpectedAfterWildcard(FmtChar, NumField, Pos),
}

impl Diagnostic for Error {
    type Hint = Hint;

    const SEVERITY: Severity = Severity::Error;

    fn span(&self) -> Span {
        match self {
            Error::TrailingCharacters(s, _, p) => Span::from_pos_len(*p, s.len() as u32),
            Error::MissingField(_, p) => Span::pos(*p),
            Error::LeadingZeroNum(_, p) => Span::ascii_char(*p),
            Error::InvalidIntChar(c, _, p) => Span::from_pos_len(*p, c.len_utf8() as u32),
            Error::IntOverflow(_, p, l) => Span::from_pos_len(*p, *l),
            Error::ExpectedDot(c, _, p) => Span::from_pos_len(*p, c.len_utf8() as u32),
            Error::MissingDot(_, p) => Span::pos(*p),
            Error::EmptyIdentifier(_, p) => Span::pos(*p),
            Error::EmptyIdentifierSegment(_, p) => Span::pos(*p),
            Error::LeadingZeroSegment(_, p) => Span::ascii_char(*p),
            Error::InvalidOp(c, p) => Span::from_pos_len(*p, c.len_utf8() as u32),
            Error::MissingComma(p) => Span::pos(*p),
            Error::TrailingComma(p) => Span::ascii_char(*p),
            Error::WildcardNotTheSoleComparator(_, p) => Span::ascii_char(*p),
            Error::ExcessiveComparators(p, l) => Span::from_pos_len(*p, *l),
            Error::EmptyVersionReq(p) => Span::pos(*p),
            Error::UnexpectedAfterWildcard(c, _, p) => Span::from_pos_len(*p, c.len_utf8() as u32),
        }
    }

    fn description(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self {
            Error::TrailingCharacters(s, p, _) => {
                let name = p.map(ident_field).unwrap_or(num_field(NumField::Major));
                write!(f, "unexpected characters `{s}`, after {name}")
            }
            Error::MissingField(p, _) => write!(f, "missing {p}"),
            Error::LeadingZeroNum(p, _) => write!(f, "invalid leading zero in {p}"),
            Error::InvalidIntChar(c, p, _) => write!(f, "invalid character `{c}` zero in {p}"),
            Error::IntOverflow(p, _, _) => write!(f, "integer overflow in {p}"),
            Error::ExpectedDot(c, p, _) => write!(f, "expected dot after {p}, found `{c}`"),
            Error::MissingDot(p, _) => write!(f, "missing dot after {p}"),
            Error::EmptyIdentifier(p, _) => write!(f, "emtpy {p}"),
            Error::EmptyIdentifierSegment(p, _) => write!(f, "emtpy identifier segment in {p}"),
            Error::LeadingZeroSegment(p, _) => write!(f, "invalid leading zero in {p} segment "),
            Error::InvalidOp(c, _) => write!(f, "invalid operator `{c}`, expected one of: `=`, `>`, `>=`, `<`, `<=`, `~`, `^` or a blank version requirement"),
            Error::MissingComma(_) => write!(f, "missing comma"),
            Error::TrailingComma(_) => write!(f, "invalid trailing comma"),
            Error::WildcardNotTheSoleComparator(wl, _) => write!(f, "wildcard `{wl}` must be the only comparator"),
            Error::ExcessiveComparators(_, _) => write!(f, "excessive number of comparators, the maximum allowed number is {}", parse::MAX_COMPARATORS),
            Error::EmptyVersionReq(_) => write!(f, "empty version requirement"),
            Error::UnexpectedAfterWildcard(c, p, _) => write!(f, "unexpected character `{c}` after wildcard {p}"),
        }
    }

    fn annotation(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self {
            Error::TrailingCharacters(s, p, _) => {
                let name = p.map(ident_field).unwrap_or(num_field(NumField::Major));
                write!(f, "unexpected characters `{s}`, after {name}")
            }
            Error::MissingField(p, _) => write!(f, "missing {p}"),
            Error::LeadingZeroNum(_, _) => write!(f, "invalid leading zero"),
            Error::InvalidIntChar(_, _, _) => write!(f, "invalid character"),
            Error::IntOverflow(_, _, _) => write!(f, "integer overflow"),
            Error::ExpectedDot(_, _, _) => write!(f, "expected dot"),
            Error::MissingDot(_, _) => write!(f, "missing dot"),
            Error::EmptyIdentifier(p, _) => write!(f, "emtpy {p}"),
            Error::EmptyIdentifierSegment(_, _) => write!(f, "emtpy identifier segment"),
            Error::LeadingZeroSegment(_, _) => write!(f, "invalid leading zero"),
            Error::InvalidOp(_, _) => write!(f, "invalid operator"),
            Error::MissingComma(_) => write!(f, "missing comma"),
            Error::TrailingComma(_) => write!(f, "invalid trailing comma"),
            Error::WildcardNotTheSoleComparator(_, _) => {
                write!(f, "wildcard must be the only comparator")
            }
            Error::ExcessiveComparators(_, _) => write!(f, "excessive number of comparators"),
            Error::EmptyVersionReq(_) => write!(f, "empty version requirement"),
            Error::UnexpectedAfterWildcard(_, _, _) => write!(f, "unexpected character"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Warning {}

impl Diagnostic for Warning {
    type Hint = Hint;

    const SEVERITY: Severity = Severity::Warning;

    fn span(&self) -> Span {
        unreachable!()
    }

    fn description(&self, _f: &mut impl std::fmt::Write) -> std::fmt::Result {
        unreachable!()
    }

    fn annotation(&self, _f: &mut impl std::fmt::Write) -> std::fmt::Result {
        unreachable!()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Info {}

impl Diagnostic for Info {
    type Hint = Hint;

    const SEVERITY: Severity = Severity::Info;

    fn span(&self) -> Span {
        unreachable!()
    }

    fn description(&self, _f: &mut impl std::fmt::Write) -> std::fmt::Result {
        unreachable!()
    }

    fn annotation(&self, _f: &mut impl std::fmt::Write) -> std::fmt::Result {
        unreachable!()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Hint {}

impl DiagnosticHint for Hint {
    fn span(&self) -> Span {
        unreachable!()
    }

    fn annotation(&self, _f: &mut impl std::fmt::Write) -> std::fmt::Result {
        unreachable!()
    }
}

impl std::fmt::Display for NumField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(num_field(*self))
    }
}

fn num_field(f: NumField) -> &'static str {
    match f {
        NumField::Major => "major version number",
        NumField::Minor => "minor version number",
        NumField::Patch => "patch version number",
    }
}

impl std::fmt::Display for IdentField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(ident_field(*self))
    }
}

fn ident_field(f: IdentField) -> &'static str {
    match f {
        IdentField::BuildMetadata => "pre-release identifier",
        IdentField::Prerelease => "build-metadata",
    }
}
