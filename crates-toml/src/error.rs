use crate::datetime::DateTimeField;
use crate::{Pos, Quote, Span};

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    MissingQuote(Quote, Pos),
    InvalidEscapeChar(char, Pos),
    InvalidUnicodeEscapeChar(char, Pos),
    InvalidUnicodeScalar(u32, Span),
    UnfinishedEscapeSequence(Span),
    InvalidCharInIdentifier(char, Pos),
    MultilineBasicStringIdent(Span),
    MultilineLiteralStringIdent(Span),

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

    DuplicateKey {
        path: Option<String>,
        key: String,
        orig: Span,
        duplicate: Span,
    },
    CannotExtendInlineTable {
        path: String,
        orig: Span,
        new: Span,
    },
    CannotExtendInlineArray {
        path: String,
        orig: Span,
        new: Span,
    },
    CannotExtendInlineArrayAsTable {
        path: String,
        orig: Span,
        new: Span,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum Warning {}

impl Error {
    pub fn display<'a, T: AsRef<str> + 'a>(&'a self, text: &'a [T]) -> ErrorDisplay<'a, T> {
        ErrorDisplay { error: self, text }
    }

    pub fn description(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self {
            Error::MissingQuote(quote, _) => write!(f, "Unterminated string literal, missing `{quote}`"),
            Error::InvalidEscapeChar(char, _) => write!(f, "Invalid escape character `{char}`, expected one of: `u`, `U`, `b`, `t`, `n`, `f`, `r`, `\"`, `\\`, `\\n`"),
            Error::InvalidUnicodeEscapeChar(char, _) => todo!(),
            Error::InvalidUnicodeScalar(_, _) => todo!(),
            Error::UnfinishedEscapeSequence(_) => todo!(),
            Error::InvalidCharInIdentifier(char, _) => write!(f, "Invalid character `{char}` in identifier, valid characters are: `a-z`, `A-Z`, `0-9`, `_` and `-`"),
            Error::MultilineBasicStringIdent(_) => todo!(),
            Error::MultilineLiteralStringIdent(_) => todo!(),

            Error::ExpectedEqFound(token, _) => write!(f, "Expected `=`, found `{token}`"),
            Error::ExpectedRightCurlyFound(token, _) => write!(f, "Expected `}}`, found `{token}`"),
            Error::ExpectedRightSquareFound(token, _) => write!(f, "Expected `]`, found `{token}`"),
            Error::ExpectedKeyFound(token, _) => write!(f, "Expected a key, found `{token}`"),
            Error::ExpectedValueFound(token, _) => write!(f, "Expected a value, found `{token}`"),
            Error::ExpectedComma(_) => todo!(),
            Error::ExpectedNewline(_) => todo!(),

            Error::InvalidIntRadix(_, _) => todo!(),
            Error::InvalidNumOrDateLiteralStart(_, _) => todo!(),
            Error::InvalidCharInNumLiteral(_, _) => todo!(),
            Error::NumLiteralStartsWithUnderscore(_) => todo!(),
            Error::NumLiteralEndsWithUnderscore(_) => todo!(),

            Error::MissingFloatFractionalPart(_) => todo!(),
            Error::FloatEndsWithUnderscore(_) => todo!(),
            Error::FloatFractEndsWithUnderscore(_) => todo!(),
            Error::InvalidCharInFloatLiteral(_, _) => todo!(),
            Error::FloatExponentStartsWithUnderscore(_) => todo!(),
            Error::FloatExponentEndsWithUnderscore(_) => todo!(),
            Error::InvalidCharInFloatExponent(_, _) => todo!(),
            Error::FloatLiteralOverflow(_) => todo!(),

            Error::EmptyPrefixedIntValue(_) => todo!(),
            Error::PrefixedIntValueStartsWithUnderscore(_) => todo!(),
            Error::PrefixedIntValueEndsWithUnderscore(_) => todo!(),
            Error::InvalidCharInPrefixedInt(_, _) => todo!(),
            Error::IntDigitTooBig(_, _, _) => todo!(),
            Error::IntLiteralOverflow(_) => todo!(),

            Error::InvalidCharInDateTime(_, _) => todo!(),
            Error::DateTimeExpectedCharFound(_, _, _) => todo!(),
            Error::DateTimeMissingChar(_, _) => todo!(),
            Error::DateTimeIncomplete(_, _) => todo!(),
            Error::DateTimeMissing(_, _) => todo!(),
            Error::DateTimeOutOfBounds(_, _, _) => todo!(),
            Error::DateTimeMissingSubsec(_) => todo!(),
            Error::LocalDateTimeOffset(_) => todo!(),
            Error::DateAndTimeTooFarApart(_) => todo!(),

            Error::DuplicateKey { path, key, .. } => {
                match path {
                    Some(path) => write!(f, "Duplicate key `{key}` in table `{path}`"),
                    None => write!(f, "Duplicate key `{key}` in document root"),
                }
            }
            Error::CannotExtendInlineTable { path, .. } => write!(f, "Cannot extend inline table `{path}`"),
            Error::CannotExtendInlineArray { path, .. } => write!(f, "Cannot extend inline array `{path}`"),
            Error::CannotExtendInlineArrayAsTable { path, .. } => write!(f, "Cannot extend inline array `{path}`, not a table"),
        }
    }
}
pub struct ErrorDisplay<'a, T> {
    error: &'a Error,
    text: &'a [T],
}

impl<'a, T: AsRef<str> + 'a> std::fmt::Display for ErrorDisplay<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: highlight spans in text
        self.error.description(f)?;
        Ok(())
    }
}
