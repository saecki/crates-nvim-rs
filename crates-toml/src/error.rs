use crate::datetime::DateTimeField;
use crate::parse::{IntPrefix, Sign};
use crate::{Pos, Quote, Span};

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    MissingQuote(Quote, Pos),
    InvalidEscapeChar(char, Pos),
    InvalidUnicodeEscapeChar(char, Pos),
    InvalidUnicodeCodepoint(u8, u32, Span),
    UnfinishedEscapeSequence(Span),
    InvalidCharInIdentifier(char, Pos),
    MultilineBasicStringIdent(Span),
    MultilineLiteralStringIdent(Span),

    ExpectedEqFound(String, Span),
    ExpectedRightCurlyFound(String, Span),
    ExpectedRightSquareFound(String, Span),
    ExpectedKeyFound(String, Span),
    ExpectedValueFound(String, Span),
    MissingComma(Pos),
    MissingNewline(Pos),

    InvalidIntRadix(char, Pos),
    InvalidNumOrDateLiteralStart(char, Pos),
    InvalidCharInNumLiteral(char, Pos),
    NumOrDateLiteralStartsWithUnderscore(Pos),
    NumLiteralEndsWithUnderscore(Pos),
    MissingNumDigitsAfterSign(Sign, Pos),

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
    IntDigitTooBig(IntPrefix, char, Pos),
    IntLiteralOverflow(Span),

    InvalidCharInDateTime(char, Pos),
    DateTimeExpectedCharFound(DateTimeField, char, char, Pos),
    DateTimeMissingChar(DateTimeField, char, Pos),
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
        use Error::*;

        match self {
            MissingQuote(quote, _) => write!(f, "Unterminated string literal, missing `{quote}`"),
            InvalidEscapeChar(char, _) => write!(f, "Invalid escape character `{char}`, expected one of: `u`, `U`, `b`, `t`, `n`, `f`, `r`, `\"`, `\\`, `\\n`"),
            InvalidUnicodeEscapeChar(char, _) => write!(f, "Invalid character `{char}` in unicode escape sequence, valid characters are: `a-f`, `A-F` and `0-9`"),
            InvalidUnicodeCodepoint(num_chars, cp, _) => write!(f, "Invalid unicode code point `0x{cp:0width$x}` (`{cp}`)", width = *num_chars as usize),
            UnfinishedEscapeSequence(_) => write!(f, "Unfinished escape sequence"),
            InvalidCharInIdentifier(char, _) => write!(f, "Invalid character `{char}` in identifier, valid characters are: `a-z`, `A-Z`, `0-9`, `_` and `-`"),
            MultilineBasicStringIdent(_) => write!(f, "Multi-line strings cannot be used as keys"),
            MultilineLiteralStringIdent(_) => write!(f, "Multi-line strings cannot be used as keys"),

            ExpectedEqFound(token, _) => write!(f, "Expected `=`, found `{token}`"),
            ExpectedRightCurlyFound(token, _) => write!(f, "Expected `}}`, found `{token}`"),
            ExpectedRightSquareFound(token, _) => write!(f, "Expected `]`, found `{token}`"),
            ExpectedKeyFound(token, _) => write!(f, "Expected a key, found `{token}`"),
            ExpectedValueFound(token, _) => write!(f, "Expected a value, found `{token}`"),
            MissingComma(_) => write!(f, "Missing comma (`,`)"),
            MissingNewline(_) => write!(f, "Missing line break"),

            InvalidIntRadix(char, _) => write!(f, "Invalid integer radix: `{char}`, valid radices are `b`, `o` and `x`"),
            InvalidNumOrDateLiteralStart(char, _) => write!(f, "Invalid character `{char}` at start of literal"),
            InvalidCharInNumLiteral(char, _) => write!(f, "Invalid character `{char}` in integer or float literal"),
            NumOrDateLiteralStartsWithUnderscore(_) => write!(f, "Literal cannot start with `_`"),
            NumLiteralEndsWithUnderscore(_) => write!(f, "Integer or float literal cannot end with `_`"),
            MissingNumDigitsAfterSign(sign, _) => write!(f, "Missing number after sign `{sign}`, expected at least one digit"),

            MissingFloatFractionalPart(_) => write!(f, "Missing fractional part of float literal, expected at least one digit"),
            FloatEndsWithUnderscore(_) => write!(f, "Float literal cannot end with `_`"),
            FloatFractEndsWithUnderscore(_) => write!(f, "Float fractional part cannot end with `_`"),
            InvalidCharInFloatLiteral(char, _) => write!(f, "Invalid character `{char}` in float literal"),
            FloatExponentStartsWithUnderscore(_) => write!(f, "Float exponent cannot start with `_`"),
            FloatExponentEndsWithUnderscore(_) => write!(f, "Float exponent cannot end with `_`"),
            InvalidCharInFloatExponent(char, _) => write!(f, "Invalid character `{char}` in float exponent"),
            FloatLiteralOverflow(_) => write!(f, "Float literal overflow, number doesn't fit into a 64-bit IEEE float"),

            EmptyPrefixedIntValue(_) => write!(f, "Missing integer digits, expected at least one"),
            PrefixedIntValueStartsWithUnderscore(_) => write!(f, "Integer literal cannot start with `_`"),
            PrefixedIntValueEndsWithUnderscore(_) => write!(f, "Integer literal cannot end with `_`"),
            InvalidCharInPrefixedInt(char, _) => write!(f, "Invalid character `{char}` in integer literal"),
            IntDigitTooBig(prefix, char, _) => {
                match prefix {
                    IntPrefix::Binary => write!(f, "Binary digit `{char}` out of range, valid digits are `0` and `1`"),
                    IntPrefix::Octal => write!(f, "Octal digit `{char}` out of range, valid digits are `0-7`"),
                    IntPrefix::Hexadecimal => write!(f, "Binary digit `{char}` out of range, valid digits are `0-9`, `a-f`, and `A-F`"),
                }
            }
            IntLiteralOverflow(_) => write!(f, "Integer literal overflow, number doesn't fit into a 64-bit signed integer"),

            InvalidCharInDateTime(char, _) => write!(f, "Invalid character `{char}` in date-time"),
            DateTimeExpectedCharFound(field, found, expected, _) => write!(f, "Invalid character `{found}` after date-time {field}, expected `{expected}`"),
            DateTimeMissingChar(field, expected, _) => write!(f, "Missing character `{expected}` after date-time {field}"),
            DateTimeIncomplete(field, _) => write!(f, "Incomplete {field} in date-time"),
            DateTimeMissing(field, _) => write!(f, "Incomplete date-time, missing {field}"),
            DateTimeOutOfBounds(field, num, _) => {
                let max = match field {
                    DateTimeField::Year => None,
                    DateTimeField::Month => Some(12),
                    DateTimeField::Day => Some(31),
                    DateTimeField::Hour => Some(23),
                    DateTimeField::Minute |
                    DateTimeField::Second |
                    DateTimeField::OffsetHour|
                    DateTimeField::OffsetMinute => Some(59),
                };

                match max {
                    Some(max) => write!(f, "Date-time {field} `{num}` out of range, the valid range is `0`..=`{max}`"),
                    None => write!(f, "Date-time {field} `{num}` out of range"),
                }
            }
            DateTimeMissingSubsec(_) => write!(f, "Missing date-time fractional second, expected at least one digit"),
            LocalDateTimeOffset(_) => write!(f, "Local-time doesn't permit an offset, see: https://toml.io/en/v1.0.0#local-time"),
            DateAndTimeTooFarApart(_) => write!(f, "Date and time too far apart, they may only be separated by exactly one space"),

            DuplicateKey { path, key, .. } => {
                match path {
                    Some(path) => write!(f, "Duplicate key `{key}` in table `{path}`"),
                    None => write!(f, "Duplicate key `{key}` in document root"),
                }
            }
            CannotExtendInlineTable { path, .. } => write!(f, "Cannot extend inline table `{path}`"),
            CannotExtendInlineArray { path, .. } => write!(f, "Cannot extend inline array `{path}`"),
            CannotExtendInlineArrayAsTable { path, .. } => write!(f, "Cannot extend inline array `{path}`, not a table"),
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
