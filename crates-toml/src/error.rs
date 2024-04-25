use common::{Diagnostic, FmtChar, FmtStr, Pos, Severity, Span};

use crate::datetime::DateTimeField;
use crate::parse::{IntPrefix, Sign};
use crate::Quote;

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    MissingQuote(Quote, Pos, Pos),
    InvalidEscapeChar(FmtChar, Pos),
    InvalidUnicodeEscapeChar(FmtChar, Pos),
    InvalidUnicodeCodepoint(u8, u32, Span),
    UnfinishedEscapeSequence(Span),
    InvalidCharInIdentifier(FmtChar, Pos),
    MultilineBasicStringIdent(Span),
    MultilineLiteralStringIdent(Span),

    ExpectedEqFound(FmtStr, Span),
    ExpectedRightCurlyFound(FmtStr, Span),
    ExpectedRightSquareFound(FmtStr, Span),
    ExpectedKeyFound(FmtStr, Span),
    ExpectedValueFound(FmtStr, Span),
    MissingComma(Pos),
    MissingNewline(Pos),
    InlineTableTrailingComma(Pos),
    SpaceBetweenArrayPars(Span),

    InvalidIntRadix(FmtChar, Pos),
    InvalidNumOrDateLiteralStart(FmtChar, Pos),
    InvalidCharInNumLiteral(FmtChar, Pos),
    NumOrDateLiteralStartsWithUnderscore(Pos),
    NumLiteralEndsWithUnderscore(Pos),
    MissingNumDigitsAfterSign(Sign, Pos),

    InvalidCharInFloatLiteral(FmtChar, Pos),
    FloatIntegralEndsWithUnderscore(Pos),
    MissingFloatFractionalPart(Pos),
    FloatFractStartsWithUnderscore(Pos),
    FloatFractEndsWithUnderscore(Pos),
    InvalidCharInFloatExponent(FmtChar, Pos),
    FloatExponentStartsWithUnderscore(Pos),
    FloatExponentEndsWithUnderscore(Pos),
    FloatLiteralOverflow(Span),

    EmptyPrefixedIntValue(Pos),
    PrefixedIntSignNotAllowed(Pos),
    UppercaseIntRadix(IntPrefix, Pos),
    PrefixedIntValueStartsWithUnderscore(Pos),
    PrefixedIntValueEndsWithUnderscore(Pos),
    InvalidCharInPrefixedInt(FmtChar, Pos),
    IntDigitTooBig(IntPrefix, FmtChar, Pos),
    IntLiteralOverflow(Span),

    InvalidCharInDateTime(FmtChar, Pos),
    DateTimeExpectedCharFound {
        after: DateTimeField,
        expected: FmtChar,
        found: FmtChar,
        pos: Pos,
    },
    DateTimeMissingChar(DateTimeField, FmtChar, Pos),
    DateTimeIncomplete(DateTimeField, Pos),
    DateTimeMissing(DateTimeField, Pos),
    DateTimeOutOfBounds(DateTimeField, u8, Span),
    DateTimeMissingSubsec(Pos),
    LocalDateTimeOffset(Pos),
    DateAndTimeTooFarApart(Span),

    DuplicateKey {
        path: Option<FmtStr>,
        key: FmtStr,
        orig: Span,
        duplicate: Span,
    },
    CannotExtendInlineTable {
        path: FmtStr,
        orig: Span,
        new: Span,
    },
    CannotExtendInlineArray {
        path: FmtStr,
        orig: Span,
        new: Span,
    },
    CannotExtendInlineArrayAsTable {
        path: FmtStr,
        orig: Span,
        new: Span,
    },
    CannotExtendTableWithDottedKey {
        path: FmtStr,
        orig: Span,
        new: Span,
    },
    CannotExtendArrayWithDottedKey {
        path: FmtStr,
        orig: Span,
        new: Span,
    },
}

impl Diagnostic for Error {
    const SEVERITY: Severity = Severity::Error;

    fn span(&self) -> Span {
        use Error::*;

        match self {
            MissingQuote(_, _, p) => Span::pos(*p),
            InvalidEscapeChar(_, p) => Span::pos(*p),
            InvalidUnicodeEscapeChar(_, p) => Span::pos(*p),
            InvalidUnicodeCodepoint(_, _, s) => *s,
            UnfinishedEscapeSequence(s) => *s,
            InvalidCharInIdentifier(_, p) => Span::pos(*p),
            MultilineBasicStringIdent(s) => *s,
            MultilineLiteralStringIdent(s) => *s,

            ExpectedEqFound(_, s) => *s,
            ExpectedRightCurlyFound(_, s) => *s,
            ExpectedRightSquareFound(_, s) => *s,
            ExpectedKeyFound(_, s) => *s,
            ExpectedValueFound(_, s) => *s,
            MissingComma(p) => Span::pos(*p),
            MissingNewline(p) => Span::pos(*p),
            InlineTableTrailingComma(p) => Span::pos(*p),
            SpaceBetweenArrayPars(s) => *s,

            InvalidIntRadix(_, p) => Span::pos(*p),
            InvalidNumOrDateLiteralStart(_, p) => Span::pos(*p),
            InvalidCharInNumLiteral(_, p) => Span::pos(*p),
            NumOrDateLiteralStartsWithUnderscore(p) => Span::pos(*p),
            NumLiteralEndsWithUnderscore(p) => Span::pos(*p),
            MissingNumDigitsAfterSign(_, p) => Span::pos(*p),

            InvalidCharInFloatLiteral(_, p) => Span::pos(*p),
            FloatIntegralEndsWithUnderscore(p) => Span::pos(*p),
            MissingFloatFractionalPart(p) => Span::pos(*p),
            FloatFractStartsWithUnderscore(p) => Span::pos(*p),
            FloatFractEndsWithUnderscore(p) => Span::pos(*p),
            InvalidCharInFloatExponent(_, p) => Span::pos(*p),
            FloatExponentStartsWithUnderscore(p) => Span::pos(*p),
            FloatExponentEndsWithUnderscore(p) => Span::pos(*p),
            FloatLiteralOverflow(s) => *s,

            EmptyPrefixedIntValue(p) => Span::pos(*p),
            PrefixedIntSignNotAllowed(p) => Span::pos(*p),
            UppercaseIntRadix(_, p) => Span::pos(*p),
            PrefixedIntValueStartsWithUnderscore(p) => Span::pos(*p),
            PrefixedIntValueEndsWithUnderscore(p) => Span::pos(*p),
            InvalidCharInPrefixedInt(_, p) => Span::pos(*p),
            IntDigitTooBig(_, _, p) => Span::pos(*p),
            IntLiteralOverflow(s) => *s,

            InvalidCharInDateTime(_, p) => Span::pos(*p),
            DateTimeExpectedCharFound { pos, .. } => Span::pos(*pos),
            DateTimeMissingChar(_, _, p) => Span::pos(*p),
            DateTimeIncomplete(_, p) => Span::pos(*p),
            DateTimeMissing(_, p) => Span::pos(*p),
            DateTimeOutOfBounds(_, _, s) => *s,
            DateTimeMissingSubsec(p) => Span::pos(*p),
            LocalDateTimeOffset(p) => Span::pos(*p),
            DateAndTimeTooFarApart(s) => *s,

            DuplicateKey { duplicate, .. } => *duplicate,
            CannotExtendInlineTable { new, .. } => *new,
            CannotExtendInlineArray { new, .. } => *new,
            CannotExtendInlineArrayAsTable { new, .. } => *new,
            CannotExtendTableWithDottedKey { new, .. } => *new,
            CannotExtendArrayWithDottedKey { new, .. } => *new,
        }
    }

    fn description(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        use Error::*;

        match self {
            MissingQuote(quote, _, _) => write!(f, "Unterminated string literal, missing `{quote}`"),
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
            InlineTableTrailingComma(_) => write!(f, "Trailing commas aren't permitted in inline tables"),
            SpaceBetweenArrayPars(_) => write!(f, "No space allowed between array header brackets"),

            InvalidIntRadix(char, _) => write!(f, "Invalid integer radix: `{char}`, valid radices are `b`, `o` and `x`"),
            InvalidNumOrDateLiteralStart(char, _) => write!(f, "Invalid character `{char}` at start of literal"),
            InvalidCharInNumLiteral(char, _) => {
                write!(f, "Invalid character `{char}` in integer or float literal")?;
                if let 'a'..='f' | 'A'..='F' = char.0 {
                    write!(f, ", hexadecimal integers need to be prefixed by `0x`")?;
                }
                Ok(())
            }
            NumOrDateLiteralStartsWithUnderscore(_) => write!(f, "Literal cannot start with `_`"),
            NumLiteralEndsWithUnderscore(_) => write!(f, "Integer or float literal cannot end with `_`"),
            MissingNumDigitsAfterSign(sign, _) => write!(f, "Missing digit after sign `{sign}`, expected at least one"),

            InvalidCharInFloatLiteral(char, _) => write!(f, "Invalid character `{char}` in float literal"),
            FloatIntegralEndsWithUnderscore(_) => write!(f, "Float integral cannot end with `_`"),
            MissingFloatFractionalPart(_) => write!(f, "Missing fractional part of float literal, expected at least one digit"),
            FloatFractStartsWithUnderscore(_) => write!(f, "Float fractional part cannot start with `_`"),
            FloatFractEndsWithUnderscore(_) => write!(f, "Float fractional part cannot end with `_`"),
            InvalidCharInFloatExponent(char, _) => write!(f, "Invalid character `{char}` in float exponent"),
            FloatExponentStartsWithUnderscore(_) => write!(f, "Float exponent cannot start with `_`"),
            FloatExponentEndsWithUnderscore(_) => write!(f, "Float exponent cannot end with `_`"),
            FloatLiteralOverflow(_) => write!(f, "Float literal overflow, number doesn't fit into a 64-bit IEEE float"),

            EmptyPrefixedIntValue(_) => write!(f, "Missing integer digits, expected at least one"),
            PrefixedIntSignNotAllowed(_) => write!(f, "Signs are not permitted for binary, octal, and hexadecimal integers"),
            UppercaseIntRadix(prefix, _) => {
                match prefix {
                    IntPrefix::Binary => write!(f, "Found uppercase binary int prefix `B`, only lowercase `b` is permitted"),
                    IntPrefix::Octal => write!(f, "Found uppercase octal int prefix `O`, only lowercase `o` is permitted"),
                    IntPrefix::Hexadecimal => write!(f, "Found uppercase hexadecimal int prefix `X`, only lowercase `x` is permitted"),
                }
            }
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
            DateTimeExpectedCharFound { after, found, expected, .. } => write!(f, "Invalid character `{found}` in date-time after {after}, expected `{expected}`"),
            DateTimeMissingChar(field, expected, _) => write!(f, "Incomplete date-time, missing character `{expected}` after {field}"),
            DateTimeIncomplete(field, _) => write!(f, "Incomplete date-time, {field} is missing digits"),
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
                    Some(max) => write!(f, "Date-time {field} `{num}` out of range, the valid range is `0..={max}`"),
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
            CannotExtendTableWithDottedKey { path, .. } => write!(f, "Cannot extend table `{path}` with dotted key"),
            CannotExtendArrayWithDottedKey { path, .. } => write!(f, "Cannot extend array `{path}` with dotted key"),
        }
    }

    fn annotation(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        use Error::*;

        match self {
            MissingQuote(..) => write!(f, "Unterminated string literal"),
            InvalidEscapeChar(..) => write!(f, "Invalid escape character"),
            InvalidUnicodeEscapeChar(..) => write!(f, "Invalid unicode escape character"),
            InvalidUnicodeCodepoint(..) => write!(f, "Invalid unicode code point"),
            UnfinishedEscapeSequence(_) => write!(f, "Unfinished escape sequence"),
            InvalidCharInIdentifier(..) => write!(f, "Invalid character in identifier"),
            MultilineBasicStringIdent(_) => write!(f, "Not a key"),
            MultilineLiteralStringIdent(_) => write!(f, "Not a key"),

            ExpectedEqFound(..) => write!(f, "Expected `=`"),
            ExpectedRightCurlyFound(..) => write!(f, "Expected `}}`"),
            ExpectedRightSquareFound(..) => write!(f, "Expected `]`"),
            ExpectedKeyFound(..) => write!(f, "Expected a key"),
            ExpectedValueFound(..) => write!(f, "Expected a value"),
            MissingComma(_) => write!(f, "Missing comma (`,`)"),
            MissingNewline(_) => write!(f, "Missing line break"),
            InlineTableTrailingComma(_) => write!(f, "Trailing comma"),
            SpaceBetweenArrayPars(_) => write!(f, "No space allowed"),

            InvalidIntRadix(..) => write!(f, "Invalid integer radix"),
            InvalidNumOrDateLiteralStart(..) => write!(f, "Invalid literal character"),
            InvalidCharInNumLiteral(..) => write!(f, "Invalid integer or float literal character"),
            NumOrDateLiteralStartsWithUnderscore(_) => write!(f, "Literal cannot start with `_`"),
            NumLiteralEndsWithUnderscore(_) => {
                write!(f, "Integer or float literal cannot end with `_`")
            }
            MissingNumDigitsAfterSign(..) => write!(f, "Missing digit after sign"),

            InvalidCharInFloatLiteral(..) => write!(f, "Invalid float literal character"),
            FloatIntegralEndsWithUnderscore(_) => write!(f, "Float literal cannot end with `_`"),
            MissingFloatFractionalPart(_) => write!(f, "Missing fractional part of float literal"),
            FloatFractStartsWithUnderscore(_) => {
                write!(f, "Float fractional part cannot start with `_`")
            }
            FloatFractEndsWithUnderscore(_) => {
                write!(f, "Float fractional part cannot end with `_`")
            }
            InvalidCharInFloatExponent(..) => write!(f, "Invalid float exponent character"),
            FloatExponentStartsWithUnderscore(_) => {
                write!(f, "Float exponent cannot start with `_`")
            }
            FloatExponentEndsWithUnderscore(_) => write!(f, "Float exponent cannot end with `_`"),
            FloatLiteralOverflow(_) => write!(f, "Float literal overflow"),

            EmptyPrefixedIntValue(_) => write!(f, "Missing integer digits"),
            PrefixedIntSignNotAllowed(_) => write!(f, "Sign not allowed"),
            UppercaseIntRadix(_, _) => write!(f, "Uppercase radix"),
            PrefixedIntValueStartsWithUnderscore(_) => {
                write!(f, "Integer literal cannot start with `_`")
            }
            PrefixedIntValueEndsWithUnderscore(_) => {
                write!(f, "Integer literal cannot end with `_`")
            }
            InvalidCharInPrefixedInt(..) => write!(f, "Invalid integer literal character"),
            IntDigitTooBig(prefix, _, _) => match prefix {
                IntPrefix::Binary => write!(f, "Binary digit out of range"),
                IntPrefix::Octal => write!(f, "Octal digit out of range"),
                IntPrefix::Hexadecimal => write!(f, "Binary digit  out of range"),
            },
            IntLiteralOverflow(_) => write!(f, "Integer literal overflow"),

            InvalidCharInDateTime(..) => write!(f, "Invalid character in date-time"),
            DateTimeExpectedCharFound { .. } => write!(f, "Invalid date-time character"),
            DateTimeMissingChar(..) => write!(f, "Missing character"),
            DateTimeIncomplete(..) => write!(f, "Missing digits"),
            DateTimeMissing(field, _) => write!(f, "Missing {field}"),
            DateTimeOutOfBounds(field, _, _) => write!(f, "Date-time {field} out of range"),
            DateTimeMissingSubsec(_) => write!(f, "Missing date-time fractional second"),
            LocalDateTimeOffset(_) => write!(f, "Local-time doesn't permit an offset"),
            DateAndTimeTooFarApart(_) => write!(f, "Date and time too far apart"),

            DuplicateKey { .. } => write!(f, "Duplicate key"),
            CannotExtendInlineTable { .. } => write!(f, "Cannot extend inline table"),
            CannotExtendInlineArray { .. } => write!(f, "Cannot extend inline array"),
            CannotExtendInlineArrayAsTable { .. } => {
                write!(f, "Cannot extend inline array, not a table")
            }
            CannotExtendTableWithDottedKey { .. } => {
                write!(f, "Cannot extend table with dotted key")
            }
            CannotExtendArrayWithDottedKey { .. } => {
                write!(f, "Cannot extend array with dotted key")
            }
        }
    }
}

impl Error {
    pub fn hint(&self) -> Option<Hint> {
        use Error::*;

        match self {
            MissingQuote(q, p, _) => Some(Hint::MissingQuote(*q, *p)),
            InvalidEscapeChar(_, _) => None,
            InvalidUnicodeEscapeChar(_, _) => None,
            InvalidUnicodeCodepoint(_, _, _) => None,
            UnfinishedEscapeSequence(_) => None,
            InvalidCharInIdentifier(_, _) => None,
            MultilineBasicStringIdent(_) => None,
            MultilineLiteralStringIdent(_) => None,

            ExpectedEqFound(_, _) => None,
            ExpectedRightCurlyFound(_, _) => None,
            ExpectedRightSquareFound(_, _) => None,
            ExpectedKeyFound(_, _) => None,
            ExpectedValueFound(_, _) => None,
            MissingComma(_) => None,
            MissingNewline(_) => None,
            InlineTableTrailingComma(_) => None,
            SpaceBetweenArrayPars(_) => None,

            InvalidIntRadix(_, _) => None,
            InvalidNumOrDateLiteralStart(_, _) => None,
            InvalidCharInNumLiteral(_, _) => None,
            NumOrDateLiteralStartsWithUnderscore(_) => None,
            NumLiteralEndsWithUnderscore(_) => None,
            MissingNumDigitsAfterSign(_, _) => None,

            InvalidCharInFloatLiteral(_, _) => None,
            FloatIntegralEndsWithUnderscore(_) => None,
            MissingFloatFractionalPart(_) => None,
            FloatFractStartsWithUnderscore(_) => None,
            FloatFractEndsWithUnderscore(_) => None,
            InvalidCharInFloatExponent(_, _) => None,
            FloatExponentStartsWithUnderscore(_) => None,
            FloatExponentEndsWithUnderscore(_) => None,
            FloatLiteralOverflow(_) => None,

            EmptyPrefixedIntValue(_) => None,
            PrefixedIntSignNotAllowed(_) => None,
            UppercaseIntRadix(_, _) => None,
            PrefixedIntValueStartsWithUnderscore(_) => None,
            PrefixedIntValueEndsWithUnderscore(_) => None,
            InvalidCharInPrefixedInt(_, _) => None,
            IntDigitTooBig(_, _, _) => None,
            IntLiteralOverflow(_) => None,

            InvalidCharInDateTime(_, _) => None,
            DateTimeExpectedCharFound { .. } => None,
            DateTimeMissingChar(_, _, _) => None,
            DateTimeIncomplete(_, _) => None,
            DateTimeMissing(_, _) => None,
            DateTimeOutOfBounds(_, _, _) => None,
            DateTimeMissingSubsec(_) => None,
            LocalDateTimeOffset(_) => None,
            DateAndTimeTooFarApart(_) => None,

            DuplicateKey { orig, .. } => Some(Hint::DuplicateKey(*orig)),
            CannotExtendInlineTable { orig, .. } => Some(Hint::CannotExtendInlineTable(*orig)),
            CannotExtendInlineArray { orig, .. } => Some(Hint::CannotExtendInlineArray(*orig)),
            CannotExtendInlineArrayAsTable { orig, .. } => {
                Some(Hint::CannotExtendInlineArrayAsTable(*orig))
            }
            CannotExtendTableWithDottedKey { orig, .. } => {
                Some(Hint::CannotExtendTableWithDottedKey(*orig))
            }
            CannotExtendArrayWithDottedKey { orig, .. } => {
                Some(Hint::CannotExtendArrayWithDottedKey(*orig))
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Warning {}

impl Diagnostic for Warning {
    const SEVERITY: Severity = Severity::Warning;

    fn span(&self) -> Span {
        todo!()
    }

    fn description(&self, _f: &mut impl std::fmt::Write) -> std::fmt::Result {
        todo!()
    }

    fn annotation(&self, _f: &mut impl std::fmt::Write) -> std::fmt::Result {
        todo!()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Hint {
    MissingQuote(Quote, Pos),
    DuplicateKey(Span),
    CannotExtendInlineTable(Span),
    CannotExtendInlineArray(Span),
    CannotExtendInlineArrayAsTable(Span),
    CannotExtendTableWithDottedKey(Span),
    CannotExtendArrayWithDottedKey(Span),
}

impl Diagnostic for Hint {
    const SEVERITY: Severity = Severity::Hint;

    fn span(&self) -> Span {
        use Hint::*;

        match self {
            MissingQuote(q, p) => Span::from_pos_len(*p, q.len()),
            DuplicateKey(s) => *s,
            CannotExtendInlineTable(s) => *s,
            CannotExtendInlineArray(s) => *s,
            CannotExtendInlineArrayAsTable(s) => *s,
            CannotExtendTableWithDottedKey(s) => *s,
            CannotExtendArrayWithDottedKey(s) => *s,
        }
    }

    fn description(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        use Hint::*;

        match self {
            MissingQuote(_, _) => write!(f, "Literal started here"),
            DuplicateKey(_) => write!(f, "Original key defined here"),
            CannotExtendInlineTable(_) => write!(f, "Original table defined here"),
            CannotExtendInlineArray(_) => write!(f, "Original array defined here"),
            CannotExtendInlineArrayAsTable(_) => write!(f, "Original array defined here"),
            CannotExtendTableWithDottedKey(_) => write!(f, "Original array defined here"),
            CannotExtendArrayWithDottedKey(_) => write!(f, "Original array defined here"),
        }
    }

    fn annotation(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        self.description(f)
    }
}
