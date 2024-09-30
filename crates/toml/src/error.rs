use common::diagnostic::{Diagnostic, DiagnosticHint, Severity};
use common::{FmtChar, FmtStr, Pos, Span};

use crate::datetime::DateTimeField;
use crate::parse::{IntPrefix, LitPart, Sign, RECURSION_LIMIT};
use crate::Quote;

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    MissingQuote(Quote, Span),
    ExcessiveQuotes(Quote, Span),
    InvalidStringChar(FmtChar, Span),
    InvalidEscapeChar(FmtChar, Pos),
    InvalidUnicodeEscapeChar(FmtChar, Pos),
    InvalidUnicodeCodepoint(u8, u32, Span),
    InvalidLineEndingEscape(Span),
    UnfinishedEscapeSequence(Span),
    InvalidCharInIdentifier(FmtChar, Pos),
    MultilineBasicStringIdent(Span),
    MultilineLiteralStringIdent(Span),
    InvalidCommentChar(FmtChar, Span),

    RecursionLimitExceeded(Pos),
    ExpectedEqOrDotFound(FmtStr, Span),
    ExpectedRightCurlyFound(FmtStr, Pos, Span),
    ExpectedRightSquareFound(FmtStr, Pos, Span),
    ExpectedDotOrRightSquareFound(FmtStr, Pos, Span),
    ExpectedKeyFound(FmtStr, Span),
    ExpectedValueFound(FmtStr, Span),
    MissingComma(Pos),
    ExpectedNewlineFound(FmtStr, Span),
    MissingNewline(Pos),
    InlineTableTrailingComma(Pos),
    SpaceBetweenArrayPars(Span),

    UnexpectedLiteralStart(FmtChar, Pos),
    UnexpectedLiteralChar(LitPart, FmtChar, Pos),
    LitStartsWithUnderscore(LitPart, Pos),
    LitEndsWithUnderscore(LitPart, Pos),
    ConsecutiveUnderscoresInLiteral(Span),
    MissingNumDigitsAfterSign(Sign, Pos),
    InvalidLeadingZero(Pos),
    ExpectedRadixOrDateTime(FmtChar, Pos),
    UnexpectedCharSignedLeadingZeroNum(FmtChar, Pos),

    UppercaseBareLitChar(FmtChar, &'static str, Pos),
    UnexpectedBareLitChar(FmtChar, &'static str, Pos),
    BareLitTrailingChars(FmtStr, &'static str, Span),
    BareLitMissingChars(&'static str, Pos),

    MissingFloatFractionalPart(Pos),
    FloatLiteralOverflow(Span),

    EmptyPrefixedIntValue(Pos),
    PrefixedIntSignNotAllowed(Pos),
    UppercaseIntRadix(IntPrefix, Pos),
    PrefixedIntValueStartsWithUnderscore(Pos),
    PrefixedIntValueEndsWithUnderscore(Pos),
    IntDigitTooBig(IntPrefix, FmtChar, Pos),
    IntLiteralOverflow(Span),

    // TODO: more info
    UnexpectedCharInDateTime(FmtChar, Pos),
    DateTimeExpectedCharFound {
        after: DateTimeField,
        expected: FmtChar,
        found: FmtChar,
        pos: Pos,
    },
    DateTimeMissingChar(DateTimeField, FmtChar, Pos),
    DateTimeIncomplete(DateTimeField, Pos),
    DateTimeMissing(DateTimeField, Pos),
    DateTimeOutOfBounds(DateTimeField, u8, (u8, u8), Span),
    DateTimeMissingSubsec(Pos),
    LocalDateTimeOffset(Pos),
    DateAndTimeTooFarApart(Span),

    DuplicateKey {
        lines: Box<[u32]>,
        path: FmtStr,
        orig: Span,
        duplicate: Span,
    },
    CannotExtendInlineTable {
        lines: Box<[u32]>,
        path: FmtStr,
        orig: Span,
        new: Span,
    },
    CannotExtendInlineArray {
        lines: Box<[u32]>,
        path: FmtStr,
        orig: Span,
        new: Span,
    },
    CannotExtendInlineArrayAsTable {
        lines: Box<[u32]>,
        path: FmtStr,
        orig: Span,
        new: Span,
    },
    CannotExtendTableWithDottedKey {
        lines: Box<[u32]>,
        path: FmtStr,
        orig: Span,
        new: Span,
    },
    CannotExtendArrayWithDottedKey {
        lines: Box<[u32]>,
        path: FmtStr,
        orig: Span,
        new: Span,
    },
}

impl Diagnostic for Error {
    type Hint = Hint;

    const SEVERITY: Severity = Severity::Error;

    fn span(&self) -> Span {
        use Error::*;
        match self {
            MissingQuote(_, s) => *s,
            ExcessiveQuotes(_, s) => *s,
            InvalidStringChar(_, s) => *s,
            InvalidEscapeChar(_, p) => Span::pos(*p),
            InvalidUnicodeEscapeChar(_, p) => Span::pos(*p),
            InvalidUnicodeCodepoint(_, _, s) => *s,
            InvalidLineEndingEscape(s) => *s,
            UnfinishedEscapeSequence(s) => *s,
            InvalidCharInIdentifier(_, p) => Span::pos(*p),
            MultilineBasicStringIdent(s) => *s,
            MultilineLiteralStringIdent(s) => *s,
            InvalidCommentChar(_, s) => *s,

            RecursionLimitExceeded(p) => Span::ascii_char(*p),
            ExpectedEqOrDotFound(_, s) => *s,
            ExpectedRightCurlyFound(_, _, s) => *s,
            ExpectedRightSquareFound(_, _, s) => *s,
            ExpectedDotOrRightSquareFound(_, _, s) => *s,
            ExpectedKeyFound(_, s) => *s,
            ExpectedValueFound(_, s) => *s,
            MissingComma(p) => Span::pos(*p),
            ExpectedNewlineFound(_, s) => *s,
            MissingNewline(p) => Span::pos(*p),
            InlineTableTrailingComma(p) => Span::ascii_char(*p),
            SpaceBetweenArrayPars(s) => *s,

            UnexpectedLiteralStart(c, p) => Span::from_pos_len(*p, c.len_utf8() as u32),
            UnexpectedLiteralChar(_, c, p) => Span::from_pos_len(*p, c.len_utf8() as u32),
            LitStartsWithUnderscore(_, p) => Span::ascii_char(*p),
            LitEndsWithUnderscore(_, p) => Span::ascii_char(*p),
            ConsecutiveUnderscoresInLiteral(s) => *s,
            MissingNumDigitsAfterSign(_, p) => Span::pos(*p),
            InvalidLeadingZero(p) => Span::ascii_char(*p),
            ExpectedRadixOrDateTime(c, p) => Span::from_pos_len(*p, c.len_utf8() as u32),
            UnexpectedCharSignedLeadingZeroNum(c, p) => Span::from_pos_len(*p, c.len_utf8() as u32),

            UppercaseBareLitChar(c, _, p) => Span::from_pos_len(*p, c.len_utf8() as u32),
            UnexpectedBareLitChar(c, _, p) => Span::from_pos_len(*p, c.len_utf8() as u32),
            BareLitTrailingChars(_, _, s) => *s,
            BareLitMissingChars(_, p) => Span::pos(*p),

            MissingFloatFractionalPart(p) => Span::pos(*p),
            FloatLiteralOverflow(s) => *s,

            EmptyPrefixedIntValue(p) => Span::pos(*p),
            PrefixedIntSignNotAllowed(p) => Span::ascii_char(*p),
            UppercaseIntRadix(_, p) => Span::ascii_char(*p),
            PrefixedIntValueStartsWithUnderscore(p) => Span::ascii_char(*p),
            PrefixedIntValueEndsWithUnderscore(p) => Span::ascii_char(*p),
            IntDigitTooBig(_, c, p) => Span::from_pos_len(*p, c.len_utf8() as u32),
            IntLiteralOverflow(s) => *s,

            UnexpectedCharInDateTime(c, p) => Span::from_pos_len(*p, c.len_utf8() as u32),
            DateTimeExpectedCharFound { found, pos, .. } => {
                Span::from_pos_len(*pos, found.len_utf8() as u32)
            }
            DateTimeMissingChar(_, _, p) => Span::pos(*p),
            DateTimeIncomplete(_, p) => Span::pos(*p),
            DateTimeMissing(_, p) => Span::pos(*p),
            DateTimeOutOfBounds(_, _, _, s) => *s,
            DateTimeMissingSubsec(p) => Span::pos(*p),
            LocalDateTimeOffset(p) => Span::ascii_char(*p),
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
            MissingQuote(quote, _) => {
                let kind = quote.kind_str();
                write!(f, "unterminated {kind} string, missing `{quote}`")
            }
            ExcessiveQuotes(quote, _) => write!(f, "excess quotes, only up to two consecutive quotes (`{}`) are allowed inside a multi-line string", quote.singleline()),
            InvalidStringChar(char, _) => write!(f, "invalid character `{char}` in string"),
            InvalidEscapeChar(char, _) => write!(f, "invalid escape character `{char}`, expected one of: `u`, `U`, `b`, `t`, `n`, `f`, `r`, `\"`, `\\`"),
            InvalidUnicodeEscapeChar(char, _) => write!(f, "invalid character `{char}` in unicode escape sequence, valid characters are: `a-f`, `A-F` and `0-9`"),
            InvalidUnicodeCodepoint(num_chars, cp, _) => write!(f, "invalid unicode scalar `0x{cp:0width$x}` (`{cp}`)", width = *num_chars as usize),
            InvalidLineEndingEscape(_) => write!(f, "invalid line ending backslash, missing newline"),
            UnfinishedEscapeSequence(_) => write!(f, "unfinished escape sequence"),
            InvalidCharInIdentifier(char, _) => write!(f, "invalid character `{char}` in identifier, valid characters are: `a-z`, `A-Z`, `0-9`, `_` and `-`"),
            MultilineBasicStringIdent(_) => write!(f, "multi-line strings cannot be used as keys"),
            MultilineLiteralStringIdent(_) => write!(f, "multi-line strings cannot be used as keys"),
            InvalidCommentChar(c, _) => write!(f, "invalid character `{c}` in comment"),

            RecursionLimitExceeded(_) => write!(f, "recursion limit of {RECURSION_LIMIT} exceeded"),
            ExpectedEqOrDotFound(token, _) => write!(f, "expected `=` or `.`, found {token}"),
            ExpectedRightCurlyFound(token, _, _) => write!(f, "expected `}}`, found {token}"),
            ExpectedRightSquareFound(token, _, _) => write!(f, "expected `]`, found {token}"),
            ExpectedDotOrRightSquareFound(token, _, _) => write!(f, "expected `.` or `]`, found {token}"),
            ExpectedKeyFound(token, _) => write!(f, "expected a key, found {token}"),
            ExpectedValueFound(token, _) => write!(f, "expected a value, found {token}"),
            MissingComma(_) => write!(f, "missing comma (`,`)"),
            ExpectedNewlineFound(token, _) => write!(f, "expected a line break, found {token}"),
            MissingNewline(_) => write!(f, "missing line break"),
            InlineTableTrailingComma(_) => write!(f, "trailing commas aren't permitted in inline tables"),
            SpaceBetweenArrayPars(_) => write!(f, "no space allowed between array header brackets"),

            UnexpectedLiteralStart(char, _) => write!(f, "unexpected character `{char}` at start of literal"),
            UnexpectedLiteralChar(part, char, _) => {
                write!(f, "unexpected character `{char}` in {part}")?;
                if *part == LitPart::IntOrFloat {
                    if let 'a'..='f' | 'A'..='F' = char.0 {
                        write!(f, ", hexadecimal integers need to be prefixed by `0x`")?;
                    }
                }
                Ok(())
            }
            LitStartsWithUnderscore(p, _) => write!(f, "{p} cannot start with `_`"),
            LitEndsWithUnderscore(p, _) => write!(f, "{p} cannot end with `_`"),
            ConsecutiveUnderscoresInLiteral(_) => {
                write!(f, "consecutive underscores (`_`) are not allowed in number literals")
            }
            MissingNumDigitsAfterSign(sign, _) => write!(f, "missing digit after sign `{sign}`, expected at least one"),
            InvalidLeadingZero(_) => write!(f, "invalid leading `0` in number"),
            ExpectedRadixOrDateTime(c, _) => write!(f, "unexpected character `{c}`, expected integer radix `b`, `o`, `x` or date-time"),
            UnexpectedCharSignedLeadingZeroNum(c, _) => write!(f, "unexpected character `{c}`"),

            UppercaseBareLitChar(c, expected, _) => write!(f, "uppercase character `{c}` in literal, expected `{expected}`"),
            UnexpectedBareLitChar(c, expected, _) => write!(f, "unexpected character `{c}` in literal, expected `{expected}`"),
            BareLitTrailingChars(s, expected, _) => write!(f, "trailing characters `{s}` in literal, expected `{expected}`"),
            BareLitMissingChars(expected, _) => write!(f, "missing characters in literal, expected `{expected}`"),

            MissingFloatFractionalPart(_) => write!(f, "missing fractional part of float literal, expected at least one digit"),
            FloatLiteralOverflow(_) => write!(f, "float literal overflow, number doesn't fit into a 64-bit IEEE float"),

            EmptyPrefixedIntValue(_) => write!(f, "missing integer digits, expected at least one"),
            PrefixedIntSignNotAllowed(_) => write!(f, "signs are not permitted for binary, octal, and hexadecimal integers"),
            UppercaseIntRadix(prefix, _) => {
                match prefix {
                    IntPrefix::Binary => write!(f, "found uppercase binary int prefix `B`, only lowercase `b` is permitted"),
                    IntPrefix::Octal => write!(f, "found uppercase octal int prefix `O`, only lowercase `o` is permitted"),
                    IntPrefix::Hexadecimal => write!(f, "found uppercase hexadecimal int prefix `X`, only lowercase `x` is permitted"),
                }
            }
            PrefixedIntValueStartsWithUnderscore(_) => write!(f, "integer literal cannot start with `_`"),
            PrefixedIntValueEndsWithUnderscore(_) => write!(f, "integer literal cannot end with `_`"),
            IntDigitTooBig(prefix, char, _) => {
                match prefix {
                    IntPrefix::Binary => write!(f, "binary digit `{char}` out of range, valid digits are `0` and `1`"),
                    IntPrefix::Octal => write!(f, "octal digit `{char}` out of range, valid digits are `0-7`"),
                    IntPrefix::Hexadecimal => write!(f, "hexadecimal digit `{char}` out of range, valid digits are `0-9`, `a-f`, and `A-F`"),
                }
            }
            IntLiteralOverflow(_) => write!(f, "integer literal overflow, number doesn't fit into a 64-bit signed integer"),

            UnexpectedCharInDateTime(char, _) => write!(f, "unexpected character `{char}` in date-time"),
            DateTimeExpectedCharFound { after, found, expected, .. } => write!(f, "unexpected character `{found}` in date-time after {after}, expected `{expected}`"),
            DateTimeMissingChar(field, expected, _) => write!(f, "incomplete date-time, missing character `{expected}` after {field}"),
            DateTimeIncomplete(field, _) => write!(f, "incomplete date-time, {field} is missing digits"),
            DateTimeMissing(field, _) => write!(f, "incomplete date-time, missing {field}"),
            DateTimeOutOfBounds(field, val, (min, max), _) => {
                write!(f, "date-time {field} `{val}` out of range, the valid range is `{min}..={max}`")
            }
            DateTimeMissingSubsec(_) => write!(f, "missing date-time fractional second, expected at least one digit"),
            LocalDateTimeOffset(_) => write!(f, "local-time doesn't permit an offset, see: https://toml.io/en/v1.0.0#local-time"),
            DateAndTimeTooFarApart(_) => write!(f, "date and time too far apart, they may only be separated by exactly one space"),

            DuplicateKey { path, .. } => write!(f, "duplicate key `{path}`"),
            CannotExtendInlineTable { path, .. } => write!(f, "cannot extend inline table `{path}`"),
            CannotExtendInlineArray { path, .. } => write!(f, "cannot extend inline array `{path}`"),
            CannotExtendInlineArrayAsTable { path, .. } => write!(f, "cannot extend inline array `{path}`, not a table"),
            CannotExtendTableWithDottedKey { path, .. } => write!(f, "cannot extend table `{path}` with dotted key"),
            CannotExtendArrayWithDottedKey { path, .. } => write!(f, "cannot extend array `{path}` with dotted key"),
        }
    }

    fn annotation(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        use Error::*;
        match self {
            MissingQuote(..) => write!(f, "unterminated string"),
            ExcessiveQuotes(..) => write!(f, "excess quotes"),
            InvalidStringChar(..) => write!(f, "invalid character"),
            InvalidEscapeChar(..) => write!(f, "invalid escape character"),
            InvalidUnicodeEscapeChar(..) => write!(f, "invalid unicode escape character"),
            InvalidUnicodeCodepoint(..) => write!(f, "invalid unicode scalar"),
            InvalidLineEndingEscape(_) => write!(f, "invalid line ending backslash"),
            UnfinishedEscapeSequence(_) => write!(f, "unfinished escape sequence"),
            InvalidCharInIdentifier(..) => write!(f, "invalid character in identifier"),
            MultilineBasicStringIdent(_) => write!(f, "not a valid key"),
            MultilineLiteralStringIdent(_) => write!(f, "not a valid key"),
            InvalidCommentChar(_, _) => write!(f, "invalid character"),

            RecursionLimitExceeded(_) => write!(f, "recursion limit of {RECURSION_LIMIT} exceeded"),
            ExpectedEqOrDotFound(..) => write!(f, "expected `=` or `.`"),
            ExpectedRightCurlyFound(..) => write!(f, "expected `}}`"),
            ExpectedRightSquareFound(..) => write!(f, "expected `]`"),
            ExpectedDotOrRightSquareFound(..) => write!(f, "Expected `.` or `]`"),
            ExpectedKeyFound(..) => write!(f, "expected a key"),
            ExpectedValueFound(..) => write!(f, "expected a value"),
            MissingComma(_) => write!(f, "missing comma (`,`)"),
            ExpectedNewlineFound(_, _) => write!(f, "expected a line break"),
            MissingNewline(_) => write!(f, "missing line break"),
            InlineTableTrailingComma(_) => write!(f, "trailing comma"),
            SpaceBetweenArrayPars(_) => write!(f, "no space allowed"),

            UnexpectedLiteralStart(..) => write!(f, "unexpected character"),
            UnexpectedLiteralChar(p, _, _) => write!(f, "unexpected character in {p}"),
            LitStartsWithUnderscore(p, _) => write!(f, "{p} cannot start with `_`"),
            LitEndsWithUnderscore(p, _) => write!(f, "{p} cannot end with `_`"),
            ConsecutiveUnderscoresInLiteral(_) => {
                write!(f, "consecutive underscores (`_`) not allowed")
            }
            MissingNumDigitsAfterSign(..) => write!(f, "missing digit after sign"),
            InvalidLeadingZero(_) => write!(f, "invalid leading `0`"),
            ExpectedRadixOrDateTime { .. } => write!(f, "unexpected character"),
            UnexpectedCharSignedLeadingZeroNum { .. } => write!(f, "unexpected character"),

            UppercaseBareLitChar(..) => write!(f, "uppercase character"),
            UnexpectedBareLitChar(..) => write!(f, "unexpected character"),
            BareLitTrailingChars(..) => write!(f, "trailing characters"),
            BareLitMissingChars(..) => write!(f, "missing characters"),

            MissingFloatFractionalPart(_) => write!(f, "missing fractional part of float literal"),
            FloatLiteralOverflow(_) => write!(f, "float literal overflow"),

            EmptyPrefixedIntValue(_) => write!(f, "missing integer digits"),
            PrefixedIntSignNotAllowed(_) => write!(f, "sign not allowed"),
            UppercaseIntRadix(_, _) => write!(f, "uppercase radix"),
            PrefixedIntValueStartsWithUnderscore(_) => {
                write!(f, "integer literal cannot start with `_`")
            }
            PrefixedIntValueEndsWithUnderscore(_) => {
                write!(f, "integer literal cannot end with `_`")
            }
            IntDigitTooBig(prefix, _, _) => match prefix {
                IntPrefix::Binary => write!(f, "binary digit out of range"),
                IntPrefix::Octal => write!(f, "octal digit out of range"),
                IntPrefix::Hexadecimal => write!(f, "hexadecimal digit  out of range"),
            },
            IntLiteralOverflow(_) => write!(f, "integer literal overflow"),

            UnexpectedCharInDateTime(..) => write!(f, "unexpected character"),
            DateTimeExpectedCharFound { .. } => write!(f, "unexpected character"),
            DateTimeMissingChar(..) => write!(f, "missing character"),
            DateTimeIncomplete(..) => write!(f, "missing digits"),
            DateTimeMissing(field, _) => write!(f, "missing {field}"),
            DateTimeOutOfBounds(field, _, _, _) => write!(f, "{field} out of range"),
            DateTimeMissingSubsec(_) => write!(f, "missing date-time fractional second"),
            LocalDateTimeOffset(_) => write!(f, "local-time doesn't permit an offset"),
            DateAndTimeTooFarApart(_) => write!(f, "date and time too far apart"),

            DuplicateKey { .. } => write!(f, "duplicate key"),
            CannotExtendInlineTable { .. } => write!(f, "cannot extend inline table"),
            CannotExtendInlineArray { .. } => write!(f, "cannot extend inline array"),
            CannotExtendInlineArrayAsTable { .. } => {
                write!(f, "cannot extend inline array, not a table")
            }
            CannotExtendTableWithDottedKey { .. } => {
                write!(f, "cannot extend table with dotted key")
            }
            CannotExtendArrayWithDottedKey { .. } => {
                write!(f, "cannot extend array with dotted key")
            }
        }
    }

    fn hint(&self) -> Option<Self::Hint> {
        use Error::*;
        match self {
            MissingQuote(..) => None,
            ExcessiveQuotes(..) => None,
            InvalidStringChar(_, _) => None,
            InvalidEscapeChar(_, _) => None,
            InvalidUnicodeEscapeChar(_, _) => None,
            InvalidUnicodeCodepoint(_, _, _) => None,
            InvalidLineEndingEscape(_) => None,
            UnfinishedEscapeSequence(_) => None,
            InvalidCharInIdentifier(_, _) => None,
            MultilineBasicStringIdent(_) => None,
            MultilineLiteralStringIdent(_) => None,
            InvalidCommentChar(_, _) => None,

            RecursionLimitExceeded(_) => None,
            ExpectedEqOrDotFound(_, _) => None,
            ExpectedRightCurlyFound(_, p, _) => Some(Hint::ExpectedRightCurlyFound(*p)),
            ExpectedRightSquareFound(_, p, _) => Some(Hint::ExpectedRightSquareFound(*p)),
            ExpectedDotOrRightSquareFound(_, p, _) => Some(Hint::ExpectedRightSquareFound(*p)),
            ExpectedKeyFound(_, _) => None,
            ExpectedValueFound(_, _) => None,
            MissingComma(_) => None,
            ExpectedNewlineFound(_, _) => None,
            MissingNewline(_) => None,
            InlineTableTrailingComma(_) => None,
            SpaceBetweenArrayPars(_) => None,

            UnexpectedLiteralStart(..) => None,
            UnexpectedLiteralChar(..) => None,
            LitStartsWithUnderscore(..) => None,
            LitEndsWithUnderscore(..) => None,
            ConsecutiveUnderscoresInLiteral(..) => None,
            MissingNumDigitsAfterSign(..) => None,
            InvalidLeadingZero(..) => None,
            ExpectedRadixOrDateTime(..) => None,
            UnexpectedCharSignedLeadingZeroNum(..) => None,

            UppercaseBareLitChar(..) => None,
            UnexpectedBareLitChar(..) => None,
            BareLitTrailingChars(..) => None,
            BareLitMissingChars(..) => None,

            MissingFloatFractionalPart(_) => None,
            FloatLiteralOverflow(_) => None,

            EmptyPrefixedIntValue(_) => None,
            PrefixedIntSignNotAllowed(_) => None,
            UppercaseIntRadix(_, _) => None,
            PrefixedIntValueStartsWithUnderscore(_) => None,
            PrefixedIntValueEndsWithUnderscore(_) => None,
            IntDigitTooBig(_, _, _) => None,
            IntLiteralOverflow(_) => None,

            UnexpectedCharInDateTime(_, _) => None,
            DateTimeExpectedCharFound { .. } => None,
            DateTimeMissingChar(_, _, _) => None,
            DateTimeIncomplete(_, _) => None,
            DateTimeMissing(_, _) => None,
            DateTimeOutOfBounds { .. } => None,
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

    fn context_lines(&self) -> Option<&[u32]> {
        use Error::*;
        match self {
            MissingQuote(..) => None,
            ExcessiveQuotes(..) => None,
            InvalidStringChar(..) => None,
            InvalidEscapeChar(..) => None,
            InvalidUnicodeEscapeChar(..) => None,
            InvalidUnicodeCodepoint(..) => None,
            InvalidLineEndingEscape(..) => None,
            UnfinishedEscapeSequence(..) => None,
            InvalidCharInIdentifier(..) => None,
            MultilineBasicStringIdent(..) => None,
            MultilineLiteralStringIdent(..) => None,
            InvalidCommentChar(..) => None,

            RecursionLimitExceeded(..) => None,
            ExpectedEqOrDotFound(..) => None,
            ExpectedRightCurlyFound(..) => None,
            ExpectedRightSquareFound(..) => None,
            ExpectedDotOrRightSquareFound(..) => None,
            ExpectedKeyFound(..) => None,
            ExpectedValueFound(..) => None,
            MissingComma(..) => None,
            ExpectedNewlineFound(..) => None,
            MissingNewline(..) => None,
            InlineTableTrailingComma(..) => None,
            SpaceBetweenArrayPars(..) => None,

            UnexpectedLiteralStart(..) => None,
            UnexpectedLiteralChar(..) => None,
            LitStartsWithUnderscore(..) => None,
            LitEndsWithUnderscore(..) => None,
            ConsecutiveUnderscoresInLiteral(..) => None,
            MissingNumDigitsAfterSign(..) => None,
            InvalidLeadingZero(..) => None,
            ExpectedRadixOrDateTime(..) => None,
            UnexpectedCharSignedLeadingZeroNum(..) => None,

            UppercaseBareLitChar(..) => None,
            UnexpectedBareLitChar(..) => None,
            BareLitTrailingChars(..) => None,
            BareLitMissingChars(..) => None,

            MissingFloatFractionalPart(..) => None,
            FloatLiteralOverflow(..) => None,

            EmptyPrefixedIntValue(..) => None,
            PrefixedIntSignNotAllowed(..) => None,
            UppercaseIntRadix(..) => None,
            PrefixedIntValueStartsWithUnderscore(..) => None,
            PrefixedIntValueEndsWithUnderscore(..) => None,
            IntDigitTooBig(..) => None,
            IntLiteralOverflow(..) => None,

            UnexpectedCharInDateTime(..) => None,
            DateTimeExpectedCharFound { .. } => None,
            DateTimeMissingChar(..) => None,
            DateTimeIncomplete(..) => None,
            DateTimeMissing(..) => None,
            DateTimeOutOfBounds { .. } => None,
            DateTimeMissingSubsec(..) => None,
            LocalDateTimeOffset(..) => None,
            DateAndTimeTooFarApart(..) => None,

            DuplicateKey { lines, .. } => Some(lines),
            CannotExtendInlineTable { lines, .. } => Some(lines),
            CannotExtendInlineArray { lines, .. } => Some(lines),
            CannotExtendInlineArrayAsTable { lines, .. } => Some(lines),
            CannotExtendTableWithDottedKey { lines, .. } => Some(lines),
            CannotExtendArrayWithDottedKey { lines, .. } => Some(lines),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
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
pub enum Hint {
    ExpectedRightCurlyFound(Pos),
    ExpectedRightSquareFound(Pos),
    DuplicateKey(Span),
    CannotExtendInlineTable(Span),
    CannotExtendInlineArray(Span),
    CannotExtendInlineArrayAsTable(Span),
    CannotExtendTableWithDottedKey(Span),
    CannotExtendArrayWithDottedKey(Span),
}

impl DiagnosticHint for Hint {
    fn span(&self) -> Span {
        use Hint::*;
        match self {
            ExpectedRightCurlyFound(p) => Span::ascii_char(*p),
            ExpectedRightSquareFound(p) => Span::ascii_char(*p),
            DuplicateKey(s) => *s,
            CannotExtendInlineTable(s) => *s,
            CannotExtendInlineArray(s) => *s,
            CannotExtendInlineArrayAsTable(s) => *s,
            CannotExtendTableWithDottedKey(s) => *s,
            CannotExtendArrayWithDottedKey(s) => *s,
        }
    }

    fn annotation(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        use Hint::*;
        match self {
            ExpectedRightCurlyFound(_) => write!(f, "left `{{` defined here"),
            ExpectedRightSquareFound(_) => write!(f, "left `[` defined here"),
            DuplicateKey(_) => write!(f, "original key defined here"),
            CannotExtendInlineTable(_) => write!(f, "original table defined here"),
            CannotExtendInlineArray(_) => write!(f, "original array defined here"),
            CannotExtendInlineArrayAsTable(_) => write!(f, "original array defined here"),
            CannotExtendTableWithDottedKey(_) => write!(f, "original array defined here"),
            CannotExtendArrayWithDottedKey(_) => write!(f, "original array defined here"),
        }
    }
}
