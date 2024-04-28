use std::fmt::Write as _;
use std::ops::Deref;

pub trait Ctx: Sized {
    type Error;
    type Warning;
    type Hint;

    fn error(&mut self, error: impl Into<Self::Error>);

    fn warn(&mut self, warning: impl Into<Self::Warning>);

    fn hint(&mut self, hint: impl Into<Self::Hint>);
}

pub struct Diagnostics<E, W, H> {
    pub errors: Vec<E>,
    pub warnings: Vec<W>,
    pub hints: Vec<H>,
}

impl<E, W, H> Default for Diagnostics<E, W, H> {
    fn default() -> Self {
        Self {
            errors: Vec::new(),
            warnings: Vec::new(),
            hints: Vec::new(),
        }
    }
}

impl<E, W, H> Ctx for Diagnostics<E, W, H> {
    type Error = E;
    type Warning = W;
    type Hint = H;

    fn error(&mut self, error: impl Into<E>) {
        self.errors.push(error.into());
    }

    fn warn(&mut self, warning: impl Into<W>) {
        self.warnings.push(warning.into());
    }

    fn hint(&mut self, hint: impl Into<H>) {
        self.hints.push(hint.into());
    }
}

pub trait Diagnostic {
    const SEVERITY: Severity;

    fn span(&self) -> Span;
    fn description(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result;
    fn annotation(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Hint,
}

impl std::fmt::Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Severity::Error => f.write_str("error"),
            Severity::Warning => f.write_str("warning"),
            Severity::Hint => f.write_str("hint"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Span {
    #[inline(always)]
    pub fn new(start: Pos, end: Pos) -> Self {
        Self { start, end }
    }

    #[inline(always)]
    pub fn from_pos_len(start: Pos, len: u32) -> Self {
        Self {
            start,
            end: start.plus(len),
        }
    }

    #[inline(always)]
    pub fn pos(pos: Pos) -> Self {
        Self {
            start: pos,
            end: pos,
        }
    }

    #[inline(always)]
    pub fn ascii_char(pos: Pos) -> Self {
        Self {
            start: pos,
            end: pos.plus(1),
        }
    }

    #[inline(always)]
    pub fn across(a: Self, b: Self) -> Self {
        Self {
            start: a.start,
            end: b.end,
        }
    }

    #[inline(always)]
    pub fn between(a: Self, b: Self) -> Self {
        Self {
            start: a.end,
            end: b.start,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pos {
    /// 0-based index of line
    pub line: u32,
    /// utf-8 byte index of line
    pub char: u32,
}

impl Pos {
    pub const ZERO: Self = Self::new(0, 0);

    #[inline(always)]
    pub const fn new(line: u32, char: u32) -> Self {
        Self { line, char }
    }

    #[inline(always)]
    pub fn after(&self, c: char) -> Self {
        Self {
            line: self.line,
            char: self.char + c.len_utf8() as u32,
        }
    }

    #[inline(always)]
    pub fn plus(&self, n: u32) -> Self {
        Self {
            line: self.line,
            char: self.char + n,
        }
    }

    #[inline(always)]
    pub fn minus(&self, n: u32) -> Self {
        Self {
            line: self.line,
            char: self.char - n,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Offset {
    pub char: u32,
}

impl Offset {
    pub fn new(char: u32) -> Self {
        Self { char }
    }

    pub fn minus(&self, n: u32) -> Self {
        Self {
            char: self.char - n,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FmtChar(pub char);

impl std::fmt::Display for FmtChar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            '\u{8}' => f.write_str("\\b"),
            '\t' => f.write_str("\\t"),
            '\n' => f.write_str("\\n"),
            '\u{C}' => f.write_str("\\f"),
            '\r' => f.write_str("\\r"),
            '\x00'..='\x1f' | '\x7f' => {
                let control_char = self.0 as u8;
                write!(f, "\\x{control_char:02}")
            }
            c => f.write_char(c),
        }
    }
}

impl From<char> for FmtChar {
    fn from(value: char) -> Self {
        Self(value)
    }
}

impl Deref for FmtChar {
    type Target = char;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FmtStr(pub Box<str>);

impl std::fmt::Display for FmtStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for c in self.0.chars() {
            std::fmt::Display::fmt(&FmtChar(c), f)?;
        }
        Ok(())
    }
}

impl FmtStr {
    pub fn from_string(value: String) -> Self {
        Self(value.into_boxed_str())
    }

    pub fn from_str(value: &str) -> Self {
        Self(value.into())
    }
}

impl From<&str> for FmtStr {
    fn from(value: &str) -> Self {
        Self::from_str(value)
    }
}

impl Deref for FmtStr {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}
