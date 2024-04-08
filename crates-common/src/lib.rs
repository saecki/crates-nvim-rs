use std::fmt::Write as _;

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
            c => f.write_char(c),
        }
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
