use std::fmt::Write as _;

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
