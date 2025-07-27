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
            '\x00'..='\x1f' | '\x7f' => {
                let control_char = self.0 as u8;
                write!(f, "\\x{control_char:02x}")
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

impl std::ops::Deref for FmtChar {
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
    pub fn empty() -> Self {
        Self::from_str("")
    }

    pub fn from_string(value: String) -> Self {
        Self(value.into_boxed_str())
    }

    #[allow(clippy::should_implement_trait)]
    pub fn from_str(value: &str) -> Self {
        Self(value.into())
    }
}

impl From<&str> for FmtStr {
    fn from(value: &str) -> Self {
        Self::from_str(value)
    }
}

impl std::ops::Deref for FmtStr {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}
