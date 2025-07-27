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
