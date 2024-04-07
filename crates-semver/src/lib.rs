pub use error::Error;
pub use parse::*;

use crate::inlinestr::InlineStr;

mod display;
mod error;
mod inlinestr;
mod parse;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Offset {
    char: u32,
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
pub enum NumField {
    Major,
    Minor,
    Patch,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum IdentField {
    Prerelease,
    BuildMetadata,
}

#[derive(Debug, PartialEq, Eq)]
pub struct VersionReq {
    pub comparators: Vec<Comparator>,
}

impl VersionReq {
    pub const EMPTY: Self = Self {
        comparators: Vec::new(),
    };

    pub fn new(comparators: Vec<Comparator>) -> Self {
        Self { comparators }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Comparator {
    /// This is always `<=` [`Self::version_offset`]. Even if [`Self::op`] is [`Op::Wl`] this will
    /// point to the start of the comparator
    pub op_offset: Offset,
    pub op: Op,
    pub version_offset: Offset,
    pub version: CompVersion,
    /// The comma after the comparator
    pub comma: Option<Offset>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Op {
    /// `=`
    Eq,
    /// `<`
    Lt,
    /// `<=`
    Le,
    /// `>`
    Gt,
    /// `>=`
    Ge,
    /// `^`
    Cr,
    /// `~`
    Tl,
    /// `*`
    Wl,
    /// A blank requirement, equivalent to [`Op::Cr`].
    Bl,
}

#[derive(Debug, PartialEq, Eq)]
pub enum CompVersion {
    /// `*` Only valid for [`Op::Wl`]
    Wl(WlChar),
    Major(u32, Option<(WlChar, Option<WlChar>)>),
    Minor(u32, u32, Option<WlChar>),
    Patch(u32, u32, u32),
    Pre(u32, u32, u32, Prerelease),
}

impl CompVersion {
    pub fn major(&self) -> Option<u32> {
        match self {
            CompVersion::Wl(_) => None,
            CompVersion::Major(major, _) => Some(*major),
            CompVersion::Minor(major, _, _) => Some(*major),
            CompVersion::Patch(major, _, _) => Some(*major),
            CompVersion::Pre(major, _, _, _) => Some(*major),
        }
    }

    pub fn minor(&self) -> Option<u32> {
        match self {
            CompVersion::Wl(_) => None,
            CompVersion::Major(_, _) => None,
            CompVersion::Minor(_, minor, _) => Some(*minor),
            CompVersion::Patch(_, minor, _) => Some(*minor),
            CompVersion::Pre(_, minor, _, _) => Some(*minor),
        }
    }

    pub fn patch(&self) -> Option<u32> {
        match self {
            CompVersion::Wl(_) => None,
            CompVersion::Major(_, _) => None,
            CompVersion::Minor(_, _, _) => None,
            CompVersion::Patch(_, _, patch) => Some(*patch),
            CompVersion::Pre(_, _, patch, _) => Some(*patch),
        }
    }

    pub fn pre(&self) -> &Prerelease {
        match self {
            CompVersion::Wl(_) => &EMPTY_PRERELEASE,
            CompVersion::Major(_, _) => &EMPTY_PRERELEASE,
            CompVersion::Minor(_, _, _) => &EMPTY_PRERELEASE,
            CompVersion::Patch(_, _, _) => &EMPTY_PRERELEASE,
            CompVersion::Pre(_, _, _, pre) => pre,
        }
    }
}

/// Wildcard character
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WlChar {
    /// `*`
    Star,
    /// `x`
    LowerX,
    /// `X`
    UpperX,
}

impl WlChar {
    pub fn char(&self) -> char {
        match self {
            Self::Star => '*',
            Self::LowerX => 'x',
            Self::UpperX => 'X',
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Version {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
    pub pre: Prerelease,
    pub meta: BuildMetadata,
}

static EMPTY_PRERELEASE: Prerelease = Prerelease::EMPTY;

#[derive(Clone, PartialEq, Eq)]
pub struct Prerelease {
    str: InlineStr,
}

impl Prerelease {
    pub const EMPTY: Self = Self {
        str: InlineStr::empty(),
    };

    pub fn as_str(&self) -> &str {
        self.str.as_str()
    }
}

impl std::ops::Deref for Prerelease {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.str.as_str()
    }
}

impl std::fmt::Debug for Prerelease {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Prerelease({:?})", self.str.as_str())
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct BuildMetadata {
    str: InlineStr,
}

impl BuildMetadata {
    pub const EMPTY: Self = Self {
        str: InlineStr::empty(),
    };

    pub fn as_str(&self) -> &str {
        self.str.as_str()
    }
}

impl std::ops::Deref for BuildMetadata {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.str.as_str()
    }
}

impl std::fmt::Debug for BuildMetadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "BuildMetadata({:?})", self.str.as_str())
    }
}
