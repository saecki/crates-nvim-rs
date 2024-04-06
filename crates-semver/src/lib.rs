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
    /// `1`
    Major(u32),
    /// `1.*`
    MajorWl(u32, WlChar),
    /// `1.2`
    Minor(u32, u32),
    /// `1.2.*`
    MinorWl(u32, u32, WlChar),
    /// `1.2.3`
    Patch(u32, u32, u32),
    /// `1.2.3-beta.1`
    Pre(u32, u32, u32, Prerelease),
}

impl CompVersion {
    pub fn major(&self) -> Option<u32> {
        match self {
            CompVersion::Wl(_) => None,
            CompVersion::Major(major)
            | CompVersion::MajorWl(major, _)
            | CompVersion::Minor(major, _)
            | CompVersion::MinorWl(major, _, _)
            | CompVersion::Patch(major, _, _)
            | CompVersion::Pre(major, _, _, _) => Some(*major),
        }
    }

    pub fn minor(&self) -> Option<u32> {
        match self {
            #[rustfmt::skip]
            CompVersion::Wl(_)
            | CompVersion::Major(_)
            | CompVersion::MajorWl(_, _) => None,
            CompVersion::Minor(_, minor)
            | CompVersion::MinorWl(_, minor, _)
            | CompVersion::Patch(_, minor, _)
            | CompVersion::Pre(_, minor, _, _) => Some(*minor),
        }
    }

    pub fn patch(&self) -> Option<u32> {
        match self {
            CompVersion::Wl(_)
            | CompVersion::Major(_)
            | CompVersion::MajorWl(_, _)
            | CompVersion::Minor(_, _)
            | CompVersion::MinorWl(_, _, _) => None,
            #[rustfmt::skip]
            CompVersion::Patch(_, _, patch)
            | CompVersion::Pre(_, _, patch, _) => Some(*patch),
        }
    }

    pub fn pre(&self) -> Option<&Prerelease> {
        match self {
            CompVersion::Wl(_)
            | CompVersion::Major(_)
            | CompVersion::MajorWl(_, _)
            | CompVersion::Minor(_, _)
            | CompVersion::MinorWl(_, _, _)
            | CompVersion::Patch(_, _, _) => None,
            CompVersion::Pre(_, _, _, pre) => Some(pre),
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

impl Ord for Version {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        todo!()
    }
}

impl PartialOrd for Version {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

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
