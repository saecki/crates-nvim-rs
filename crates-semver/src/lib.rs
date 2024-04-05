pub use error::Error;
pub use parse::*;

use crate::inlinestr::InlineStr;

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
    /// can be larger than [Self::version_offset] when [`Self::op`] is [`Op::Wl`]
    pub op_offset: Offset,
    pub op: Op,
    pub version_offset: Offset,
    pub version: CompVersion,
    pub comma: Option<Offset>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum CompVersion {
    /// Only valid for [`Op::Wl`]
    Empty,
    Major(u32),
    Minor(u32, u32),
    Patch(u32, u32, u32),
    Pre(u32, u32, u32, Prerelease),
}

impl CompVersion {
    pub fn major(&self) -> Option<u32> {
        match self {
            CompVersion::Empty => None,
            CompVersion::Major(major) => Some(*major),
            CompVersion::Minor(major, _) => Some(*major),
            CompVersion::Patch(major, _, _) => Some(*major),
            CompVersion::Pre(major, _, _, _) => Some(*major),
        }
    }

    pub fn minor(&self) -> Option<u32> {
        match self {
            CompVersion::Empty => None,
            CompVersion::Major(_) => None,
            CompVersion::Minor(_, minor) => Some(*minor),
            CompVersion::Patch(_, minor, _) => Some(*minor),
            CompVersion::Pre(_, minor, _, _) => Some(*minor),
        }
    }

    pub fn patch(&self) -> Option<u32> {
        match self {
            CompVersion::Empty => None,
            CompVersion::Major(_) => None,
            CompVersion::Minor(_, _) => None,
            CompVersion::Patch(_, _, patch) => Some(*patch),
            CompVersion::Pre(_, _, patch, _) => Some(*patch),
        }
    }

    pub fn pre(&self) -> Option<&Prerelease> {
        match self {
            CompVersion::Empty => None,
            CompVersion::Major(_) => None,
            CompVersion::Minor(_, _) => None,
            CompVersion::Patch(_, _, _) => None,
            CompVersion::Pre(_, _, _, pre) => Some(pre),
        }
    }
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
}

impl std::fmt::Debug for BuildMetadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "BuildMetadata({:?})", self.str.as_str())
    }
}
