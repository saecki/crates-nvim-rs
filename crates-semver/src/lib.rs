use std::cmp::Ordering;

pub use error::Error;
pub use parse::*;

use crate::inlinestr::InlineStr;

mod display;
mod error;
mod eval;
mod inlinestr;
mod parse;
#[cfg(test)]
mod test;

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

    pub fn matches(&self, version: &Version) -> bool {
        eval::matches_requirement(self, version)
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

impl Comparator {
    pub fn matches(&self, version: &Version) -> bool {
        eval::matches_comparator(self, version)
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

#[derive(Debug, PartialEq, Eq)]
pub enum CompVersion {
    /// `*` Only valid for [`Op::Wl`]
    Wl(WlChar),
    Major(u32, Option<(WlChar, Option<WlChar>)>),
    Minor(u32, u32, Option<WlChar>),
    Patch(u32, u32, u32, Option<BuildMetadata>),
    Pre(u32, u32, u32, Prerelease, Option<BuildMetadata>),
}

impl CompVersion {
    pub fn major(&self) -> Option<u32> {
        match self {
            CompVersion::Wl(_) => None,
            CompVersion::Major(major, _) => Some(*major),
            CompVersion::Minor(major, _, _) => Some(*major),
            CompVersion::Patch(major, _, _, _) => Some(*major),
            CompVersion::Pre(major, _, _, _, _) => Some(*major),
        }
    }

    pub fn minor(&self) -> Option<u32> {
        match self {
            CompVersion::Wl(_) => None,
            CompVersion::Major(_, _) => None,
            CompVersion::Minor(_, minor, _) => Some(*minor),
            CompVersion::Patch(_, minor, _, _) => Some(*minor),
            CompVersion::Pre(_, minor, _, _, _) => Some(*minor),
        }
    }

    pub fn patch(&self) -> Option<u32> {
        match self {
            CompVersion::Wl(_) => None,
            CompVersion::Major(_, _) => None,
            CompVersion::Minor(_, _, _) => None,
            CompVersion::Patch(_, _, patch, _) => Some(*patch),
            CompVersion::Pre(_, _, patch, _, _) => Some(*patch),
        }
    }

    pub fn pre(&self) -> &Prerelease {
        match self {
            CompVersion::Wl(_) => &EMPTY_PRERELEASE,
            CompVersion::Major(_, _) => &EMPTY_PRERELEASE,
            CompVersion::Minor(_, _, _) => &EMPTY_PRERELEASE,
            CompVersion::Patch(_, _, _, _) => &EMPTY_PRERELEASE,
            CompVersion::Pre(_, _, _, pre, _) => pre,
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

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
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

impl PartialOrd for Prerelease {
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(rhs))
    }
}

impl Ord for Prerelease {
    fn cmp(&self, rhs: &Self) -> std::cmp::Ordering {
        // copy pasta from the semver crate

        match self.is_empty() {
            true if rhs.is_empty() => return Ordering::Equal,
            // A real release compares greater than prerelease.
            true => return Ordering::Greater,
            // Prerelease compares less than the real release.
            false if rhs.is_empty() => return Ordering::Less,
            false => {}
        }

        let lhs = self.as_str().split('.');
        let mut rhs = rhs.as_str().split('.');

        for lhs in lhs {
            let Some(rhs) = rhs.next() else {
                // Spec: "A larger set of pre-release fields has a higher
                // precedence than a smaller set, if all of the preceding
                // identifiers are equal."
                return Ordering::Greater;
            };

            let string_cmp = || Ord::cmp(lhs, rhs);
            let is_ascii_digit = |b: u8| b.is_ascii_digit();
            let ordering = match (
                lhs.bytes().all(is_ascii_digit),
                rhs.bytes().all(is_ascii_digit),
            ) {
                // Respect numeric ordering, for example 99 < 100. Spec says:
                // "Identifiers consisting of only digits are compared
                // numerically."
                (true, true) => Ord::cmp(&lhs.len(), &rhs.len()).then_with(string_cmp),
                // Spec: "Numeric identifiers always have lower precedence than
                // non-numeric identifiers."
                (true, false) => return Ordering::Less,
                (false, true) => return Ordering::Greater,
                // Spec: "Identifiers with letters or hyphens are compared
                // lexically in ASCII sort order."
                (false, false) => string_cmp(),
            };

            if ordering != Ordering::Equal {
                return ordering;
            }
        }

        if rhs.next().is_none() {
            Ordering::Equal
        } else {
            Ordering::Less
        }
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

impl PartialOrd for BuildMetadata {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        Some(self.cmp(rhs))
    }
}

impl Ord for BuildMetadata {
    fn cmp(&self, rhs: &Self) -> Ordering {
        // copy pasta from the semver crate

        let lhs = self.as_str().split('.');
        let mut rhs = rhs.as_str().split('.');

        for lhs in lhs {
            let Some(rhs) = rhs.next() else {
                return Ordering::Greater;
            };

            let is_ascii_digit = |b: u8| b.is_ascii_digit();
            let ordering = match (
                lhs.bytes().all(is_ascii_digit),
                rhs.bytes().all(is_ascii_digit),
            ) {
                (true, true) => {
                    // 0 < 00 < 1 < 01 < 001 < 2 < 02 < 002 < 10
                    let lhval = lhs.trim_start_matches('0');
                    let rhval = rhs.trim_start_matches('0');
                    Ord::cmp(&lhval.len(), &rhval.len())
                        .then_with(|| Ord::cmp(lhval, rhval))
                        .then_with(|| Ord::cmp(&lhs.len(), &rhs.len()))
                }
                (true, false) => return Ordering::Less,
                (false, true) => return Ordering::Greater,
                (false, false) => Ord::cmp(lhs, rhs),
            };

            if ordering != Ordering::Equal {
                return ordering;
            }
        }

        if rhs.next().is_none() {
            Ordering::Equal
        } else {
            Ordering::Less
        }
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
