pub use error::Error;
pub use parse::*;

use crate::inlinestr::InlineStr;

mod error;
mod inlinestr;
mod parse;

pub struct VersionReq {
    pub op: Op,
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
    pub pre: Prerelease,
}

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

pub struct Version {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
    pub pre: Prerelease,
    pub meta: BuildMetadata,
}

pub struct Prerelease {
    str: InlineStr,
}

impl Prerelease {
    pub const EMPTY: Self = Self {
        str: InlineStr::new(),
    };
}

pub struct BuildMetadata {
    str: InlineStr,
}

impl BuildMetadata {
    pub const EMPTY: Self = Self {
        str: InlineStr::new(),
    };
}
