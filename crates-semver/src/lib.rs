use crate::inlinestr::InlineStr;

mod inlinestr;

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

pub struct BuildMetadata {
    str: InlineStr,
}

pub fn parse_requirement(input: &str) -> VersionReq {
    todo!()
}

pub fn parse_version(input: &str) -> Version {
    todo!()
}
