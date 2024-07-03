use indexmap::IndexMap;
use serde::Deserialize;

pub use error::{Error, ErrorKind, ParseError};

mod error;
pub mod fetch;
#[cfg(test)]
mod test;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Crate {
    name: Box<str>,
    versions: Vec<Version>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Version {
    vers: semver::Version,
    yanked: bool,
    rust_version: Option<semver::Version>,
    default_features: Option<Vec<FeatureMember>>,
    features: IndexMap<Box<str>, Vec<FeatureMember>>,
    deps: Vec<Dependency>,
}

/// One of:
/// - `<feature_name>`              a plain feature
/// - `dep:<package_name>`          an explicit dependency
/// - `<package_name>/<feature>`    a feature of a dependency
/// - `<package_name>?/<feature>`   a weak dependency feature of an optional dependency, that is only
///                                 enabled if something else enabled the optional dependency
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FeatureMember(pub Box<str>);

impl FeatureMember {
    pub fn as_dep(&self) -> Option<&str> {
        self.0.strip_prefix("dep:")
    }

    pub fn is_dep(&self) -> bool {
        self.0.starts_with("dep:")
    }

    /// Whether `self` is a dependency feature or a weak dependency feature
    pub fn is_dep_feature(&self) -> bool {
        self.0.contains("/")
    }

    pub fn is_weak_dep_feature(&self) -> bool {
        self.0.contains("?/")
    }
}

impl std::cmp::PartialOrd for FeatureMember {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl std::cmp::Ord for FeatureMember {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self.as_dep(), other.as_dep()) {
            (Some(a), Some(b)) => a.cmp(b),
            (Some(_), None) => std::cmp::Ordering::Greater,
            (None, Some(_)) => std::cmp::Ordering::Less,
            (None, None) => match (self.is_dep_feature(), other.is_dep_feature()) {
                (true, true) | (false, false) => self.0.cmp(&other.0),
                (true, false) => std::cmp::Ordering::Greater,
                (false, true) => std::cmp::Ordering::Less,
            },
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize)]
pub struct Dependency {
    name: Box<str>,
    package: Option<Box<str>>,
    req: semver::VersionReq,
    default_features: bool,
    features: Vec<Box<str>>,
    kind: DependencyKind,
    target: Option<Box<str>>,
    optional: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DependencyKind {
    Normal,
    Dev,
    Build,
}
