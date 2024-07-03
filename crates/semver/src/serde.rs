use common::diagnostic::Diagnostic as _;
use common::Pos;
use serde::de::Visitor;

use crate::{parse_requirement, parse_version, Version, VersionReq};

pub struct SerdeError(crate::Error);

impl std::fmt::Display for SerdeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.description(f)
    }
}

impl serde::Serialize for Version {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.collect_str(self)
    }
}

impl<'de> serde::Deserialize<'de> for Version {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        deserializer.deserialize_str(VersionVisitor)
    }
}

struct VersionVisitor;

impl<'de> Visitor<'de> for VersionVisitor {
    type Value = Version;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("A semver version")
    }

    fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
        parse_version(v, Pos::ZERO).map_err(|e| E::custom(SerdeError(e)))
    }
}

impl serde::Serialize for VersionReq {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.collect_str(self)
    }
}

impl<'de> serde::Deserialize<'de> for VersionReq {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        deserializer.deserialize_str(VersionReqVisitor)
    }
}

struct VersionReqVisitor;

impl<'de> Visitor<'de> for VersionReqVisitor {
    type Value = VersionReq;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("A semver version")
    }

    fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
        parse_requirement(v, Pos::ZERO).map_err(|e| E::custom(SerdeError(e)))
    }
}
