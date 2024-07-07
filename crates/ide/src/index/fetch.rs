use std::fmt::Write as _;
use std::time::Duration;

use common::FmtStr;
use http_req::request::Request;
use http_req::response::StatusCode;
use indexmap::IndexMap;
use serde_derive::Deserialize;

use crate::index::{
    Crate, Dependency, DependencyKind, Error, ErrorKind, FeatureMember, ParseError, Version,
};

const USER_AGENT: &str = "crates-nvim-rs (https://github.com/saecki/crates-nvim-rs)";

macro_rules! boxed_str {
    ($len:expr, $pat:expr, $str:expr) => {{
        let mut str = String::new();
        // reserve exact capacity to avoid reallocation when converting into `Box<str>`
        str.reserve_exact($len + $str.len());
        _ = write!(&mut str, $pat, $str);
        str.into_boxed_str()
    }};
}

pub struct DependencySource {
    pub registry: DependencyRegistry,
    pub name: Box<str>,
}

impl DependencySource {
    fn sparse_index_url(&self) -> Box<str> {
        let Self { registry, name } = self;
        match registry {
            DependencyRegistry::CratesIo => match name.len() {
                0 => unreachable!(),
                1 => boxed_str!(26, "https://index.crates.io/1/{}", name),
                2 => boxed_str!(26, "https://index.crates.io/2/{}", name),
                3 => {
                    let digit0 = &name[0..1];
                    boxed_str!(28, "https://index.crates.io/3/{digit0}/{}", name)
                }
                4.. => {
                    let digits01 = &name[0..2];
                    let digits23 = &name[2..4];
                    boxed_str!(30, "https://index.crates.io/{digits01}/{digits23}/{}", name)
                }
            },
            // TODO: if `sparse+` is specified we know what to do
            // otherwise consider fetching `config.json`
            DependencyRegistry::Other(_index) => todo!(),
        }
    }
}

pub enum DependencyRegistry {
    CratesIo,
    Other(Box<str>),
}

#[derive(Clone, PartialEq, Eq, Deserialize)]
struct DeserializeVersion<'a> {
    name: &'a str,
    vers: semver::Version,
    features: IndexMap<&'a str, Vec<&'a str>>,
    #[serde(default)]
    features2: IndexMap<&'a str, Vec<&'a str>>,
    deps: Vec<Dependency>,
    yanked: bool,
    rust_version: Option<semver::Version>,
    // TODO: is this useful?
    // v: &'a str,
}

pub fn fetch_crate<'a>(source: DependencySource) -> Result<Crate, Error> {
    let url = source.sparse_index_url();

    fetch_crate_from_url(&url).map_err(|kind| Error {
        name: FmtStr(source.name),
        url: FmtStr(url),
        kind,
    })
}

fn fetch_crate_from_url(url: &str) -> Result<Crate, ErrorKind> {
    let uri = http_req::uri::Uri::try_from(url.as_ref()).expect("url to be valid");

    let mut req = Request::new(&uri);
    req.header("User-Agent", USER_AGENT);
    req.timeout(Duration::from_secs(10));

    let mut resp_body = Vec::new();
    let resp = req
        .send(&mut resp_body)
        .map_err(|e| ErrorKind::Request(e))?;

    let status = resp.status_code();
    if !status.is_success() {
        return if status == StatusCode::new(404) {
            Err(ErrorKind::NotFound)
        } else {
            Err(ErrorKind::Status(status))
        };
    }

    let str = String::from_utf8(resp_body).map_err(|e| ErrorKind::Utf8(e))?;

    parse_crate(&str)
}

pub(crate) fn parse_crate(str: &str) -> Result<Crate, ErrorKind> {
    let mut name: Option<Box<str>> = None;
    let mut versions = Vec::with_capacity(str.lines().count());
    for l in str.lines() {
        let v: DeserializeVersion = serde_json::from_str(l).map_err(|e| ErrorKind::Json(e))?;
        match &name {
            Some(name) => {
                if name.as_ref() != v.name {
                    return Err(ErrorKind::Parse(ParseError::MismatchedNames));
                }
            }
            None => name = Some(v.name.into()),
        }

        let mut features = (v.features.iter())
            .chain(v.features2.iter())
            .map(|(&name, members)| {
                let mut members = (members.iter())
                    .map(|&m| {
                        let explicit_dep = m.starts_with("dep:");
                        let feat_exists =
                            || v.features.contains_key(m) || v.features2.contains_key(m);
                        let opt_dep_exists = || {
                            v.deps.iter().any(|d| {
                                d.kind == DependencyKind::Normal
                                    && d.optional
                                    && d.name.as_ref() == m
                            })
                        };

                        // normalize feature members to explicit `dep:<package_name>` syntax
                        if !explicit_dep && !feat_exists() && opt_dep_exists() {
                            FeatureMember(boxed_str!(4, "dep:{}", m))
                        } else {
                            FeatureMember(m.into())
                        }
                    })
                    .collect::<Vec<_>>();

                members.sort();

                (name.into(), members)
            })
            .collect::<IndexMap<_, _>>();

        let default_features = features.swap_remove("default");

        features.sort_keys();

        let version = Version {
            vers: v.vers,
            yanked: v.yanked,
            rust_version: v.rust_version,
            default_features,
            features,
            deps: v.deps,
        };

        versions.push(version);
    }

    versions.sort_by(|a, b| a.vers.cmp(&b.vers));

    let Some(name) = name else {
        return Err(ErrorKind::Parse(ParseError::NoVersion));
    };

    Ok(Crate { name, versions })
}
