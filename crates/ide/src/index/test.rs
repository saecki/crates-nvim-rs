use common::Pos;
use pretty_assertions::assert_eq;

use super::*;

fn semver_version(str: &str) -> semver::Version {
    semver::parse_version(str, Pos::ZERO).unwrap()
}

fn semver_req(str: &str) -> semver::VersionReq {
    semver::parse_requirement(str, Pos::ZERO).unwrap()
}

fn feat<const SIZE: usize>(name: &str, members: [&str; SIZE]) -> (Box<str>, Vec<FeatureMember>) {
    (name.into(), feat_members(members))
}

fn feat_members<const SIZE: usize>(members: [&str; SIZE]) -> Vec<FeatureMember> {
    members
        .into_iter()
        .map(|m| FeatureMember(m.into()))
        .collect()
}

fn dep_feats<const SIZE: usize>(features: [&str; SIZE]) -> Vec<Box<str>> {
    features.map(|s| s.into()).to_vec()
}

#[test]
fn parse_diesel_index() {
    let str = include_str!("../../tests/diesel_index.json");
    let krate = fetch::parse_crate(str).unwrap();

    assert_eq!(&*krate.name, "diesel");

    let version = &krate.versions[4];

    assert_eq!(
        &Version {
            vers: semver_version("2.1.4"),
            yanked: false,
            rust_version: Some(semver_version("1.65.0")),
            default_features: Some(feat_members(["32-column-tables", "with-deprecated"])),
            features: IndexMap::from_iter([
                feat(
                    "128-column-tables",
                    ["64-column-tables", "diesel_derives/128-column-tables"]
                ),
                feat("32-column-tables", ["diesel_derives/32-column-tables"]),
                feat(
                    "64-column-tables",
                    ["32-column-tables", "diesel_derives/64-column-tables"]
                ),
                feat("chrono", ["diesel_derives/chrono", "dep:chrono"]),
                feat(
                    "extras",
                    [
                        "chrono",
                        "network-address",
                        "numeric",
                        "r2d2",
                        "time",
                        "dep:serde_json",
                        "dep:uuid",
                    ]
                ),
                feat("huge-tables", ["64-column-tables"]),
                feat(
                    "i-implement-a-third-party-backend-and-opt-into-breaking-changes",
                    []
                ),
                feat("ipnet-address", ["dep:ipnet", "dep:libc"]),
                feat("large-tables", ["32-column-tables"]),
                feat(
                    "mysql",
                    [
                        "mysql_backend",
                        "dep:bitflags",
                        "dep:mysqlclient-sys",
                        "dep:percent-encoding",
                        "dep:url",
                    ]
                ),
                feat("mysql_backend", ["diesel_derives/mysql", "dep:byteorder"]),
                feat("network-address", ["dep:ipnetwork", "dep:libc"]),
                feat("nightly-error-messages", []),
                feat(
                    "numeric",
                    [
                        "dep:bigdecimal",
                        "dep:num-bigint",
                        "dep:num-integer",
                        "dep:num-traits",
                    ]
                ),
                feat("postgres", ["postgres_backend", "dep:pq-sys"]),
                feat(
                    "postgres_backend",
                    [
                        "diesel_derives/postgres",
                        "dep:bitflags",
                        "dep:byteorder",
                        "dep:itoa",
                    ]
                ),
                feat("r2d2", ["diesel_derives/r2d2", "dep:r2d2"]),
                feat("returning_clauses_for_sqlite_3_35", []),
                feat(
                    "sqlite",
                    [
                        "diesel_derives/sqlite",
                        "time?/formatting",
                        "time?/parsing",
                        "dep:libsqlite3-sys",
                    ]
                ),
                feat("time", ["diesel_derives/time", "dep:time"]),
                feat("unstable", ["diesel_derives/nightly"]),
                feat("with-deprecated", ["diesel_derives/with-deprecated"]),
                feat("without-deprecated", ["diesel_derives/without-deprecated"]),
            ]),
            deps: vec![
                Dependency {
                    name: "bigdecimal".into(),
                    package: None,
                    req: semver_req(">=0.0.13, <0.5.0"),
                    default_features: true,
                    features: dep_feats([]),
                    kind: DependencyKind::Normal,
                    target: None,
                    optional: true
                },
                Dependency {
                    name: "bitflags".into(),
                    package: None,
                    req: semver_req("^2.0.0"),
                    default_features: true,
                    features: dep_feats([]),
                    kind: DependencyKind::Normal,
                    target: None,
                    optional: true
                },
                Dependency {
                    name: "byteorder".into(),
                    package: None,
                    req: semver_req("^1.0"),
                    default_features: true,
                    features: dep_feats([]),
                    kind: DependencyKind::Normal,
                    target: None,
                    optional: true
                },
                Dependency {
                    name: "cfg-if".into(),
                    package: None,
                    req: semver_req("^1"),
                    default_features: true,
                    features: dep_feats([]),
                    kind: DependencyKind::Dev,
                    target: None,
                    optional: false
                },
                Dependency {
                    name: "chrono".into(),
                    package: None,
                    req: semver_req("^0.4.20"),
                    default_features: false,
                    features: dep_feats(["clock", "std"]),
                    kind: DependencyKind::Normal,
                    target: None,
                    optional: true
                },
                Dependency {
                    name: "diesel_derives".into(),
                    package: None,
                    req: semver_req("~2.1.1"),
                    default_features: true,
                    features: dep_feats([]),
                    kind: DependencyKind::Normal,
                    target: None,
                    optional: false
                },
                Dependency {
                    name: "dotenvy".into(),
                    package: None,
                    req: semver_req("^0.15"),
                    default_features: true,
                    features: dep_feats([]),
                    kind: DependencyKind::Dev,
                    target: None,
                    optional: false
                },
                Dependency {
                    name: "ipnet".into(),
                    package: None,
                    req: semver_req("^2.5.0"),
                    default_features: true,
                    features: dep_feats([]),
                    kind: DependencyKind::Normal,
                    target: None,
                    optional: true
                },
                Dependency {
                    name: "ipnetwork".into(),
                    package: None,
                    req: semver_req(">=0.12.2, <0.21.0"),
                    default_features: true,
                    features: dep_feats([]),
                    kind: DependencyKind::Normal,
                    target: None,
                    optional: true
                },
                Dependency {
                    name: "ipnetwork".into(),
                    package: None,
                    req: semver_req(">=0.12.2, <0.21.0"),
                    default_features: true,
                    features: dep_feats([]),
                    kind: DependencyKind::Dev,
                    target: None,
                    optional: false
                },
                Dependency {
                    name: "itoa".into(),
                    package: None,
                    req: semver_req("^1.0.0"),
                    default_features: true,
                    features: dep_feats([]),
                    kind: DependencyKind::Normal,
                    target: None,
                    optional: true
                },
                Dependency {
                    name: "libc".into(),
                    package: None,
                    req: semver_req("^0.2.0"),
                    default_features: true,
                    features: dep_feats([]),
                    kind: DependencyKind::Normal,
                    target: None,
                    optional: true
                },
                Dependency {
                    name: "libsqlite3-sys".into(),
                    package: None,
                    req: semver_req(">=0.17.2, <0.28.0"),
                    default_features: true,
                    features: dep_feats(["bundled_bindings"]),
                    kind: DependencyKind::Normal,
                    target: None,
                    optional: true
                },
                Dependency {
                    name: "mysqlclient-sys".into(),
                    package: None,
                    req: semver_req("^0.2.5"),
                    default_features: true,
                    features: dep_feats([]),
                    kind: DependencyKind::Normal,
                    target: None,
                    optional: true
                },
                Dependency {
                    name: "num-bigint".into(),
                    package: None,
                    req: semver_req(">=0.2.0, <0.5.0"),
                    default_features: true,
                    features: dep_feats([]),
                    kind: DependencyKind::Normal,
                    target: None,
                    optional: true
                },
                Dependency {
                    name: "num-integer".into(),
                    package: None,
                    req: semver_req("^0.1.39"),
                    default_features: true,
                    features: dep_feats([]),
                    kind: DependencyKind::Normal,
                    target: None,
                    optional: true
                },
                Dependency {
                    name: "num-traits".into(),
                    package: None,
                    req: semver_req("^0.2.0"),
                    default_features: true,
                    features: dep_feats([]),
                    kind: DependencyKind::Normal,
                    target: None,
                    optional: true
                },
                Dependency {
                    name: "percent-encoding".into(),
                    package: None,
                    req: semver_req("^2.1.0"),
                    default_features: true,
                    features: dep_feats([]),
                    kind: DependencyKind::Normal,
                    target: None,
                    optional: true
                },
                Dependency {
                    name: "pq-sys".into(),
                    package: None,
                    req: semver_req("^0.4.0"),
                    default_features: true,
                    features: dep_feats([]),
                    kind: DependencyKind::Normal,
                    target: None,
                    optional: true
                },
                Dependency {
                    name: "quickcheck".into(),
                    package: None,
                    req: semver_req("^1.0.3"),
                    default_features: true,
                    features: dep_feats([]),
                    kind: DependencyKind::Normal,
                    target: None,
                    optional: true
                },
                Dependency {
                    name: "quickcheck".into(),
                    package: None,
                    req: semver_req("^1.0.3"),
                    default_features: true,
                    features: dep_feats([]),
                    kind: DependencyKind::Dev,
                    target: None,
                    optional: false
                },
                Dependency {
                    name: "r2d2".into(),
                    package: None,
                    req: semver_req(">=0.8.2, <0.9.0"),
                    default_features: true,
                    features: dep_feats([]),
                    kind: DependencyKind::Normal,
                    target: None,
                    optional: true
                },
                Dependency {
                    name: "serde_json".into(),
                    package: None,
                    req: semver_req(">=0.8.0, <2.0"),
                    default_features: true,
                    features: dep_feats([]),
                    kind: DependencyKind::Normal,
                    target: None,
                    optional: true
                },
                Dependency {
                    name: "time".into(),
                    package: None,
                    req: semver_req("^0.3.9"),
                    default_features: true,
                    features: dep_feats(["macros"]),
                    kind: DependencyKind::Normal,
                    target: None,
                    optional: true
                },
                Dependency {
                    name: "url".into(),
                    package: None,
                    req: semver_req("^2.1.0"),
                    default_features: true,
                    features: dep_feats([]),
                    kind: DependencyKind::Normal,
                    target: None,
                    optional: true
                },
                Dependency {
                    name: "uuid".into(),
                    package: None,
                    req: semver_req(">=0.7.0, <2.0.0"),
                    default_features: true,
                    features: dep_feats([]),
                    kind: DependencyKind::Normal,
                    target: None,
                    optional: true
                },
            ],
        },
        version
    );
}
