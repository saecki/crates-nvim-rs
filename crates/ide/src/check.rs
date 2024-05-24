use common::{FmtStr, Span};
use semver::{SemverCtx, VersionReq};
use toml::map::{MapArray, MapArrayInlineEntry, MapNode, MapTable, MapTableEntry, Scalar};
use toml::parse::{BoolVal, StringVal};
use toml::util::Datatype;

use crate::cargo;
use crate::IdeCtx;

#[derive(Debug, Default, PartialEq)]
pub struct State<'a> {
    dependencies: Vec<Dependency<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Dependency<'a> {
    /// ```toml
    /// [dependencies]
    /// # always the `<name>`
    /// <name> = "..."
    /// <name> = { package = "..." }
    ///
    /// [dependencies.<name>]
    /// version = "..."
    /// ```
    pub name: &'a str,
    /// ```toml
    /// [dependencies]
    /// # either `<name>`
    /// <name> = "..."
    ///
    /// # or `<package>`
    /// explicit_name = { package = "<package>", ... }
    ///
    /// [dependencies.explicit_name]
    /// package = "<package>"
    /// ```
    pub package: &'a str,
    pub kind: DependencyKind,
    pub target: Option<&'a StringVal<'a>>,
    pub spec: DependencySpec<'a>,
    pub features: DependencyFeatures<'a>,
    /// The entire toml entry
    pub entry: &'a MapTableEntry<'a>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DependencyKind {
    Normal,
    Dev,
    Build,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DependencySpec<'a> {
    Workspace(&'a BoolVal),
    Git {
        repo: &'a StringVal<'a>,
        spec: DependencyGitSpec<'a>,
        version: Option<DependencyVersion<'a>>,
    },
    Path {
        path: &'a StringVal<'a>,
        version: Option<DependencyVersion<'a>>,
        registry: Option<&'a StringVal<'a>>,
    },
    Registry {
        version: DependencyVersion<'a>,
        registry: Option<&'a StringVal<'a>>,
    },
    /// One of the following combinations is present:
    /// - `git` and `registry`
    /// - `git` and `path`
    Conflicting,
    /// None of the following is present:
    /// - `workspace`
    /// - `path`
    /// - `git`
    /// - `version`
    ///
    /// This is still valid, but will be considered an error in future versions.
    /// Default to the latest crates.io version.
    Missing,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DependencyVersion<'a> {
    str: &'a StringVal<'a>,
    req: Option<VersionReq>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DependencyGitSpec<'a> {
    Branch(&'a StringVal<'a>),
    Tag(&'a StringVal<'a>),
    Rev(&'a StringVal<'a>),
    /// More than one of the following is present:
    /// - `branch`
    /// - `tag`
    /// - `rev`
    Conflicting,
    None,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct DependencyFeatures<'a> {
    pub default: Option<&'a BoolVal>,
    pub list: Vec<&'a StringVal<'a>>,
}

pub fn check<'a>(ctx: &mut impl IdeCtx, map: &'a MapTable<'a>) -> State<'a> {
    let mut state = State::default();
    for (key, entry) in map.iter() {
        match *key {
            // TODO
            "package" => (),
            "lib" => (),
            "bin" => (),
            "example" => (),
            "test" => (),
            "bench" => (),
            "badges" => (),
            "features" => (),
            "lints" => (),
            "patch" => (),
            "replace" => (),
            "profile" => (),
            "workspace" => (),

            "dependencies" => {
                if let Some(table) = expect_table_in_table(ctx, entry) {
                    parse_dependencies(ctx, &mut state, table, DependencyKind::Normal, None)
                }
            }
            "dev-dependencies" => {
                if let Some(table) = expect_table_in_table(ctx, entry) {
                    parse_dependencies(ctx, &mut state, table, DependencyKind::Dev, None)
                }
            }
            "dev_dependencies" => {
                const OLD: &str = "dev_dependencies";
                const NEW: &str = "dev-dependencies";
                let ignored = deprecated_underscore(ctx, map, OLD, NEW, entry);
                if !ignored {
                    if let Some(table) = expect_table_in_table(ctx, entry) {
                        parse_dependencies(ctx, &mut state, table, DependencyKind::Dev, None)
                    }
                }
            }
            "build-dependencies" => {
                if let Some(table) = expect_table_in_table(ctx, entry) {
                    parse_dependencies(ctx, &mut state, table, DependencyKind::Build, None)
                }
            }
            "build_dependencies" => {
                const OLD: &str = "build_dependencies";
                const NEW: &str = "build-dependencies";
                let ignored = deprecated_underscore(ctx, map, OLD, NEW, entry);
                if !ignored {
                    if let Some(table) = expect_table_in_table(ctx, entry) {
                        parse_dependencies(ctx, &mut state, table, DependencyKind::Build, None)
                    }
                }
            }
            "target" => todo!(),
            _ => todo!("warning unused {key}"),
        }
    }
    state
}

#[derive(Default)]
struct DependencyBuilder<'a> {
    workspace: Option<&'a BoolVal>,
    version: Option<&'a StringVal<'a>>,
    registry: Option<&'a StringVal<'a>>,
    path: Option<&'a StringVal<'a>>,
    git: Option<&'a StringVal<'a>>,
    branch: Option<&'a StringVal<'a>>,
    tag: Option<&'a StringVal<'a>>,
    rev: Option<&'a StringVal<'a>>,
}

impl<'a> DependencyBuilder<'a> {
    fn try_build(
        &self,
        ctx: &mut impl IdeCtx,
        entry: &'a MapTableEntry<'a>,
        name: &'a str,
        package: &'a str,
        features: DependencyFeatures<'a>,
        kind: DependencyKind,
        target: Option<&'a StringVal<'a>>,
    ) -> Dependency<'a> {
        let spec = 'spec: {
            if let Some(workspace) = self.workspace {
                if workspace.val == false {
                    todo!("error workspace cannot be false");
                }

                // TODO: diagnostics
                if let Some(_version) = self.version {
                    todo!("warning")
                }
                if let Some(_registry) = self.registry {
                    todo!("warning")
                }
                if let Some(_path) = self.path {
                    todo!("warning")
                }
                if let Some(_git) = self.git {
                    todo!("warning")
                }
                if let Some(_tag) = self.tag {
                    todo!("warning")
                }
                if let Some(_branch) = self.branch {
                    todo!("warning")
                }
                if let Some(_rev) = self.rev {
                    todo!("warning")
                }

                break 'spec DependencySpec::Workspace(workspace);
            }

            match (self.git, self.path, self.registry) {
                (Some(_git), Some(_path), Some(_registry)) => {
                    // TODO: error
                    DependencySpec::Conflicting
                }
                (Some(_git), _, Some(_registry)) => {
                    // TODO: error
                    DependencySpec::Conflicting
                }
                (Some(_git), Some(_path), _) => {
                    // TODO: error
                    DependencySpec::Conflicting
                }
                (Some(repo), None, None) => {
                    // TODO: diagnostics

                    let spec = match (self.branch, self.tag, self.rev) {
                        (Some(_b), Some(_t), Some(_r)) => {
                            // TODO: error
                            DependencyGitSpec::Conflicting
                        }
                        (Some(_b), Some(_t), None) => {
                            // TODO: error
                            DependencyGitSpec::Conflicting
                        }
                        (Some(_b), None, Some(_r)) => {
                            // TODO: error
                            DependencyGitSpec::Conflicting
                        }
                        (None, Some(_t), Some(_r)) => {
                            // TODO: error
                            DependencyGitSpec::Conflicting
                        }
                        (Some(branch), None, None) => DependencyGitSpec::Branch(branch),
                        (None, Some(tag), None) => DependencyGitSpec::Tag(tag),
                        (None, None, Some(rev)) => DependencyGitSpec::Rev(rev),
                        (None, None, None) => DependencyGitSpec::None,
                    };

                    let version = self.version.map(|v| parse_version_req(ctx, v));

                    DependencySpec::Git {
                        repo,
                        spec,
                        version,
                    }
                }
                (None, Some(path), registry) => {
                    // TODO: diagnostics

                    let version = self.version.map(|v| parse_version_req(ctx, v));

                    DependencySpec::Path {
                        path,
                        version,
                        registry,
                    }
                }
                (None, None, registry) => {
                    match self.version {
                        Some(version) => {
                            // TODO: diagnostics

                            let version = parse_version_req(ctx, version);
                            DependencySpec::Registry { version, registry }
                        }
                        None => {
                            // TODO: error
                            DependencySpec::Missing
                        }
                    }
                }
            }
        };

        Dependency {
            name,
            package,
            kind,
            target,
            spec,
            features,
            entry,
        }
    }
}

fn parse_dependencies<'a>(
    ctx: &mut impl IdeCtx,
    state: &mut State<'a>,
    table: &'a MapTable<'a>,
    kind: DependencyKind,
    target: Option<&'a StringVal<'a>>,
) {
    for (&name, entry) in table.iter() {
        let mut package = name;
        let mut features = DependencyFeatures::default();

        let dep = match &entry.node {
            MapNode::Scalar(Scalar::String(version)) => {
                let version = parse_version_req(ctx, version);
                let spec = DependencySpec::Registry {
                    version,
                    registry: None,
                };
                Dependency {
                    name,
                    package: name,
                    kind,
                    target,
                    spec,
                    features,
                    entry,
                }
            }
            MapNode::Scalar(_) => todo!("error"),
            MapNode::Table(t) => {
                let mut builder = DependencyBuilder::default();
                for (k, e) in t.iter() {
                    match *k {
                        "workspace" => builder.workspace = expect_bool_in_table(ctx, e),
                        "version" => builder.version = expect_string_in_table(ctx, e),
                        "registry" => builder.registry = expect_string_in_table(ctx, e),
                        "path" => builder.path = expect_string_in_table(ctx, e),
                        "git" => builder.git = expect_string_in_table(ctx, e),
                        "branch" => builder.branch = expect_string_in_table(ctx, e),
                        "tag" => builder.tag = expect_string_in_table(ctx, e),
                        "rev" => builder.rev = expect_string_in_table(ctx, e),
                        "package" => {
                            if let Some(p) = expect_string_in_table(ctx, e) {
                                package = p.text;
                            }
                        }
                        "default-features" => {
                            features.default = expect_bool_in_table(ctx, e);
                        }
                        "default_features" => {
                            const OLD: &str = "default_features";
                            const NEW: &str = "default-features";
                            let ignored = deprecated_underscore(ctx, table, OLD, NEW, e);
                            if !ignored {
                                features.default = expect_bool_in_table(ctx, e);
                            }
                        }
                        "features" => {
                            parse_dependency_features(ctx, &mut features.list, e);
                        }
                        "optional" => {
                            expect_bool_in_table(ctx, e);
                        }
                        _ => todo!("warning"),
                    };
                }

                builder.try_build(ctx, entry, name, package, features, kind, target)
            }
            MapNode::Array(_) => todo!("error"),
        };

        state.dependencies.push(dep);
    }
}

fn parse_version_req<'a>(
    ctx: &mut impl SemverCtx,
    str: &'a StringVal<'a>,
) -> DependencyVersion<'a> {
    let pos = str.text_span().start;
    let req = match semver::parse_requirement(str.lit, pos) {
        Ok(v) => Some(v),
        Err(e) => {
            ctx.error(e);
            None
        }
    };
    DependencyVersion { str, req }
}

fn parse_dependency_features<'a>(
    ctx: &mut impl IdeCtx,
    features: &mut Vec<&'a StringVal<'a>>,
    entry: &'a MapTableEntry<'a>,
) {
    let Some(array) = expect_array_in_table(ctx, entry) else {
        return;
    };
    let array = match array {
        MapArray::Toplevel(_) => todo!("error: array of tables"),
        MapArray::Inline(i) => i,
    };

    for (i, e) in array.iter().enumerate() {
        if let Some(str) = expect_string_in_array(ctx, i, e) {
            features.push(str);
        }
    }
}

fn expect_table_in_table<'a>(
    ctx: &mut impl IdeCtx,
    entry: &'a MapTableEntry<'a>,
) -> Option<&'a MapTable<'a>> {
    match &entry.node {
        MapNode::Table(a) => Some(a),
        MapNode::Scalar(Scalar::Invalid(_, _)) => None,
        n => {
            let repr = entry.reprs.first();
            let key = FmtStr::from_str(repr.key.repr_ident().text);
            let span = Span::across(repr.key.repr_ident().lit_span(), repr.kind.span());
            ctx.error(cargo::Error::WrongDatatypeInTable {
                key,
                expected: Datatype::Table,
                found: n.datatype(),
                span,
            });
            None
        }
    }
}

fn expect_array_in_table<'a>(
    ctx: &mut impl IdeCtx,
    entry: &'a MapTableEntry<'a>,
) -> Option<&'a MapArray<'a>> {
    match &entry.node {
        MapNode::Array(a) => Some(a),
        MapNode::Scalar(Scalar::Invalid(_, _)) => None,
        n => {
            let repr = entry.reprs.first();
            let key = FmtStr::from_str(repr.key.repr_ident().text);
            let span = Span::across(repr.key.repr_ident().lit_span(), repr.kind.span());
            ctx.error(cargo::Error::WrongDatatypeInTable {
                key,
                expected: Datatype::Array,
                found: n.datatype(),
                span,
            });
            None
        }
    }
}

fn expect_string_in_table<'a>(
    ctx: &mut impl IdeCtx,
    entry: &'a MapTableEntry<'a>,
) -> Option<&'a StringVal<'a>> {
    match &entry.node {
        MapNode::Scalar(Scalar::String(s)) => Some(s),
        MapNode::Scalar(Scalar::Invalid(_, _)) => None,
        n => {
            let repr = entry.reprs.first();
            let key = FmtStr::from_str(repr.key.repr_ident().text);
            let span = Span::across(repr.key.repr_ident().lit_span(), repr.kind.span());
            ctx.error(cargo::Error::WrongDatatypeInTable {
                key,
                expected: Datatype::String,
                found: n.datatype(),
                span,
            });
            None
        }
    }
}

fn expect_string_in_array<'a>(
    ctx: &mut impl IdeCtx,
    index: usize,
    entry: &'a MapArrayInlineEntry<'a>,
) -> Option<&'a StringVal<'a>> {
    match &entry.node {
        MapNode::Scalar(Scalar::String(s)) => Some(s),
        MapNode::Scalar(Scalar::Invalid(_, _)) => None,
        n => {
            ctx.error(cargo::Error::WrongDatatypeInArray {
                index,
                expected: Datatype::String,
                found: n.datatype(),
                span: entry.repr.span(),
            });
            None
        }
    }
}

fn expect_bool_in_table<'a>(
    ctx: &mut impl IdeCtx,
    entry: &'a MapTableEntry<'a>,
) -> Option<&'a BoolVal> {
    match &entry.node {
        MapNode::Scalar(Scalar::Bool(b)) => Some(b),
        MapNode::Scalar(Scalar::Invalid(_, _)) => None,
        n => {
            let repr = entry.reprs.first();
            let key = FmtStr::from_str(repr.key.repr_ident().text);
            let span = Span::across(repr.key.repr_ident().lit_span(), repr.kind.span());
            ctx.error(cargo::Error::WrongDatatypeInTable {
                key,
                expected: Datatype::Bool,
                found: n.datatype(),
                span,
            });
            None
        }
    }
}

/// Returns whether the key is ignored
fn deprecated_underscore(
    ctx: &mut impl IdeCtx,
    table: &MapTable,
    old: &'static str,
    new: &'static str,
    old_entry: &MapTableEntry,
) -> bool {
    // TODO: in the 2024 edition this becomes an error
    if let Some(new_entry) = table.get(new) {
        ctx.warn(cargo::Warning::RedundantDeprecatedUnderscore {
            old,
            new,
            old_span: old_entry.reprs.first().kind.span(),
            new_span: new_entry.reprs.first().kind.span(),
        });
        true
    } else {
        ctx.warn(cargo::Warning::DeprecatedUnderscore {
            old,
            new,
            span: old_entry.reprs.first().kind.span(),
        });
        false
    }
}
