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
    Path(&'a StringVal<'a>),
    Git {
        repo: &'a StringVal<'a>,
        spec: DependencyGitSpec<'a>,
    },
    Version {
        version: &'a StringVal<'a>,
        registry: Option<&'a StringVal<'a>>,
        req: Option<VersionReq>,
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
    Missing,
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
            "build-dependencies" => {
                if let Some(table) = expect_table_in_table(ctx, entry) {
                    parse_dependencies(ctx, &mut state, table, DependencyKind::Build, None)
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
        let spec = if let Some(workspace) = self.workspace {
            if workspace.val == false {
                todo!("error workspace cannot be false");
            }

            // TODO: diagnostics
            if self.version.is_some() {
                todo!("warning")
            }
            if self.registry.is_some() {
                todo!("warning")
            }
            if self.path.is_some() {
                todo!("warning")
            }
            if self.git.is_some() {
                todo!("warning")
            }
            if self.rev.is_some() {
                todo!("warning")
            }

            DependencySpec::Workspace(workspace)
        } else if let Some(path) = self.path {
            // TODO: diagnostics

            DependencySpec::Path(path)
        } else if let Some(repo) = self.git {
            // TODO: diagnostics

            let spec = if let Some(branch) = self.branch {
                if let Some(tag) = self.tag {
                    todo!("error {tag:?}")
                }
                if let Some(rev) = self.rev {
                    todo!("error {rev:?}")
                }

                DependencyGitSpec::Branch(branch)
            } else if let Some(tag) = self.tag {
                if let Some(rev) = self.rev {
                    todo!("error {rev:?}")
                }

                DependencyGitSpec::Tag(tag)
            } else if let Some(rev) = self.rev {
                DependencyGitSpec::Rev(rev)
            } else {
                DependencyGitSpec::None
            };

            DependencySpec::Git { repo, spec }
        } else if let Some(version) = self.version {
            // TODO: diagnostics
            if self.path.is_some() {
                todo!("warning")
            }
            if self.git.is_some() {
                todo!("warning")
            }
            if self.rev.is_some() {
                todo!("warning")
            }

            let req = parse_version_req(ctx, version);
            DependencySpec::Version {
                version,
                registry: self.registry,
                req,
            }
        } else {
            // TODO warning (error in future versions)
            DependencySpec::Missing
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
                let req = parse_version_req(ctx, version);
                let spec = DependencySpec::Version {
                    version,
                    registry: None,
                    req,
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
                            // should take predendence over `default_features`
                            features.default = expect_bool_in_table(ctx, e);
                        }
                        "default_features" => {
                            let old = "default_features";
                            let new = "default-features";
                            if let Some(n) = t.get(new) {
                                ctx.warn(cargo::Warning::RedundantDeprecatedUnderscore {
                                    old,
                                    new,
                                    old_span: e.reprs.first().kind.span(),
                                    new_span: n.reprs.first().kind.span(),
                                });
                            } else {
                                ctx.warn(cargo::Warning::DeprecatedUnderscore {
                                    old,
                                    new,
                                    span: e.reprs.first().kind.span(),
                                });
                            }
                            // TODO: warning or hint

                            // `default-features` takes predendence
                            if features.default.is_none() {
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

fn parse_version_req(ctx: &mut impl SemverCtx, str: &StringVal) -> Option<VersionReq> {
    let pos = str.text_span().start;
    match semver::parse_requirement(str.lit, pos) {
        Ok(v) => Some(v),
        Err(e) => {
            ctx.error(e);
            None
        }
    }
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
