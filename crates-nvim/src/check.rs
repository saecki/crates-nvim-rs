use common::Span;
use semver::{SemverCtx, VersionReq};
use toml::map::{MapArray, MapArrayInlineEntry, MapNode, MapTable, MapTableEntry, Scalar};
use toml::parse::{BoolVal, InlineArrayValue, StringVal};

use crate::error::CargoError;
use crate::NvimCtx;

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
        // TODO: add GitSpec enum?
        branch: Option<&'a StringVal<'a>>,
        rev: Option<&'a StringVal<'a>>,
    },
    Version {
        version: &'a StringVal<'a>,
        registry: Option<&'a StringVal<'a>>,
        req: Option<VersionReq>,
    },
    /// None of the following is present
    /// - `workspace`
    /// - `path`
    /// - `git`
    /// - `version`
    Missing,
}

pub struct DependencyFeature<'a> {
    pub repr: InlineArrayValue<'a>,
}

pub fn check<'a>(ctx: &mut impl NvimCtx, map: &'a MapTable<'a>) -> State<'a> {
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

            "dependencies" => match &entry.node {
                MapNode::Table(table) => {
                    parse_dependencies(ctx, &mut state, table, DependencyKind::Normal, None)
                }
                _ => ctx.error(CargoError::ExpectedTable(
                    key.to_string(),
                    entry.reprs.first().key.repr_ident().lit_span(),
                )),
            },
            "dev-dependencies" => todo!(),
            "build-dependencies" => todo!(),
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
    rev: Option<&'a StringVal<'a>>,
}

impl<'a> DependencyBuilder<'a> {
    fn try_build(
        &self,
        ctx: &mut impl NvimCtx,
        entry: &'a MapTableEntry<'a>,
        name: &'a str,
        package: &'a str,
        kind: DependencyKind,
        target: Option<&'a StringVal<'a>>,
    ) -> Dependency<'a> {
        let spec = if let Some(workspace) = self.workspace {
            if workspace.val == false {
                todo!("error");
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
        } else if let Some(git) = self.git {
            // TODO: diagnostics

            DependencySpec::Git {
                repo: git,
                branch: self.branch,
                rev: self.rev,
            }
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
            entry,
        }
    }
}

fn parse_dependencies<'a>(
    ctx: &mut impl NvimCtx,
    state: &mut State<'a>,
    table: &'a MapTable<'a>,
    kind: DependencyKind,
    target: Option<&'a StringVal<'a>>,
) {
    for (&name, entry) in table.iter() {
        let mut package = name;

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
                    entry,
                }
            }
            MapNode::Scalar(_) => todo!("error"),
            MapNode::Table(t) => {
                let mut builder = DependencyBuilder::default();
                for (k, e) in t.iter() {
                    match *k {
                        "workspace" => builder.workspace = expect_bool_in_table(ctx, entry),
                        "version" => builder.version = expect_string_in_table(ctx, e),
                        "registry" => builder.registry = expect_string_in_table(ctx, e),
                        "path" => builder.path = expect_string_in_table(ctx, e),
                        "git" => builder.git = expect_string_in_table(ctx, e),
                        "branch" => builder.branch = expect_string_in_table(ctx, e),
                        "rev" => builder.rev = expect_string_in_table(ctx, e),
                        "package" => {
                            if let Some(p) = expect_string_in_table(ctx, entry) {
                                package = p.text;
                            }
                        }
                        "default-features" => todo!(),
                        "default_features" => todo!("warning or hint"),
                        "features" => {
                            check_dependency_features(ctx, e);
                        }
                        "optional" => {
                            expect_bool_in_table(ctx, entry);
                        }
                        _ => todo!("warning"),
                    };
                }

                builder.try_build(ctx, entry, name, package, kind, target)
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

fn check_dependency_features<'a>(ctx: &mut impl NvimCtx, entry: &'a MapTableEntry<'a>) {
    let Some(array) = expect_array_in_table(ctx, entry) else {
        return;
    };
    let features = match array {
        MapArray::Toplevel(_) => todo!("error: array of tables"),
        MapArray::Inline(i) => i,
    };

    let features_span = features.repr.span();
    if features_span.start.line != features_span.end.line {
        todo!("multiline arrays")
    }

    for e in features.iter() {
        expect_string_in_array(ctx, e);
    }
}

fn expect_array_in_table<'a>(
    ctx: &mut impl NvimCtx,
    entry: &'a MapTableEntry<'a>,
) -> Option<&'a MapArray<'a>> {
    match &entry.node {
        MapNode::Array(a) => Some(a),
        _ => {
            let repr = entry.reprs.first();
            let key = repr.key.repr_ident().text.to_string();
            let span = Span::across(repr.key.repr_ident().lit_span(), repr.kind.span());
            ctx.error(CargoError::ExpectedArrayInTable(key, span));
            None
        }
    }
}

fn expect_string_in_table<'a>(
    ctx: &mut impl NvimCtx,
    entry: &'a MapTableEntry<'a>,
) -> Option<&'a StringVal<'a>> {
    match &entry.node {
        MapNode::Scalar(Scalar::String(s)) => Some(s),
        _ => {
            let repr = entry.reprs.first();
            let key = repr.key.repr_ident().text.to_string();
            let span = Span::across(repr.key.repr_ident().lit_span(), repr.kind.span());
            ctx.error(CargoError::ExpectedStringInTable(key, span));
            None
        }
    }
}

fn expect_string_in_array<'a>(
    ctx: &mut impl NvimCtx,
    entry: &'a MapArrayInlineEntry<'a>,
) -> Option<&'a StringVal<'a>> {
    match &entry.node {
        MapNode::Scalar(Scalar::String(s)) => Some(s),
        _ => {
            ctx.error(CargoError::ExpectedStringInArray(entry.repr.span()));
            None
        }
    }
}

fn expect_bool_in_table<'a>(
    ctx: &mut impl NvimCtx,
    entry: &'a MapTableEntry<'a>,
) -> Option<&'a BoolVal> {
    match &entry.node {
        MapNode::Scalar(Scalar::Bool(b)) => Some(b),
        _ => {
            let repr = entry.reprs.first();
            let key = repr.key.repr_ident().text.to_string();
            let span = Span::across(repr.key.repr_ident().lit_span(), repr.kind.span());
            ctx.error(CargoError::ExpectedBoolInTable(key, span));
            None
        }
    }
}
