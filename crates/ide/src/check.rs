use common::{FmtStr, Span};
use semver::{SemverCtx, VersionReq};
use toml::map::{MapArray, MapArrayInlineEntry, MapNode, MapTable, MapTableEntry, Scalar};
use toml::parse::{BoolVal, Ident, StringVal};
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
    pub package: Option<StringAssignment<'a>>,
    pub kind: DependencyKind,
    pub target: Option<&'a str>,
    pub spec: DependencySpec<'a>,
    pub features: DependencyFeatures<'a>,
    pub optional: Option<BoolAssignment<'a>>,
    /// The entire toml entry
    pub entry: &'a MapTableEntry<'a>,
}

impl<'a> Dependency<'a> {
    pub fn package(&self) -> &'a str {
        (self.package.as_ref())
            .map(|p| p.val.text)
            .unwrap_or(self.name)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DependencyKind {
    Normal,
    Dev,
    Build,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DependencySpec<'a> {
    Workspace(BoolAssignment<'a>),
    Git {
        repo: StringAssignment<'a>,
        spec: DependencyGitSpec<'a>,
        version: Option<DependencyVersion<'a>>,
    },
    Path {
        path: StringAssignment<'a>,
        version: Option<DependencyVersion<'a>>,
        registry: Option<StringAssignment<'a>>,
    },
    Registry {
        version: DependencyVersion<'a>,
        registry: Option<StringAssignment<'a>>,
    },
    /// One of the following combinations is present:
    /// - `git` and `registry`
    /// - `git` and `path`
    Conflicting,
    /// Invalid specification:
    /// - `workspace` is false
    Invalid,
    /// None of the following is present:
    /// - `workspace`
    /// - `path`
    /// - `git`
    /// - `version`
    ///
    /// This is still valid, but will be considered an error in future versions.
    /// Default to the latest crates.io version.
    Missing {
        registry: Option<StringAssignment<'a>>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DependencyVersion<'a> {
    str: StringAssignment<'a>,
    req: Option<VersionReq>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DependencyGitSpec<'a> {
    Branch(StringAssignment<'a>),
    Tag(StringAssignment<'a>),
    Rev(StringAssignment<'a>),
    /// More than one of the following is present:
    /// - `branch`
    /// - `tag`
    /// - `rev`
    Conflicting,
    None,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct DependencyFeatures<'a> {
    pub default: Option<BoolAssignment<'a>>,
    pub list: Vec<&'a StringVal<'a>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringAssignment<'a> {
    pub ident: &'a Ident<'a>,
    pub val: &'a StringVal<'a>,
}

impl<'a> StringAssignment<'a> {
    pub fn span(&self) -> Span {
        Span::new(self.ident.lit_start, self.val.lit_span.end)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BoolAssignment<'a> {
    pub ident: &'a Ident<'a>,
    pub val: &'a BoolVal,
}

impl<'a> BoolAssignment<'a> {
    pub fn span(&self) -> Span {
        Span::new(self.ident.lit_start, self.val.lit_span.end)
    }
}

pub fn check<'a>(ctx: &mut impl IdeCtx, table: &'a MapTable<'a>) -> State<'a> {
    let mut state = State::default();
    for (key, entry) in table.iter() {
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
                let ignored = deprecated_underscore(ctx, table, OLD, NEW, entry);
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
                let ignored = deprecated_underscore(ctx, table, OLD, NEW, entry);
                if !ignored {
                    if let Some(table) = expect_table_in_table(ctx, entry) {
                        parse_dependencies(ctx, &mut state, table, DependencyKind::Build, None)
                    }
                }
            }
            "target" => {
                if let Some(table) = expect_table_in_table(ctx, entry) {
                    parse_target(ctx, &mut state, table);
                }
            }
            _ => todo!("warning unused {key}"),
        }
    }
    state
}

pub fn parse_target<'a>(ctx: &mut impl IdeCtx, state: &mut State<'a>, table: &'a MapTable<'a>) {
    for (key, entry) in table.iter() {
        // TODO: validate target spec
        if let Some(table) = expect_table_in_table(ctx, entry) {
            parse_target_dependencies(ctx, state, table, key);
        }
    }
}

pub fn parse_target_dependencies<'a>(
    ctx: &mut impl IdeCtx,
    state: &mut State<'a>,
    table: &'a MapTable<'a>,
    target: &'a str,
) {
    for (key, entry) in table.iter() {
        match *key {
            "dependencies" => {
                if let Some(table) = expect_table_in_table(ctx, entry) {
                    parse_dependencies(ctx, state, table, DependencyKind::Normal, Some(target))
                }
            }
            "dev-dependencies" => {
                if let Some(table) = expect_table_in_table(ctx, entry) {
                    parse_dependencies(ctx, state, table, DependencyKind::Dev, Some(target))
                }
            }
            "dev_dependencies" => {
                const OLD: &str = "dev_dependencies";
                const NEW: &str = "dev-dependencies";
                let ignored = deprecated_underscore(ctx, table, OLD, NEW, entry);
                if !ignored {
                    if let Some(table) = expect_table_in_table(ctx, entry) {
                        parse_dependencies(ctx, state, table, DependencyKind::Dev, Some(target))
                    }
                }
            }
            "build-dependencies" => {
                if let Some(table) = expect_table_in_table(ctx, entry) {
                    parse_dependencies(ctx, state, table, DependencyKind::Build, Some(target))
                }
            }
            "build_dependencies" => {
                const OLD: &str = "build_dependencies";
                const NEW: &str = "build-dependencies";
                let ignored = deprecated_underscore(ctx, table, OLD, NEW, entry);
                if !ignored {
                    if let Some(table) = expect_table_in_table(ctx, entry) {
                        parse_dependencies(ctx, state, table, DependencyKind::Build, Some(target))
                    }
                }
            }
            _ => todo!("warning unused {key}"),
        }
    }
}

#[derive(Default)]
struct DependencyBuilder<'a> {
    package: Option<StringAssignment<'a>>,
    workspace: Option<BoolAssignment<'a>>,
    version: Option<StringAssignment<'a>>,
    registry: Option<StringAssignment<'a>>,
    path: Option<StringAssignment<'a>>,
    git: Option<StringAssignment<'a>>,
    branch: Option<StringAssignment<'a>>,
    tag: Option<StringAssignment<'a>>,
    rev: Option<StringAssignment<'a>>,
    optional: Option<BoolAssignment<'a>>,
}

impl<'a> DependencyBuilder<'a> {
    fn try_build(
        self,
        ctx: &mut impl IdeCtx,
        entry: &'a MapTableEntry<'a>,
        name: &'a str,
        features: DependencyFeatures<'a>,
        kind: DependencyKind,
        target: Option<&'a str>,
    ) -> Dependency<'a> {
        let spec = 'spec: {
            if let Some(workspace) = self.workspace {
                if workspace.val.val == false {
                    ctx.error(cargo::Error::DepWorkspaceIsFalse(workspace.span()));
                    break 'spec DependencySpec::Invalid;
                }

                let ignored = [
                    self.version.map(|v| ("version", v.span())),
                    self.registry.map(|v| ("registry", v.span())),
                    self.path.map(|v| ("path", v.span())),
                    self.git.map(|v| ("git", v.span())),
                    self.branch.map(|v| ("branch", v.span())),
                    self.tag.map(|v| ("tag", v.span())),
                    self.rev.map(|v| ("rev", v.span())),
                ];
                let workspace_span = workspace.span();
                for (key, key_span) in ignored.into_iter().flatten() {
                    ctx.warn(cargo::Warning::WorkspaceDepIgnoredKey {
                        key,
                        key_span,
                        workspace_span,
                    });
                }

                break 'spec DependencySpec::Workspace(workspace);
            }

            if self.git.is_none() {
                let ignored = [
                    self.branch.as_ref().map(|v| ("branch", v.span())),
                    self.tag.as_ref().map(|v| ("tag", v.span())),
                    self.rev.as_ref().map(|v| ("rev", v.span())),
                ];
                for (key, span) in ignored.into_iter().flatten() {
                    ctx.error(cargo::Error::DepIgnoredGitKey(key, span));
                }
            }

            match (self.git, self.path, self.registry) {
                (Some(git), _, Some(registry)) => {
                    ctx.error(cargo::Error::AmbigousDepSpecGitRegistry(git.span()));
                    ctx.error(cargo::Error::AmbigousDepSpecGitRegistry(registry.span()));
                    DependencySpec::Conflicting
                }
                (Some(git), Some(path), _) => {
                    ctx.error(cargo::Error::AmbigousDepSpecGitPath(git.span()));
                    ctx.error(cargo::Error::AmbigousDepSpecGitPath(path.span()));
                    DependencySpec::Conflicting
                }
                (Some(repo), None, None) => {
                    let num = self.branch.is_some() as u8
                        + self.tag.is_some() as u8
                        + self.rev.is_some() as u8;
                    let spec = if num > 1 {
                        if let Some(b) = &self.branch {
                            ctx.error(cargo::Error::AmbigousGitSpec(b.span()));
                        }
                        if let Some(t) = &self.tag {
                            ctx.error(cargo::Error::AmbigousGitSpec(t.span()));
                        }
                        if let Some(r) = &self.rev {
                            ctx.error(cargo::Error::AmbigousGitSpec(r.span()));
                        }
                        DependencyGitSpec::Conflicting
                    } else if let Some(branch) = self.branch {
                        DependencyGitSpec::Branch(branch)
                    } else if let Some(tag) = self.tag {
                        DependencyGitSpec::Tag(tag)
                    } else if let Some(rev) = self.rev {
                        DependencyGitSpec::Rev(rev)
                    } else {
                        DependencyGitSpec::None
                    };
                    let version = self.version.map(|v| parse_version_req(ctx, v));

                    DependencySpec::Git {
                        repo,
                        spec,
                        version,
                    }
                }
                (None, Some(path), registry) => {
                    let version = self.version.map(|v| parse_version_req(ctx, v));

                    DependencySpec::Path {
                        path,
                        version,
                        registry,
                    }
                }
                (None, None, registry) => {
                    if let Some(version) = self.version {
                        let version = parse_version_req(ctx, version);
                        DependencySpec::Registry { version, registry }
                    } else {
                        for repr in entry.reprs.iter() {
                            // TODO: in the 2024 edition this becomes an error
                            ctx.warn(cargo::Warning::MissingDepSpec(repr.repr_span()));
                        }
                        DependencySpec::Missing { registry }
                    }
                }
            }
        };

        Dependency {
            name,
            package: self.package,
            kind,
            target,
            spec,
            features,
            optional: self.optional,
            entry,
        }
    }
}

fn parse_dependencies<'a>(
    ctx: &mut impl IdeCtx,
    state: &mut State<'a>,
    table: &'a MapTable<'a>,
    kind: DependencyKind,
    target: Option<&'a str>,
) {
    for (&name, entry) in table.iter() {
        let mut features = DependencyFeatures::default();

        let dep = match &entry.node {
            MapNode::Scalar(Scalar::String(val)) => {
                let ident = entry.reprs.first().key.repr_ident();
                let str = StringAssignment { ident, val };
                let version = parse_version_req(ctx, str);
                let spec = DependencySpec::Registry {
                    version,
                    registry: None,
                };
                Dependency {
                    name,
                    package: None,
                    kind,
                    target,
                    spec,
                    features,
                    optional: None,
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
                        "package" => builder.package = expect_string_in_table(ctx, e),
                        "optional" => builder.optional = expect_bool_in_table(ctx, e),
                        "default-features" => features.default = expect_bool_in_table(ctx, e),
                        "default_features" => {
                            const OLD: &str = "default_features";
                            const NEW: &str = "default-features";
                            let ignored = deprecated_underscore(ctx, table, OLD, NEW, e);
                            if !ignored {
                                features.default = expect_bool_in_table(ctx, e);
                            }
                        }
                        "features" => parse_dependency_features(ctx, &mut features.list, e),
                        _ => {
                            for repr in e.reprs.iter() {
                                ctx.warn(cargo::Warning::DepIgnoredUnknownKey(
                                    FmtStr::from_str(k),
                                    repr.repr_span(),
                                ));
                            }
                        }
                    };
                }

                builder.try_build(ctx, entry, name, features, kind, target)
            }
            MapNode::Array(_) => todo!("error"),
        };

        state.dependencies.push(dep);
    }
}

fn parse_version_req<'a>(
    ctx: &mut impl SemverCtx,
    str: StringAssignment<'a>,
) -> DependencyVersion<'a> {
    let pos = str.val.text_span().start;
    // TODO: spans from the semver parser may be incorrect when escape sequences are used for ascii characters
    let req = match semver::parse_requirement(str.val.text, pos) {
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
            ctx.error(cargo::Error::WrongDatatypeInTable {
                key,
                expected: Datatype::Table,
                found: n.datatype(),
                span: repr.repr_span(),
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
            ctx.error(cargo::Error::WrongDatatypeInTable {
                key,
                expected: Datatype::Array,
                found: n.datatype(),
                span: repr.repr_span(),
            });
            None
        }
    }
}

fn expect_string_in_table<'a>(
    ctx: &mut impl IdeCtx,
    entry: &'a MapTableEntry<'a>,
) -> Option<StringAssignment<'a>> {
    match &entry.node {
        MapNode::Scalar(Scalar::String(val)) => {
            let ident = entry.reprs.first().key.repr_ident();
            Some(StringAssignment { ident, val })
        }
        MapNode::Scalar(Scalar::Invalid(_, _)) => None,
        n => {
            let repr = entry.reprs.first();
            let key = FmtStr::from_str(repr.key.repr_ident().text);
            ctx.error(cargo::Error::WrongDatatypeInTable {
                key,
                expected: Datatype::String,
                found: n.datatype(),
                span: repr.repr_span(),
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
) -> Option<BoolAssignment<'a>> {
    match &entry.node {
        MapNode::Scalar(Scalar::Bool(val)) => {
            let ident = entry.reprs.first().key.repr_ident();
            Some(BoolAssignment { ident, val })
        }
        MapNode::Scalar(Scalar::Invalid(_, _)) => None,
        n => {
            let repr = entry.reprs.first();
            let key = FmtStr::from_str(repr.key.repr_ident().text);
            ctx.error(cargo::Error::WrongDatatypeInTable {
                key,
                expected: Datatype::Bool,
                found: n.datatype(),
                span: repr.repr_span(),
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
