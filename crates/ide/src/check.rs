use common::{FmtStr, Span};
use semver::{SemverCtx, VersionReq};
use toml::map::{
    self, MapArray, MapArrayInlineEntry, MapNode, MapTable, MapTableEntry, MapTableEntryRepr,
    ParentId, Scalar,
};
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

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
pub struct DependencyVersion<'a> {
    str: StringAssignment<'a>,
    req: Option<VersionReq>,
}

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, Default, PartialEq)]
pub struct DependencyFeatures<'a> {
    pub default: Option<BoolAssignment<'a>>,
    pub list: Vec<&'a StringVal<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StringAssignment<'a> {
    pub repr: &'a MapTableEntryRepr<'a>,
    pub val: &'a StringVal<'a>,
}

impl<'a> StringAssignment<'a> {
    pub fn ident(&self) -> &'a Ident<'a> {
        self.repr.key.repr_ident()
    }

    pub fn span(&self) -> Span {
        self.repr.repr_span()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BoolAssignment<'a> {
    pub repr: &'a MapTableEntryRepr<'a>,
    pub val: &'a BoolVal,
}

impl<'a> BoolAssignment<'a> {
    pub fn ident(&self) -> &'a Ident<'a> {
        self.repr.key.repr_ident()
    }

    pub fn span(&self) -> Span {
        self.repr.repr_span()
    }
}

pub fn check<'a>(ctx: &mut impl IdeCtx, table: &'a MapTable<'a>) -> State<'a> {
    let mut state = State::default();
    for (key, entry) in table.iter() {
        let path = map::Path::root(&entry.reprs);
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
                if let Some(table) = expect_table_in_table(ctx, &path, entry) {
                    parse_dependencies(ctx, &mut state, &path, table, DependencyKind::Normal, None)
                }
            }
            "dev-dependencies" => {
                if let Some(table) = expect_table_in_table(ctx, &path, entry) {
                    parse_dependencies(ctx, &mut state, &path, table, DependencyKind::Dev, None)
                }
            }
            "dev_dependencies" => {
                const OLD: &str = "dev_dependencies";
                const NEW: &str = "dev-dependencies";
                let ignored = deprecated_underscore(ctx, None, table, OLD, NEW, entry);
                if !ignored {
                    if let Some(table) = expect_table_in_table(ctx, &path, entry) {
                        parse_dependencies(ctx, &mut state, &path, table, DependencyKind::Dev, None)
                    }
                }
            }
            "build-dependencies" => {
                if let Some(table) = expect_table_in_table(ctx, &path, entry) {
                    parse_dependencies(ctx, &mut state, &path, table, DependencyKind::Build, None)
                }
            }
            "build_dependencies" => {
                const OLD: &str = "build_dependencies";
                const NEW: &str = "build-dependencies";
                let ignored = deprecated_underscore(ctx, None, table, OLD, NEW, entry);
                if !ignored {
                    if let Some(table) = expect_table_in_table(ctx, &path, entry) {
                        parse_dependencies(
                            ctx,
                            &mut state,
                            &path,
                            table,
                            DependencyKind::Build,
                            None,
                        )
                    }
                }
            }
            "target" => {
                if let Some(table) = expect_table_in_table(ctx, &path, entry) {
                    parse_target(ctx, &mut state, &path, table);
                }
            }
            _ => warn_unused(ctx, &path, &entry),
        }
    }
    state
}

pub fn parse_target<'a>(
    ctx: &mut impl IdeCtx,
    state: &mut State<'a>,
    path: &map::Path<'a, '_>,
    table: &'a MapTable<'a>,
) {
    for (key, entry) in table.iter() {
        let path = path.append_key(&entry.reprs);
        // TODO: validate target spec
        if let Some(table) = expect_table_in_table(ctx, &path, entry) {
            parse_target_dependencies(ctx, state, &path, table, key);
        }
    }
}

pub fn parse_target_dependencies<'a>(
    ctx: &mut impl IdeCtx,
    state: &mut State<'a>,
    path: &map::Path<'a, '_>,
    table: &'a MapTable<'a>,
    target: &'a str,
) {
    for (key, entry) in table.iter() {
        let path = path.append_key(&entry.reprs);
        match *key {
            "dependencies" => {
                if let Some(table) = expect_table_in_table(ctx, &path, entry) {
                    parse_dependencies(
                        ctx,
                        state,
                        &path,
                        table,
                        DependencyKind::Normal,
                        Some(target),
                    )
                }
            }
            "dev-dependencies" => {
                if let Some(table) = expect_table_in_table(ctx, &path, entry) {
                    parse_dependencies(ctx, state, &path, table, DependencyKind::Dev, Some(target))
                }
            }
            "dev_dependencies" => {
                const OLD: &str = "dev_dependencies";
                const NEW: &str = "dev-dependencies";
                let ignored = deprecated_underscore(ctx, path.prev, table, OLD, NEW, entry);
                if !ignored {
                    if let Some(table) = expect_table_in_table(ctx, &path, entry) {
                        parse_dependencies(
                            ctx,
                            state,
                            &path,
                            table,
                            DependencyKind::Dev,
                            Some(target),
                        )
                    }
                }
            }
            "build-dependencies" => {
                if let Some(table) = expect_table_in_table(ctx, &path, entry) {
                    parse_dependencies(
                        ctx,
                        state,
                        &path,
                        table,
                        DependencyKind::Build,
                        Some(target),
                    )
                }
            }
            "build_dependencies" => {
                const OLD: &str = "build_dependencies";
                const NEW: &str = "build-dependencies";
                let ignored = deprecated_underscore(ctx, path.prev, table, OLD, NEW, entry);
                if !ignored {
                    if let Some(table) = expect_table_in_table(ctx, &path, entry) {
                        parse_dependencies(
                            ctx,
                            state,
                            &path,
                            table,
                            DependencyKind::Build,
                            Some(target),
                        )
                    }
                }
            }
            _ => warn_unused(ctx, &path, entry),
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
        path: &map::Path,
        entry: &'a MapTableEntry<'a>,
        name: &'a str,
        features: DependencyFeatures<'a>,
        kind: DependencyKind,
        target: Option<&'a str>,
    ) -> Dependency<'a> {
        let spec = 'spec: {
            if let Some(workspace) = self.workspace {
                if workspace.val.val == false {
                    ctx.error(cargo::Error::new(
                        path.context_lines([workspace.repr.parent]),
                        path.fmt_path(),
                        workspace.span(),
                        cargo::ErrorKind::DepWorkspaceIsFalse,
                    ));
                    break 'spec DependencySpec::Invalid;
                }

                let ignored = [
                    self.version.map(|v| v.repr),
                    self.registry.map(|v| v.repr),
                    self.path.map(|v| v.repr),
                    self.git.map(|v| v.repr),
                    self.branch.map(|v| v.repr),
                    self.tag.map(|v| v.repr),
                    self.rev.map(|v| v.repr),
                ];
                let workspace_span = workspace.span();
                for repr in ignored.into_iter().flatten() {
                    ctx.warn(cargo::Warning::new(
                        path.context_lines([repr.parent]),
                        path.joined_path(repr.key.repr_ident()),
                        repr.repr_span(),
                        cargo::WarningKind::WorkspaceDepIgnoredKey { workspace_span },
                    ));
                }

                break 'spec DependencySpec::Workspace(workspace);
            }

            if self.git.is_none() {
                let ignored = [
                    ("branch", &self.branch),
                    ("tag", &self.tag),
                    ("rev", &self.rev),
                ];
                for (key, assignment) in ignored {
                    if let Some(a) = assignment {
                        ctx.error(cargo::Error::new(
                            path.context_lines([a.repr.parent]),
                            path.fmt_path(),
                            a.span(),
                            cargo::ErrorKind::DepIgnoredGitKey(key),
                        ));
                    }
                }
            }

            match (self.git, self.path, self.registry) {
                (Some(git), _, Some(registry)) => {
                    ctx.error(cargo::Error::new(
                        path.context_lines([git.repr.parent]),
                        path.fmt_path(),
                        git.span(),
                        cargo::ErrorKind::AmbigousDepSpecGitRegistry,
                    ));
                    ctx.error(cargo::Error::new(
                        path.context_lines([registry.repr.parent]),
                        path.fmt_path(),
                        registry.span(),
                        cargo::ErrorKind::AmbigousDepSpecGitRegistry,
                    ));
                    DependencySpec::Conflicting
                }
                (Some(git), Some(path_key), _) => {
                    ctx.error(cargo::Error::new(
                        path.context_lines([git.repr.parent]),
                        path.fmt_path(),
                        git.span(),
                        cargo::ErrorKind::AmbigousDepSpecGitRegistry,
                    ));
                    ctx.error(cargo::Error::new(
                        path.context_lines([path_key.repr.parent]),
                        path.fmt_path(),
                        path_key.span(),
                        cargo::ErrorKind::AmbigousDepSpecGitRegistry,
                    ));
                    DependencySpec::Conflicting
                }
                (Some(repo), None, None) => {
                    let num = self.branch.is_some() as u8
                        + self.tag.is_some() as u8
                        + self.rev.is_some() as u8;
                    let spec = if num > 1 {
                        for key in [&self.branch, &self.tag, &self.rev] {
                            if let Some(key) = key {
                                ctx.error(cargo::Error::new(
                                    path.context_lines([key.repr.parent]),
                                    path.fmt_path(),
                                    key.span(),
                                    cargo::ErrorKind::AmbigousGitSpec,
                                ));
                            }
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
                            ctx.warn(cargo::Warning::new(
                                map::context_lines(path.prev, [repr.parent]),
                                path.fmt_path(),
                                repr.repr_span(),
                                cargo::WarningKind::MissingDepSpec,
                            ));
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
    path: &map::Path<'a, '_>,
    table: &'a MapTable<'a>,
    kind: DependencyKind,
    target: Option<&'a str>,
) {
    for (&name, entry) in table.iter() {
        let path = path.append_key(&entry.reprs);
        let mut features = DependencyFeatures::default();

        let dep = match &entry.node {
            MapNode::Scalar(Scalar::String(val)) => {
                let repr = entry.reprs.first();
                let str = StringAssignment { repr, val };
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
            MapNode::Table(table) => {
                let mut builder = DependencyBuilder::default();
                parse_dependency(ctx, &mut builder, &mut features, &path, table);
                builder.try_build(ctx, &path, entry, name, features, kind, target)
            }
            MapNode::Array(_) => todo!("error"),
        };

        state.dependencies.push(dep);
    }
}

fn parse_dependency<'a>(
    ctx: &mut impl IdeCtx,
    builder: &mut DependencyBuilder<'a>,
    features: &mut DependencyFeatures<'a>,
    path: &map::Path<'a, '_>,
    table: &'a MapTable<'a>,
) {
    for (key, entry) in table.iter() {
        let path = path.append_key(&entry.reprs);
        match *key {
            "workspace" => builder.workspace = expect_bool_in_table(ctx, &path, entry),
            "version" => builder.version = expect_string_in_table(ctx, &path, entry),
            "registry" => builder.registry = expect_string_in_table(ctx, &path, entry),
            "path" => builder.path = expect_string_in_table(ctx, &path, entry),
            "git" => builder.git = expect_string_in_table(ctx, &path, entry),
            "branch" => builder.branch = expect_string_in_table(ctx, &path, entry),
            "tag" => builder.tag = expect_string_in_table(ctx, &path, entry),
            "rev" => builder.rev = expect_string_in_table(ctx, &path, entry),
            "package" => builder.package = expect_string_in_table(ctx, &path, entry),
            "optional" => builder.optional = expect_bool_in_table(ctx, &path, entry),
            "default-features" => features.default = expect_bool_in_table(ctx, &path, entry),
            "default_features" => {
                const OLD: &str = "default_features";
                const NEW: &str = "default-features";
                let ignored = deprecated_underscore(ctx, path.prev, table, OLD, NEW, entry);
                if !ignored {
                    features.default = expect_bool_in_table(ctx, &path, entry);
                }
            }
            "features" => parse_dependency_features(ctx, &mut features.list, &path, entry),
            _ => warn_unused(ctx, &path, entry),
        };
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
    path: &map::Path,
    entry: &'a MapTableEntry<'a>,
) {
    let Some(array) = expect_array_in_table(ctx, path, entry) else {
        return;
    };
    let array = match array {
        MapArray::Toplevel(_) => todo!("error: array of tables"),
        MapArray::Inline(i) => i,
    };

    for (i, entry) in array.iter().enumerate() {
        let path = path.append_index(i);
        if let Some(str) = expect_string_in_array(ctx, &path, array.parent, entry) {
            features.push(str);
        }
    }
}

fn expect_table_in_table<'a>(
    ctx: &mut impl IdeCtx,
    path: &map::Path,
    entry: &'a MapTableEntry<'a>,
) -> Option<&'a MapTable<'a>> {
    match &entry.node {
        MapNode::Table(a) => Some(a),
        MapNode::Scalar(Scalar::Invalid(_, _)) => None,
        n => {
            for repr in entry.reprs.iter() {
                ctx.error(wrong_datatype(path, n, repr, Datatype::Table));
            }
            None
        }
    }
}

fn expect_array_in_table<'a>(
    ctx: &mut impl IdeCtx,
    path: &map::Path,
    entry: &'a MapTableEntry<'a>,
) -> Option<&'a MapArray<'a>> {
    match &entry.node {
        MapNode::Array(a) => Some(a),
        MapNode::Scalar(Scalar::Invalid(_, _)) => None,
        n => {
            for repr in entry.reprs.iter() {
                ctx.error(wrong_datatype(path, n, repr, Datatype::Array));
            }
            None
        }
    }
}

fn expect_string_in_table<'a>(
    ctx: &mut impl IdeCtx,
    path: &map::Path,
    entry: &'a MapTableEntry<'a>,
) -> Option<StringAssignment<'a>> {
    match &entry.node {
        MapNode::Scalar(Scalar::String(val)) => {
            let repr = entry.reprs.first();
            Some(StringAssignment { repr, val })
        }
        MapNode::Scalar(Scalar::Invalid(_, _)) => None,
        n => {
            for repr in entry.reprs.iter() {
                ctx.error(wrong_datatype(path, n, repr, Datatype::String));
            }
            None
        }
    }
}

fn expect_string_in_array<'a>(
    ctx: &mut impl IdeCtx,
    path: &map::Path,
    parent: ParentId,
    entry: &'a MapArrayInlineEntry<'a>,
) -> Option<&'a StringVal<'a>> {
    match &entry.node {
        MapNode::Scalar(Scalar::String(s)) => Some(s),
        MapNode::Scalar(Scalar::Invalid(_, _)) => None,
        n => {
            ctx.error(cargo::Error::new(
                map::context_lines(path.prev, [parent]),
                path.fmt_path(),
                entry.repr.span(),
                cargo::ErrorKind::WrongDatatype {
                    expected: Datatype::String,
                    found: n.datatype(),
                },
            ));
            None
        }
    }
}

fn expect_bool_in_table<'a>(
    ctx: &mut impl IdeCtx,
    path: &map::Path,
    entry: &'a MapTableEntry<'a>,
) -> Option<BoolAssignment<'a>> {
    match &entry.node {
        MapNode::Scalar(Scalar::Bool(val)) => {
            let repr = entry.reprs.first();
            Some(BoolAssignment { repr, val })
        }
        MapNode::Scalar(Scalar::Invalid(_, _)) => None,
        n => {
            for repr in entry.reprs.iter() {
                ctx.error(wrong_datatype(path, n, repr, Datatype::Bool));
            }
            None
        }
    }
}

fn warn_unused(ctx: &mut impl IdeCtx, path: &map::Path, entry: &MapTableEntry) {
    for repr in entry.reprs.iter() {
        ctx.warn(cargo::Warning::new(
            map::context_lines(path.prev, [repr.parent]),
            path.fmt_path(),
            repr.repr_span(),
            cargo::WarningKind::IgnoredUnknownKey,
        ));
    }
}

/// Returns whether the key is ignored
fn deprecated_underscore(
    ctx: &mut impl IdeCtx,
    path: Option<&map::Path>,
    table: &MapTable,
    old: &'static str,
    new: &'static str,
    old_entry: &MapTableEntry,
) -> bool {
    let ignored;
    // TODO: in the 2024 edition this becomes an error
    let kind = if let Some(new_entry) = table.get(new) {
        ignored = true;
        cargo::WarningKind::RedundantDeprecatedUnderscore {
            old,
            new,
            new_span: new_entry.reprs.first().kind.span(),
        }
    } else {
        ignored = false;
        cargo::WarningKind::DeprecatedUnderscore { old, new }
    };

    for repr in old_entry.reprs.iter() {
        ctx.warn(cargo::Warning::new(
            map::context_lines(path, [repr.parent]),
            path.map_or(FmtStr::empty(), map::Path::fmt_path),
            repr.repr_span(),
            kind.clone(),
        ));
    }

    ignored
}

fn wrong_datatype(
    path: &map::Path,
    node: &MapNode,
    repr: &MapTableEntryRepr,
    expected: Datatype,
) -> cargo::Error {
    cargo::Error::new(
        map::context_lines(path.prev, [repr.parent]),
        path.fmt_path(),
        repr.repr_span(),
        cargo::ErrorKind::WrongDatatype {
            expected,
            found: node.datatype(),
        },
    )
}
