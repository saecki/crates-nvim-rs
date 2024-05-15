use common::Span;
use semver::VersionReq;
use toml::map::{
    MapArray, MapArrayInlineEntry, MapNode, MapTable, MapTableEntry, MapTableEntryRepr, Scalar,
};
use toml::onevec::OneVec;
use toml::parse::{BoolVal, InlineArrayValue, StringVal};

use crate::error::CargoError;
use crate::NvimCtx;

#[derive(Debug, Default, PartialEq)]
pub struct State<'a> {
    dependencies: Vec<Dependency<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Dependency<'a> {
    pub kind: DependencyKind,
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
    pub workspace: Option<&'a BoolVal>,
    pub registry: Option<&'a StringVal<'a>>,
    pub version: Option<(&'a StringVal<'a>, Option<VersionReq>)>,
    pub path: Option<&'a str>,
    pub git: Option<&'a str>,
    pub rev: Option<&'a str>,
    pub entry: MapTableEntry<'a>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DependencyKind {
    Normal,
    Dev,
    Build,
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
                MapNode::Table(table) => parse_dependencies(
                    ctx,
                    &mut state,
                    &entry.reprs,
                    table,
                    DependencyKind::Normal,
                    None,
                ),
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
    version: Option<(&'a StringVal<'a>, Option<VersionReq>)>,
    registry: Option<&'a StringVal<'a>>,
    path: Option<&'a StringVal<'a>>,
    git: Option<&'a StringVal<'a>>,
    rev: Option<&'a StringVal<'a>>,
}

impl<'a> DependencyBuilder<'a> {
    fn try_build(&self, ctx: &mut impl NvimCtx) -> Option<Dependency<'a>> {
        todo!()
    }
}

fn parse_dependencies<'a>(
    ctx: &mut impl NvimCtx,
    state: &mut State<'a>,
    table_reprs: &'a OneVec<MapTableEntryRepr<'a>>,
    table: &'a MapTable<'a>,
    kind: DependencyKind,
    target: Option<&'a str>,
) {
    for (crate_name, entry) in table.iter() {
        let dep = match &entry.node {
            MapNode::Scalar(Scalar::String(version)) => {
                todo!()
            }
            MapNode::Scalar(_) => todo!("error"),
            MapNode::Table(t) => {
                let repr = entry.reprs.first();
                let mut builder = DependencyBuilder::default();
                for (k, e) in t.iter() {
                    match *k {
                        "version" => {
                            if let Some(s) = expect_string_in_table(ctx, e) {
                                let pos = s.text_span().start;
                                let req = match semver::parse_requirement(&s.lit, pos) {
                                    Ok(v) => Some(v),
                                    Err(e) => {
                                        ctx.error(e);
                                        None
                                    }
                                };

                                builder.version = Some((s, req));
                            }
                        }
                        "registry" => {
                            expect_string_in_table(ctx, e);
                        }
                        "path" => todo!(),
                        "git" => todo!(),
                        "branch" => todo!(),
                        "rev" => todo!(),
                        "package" => todo!(),
                        "default-features" => todo!(),
                        "default_features" => todo!("warning or error"),
                        "features" => {
                            check_dependency_features(ctx, e);
                        }
                        "workspace" => todo!(),
                        "optional" => todo!(),
                        _ => todo!("warning"),
                    };
                }

                match builder.try_build(ctx) {
                    Some(d) => d,
                    None => continue,
                }
            }
            MapNode::Array(_) => todo!("error"),
        };

        state.dependencies.push(dep);
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
