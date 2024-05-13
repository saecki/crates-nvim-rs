use toml::map::{
    MapArray, MapArrayInlineEntry, MapNode, MapTable, MapTableEntry, MapTableEntryRepr,
    MapTableEntryReprKind, Scalar,
};
use toml::onevec::OneVec;
use toml::parse::{InlineArrayValue, StringVal};

use crate::error::CargoError;
use crate::CargoCtx;

#[derive(Clone, Default, PartialEq)]
pub struct State<'a> {
    dependencies: Vec<Dependency<'a>>,
}

pub struct Dependency<'a> {
    pub kind: DependencyKind,
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

pub fn check<'a>(ctx: &mut impl CargoCtx, map: &'a MapTable<'a>) -> State<'a> {
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
                    DependencyKind::Default,
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

fn parse_dependencies<'a>(
    ctx: &mut impl CargoCtx,
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
                let syntax = match &repr.kind {
                    MapTableEntryReprKind::Table(_) => todo!(),
                    MapTableEntryReprKind::ArrayEntry(_) => todo!("error"),
                    MapTableEntryReprKind::ToplevelAssignment(_) => todo!(),
                    MapTableEntryReprKind::InlineTableAssignment(_) => todo!(),
                };
                let mut builder = CrateBuilder::new(
                    Range::from_span_cols(repr.key.repr_ident().lit_span()),
                    Range::from_span_lines(repr.kind.span()),
                    syntax,
                    todo!("section"),
                );
                for (k, e) in t.iter() {
                    match *k {
                        "version" => {
                            if let Some(s) = expect_string_in_table(ctx, e) {
                                todo!();
                            }
                        }
                        "registry" => todo!(),
                        "path" => todo!(),
                        "git" => todo!(),
                        "branch" => todo!(),
                        "rev" => todo!(),
                        "package" => todo!(),
                        "default-features" => todo!(),
                        "default_features" => todo!("warning or error"),
                        "features" => builder.feat = parse_dependency_features(ctx, e),
                        "workspace" => todo!(),
                        "optional" => todo!(),
                        _ => todo!("warning"),
                    }
                }

                match builder.try_build(ctx) {
                    Some(c) => c,
                    None => continue,
                }
            }
            MapNode::Array(_) => todo!("error"),
        };

        state.dependencies.push(dep);
    }
}

fn parse_dependency_features<'a>(
    ctx: &mut impl CargoCtx,
    entry: &'a MapTableEntry<'a>,
) -> Vec<DependencyFeature<'a>> {
    let array = expect_array_in_table(ctx, entry)?;
    let features = match array {
        MapArray::Toplevel(_) => todo!("error: array of tables"),
        MapArray::Inline(i) => i,
    };

    let features_span = features.repr.span();
    if features_span.start.line != features_span.end.line {
        todo!("multiline arrays")
    }

    let mut items = Vec::with_capacity(features.len());
    for (i, e) in features.iter().enumerate() {
        let Some(f) = expect_string_in_array(ctx, e) else {
            continue;
        };

        let decl_start_col = (i.checked_sub(1))
            .map(|i| features[i].repr.span().end.char)
            .unwrap_or_else(|| features.repr.l_par.char + 1);

        let decl_end_col = (e.repr.comma.map(|p| p.char))
            .or_else(|| features.get(i + 1).map(|f| f.repr.span().start.char))
            .or_else(|| features.repr.r_par.map(|p| p.char))
            .unwrap_or(features.repr.span().end.char);

        items.push(TomlFeature {
            name: f.text.to_string(),
            col: Range::from_span_cols(f.text_span()),
            decl_col: Range::new(decl_start_col, decl_end_col),
            quote: Quotes {
                s: f.l_quote().to_string(),
                e: f.r_quote().map(ToString::to_string),
            },
            comma: e.repr.comma.is_some(),
        });
    }

    let line = features_span.start.line;
    let col = Range::from_span_cols(features_span);
    let decl_col = Range::from_span_cols(entry.reprs.first().kind.span());

    let text = lines[line as usize].as_ref()[col.s as usize..col.e as usize].to_string();

    Some(TomlCrateFeat {
        items,
        text,
        line,
        col,
        decl_col,
    })
}

fn expect_array_in_table<'a>(
    ctx: &mut impl CargoCtx,
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
    ctx: &mut impl CargoCtx,
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
    ctx: &mut impl CargoCtx,
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
