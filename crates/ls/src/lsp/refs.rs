use std::str::FromStr;

use common::{Pos, Span};
use toml::parse::{Assignment, Ident, Key, Value};
use toml::{Ast, Toml, Toplevel};

use crate::edit::{OffsetEncoding, SpanExt};

pub fn references(
    toml: &Toml,
    pos: Pos,
    encoding: OffsetEncoding,
) -> Option<Vec<lsp_types::Location>> {
    let Refs::Ident(ident) = find_refs(&toml.ast, pos)?;

    let entry = ident.mapped()?.get();

    let refs = entry.reprs.iter().map(|repr| {
        let span = repr.key.repr_ident().lit_span();
        let range = span.to_lsp_range(&toml.ast.source, encoding);
        let uri = {
            // FIXME: store VfsPath in source
            let uri = format!("file://{}", toml.ast.source.path);
            lsp_types::Url::from_str(&uri).expect("source path to be valid")
        };
        lsp_types::Location::new(uri, range)
    });

    Some(refs.collect())
}

pub fn document_highlight(
    toml: &Toml,
    pos: Pos,
    encoding: OffsetEncoding,
) -> Option<Vec<lsp_types::DocumentHighlight>> {
    let Refs::Ident(ident) = find_refs(&toml.ast, pos)?;

    let entry = ident.mapped()?.get();

    let highlights = entry.reprs.iter().map(|repr| {
        let span = repr.key.repr_ident().lit_span();
        let range = span.to_lsp_range(&toml.ast.source, encoding);
        lsp_types::DocumentHighlight {
            range,
            kind: Some(lsp_types::DocumentHighlightKind::WRITE),
        }
    });

    Some(highlights.collect())
}

enum Refs<'a> {
    Ident(&'a Ident<'a>),
}

fn find_refs<'a>(ast: &Ast<'a>, pos: Pos) -> Option<Refs<'a>> {
    let t = binary_search(ast.toplevel, pos, |t| t.span()).left_bias()?;

    let assignments = match t {
        Toplevel::Root(assignment) => assignment,
        Toplevel::Table(table) => {
            if let Some(key) = &table.header.key
                && let Some(ident) = find_refs_in_key(key, pos)
            {
                return Some(ident);
            }

            &table.assignments
        }
        Toplevel::Array(array_entry) => {
            if let Some(key) = &array_entry.header.key
                && let Some(ident) = find_refs_in_key(key, pos)
            {
                return Some(ident);
            }

            &array_entry.assignments
        }
    };

    let assignment = binary_search(assignments, pos, |a| a.assignment.span()).select(|a, b| {
        if b.assignment.key.first().kind.is_plain() {
            b
        } else {
            a
        }
    })?;
    find_refs_in_assignment(&assignment.assignment, pos)
}

fn find_refs_in_assignment<'a>(assignment: &'a Assignment<'a>, pos: Pos) -> Option<Refs<'a>> {
    find_refs_in_key(&assignment.key, pos).or_else(|| find_refs_in_value(&assignment.val, pos))
}

fn find_refs_in_key<'a>(key: &'a Key<'a>, pos: Pos) -> Option<Refs<'a>> {
    match key {
        Key::One(ident) => find_refs_in_ident(ident, pos),
        Key::Dotted(idents) => idents
            .iter()
            .find_map(|i| find_refs_in_ident(&i.ident, pos)),
    }
}

fn find_refs_in_value<'a>(val: &'a Value<'a>, pos: Pos) -> Option<Refs<'a>> {
    match val {
        Value::String(_) => None, // TODO: features references
        Value::Int(_) => None,
        Value::Float(_) => None,
        Value::Bool(_) => None,
        Value::DateTime(_) => None,
        Value::InlineTable(inline_table) => {
            let assignment = binary_search(&inline_table.assignments, pos, |a| a.assignment.span())
                .select(|a, b| {
                    if b.assignment.key.first().kind.is_plain() {
                        b
                    } else {
                        a
                    }
                })?;
            find_refs_in_assignment(&assignment.assignment, pos)
        }
        Value::InlineArray(inline_array) => {
            let val = binary_search(&inline_array.values, pos, |v| v.val.span()).right_bias()?;
            find_refs_in_value(&val.val, pos)
        }
        Value::Invalid(_) => None,
    }
}

fn find_refs_in_ident<'a>(ident: &'a Ident<'a>, pos: Pos) -> Option<Refs<'a>> {
    end_inclusive_span_cmp(ident.lit_span(), pos)
        .is_eq()
        .then_some(ident)
        .map(Refs::Ident)
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum SearchResult<T> {
    None,
    One(T),
    Between(T, T),
}

impl<T> SearchResult<T> {
    fn select(self, select: impl FnOnce(T, T) -> T) -> Option<T> {
        match self {
            SearchResult::None => None,
            SearchResult::One(v) => Some(v),
            SearchResult::Between(a, b) => Some(select(a, b)),
        }
    }

    /// If between two results, select the left one.
    fn left_bias(self) -> Option<T> {
        match self {
            SearchResult::None => None,
            SearchResult::One(v) => Some(v),
            SearchResult::Between(a, _) => Some(a),
        }
    }

    /// If between two results, select the right one.
    fn right_bias(self) -> Option<T> {
        match self {
            SearchResult::None => None,
            SearchResult::One(v) => Some(v),
            SearchResult::Between(_, b) => Some(b),
        }
    }
}

fn binary_search<T>(items: &[T], pos: Pos, span_fn: impl Fn(&T) -> Span) -> SearchResult<&T> {
    let res = items.binary_search_by(|item| {
        let span = span_fn(item);
        end_inclusive_span_cmp(span, pos)
    });
    let Ok(idx) = res else {
        return SearchResult::None;
    };

    let item = &items[idx];

    let item_span = span_fn(item);
    if item_span.start == pos
        && let Some(prev_idx) = idx.checked_sub(1)
        && let Some(prev) = items.get(prev_idx)
        && span_fn(prev).end == pos
    {
        return SearchResult::Between(prev, item);
    }

    if item_span.end == pos
        && let Some(next) = items.get(idx + 1)
        && span_fn(next).start == pos
    {
        return SearchResult::Between(item, next);
    }

    SearchResult::One(item)
}

/// This does an inclusive range check, so if the cursor is at the end of the
/// identifier, this will return [`std::cmp::Equal`]. Even though [`Span`] is
/// usually considered an exclusive range, this is the expected behavior for
/// most editors.
fn end_inclusive_span_cmp(span: Span, cursor: Pos) -> std::cmp::Ordering {
    if span.end < cursor {
        std::cmp::Ordering::Less
    } else if span.start > cursor {
        std::cmp::Ordering::Greater
    } else {
        std::cmp::Ordering::Equal
    }
}
