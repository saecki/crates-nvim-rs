//! ## Structure
//! - [MapTable] // root
//!     - entries:
//!         - "children_1": [MapTableEntry]
//!             - reprs: [OneVec<MapTableEntryRepr>]
//!                 - parent: [ParentId] // refers to root (nothing to index into)
//!                 - key: ...
//!                 - kind: ...
//!             - node: [MapNode::Table]: [MapTable]
//!                 - entries:
//!                     - "node_1": [MapTableEntry]
//!                         - node: [MapNode::Scalar]
//!                         - reprs: [OneVec<MapTableEntryRepr>]
//!                             - parent: [ParentId] // index into children_1
//!                             - key: ...
//!                             - kind: ...
//!                     - "node_2": MapTableEntry
//!                         - node: [MapNode::Scalar]
//!                         - reprs: [OneVec<MapTableEntryRepr>]
//!                             - parent: [ParentId] // index into children_1
//!                             - key: ...
//!                             - kind: ...
//!
//! ## Possible representations of the above
//! ```toml
//! # 1
//! [children_1]
//! node_1 = 1
//! node_2 = false
//!
//! # 2
//! children_1.node_1 = 1
//! children_1.node_2 = false
//!
//! # 3
//! children_1 = { node_1 = 1, node_2 = false }
//! ```

use std::collections::hash_map::Entry::*;
use std::collections::HashMap;

use bumpalo::Bump;
use common::{FmtChar, FmtStr, Span};

use crate::onevec::OneVec;
use crate::parse::{
    ArrayEntry, Ast, BoolVal, DateTimeVal, DottedIdent, FloatVal, Ident, InlineArray,
    InlineArrayValue, InlineTableAssignment, IntVal, Key, StringVal, Table, ToplevelAssignment,
    Value,
};
use crate::{Asts, Error, TomlCtx};

#[cfg(test)]
mod test;

// The id is irrelevant, since the caller won't have any [`MapTableEntry::reprs`]
// array to index anyway, but this will most likely panic if used wrong.
const ROOT_PARENT: ParentId = ParentId(u32::MAX);

#[derive(Debug, Default, PartialEq)]
pub struct MapTable<'a> {
    inner: HashMap<&'a str, MapTableEntry<'a>>,
}

impl<'a> MapTable<'a> {
    pub fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    pub fn get(&self, key: &'a str) -> Option<&MapTableEntry<'a>> {
        self.inner.get(key)
    }

    pub fn from_pairs(pairs: impl IntoIterator<Item = (&'a str, MapTableEntry<'a>)>) -> Self {
        Self {
            inner: HashMap::from_iter(pairs),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&&str, &MapTableEntry<'a>)> {
        self.inner.iter()
    }
}

impl<'a> IntoIterator for MapTable<'a> {
    type Item = (&'a str, MapTableEntry<'a>);
    type IntoIter = std::collections::hash_map::IntoIter<&'a str, MapTableEntry<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

#[derive(Debug, PartialEq)]
pub struct MapTableEntry<'a> {
    pub node: MapNode<'a>,
    /// References to the actual representations inside the toml file.
    pub reprs: OneVec<MapTableEntryRepr<'a>>,
}

impl<'a> MapTableEntry<'a> {
    fn new(node: MapNode<'a>, reprs: OneVec<MapTableEntryRepr<'a>>) -> Self {
        Self { node, reprs }
    }

    fn from_one(node: MapNode<'a>, repr: MapTableEntryRepr<'a>) -> Self {
        Self {
            node,
            reprs: OneVec::new(repr),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MapTableEntryRepr<'a> {
    /// Index of the parent defined in the parent [`MapTableEntry::reprs`].
    pub parent: ParentId,
    pub key: MapTableKeyRepr<'a>,
    pub kind: MapTableEntryReprKind<'a>,
}

impl<'a> MapTableEntryRepr<'a> {
    fn new(parent: ParentId, key: MapTableKeyRepr<'a>, kind: MapTableEntryReprKind<'a>) -> Self {
        Self { parent, key, kind }
    }

    /// The span from the [`MapTableKeyRepr::repr_ident`] to the end of the value.
    pub fn repr_span(&self) -> Span {
        Span::new(self.key.repr_ident().lit_start, self.kind.span().end)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParentId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MapTableEntryReprKind<'a> {
    Table(&'a Table<'a>),
    ArrayEntry(&'a ArrayEntry<'a>),
    ToplevelAssignment(&'a ToplevelAssignment<'a>),
    InlineTableAssignment(&'a InlineTableAssignment<'a>),
}

impl MapTableEntryReprKind<'_> {
    #[inline]
    pub fn span(&self) -> Span {
        match self {
            MapTableEntryReprKind::Table(t) => t.span(),
            MapTableEntryReprKind::ArrayEntry(a) => a.span(),
            MapTableEntryReprKind::ToplevelAssignment(a) => a.span(),
            MapTableEntryReprKind::InlineTableAssignment(a) => a.span(),
        }
    }

    #[inline]
    pub fn is_assignment(&self) -> bool {
        match self {
            MapTableEntryReprKind::Table(_) => false,
            MapTableEntryReprKind::ArrayEntry(_) => false,
            MapTableEntryReprKind::ToplevelAssignment(_) => true,
            MapTableEntryReprKind::InlineTableAssignment(_) => true,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MapTableKeyRepr<'a> {
    One(&'a Ident<'a>),
    Dotted(u32, &'a [DottedIdent<'a>]),
}

impl<'a> MapTableKeyRepr<'a> {
    /// The [`Ident`] responsible for declaring a table, might be an identifier in a dotted key.
    pub fn repr_ident(&self) -> &'a Ident<'a> {
        match self {
            MapTableKeyRepr::One(i) => i,
            MapTableKeyRepr::Dotted(idx, idents) => &idents[*idx as usize].ident,
        }
    }

    pub fn is_last_ident(&self) -> bool {
        match self {
            MapTableKeyRepr::One(_) => true,
            MapTableKeyRepr::Dotted(idx, idents) => *idx as usize == idents.len() - 1,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum MapArray<'a> {
    Toplevel(MapArrayToplevel<'a>),
    Inline(MapArrayInline<'a>),
}

#[derive(Debug, PartialEq)]
pub struct MapArrayToplevel<'a> {
    inner: OneVec<MapArrayToplevelEntry<'a>>,
}

impl<'a> MapArrayToplevel<'a> {
    pub fn new(node: MapTable<'a>, parent: ParentId, repr: &'a ArrayEntry<'a>) -> Self {
        Self {
            inner: OneVec::new(MapArrayToplevelEntry::new(node, parent, repr)),
        }
    }

    fn push(&mut self, entry: MapArrayToplevelEntry<'a>) {
        self.inner.push(entry);
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.len() == 0
    }
}

impl<'a> IntoIterator for MapArrayToplevel<'a> {
    type Item = MapArrayToplevelEntry<'a>;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

#[derive(Debug, PartialEq)]
pub struct MapArrayToplevelEntry<'a> {
    pub node: MapTable<'a>,
    pub parent: ParentId,
    pub repr: &'a ArrayEntry<'a>,
}

impl<'a> MapArrayToplevelEntry<'a> {
    pub fn new(node: MapTable<'a>, parent: ParentId, repr: &'a ArrayEntry<'a>) -> Self {
        Self { node, parent, repr }
    }
}

#[derive(Debug, PartialEq)]
pub struct MapArrayInline<'a> {
    pub parent: ParentId,
    pub repr: &'a InlineArray<'a>,
    inner: Box<[MapArrayInlineEntry<'a>]>,
}

impl<'a> MapArrayInline<'a> {
    pub fn new(parent: ParentId, repr: &'a InlineArray<'a>) -> Self {
        Self {
            repr,
            parent,
            inner: Box::new([]),
        }
    }

    pub fn from_iter<T>(parent: ParentId, repr: &'a InlineArray<'a>, iter: T) -> Self
    where
        T: IntoIterator<Item = MapArrayInlineEntry<'a>>,
        <T as IntoIterator>::IntoIter: ExactSizeIterator<Item = MapArrayInlineEntry<'a>>,
    {
        Self {
            parent,
            repr,
            inner: iter.into_iter().collect(),
        }
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.len() == 0
    }

    pub fn get(&'a self, idx: usize) -> Option<&'a MapArrayInlineEntry<'a>> {
        self.inner.get(idx)
    }

    pub fn iter(&'a self) -> impl Iterator<Item = &'a MapArrayInlineEntry<'a>> {
        self.inner.iter()
    }
}

impl<'a> IntoIterator for MapArrayInline<'a> {
    type Item = MapArrayInlineEntry<'a>;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_vec().into_iter()
    }
}

impl<'a, I: std::slice::SliceIndex<[MapArrayInlineEntry<'a>]>> std::ops::Index<I>
    for MapArrayInline<'a>
{
    type Output = I::Output;

    #[inline]
    fn index(&self, index: I) -> &Self::Output {
        std::ops::Index::index(&*self.inner, index)
    }
}

impl<'a, I: std::slice::SliceIndex<[MapArrayInlineEntry<'a>]>> std::ops::IndexMut<I>
    for MapArrayInline<'a>
{
    #[inline]
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        std::ops::IndexMut::index_mut(&mut *self.inner, index)
    }
}

#[derive(Debug, PartialEq)]
pub struct MapArrayInlineEntry<'a> {
    pub node: MapNode<'a>,
    pub repr: &'a InlineArrayValue<'a>,
}

impl<'a> MapArrayInlineEntry<'a> {
    pub fn new(node: MapNode<'a>, repr: &'a InlineArrayValue<'a>) -> Self {
        Self { node, repr }
    }
}

#[derive(Debug, PartialEq)]
pub enum MapNode<'a> {
    Table(MapTable<'a>),
    Array(MapArray<'a>),
    Scalar(Scalar<'a>),
}

#[derive(Debug, PartialEq)]
pub enum Scalar<'a> {
    String(&'a StringVal<'a>),
    Int(&'a IntVal<'a>),
    Float(&'a FloatVal<'a>),
    Bool(&'a BoolVal),
    DateTime(&'a DateTimeVal<'a>),
    Invalid(&'a str, Span),
}

impl Scalar<'_> {
    #[inline]
    pub fn span(&self) -> Span {
        match self {
            Scalar::String(s) => s.lit_span,
            Scalar::Int(i) => i.lit_span,
            Scalar::Float(f) => f.lit_span,
            Scalar::Bool(b) => b.lit_span,
            Scalar::DateTime(d) => d.lit_span,
            Scalar::Invalid(_, span) => *span,
        }
    }
}

/// Linked list of path segments with parent span information, mainly used for diagnostics.
#[derive(Clone)]
pub struct Path<'a, 'b> {
    pub prev: Option<&'b Path<'a, 'b>>,
    pub segment: PathSegment<'a, 'b>,
}

#[derive(Clone)]
pub enum PathSegment<'a, 'b> {
    Table(&'b OneVec<MapTableEntryRepr<'a>>),
    Array(usize),
}

impl<'a, 'b> Path<'a, 'b> {
    #[inline(always)]
    pub fn root(reprs: &'b OneVec<MapTableEntryRepr<'a>>) -> Self {
        append_key(None, reprs)
    }

    #[inline(always)]
    pub fn append_key(&'b self, reprs: &'b OneVec<MapTableEntryRepr<'a>>) -> Self {
        append_key(Some(self), reprs)
    }

    #[inline(always)]
    pub fn append_index(&'b self, index: usize) -> Self {
        append_index(Some(self), index)
    }

    pub fn joined_path(&self, ident: &Ident) -> FmtStr {
        joined_path(Some(self), ident)
    }

    pub fn fmt_path(&self) -> FmtStr {
        let path = fmt_path(&self);
        FmtStr::from_string(path)
    }

    pub fn context_lines<const LEN: usize>(&self, parents: [ParentId; LEN]) -> Box<[u32]> {
        let mut lines = Vec::new();
        for p in parents {
            collect_lines(&mut lines, self, p);
        }
        lines.sort();
        lines.dedup();
        lines.into_boxed_slice()
    }
}

#[inline(always)]
pub fn append_key<'a, 'b>(
    prev: Option<&'b Path<'a, 'b>>,
    reprs: &'b OneVec<MapTableEntryRepr<'a>>,
) -> Path<'a, 'b> {
    Path {
        prev,
        segment: PathSegment::Table(reprs),
    }
}

#[inline(always)]
pub fn append_index<'a, 'b>(prev: Option<&'b Path<'a, 'b>>, index: usize) -> Path<'a, 'b> {
    Path {
        prev,
        segment: PathSegment::Array(index),
    }
}

fn fmt_path(path: &Path) -> String {
    match path.prev {
        Some(prev) => {
            use std::fmt::Write as _;
            let mut buf = fmt_path(prev);
            match path.segment {
                PathSegment::Table(reprs) => {
                    let key = reprs.first().key.repr_ident();
                    buf.push('.');
                    fmt_path_segment(&mut buf, key).unwrap();
                }
                PathSegment::Array(i) => write!(&mut buf, "[{i}]").unwrap(),
            }
            buf
        }
        None => match path.segment {
            PathSegment::Table(reprs) => {
                let mut buf = String::new();
                let key = reprs.first().key.repr_ident();
                fmt_path_segment(&mut buf, key).unwrap();
                buf
            }
            PathSegment::Array(_) => unreachable!(),
        },
    }
}

pub fn joined_path(prev: Option<&Path>, key: &Ident) -> FmtStr {
    let str = match prev {
        Some(prev) => {
            let mut buf = fmt_path(prev);
            buf.push('.');
            fmt_path_segment(&mut buf, key).unwrap();
            buf
        }
        None => {
            let mut buf = String::new();
            fmt_path_segment(&mut buf, key).unwrap();
            buf
        }
    };
    FmtStr::from_string(str)
}

fn fmt_path_segment(f: &mut impl std::fmt::Write, key: &Ident) -> std::fmt::Result {
    if key.text.is_empty() {
        f.write_str("''")?;
    } else {
        let is_invalid_plain_ident =
            (key.text.chars()).any(|c| !matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'));
        if is_invalid_plain_ident {
            f.write_char('\'')?;
            for c in key.text.chars() {
                write!(f, "{}", FmtChar(c))?;
            }
            f.write_char('\'')?;
        } else {
            f.write_str(key.text)?;
        }
    }
    Ok(())
}

pub fn context_lines<const LEN: usize>(
    path: Option<&Path>,
    parents: [ParentId; LEN],
) -> Box<[u32]> {
    let Some(path) = path else {
        return Box::new([]);
    };

    let mut lines = Vec::new();
    for p in parents {
        collect_lines(&mut lines, path, p);
    }
    lines.sort();
    lines.dedup();
    lines.into_boxed_slice()
}

fn collect_lines(lines: &mut Vec<u32>, mut path: &Path, mut parent: ParentId) {
    loop {
        match path.segment {
            PathSegment::Table(reprs) => {
                let repr = &reprs[parent.0 as usize];
                lines.push(repr.key.repr_ident().lit_start.line);
                parent = repr.parent;
            }
            PathSegment::Array(_) => (),
        }

        let Some(prev) = path.prev else { break };
        path = prev;
    }
}

/// Value to be lazily mapped and inserted
enum InsertValue<'a> {
    Value(&'a Value<'a>),
    TableAssignments(&'a [ToplevelAssignment<'a>]),
}

pub fn map<'a>(ctx: &mut impl TomlCtx, asts: &'_ Asts<'a>) -> MapTable<'a> {
    let mut root = MapTable::new();
    let mut bump = Bump::new();
    for a in asts.asts.iter() {
        match a {
            Ast::Assignment(assignment) => {
                let repr_kind = MapTableEntryReprKind::ToplevelAssignment(assignment);
                insert_node_at_path(
                    ctx,
                    &bump,
                    None,
                    ROOT_PARENT,
                    &mut root,
                    &assignment.assignment.key,
                    InsertValue::Value(&assignment.assignment.val),
                    repr_kind,
                );
            }
            Ast::Table(table) => {
                let Some(key) = &table.header.key else {
                    continue;
                };

                let repr_kind = MapTableEntryReprKind::Table(table);
                insert_node_at_path(
                    ctx,
                    &bump,
                    None,
                    ROOT_PARENT,
                    &mut root,
                    key,
                    InsertValue::TableAssignments(&table.assignments),
                    repr_kind,
                );
            }
            Ast::Array(array_entry) => {
                let Some(key) = &array_entry.header.key else {
                    continue;
                };
                insert_array_entry_at_path(ctx, &bump, &mut root, key, array_entry);
            }
            Ast::Comment(_) => (),
        }
        bump.reset();
    }
    root
}

fn map_insert_value<'a, 'b>(
    ctx: &mut impl TomlCtx,
    bump: &'b Bump,
    path: &'b Path<'a, 'b>,
    parent: ParentId,
    value: InsertValue<'a>,
) -> MapNode<'a> {
    match value {
        InsertValue::Value(value) => map_value(ctx, bump, path, parent, value),
        InsertValue::TableAssignments(assignments) => {
            let mut map = MapTable::new();
            insert_top_level_assignments(ctx, bump, Some(path), parent, &mut map, assignments);
            MapNode::Table(map)
        }
    }
}

fn map_value<'a, 'b>(
    ctx: &mut impl TomlCtx,
    bump: &'b Bump,
    path: &'b Path<'a, 'b>,
    parent: ParentId,
    value: &'a Value<'a>,
) -> MapNode<'a> {
    match value {
        Value::String(s) => MapNode::Scalar(Scalar::String(s)),
        Value::Int(i) => MapNode::Scalar(Scalar::Int(i)),
        Value::Float(f) => MapNode::Scalar(Scalar::Float(f)),
        Value::Bool(b) => MapNode::Scalar(Scalar::Bool(b)),
        Value::DateTime(d) => MapNode::Scalar(Scalar::DateTime(d)),
        Value::InlineTable(table) => {
            let mut map = MapTable::new();
            for assignment in table.assignments.iter() {
                let repr_kind = MapTableEntryReprKind::InlineTableAssignment(assignment);
                insert_node_at_path(
                    ctx,
                    bump,
                    Some(path),
                    parent,
                    &mut map,
                    &assignment.assignment.key,
                    InsertValue::Value(&assignment.assignment.val),
                    repr_kind,
                );
            }
            MapNode::Table(map)
        }
        Value::InlineArray(inline_array) => {
            let entries = (inline_array.values.iter().enumerate()).map(|(index, value)| {
                let path = append_index(Some(path), index);
                let node = map_value(ctx, bump, &path, parent, &value.val);
                MapArrayInlineEntry::new(node, value)
            });
            let array = MapArrayInline::from_iter(parent, inline_array, entries);
            MapNode::Array(MapArray::Inline(array))
        }
        Value::Invalid(s, r) => MapNode::Scalar(Scalar::Invalid(s, *r)),
    }
}

fn insert_node_at_path<'a, 'b>(
    ctx: &mut impl TomlCtx,
    bump: &'b Bump,
    mut path: Option<&'b Path<'a, 'b>>,
    mut parent: ParentId,
    map: &'b mut MapTable<'a>,
    key: &'a Key<'a>,
    value: InsertValue<'a>,
    repr_kind: MapTableEntryReprKind<'a>,
) {
    let idents = match key {
        Key::One(i) => {
            let key_repr = MapTableKeyRepr::One(i);
            let repr = MapTableEntryRepr::new(parent, key_repr, repr_kind);
            let res = insert_node(ctx, bump, path, map, i, value, repr);
            if let Err(e) = res {
                ctx.error(e);
            }
            return;
        }
        Key::Dotted(idents) => idents,
    };

    let [other @ .., last] = idents else {
        unreachable!()
    };
    let mut current = map;
    for (i, o) in other.iter().enumerate() {
        let entry = match current.inner.entry(o.ident.text) {
            Occupied(occupied) => occupied.into_mut(),
            Vacant(mut vacant) => {
                for j in i..idents.len() - 1 {
                    let key_repr = MapTableKeyRepr::Dotted(j as u32, idents);
                    let repr = MapTableEntryRepr::new(parent, key_repr, repr_kind);
                    let node = MapNode::Table(MapTable::new());
                    let entry = vacant.insert(MapTableEntry::from_one(node, repr));

                    parent = ParentId(0);
                    path = Some(bump.alloc(append_key(path, &entry.reprs)));

                    let MapNode::Table(next) = &mut entry.node else {
                        unreachable!()
                    };
                    vacant = match next.inner.entry(idents[j + 1].ident.text) {
                        Occupied(_) => unreachable!(),
                        Vacant(vacant) => vacant,
                    };
                }

                let key_repr = MapTableKeyRepr::Dotted((idents.len() - 1) as u32, idents);
                let repr = MapTableEntryRepr::new(parent, key_repr, repr_kind);
                let reprs = OneVec::new(repr);

                parent = ParentId(0);
                let path = append_key(path, &reprs);

                let node = map_insert_value(ctx, bump, &path, parent, value);
                vacant.insert(MapTableEntry::new(node, reprs));

                return;
            }
        };

        let key_repr = MapTableKeyRepr::Dotted(i as u32, idents);
        let repr = MapTableEntryRepr::new(parent, key_repr, repr_kind);
        match get_table_to_extend(bump, path, entry, repr) {
            Ok((next_parent, next_path, next)) => {
                parent = next_parent;
                path = next_path;
                current = next;
            }
            Err(e) => {
                ctx.error(e);
                return;
            }
        };
    }

    let key_repr = MapTableKeyRepr::Dotted((idents.len() - 1) as u32, idents);
    let repr = MapTableEntryRepr::new(parent, key_repr, repr_kind);

    let res = insert_node(ctx, bump, path, current, &last.ident, value, repr);
    if let Err(e) = res {
        ctx.error(e);
    }
}

fn insert_node<'a, 'b>(
    ctx: &mut impl TomlCtx,
    bump: &'b Bump,
    path: Option<&'b Path<'a, 'b>>,
    map: &mut MapTable<'a>,
    key: &'a Ident<'a>,
    value: InsertValue<'a>,
    repr: MapTableEntryRepr<'a>,
) -> Result<(), Error> {
    use std::collections::hash_map::Entry::*;

    let existing_entry = match map.inner.entry(key.text) {
        Occupied(occupied) => occupied.into_mut(),
        Vacant(vacant) => {
            // no previous entries in this chain -> this will be the first index
            let parent = ParentId(0);
            let reprs = OneVec::new(repr);
            let path = append_key(path, &reprs);
            let node = map_insert_value(ctx, bump, &path, parent, value);
            vacant.insert(MapTableEntry::new(node, reprs));
            return Ok(());
        }
    };

    let InsertValue::TableAssignments(assignments) = value else {
        return Err(duplicate_key_error(
            path,
            existing_entry.reprs.first(),
            &repr,
        ));
    };
    let existing_table = match &mut existing_entry.node {
        MapNode::Table(t) => t,
        MapNode::Array(_) | MapNode::Scalar(_) => {
            return Err(duplicate_key_error(
                path,
                existing_entry.reprs.first(),
                &repr,
            ));
        }
    };
    for existing_repr in existing_entry.reprs.iter() {
        match existing_repr.kind {
            MapTableEntryReprKind::Table(_) | MapTableEntryReprKind::ArrayEntry(_)
                if !existing_repr.key.is_last_ident() =>
            {
                // allow super tables, that are declared out of order
            }
            MapTableEntryReprKind::Table(_)
            | MapTableEntryReprKind::ArrayEntry(_)
            | MapTableEntryReprKind::ToplevelAssignment(_)
            | MapTableEntryReprKind::InlineTableAssignment(_) => {
                return Err(duplicate_key_error(path, existing_repr, &repr));
            }
        }
    }

    // extend existing table with items from super table
    let parent = insert_repr(&mut existing_entry.reprs, repr);
    let path = append_key(path, &existing_entry.reprs);
    insert_top_level_assignments(ctx, bump, Some(&path), parent, existing_table, assignments);

    Ok(())
}

fn insert_array_entry_at_path<'a, 'b>(
    ctx: &mut impl TomlCtx,
    bump: &'b Bump,
    map: &'b mut MapTable<'a>,
    key: &'a Key<'a>,
    array_entry: &'a ArrayEntry<'a>,
) {
    let mut parent = ROOT_PARENT;
    let mut path = None;
    let idents = match key {
        Key::One(i) => {
            let key_repr = MapTableKeyRepr::One(i);
            let res = insert_array_entry(ctx, bump, path, map, parent, i, key_repr, array_entry);
            if let Err(e) = res {
                ctx.error(e);
            }
            return;
        }
        Key::Dotted(idents) => idents,
    };

    let [other @ .., last] = idents else {
        unreachable!()
    };
    let mut current = map;
    for (i, o) in other.iter().enumerate() {
        let entry = match current.inner.entry(o.ident.text) {
            Occupied(occupied) => occupied.into_mut(),
            Vacant(mut vacant) => {
                for j in i..idents.len() - 1 {
                    let key_repr = MapTableKeyRepr::Dotted(j as u32, idents);
                    let repr_kind = MapTableEntryReprKind::ArrayEntry(array_entry);
                    let repr = MapTableEntryRepr::new(parent, key_repr, repr_kind);
                    let node = MapNode::Table(MapTable::new());
                    let entry = vacant.insert(MapTableEntry::from_one(node, repr));

                    parent = ParentId(0);
                    path = Some(bump.alloc(append_key(path, &entry.reprs)));

                    let MapNode::Table(next) = &mut entry.node else {
                        unreachable!()
                    };
                    vacant = match next.inner.entry(idents[j + 1].ident.text) {
                        Occupied(_) => unreachable!(),
                        Vacant(vacant) => vacant,
                    };
                }

                let key_repr = MapTableKeyRepr::Dotted((idents.len() - 1) as u32, idents);
                let repr_kind = MapTableEntryReprKind::ArrayEntry(array_entry);
                let repr = MapTableEntryRepr::new(parent, key_repr, repr_kind);
                let reprs = OneVec::new(repr);

                parent = ParentId(0);
                let path = append_key(path, &reprs);
                let path = path.append_index(0);

                let mut node = MapTable::new();
                insert_top_level_assignments(
                    ctx,
                    bump,
                    Some(&path),
                    parent,
                    &mut node,
                    &array_entry.assignments,
                );
                let toplevel_array = MapArrayToplevel::new(node, parent, array_entry);
                let node = MapNode::Array(MapArray::Toplevel(toplevel_array));

                vacant.insert(MapTableEntry::new(node, reprs));
                return;
            }
        };

        let key_repr = MapTableKeyRepr::Dotted(i as u32, idents);
        let repr_kind = MapTableEntryReprKind::ArrayEntry(array_entry);
        let repr = MapTableEntryRepr::new(parent, key_repr, repr_kind);
        match get_table_to_extend(bump, path, entry, repr) {
            Ok((next_parent, next_path, next)) => {
                parent = next_parent;
                path = next_path;
                current = next;
            }
            Err(e) => {
                ctx.error(e);
                return;
            }
        };
    }

    let key_repr = MapTableKeyRepr::Dotted((idents.len() - 1) as u32, idents);
    let res = insert_array_entry(
        ctx,
        bump,
        path,
        current,
        parent,
        &last.ident,
        key_repr,
        array_entry,
    );
    if let Err(e) = res {
        ctx.error(e);
    }
}

fn insert_array_entry<'a, 'b>(
    ctx: &mut impl TomlCtx,
    bump: &'b Bump,
    path: Option<&'b Path<'a, 'b>>,
    map: &mut MapTable<'a>,
    parent: ParentId,
    key: &'a Ident<'a>,
    key_repr: MapTableKeyRepr<'a>,
    array_entry: &'a ArrayEntry<'a>,
) -> Result<(), Error> {
    let repr_kind = MapTableEntryReprKind::ArrayEntry(array_entry);
    let repr = MapTableEntryRepr::new(parent, key_repr, repr_kind);

    match map.inner.entry(key.text) {
        Occupied(occupied) => {
            let entry = occupied.into_mut();
            let array = match &mut entry.node {
                MapNode::Array(MapArray::Toplevel(a)) => a,
                MapNode::Array(MapArray::Inline(_)) => {
                    let orig = entry.reprs.first();
                    return Err(Error::CannotExtendInlineArray {
                        lines: context_lines(path, [orig.parent, repr.parent]),
                        path: joined_path(path, repr.key.repr_ident()),
                        orig: orig.kind.span(),
                        new: repr.key.repr_ident().lit_span(),
                    });
                }
                MapNode::Table(_) | MapNode::Scalar(_) => {
                    return Err(duplicate_key_error(path, entry.reprs.first(), &repr));
                }
            };

            let parent = insert_repr(&mut entry.reprs, repr);
            let path = append_key(path, &entry.reprs);
            let idx = array.len();
            let path = append_index(Some(&path), idx);

            let mut node = MapTable::new();
            insert_top_level_assignments(
                ctx,
                bump,
                Some(&path),
                parent,
                &mut node,
                &array_entry.assignments,
            );
            array.push(MapArrayToplevelEntry::new(node, parent, array_entry));
        }
        Vacant(vacant) => {
            let parent = ParentId(0);
            let reprs = OneVec::new(repr);
            let path = append_key(path, &reprs);
            let path = append_index(Some(&path), 0);

            let mut node = MapTable::new();
            insert_top_level_assignments(
                ctx,
                bump,
                Some(&path),
                parent,
                &mut node,
                &array_entry.assignments,
            );
            let toplevel_array = MapArrayToplevel::new(node, parent, array_entry);
            let node = MapNode::Array(MapArray::Toplevel(toplevel_array));

            vacant.insert(MapTableEntry::new(node, reprs));
        }
    }

    Ok(())
}

fn insert_top_level_assignments<'a, 'b>(
    ctx: &mut impl TomlCtx,
    bump: &'b Bump,
    path: Option<&'b Path<'a, 'b>>,
    parent: ParentId,
    map: &mut MapTable<'a>,
    assignments: &'a [ToplevelAssignment<'a>],
) {
    for assignment in assignments.iter() {
        let repr_kind = MapTableEntryReprKind::ToplevelAssignment(assignment);
        insert_node_at_path(
            ctx,
            bump,
            path,
            parent,
            map,
            &assignment.assignment.key,
            InsertValue::Value(&assignment.assignment.val),
            repr_kind,
        );
    }
}

fn get_table_to_extend<'a, 'b>(
    bump: &'b Bump,
    prev: Option<&'b Path<'a, 'b>>,
    entry: &'b mut MapTableEntry<'a>,
    repr: MapTableEntryRepr<'a>,
) -> Result<(ParentId, Option<&'b Path<'a, 'b>>, &'b mut MapTable<'a>), Error>
where
    'a: 'b,
{
    let is_assignment = repr.kind.is_assignment();

    let (next_parent, next_path, next) = match &mut entry.node {
        MapNode::Table(t) => {
            let parent = insert_repr(&mut entry.reprs, repr);
            let path = bump.alloc(append_key(prev, &entry.reprs));
            (parent, path, t)
        }
        MapNode::Array(MapArray::Toplevel(a)) => {
            if repr.kind.is_assignment() {
                let orig = entry.reprs.first();
                return Err(Error::CannotExtendArrayWithDottedKey {
                    lines: context_lines(prev, [orig.parent, repr.parent]),
                    orig: orig.kind.span(),
                    path: joined_path(prev, repr.key.repr_ident()),
                    new: repr.key.repr_ident().lit_span(),
                });
            }

            // From the toml spec (https://toml.io/en/v1.0.0#array-of-tables):
            // Any reference to an array of tables points to the most recently
            // defined table element of the array. This allows you to define
            // sub-tables, and even sub-arrays of tables, inside the most recent
            // table.

            let parent = insert_repr(&mut entry.reprs, repr);
            let path = Some(&*bump.alloc(append_key(prev, &entry.reprs)));
            let path = bump.alloc(append_index(path, a.inner.len() - 1));
            let t = &mut a.inner.last_mut().node;
            (parent, path, t)
        }
        MapNode::Array(MapArray::Inline(_)) => {
            let orig = entry.reprs.first();
            return Err(Error::CannotExtendInlineArrayAsTable {
                lines: context_lines(prev, [orig.parent, repr.parent]),
                path: joined_path(prev, repr.key.repr_ident()),
                orig: orig.kind.span(),
                new: repr.key.repr_ident().lit_span(),
            });
        }
        MapNode::Scalar(_) => {
            return Err(duplicate_key_error(prev, entry.reprs.first(), &repr));
        }
    };

    for existing in entry.reprs.iter() {
        match &existing.kind {
            MapTableEntryReprKind::Table(_) => {
                if is_assignment {
                    let orig = entry.reprs.first();
                    let dupe = entry.reprs.last();
                    return Err(Error::CannotExtendTableWithDottedKey {
                        lines: context_lines(prev, [orig.parent, dupe.parent]),
                        path: next_path.fmt_path(),
                        orig: orig.kind.span(),
                        new: dupe.key.repr_ident().lit_span(),
                    });
                }
            }
            MapTableEntryReprKind::ArrayEntry(_) => (),
            MapTableEntryReprKind::ToplevelAssignment(_)
            | MapTableEntryReprKind::InlineTableAssignment(_) => {
                if existing.key.is_last_ident() {
                    // `next` is an inline table
                    let orig = entry.reprs.first();
                    let dupe = entry.reprs.last();
                    return Err(Error::CannotExtendInlineTable {
                        lines: context_lines(prev, [orig.parent, dupe.parent]),
                        path: next_path.fmt_path(),
                        orig: orig.kind.span(),
                        new: entry.reprs.last().key.repr_ident().lit_span(),
                    });
                }
            }
        }
    }

    Ok((next_parent, Some(next_path), next))
}

fn insert_repr<'a>(
    reprs: &mut OneVec<MapTableEntryRepr<'a>>,
    repr: MapTableEntryRepr<'a>,
) -> ParentId {
    let id = ParentId(reprs.len() as u32);
    reprs.push(repr);
    id
}

fn duplicate_key_error(
    path: Option<&Path>,
    original: &MapTableEntryRepr<'_>,
    duplicate: &MapTableEntryRepr<'_>,
) -> Error {
    Error::DuplicateKey {
        lines: context_lines(path, [original.parent, duplicate.parent]),
        path: joined_path(path, duplicate.key.repr_ident()),
        orig: original.key.repr_ident().lit_span(),
        duplicate: duplicate.key.repr_ident().lit_span(),
    }
}
