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
// TODO: update the comment above
use std::fmt::Write as _;
use std::mem::MaybeUninit;

use bumpalo::Bump;
use common::OneVec;
use common::{FmtChar, FmtStr, Span};

use crate::parse::{
    ArrayEntry, BoolVal, CommentRange, Cyclic, DateTimeVal, DottedIdent, FloatVal, Ident,
    InlineArray, InlineArrayValue, InlineTable, InlineTableAssignment, IntVal, Key, StringVal,
    Table, Toplevel, ToplevelAssignment, Value,
};
use crate::{Ast, Error, TomlCtx};

#[cfg(test)]
mod test;

#[cfg(feature = "indexmap")]
use indexmap::map::Entry::{Occupied, Vacant};
#[cfg(feature = "indexmap")]
use indexmap::map::VacantEntry;
#[cfg(feature = "indexmap")]
pub type MapInner<'a> = indexmap::IndexMap<&'a str, &'a mut MapTableEntry<'a>>;
#[cfg(feature = "indexmap")]
pub type MapIter<'b, 'a> = indexmap::map::Iter<'b, &'a str, &'a mut MapTableEntry<'a>>;

#[cfg(not(feature = "indexmap"))]
use std::collections::hash_map::Entry::{Occupied, Vacant};
#[cfg(not(feature = "indexmap"))]
use std::collections::hash_map::VacantEntry;
#[cfg(not(feature = "indexmap"))]
pub type MapInner<'a> = std::collections::hash_map::HashMap<&'a str, &'a MapTableEntry<'a>>;
#[cfg(not(feature = "indexmap"))]
pub type MapIter<'b, 'a> = std::collections::hash_map::Iter<'b, &'a str, &'a MapTableEntry<'a>>;

#[derive(Debug, PartialEq)]
pub struct Map<'a> {
    root: &'a MapTable<'a>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ReprIdx(u32);

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ParentTable {
    map: Cyclic<MapTable<'static>>,
    idx: ReprIdx,
}

impl ParentTable {
    fn repr(&self) -> &MapTableRepr<'_> {
        &self.map.get().reprs[self.idx.0 as usize]
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ParentToplevelArray {
    array: Cyclic<MapArrayToplevel<'static>>,
    idx: ReprIdx,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ParentInlineArray {
    array: Cyclic<MapArrayInline<'static>>,
    idx: ReprIdx,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum ParentEntry {
    Root,
    Table(Cyclic<MapTableEntry<'static>>, ReprIdx),
    ToplevelArray(Cyclic<MapArrayToplevelEntry<'static>>, ReprIdx),
    InlineArray(Cyclic<MapArrayInlineEntry<'static>>, ReprIdx),
}

#[derive(Debug, PartialEq)]
pub struct MapTable<'a> {
    inner: MapInner<'a>,
    pub reprs: OneVec<MapTableRepr<'a>>,
    pub parent: ParentEntry,
}

impl<'a> AsRef<MapInner<'a>> for MapTable<'a> {
    fn as_ref(&self) -> &MapInner<'a> {
        &self.inner
    }
}

impl<'a> MapTable<'a> {
    pub fn new(repr: MapTableRepr<'a>, parent: ParentEntry) -> Self {
        Self {
            reprs: OneVec::new(repr),
            inner: MapInner::new(),
            parent,
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn get(&self, key: &str) -> Option<&MapTableEntry<'a>> {
        self.inner.get(key).map(|e| &**e)
    }

    pub fn iter(&self) -> MapIter<'_, 'a> {
        self.inner.iter()
    }

    pub(crate) fn insert_repr(&mut self, repr: MapTableRepr<'a>) -> ParentTable {
        let idx = ReprIdx(self.reprs.len() as u32);
        self.reprs.push(repr);
        let map = Cyclic::new(self as *const MapTable<'_> as *const MapTable<'static>);
        ParentTable { map, idx }
    }
}

impl<'a> IntoIterator for MapTable<'a> {
    type Item = (&'a str, &'a mut MapTableEntry<'a>);
    type IntoIter = <MapInner<'a> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

/// All possible ast definitions, that can make up a table.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MapTableRepr<'a> {
    /// The entire file. This is the root table.
    Root(Span),
    /// This table would be part of the representation of the `a` and `a.table`
    /// tables.
    /// ```toml
    /// [a.table]
    /// ...
    /// ```
    Table(&'a Table<'a>),
    /// ```toml
    /// an.inline.table = { }
    /// #                 ^^^ this part here
    /// ```
    InlineTable(&'a InlineTable<'a>),
    /// This array of tables entry would be part of the represenation of the
    /// `an` and `an.array` tables. The `an.array.entry` key corresponds to
    /// a [`MapArray::Toplevel`], which in will contain a [`MapArrayToplevelEntry`]
    /// for this definition.
    /// ```toml
    /// [[an.array.entry]]
    /// ...
    /// ```
    ArrayEntry(&'a ArrayEntry<'a>),
    /// This assignment would be part of the represenation for the `a` table and
    /// the `a.toplevel` table.
    ///
    /// Note that only assignments with dotted keys can make up a table.
    /// And the last identifier of the dotted key, cannot be part of the
    /// representation that makes up a [`MapTableRepr`], but it will represent
    /// a [`MapTableEntryRepr`].
    /// ```toml
    /// a.toplevel.assignment = 12
    /// ```
    ToplevelAssignment(&'a ToplevelAssignment<'a>),
    /// Both assignments inside the inline table would make up part of the
    /// `things.table` table.
    /// ```toml
    /// things = { table.a = 12, table.b = 13 }
    ///            ^^^^^ here    ^^^^^ and here
    /// ```
    InlineTableAssignment(&'a InlineTableAssignment<'a>),
}

impl<'a> MapTableRepr<'a> {
    pub fn span(&self) -> Span {
        match self {
            MapTableRepr::Root(span) => *span,
            MapTableRepr::Table(table) => table.span(),
            MapTableRepr::InlineTable(table) => table.span(),
            MapTableRepr::ArrayEntry(array) => array.span(),
            MapTableRepr::ToplevelAssignment(assignment) => assignment.span(),
            MapTableRepr::InlineTableAssignment(assignment) => assignment.span(),
        }
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct MapTableEntryRepr<'a> {
    /// Index of the parent defined in the parent [`MapTableEntry::reprs`].
    pub parent: ParentTable,
    pub key: MapTableKeyRepr<'a>,
    pub kind: MapTableEntryReprKind<'a>,
}

impl<'a> MapTableEntryRepr<'a> {
    fn new(parent: ParentTable, key: MapTableKeyRepr<'a>, kind: MapTableEntryReprKind<'a>) -> Self {
        Self { parent, key, kind }
    }

    /// The span from the [`MapTableKeyRepr::repr_ident`] to the end of the value.
    pub fn repr_span(&self) -> Span {
        Span::new(self.key.repr_ident().lit_start, self.kind.span().end)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MapTableEntryReprKind<'a> {
    Table(&'a Table<'a>),
    ArrayEntry(&'a ArrayEntry<'a>),
    ToplevelAssignment(&'a ToplevelAssignment<'a>),
    InlineTableAssignment(&'a InlineTableAssignment<'a>),
}

impl<'a> MapTableEntryReprKind<'a> {
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
    pub fn comments(&self) -> Option<CommentRange> {
        match self {
            MapTableEntryReprKind::Table(t) => Some(t.comments),
            MapTableEntryReprKind::ArrayEntry(a) => Some(a.comments),
            MapTableEntryReprKind::ToplevelAssignment(a) => Some(a.comments),
            MapTableEntryReprKind::InlineTableAssignment(_) => None,
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

    pub fn table_repr(&self) -> MapTableRepr<'a> {
        match self {
            MapTableEntryReprKind::Table(table) => MapTableRepr::Table(table),
            MapTableEntryReprKind::ArrayEntry(array) => MapTableRepr::ArrayEntry(array),
            MapTableEntryReprKind::ToplevelAssignment(assignment) => {
                MapTableRepr::ToplevelAssignment(assignment)
            }
            MapTableEntryReprKind::InlineTableAssignment(assignment) => {
                MapTableRepr::InlineTableAssignment(assignment)
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
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
    pub parent: ParentEntry,
}

impl<'a> MapArrayToplevel<'a> {
    pub fn new(bump: &'a Bump, node: MapTable<'a>, repr: &'a ArrayEntry<'a>, parent_entry: ParentEntry) -> &'a Self {
        let reserved = reserve::<Self>(bump);
        let parent = reserved.parent_array(ReprIdx(0));
        let array = Self {
            inner: OneVec::new(MapArrayToplevelEntry::new(node, repr, parent, 0)),
            parent: parent_entry,
        };
        reserved.init(array)
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

    pub fn as_slice(&self) -> &[MapArrayToplevelEntry<'a>] {
        self.inner.as_slice()
    }

    pub fn first(&self) -> &MapArrayToplevelEntry<'a> {
        self.inner.first()
    }

    pub fn last(&self) -> &MapArrayToplevelEntry<'a> {
        self.inner.last()
    }

    pub fn iter(&self) -> impl Iterator<Item = &MapArrayToplevelEntry<'a>> {
        self.inner.iter()
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
    pub repr: &'a ArrayEntry<'a>,
    pub parent: ParentToplevelArray,
    pub idx: u32,
}

impl<'a> MapArrayToplevelEntry<'a> {
    pub fn new(node: MapTable<'a>, repr: &'a ArrayEntry<'a>, parent: ParentToplevelArray, idx: u32) -> Self {
        Self { node, repr, parent, idx }
    }
}

#[derive(Debug, PartialEq)]
pub struct MapArrayInline<'a> {
    pub parent: ParentInlineArray,
    pub repr: &'a InlineArray<'a>,
    inner: Box<[MapArrayInlineEntry<'a>]>,
}

impl<'a> MapArrayInline<'a> {
    pub fn new(parent: ParentInlineArray, repr: &'a InlineArray<'a>) -> Self {
        Self {
            repr,
            parent,
            inner: Box::new([]),
        }
    }

    pub fn from_iter<T>(parent: ParentInlineArray, repr: &'a InlineArray<'a>, iter: T) -> Self
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

    pub fn as_slice(&self) -> &[MapArrayInlineEntry<'a>] {
        &self.inner
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
    pub parent: ParentInlineArray,
    pub idx: u32,
}

impl<'a> MapArrayInlineEntry<'a> {
    pub fn new(node: MapNode<'a>, repr: &'a InlineArrayValue<'a>, parent: ParentInlineArray, idx: u32) -> Self {
        Self { node, repr, parent, idx }
    }
}

#[derive(Debug, PartialEq)]
pub enum MapNode<'a> {
    Table(&'a mut MapTable<'a>),
    Array(MapArray<'a>),
    Scalar(Scalar<'a>),
}

#[derive(Debug, PartialEq)]
pub enum Scalar<'a> {
    String(&'a StringVal<'a>),
    Int(&'a IntVal),
    Float(&'a FloatVal),
    Bool(&'a BoolVal),
    DateTime(&'a DateTimeVal),
    Invalid(&'a Span),
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
            Scalar::Invalid(span) => **span,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct ParentId(pub u32);

/// Linked list of path segments with parent span information, mainly used for diagnostics.
#[derive(Clone)]
pub struct Path<'a, 'b> {
    pub prev: Option<&'b Path<'a, 'b>>,
    pub segment: PathSegment<'a, 'b>,
}

#[derive(Clone, Debug)]
pub enum PathSegment<'a, 'b> {
    Table(&'b OneVec<MapTableEntryRepr<'a>>),
    Array(ParentId, usize),
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
    pub fn append_index(&'b self, parent: ParentId, index: usize) -> Self {
        append_index(Some(self), parent, index)
    }

    pub fn joined_path(&self, ident: &Ident) -> FmtStr {
        joined_path(Some(self), ident)
    }

    pub fn fmt_path(&self) -> FmtStr {
        let path = fmt_path(self);
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
pub fn append_index<'a, 'b>(
    prev: Option<&'b Path<'a, 'b>>,
    parent: ParentId,
    index: usize,
) -> Path<'a, 'b> {
    Path {
        prev,
        segment: PathSegment::Array(parent, index),
    }
}

fn fmt_path(path: &Path) -> String {
    let mut buf = String::new();
    if let Some(prev) = path.prev {
        buf = fmt_path(prev)
    };
    match path.segment {
        PathSegment::Table(reprs) => {
            if path.prev.is_some() {
                buf.push('.');
            }
            let key = reprs.first().key.repr_ident();
            fmt_ident(&mut buf, key).unwrap();
        }
        PathSegment::Array(_, i) => write!(&mut buf, "[{i}]").unwrap(),
    }
    buf
}

pub fn joined_path(prev: Option<&Path>, key: &Ident) -> FmtStr {
    let str = match prev {
        Some(prev) => {
            let mut buf = fmt_path(prev);
            buf.push('.');
            fmt_ident(&mut buf, key).unwrap();
            buf
        }
        None => {
            let mut buf = String::new();
            fmt_ident(&mut buf, key).unwrap();
            buf
        }
    };
    FmtStr::from_string(str)
}

pub struct FmtIdent<'a>(pub &'a str);

impl std::fmt::Display for FmtIdent<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt_ident_str(f, self.0)
    }
}

pub fn fmt_ident(f: &mut impl std::fmt::Write, key: &Ident) -> std::fmt::Result {
    fmt_ident_str(f, key.text)
}

fn fmt_ident_str(f: &mut impl std::fmt::Write, key: &str) -> std::fmt::Result {
    if key.is_empty() {
        f.write_str("''")?;
    } else {
        let is_invalid_plain_ident =
            (key.chars()).any(|c| !matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'));
        if is_invalid_plain_ident {
            f.write_char('\'')?;
            for c in key.chars() {
                write!(f, "{}", FmtChar(c))?;
            }
            f.write_char('\'')?;
        } else {
            f.write_str(key)?;
        }
    }
    Ok(())
}

pub fn context_lines<const LEN: usize>(
    parent: ParentEntry,
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

fn collect_lines(lines: &mut Vec<u32>, mut parent: ParentEntry, mut idx: ReprIdx) {
    loop {
        match parent {
            ParentEntry::Root => break,
            ParentEntry::Table(cyclic) => cyclic.get().reprs
            ParentEntry::ToplevelArray(cyclic) => todo!(),
            ParentEntry::InlineArray(cyclic) => todo!(),
        }

        let Some(prev) = path.prev else { break };
        path = prev;
    }
}

/// Value to be lazily mapped and inserted
enum InsertValue<'a> {
    Value(&'a Value<'a>),
    TableAssignments(&'a Table<'a>),
}

pub fn map<'a>(ctx: &mut impl TomlCtx, bump: &'a Bump, ast: &'_ Ast<'a>) -> Map<'a> {
    let reserved = reserve::<MapTable>(bump);
    let mut root = MapTable::new(MapTableRepr::Root(ast.span), ParentEntry::Root);
    for (a, i) in ast.toplevel.iter().zip(0..) {
        let parent = reserved.parent_table(ReprIdx(i));
        match a {
            Toplevel::Assignment(assignment) => {
                let repr_kind = MapTableEntryReprKind::ToplevelAssignment(assignment);
                insert_node_at_path(
                    ctx,
                    &bump,
                    parent,
                    &mut root.inner,
                    &assignment.assignment.key,
                    InsertValue::Value(&assignment.assignment.val),
                    repr_kind,
                );
            }
            Toplevel::Table(table) => {
                let Some(key) = &table.header.key else {
                    continue;
                };

                let repr_kind = MapTableEntryReprKind::Table(table);
                insert_node_at_path(
                    ctx,
                    &bump,
                    parent,
                    &mut root.inner,
                    key,
                    InsertValue::TableAssignments(table),
                    repr_kind,
                );
            }
            Toplevel::Array(array_entry) => {
                let Some(key) = &array_entry.header.key else {
                    continue;
                };
                insert_array_entry_at_path(
                    ctx,
                    &bump,
                    parent,
                    &mut root.inner,
                    key,
                    array_entry,
                );
            }
        }
    }
    let root = reserved.init(root);
    Map { root }
}

fn map_insert_value<'a>(
    ctx: &mut impl TomlCtx,
    bump: &'a Bump,
    parent_entry: ParentEntry,
    value: InsertValue<'a>,
) -> MapNode<'a> {
    match value {
        InsertValue::Value(value) => map_value(ctx, bump, parent_entry, value),
        InsertValue::TableAssignments(table) => {
            let reserved = reserve::<MapTable>(bump);
            let mut map = MapTable::new(MapTableRepr::Table(table), parent_entry);
            insert_top_level_assignments(
                ctx,
                bump,
                reserved.parent_table(ReprIdx(0)),
                &mut map.inner,
                &table.assignments,
            );
            MapNode::Table(reserved.init(map))
        }
    }
}

fn map_value<'a>(
    ctx: &mut impl TomlCtx,
    bump: &'a Bump,
    parent_entry: ParentEntry,
    value: &'a Value<'a>,
) -> MapNode<'a> {
    match value {
        Value::String(s) => MapNode::Scalar(Scalar::String(s)),
        Value::Int(i) => MapNode::Scalar(Scalar::Int(i)),
        Value::Float(f) => MapNode::Scalar(Scalar::Float(f)),
        Value::Bool(b) => MapNode::Scalar(Scalar::Bool(b)),
        Value::DateTime(d) => MapNode::Scalar(Scalar::DateTime(d)),
        Value::InlineTable(table) => {
            let reserved = reserve::<MapTable>(bump);
            let mut map = MapTable::new(MapTableRepr::InlineTable(table), parent_entry);
            for (assignment, i) in table.assignments.iter().zip(0..) {
                let repr_kind = MapTableEntryReprKind::InlineTableAssignment(assignment);
                insert_node_at_path(
                    ctx,
                    bump,
                    reserved.parent_table(ReprIdx(i)),
                    &mut map.inner,
                    &assignment.assignment.key,
                    InsertValue::Value(&assignment.assignment.val),
                    repr_kind,
                );
            }
            MapNode::Table(reserved.init(val))
        }
        Value::InlineArray(inline_array) => {
            let entries = (inline_array.values.iter().zip(0..)).map(|(value, idx)| {
                let array = MapArrayInlineEntry::new(node, value)
                let parent = ParentEntry::InlineArray { parent, idx };
                let node = map_value(ctx, bump, &path, parent, &value.val);
            });
            let array = MapArrayInline::(parent, inline_array, entries);
            MapNode::Array(MapArray::Inline(array))
        }
        Value::Invalid(s) => MapNode::Scalar(Scalar::Invalid(s)),
    }
}

fn reserve<'a, T>(bump: &'a Bump) -> Reserved<'a, T> {
    let loc = bump.alloc(MaybeUninit::uninit());
    Reserved { loc }
}

struct Reserved<'a, T> {
    loc: &'a mut MaybeUninit<T>,
}

impl<'a, T> Reserved<'a, T> {
    pub fn init(self, val: T) -> &'a mut T {
        self.loc.write(val)
    }
}

impl<'a> Reserved<'a, MapTableEntry<'a>> {
    pub fn parent_entry(&self, idx: ReprIdx) -> ParentEntry {
        let table_entry = Cyclic::new(self.loc as *const MaybeUninit<MapTableEntry<'a>> as *const MapTableEntry<'static>);
        ParentEntry::Table(table_entry, idx)
    }
}

impl<'a> Reserved<'a, MapTable<'a>> {
    pub fn parent_table(&self, idx: ReprIdx) -> ParentTable {
        let map = Cyclic::new(self.loc as *const MaybeUninit<MapTable<'a>> as *const MapTable<'static>);
        ParentTable { map, idx }
    }
}

impl<'a> Reserved<'a, MapArrayToplevel<'a>> {
    pub fn parent_array(&self, idx: ReprIdx) -> ParentToplevelArray {
        let array = Cyclic::new(self.loc as *const MaybeUninit<MapArrayToplevel<'a>> as *const MapArrayToplevel<'static>);
        ParentToplevelArray { array, idx }
    }
}

#[allow(clippy::too_many_arguments)]
fn insert_node_at_path<'a>(
    ctx: &mut impl TomlCtx,
    bump: &'a Bump,
    mut parent: ParentTable,
    map: &mut MapInner<'a>,
    key: &'a Key<'a>,
    value: InsertValue<'a>,
    repr_kind: MapTableEntryReprKind<'a>,
) {
    let idents = match key {
        Key::One(ident) => {
            let key_repr = MapTableKeyRepr::One(ident);
            let repr = MapTableEntryRepr::new(parent, key_repr, repr_kind);
            let res = insert_node(ctx, bump, map, ident, value, repr);
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
    for (o, i) in other.iter().zip(0..) {
        let entry = match current.entry(o.ident.text) {
            Occupied(occupied) => occupied.into_mut(),
            Vacant(vacant) => {
                insert_at_vacant_path(ctx, bump, parent, vacant, idents, i, value, repr_kind);
                return;
            }
        };

        let key_repr = MapTableKeyRepr::Dotted(i as u32, idents);
        let repr = MapTableEntryRepr::new(parent, key_repr, repr_kind);
        match get_table_to_extend(entry, repr) {
            Ok((next_parent, next)) => {
                parent = next_parent;
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
    let res = insert_node(ctx, bump, current, &last.ident, value, repr);
    if let Err(e) = res {
        ctx.error(e);
    }
}

fn insert_at_vacant_path<'a>(
    ctx: &mut impl TomlCtx,
    bump: &'a Bump,
    mut parent: ParentTable,
    mut vacant: VacantEntry<'_, &'a str, &'a mut MapTableEntry<'a>>,
    idents: &'a [DottedIdent<'a>],
    i: u32,
    value: InsertValue<'a>,
    repr_kind: MapTableEntryReprKind<'a>,
) {
    for (pair, i) in idents[i as usize..].windows(2).zip(i..) {
        let reserved = reserve::<MapTableEntry>(bump);
        let parent_entry = reserved.parent_entry(ReprIdx(0));
        let key_repr = MapTableKeyRepr::Dotted(i, idents);
        let repr = MapTableEntryRepr::new(parent, key_repr, repr_kind);

        let node = {
            let reserved = reserve::<MapTable>(bump);
            let map = MapTable::new(repr_kind.table_repr(), parent_entry);
            parent = reserved.parent_table(ReprIdx(0));
            MapNode::Table(reserved.init(map))
        };

        let table_entry = MapTableEntry::from_one(node, repr);
        let entry = vacant.insert(reserved.init(table_entry));

        let MapNode::Table(next) = &mut entry.node else {
            unreachable!()
        };
        vacant = match next.inner.entry(pair[1].ident.text) {
            Occupied(_) => unreachable!(),
            Vacant(vacant) => vacant,
        };
    }

    let reserved = reserve::<MapTableEntry>(bump);
    let parent_entry = reserved.parent_entry(ReprIdx(0));
    let key_repr = MapTableKeyRepr::Dotted((idents.len() - 1) as u32, idents);
    let repr = MapTableEntryRepr::new(parent, key_repr, repr_kind);
    let node = map_insert_value(ctx, bump, parent_entry, value);
    let table_entry = MapTableEntry::from_one(node, repr);
    vacant.insert(reserved.init(table_entry));
}

fn insert_node<'a>(
    ctx: &mut impl TomlCtx,
    bump: &'a Bump,
    map: &mut MapInner<'a>,
    key: &'a Ident<'a>,
    value: InsertValue<'a>,
    repr: MapTableEntryRepr<'a>,
) -> Result<(), Error> {
    let existing_entry = match map.entry(key.text) {
        Occupied(occupied) => occupied.into_mut(),
        Vacant(vacant) => {
            let reserved = reserve::<MapTableEntry>(bump);
            let parent_entry = reserved.parent_entry(ReprIdx(0));
            let node = map_insert_value(ctx, bump, parent_entry, value);
            let table_entry = MapTableEntry::from_one(node, repr);
            vacant.insert(reserved.init(table_entry));
            return Ok(());
        }
    };
    existing_entry.reprs.push(repr);

    let InsertValue::TableAssignments(table) = value else {
        return Err(duplicate_key_error(
            path,
            existing_entry.reprs.first(),
            &repr,
        ));
    };
    let existing_table = match &mut existing_entry.node {
        MapNode::Table(table) => table,
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

    // Extend existing table with items from super table.
    // ```toml
    // [a.b.c] # this would be the existing table
    //
    // [a.b] # this would be the super table
    // ```
    let parent = existing_table.insert_repr(repr.kind.table_repr());
    insert_top_level_assignments(
        ctx,
        bump,
        parent,
        &mut existing_table.inner,
        &table.assignments,
    );

    Ok(())
}

fn insert_array_entry_at_path<'a>(
    ctx: &mut impl TomlCtx,
    bump: &'a Bump,
    parent: ParentTable,
    map: &mut MapInner<'a>,
    key: &'a Key<'a>,
    array_entry: &'a ArrayEntry<'a>,
) {
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
        let entry = match current.entry(o.ident.text) {
            Occupied(occupied) => occupied.into_mut(),
            Vacant(mut vacant) => {
                for j in i..idents.len() - 1 {
                    let key_repr = MapTableKeyRepr::Dotted(j as u32, idents);
                    let repr_kind = MapTableEntryReprKind::ArrayEntry(array_entry);
                    let repr = MapTableEntryRepr::new(parent, key_repr, repr_kind);
                    let node = MapNode::Table(MapTable::new(repr_kind.table_repr()));
                    let entry = vacant.insert(MapTableEntry::from_one(node, repr));

                    path = Some(bump.alloc(append_key(path, &entry.reprs)));
                    parent = ParentThingy(0);

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

                let path = append_key(path, &reprs);
                parent = ParentThingy(0);
                let path = path.append_index(parent, 0);

                let mut node = MapTable::new(MapTableRepr::ArrayEntry(array_entry));
                insert_top_level_assignments(
                    ctx,
                    bump,
                    Some(&path),
                    parent,
                    &mut node.inner,
                    &array_entry.assignments,
                );
                let toplevel_array = MapArrayToplevel::new(bump, node, parent, array_entry);
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

#[allow(clippy::too_many_arguments)]
fn insert_array_entry<'a, 'b>(
    ctx: &mut impl TomlCtx,
    bump: &'b Bump,
    path: Option<&'b Path<'a, 'b>>,
    map: &'b mut MapInner<'a>,
    parent: ParentThingy,
    key: &'a Ident<'a>,
    key_repr: MapTableKeyRepr<'a>,
    array_entry: &'a ArrayEntry<'a>,
) -> Result<(), Error> {
    let repr_kind = MapTableEntryReprKind::ArrayEntry(array_entry);
    let repr = MapTableEntryRepr::new(parent, key_repr, repr_kind);

    match map.entry(key.text) {
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
            let path = append_index(Some(&path), parent, idx);

            let mut node = MapTable::new(MapTableRepr::ArrayEntry(array_entry));
            insert_top_level_assignments(
                ctx,
                bump,
                Some(&path),
                parent,
                &mut node.inner,
                &array_entry.assignments,
            );
            array.push(MapArrayToplevelEntry::new(node, parent, array_entry));
        }
        Vacant(vacant) => {
            let reprs = OneVec::new(repr);
            let path = append_key(path, &reprs);
            let parent = ParentThingy(0);
            let path = append_index(Some(&path), parent, 0);

            let mut node = MapTable::new(MapTableRepr::ArrayEntry(array_entry));
            insert_top_level_assignments(
                ctx,
                bump,
                Some(&path),
                parent,
                &mut node.inner,
                &array_entry.assignments,
            );
            let toplevel_array = MapArrayToplevel::new(node, parent, array_entry);
            let node = MapNode::Array(MapArray::Toplevel(toplevel_array));

            vacant.insert(MapTableEntry::new(node, reprs));
        }
    }

    Ok(())
}

fn insert_top_level_assignments<'a>(
    ctx: &mut impl TomlCtx,
    bump: &'a Bump,
    parent: ParentTable,
    map: &mut MapInner<'a>,
    assignments: &'a [ToplevelAssignment<'a>],
) {
    for assignment in assignments.iter() {
        let repr_kind = MapTableEntryReprKind::ToplevelAssignment(assignment);
        insert_node_at_path(
            ctx,
            bump,
            parent,
            map,
            &assignment.assignment.key,
            InsertValue::Value(&assignment.assignment.val),
            repr_kind,
        );
    }
}

fn get_table_to_extend<'a, 'b>(
    entry: &'b mut MapTableEntry<'a>,
    repr: MapTableEntryRepr<'a>,
) -> Result<(ParentTable, &'b mut MapInner<'a>), Error> {
    let (parent_idx, next) = match &mut entry.node {
        MapNode::Table(t) => {
            let next_parent_idx = insert_repr(&mut entry.reprs, repr);
            (next_parent_idx, t)
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
            let path = bump.alloc(append_index(path, parent, a.inner.len() - 1));
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

    next.reprs.push(repr.kind.table_repr());

    for existing in entry.reprs.iter() {
        match &existing.kind {
            MapTableEntryReprKind::Table(_) => {
                if repr.kind.is_assignment() {
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

    Ok((next_parent, &mut next.inner))
}

fn insert_repr<'a>(
    entry: &mut MapTableEntry,
    repr: MapTableEntryRepr<'a>,
) -> ParentTable {
    let id = ReprIdx(entry.reprs.len() as u32);
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
