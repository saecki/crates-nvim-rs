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
use std::fmt::Write as _;
// TODO: Update the comment above.

use bumpalo::Bump;
use common::OneVec;
use common::{FmtChar, FmtStr, Span};

use crate::map::construct::{MapErrorKind, Mapper};
use crate::map::parent::{
    ParentEntry, ParentInlineArray, ParentTable, ParentTableEntry, ParentToplevelArray,
};
use crate::parse::{
    ArrayEntry, BoolVal, CommentRange, DateTimeVal, DottedIdent, FloatVal, Ident, InlineArray,
    InlineArrayValue, InlineTable, InlineTableAssignment, IntVal, StringVal, Table,
    ToplevelAssignment,
};
use crate::{Ast, Error, TomlCtx};

mod construct;
pub mod parent;
#[cfg(test)]
mod test;

pub type MapInner<'a> = indexmap::IndexMap<&'a str, &'a mut MapTableEntry<'a>>;
pub type MapIter<'b, 'a> = indexmap::map::Iter<'b, &'a str, &'a mut MapTableEntry<'a>>;

#[derive(Debug, PartialEq)]
pub struct MapTable<'a> {
    inner: MapInner<'a>,
    pub reprs: OneVec<MapTableRepr<'a>>,
}

impl<'a> AsRef<MapInner<'a>> for MapTable<'a> {
    fn as_ref(&self) -> &MapInner<'a> {
        &self.inner
    }
}

impl<'a> MapTable<'a> {
    pub fn new(repr: MapTableRepr<'a>) -> Self {
        Self {
            reprs: OneVec::new(repr),
            inner: MapInner::new(),
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
}

impl<'a> IntoIterator for MapTable<'a> {
    type Item = (&'a str, &'a mut MapTableEntry<'a>);
    type IntoIter = <MapInner<'a> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

/// All possible ast definitions, that can make up a table.
#[derive(Clone, Debug, PartialEq)]
pub enum MapTableRepr<'a> {
    /// The entire file. This is the root table.
    Root(Span),
    /// This table would be part of the representation of the `a` and `a.table`
    /// tables.
    /// ```toml
    /// [a.table]
    /// ...
    /// ```
    Table(&'a Table<'a>, ParentEntry<'a>),
    /// ```toml
    /// an.inline.table = { }
    /// #                 ^^^ this part here
    /// ```
    InlineTable(&'a InlineTable<'a>, ParentEntry<'a>),
    /// This array of tables entry would be part of the represenation of the
    /// `an` and `an.array` tables. The `an.array.entry` key corresponds to
    /// a [`MapArray::Toplevel`], which in will contain a [`MapArrayToplevelEntry`]
    /// for this definition.
    /// ```toml
    /// [[an.array.entry]]
    /// ...
    /// ```
    ArrayEntry(&'a ArrayEntry<'a>, ParentEntry<'a>),
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
    ToplevelAssignment(&'a ToplevelAssignment<'a>, ParentEntry<'a>),
    /// Both assignments inside the inline table would make up part of the
    /// `things.table` table.
    /// ```toml
    /// things = { table.a = 12, table.b = 13 }
    ///            ^^^^^ here    ^^^^^ and here
    /// ```
    InlineTableAssignment(&'a InlineTableAssignment<'a>, ParentEntry<'a>),
}

impl<'a> MapTableRepr<'a> {
    pub fn span(&self) -> Span {
        match self {
            MapTableRepr::Root(span) => *span,
            MapTableRepr::Table(table, _) => table.span(),
            MapTableRepr::InlineTable(table, _) => table.span(),
            MapTableRepr::ArrayEntry(array, _) => array.span(),
            MapTableRepr::ToplevelAssignment(assignment, _) => assignment.span(),
            MapTableRepr::InlineTableAssignment(assignment, _) => assignment.span(),
        }
    }

    pub fn parent_entry(&self) -> Option<ParentEntry<'a>> {
        match self {
            MapTableRepr::Root(_) => None,
            MapTableRepr::Table(_, parent_entry) => Some(*parent_entry),
            MapTableRepr::InlineTable(_, parent_entry) => Some(*parent_entry),
            MapTableRepr::ArrayEntry(_, parent_entry) => Some(*parent_entry),
            MapTableRepr::ToplevelAssignment(_, parent_entry) => Some(*parent_entry),
            MapTableRepr::InlineTableAssignment(_, parent_entry) => Some(*parent_entry),
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
    pub parent: ParentTable<'a>,
    pub key: MapTableKeyRepr<'a>,
    pub kind: MapTableEntryReprKind<'a>,
}

impl<'a> MapTableEntryRepr<'a> {
    fn new(
        parent: ParentTable<'a>,
        key: MapTableKeyRepr<'a>,
        kind: MapTableEntryReprKind<'a>,
    ) -> Self {
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

    pub fn table_repr(&self, parent_entry: ParentEntry<'a>) -> MapTableRepr<'a> {
        match self {
            MapTableEntryReprKind::Table(table) => MapTableRepr::Table(table, parent_entry),
            MapTableEntryReprKind::ArrayEntry(array) => {
                MapTableRepr::ArrayEntry(array, parent_entry)
            }
            MapTableEntryReprKind::ToplevelAssignment(assignment) => {
                MapTableRepr::ToplevelAssignment(assignment, parent_entry)
            }
            MapTableEntryReprKind::InlineTableAssignment(assignment) => {
                MapTableRepr::InlineTableAssignment(assignment, parent_entry)
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
    Toplevel(&'a mut MapArrayToplevel<'a>),
    Inline(&'a MapArrayInline<'a>),
}

// FIXME: avoid mutable references, maybe using some sort of transmute trick at the end.
#[derive(Debug, PartialEq)]
pub struct MapArrayToplevel<'a> {
    inner: OneVec<&'a mut MapArrayToplevelEntry<'a>>,
}

impl<'a> MapArrayToplevel<'a> {
    pub fn new(entry: &'a mut MapArrayToplevelEntry<'a>) -> Self {
        Self {
            inner: OneVec::new(entry),
        }
    }

    fn push(&mut self, entry: &'a mut MapArrayToplevelEntry<'a>) {
        self.inner.push(entry);
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.len() == 0
    }

    pub fn as_slice(&self) -> &[&'a mut MapArrayToplevelEntry<'a>] {
        self.inner.as_slice()
    }

    pub fn first(&self) -> &MapArrayToplevelEntry<'a> {
        self.inner.first()
    }

    pub fn last(&self) -> &MapArrayToplevelEntry<'a> {
        self.inner.last()
    }

    pub fn iter(&self) -> impl Iterator<Item = &&'a mut MapArrayToplevelEntry<'a>> {
        self.inner.iter()
    }
}

#[derive(Debug, PartialEq)]
pub struct MapArrayToplevelEntry<'a> {
    pub node: &'a mut MapTable<'a>,
    /// The definition of this array of tables entry.
    /// ```toml
    /// [[a.b]] # this here
    /// ```
    pub definition: &'a ArrayEntry<'a>,
    pub parent: ParentToplevelArray<'a>,
    pub parent_entry: ParentEntry<'a>,
    /// A list of tables that extend the this array of tables entry.
    /// ```toml
    /// [[a.b]] # another entry
    ///
    /// [[a.b]] # the entry that is extended
    ///
    /// [a.b.c] # the extension
    /// ```
    pub extensions: Vec<ParentTableEntry<'a>>,
    pub idx: u32,
}

impl<'a> MapArrayToplevelEntry<'a> {
    pub fn new(
        node: &'a mut MapTable<'a>,
        repr: &'a ArrayEntry<'a>,
        parent_entry: ParentEntry<'a>,
        parent: ParentToplevelArray<'a>,
        idx: u32,
    ) -> Self {
        Self {
            node,
            definition: repr,
            parent_entry,
            parent,
            extensions: Vec::new(),
            idx,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum MapArrayToplevelEntryRepr<'a> {
    Definition(&'a ArrayEntry<'a>, ParentToplevelArray<'a>),
    Extension(ParentTable<'a>),
}

#[derive(Debug, PartialEq)]
pub struct MapArrayInline<'a> {
    pub parent: ParentEntry<'a>,
    pub repr: &'a InlineArray<'a>,
    inner: &'a [MapArrayInlineEntry<'a>],
}

impl<'a> MapArrayInline<'a> {
    pub fn new(
        parent: ParentEntry<'a>,
        repr: &'a InlineArray<'a>,
        inner: &'a [MapArrayInlineEntry<'a>],
    ) -> Self {
        Self {
            parent,
            repr,
            inner,
        }
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.len() == 0
    }

    pub fn get(&self, idx: usize) -> Option<&MapArrayInlineEntry<'a>> {
        self.inner.get(idx)
    }

    pub fn as_slice(&self) -> &[MapArrayInlineEntry<'a>] {
        &self.inner
    }

    pub fn iter(&self) -> impl Iterator<Item = &MapArrayInlineEntry<'a>> {
        self.inner.iter()
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

#[derive(Debug, PartialEq)]
pub struct MapArrayInlineEntry<'a> {
    pub node: MapNode<'a>,
    pub repr: &'a InlineArrayValue<'a>,
    pub parent: ParentInlineArray<'a>,
    pub idx: u32,
}

impl<'a> MapArrayInlineEntry<'a> {
    pub fn new(
        node: MapNode<'a>,
        repr: &'a InlineArrayValue<'a>,
        parent: ParentInlineArray<'a>,
        idx: u32,
    ) -> Self {
        Self {
            node,
            repr,
            parent,
            idx,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum MapNode<'a> {
    Table(&'a mut MapTable<'a>),
    Array(MapArray<'a>),
    Scalar(Scalar<'a>),
}

// TODO: Add ParentEntry to scalars
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

pub fn map<'a>(ctx: &mut impl TomlCtx, bump: &'a Bump, ast: &Ast<'a>) -> &'a MapTable<'a> {
    let mut mapper = Mapper::default();
    let map = construct::map(&mut mapper, bump, ast);

    for e in mapper.errors {
        let lines = context_lines([e.orig_parent, e.new_parent]);
        let path = joined_path(e.new_parent, e.new_ident);
        let orig = e.orig_ident.lit_span();
        let new = e.new_ident.lit_span();

        let error = match e.kind {
            MapErrorKind::DuplicateKey => Error::DuplicateKey {
                lines,
                path,
                orig,
                new,
            },
            MapErrorKind::CannotExtendTableWithDottedKey => Error::CannotExtendTableWithDottedKey {
                lines,
                path,
                orig,
                new,
            },
            MapErrorKind::CannotExtendInlineTable => Error::CannotExtendInlineTable {
                lines,
                path,
                orig,
                new,
            },
            MapErrorKind::CannotExtendArrayWithDottedKey => Error::CannotExtendArrayWithDottedKey {
                lines,
                path,
                orig,
                new,
            },
            MapErrorKind::CannotExtendInlineArray => Error::CannotExtendInlineArray {
                lines,
                path,
                orig,
                new,
            },
            MapErrorKind::CannotExtendInlineArrayAsTable => Error::CannotExtendInlineArrayAsTable {
                lines,
                path,
                orig,
                new,
            },
        };
        ctx.error(error);
    }

    map
}

fn fmt_path(parent_entry: ParentEntry) -> String {
    match parent_entry {
        ParentEntry::Table(entry) => {
            let repr = entry.repr();
            let mut buf = (repr.parent.repr())
                .parent_entry()
                .map(fmt_path)
                .unwrap_or_default();
            if !buf.is_empty() {
                buf.push('.');
            }
            let ident = repr.key.repr_ident();
            fmt_ident(&mut buf, ident).unwrap();
            buf
        }
        ParentEntry::ToplevelArray(entry) => {
            let array_entry = entry.get();
            let mut buf = fmt_path(array_entry.parent_entry);
            write!(&mut buf, "[{}]", array_entry.idx).ok();
            buf
        }
        ParentEntry::ToplevelArrayExtension(entry) => {
            let mut buf = fmt_path(entry.parent_table_entry().wrap());
            write!(&mut buf, "[{}]", entry.get().idx).ok();
            buf
        }
        ParentEntry::InlineArray(entry) => {
            let array_entry = entry.get();
            let mut buf = fmt_path(array_entry.parent.get().parent);
            write!(&mut buf, "[{}]", array_entry.idx).ok();
            buf
        }
    }
}

pub fn joined_path(parent: ParentTable, ident: &Ident) -> FmtStr {
    let parent_entry = parent.repr().parent_entry();
    let mut buf = parent_entry.map(fmt_path).unwrap_or_default();
    if parent_entry.is_some() {
        buf.push('.');
    }
    fmt_ident(&mut buf, ident).unwrap();
    FmtStr::from_string(buf)
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

pub fn context_lines<const LEN: usize>(parents: [ParentTable; LEN]) -> Box<[u32]> {
    let mut lines = Vec::new();
    for parent in parents {
        if let Some(parent_entry) = parent.repr().parent_entry() {
            collect_lines(&mut lines, parent_entry);
        }
    }
    lines.sort();
    lines.dedup();
    lines.into_boxed_slice()
}

fn collect_lines(lines: &mut Vec<u32>, parent_entry: ParentEntry) {
    match parent_entry {
        ParentEntry::Table(entry) => {
            let repr = entry.repr();
            if let Some(parent) = repr.parent.repr().parent_entry() {
                collect_lines(lines, parent);
            }
            lines.push(repr.key.repr_ident().lit_start.line);
        }
        ParentEntry::ToplevelArray(entry) => {
            let array_entry = entry.get();
            collect_lines(lines, array_entry.parent_entry);
            lines.push(array_entry.definition.start().line);
        }
        ParentEntry::ToplevelArrayExtension(entry) => {
            collect_lines(lines, ParentEntry::Table(entry.parent_table_entry()));
            lines.push(entry.get().definition.start().line);
        }
        ParentEntry::InlineArray(entry) => {
            let array_entry = entry.get();
            collect_lines(lines, array_entry.parent.get().parent);
            lines.push(array_entry.repr.start().line);
        }
    }
}
