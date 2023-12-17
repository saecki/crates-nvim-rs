use std::collections::HashMap;

use crate::onevec::OneVec;
use crate::{
    ArrayEntry, Assignment, Ast, BoolVal, Ctx, DateTimeVal, DottedIdent, Error, FloatVal, Ident,
    InlineArray, InlineArrayValue, InlineTableAssignment, IntVal, Key, Span, StringVal, Table,
    Value,
};

use entry::*;

mod entry;
#[cfg(test)]
mod test;

#[derive(Debug, PartialEq)]
pub struct MapTable<'a> {
    inner: HashMap<&'a str, MapTableEntries<'a>>,
}

impl<'a> MapTable<'a> {
    pub fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    pub fn get(&self, key: &'a str) -> Option<&MapTableEntries<'a>> {
        self.inner.get(key)
    }

    fn get_mut(&mut self, key: &'a str) -> Option<&mut MapTableEntries<'a>> {
        self.inner.get_mut(&key)
    }

    pub fn from_pairs(pairs: impl IntoIterator<Item = (&'a str, MapTableEntries<'a>)>) -> Self {
        Self {
            inner: HashMap::from_iter(pairs),
        }
    }

    fn insert(
        &mut self,
        key: &'a str,
        node: MapNode<'a>,
        repr: MapTableEntryRepr<'a>,
    ) -> Result<(), Error> {
        use std::collections::hash_map::Entry::*;

        let existing = self.inner.entry(key);
        match existing {
            Occupied(occupied) => Err(Error::DuplicateKey(
                key.to_string(),
                occupied.get().reprs.first().key.referenced_ident().lit_span,
                repr.key.referenced_ident().lit_span,
            )),
            Vacant(vacant) => {
                vacant.insert(MapTableEntries::new(node, repr));
                Ok(())
            }
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&&str, &MapTableEntries<'_>)> {
        self.inner.iter()
    }
}

#[derive(Debug, PartialEq)]
pub struct MapTableEntries<'a> {
    pub node: MapNode<'a>,
    /// References to the actual representations inside the toml file.
    pub reprs: OneVec<MapTableEntryRepr<'a>>,
}

impl<'a> MapTableEntries<'a> {
    fn new(node: MapNode<'a>, repr: MapTableEntryRepr<'a>) -> Self {
        Self {
            node,
            reprs: OneVec::new(repr),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MapTableEntryRepr<'a> {
    pub key: MapTableKeyRepr<'a>,
    pub kind: MapTableEntryReprKind<'a>,
}

impl<'a> MapTableEntryRepr<'a> {
    fn new(key: MapTableKeyRepr<'a>, kind: MapTableEntryReprKind<'a>) -> Self {
        Self { key, kind }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MapTableEntryReprKind<'a> {
    Table(&'a Table<'a>),
    ArrayEntry(&'a ArrayEntry<'a>),
    ToplevelAssignment(&'a Assignment<'a>),
    InlineTableAssignment(&'a InlineTableAssignment<'a>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MapTableKeyRepr<'a> {
    One(&'a Ident<'a>),
    Dotted(u32, &'a [DottedIdent<'a>]),
}

impl<'a> MapTableKeyRepr<'a> {
    pub fn referenced_ident(&self) -> &'a Ident<'a> {
        match self {
            MapTableKeyRepr::One(i) => i,
            MapTableKeyRepr::Dotted(idx, idents) => &idents[*idx as usize].ident,
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
    pub fn new(node: MapTable<'a>, repr: &'a ArrayEntry<'a>) -> Self {
        Self {
            inner: OneVec::new(MapArrayToplevelEntry::new(node, repr)),
        }
    }

    fn push(&mut self, node: MapTable<'a>, repr: &'a ArrayEntry<'a>) {
        self.inner.push(MapArrayToplevelEntry::new(node, repr));
    }
}

#[derive(Debug, PartialEq)]
pub struct MapArrayToplevelEntry<'a> {
    pub node: MapTable<'a>,
    pub repr: &'a ArrayEntry<'a>,
}

impl<'a> MapArrayToplevelEntry<'a> {
    pub fn new(node: MapTable<'a>, repr: &'a ArrayEntry<'a>) -> Self {
        Self { node, repr }
    }
}

#[derive(Debug, PartialEq)]
pub struct MapArrayInline<'a> {
    pub repr: &'a InlineArray<'a>,
    inner: Vec<MapArrayInlineEntry<'a>>,
}

impl<'a> MapArrayInline<'a> {
    pub fn new(repr: &'a InlineArray<'a>) -> Self {
        Self {
            repr,
            inner: Vec::new(),
        }
    }

    pub fn from_iter(
        repr: &'a InlineArray<'a>,
        iter: impl Iterator<Item = MapArrayInlineEntry<'a>>,
    ) -> Self {
        Self {
            repr,
            inner: iter.collect(),
        }
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

impl Ctx {
    pub fn map<'a>(&mut self, asts: &'a [Ast<'a>]) -> MapTable<'a> {
        let mut root = MapTable::new();
        for a in asts {
            match a {
                Ast::Assignment(assignment) => {
                    let node = self.map_value(&assignment.val);
                    let repr_kind = MapTableEntryReprKind::ToplevelAssignment(assignment);
                    self.insert_node(&mut root, &assignment.key, node, repr_kind);
                }
                Ast::Table(table) => {
                    if let Some(key) = &table.header.key {
                        let mut map = MapTable::new();
                        for assignment in table.assignments.iter() {
                            let node = self.map_value(&assignment.val);
                            let repr_kind = MapTableEntryReprKind::ToplevelAssignment(assignment);
                            self.insert_node(&mut map, &assignment.key, node, repr_kind);
                        }
                        let repr_kind = MapTableEntryReprKind::Table(table);
                        self.insert_node(&mut root, key, MapNode::Table(map), repr_kind);
                    } else {
                        for assignment in table.assignments.iter() {
                            todo!("maybe push some sort of unused warning or hint");
                        }
                    }
                }
                Ast::Array(array_entry) => {
                    if let Some(key) = &array_entry.header.key {
                        let mut map = MapTable::new();
                        for assignment in array_entry.assignments.iter() {
                            let node = self.map_value(&assignment.val);
                            let repr_kind = MapTableEntryReprKind::ToplevelAssignment(assignment);
                            self.insert_node(&mut map, &assignment.key, node, repr_kind);
                        }

                        self.insert_array_entry(&mut root, key, map, array_entry);
                    }
                }
                Ast::Comment(_) => (),
            }
        }
        root
    }

    fn map_value<'a>(&mut self, value: &'a Value<'a>) -> MapNode<'a> {
        match value {
            Value::String(s) => MapNode::Scalar(Scalar::String(s)),
            Value::Int(i) => MapNode::Scalar(Scalar::Int(i)),
            Value::Float(f) => MapNode::Scalar(Scalar::Float(f)),
            Value::Bool(b) => MapNode::Scalar(Scalar::Bool(b)),
            Value::DateTime(d) => MapNode::Scalar(Scalar::DateTime(d)),
            Value::InlineTable(table) => {
                let mut map = MapTable::new();
                for assignment in table.assignments.iter() {
                    let node = self.map_value(&assignment.val);
                    let repr_kind = MapTableEntryReprKind::InlineTableAssignment(assignment);
                    self.insert_node(&mut map, &assignment.key, node, repr_kind);
                }
                MapNode::Table(map)
            }
            Value::InlineArray(inline_array) => {
                let entries = inline_array.values.iter().map(|value| {
                    let node = self.map_value(&value.val);
                    MapArrayInlineEntry::new(node, value)
                });
                let array = MapArrayInline::from_iter(inline_array, entries);
                MapNode::Array(MapArray::Inline(array))
            }
            Value::Invalid(s, r) => MapNode::Scalar(Scalar::Invalid(s, *r)),
        }
    }

    fn insert_node<'a>(
        &mut self,
        map: &mut MapTable<'a>,
        key: &'a Key<'a>,
        node: MapNode<'a>,
        repr_kind: MapTableEntryReprKind<'a>,
    ) {
        if let Err(e) = insert_node(map, key, node, repr_kind) {
            self.errors.push(e);
        }
    }

    fn insert_array_entry<'a>(
        &mut self,
        map: &mut MapTable<'a>,
        key: &'a Key<'a>,
        node: MapTable<'a>,
        array_entry: &'a ArrayEntry<'a>,
    ) {
        if let Err(e) = insert_array_entry(map, key, node, array_entry) {
            self.errors.push(e);
        }
    }
}

enum ReprKind {
    Table,
    Assigment,
}

fn insert_node<'a>(
    map: &mut MapTable<'a>,
    key: &'a Key<'a>,
    node: MapNode<'a>,
    repr_kind: MapTableEntryReprKind<'a>,
) -> Result<(), Error> {
    match &key {
        Key::One(i) => {
            let key_repr = MapTableKeyRepr::One(i);
            let repr = MapTableEntryRepr::new(key_repr, repr_kind);
            map.insert(i.text(), node, repr)
        }
        Key::Dotted(idents) => {
            let [other @ .., last] = idents.as_slice() else {
                unreachable!()
            };
            let mut current = map;
            for (i, o) in other.iter().enumerate() {
                match current.entry(o.ident.text()) {
                    MapEntry::Occupied(mut occupied) => {
                        let entries = occupied.get_mut();
                        let next = match &mut entries.node {
                            MapNode::Table(t) => t,
                            MapNode::Array(_) => todo!("error"),
                            MapNode::Scalar(_) => todo!("error"),
                        };

                        // TODO: collision detection depending on what is inserted
                        for repr in entries.reprs.iter() {
                            match repr.kind {
                                MapTableEntryReprKind::Table(_) => todo!(),
                                MapTableEntryReprKind::ArrayEntry(_) => todo!(),
                                MapTableEntryReprKind::ToplevelAssignment(_) => todo!(),
                                MapTableEntryReprKind::InlineTableAssignment(_) => todo!(),
                            }
                        }

                        let key_repr = MapTableKeyRepr::Dotted(i as u32, idents);
                        let repr = MapTableEntryRepr::new(key_repr, repr_kind);
                        entries.reprs.push(repr);
                        current = next;
                    }
                    MapEntry::Vacant(vacant) => {
                        let mut node = node;
                        for (j, o) in (&idents[(i + 1)..]).iter().enumerate().rev() {
                            let key_idx = (i + 1 + j) as u32;
                            let key_repr = MapTableKeyRepr::Dotted(key_idx, idents);
                            let repr = MapTableEntryRepr::new(key_repr, repr_kind);
                            node = MapNode::Table(MapTable::from_pairs([(
                                o.ident.text(),
                                MapTableEntries::new(node, repr),
                            )]));
                        }

                        let key_repr = MapTableKeyRepr::Dotted(i as u32, idents);
                        let repr = MapTableEntryRepr::new(key_repr, repr_kind);
                        vacant.insert(node, repr);
                        return Ok(());
                    }
                }
            }

            let key_repr = MapTableKeyRepr::Dotted((idents.len() - 1) as u32, idents);
            let repr = MapTableEntryRepr::new(key_repr, repr_kind);
            current.insert(last.ident.text(), node, repr)
        }
    }
}

fn insert_array_entry<'a>(
    map: &mut MapTable<'a>,
    key: &'a Key<'a>,
    node: MapTable<'a>,
    array_entry: &'a ArrayEntry<'a>,
) -> Result<(), Error> {
    fn insert<'a>(
        map: &mut MapTable<'a>,
        ident: &'a str,
        node: MapTable<'a>,
        key_repr: MapTableKeyRepr<'a>,
        array_entry: &'a ArrayEntry<'a>,
    ) -> Result<(), Error> {
        let repr_kind = MapTableEntryReprKind::ArrayEntry(array_entry);
        let repr = MapTableEntryRepr::new(key_repr, repr_kind);

        match map.entry(ident) {
            MapEntry::Occupied(mut e) => {
                let entries = e.get_mut();
                let array = match &mut entries.node {
                    MapNode::Array(a) => a,
                    MapNode::Table(_) => todo!("error"),
                    MapNode::Scalar(_) => todo!("error"),
                };

                match array {
                    MapArray::Toplevel(toplevel_array) => {
                        entries.reprs.push(repr);
                        toplevel_array.push(node, array_entry);
                    }
                    MapArray::Inline(_) => todo!("error"),
                }
            }
            MapEntry::Vacant(vacant) => {
                let toplevel_array = MapArrayToplevel::new(node, array_entry);
                let node = MapNode::Array(MapArray::Toplevel(toplevel_array));
                vacant.insert(node, repr);
            }
        }

        Ok(())
    }

    match key {
        Key::One(i) => {
            let key_repr = MapTableKeyRepr::One(i);
            insert(map, i.text(), node, key_repr, array_entry)
        }
        Key::Dotted(idents) => {
            let [other @ .., last] = idents.as_slice() else {
                unreachable!()
            };
            let mut current = map;
            for (i, o) in other.iter().enumerate() {
                match current.entry(o.ident.text()) {
                    MapEntry::Occupied(mut occupied) => {
                        let entries = occupied.get_mut();
                        let next = match &mut entries.node {
                            MapNode::Table(t) => t,
                            MapNode::Array(_) => todo!(),
                            MapNode::Scalar(_) => todo!(),
                        };

                        // TODO: collision check
                        for repr in entries.reprs.iter() {
                            match &repr.kind {
                                MapTableEntryReprKind::Table(_) => todo!(),
                                MapTableEntryReprKind::ArrayEntry(_) => todo!(),
                                MapTableEntryReprKind::ToplevelAssignment(_) => todo!(),
                                MapTableEntryReprKind::InlineTableAssignment(_) => todo!(),
                            }
                        }

                        let key_repr = MapTableKeyRepr::Dotted(i as u32, idents);
                        let repr_kind = MapTableEntryReprKind::ArrayEntry(array_entry);
                        let repr = MapTableEntryRepr::new(key_repr, repr_kind);
                        entries.reprs.push(repr);
                        current = next;
                    }
                    MapEntry::Vacant(vacant) => {
                        let toplevel_array = MapArrayToplevel::new(node, array_entry);
                        let mut node = MapNode::Array(MapArray::Toplevel(toplevel_array));

                        let key_idx_offset = i + 1;
                        for (j, o) in (&idents[key_idx_offset..]).iter().enumerate().rev() {
                            let key_idx = (key_idx_offset + j) as u32;
                            let key_repr = MapTableKeyRepr::Dotted(key_idx, idents);
                            let repr_kind = MapTableEntryReprKind::ArrayEntry(array_entry);
                            let repr = MapTableEntryRepr::new(key_repr, repr_kind);
                            node = MapNode::Table(MapTable::from_pairs([(
                                o.ident.text(),
                                MapTableEntries::new(node, repr),
                            )]));
                        }

                        let key_repr = MapTableKeyRepr::Dotted(i as u32, idents);
                        let repr_kind = MapTableEntryReprKind::ArrayEntry(array_entry);
                        let repr = MapTableEntryRepr::new(key_repr, repr_kind);
                        vacant.insert(node, repr);
                        return Ok(());
                    }
                }
            }

            let key_repr = MapTableKeyRepr::Dotted((idents.len() - 1) as u32, idents);
            insert(current, last.ident.text(), node, key_repr, array_entry)
        }
    }
}
