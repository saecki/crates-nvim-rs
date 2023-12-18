use std::collections::hash_map::Entry::*;
use std::collections::HashMap;

use crate::onevec::OneVec;
use crate::parse::{
    ArrayEntry, Assignment, Ast, BoolVal, DateTimeVal, DottedIdent, FloatVal, Ident, InlineArray,
    InlineArrayValue, InlineTableAssignment, IntVal, Key, StringVal, Table, Value,
};
use crate::{Ctx, Error, Span};

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

    pub fn from_pairs(pairs: impl IntoIterator<Item = (&'a str, MapTableEntries<'a>)>) -> Self {
        Self {
            inner: HashMap::from_iter(pairs),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&&str, &MapTableEntries<'a>)> {
        self.inner.iter()
    }
}

impl<'a> IntoIterator for MapTable<'a> {
    type Item = (&'a str, MapTableEntries<'a>);
    type IntoIter = std::collections::hash_map::IntoIter<&'a str, MapTableEntries<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

#[derive(Debug, PartialEq)]
pub struct MapTableEntries<'a> {
    pub node: MapNode<'a>,
    /// References to the actual representations inside the toml file.
    pub reprs: OneVec<MapTableEntryRepr<'a>>,
}

impl<'a> MapTableEntries<'a> {
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
        iter: impl IntoIterator<Item = MapArrayInlineEntry<'a>>,
    ) -> Self {
        Self {
            repr,
            inner: Vec::from_iter(iter),
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
                        // TODO: maybe push some sort of unused warning or hint
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
                    let node = self.map_value(&assignment.assignment.val);
                    let repr_kind = MapTableEntryReprKind::InlineTableAssignment(assignment);
                    self.insert_node(&mut map, &assignment.assignment.key, node, repr_kind);
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
        let idents = match &key {
            Key::One(i) => {
                let key_repr = MapTableKeyRepr::One(i);
                let repr = MapTableEntryRepr::new(key_repr, repr_kind);
                if let Err(e) = self.insert_final_node(map, i.text(), node, repr) {
                    self.errors.push(e);
                }
                return;
            }
            Key::Dotted(idents) => idents,
        };

        let [other @ .., last] = idents.as_slice() else {
            unreachable!()
        };
        let mut current = map;
        for (i, o) in other.iter().enumerate() {
            let entries = match current.inner.entry(o.ident.text()) {
                Occupied(occupied) => occupied.into_mut(),
                Vacant(vacant) => {
                    let mut node = node;
                    for (j, o) in idents[(i + 1)..].iter().enumerate().rev() {
                        let key_idx = (i + 1 + j) as u32;
                        let key_repr = MapTableKeyRepr::Dotted(key_idx, idents);
                        let repr = MapTableEntryRepr::new(key_repr, repr_kind);
                        node = MapNode::Table(MapTable::from_pairs([(
                            o.ident.text(),
                            MapTableEntries::from_one(node, repr),
                        )]));
                    }

                    let key_repr = MapTableKeyRepr::Dotted(i as u32, idents);
                    let repr = MapTableEntryRepr::new(key_repr, repr_kind);
                    vacant.insert(MapTableEntries::from_one(node, repr));
                    return;
                }
            };

            let next = match get_table_to_extend(&mut entries.node, &entries.reprs, &o.ident) {
                Ok(t) => t,
                Err(e) => return self.errors.push(e),
            };

            let key_repr = MapTableKeyRepr::Dotted(i as u32, idents);
            let repr = MapTableEntryRepr::new(key_repr, repr_kind);
            entries.reprs.push(repr);
            current = next;
        }

        let key_repr = MapTableKeyRepr::Dotted((idents.len() - 1) as u32, idents);
        let repr = MapTableEntryRepr::new(key_repr, repr_kind);

        if let Err(e) = self.insert_final_node(current, last.ident.text(), node, repr) {
            self.errors.push(e);
        }
    }

    fn insert_final_node<'a>(
        &mut self,
        map: &mut MapTable<'a>,
        key: &'a str,
        node: MapNode<'a>,
        repr: MapTableEntryRepr<'a>,
    ) -> Result<(), Error> {
        use std::collections::hash_map::Entry::*;

        let existing_entries = match map.inner.entry(key) {
            Occupied(occupied) => occupied.into_mut(),
            Vacant(vacant) => {
                vacant.insert(MapTableEntries::from_one(node, repr));
                return Ok(());
            }
        };

        if !matches!(repr.kind, MapTableEntryReprKind::Table(_)) {
            return Err(duplicate_key_error(key, existing_entries, &repr.key));
        }
        let MapNode::Table(new_table) = node else {
            unreachable!()
        };

        let existing_table = match &mut existing_entries.node {
            MapNode::Table(t) => t,
            MapNode::Array(_) | MapNode::Scalar(_) => {
                return Err(duplicate_key_error(key, existing_entries, &repr.key));
            }
        };

        for existing_repr in existing_entries.reprs.iter() {
            match existing_repr.kind {
                MapTableEntryReprKind::Table(_) if !existing_repr.key.is_last_ident() => {
                    // allow super tables, that are declared out of order
                }
                MapTableEntryReprKind::Table(_)
                | MapTableEntryReprKind::ArrayEntry(_)
                | MapTableEntryReprKind::ToplevelAssignment(_)
                | MapTableEntryReprKind::InlineTableAssignment(_) => {
                    return Err(duplicate_key_error(key, existing_entries, &repr.key))
                }
            }
        }

        // extend existing table with items from super table
        for (k, e) in new_table.into_iter() {
            match existing_table.inner.entry(k) {
                Occupied(_) => {
                    let err = Error::DuplicateKey(
                        key.to_string(),
                        existing_entries.reprs.first().key.repr_ident().lit_span,
                        repr.key.repr_ident().lit_span,
                    );
                    self.errors.push(err);
                }
                Vacant(vacant) => {
                    vacant.insert(MapTableEntries::new(e.node, e.reprs));
                }
            }
        }
        existing_entries.reprs.push(repr);

        Ok(())
    }

    fn insert_array_entry<'a>(
        &mut self,
        map: &mut MapTable<'a>,
        key: &'a Key<'a>,
        node: MapTable<'a>,
        array_entry: &'a ArrayEntry<'a>,
    ) {
        let idents = match key {
            Key::One(i) => {
                let key_repr = MapTableKeyRepr::One(i);
                let res = self.insert_final_array_entry(map, i.text(), node, key_repr, array_entry);
                if let Err(e) = res {
                    self.errors.push(e);
                }
                return;
            }
            Key::Dotted(idents) => idents,
        };

        let [other @ .., last] = idents.as_slice() else {
            unreachable!()
        };
        let mut current = map;
        for (i, o) in other.iter().enumerate() {
            let entries = match current.inner.entry(o.ident.text()) {
                Occupied(occupied) => occupied.into_mut(),
                Vacant(vacant) => {
                    let toplevel_array = MapArrayToplevel::new(node, array_entry);
                    let mut node = MapNode::Array(MapArray::Toplevel(toplevel_array));

                    let key_idx_offset = i + 1;
                    for (j, o) in idents[key_idx_offset..].iter().enumerate().rev() {
                        let key_idx = (key_idx_offset + j) as u32;
                        let key_repr = MapTableKeyRepr::Dotted(key_idx, idents);
                        let repr_kind = MapTableEntryReprKind::ArrayEntry(array_entry);
                        let repr = MapTableEntryRepr::new(key_repr, repr_kind);
                        node = MapNode::Table(MapTable::from_pairs([(
                            o.ident.text(),
                            MapTableEntries::from_one(node, repr),
                        )]));
                    }

                    let key_repr = MapTableKeyRepr::Dotted(i as u32, idents);
                    let repr_kind = MapTableEntryReprKind::ArrayEntry(array_entry);
                    let repr = MapTableEntryRepr::new(key_repr, repr_kind);
                    vacant.insert(MapTableEntries::from_one(node, repr));
                    return;
                }
            };

            let next = match get_table_to_extend(&mut entries.node, &entries.reprs, &o.ident) {
                Ok(t) => t,
                Err(e) => return self.errors.push(e),
            };

            let key_repr = MapTableKeyRepr::Dotted(i as u32, idents);
            let repr_kind = MapTableEntryReprKind::ArrayEntry(array_entry);
            let repr = MapTableEntryRepr::new(key_repr, repr_kind);
            entries.reprs.push(repr);
            current = next;
        }

        let key_repr = MapTableKeyRepr::Dotted((idents.len() - 1) as u32, idents);
        if let Err(e) =
            self.insert_final_array_entry(current, last.ident.text(), node, key_repr, array_entry)
        {
            self.errors.push(e);
        }
    }

    fn insert_final_array_entry<'a>(
        &mut self,
        map: &mut MapTable<'a>,
        key: &'a str,
        node: MapTable<'a>,
        key_repr: MapTableKeyRepr<'a>,
        array_entry: &'a ArrayEntry<'a>,
    ) -> Result<(), Error> {
        let repr_kind = MapTableEntryReprKind::ArrayEntry(array_entry);
        let repr = MapTableEntryRepr::new(key_repr, repr_kind);

        match map.inner.entry(key) {
            Occupied(occupied) => {
                let entries = occupied.into_mut();
                let array = match &mut entries.node {
                    MapNode::Array(MapArray::Toplevel(a)) => a,
                    MapNode::Array(MapArray::Inline(_)) => {
                        return Err(Error::CannotExtendInlineArray(
                            key.to_string(),
                            entries.reprs.first().key.repr_ident().lit_span,
                            repr.key.repr_ident().lit_span,
                        ));
                    }
                    MapNode::Table(_) | MapNode::Scalar(_) => {
                        return Err(Error::DuplicateKey(
                            key.to_string(),
                            entries.reprs.first().key.repr_ident().lit_span,
                            repr.key.repr_ident().lit_span,
                        ));
                    }
                };

                entries.reprs.push(repr);
                array.push(node, array_entry);
            }
            Vacant(vacant) => {
                let toplevel_array = MapArrayToplevel::new(node, array_entry);
                let node = MapNode::Array(MapArray::Toplevel(toplevel_array));
                vacant.insert(MapTableEntries::from_one(node, repr));
            }
        }

        Ok(())
    }
}

fn get_table_to_extend<'a, 'b>(
    node: &'b mut MapNode<'a>,
    reprs: &OneVec<MapTableEntryRepr<'a>>,
    ident: &'b Ident<'a>,
) -> Result<&'b mut MapTable<'a>, Error>
where
    'a: 'b,
{
    let next = match node {
        MapNode::Table(t) => t,
        MapNode::Array(MapArray::Toplevel(t)) => {
            // From the toml spec (https://toml.io/en/v1.0.0#array-of-tables):
            // Any reference to an array of tables points to the most recently
            // defined table element of the array. This allows you to define
            // sub-tables, and even sub-arrays of tables, inside the most recent
            // table.
            &mut t.inner.last_mut().node
        }
        MapNode::Array(MapArray::Inline(_)) => {
            let key = ident.text.to_string();
            let orig = reprs.first().key.repr_ident().lit_span;
            let new = ident.lit_span;
            return Err(Error::CannotExtendInlineArray(key, orig, new));
        }
        MapNode::Scalar(_) => {
            let key = ident.text.to_string();
            let orig = reprs.first().key.repr_ident().lit_span;
            let new = ident.lit_span;
            return Err(Error::DuplicateKey(key, orig, new));
        }
    };

    for existing in reprs.iter() {
        match &existing.kind {
            MapTableEntryReprKind::Table(_) => (),
            MapTableEntryReprKind::ArrayEntry(_) => (),
            MapTableEntryReprKind::ToplevelAssignment(_) => {
                if existing.key.is_last_ident() {
                    // `next` is an inline table
                    let key = ident.text.to_string();
                    let orig = existing.key.repr_ident().lit_span;
                    let new = ident.lit_span;
                    return Err(Error::CannotExtendInlineTable(key, orig, new));
                }
            }
            MapTableEntryReprKind::InlineTableAssignment(_) => {
                // we're inside an inline table, which can't be extended
                let key = ident.text.to_string();
                let orig = existing.key.repr_ident().lit_span;
                let new = ident.lit_span;
                return Err(Error::CannotExtendInlineTable(key, orig, new));
            }
        }
    }

    Ok(next)
}

fn duplicate_key_error(
    key: &str,
    entries: &MapTableEntries<'_>,
    duplicate: &MapTableKeyRepr<'_>,
) -> Error {
    Error::DuplicateKey(
        key.to_string(),
        entries.reprs.first().key.repr_ident().lit_span,
        duplicate.repr_ident().lit_span,
    )
}
