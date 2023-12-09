use std::collections::HashMap;

use crate::toml::{
    Ast, BoolVal, Ctx, DateTimeVal, Error, FloatVal, Ident, IntVal, Key, Span, StringVal, Value,
};

#[cfg(test)]
mod test;

#[derive(Debug, PartialEq)]
pub struct MapTable<'a> {
    kind: TableKind,
    inner: HashMap<&'a str, MapEntry<'a>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TableKind {
    /// The root table of the file.
    Root,
    /// A top level table.
    TopLevel,
    /// An inline table.
    Inline,
    /// Not actually a table, but a dotted key.
    DottedKey,
}

impl<'a> MapTable<'a> {
    pub fn new(kind: TableKind) -> Self {
        Self {
            kind,
            inner: HashMap::new(),
        }
    }

    pub fn from_pairs(
        kind: TableKind,
        pairs: impl IntoIterator<Item = (&'a str, MapEntry<'a>)>,
    ) -> Self {
        Self {
            kind,
            inner: HashMap::from_iter(pairs),
        }
    }

    pub fn insert(&mut self, key: &'a str, entry: MapEntry<'a>) -> Result<(), Error> {
        match self.inner.get_mut(key) {
            Some(entry) => {
                todo!("error {entry:?}")
            }
            None => {
                self.inner.insert(key, entry);
            }
        }
        Ok(())
    }

    pub fn get(&'a self, key: &str) -> Option<&'a MapEntry<'a>> {
        self.inner.get(key)
    }

    pub fn get_mut<'b>(&'b mut self, key: &str) -> Option<&'b mut MapEntry<'a>>
    where
        'a: 'b,
    {
        self.inner.get_mut(key)
    }

    pub fn get_or_insert_with<'b>(
        &'b mut self,
        key: &'a str,
        default: impl FnOnce() -> MapEntry<'a>,
    ) -> &'b mut MapEntry<'a>
    where
        'a: 'b,
    {
        self.inner.entry(key).or_insert_with(default)
    }
}

#[derive(Debug, PartialEq)]
pub struct MapArray<'a> {
    kind: ArrayKind,
    inner: Vec<MapEntry<'a>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ArrayKind {
    /// A toplevel array.
    TopLevel,
    /// An inline array.
    Inline,
}

impl<'a> MapArray<'a> {
    pub fn new(kind: ArrayKind) -> Self {
        Self {
            kind,
            inner: Vec::new(),
        }
    }

    pub fn from_iter(kind: ArrayKind, iter: impl IntoIterator<Item = MapEntry<'a>>) -> Self {
        Self {
            kind,
            inner: Vec::from_iter(iter),
        }
    }

    pub fn push(&mut self, entry: MapEntry<'a>) {
        self.inner.push(entry);
    }
}

#[derive(Debug, PartialEq)]
pub enum MapEntry<'a> {
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
        let mut root = MapTable::new(TableKind::Root);
        for a in asts {
            match a {
                Ast::Assignment(assignment) => {
                    let entry = self.map_value(&assignment.val);
                    self.insert_entry(&mut root, &assignment.key, entry);
                }
                Ast::Table(table) => {
                    let mut map = MapTable::new(TableKind::TopLevel);
                    for assignment in table.assignments.iter() {
                        let entry = self.map_value(&assignment.val);
                        self.insert_entry(&mut map, &assignment.key, entry);
                    }
                    if let Some(key) = &table.header.key {
                        self.insert_entry(&mut root, key, MapEntry::Table(map));
                    }
                }
                Ast::Array(array_entry) => {
                    let mut map = MapTable::new(TableKind::TopLevel);
                    for assignment in array_entry.assignments.iter() {
                        let entry = self.map_value(&assignment.val);
                        self.insert_entry(&mut map, &assignment.key, entry);
                    }
                    if let Some(key) = &array_entry.header.key {
                        self.insert_array_entry(&mut root, key, MapEntry::Table(map));
                    }
                }
                Ast::Comment(_) => (),
            }
        }
        root
    }

    fn map_value<'a>(&mut self, value: &'a Value<'a>) -> MapEntry<'a> {
        match value {
            Value::String(s) => MapEntry::Scalar(Scalar::String(s)),
            Value::Int(i) => MapEntry::Scalar(Scalar::Int(i)),
            Value::Float(f) => MapEntry::Scalar(Scalar::Float(f)),
            Value::Bool(b) => MapEntry::Scalar(Scalar::Bool(b)),
            Value::DateTime(d) => MapEntry::Scalar(Scalar::DateTime(d)),
            Value::InlineTable(table) => {
                let mut map = MapTable::new(TableKind::Inline);
                for assignment in table.assignments.iter() {
                    let entry = self.map_value(&assignment.val);
                    self.insert_entry(&mut map, &assignment.key, entry);
                }
                MapEntry::Table(map)
            }
            Value::InlineArray(inline_array) => {
                let mut array = MapArray::new(ArrayKind::Inline);
                for value in inline_array.values.iter() {
                    let entry = self.map_value(&value.val);
                    array.push(entry);
                }
                MapEntry::Array(array)
            }
            Value::Invalid(s, r) => MapEntry::Scalar(Scalar::Invalid(s, *r)),
        }
    }

    fn insert_entry<'a>(&mut self, map: &mut MapTable<'a>, key: &'a Key<'a>, entry: MapEntry<'a>) {
        if let Err(e) = insert_entry(map, key, entry) {
            self.errors.push(e);
        }
    }

    fn insert_array_entry<'a>(
        &mut self,
        map: &mut MapTable<'a>,
        key: &'a Key<'a>,
        entry: MapEntry<'a>,
    ) {
        if let Err(e) = insert_array_entry(map, key, entry) {
            self.errors.push(e);
        }
    }
}

fn insert_entry<'a>(
    map: &mut MapTable<'a>,
    key: &'a Key<'a>,
    entry: MapEntry<'a>,
) -> Result<(), Error> {
    match key {
        Key::One(i) => map.insert(i.text.as_ref(), entry),
        Key::Dotted(idents) => {
            let [other @ .., last] = idents.as_slice() else {
                unreachable!()
            };
            let mut current = map;
            for o in other.iter() {
                let e = current.get_or_insert_with(o.ident.text.as_ref(), || {
                    MapEntry::Table(MapTable::new(TableKind::DottedKey))
                });

                let next = match e {
                    MapEntry::Table(t) => t,
                    MapEntry::Array(_) => todo!(),
                    MapEntry::Scalar(_) => todo!(),
                };

                match next.kind {
                    TableKind::Root => (),
                    TableKind::TopLevel => (),
                    TableKind::Inline => todo!("error"),
                    TableKind::DottedKey => (),
                }

                current = next;
            }

            current.insert(last.ident.text.as_ref(), entry)
        }
    }
}

fn insert_array_entry<'a>(
    map: &mut MapTable<'a>,
    key: &'a Key<'a>,
    entry: MapEntry<'a>,
) -> Result<(), Error> {
    fn insert<'a>(
        map: &mut MapTable<'a>,
        ident: &'a Ident<'a>,
        entry: MapEntry<'a>,
    ) -> Result<(), Error> {
        match map.get_mut(ident.text.as_ref()) {
            Some(e) => {
                let array = match e {
                    MapEntry::Table(_) => todo!("error"),
                    MapEntry::Array(a) => a,
                    MapEntry::Scalar(_) => todo!("error"),
                };

                match array.kind {
                    ArrayKind::TopLevel => (),
                    ArrayKind::Inline => todo!("error"),
                }

                array.push(entry);
                Ok(())
            }
            None => map.insert(
                ident.text.as_ref(),
                MapEntry::Array(MapArray::from_iter(ArrayKind::TopLevel, [entry])),
            ),
        }
    }

    match key {
        Key::One(i) => insert(map, i, entry),
        Key::Dotted(idents) => {
            let [other @ .., last] = idents.as_slice() else {
                unreachable!()
            };
            let mut current = map;
            for o in other.iter() {
                let e = current.get_or_insert_with(o.ident.text.as_ref(), || {
                    MapEntry::Table(MapTable::new(TableKind::DottedKey))
                });

                let next = match e {
                    MapEntry::Table(t) => t,
                    MapEntry::Array(_) => todo!(),
                    MapEntry::Scalar(_) => todo!(),
                };

                match next.kind {
                    TableKind::Root => (),
                    TableKind::TopLevel => (),
                    TableKind::Inline => todo!("error"),
                    TableKind::DottedKey => (),
                }

                current = next;
            }

            insert(current, &last.ident, entry)
        }
    }
}
