use std::collections::HashMap;

use crate::toml::{Ast, BoolVal, Ctx, DateTimeVal, FloatVal, IntVal, Key, Range, StringVal, Value};

pub struct MapTable<'a> {
    inner: HashMap<&'a str, MapEntry<'a>>,
}

impl<'a> MapTable<'a> {
    pub fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: &'a str, entry: MapEntry<'a>) {
        self.inner.insert(key, entry);
    }
}

pub struct MapArray<'a> {
    inner: Vec<MapEntry<'a>>,
}

impl<'a> MapArray<'a> {
    pub fn new() -> Self {
        Self { inner: Vec::new() }
    }

    pub fn push(&mut self, entry: MapEntry<'a>) {
        self.inner.push(entry);
    }
}

pub enum MapEntry<'a> {
    Table(MapTable<'a>),
    Array(MapArray<'a>),
    Scalar(Scalar<'a>),
}

pub enum Scalar<'a> {
    String(&'a StringVal<'a>),
    Int(&'a IntVal<'a>),
    Float(&'a FloatVal<'a>),
    Bool(&'a BoolVal),
    DateTime(&'a DateTimeVal<'a>),
    Invalid(&'a str, Range),
}

impl Ctx {
    pub fn map<'a>(&mut self, asts: &'a [Ast<'a>]) -> MapTable<'a> {
        let mut root = MapTable::new();
        for a in asts {
            match a {
                Ast::Assignment(assignment) => {
                    let entry = self.map_value(&assignment.val);
                    insert_entry(&mut root, &assignment.key, entry);
                }
                Ast::Table(table) => {
                    let mut map = MapTable::new();
                    for assignment in table.assignments.iter() {
                        let entry = self.map_value(&assignment.val);
                        insert_entry(&mut map, &assignment.key, entry);
                    }
                    if let Some(key) = &table.header.key {
                        insert_entry(&mut root, key, MapEntry::Table(map));
                    }
                }
                Ast::Array(array_entry) => {
                    let mut map = MapTable::new();
                    for assignment in array_entry.assignments.iter() {
                        let entry = self.map_value(&assignment.val);
                        insert_entry(&mut map, &assignment.key, entry);
                    }
                    // TODO: find existing array
                    let mut array = MapArray::new();
                    array.push(MapEntry::Table(map));
                    if let Some(key) = &array_entry.header.key {
                        insert_entry(&mut root, key, MapEntry::Array(array));
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
                let mut map = MapTable::new();
                for assignment in table.assignments.iter() {
                    let entry = self.map_value(&assignment.val);
                    insert_entry(&mut map, &assignment.key, entry);
                }
                MapEntry::Table(map)
            }
            Value::InlineArray(inline_array) => {
                let mut array = MapArray::new();
                for value in inline_array.values.iter() {
                    let entry = self.map_value(&value.val);
                    array.push(entry);
                }
                MapEntry::Array(array)
            }
            Value::Invalid(s, r) => MapEntry::Scalar(Scalar::Invalid(s, *r)),
        }
    }
}

// TODO: key collision checks
fn insert_entry<'a>(root: &mut MapTable<'a>, key: &'a Key<'a>, entry: MapEntry<'a>) {
    match key {
        Key::One(i) => {
            root.insert(i.text.as_ref(), entry);
        }
        Key::Dotted(idents) => {
            let [first, other @ .., last] = idents.as_slice() else {
                unreachable!("dotted ident should contains at least 2 elements")
            };

            let mut map = MapTable::new();
            map.insert(last.ident.text.as_ref(), entry);

            for i in other.iter().rev() {
                let mut new_map = MapTable::new();
                let entry = MapEntry::Table(map);
                new_map.insert(i.ident.text.as_ref(), entry);
                map = new_map;
            }

            let entry = MapEntry::Table(MapTable::from(map));
            root.insert(first.ident.text.as_ref(), entry);
        }
    }
}
