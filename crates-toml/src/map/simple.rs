use std::collections::HashMap;

use crate::datetime::DateTime;
use crate::map::{MapArray, MapNode, MapTable, Scalar};

#[derive(PartialEq)]
pub enum SimpleVal {
    Table(HashMap<String, SimpleVal>),
    Array(Vec<SimpleVal>),
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    DateTime(DateTime),
    Invalid(String),
}

impl std::fmt::Debug for SimpleVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SimpleVal::Table(t) => f.debug_map().entries(t.iter()).finish(),
            SimpleVal::Array(a) => f.debug_list().entries(a.iter()).finish(),
            SimpleVal::String(s) => std::fmt::Debug::fmt(s, f),
            SimpleVal::Int(s) => std::fmt::Debug::fmt(s, f),
            SimpleVal::Float(s) => std::fmt::Debug::fmt(s, f),
            SimpleVal::Bool(s) => std::fmt::Debug::fmt(s, f),
            SimpleVal::DateTime(s) => std::fmt::Debug::fmt(s, f),
            SimpleVal::Invalid(s) => f.debug_tuple("Invalid").field(s).finish(),
        }
    }
}

pub fn map_table(map: MapTable) -> HashMap<String, SimpleVal> {
    let iter = map
        .into_iter()
        .map(|(k, e)| (k.to_string(), map_val(e.node)));
    HashMap::from_iter(iter)
}

pub fn map_val(node: MapNode) -> SimpleVal {
    match node {
        MapNode::Table(t) => SimpleVal::Table(map_table(t)),
        MapNode::Array(MapArray::Toplevel(a)) => SimpleVal::Array(
            a.inner
                .into_iter()
                .map(|e| SimpleVal::Table(map_table(e.node)))
                .collect(),
        ),
        MapNode::Array(MapArray::Inline(a)) => {
            SimpleVal::Array(a.inner.into_iter().map(|e| map_val(e.node)).collect())
        }
        MapNode::Scalar(s) => match s {
            Scalar::String(s) => SimpleVal::String(s.text.to_string()),
            Scalar::Int(i) => SimpleVal::Int(i.val),
            Scalar::Float(f) => SimpleVal::Float(f.val),
            Scalar::Bool(b) => SimpleVal::Bool(b.val),
            Scalar::DateTime(d) => SimpleVal::DateTime(d.val),
            Scalar::Invalid(i, _) => SimpleVal::Invalid(i.to_string()),
        },
    }
}
