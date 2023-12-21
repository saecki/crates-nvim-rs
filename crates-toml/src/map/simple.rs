use std::collections::HashMap;

use crate::datetime::DateTime;
use crate::map::{MapArray, MapNode, MapTable, Scalar};

#[derive(Debug, PartialEq)]
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
