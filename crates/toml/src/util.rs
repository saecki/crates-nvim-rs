use std::collections::HashMap;

use crate::datetime::DateTime;
use crate::map::{MapArray, MapNode, MapTable, Scalar};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Datatype {
    Table,
    Array,
    String,
    Int,
    Float,
    Bool,
    DateTime,
    Invalid,
}

impl Datatype {
    pub fn to_str(&self) -> &'static str {
        match self {
            Datatype::Table => "table",
            Datatype::Array => "array",
            Datatype::String => "string",
            Datatype::Int => "int",
            Datatype::Float => "float",
            Datatype::Bool => "bool",
            Datatype::DateTime => "date-time",
            Datatype::Invalid => "invalid",
        }
    }
}

impl std::fmt::Display for Datatype {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_str())
    }
}

impl MapNode<'_> {
    pub fn datatype(&self) -> Datatype {
        match self {
            MapNode::Table(_) => Datatype::Table,
            MapNode::Array(_) => Datatype::Array,
            MapNode::Scalar(s) => s.datatype(),
        }
    }
}

impl Scalar<'_> {
    pub fn datatype(&self) -> Datatype {
        match self {
            Scalar::String(_) => Datatype::String,
            Scalar::Int(_) => Datatype::Int,
            Scalar::Float(_) => Datatype::Float,
            Scalar::Bool(_) => Datatype::Bool,
            Scalar::DateTime(_) => Datatype::DateTime,
            Scalar::Invalid(_, _) => Datatype::Invalid,
        }
    }
}

impl SimpleVal {
    pub fn datatype(&self) -> Datatype {
        match self {
            SimpleVal::Table(_) => Datatype::Table,
            SimpleVal::Array(_) => Datatype::Array,
            SimpleVal::String(_) => Datatype::String,
            SimpleVal::Int(_) => Datatype::Int,
            SimpleVal::Float(_) => Datatype::Float,
            SimpleVal::Bool(_) => Datatype::Bool,
            SimpleVal::DateTime(_) => Datatype::DateTime,
            SimpleVal::Invalid(_) => Datatype::Invalid,
        }
    }
}

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

pub fn map_simple(map: MapTable) -> HashMap<String, SimpleVal> {
    let iter = map
        .into_iter()
        .map(|(k, e)| (k.to_string(), map_simple_val(e.node)));
    HashMap::from_iter(iter)
}

pub fn map_simple_val(node: MapNode) -> SimpleVal {
    match node {
        MapNode::Table(t) => SimpleVal::Table(map_simple(t)),
        MapNode::Array(MapArray::Toplevel(a)) => SimpleVal::Array(
            a.into_iter()
                .map(|e| SimpleVal::Table(map_simple(e.node)))
                .collect(),
        ),
        MapNode::Array(MapArray::Inline(a)) => {
            SimpleVal::Array(a.into_iter().map(|e| map_simple_val(e.node)).collect())
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
