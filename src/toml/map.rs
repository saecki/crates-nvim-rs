use std::collections::HashMap;

use crate::toml::{Assignment, Ast, BoolVal, FloatVal, IntVal, Key, StringVal, Value};

#[cfg(test)]
mod test;

#[derive(Debug, PartialEq)]
pub enum AstRef<'a> {
    Scalar(Scalar<'a>),
    Table(HashMap<&'a str, (&'a Ast<'a>, AstRef<'a>)>),
    Array(Vec<(&'a Ast<'a>, AstRef<'a>)>),
}

#[derive(Debug, PartialEq)]
pub enum Scalar<'a> {
    String(&'a StringVal<'a>),
    Int(&'a IntVal<'a>),
    Float(&'a FloatVal<'a>),
    Bool(&'a BoolVal),
}

pub fn map<'a>(asts: &'a [Ast<'a>]) -> HashMap<&'a str, (&'a Ast<'a>, AstRef<'a>)> {
    let mut map = HashMap::new();
    for a in asts {
        match a {
            Ast::Assignment(Assignment { key, val, .. }) => match key {
                Key::One(i) => {
                    let ident = i.text.as_ref();
                    let rhs = map_rhs(val);
                    map.insert(ident, (a, rhs));
                }
                Key::Dotted(_) => todo!(),
            },
            Ast::Table(_, _) => todo!(),
            Ast::Array(_, _) => todo!(),
        }
    }

    map
}

fn map_rhs<'a>(value: &'a Value<'a>) -> AstRef<'a> {
    match value {
        Value::String(s) => AstRef::Scalar(Scalar::String(s)),
        Value::Int(i) => AstRef::Scalar(Scalar::Int(i)),
        Value::Float(f) => AstRef::Scalar(Scalar::Float(f)),
        Value::Bool(b) => AstRef::Scalar(Scalar::Bool(b)),
        Value::InlineArray(a) => todo!(),
        Value::InlineTable(_) => todo!(),
    }
}
