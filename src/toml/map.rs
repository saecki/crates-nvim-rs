use std::collections::HashMap;

use crate::toml::{Ast, BoolVal, FloatVal, IntVal, Key, StringVal, Value};

#[cfg(test)]
mod test;

#[derive(Debug, PartialEq)]
pub enum AstRef<'a> {
    Scalar(Scalar<'a>),
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
            Ast::Assignment(key, _, v) => match key {
                Key::One(i) => {
                    let ident = i.text.as_ref();
                    let rhs = map_rhs(v);
                    map.insert(ident, (a, rhs));
                }
                Key::Dotted(_) => todo!(),
            },
            Ast::TableHeader(_) => todo!(),
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
        Value::Array(_) => todo!(),
        Value::InlineTable(_) => todo!(),
    }
}
