// use std::collections::HashMap;
//
// use crate::toml::{
//     Array, Assignment, Ast, BoolVal, FloatVal, InlineArray, InlineTable, IntVal, Key, StringVal,
//     Table, Value,
// };
//
// #[cfg(test)]
// mod test;
//
// #[derive(Debug, PartialEq)]
// pub enum MapEntry<'a> {
//     Assignment(AssignmentRef<'a>, Assignment<'a>),
//     Table(Vec<TableRef<'a>>, HashMap<&'a str, MapEntry<'a>>),
//     Array(Vec<ArrayRef<'a>>, Vec<MapEntry<'a>>),
//     Scalar(Scalar<'a>),
// }
//
// pub struct AssignmentRef {
//     pub key: Assignment
// }
//
// #[derive(Debug, PartialEq)]
// pub enum TableRef<'a> {
//     Table(&'a Table<'a>),
//     InlineTable(&'a InlineTable<'a>),
// }
//
// #[derive(Debug, PartialEq)]
// pub enum ArrayRef<'a> {
//     Array(&'a Array<'a>),
//     InlineArray(&'a InlineArray<'a>),
// }
//
// #[derive(Debug, PartialEq)]
// pub enum Scalar<'a> {
//     String(&'a StringVal<'a>),
//     Int(&'a IntVal<'a>),
//     Float(&'a FloatVal<'a>),
//     Bool(&'a BoolVal),
// }
//
// pub fn map<'a>(asts: &'a [Ast<'a>]) -> HashMap<&'a str, MapEntry<'a>> {
//     let mut map = HashMap::new();
//     for a in asts {
//         match a {
//             Ast::Assignment(a @ Assignment { key, val, .. }) => match key {
//                 Key::One(i) => {
//                     let ident = i.text.as_ref();
//                     let ast_ref = AstRef::Assignment(a);
//                     let map_val = map_value(val);
//                     map.insert(ident, (ast_ref, map_val));
//                 }
//                 Key::Dotted(_) => todo!(),
//             },
//             Ast::Table(_) => todo!(),
//             Ast::Array(_) => todo!(),
//         }
//     }
//
//     map
// }
//
// fn map_value<'a>(value: &'a Value<'a>) -> MapEntry<'a> {
//     match value {
//         Value::String(s) => MapEntry::Scalar(Scalar::String(s)),
//         Value::Int(i) => MapEntry::Scalar(Scalar::Int(i)),
//         Value::Float(f) => MapEntry::Scalar(Scalar::Float(f)),
//         Value::Bool(b) => MapEntry::Scalar(Scalar::Bool(b)),
//         Value::InlineArray(array) => {
//             let values = array
//                 .values
//                 .iter()
//                 .map(|val| {
//                     let ast_ref = AstRef::Value(val);
//                     let map_val = map_value(val);
//                     (ast_ref, map_val)
//                 })
//                 .collect();
//             MapEntry::Array(values)
//         }
//         Value::InlineTable(_) => todo!(),
//     }
// }
