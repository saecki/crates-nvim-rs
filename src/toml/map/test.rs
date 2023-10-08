// use std::collections::HashMap;
//
// use pretty_assertions::assert_eq;
//
// use crate::toml::{
//     self, Assignment, Ast, AstRef, BoolVal, Ctx, Ident, IdentKind, Key, MapEntry, Pos, Range,
//     Scalar, Value,
// };
//
// fn check<'a>(input: &str, expected: HashMap<&'a str, (AstRef<'a>, MapEntry<'a>)>) {
//     let mut ctx = Ctx::default();
//     let tokens = ctx.lex(input).unwrap();
//     let asts = ctx.parse(tokens).unwrap();
//     let map = toml::map(&asts);
//     assert_eq!(map, expected);
// }
//
// #[test]
// fn assignment() {
//     let mut map = HashMap::new();
//     let ast = Ast::Assignment(Assignment {
//         key: Key::One(Ident {
//             lit: "something",
//             lit_range: Range {
//                 start: Pos { line: 0, char: 0 },
//                 end: Pos { line: 0, char: 9 },
//             },
//             text: std::borrow::Cow::Borrowed("something"),
//             text_range: Range {
//                 start: Pos { line: 0, char: 0 },
//                 end: Pos { line: 0, char: 9 },
//             },
//             kind: IdentKind::Plain,
//         }),
//         eq: Pos { line: 0, char: 10 },
//         val: Value::Bool(BoolVal {
//             val: true,
//             lit_range: Range {
//                 start: Pos { line: 0, char: 12 },
//                 end: Pos { line: 0, char: 16 },
//             },
//         }),
//     });
//
//     let Ast::Assignment(assignment) = &ast else {
//         unreachable!()
//     };
//     let Value::Bool(bool_val) = &assignment.val else {
//         unreachable!()
//     };
//
//     let ast_ref = AstRef::Assignment(assignment);
//     let map_val = MapEntry::Scalar(Scalar::Bool(&bool_val));
//     map.insert("something", (ast_ref, map_val));
//     check("something = true", map);
// }
