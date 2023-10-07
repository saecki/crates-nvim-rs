use std::borrow::Cow;

use pretty_assertions::assert_eq;

use crate::toml::{
    Array, Ast, BoolVal, Ctx, Error, Ident, IdentKind, IntVal, Key, Pos, Quote, Range, StringVal,
    Value,
};

fn check<const SIZE: usize>(input: &str, expected: [Ast; SIZE]) {
    let mut ctx = Ctx::default();
    let tokens = ctx.lex(input).unwrap();
    let asts = ctx.parse(tokens).unwrap();
    assert_eq!(asts, expected);
}

fn check_error<const SIZE: usize>(input: &str, expected: [Ast; SIZE], error: Error) {
    let mut ctx = Ctx::default();
    let tokens = ctx.lex(input).unwrap();
    let asts = ctx.parse(tokens).unwrap();
    assert_eq!(asts, expected);
    assert_eq!(ctx.errors, [error]);
}

#[test]
fn assign_bool() {
    check(
        "abc = false",
        [Ast::Assignment(
            Key::One(Ident {
                lit: "abc",
                lit_range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 3 },
                },
                text: Cow::Borrowed("abc"),
                text_range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 3 },
                },
                kind: IdentKind::Plain,
            }),
            Pos { line: 0, char: 4 },
            Value::Bool(BoolVal {
                lit_range: Range {
                    start: Pos { line: 0, char: 6 },
                    end: Pos { line: 0, char: 11 },
                },
                val: false,
            }),
        )],
    );
}

#[test]
fn int_identifier() {
    check(
        "123 = false",
        [Ast::Assignment(
            Key::One(Ident {
                lit: "123",
                lit_range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 3 },
                },
                text: Cow::Borrowed("123"),
                text_range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 3 },
                },
                kind: IdentKind::Plain,
            }),
            Pos { line: 0, char: 4 },
            Value::Bool(BoolVal {
                lit_range: Range {
                    start: Pos { line: 0, char: 6 },
                    end: Pos { line: 0, char: 11 },
                },
                val: false,
            }),
        )],
    );
}

#[test]
fn invalid_int_identifier() {
    check_error(
        "+99 = false",
        [Ast::Assignment(
            Key::One(Ident {
                lit: "+99",
                lit_range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 3 },
                },
                text: Cow::Borrowed("+99"),
                text_range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 3 },
                },
                kind: IdentKind::Plain,
            }),
            Pos { line: 0, char: 4 },
            Value::Bool(BoolVal {
                lit_range: Range {
                    start: Pos { line: 0, char: 6 },
                    end: Pos { line: 0, char: 11 },
                },
                val: false,
            }),
        )],
        Error::InvalidCharInIdentifier('+', Pos { line: 0, char: 0 }),
    );
}

#[test]
fn invalid_float_identifier() {
    check_error(
        "23e+3 = 'hello'",
        [Ast::Assignment(
            Key::One(Ident {
                lit: "23e+3",
                lit_range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 5 },
                },
                text: Cow::Borrowed("23e+3"),
                text_range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 5 },
                },
                kind: IdentKind::Plain,
            }),
            Pos { line: 0, char: 6 },
            Value::String(StringVal {
                lit: "'hello'",
                lit_range: Range {
                    start: Pos { line: 0, char: 8 },
                    end: Pos { line: 0, char: 15 },
                },
                text: Cow::Borrowed("hello"),
                text_range: Range {
                    start: Pos { line: 0, char: 9 },
                    end: Pos { line: 0, char: 14 },
                },
                quote: Quote::Literal,
            }),
        )],
        Error::InvalidCharInIdentifier('+', Pos { line: 0, char: 3 }),
    );
}

#[test]
fn inline_array() {
    check(
        "array = [0, 1, 2]",
        [Ast::Assignment(
            Key::One(Ident {
                lit: "array",
                lit_range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 5 },
                },
                text: Cow::Borrowed("array"),
                text_range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 5 },
                },
                kind: IdentKind::Plain,
            }),
            Pos { line: 0, char: 6 },
            Value::Array(Array {
                range: Range {
                    start: Pos { line: 0, char: 8 },
                    end: Pos { line: 0, char: 17 },
                },
                values: vec![
                    Value::Int(IntVal {
                        lit: "0",
                        lit_range: Range {
                            start: Pos { line: 0, char: 9 },
                            end: Pos { line: 0, char: 10 },
                        },
                        val: 0,
                    }),
                    Value::Int(IntVal {
                        lit: "1",
                        lit_range: Range {
                            start: Pos { line: 0, char: 12 },
                            end: Pos { line: 0, char: 13 },
                        },
                        val: 1,
                    }),
                    Value::Int(IntVal {
                        lit: "2",
                        lit_range: Range {
                            start: Pos { line: 0, char: 15 },
                            end: Pos { line: 0, char: 16 },
                        },
                        val: 2,
                    }),
                ],
            }),
        )],
    );
}
