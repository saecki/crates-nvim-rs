use std::borrow::Cow;

use pretty_assertions::assert_eq;

use crate::toml::{
    Assignment, Ast, BoolVal, Ctx, Error, Ident, IdentKind, InlineArray, InlineArrayValue,
    InlineTable, InlineTableAssignment, IntVal, Key, Pos, Quote, Range, StringVal, Table,
    TableHeader, Value, Warning, ArrayHeader, Array, DottedIdent,
};

fn check<const SIZE: usize>(input: &str, expected: [Ast; SIZE]) {
    let mut ctx = Ctx::default();
    let tokens = ctx.lex(input).unwrap();
    let asts = ctx.parse(tokens).unwrap();
    assert_eq!(expected.as_slice(), asts);
    assert_eq!(Vec::<Error>::new(), ctx.errors);
    assert_eq!(Vec::<Warning>::new(), ctx.warnings);
}

fn check_error<const SIZE: usize>(input: &str, expected: [Ast; SIZE], error: Error) {
    let mut ctx = Ctx::default();
    let tokens = ctx.lex(input).unwrap();
    let asts = ctx.parse(tokens).unwrap();
    assert_eq!(expected.as_slice(), asts);
    assert_eq!(vec![error], ctx.errors);
    assert_eq!(Vec::<Warning>::new(), ctx.warnings);
}

fn check_errors<const AST_SIZE: usize, const ERR_SIZE: usize>(
    input: &str,
    expected: [Ast; AST_SIZE],
    errors: [Error; ERR_SIZE],
) {
    let mut ctx = Ctx::default();
    let tokens = ctx.lex(input).unwrap();
    let asts = ctx.parse(tokens).unwrap();
    assert_eq!(expected.as_slice(), asts);
    assert_eq!(errors.as_slice(), ctx.errors);
    assert_eq!(Vec::<Warning>::new(), ctx.warnings);
}

#[test]
fn assign_bool() {
    check(
        "abc = false",
        [Ast::Assignment(Assignment {
            key: Key::One(Ident {
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
            eq: Pos { line: 0, char: 4 },
            val: Value::Bool(BoolVal {
                lit_range: Range {
                    start: Pos { line: 0, char: 6 },
                    end: Pos { line: 0, char: 11 },
                },
                val: false,
            }),
        })],
    );
}

#[test]
fn dotted_key() {
    check(
        "a.b.c = false",
        [Ast::Assignment(Assignment {
            key: Key::Dotted(vec![
                DottedIdent {
                    ident: Ident {
                        lit: "a",
                        lit_range: Range {
                            start: Pos { line: 0, char: 0 },
                            end: Pos { line: 0, char: 1 },
                        },
                        text: Cow::Borrowed("a"),
                        text_range: Range {
                            start: Pos { line: 0, char: 0 },
                            end: Pos { line: 0, char: 1 },
                        },
                        kind: IdentKind::Plain,
                    },
                    dot: Some(Pos { line: 0, char: 1 }),
                },
                DottedIdent {
                    ident: Ident {
                        lit: "b",
                        lit_range: Range {
                            start: Pos { line: 0, char: 2 },
                            end: Pos { line: 0, char: 3 },
                        },
                        text: Cow::Borrowed("b"),
                        text_range: Range {
                            start: Pos { line: 0, char: 2 },
                            end: Pos { line: 0, char: 3 },
                        },
                        kind: IdentKind::Plain,
                    },
                    dot: Some(Pos { line: 0, char: 3 }),
                },
                DottedIdent {
                    ident: Ident {
                        lit: "c",
                        lit_range: Range {
                            start: Pos { line: 0, char: 4 },
                            end: Pos { line: 0, char: 5 },
                        },
                        text: Cow::Borrowed("c"),
                        text_range: Range {
                            start: Pos { line: 0, char: 4 },
                            end: Pos { line: 0, char: 5 },
                        },
                        kind: IdentKind::Plain,
                    },
                    dot: None,
                },
            ]),
            eq: Pos { line: 0, char: 6 },
            val: Value::Bool(BoolVal {
                lit_range: Range {
                    start: Pos { line: 0, char: 8 },
                    end: Pos { line: 0, char: 13 },
                },
                val: false,
            }),
        })],
    );
}

#[test]
fn int_identifier() {
    check(
        "123 = false",
        [Ast::Assignment(Assignment {
            key: Key::One(Ident {
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
            eq: Pos { line: 0, char: 4 },
            val: Value::Bool(BoolVal {
                lit_range: Range {
                    start: Pos { line: 0, char: 6 },
                    end: Pos { line: 0, char: 11 },
                },
                val: false,
            }),
        })],
    );
}

#[test]
fn invalid_int_identifier() {
    check_error(
        "+99 = false",
        [Ast::Assignment(Assignment {
            key: Key::One(Ident {
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
            eq: Pos { line: 0, char: 4 },
            val: Value::Bool(BoolVal {
                lit_range: Range {
                    start: Pos { line: 0, char: 6 },
                    end: Pos { line: 0, char: 11 },
                },
                val: false,
            }),
        })],
        Error::InvalidCharInIdentifier('+', Pos { line: 0, char: 0 }),
    );
}

#[test]
fn invalid_float_identifier() {
    check_error(
        "23e+3 = 'hello'",
        [Ast::Assignment(Assignment {
            key: Key::One(Ident {
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
            eq: Pos { line: 0, char: 6 },
            val: Value::String(StringVal {
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
        })],
        Error::InvalidCharInIdentifier('+', Pos { line: 0, char: 3 }),
    );
}

#[test]
fn inline_array() {
    check(
        "array = [0, 1, 2]",
        [Ast::Assignment(Assignment {
            key: Key::One(Ident {
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
            eq: Pos { line: 0, char: 6 },
            val: Value::InlineArray(InlineArray {
                range: Range {
                    start: Pos { line: 0, char: 8 },
                    end: Pos { line: 0, char: 17 },
                },
                values: vec![
                    InlineArrayValue {
                        val: Value::Int(IntVal {
                            lit: "0",
                            lit_range: Range {
                                start: Pos { line: 0, char: 9 },
                                end: Pos { line: 0, char: 10 },
                            },
                            val: 0,
                        }),
                        comma: Some(Pos { line: 0, char: 10 }),
                    },
                    InlineArrayValue {
                        val: Value::Int(IntVal {
                            lit: "1",
                            lit_range: Range {
                                start: Pos { line: 0, char: 12 },
                                end: Pos { line: 0, char: 13 },
                            },
                            val: 1,
                        }),
                        comma: Some(Pos { line: 0, char: 13 }),
                    },
                    InlineArrayValue {
                        val: Value::Int(IntVal {
                            lit: "2",
                            lit_range: Range {
                                start: Pos { line: 0, char: 15 },
                                end: Pos { line: 0, char: 16 },
                            },
                            val: 2,
                        }),
                        comma: None,
                    },
                ],
            }),
        })],
    );
}

#[test]
fn inline_array_recover_comma() {
    check_error(
        "array = [0, 1  2]",
        [Ast::Assignment(Assignment {
            key: Key::One(Ident {
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
            eq: Pos { line: 0, char: 6 },
            val: Value::InlineArray(InlineArray {
                range: Range {
                    start: Pos { line: 0, char: 8 },
                    end: Pos { line: 0, char: 17 },
                },
                values: vec![
                    InlineArrayValue {
                        val: Value::Int(IntVal {
                            lit: "0",
                            lit_range: Range {
                                start: Pos { line: 0, char: 9 },
                                end: Pos { line: 0, char: 10 },
                            },
                            val: 0,
                        }),
                        comma: Some(Pos { line: 0, char: 10 }),
                    },
                    InlineArrayValue {
                        val: Value::Int(IntVal {
                            lit: "1",
                            lit_range: Range {
                                start: Pos { line: 0, char: 12 },
                                end: Pos { line: 0, char: 13 },
                            },
                            val: 1,
                        }),
                        comma: None,
                    },
                    InlineArrayValue {
                        val: Value::Int(IntVal {
                            lit: "2",
                            lit_range: Range {
                                start: Pos { line: 0, char: 15 },
                                end: Pos { line: 0, char: 16 },
                            },
                            val: 2,
                        }),
                        comma: None,
                    },
                ],
            }),
        })],
        Error::ExpectedComma(Pos { line: 0, char: 13 }),
    );
}

#[test]
fn inline_array_recover_invalid() {
    check_error(
        "array = [0, 1, 2, =]",
        [Ast::Assignment(Assignment {
            key: Key::One(Ident {
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
            eq: Pos { line: 0, char: 6 },
            val: Value::InlineArray(InlineArray {
                range: Range {
                    start: Pos { line: 0, char: 8 },
                    end: Pos { line: 0, char: 20 },
                },
                values: vec![
                    InlineArrayValue {
                        val: Value::Int(IntVal {
                            lit: "0",
                            lit_range: Range {
                                start: Pos { line: 0, char: 9 },
                                end: Pos { line: 0, char: 10 },
                            },
                            val: 0,
                        }),
                        comma: Some(Pos { line: 0, char: 10 }),
                    },
                    InlineArrayValue {
                        val: Value::Int(IntVal {
                            lit: "1",
                            lit_range: Range {
                                start: Pos { line: 0, char: 12 },
                                end: Pos { line: 0, char: 13 },
                            },
                            val: 1,
                        }),
                        comma: Some(Pos { line: 0, char: 13 }),
                    },
                    InlineArrayValue {
                        val: Value::Int(IntVal {
                            lit: "2",
                            lit_range: Range {
                                start: Pos { line: 0, char: 15 },
                                end: Pos { line: 0, char: 16 },
                            },
                            val: 2,
                        }),
                        comma: Some(Pos { line: 0, char: 16 }),
                    },
                ],
            }),
        })],
        Error::ExpectedValue(
            "=".into(),
            Range {
                start: Pos { line: 0, char: 18 },
                end: Pos { line: 0, char: 19 },
            },
        ),
    );
}

#[test]
fn inline_table() {
    check(
        "table = { a = 3, b = true }",
        [Ast::Assignment(Assignment {
            key: Key::One(Ident {
                lit: "table",
                lit_range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 5 },
                },
                text: Cow::Borrowed("table"),
                text_range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 5 },
                },
                kind: IdentKind::Plain,
            }),
            eq: Pos { line: 0, char: 6 },
            val: Value::InlineTable(InlineTable {
                range: Range {
                    start: Pos { line: 0, char: 8 },
                    end: Pos { line: 0, char: 27 },
                },
                assignments: vec![
                    InlineTableAssignment {
                        key: Key::One(Ident {
                            lit: "a",
                            lit_range: Range {
                                start: Pos { line: 0, char: 10 },
                                end: Pos { line: 0, char: 11 },
                            },
                            text: Cow::Borrowed("a"),
                            text_range: Range {
                                start: Pos { line: 0, char: 10 },
                                end: Pos { line: 0, char: 11 },
                            },
                            kind: IdentKind::Plain,
                        }),
                        eq: Pos { line: 0, char: 12 },
                        val: Value::Int(IntVal {
                            lit: "3",
                            lit_range: Range {
                                start: Pos { line: 0, char: 14 },
                                end: Pos { line: 0, char: 15 },
                            },
                            val: 3,
                        }),
                        comma: Some(Pos { line: 0, char: 15 }),
                    },
                    InlineTableAssignment {
                        key: Key::One(Ident {
                            lit: "b",
                            lit_range: Range {
                                start: Pos { line: 0, char: 17 },
                                end: Pos { line: 0, char: 18 },
                            },
                            text: Cow::Borrowed("b"),
                            text_range: Range {
                                start: Pos { line: 0, char: 17 },
                                end: Pos { line: 0, char: 18 },
                            },
                            kind: IdentKind::Plain,
                        }),
                        eq: Pos { line: 0, char: 19 },
                        val: Value::Bool(BoolVal {
                            lit_range: Range {
                                start: Pos { line: 0, char: 21 },
                                end: Pos { line: 0, char: 25 },
                            },
                            val: true,
                        }),
                        comma: None,
                    },
                ],
            }),
        })],
    );
}

#[test]
fn inline_table_recover_missing_comma() {
    check_error(
        "table = { a = 3  b = true }",
        [Ast::Assignment(Assignment {
            key: Key::One(Ident {
                lit: "table",
                lit_range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 5 },
                },
                text: Cow::Borrowed("table"),
                text_range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 5 },
                },
                kind: IdentKind::Plain,
            }),
            eq: Pos { line: 0, char: 6 },
            val: Value::InlineTable(InlineTable {
                range: Range {
                    start: Pos { line: 0, char: 8 },
                    end: Pos { line: 0, char: 27 },
                },
                assignments: vec![
                    InlineTableAssignment {
                        key: Key::One(Ident {
                            lit: "a",
                            lit_range: Range {
                                start: Pos { line: 0, char: 10 },
                                end: Pos { line: 0, char: 11 },
                            },
                            text: Cow::Borrowed("a"),
                            text_range: Range {
                                start: Pos { line: 0, char: 10 },
                                end: Pos { line: 0, char: 11 },
                            },
                            kind: IdentKind::Plain,
                        }),
                        eq: Pos { line: 0, char: 12 },
                        val: Value::Int(IntVal {
                            lit: "3",
                            lit_range: Range {
                                start: Pos { line: 0, char: 14 },
                                end: Pos { line: 0, char: 15 },
                            },
                            val: 3,
                        }),
                        comma: None,
                    },
                    InlineTableAssignment {
                        key: Key::One(Ident {
                            lit: "b",
                            lit_range: Range {
                                start: Pos { line: 0, char: 17 },
                                end: Pos { line: 0, char: 18 },
                            },
                            text: Cow::Borrowed("b"),
                            text_range: Range {
                                start: Pos { line: 0, char: 17 },
                                end: Pos { line: 0, char: 18 },
                            },
                            kind: IdentKind::Plain,
                        }),
                        eq: Pos { line: 0, char: 19 },
                        val: Value::Bool(BoolVal {
                            lit_range: Range {
                                start: Pos { line: 0, char: 21 },
                                end: Pos { line: 0, char: 25 },
                            },
                            val: true,
                        }),
                        comma: None,
                    },
                ],
            }),
        })],
        Error::ExpectedComma(Pos { line: 0, char: 15 }),
    );
}

#[test]
fn inline_table_recover_invalid() {
    check_error(
        "table = { a = 3, b = true, = }",
        [Ast::Assignment(Assignment {
            key: Key::One(Ident {
                lit: "table",
                lit_range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 5 },
                },
                text: Cow::Borrowed("table"),
                text_range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 5 },
                },
                kind: IdentKind::Plain,
            }),
            eq: Pos { line: 0, char: 6 },
            val: Value::InlineTable(InlineTable {
                range: Range {
                    start: Pos { line: 0, char: 8 },
                    end: Pos { line: 0, char: 30 },
                },
                assignments: vec![
                    InlineTableAssignment {
                        key: Key::One(Ident {
                            lit: "a",
                            lit_range: Range {
                                start: Pos { line: 0, char: 10 },
                                end: Pos { line: 0, char: 11 },
                            },
                            text: Cow::Borrowed("a"),
                            text_range: Range {
                                start: Pos { line: 0, char: 10 },
                                end: Pos { line: 0, char: 11 },
                            },
                            kind: IdentKind::Plain,
                        }),
                        eq: Pos { line: 0, char: 12 },
                        val: Value::Int(IntVal {
                            lit: "3",
                            lit_range: Range {
                                start: Pos { line: 0, char: 14 },
                                end: Pos { line: 0, char: 15 },
                            },
                            val: 3,
                        }),
                        comma: Some(Pos { line: 0, char: 15 }),
                    },
                    InlineTableAssignment {
                        key: Key::One(Ident {
                            lit: "b",
                            lit_range: Range {
                                start: Pos { line: 0, char: 17 },
                                end: Pos { line: 0, char: 18 },
                            },
                            text: Cow::Borrowed("b"),
                            text_range: Range {
                                start: Pos { line: 0, char: 17 },
                                end: Pos { line: 0, char: 18 },
                            },
                            kind: IdentKind::Plain,
                        }),
                        eq: Pos { line: 0, char: 19 },
                        val: Value::Bool(BoolVal {
                            lit_range: Range {
                                start: Pos { line: 0, char: 21 },
                                end: Pos { line: 0, char: 25 },
                            },
                            val: true,
                        }),
                        comma: Some(Pos { line: 0, char: 25 }),
                    },
                ],
            }),
        })],
        Error::ExpectedKey(
            "=".into(),
            Range {
                start: Pos { line: 0, char: 27 },
                end: Pos { line: 0, char: 28 },
            },
        ),
    );
}

#[test]
fn table_header() {
    check(
        "[my_table]\nentry = false\n",
        [Ast::Table(Table {
            header: TableHeader {
                l_par: Pos { line: 0, char: 0 },
                key: Some(Key::One(Ident {
                    lit: "my_table",
                    lit_range: Range {
                        start: Pos { line: 0, char: 1 },
                        end: Pos { line: 0, char: 9 },
                    },
                    text: Cow::Borrowed("my_table"),
                    text_range: Range {
                        start: Pos { line: 0, char: 1 },
                        end: Pos { line: 0, char: 9 },
                    },
                    kind: IdentKind::Plain,
                })),
                r_par: Some(Pos { line: 0, char: 9 }),
            },
            assignments: vec![Assignment {
                key: Key::One(Ident {
                    lit: "entry",
                    lit_range: Range {
                        start: Pos { line: 1, char: 0 },
                        end: Pos { line: 1, char: 5 },
                    },
                    text: Cow::Borrowed("entry"),
                    text_range: Range {
                        start: Pos { line: 1, char: 0 },
                        end: Pos { line: 1, char: 5 },
                    },
                    kind: IdentKind::Plain,
                }),
                eq: Pos { line: 1, char: 6 },
                val: Value::Bool(BoolVal {
                    lit_range: Range {
                        start: Pos { line: 1, char: 8 },
                        end: Pos { line: 1, char: 13 },
                    },
                    val: false,
                }),
            }],
        })],
    )
}

#[test]
fn array_header() {
    check(
        "[[my_array]]\nentry = false\n",
        [Ast::Array(Array {
            header: ArrayHeader {
                l_pars: (Pos { line: 0, char: 0 }, Pos { line: 0, char: 1 }),
                key: Some(Key::One(Ident {
                    lit: "my_array",
                    lit_range: Range {
                        start: Pos { line: 0, char: 2 },
                        end: Pos { line: 0, char: 10 },
                    },
                    text: Cow::Borrowed("my_array"),
                    text_range: Range {
                        start: Pos { line: 0, char: 2 },
                        end: Pos { line: 0, char: 10 },
                    },
                    kind: IdentKind::Plain,
                })),
                r_pars: (Some(Pos { line: 0, char: 10 }), Some(Pos { line: 0, char: 11 })),
            },
            assignments: vec![Assignment {
                key: Key::One(Ident {
                    lit: "entry",
                    lit_range: Range {
                        start: Pos { line: 1, char: 0 },
                        end: Pos { line: 1, char: 5 },
                    },
                    text: Cow::Borrowed("entry"),
                    text_range: Range {
                        start: Pos { line: 1, char: 0 },
                        end: Pos { line: 1, char: 5 },
                    },
                    kind: IdentKind::Plain,
                }),
                eq: Pos { line: 1, char: 6 },
                val: Value::Bool(BoolVal {
                    lit_range: Range {
                        start: Pos { line: 1, char: 8 },
                        end: Pos { line: 1, char: 13 },
                    },
                    val: false,
                }),
            }],
        })],
    )
}
