use std::borrow::Cow;

use pretty_assertions::assert_eq;

use crate::toml::{
    ArrayKind, BoolVal, Ctx, Error, FloatVal, IntVal, MapArray, MapEntry, MapTable, Pos, Quote,
    Range, Scalar, StringVal, TableKind, Warning,
};

fn check(input: &str, expected: MapTable) {
    let mut ctx = Ctx::default();
    let tokens = ctx.lex(input);
    let asts = ctx.parse(tokens);
    let map = ctx.map(&asts);
    assert_eq!(
        expected, map,
        "\nerrors: {:#?}\nwarnings: {:#?}",
        ctx.errors, ctx.warnings
    );
    assert_eq!(Vec::<Error>::new(), ctx.errors);
    assert_eq!(Vec::<Warning>::new(), ctx.warnings);
}

#[test]
fn dotted_key() {
    check(
        "a.b.c = 1",
        MapTable::from_pairs(
            TableKind::Root,
            [(
                "a",
                MapEntry::Table(MapTable::from_pairs(
                    TableKind::DottedKey,
                    [(
                        "b",
                        MapEntry::Table(MapTable::from_pairs(
                            TableKind::DottedKey,
                            [(
                                "c",
                                MapEntry::Scalar(Scalar::Int(&IntVal {
                                    lit: "1",
                                    lit_range: Range {
                                        start: Pos { line: 0, char: 8 },
                                        end: Pos { line: 0, char: 9 },
                                    },
                                    val: 1,
                                })),
                            )],
                        )),
                    )],
                )),
            )],
        ),
    )
}

#[test]
fn dotted_keys_extend() {
    check(
        "a.b.c = 1\na.b.d = 2",
        MapTable::from_pairs(
            TableKind::Root,
            [(
                "a",
                MapEntry::Table(MapTable::from_pairs(
                    TableKind::DottedKey,
                    [(
                        "b",
                        MapEntry::Table(MapTable::from_pairs(
                            TableKind::DottedKey,
                            [
                                (
                                    "c",
                                    MapEntry::Scalar(Scalar::Int(&IntVal {
                                        lit: "1",
                                        lit_range: Range {
                                            start: Pos { line: 0, char: 8 },
                                            end: Pos { line: 0, char: 9 },
                                        },
                                        val: 1,
                                    })),
                                ),
                                (
                                    "d",
                                    MapEntry::Scalar(Scalar::Int(&IntVal {
                                        lit: "2",
                                        lit_range: Range {
                                            start: Pos { line: 1, char: 8 },
                                            end: Pos { line: 1, char: 9 },
                                        },
                                        val: 2,
                                    })),
                                ),
                            ],
                        )),
                    )],
                )),
            )],
        ),
    )
}

#[test]
fn table() {
    check(
        "[mytable]\nabc = true\ndef = 23.0",
        MapTable::from_pairs(
            TableKind::Root,
            [(
                "mytable",
                MapEntry::Table(MapTable::from_pairs(
                    TableKind::TopLevel,
                    [
                        (
                            "abc",
                            MapEntry::Scalar(Scalar::Bool(&BoolVal {
                                lit_range: Range {
                                    start: Pos { line: 1, char: 6 },
                                    end: Pos { line: 1, char: 10 },
                                },
                                val: true,
                            })),
                        ),
                        (
                            "def",
                            MapEntry::Scalar(Scalar::Float(&FloatVal {
                                lit: "23.0",
                                lit_range: Range {
                                    start: Pos { line: 2, char: 6 },
                                    end: Pos { line: 2, char: 10 },
                                },
                                val: 23.0,
                            })),
                        ),
                    ],
                )),
            )],
        ),
    )
}

#[test]
fn inline_array() {
    check(
        "array = [1, 2, 4, 8, 16]",
        MapTable::from_pairs(
            TableKind::Root,
            [(
                "array",
                MapEntry::Array(MapArray::from_iter(
                    ArrayKind::Inline,
                    [
                        MapEntry::Scalar(Scalar::Int(&IntVal {
                            lit: "1",
                            lit_range: Range {
                                start: Pos { line: 0, char: 9 },
                                end: Pos { line: 0, char: 10 },
                            },
                            val: 1,
                        })),
                        MapEntry::Scalar(Scalar::Int(&IntVal {
                            lit: "2",
                            lit_range: Range {
                                start: Pos { line: 0, char: 12 },
                                end: Pos { line: 0, char: 13 },
                            },
                            val: 2,
                        })),
                        MapEntry::Scalar(Scalar::Int(&IntVal {
                            lit: "4",
                            lit_range: Range {
                                start: Pos { line: 0, char: 15 },
                                end: Pos { line: 0, char: 16 },
                            },
                            val: 4,
                        })),
                        MapEntry::Scalar(Scalar::Int(&IntVal {
                            lit: "8",
                            lit_range: Range {
                                start: Pos { line: 0, char: 18 },
                                end: Pos { line: 0, char: 19 },
                            },
                            val: 8,
                        })),
                        MapEntry::Scalar(Scalar::Int(&IntVal {
                            lit: "16",
                            lit_range: Range {
                                start: Pos { line: 0, char: 21 },
                                end: Pos { line: 0, char: 23 },
                            },
                            val: 16,
                        })),
                    ],
                )),
            )],
        ),
    )
}

#[test]
fn array_of_tables() {
    check(
        "\
[[currencies]]
name = 'Euro'
symbol = '€'

[[currencies]]
name = 'Dollar'
symbol = '$'

[[currencies]]
name = 'Pound'
symbol = '£'
",
        MapTable::from_pairs(
            TableKind::Root,
            [(
                "currencies",
                MapEntry::Array(MapArray::from_iter(
                    ArrayKind::TopLevel,
                    [
                        MapEntry::Table(MapTable::from_pairs(
                            TableKind::TopLevel,
                            [
                                (
                                    "name",
                                    MapEntry::Scalar(Scalar::String(&StringVal {
                                        lit: "'Euro'",
                                        lit_range: Range {
                                            start: Pos { line: 1, char: 7 },
                                            end: Pos { line: 1, char: 13 },
                                        },
                                        text: Cow::Borrowed("Euro"),
                                        text_range: Range {
                                            start: Pos { line: 1, char: 8 },
                                            end: Pos { line: 1, char: 12 },
                                        },
                                        quote: Quote::Literal,
                                    })),
                                ),
                                (
                                    "symbol",
                                    MapEntry::Scalar(Scalar::String(&StringVal {
                                        lit: "'€'",
                                        lit_range: Range {
                                            start: Pos { line: 2, char: 9 },
                                            end: Pos { line: 2, char: 14 },
                                        },
                                        text: Cow::Borrowed("€"),
                                        text_range: Range {
                                            start: Pos { line: 2, char: 10 },
                                            end: Pos { line: 2, char: 13 },
                                        },
                                        quote: Quote::Literal,
                                    })),
                                ),
                            ],
                        )),
                        MapEntry::Table(MapTable::from_pairs(
                            TableKind::TopLevel,
                            [
                                (
                                    "name",
                                    MapEntry::Scalar(Scalar::String(&StringVal {
                                        lit: "'Dollar'",
                                        lit_range: Range {
                                            start: Pos { line: 5, char: 7 },
                                            end: Pos { line: 5, char: 15 },
                                        },
                                        text: Cow::Borrowed("Dollar"),
                                        text_range: Range {
                                            start: Pos { line: 5, char: 8 },
                                            end: Pos { line: 5, char: 14 },
                                        },
                                        quote: Quote::Literal,
                                    })),
                                ),
                                (
                                    "symbol",
                                    MapEntry::Scalar(Scalar::String(&StringVal {
                                        lit: "'$'",
                                        lit_range: Range {
                                            start: Pos { line: 6, char: 9 },
                                            end: Pos { line: 6, char: 12 },
                                        },
                                        text: Cow::Borrowed("$"),
                                        text_range: Range {
                                            start: Pos { line: 6, char: 10 },
                                            end: Pos { line: 6, char: 11 },
                                        },
                                        quote: Quote::Literal,
                                    })),
                                ),
                            ],
                        )),
                        MapEntry::Table(MapTable::from_pairs(
                            TableKind::TopLevel,
                            [
                                (
                                    "name",
                                    MapEntry::Scalar(Scalar::String(&StringVal {
                                        lit: "'Pound'",
                                        lit_range: Range {
                                            start: Pos { line: 9, char: 7 },
                                            end: Pos { line: 9, char: 14 },
                                        },
                                        text: Cow::Borrowed("Pound"),
                                        text_range: Range {
                                            start: Pos { line: 9, char: 8 },
                                            end: Pos { line: 9, char: 13 },
                                        },
                                        quote: Quote::Literal,
                                    })),
                                ),
                                (
                                    "symbol",
                                    MapEntry::Scalar(Scalar::String(&StringVal {
                                        lit: "'£'",
                                        lit_range: Range {
                                            start: Pos { line: 10, char: 9 },
                                            end: Pos { line: 10, char: 13 },
                                        },
                                        text: Cow::Borrowed("£"),
                                        text_range: Range {
                                            start: Pos { line: 10, char: 10 },
                                            end: Pos { line: 10, char: 12 },
                                        },
                                        quote: Quote::Literal,
                                    })),
                                ),
                            ],
                        )),
                    ],
                )),
            )],
        ),
    )
}
