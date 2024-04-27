use std::collections::HashMap;

use pretty_assertions::assert_eq;

use super::*;
use crate::datetime::{DateTimeField, Offset};
use crate::test::*;

fn check_comments<'a, const SIZE: usize>(
    input: &str,
    expected_builder: impl FnOnce(&'a Bump, &'_ mut BVec<'a, AssocComment<'a>>) -> [Ast<'a>; SIZE],
) {
    let mut ctx = TomlDiagnostics::default();
    let bump = Bump::new();
    let tokens = ctx.lex(&bump, input);
    let asts = ctx.parse(&bump, &tokens);

    // HACK
    let expected_bump = Box::leak(Box::new(Bump::new()));
    let mut expected_comments = BVec::new_in(expected_bump);
    let expected_asts = expected_builder(expected_bump, &mut expected_comments);
    assert_eq!(
        Asts {
            asts: &expected_asts,
            comments: &expected_comments,
        },
        asts,
        "\nerrors: {:#?}\nwarnings: {:#?}",
        ctx.errors,
        ctx.warnings
    );
    assert_eq!(Vec::<Error>::new(), ctx.errors);
    assert_eq!(Vec::<Warning>::new(), ctx.warnings);
}

fn check<'a, const SIZE: usize>(
    input: &str,
    expected_builder: impl FnOnce(&'a Bump, &[AssocComment<'a>]) -> [Ast<'a>; SIZE],
) {
    check_comments(input, |bump, comments| expected_builder(bump, comments))
}

fn check_error<'a, const SIZE: usize>(
    input: &str,
    expected_builder: impl FnOnce(&'a Bump, &[AssocComment<'a>]) -> [Ast<'a>; SIZE],
    error: Error,
) {
    let mut ctx = TomlDiagnostics::default();
    let bump = Bump::new();
    let tokens = ctx.lex(&bump, input);
    let asts = ctx.parse(&bump, &tokens);

    // HACK
    let expected_bump = Box::leak(Box::new(Bump::new()));
    let expected_comments = [];
    let expected_asts = expected_builder(expected_bump, &expected_comments);

    assert_eq!(
        Asts {
            asts: &expected_asts,
            comments: &expected_comments,
        },
        asts,
        "\nerrors: {:#?}\nwarnings: {:#?}",
        ctx.errors,
        ctx.warnings
    );
    assert_eq!(vec![error], ctx.errors);
    assert_eq!(Vec::<Warning>::new(), ctx.warnings);
}

#[test]
fn float_special_values() {
    let input = "
    a0 = nan
    a1 = +nan
    a2 = -nan

    b0 = inf
    b1 = +inf
    b2 = -inf

    c0 = +0.0
    c1 = -0.0
";

    let (_, table) = parse_simple(input);

    assert!(expect_float(&table, "a0").is_nan());
    assert!(expect_float(&table, "a0").is_sign_positive());
    assert!(expect_float(&table, "a1").is_nan());
    assert!(expect_float(&table, "a1").is_sign_positive());
    assert!(expect_float(&table, "a2").is_nan());
    assert!(expect_float(&table, "a2").is_sign_negative());

    assert!(expect_float(&table, "b0").is_infinite());
    assert!(expect_float(&table, "b0").is_sign_positive());
    assert!(expect_float(&table, "b1").is_infinite());
    assert!(expect_float(&table, "b1").is_sign_positive());
    assert!(expect_float(&table, "b2").is_infinite());
    assert!(expect_float(&table, "b2").is_sign_negative());

    assert_eq!(expect_float(&table, "c0"), 0.0);
    assert!(expect_float(&table, "c0").is_sign_positive());
    assert_eq!(expect_float(&table, "c1"), 0.0);
    assert!(expect_float(&table, "c1").is_sign_negative());
}

#[test]
fn assign_negative_int() {
    check_simple(
        "num = -2",
        HashMap::from_iter([("num".into(), SimpleVal::Int(-2))]),
    );
}

#[test]
fn assign_positive_int() {
    check_simple(
        "num = +83",
        HashMap::from_iter([("num".into(), SimpleVal::Int(83))]),
    );
}

#[test]
fn assign_sgined_zero_ints() {
    check_simple(
        "num = 0",
        HashMap::from_iter([("num".into(), SimpleVal::Int(0))]),
    );
    check_simple(
        "num = -0",
        HashMap::from_iter([("num".into(), SimpleVal::Int(0))]),
    );
    check_simple(
        "num = +0",
        HashMap::from_iter([("num".into(), SimpleVal::Int(0))]),
    );
}

#[test]
fn assign_sgined_zero_floats() {
    check_simple(
        "num = 0.0",
        HashMap::from_iter([("num".into(), SimpleVal::Float(0.0))]),
    );
    check_simple(
        "num = -0.0",
        HashMap::from_iter([("num".into(), SimpleVal::Float(-0.0))]),
    );
    check_simple(
        "num = +0.0",
        HashMap::from_iter([("num".into(), SimpleVal::Float(0.0))]),
    );
}

#[test]
fn sign_prefixed_binary_int() {
    check_error(
        "num = -0b10100",
        |_, c| [Ast::Assignment(tainvalid(c, 0, 0, "num", "-0b10100"))],
        Error::PrefixedIntSignNotAllowed(Pos { line: 0, char: 6 }),
    );
    check_error(
        "num = +0b10100",
        |_, c| [Ast::Assignment(tainvalid(c, 0, 0, "num", "+0b10100"))],
        Error::PrefixedIntSignNotAllowed(Pos { line: 0, char: 6 }),
    );
}

#[test]
fn sign_prefixed_octal_int() {
    check_error(
        "num = -0o361",
        |_, c| [Ast::Assignment(tainvalid(c, 0, 0, "num", "-0o361"))],
        Error::PrefixedIntSignNotAllowed(Pos { line: 0, char: 6 }),
    );
    check_error(
        "num = +0o361",
        |_, c| [Ast::Assignment(tainvalid(c, 0, 0, "num", "+0o361"))],
        Error::PrefixedIntSignNotAllowed(Pos { line: 0, char: 6 }),
    );
}

#[test]
fn signed_prefixed_hexadecimal_int() {
    check_error(
        "num = -0xc20",
        |_, c| [Ast::Assignment(tainvalid(c, 0, 0, "num", "-0xc20"))],
        Error::PrefixedIntSignNotAllowed(Pos { line: 0, char: 6 }),
    );
    check_error(
        "num = +0xc20",
        |_, c| [Ast::Assignment(tainvalid(c, 0, 0, "num", "+0xc20"))],
        Error::PrefixedIntSignNotAllowed(Pos { line: 0, char: 6 }),
    );
}

#[test]
fn uppercase_binray_radix_not_allowed() {
    check_simple_error(
        "num = 0B10",
        HashMap::from_iter([("num".into(), SimpleVal::Invalid("0B10".into()))]),
        Error::UppercaseIntRadix(IntPrefix::Binary, Pos::new(0, 7)),
    );
}

#[test]
fn uppercase_octal_radix_not_allowed() {
    check_simple_error(
        "num = 0O10",
        HashMap::from_iter([("num".into(), SimpleVal::Invalid("0O10".into()))]),
        Error::UppercaseIntRadix(IntPrefix::Octal, Pos::new(0, 7)),
    );
}

#[test]
fn uppercase_hexadecimal_radix_not_allowed() {
    check_simple_error(
        "num = 0X10",
        HashMap::from_iter([("num".into(), SimpleVal::Invalid("0X10".into()))]),
        Error::UppercaseIntRadix(IntPrefix::Hexadecimal, Pos::new(0, 7)),
    );
}

#[test]
fn assign_bool() {
    check("abc = false", |_, c| {
        [Ast::Assignment(tabool(c, 0, 0, "abc", false))]
    });
}

#[test]
fn assign_float() {
    check("abc = 23.5", |_, c| {
        [Ast::Assignment(tafloat(c, 0, 0, "abc", "23.5"))]
    });
}

#[test]
fn assign_float_with_exp1() {
    check("abc = 23.5e+9", |_, c| {
        [Ast::Assignment(tafloat(c, 0, 0, "abc", "23.5e+9"))]
    });
}

#[test]
fn assign_float_with_exp2() {
    check("abc = 23.5e-1_2", |_, c| {
        [Ast::Assignment(tafloat(c, 0, 0, "abc", "23.5e-1_2"))]
    });
}

#[test]
fn float_fractional_part_ends_with_underscore() {
    check_error(
        "abc = 23.5_e9",
        |_, c| [Ast::Assignment(tainvalid(c, 0, 0, "abc", "23.5_e9"))],
        Error::LitEndsWithUnderscore(LitPart::FloatFract, Pos { line: 0, char: 10 }),
    );
}

#[test]
fn int_with_underscore() {
    check("abc = 1_000", |_, c| {
        [Ast::Assignment(taint(c, 0, 0, "abc", "1_000"))]
    });
}

#[test]
fn invalid_prefixed_int_radix() {
    check_error(
        "abc = 0c324",
        |_, c| [Ast::Assignment(tainvalid(c, 0, 0, "abc", "0c324"))],
        Error::InvalidIntRadix(FmtChar('c'), Pos { line: 0, char: 7 }),
    );
}

#[test]
fn prefixed_int_digit_too_big() {
    check_error(
        "abc = 0o384",
        |_, c| [Ast::Assignment(tainvalid(c, 0, 0, "abc", "0o384"))],
        Error::IntDigitTooBig(IntPrefix::Octal, FmtChar('8'), Pos { line: 0, char: 9 }),
    );
}

#[test]
fn prefixed_int_starts_with_underscore() {
    check_error(
        "abc = 0o_43",
        |_, c| [Ast::Assignment(tainvalid(c, 0, 0, "abc", "0o_43"))],
        Error::PrefixedIntValueStartsWithUnderscore(Pos { line: 0, char: 8 }),
    );
}

#[test]
fn prefixed_int_ends_with_underscore() {
    check_error(
        "abc = 0o43_",
        |_, c| [Ast::Assignment(tainvalid(c, 0, 0, "abc", "0o43_"))],
        Error::PrefixedIntValueEndsWithUnderscore(Pos { line: 0, char: 10 }),
    );
}

#[test]
fn dotted_key() {
    check("a.b.c = false", |bump, comments| {
        [Ast::Assignment(twrap(
            comments,
            0,
            Assignment {
                key: Key::Dotted(bump.alloc([
                    DottedIdent {
                        ident: Ident::from_plain_lit(
                            "a",
                            Span::from_pos_len(Pos { line: 0, char: 0 }, 1),
                        ),
                        dot: Some(Pos { line: 0, char: 1 }),
                    },
                    DottedIdent {
                        ident: Ident::from_plain_lit(
                            "b",
                            Span::from_pos_len(Pos { line: 0, char: 2 }, 1),
                        ),
                        dot: Some(Pos { line: 0, char: 3 }),
                    },
                    DottedIdent {
                        ident: Ident::from_plain_lit(
                            "c",
                            Span::from_pos_len(Pos { line: 0, char: 4 }, 1),
                        ),
                        dot: None,
                    },
                ])),
                eq: Pos { line: 0, char: 6 },
                val: bool(0, 8, false),
            },
        ))]
    });
}

#[test]
fn int_identifier() {
    check("123 = false", |_, c| {
        [Ast::Assignment(tabool(c, 0, 0, "123", false))]
    });
}

#[test]
fn invalid_int_identifier() {
    check_error(
        "+99 = false",
        |_, c| [Ast::Assignment(tabool(c, 0, 0, "+99", false))],
        Error::InvalidCharInIdentifier(FmtChar('+'), Pos { line: 0, char: 0 }),
    );
}

#[test]
fn space_between_array_header_brackets() {
    check_simple_error(
        "[ [a.b]]",
        HashMap::from_iter([(
            "a".into(),
            SimpleVal::Table(HashMap::from_iter([(
                "b".into(),
                SimpleVal::Array(vec![SimpleVal::Table(HashMap::new())]),
            )])),
        )]),
        Error::SpaceBetweenArrayPars(Span::from_pos_len(Pos { line: 0, char: 1 }, 1)),
    );

    check_simple_error(
        "[[a.b] ]",
        HashMap::from_iter([(
            "a".into(),
            SimpleVal::Table(HashMap::from_iter([(
                "b".into(),
                SimpleVal::Array(vec![SimpleVal::Table(HashMap::new())]),
            )])),
        )]),
        Error::SpaceBetweenArrayPars(Span::from_pos_len(Pos { line: 0, char: 6 }, 1)),
    );
}

#[test]
fn invalid_float_literal_as_identifier() {
    check_error(
        "23e+3 = 'hello'",
        |_, c| {
            [Ast::Assignment(tastring(
                c,
                0,
                0,
                "23e+3",
                "'hello'",
                Quote::Literal,
            ))]
        },
        Error::InvalidCharInIdentifier(FmtChar('+'), Pos { line: 0, char: 3 }),
    );
}

#[test]
fn multi_line_literal_string_identifier() {
    check_error(
        "'''abc''' = false",
        |_, _| [],
        Error::MultilineLiteralStringIdent(Span::from_pos_len(Pos::new(0, 0), 9)),
    );
}

#[test]
fn multi_line_basic_string_identifier() {
    check_error(
        r#""""abc""" = false"#,
        |_, _| [],
        Error::MultilineBasicStringIdent(Span::from_pos_len(Pos::new(0, 0), 9)),
    );
}

#[test]
fn inline_array() {
    check("array = [0, 1, 2]", |bump, comments| {
        [Ast::Assignment(ta(
            comments,
            0,
            0,
            "array",
            Value::InlineArray(InlineArray {
                comments: empty_comments(comments, 0),
                l_par: Pos { line: 0, char: 8 },
                values: bump.alloc([
                    InlineArrayValue {
                        comments: empty_comments(comments, 1),
                        val: int(0, 9, "0"),
                        comma: Some(Pos { line: 0, char: 10 }),
                    },
                    InlineArrayValue {
                        comments: empty_comments(comments, 1),
                        val: int(0, 12, "1"),
                        comma: Some(Pos { line: 0, char: 13 }),
                    },
                    InlineArrayValue {
                        comments: empty_comments(comments, 1),
                        val: int(0, 15, "2"),
                        comma: None,
                    },
                ]),
                r_par: Some(Pos { line: 0, char: 16 }),
            }),
        ))]
    });
}

#[test]
fn multi_line_inline_array() {
    check("array = [\n  0,\n  1,\n  2,\n]", |bump, comments| {
        [Ast::Assignment(ta(
            comments,
            0,
            0,
            "array",
            Value::InlineArray(InlineArray {
                comments: empty_comments(comments, 0),
                l_par: Pos { line: 0, char: 8 },
                values: bump.alloc([
                    InlineArrayValue {
                        comments: empty_comments(comments, 1),
                        val: int(1, 2, "0"),
                        comma: Some(Pos { line: 1, char: 3 }),
                    },
                    InlineArrayValue {
                        comments: empty_comments(comments, 1),
                        val: int(2, 2, "1"),
                        comma: Some(Pos { line: 2, char: 3 }),
                    },
                    InlineArrayValue {
                        comments: empty_comments(comments, 1),
                        val: int(3, 2, "2"),
                        comma: Some(Pos { line: 3, char: 3 }),
                    },
                ]),
                r_par: Some(Pos { line: 4, char: 0 }),
            }),
        ))]
    });
}

#[test]
fn inline_array_recover_comma() {
    check_error(
        "array = [0, 1  2]",
        |bump, comments| {
            [Ast::Assignment(ta(
                comments,
                0,
                0,
                "array",
                Value::InlineArray(InlineArray {
                    comments: empty_comments(comments, 0),
                    l_par: Pos { line: 0, char: 8 },
                    values: bump.alloc([
                        InlineArrayValue {
                            comments: empty_comments(comments, 1),
                            val: int(0, 9, "0"),
                            comma: Some(Pos { line: 0, char: 10 }),
                        },
                        InlineArrayValue {
                            comments: empty_comments(comments, 1),
                            val: int(0, 12, "1"),
                            comma: None,
                        },
                        InlineArrayValue {
                            comments: empty_comments(comments, 1),
                            val: int(0, 15, "2"),
                            comma: None,
                        },
                    ]),
                    r_par: Some(Pos { line: 0, char: 16 }),
                }),
            ))]
        },
        Error::MissingComma(Pos { line: 0, char: 13 }),
    );
}

#[test]
fn inline_array_recover_invalid() {
    check_error(
        "array = [0, 1, 2, =]",
        |bump, comments| {
            [Ast::Assignment(ta(
                comments,
                0,
                0,
                "array",
                Value::InlineArray(InlineArray {
                    comments: empty_comments(comments, 0),
                    l_par: Pos { line: 0, char: 8 },
                    values: bump.alloc([
                        InlineArrayValue {
                            comments: empty_comments(comments, 1),
                            val: int(0, 9, "0"),
                            comma: Some(Pos { line: 0, char: 10 }),
                        },
                        InlineArrayValue {
                            comments: empty_comments(comments, 1),
                            val: int(0, 12, "1"),
                            comma: Some(Pos { line: 0, char: 13 }),
                        },
                        InlineArrayValue {
                            comments: empty_comments(comments, 1),
                            val: int(0, 15, "2"),
                            comma: Some(Pos { line: 0, char: 16 }),
                        },
                    ]),
                    r_par: Some(Pos { line: 0, char: 19 }),
                }),
            ))]
        },
        Error::ExpectedValueFound("=".into(), Span::from_pos_len(Pos { line: 0, char: 18 }, 1)),
    );
}

#[test]
fn nested_inline_array() {
    check("array = [[0], [1]]", |bump, comments| {
        [Ast::Assignment(ta(
            comments,
            0,
            0,
            "array",
            Value::InlineArray(InlineArray {
                comments: empty_comments(comments, 0),
                l_par: Pos { line: 0, char: 8 },
                values: bump.alloc([
                    InlineArrayValue {
                        comments: empty_comments(comments, 1),
                        val: Value::InlineArray(InlineArray {
                            comments: empty_comments(comments, 1),
                            l_par: Pos { line: 0, char: 9 },
                            values: bump.alloc([InlineArrayValue {
                                comments: empty_comments(comments, 2),
                                val: int(0, 10, "0"),
                                comma: None,
                            }]),
                            r_par: Some(Pos { line: 0, char: 11 }),
                        }),
                        comma: Some(Pos { line: 0, char: 12 }),
                    },
                    InlineArrayValue {
                        comments: empty_comments(comments, 1),
                        val: Value::InlineArray(InlineArray {
                            comments: empty_comments(comments, 1),
                            l_par: Pos { line: 0, char: 14 },
                            values: bump.alloc([InlineArrayValue {
                                comments: empty_comments(comments, 2),
                                val: int(0, 15, "1"),
                                comma: None,
                            }]),
                            r_par: Some(Pos { line: 0, char: 16 }),
                        }),
                        comma: None,
                    },
                ]),
                r_par: Some(Pos { line: 0, char: 17 }),
            }),
        ))]
    });
}

#[test]
fn inline_table() {
    check("table = { a = 3, b = true }", |bump, comments| {
        [Ast::Assignment(ta(
            comments,
            0,
            0,
            "table",
            Value::InlineTable(InlineTable {
                l_par: Pos { line: 0, char: 8 },
                assignments: bump.alloc([
                    InlineTableAssignment {
                        assignment: aint(0, 10, "a", "3"),
                        comma: Some(Pos { line: 0, char: 15 }),
                    },
                    InlineTableAssignment {
                        assignment: abool(0, 17, "b", true),
                        comma: None,
                    },
                ]),
                r_par: Some(Pos { line: 0, char: 26 }),
            }),
        ))]
    });
}

#[test]
fn inline_table_recover_missing_comma() {
    check_error(
        "table = { a = 3  b = true }",
        |bump, comments| {
            [Ast::Assignment(ta(
                comments,
                0,
                0,
                "table",
                Value::InlineTable(InlineTable {
                    l_par: Pos { line: 0, char: 8 },
                    assignments: bump.alloc([
                        InlineTableAssignment {
                            assignment: aint(0, 10, "a", "3"),
                            comma: None,
                        },
                        InlineTableAssignment {
                            assignment: abool(0, 17, "b", true),
                            comma: None,
                        },
                    ]),
                    r_par: Some(Pos { line: 0, char: 26 }),
                }),
            ))]
        },
        Error::MissingComma(Pos { line: 0, char: 15 }),
    );
}

#[test]
fn inline_table_recover_invalid() {
    check_error(
        "table = { a = 3, b = true, = }",
        |bump, comments| {
            [Ast::Assignment(ta(
                comments,
                0,
                0,
                "table",
                Value::InlineTable(InlineTable {
                    l_par: Pos { line: 0, char: 8 },
                    assignments: bump.alloc([
                        InlineTableAssignment {
                            assignment: aint(0, 10, "a", "3"),
                            comma: Some(Pos { line: 0, char: 15 }),
                        },
                        InlineTableAssignment {
                            assignment: abool(0, 17, "b", true),
                            comma: Some(Pos { line: 0, char: 25 }),
                        },
                    ]),
                    r_par: Some(Pos { line: 0, char: 29 }),
                }),
            ))]
        },
        Error::ExpectedKeyFound(
            "=".into(),
            Span {
                start: Pos { line: 0, char: 27 },
                end: Pos { line: 0, char: 28 },
            },
        ),
    );
}

#[test]
fn table_header() {
    check("[my_table]\nentry = false\n", |bump, comments| {
        [Ast::Table(Table {
            comments: empty_comments(comments, 0),
            header: TableHeader::new(
                Pos { line: 0, char: 0 },
                Some(Key::One(Ident::from_plain_lit(
                    "my_table",
                    Span::from_pos_len(Pos { line: 0, char: 1 }, 8),
                ))),
                Some(Pos { line: 0, char: 9 }),
            ),
            assignments: bvec![in bump; tabool(comments, 1, 1, "entry", false)],
        })]
    })
}

#[test]
fn array_header() {
    check("[[my_array]]\nentry = false\n", |bump, comments| {
        [Ast::Array(ArrayEntry {
            comments: empty_comments(comments, 0),
            header: ArrayHeader::new(
                (Pos { line: 0, char: 0 }, Pos { line: 0, char: 1 }),
                Some(Key::One(Ident::from_plain_lit(
                    "my_array",
                    Span::from_pos_len(Pos { line: 0, char: 2 }, 8),
                ))),
                (
                    Some(Pos { line: 0, char: 10 }),
                    Some(Pos { line: 0, char: 11 }),
                ),
            ),
            assignments: bvec![in bump; tabool(comments, 1, 1, "entry", false)],
        })]
    })
}

#[test]
fn newline_is_required_after_table_header() {
    check_error(
        "[my_table]entry = false\n",
        |bump, comments| {
            [Ast::Table(Table {
                comments: empty_comments(comments, 0),
                header: TableHeader::new(
                    Pos { line: 0, char: 0 },
                    Some(Key::One(Ident::from_plain_lit(
                        "my_table",
                        Span::from_pos_len(Pos { line: 0, char: 1 }, 8),
                    ))),
                    Some(Pos { line: 0, char: 9 }),
                ),
                assignments: bvec![in bump; twrap(comments, 1, abool(0, 10, "entry", false))],
            })]
        },
        Error::MissingNewline(Pos { line: 0, char: 10 }),
    )
}

#[test]
fn newline_is_required_after_assignment() {
    check_error(
        "a = false b = 87",
        |_, comments| {
            [
                Ast::Assignment(twrap(comments, 0, abool(0, 0, "a", false))),
                Ast::Assignment(twrap(comments, 0, aint(0, 10, "b", "87"))),
            ]
        },
        Error::MissingNewline(Pos { line: 0, char: 10 }),
    )
}

#[test]
fn table_header_with_associated_comment_above() {
    check_comments("# associated\n[my_table]\n", |bump, comments| {
        [Ast::Table(Table {
            comments: build_comments(
                comments,
                0,
                [AssocComment {
                    pos: AssocPos::Above,
                    level: 0,
                    comment: Comment {
                        span: Span::from_pos_len(Pos { line: 0, char: 0 }, 12),
                        text: " associated",
                    },
                }],
            ),
            header: TableHeader::new(
                Pos { line: 1, char: 0 },
                Some(Key::One(Ident::from_plain_lit(
                    "my_table",
                    Span::from_pos_len(Pos { line: 1, char: 1 }, 8),
                ))),
                Some(Pos { line: 1, char: 9 }),
            ),
            assignments: BVec::new_in(bump),
        })]
    })
}

#[test]
fn non_associated_comment() {
    check("# free standing\n\n[my_table]\n", |bump, comments| {
        [
            Ast::Comment(Comment {
                span: Span::from_pos_len(Pos { line: 0, char: 0 }, 15),
                text: " free standing",
            }),
            Ast::Table(Table {
                comments: empty_comments(comments, 0),
                header: TableHeader::new(
                    Pos { line: 2, char: 0 },
                    Some(Key::One(Ident::from_plain_lit(
                        "my_table",
                        Span::from_pos_len(Pos { line: 2, char: 1 }, 8),
                    ))),
                    Some(Pos { line: 2, char: 9 }),
                ),
                assignments: BVec::new_in(bump),
            }),
        ]
    })
}

#[test]
fn comment_after_table_header() {
    check_comments("[my_table] # comment\nentry = false\n", |bump, comments| {
        [Ast::Table(Table {
            comments: build_comments(
                comments,
                0,
                [AssocComment {
                    pos: AssocPos::LineEnd,
                    level: 0,
                    comment: Comment {
                        span: Span::from_pos_len(Pos { line: 0, char: 11 }, 9),
                        text: " comment",
                    },
                }],
            ),
            header: TableHeader::new(
                Pos { line: 0, char: 0 },
                Some(Key::One(Ident::from_plain_lit(
                    "my_table",
                    Span::from_pos_len(Pos { line: 0, char: 1 }, 8),
                ))),
                Some(Pos { line: 0, char: 9 }),
            ),
            assignments: bvec![in bump; tabool(comments, 1, 1, "entry", false)],
        })]
    })
}

#[test]
fn associated_comments_above_assignment() {
    check_comments("# comment 1\n# comment 2\nabc = false", |_, comments| {
        [Ast::Assignment(ToplevelAssignment {
            comments: build_comments(
                comments,
                0,
                [
                    AssocComment {
                        pos: AssocPos::Above,
                        level: 0,
                        comment: Comment {
                            span: Span::from_pos_len(Pos { line: 0, char: 0 }, 11),
                            text: " comment 1",
                        },
                    },
                    AssocComment {
                        pos: AssocPos::Above,
                        level: 0,
                        comment: Comment {
                            span: Span::from_pos_len(Pos { line: 1, char: 0 }, 11),
                            text: " comment 2",
                        },
                    },
                ],
            ),
            assignment: abool(2, 0, "abc", false),
        })]
    })
}

#[test]
fn comment_after_assignment() {
    check_comments("abc = false # comment\n", |_, comments| {
        [Ast::Assignment(ToplevelAssignment {
            comments: build_comments(
                comments,
                0,
                [AssocComment {
                    pos: AssocPos::LineEnd,
                    level: 0,
                    comment: Comment {
                        span: Span::from_pos_len(Pos { line: 0, char: 12 }, 9),
                        text: " comment",
                    },
                }],
            ),
            assignment: abool(0, 0, "abc", false),
        })]
    })
}

#[test]
fn comment_separated_by_blank_line_is_not_associated() {
    check_comments(
        "# free standing\n\n# associated\nabc = false",
        |_, comments| {
            [
                Ast::Comment(Comment {
                    span: Span::from_pos_len(Pos { line: 0, char: 0 }, 15),
                    text: " free standing",
                }),
                Ast::Assignment(ToplevelAssignment {
                    comments: build_comments(
                        comments,
                        0,
                        [AssocComment {
                            pos: AssocPos::Above,
                            level: 0,
                            comment: Comment {
                                span: Span::from_pos_len(Pos { line: 2, char: 0 }, 12),
                                text: " associated",
                            },
                        }],
                    ),
                    assignment: abool(3, 0, "abc", false),
                }),
            ]
        },
    )
}

#[test]
fn comment_is_last_token() {
    check("abc = false\n# free standing", |_, comments| {
        [
            Ast::Assignment(tabool(comments, 0, 0, "abc", false)),
            Ast::Comment(Comment {
                span: Span::from_pos_len(Pos { line: 1, char: 0 }, 15),
                text: " free standing",
            }),
        ]
    })
}

#[test]
fn comment_contained_by_table() {
    check_comments(
        "[my_table]\n\n# contained comment\n\nabc = false",
        |bump, comments| {
            [Ast::Table(Table {
                comments: build_comments(
                    comments,
                    0,
                    [AssocComment {
                        pos: AssocPos::Contained,
                        level: 0,
                        comment: Comment {
                            span: Span::from_pos_len(Pos { line: 2, char: 0 }, 19),
                            text: " contained comment",
                        },
                    }],
                ),
                header: TableHeader::new(
                    Pos { line: 0, char: 0 },
                    Some(Key::One(Ident::from_plain_lit(
                        "my_table",
                        Span::from_pos_len(Pos { line: 0, char: 1 }, 8),
                    ))),
                    Some(Pos { line: 0, char: 9 }),
                ),
                assignments: bvec![in bump; tabool(comments, 1, 4, "abc", false)],
            })]
        },
    )
}

#[test]
fn associated_comments_in_inline_array() {
    check_comments(
        "array = [\n# comment 1\n# comment 2\n\n# above value\n1 # after value\n# contained comment\n, # after comma\n# comment 3\n]",
        |bump, comments| {
            let asts = [Ast::Assignment(ToplevelAssignment {
                comments: CommentRange::new(CommentId(0), 7, 0),
                assignment: a(
                    0,
                    0,
                    "array",
                    Value::InlineArray(InlineArray {
                        comments: {
                            build_comments(comments, 0, [
                                AssocComment {
                                    pos: AssocPos::Contained,
                                    level: 0,
                                    comment: Comment {
                                        span: Span::from_pos_len(Pos { line: 1, char: 0 }, 11),
                                        text: " comment 1",
                                    },
                                },
                                AssocComment {
                                    pos: AssocPos::Contained,
                                    level: 0,
                                    comment: Comment {
                                        span: Span::from_pos_len(Pos { line: 2, char: 0 }, 11),
                                        text: " comment 2",
                                    },
                                },
                            ]);

                            // last comment is added at the end
                            CommentRange::new(CommentId(0), 7, 0)
                        },
                        l_par: Pos { line: 0, char: 8 },
                        values: bump.alloc([
                            InlineArrayValue {
                                comments: build_comments(comments, 1, [
                                    AssocComment{
                                        pos: AssocPos::Above,
                                        level: 1,
                                        comment: Comment {
                                            span: Span::from_pos_len(Pos { line: 4, char: 0 }, 13),
                                            text: " above value",
                                        },
                                    },
                                    AssocComment{
                                        pos: AssocPos::LineEnd,
                                        level: 1,
                                        comment: Comment {
                                            span: Span::from_pos_len(Pos { line: 5, char: 2 }, 13),
                                            text: " after value",
                                        },
                                    },
                                    AssocComment{
                                        pos: AssocPos::Contained,
                                        level: 1,
                                        comment: Comment {
                                            span: Span::from_pos_len(Pos { line: 6, char: 0 }, 19),
                                            text: " contained comment",
                                        },
                                    },
                                    AssocComment{
                                        pos: AssocPos::LineEnd,
                                        level: 1,
                                        comment: Comment {
                                            span: Span::from_pos_len(Pos { line: 7, char: 2 }, 13),
                                            text: " after comma",
                                        },
                                    },
                                ]),
                                val: int(5, 0, "1"), comma: Some(Pos { line: 7, char: 0 }),
                            }
                        ]),
                        r_par: Some(Pos { line: 9, char: 0 }),
                    }),
                ),
            })];

            // add last comment
            build_comments(comments, 0, [AssocComment {
                pos: AssocPos::Contained,
                level: 0,
                comment: Comment {
                    span: Span::from_pos_len(Pos { line: 8, char: 0 }, 11),
                    text: " comment 3",
                },
            }]);

            asts
        }
    );
}

#[test]
fn offset_date_time_with_subsec() {
    check("abc = 2023-12-05T10:11:12.3324243-04:30", |_, c| {
        [Ast::Assignment(ta(
            c,
            0,
            0,
            "abc",
            Value::DateTime(DateTimeVal {
                lit: "2023-12-05T10:11:12.3324243-04:30",
                lit_span: Span::from_pos_len(Pos { line: 0, char: 6 }, 33),
                val: DateTime::OffsetDateTime(
                    Date::new(2023, 12, 5),
                    Time::new(10, 11, 12, 332_424_300),
                    Offset::Custom(-(4 * 60 + 30)),
                ),
            }),
        ))]
    });
}

#[test]
fn offset_date_time_without_subsec() {
    check("abc = 2023-12-05T10:11:12+04:30", |_, c| {
        [Ast::Assignment(ta(
            c,
            0,
            0,
            "abc",
            Value::DateTime(DateTimeVal {
                lit: "2023-12-05T10:11:12+04:30",
                lit_span: Span::from_pos_len(Pos { line: 0, char: 6 }, 25),
                val: DateTime::OffsetDateTime(
                    Date::new(2023, 12, 5),
                    Time::new(10, 11, 12, 0),
                    Offset::Custom(4 * 60 + 30),
                ),
            }),
        ))]
    });
}

#[test]
fn offset_date_time_with_z_suffix() {
    check("abc = 2023-12-05T10:11:12Z", |_, c| {
        [Ast::Assignment(ta(
            c,
            0,
            0,
            "abc",
            Value::DateTime(DateTimeVal {
                lit: "2023-12-05T10:11:12Z",
                lit_span: Span::from_pos_len(Pos { line: 0, char: 6 }, 20),
                val: DateTime::OffsetDateTime(
                    Date::new(2023, 12, 5),
                    Time::new(10, 11, 12, 0),
                    Offset::Utc,
                ),
            }),
        ))]
    });
}

#[test]
fn space_separated_time() {
    check("abc = 2023-12-05 10:11:12", |_, c| {
        [Ast::Assignment(ta(
            c,
            0,
            0,
            "abc",
            Value::DateTime(DateTimeVal {
                lit: "2023-12-05 10:11:12",
                lit_span: Span::from_pos_len(Pos { line: 0, char: 6 }, 19),
                val: DateTime::LocalDateTime(Date::new(2023, 12, 5), Time::new(10, 11, 12, 0)),
            }),
        ))]
    });
}

#[test]
fn local_date() {
    check("abc = 2023-12-05", |_, c| {
        [Ast::Assignment(ta(
            c,
            0,
            0,
            "abc",
            Value::DateTime(DateTimeVal {
                lit: "2023-12-05",
                lit_span: Span::from_pos_len(Pos { line: 0, char: 6 }, 10),
                val: DateTime::LocalDate(Date::new(2023, 12, 5)),
            }),
        ))]
    });
}

#[test]
fn local_time() {
    check("abc = 10:11:12", |_, c| {
        [Ast::Assignment(ta(
            c,
            0,
            0,
            "abc",
            Value::DateTime(DateTimeVal {
                lit: "10:11:12",
                lit_span: Span::from_pos_len(Pos { line: 0, char: 6 }, 8),
                val: DateTime::LocalTime(Time::new(10, 11, 12, 0)),
            }),
        ))]
    });
}

#[test]
fn local_time_hour_out_of_range() {
    check_error(
        "abc = 25:00:00",
        |_, c| [Ast::Assignment(tainvalid(c, 0, 0, "abc", "25:00:00"))],
        Error::DateTimeOutOfBounds(
            DateTimeField::Hour,
            25,
            (0, 23),
            Span::from_pos_len(Pos { line: 0, char: 6 }, 2),
        ),
    );
}

// TODO: date time error tests
