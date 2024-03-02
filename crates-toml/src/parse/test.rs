use std::collections::HashMap;

use pretty_assertions::assert_eq;

use super::*;
use crate::datetime::{DateTimeField, Offset};
use crate::map::simple::SimpleVal;
use crate::test::{check_simple, check_simple_error};
use crate::Warning;

fn check<const SIZE: usize>(input: &str, expected: [Ast; SIZE]) {
    let mut ctx = Ctx::default();
    let tokens = ctx.lex(input);
    let asts = ctx.parse(&tokens);
    assert_eq!(
        expected.as_slice(),
        asts,
        "\nerrors: {:#?}\nwarnings: {:#?}",
        ctx.errors,
        ctx.warnings
    );
    assert_eq!(Vec::<Error>::new(), ctx.errors);
    assert_eq!(Vec::<Warning>::new(), ctx.warnings);
}

fn check_error<const SIZE: usize>(input: &str, expected: [Ast; SIZE], error: Error) {
    let mut ctx = Ctx::default();
    let tokens = ctx.lex(input);
    let asts = ctx.parse(&tokens);
    assert_eq!(
        expected.as_slice(),
        asts,
        "\nerrors: {:#?}\nwarnings: {:#?}",
        ctx.errors,
        ctx.warnings
    );
    assert_eq!(vec![error], ctx.errors);
    assert_eq!(Vec::<Warning>::new(), ctx.warnings);
}

fn int<'a>(line: u32, char: u32, lit: &'a str) -> Value<'a> {
    let val_span = Span::from_pos_len(Pos { line, char }, lit.len() as u32);
    let num = lit.replace("_", "").parse::<i64>().unwrap();
    Value::Int(IntVal {
        lit,
        lit_span: val_span,
        val: num,
    })
}

fn bool<'a>(line: u32, char: u32, val: bool) -> Value<'a> {
    let val_span = Span::from_pos_len(Pos { line, char }, if val { 4 } else { 5 });
    Value::Bool(BoolVal {
        lit_span: val_span,
        val,
    })
}

fn a<'a>(line: u32, char: u32, ident: &'a str, val: Value<'a>) -> Assignment<'a> {
    let ident_span = Span::from_pos_len(Pos { line, char }, ident.len() as u32);
    Assignment {
        key: Key::One(Ident {
            lit: ident,
            lit_span: ident_span,
            text: ident,
            text_span: ident_span,
            kind: IdentKind::Plain,
        }),
        eq: ident_span.end.plus(1),
        val,
    }
}

fn ainvalid<'a>(line: u32, char: u32, ident: &'a str, val: &'a str) -> Assignment<'a> {
    let val_span = Span::from_pos_len(
        Pos {
            line,
            char: char + ident.len() as u32 + 3,
        },
        val.len() as u32,
    );
    let val = Value::Invalid(val, val_span);
    a(line, char, ident, val)
}

fn aint<'a>(line: u32, char: u32, ident: &'a str, lit: &'a str) -> Assignment<'a> {
    let val = int(line, char + ident.len() as u32 + 3, lit);
    a(line, char, ident, val)
}

fn afloat<'a>(line: u32, char: u32, ident: &'a str, val: &'a str) -> Assignment<'a> {
    let val_span = Span::from_pos_len(
        Pos {
            line,
            char: char + ident.len() as u32 + 3,
        },
        val.len() as u32,
    );
    let num = val.replace("_", "").parse::<f64>().unwrap();
    let val = Value::Float(FloatVal {
        lit: val,
        lit_span: val_span,
        val: num,
    });
    a(line, char, ident, val)
}

fn abool<'a>(line: u32, char: u32, ident: &'a str, val: bool) -> Assignment<'a> {
    let val = bool(line, char + ident.len() as u32 + 3, val);
    a(line, char, ident, val)
}
fn astring<'a>(line: u32, char: u32, ident: &'a str, lit: &'a str, quote: Quote) -> Assignment<'a> {
    let lit_span = Span::from_pos_len(
        Pos {
            line,
            char: char + ident.len() as u32 + 3,
        },
        lit.len() as u32,
    );
    let text = lit.trim_start_matches("'");
    let start_offset = lit.len() - text.len();
    let text = text.trim_end_matches("'");
    let end_offset = lit.len() - text.len() - start_offset;
    let text_span = Span {
        start: lit_span.start.plus(start_offset as u32),
        end: lit_span.end.minus(end_offset as u32),
    };
    let val = Value::String(StringVal {
        lit_span,
        lit,
        text,
        text_span,
        quote,
    });
    a(line, char, ident, val)
}

fn ta<'a>(line: u32, ident: &'a str, val: Value<'a>) -> ToplevelAssignment<'a> {
    ToplevelAssignment {
        comments: Vec::new(),
        assignment: a(line, 0, ident, val),
    }
}

fn tainvalid<'a>(line: u32, ident: &'a str, val: &'a str) -> ToplevelAssignment<'a> {
    ainvalid(line, 0, ident, val).into()
}

fn taint<'a>(line: u32, ident: &'a str, val: &'a str) -> ToplevelAssignment<'a> {
    aint(line, 0, ident, val).into()
}

fn tafloat<'a>(line: u32, ident: &'a str, val: &'a str) -> ToplevelAssignment<'a> {
    afloat(line, 0, ident, val).into()
}

fn tabool<'a>(line: u32, ident: &'a str, val: bool) -> ToplevelAssignment<'a> {
    abool(line, 0, ident, val).into()
}

fn tastring<'a>(line: u32, ident: &'a str, lit: &'a str, quote: Quote) -> ToplevelAssignment<'a> {
    astring(line, 0, ident, lit, quote).into()
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
fn negative_prefixed_binary_int() {
    check_simple(
        "num = -0b10100",
        HashMap::from_iter([("num".into(), SimpleVal::Int(-0b10100))]),
    );
}

#[test]
fn negative_prefixed_octal_int() {
    check_simple(
        "num = -0o361",
        HashMap::from_iter([("num".into(), SimpleVal::Int(-0o361))]),
    );
}

#[test]
fn negative_prefixed_hexadecimal_int() {
    check_simple(
        "num = -0xc20",
        HashMap::from_iter([("num".into(), SimpleVal::Int(-0xc20))]),
    );
}

#[test]
fn positive_sign_not_allowed_for_prefixed_int() {
    check_simple_error(
        "num = +0xc20",
        HashMap::from_iter([("num".into(), SimpleVal::Invalid("+0xc20".into()))]),
        Error::PrefixedIntPositiveSignNotAllowed(Pos::new(0, 6)),
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
    check("abc = false", [Ast::Assignment(tabool(0, "abc", false))]);
}

#[test]
fn assign_float() {
    check("abc = 23.5", [Ast::Assignment(tafloat(0, "abc", "23.5"))]);
}

#[test]
fn assign_float_with_exp1() {
    check(
        "abc = 23.5e+9",
        [Ast::Assignment(tafloat(0, "abc", "23.5e+9"))],
    );
}

#[test]
fn assign_float_with_exp2() {
    check(
        "abc = 23.5e-1_2",
        [Ast::Assignment(tafloat(0, "abc", "23.5e-1_2"))],
    );
}

#[test]
fn float_fractional_part_ends_with_underscore() {
    check_error(
        "abc = 23.5_e9",
        [Ast::Assignment(tainvalid(0, "abc", "23.5_e9"))],
        Error::FloatFractEndsWithUnderscore(Pos { line: 0, char: 10 }),
    );
}

#[test]
fn int_with_underscore() {
    check("abc = 1_000", [Ast::Assignment(taint(0, "abc", "1_000"))]);
}

#[test]
fn invalid_prefixed_int_radix() {
    check_error(
        "abc = 0c324",
        [Ast::Assignment(tainvalid(0, "abc", "0c324"))],
        Error::InvalidIntRadix(FmtChar('c'), Pos { line: 0, char: 7 }),
    );
}

#[test]
fn prefixed_int_digit_too_big() {
    check_error(
        "abc = 0o384",
        [Ast::Assignment(tainvalid(0, "abc", "0o384"))],
        Error::IntDigitTooBig(IntPrefix::Octal, FmtChar('8'), Pos { line: 0, char: 9 }),
    );
}

#[test]
fn prefixed_int_starts_with_underscore() {
    check_error(
        "abc = 0o_43",
        [Ast::Assignment(tainvalid(0, "abc", "0o_43"))],
        Error::PrefixedIntValueStartsWithUnderscore(Pos { line: 0, char: 8 }),
    );
}

#[test]
fn prefixed_int_ends_with_underscore() {
    check_error(
        "abc = 0o43_",
        [Ast::Assignment(tainvalid(0, "abc", "0o43_"))],
        Error::PrefixedIntValueEndsWithUnderscore(Pos { line: 0, char: 10 }),
    );
}

#[test]
fn dotted_key() {
    check(
        "a.b.c = false",
        [Ast::Assignment(
            Assignment {
                key: Key::Dotted(vec![
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
                ]),
                eq: Pos { line: 0, char: 6 },
                val: bool(0, 8, false),
            }
            .into(),
        )],
    );
}

#[test]
fn int_identifier() {
    check("123 = false", [Ast::Assignment(tabool(0, "123", false))]);
}

#[test]
fn invalid_int_identifier() {
    check_error(
        "+99 = false",
        [Ast::Assignment(tabool(0, "+99", false))],
        Error::InvalidCharInIdentifier(FmtChar('+'), Pos { line: 0, char: 0 }),
    );
}

#[test]
fn invalid_float_literal_as_identifier() {
    check_error(
        "23e+3 = 'hello'",
        [Ast::Assignment(tastring(
            0,
            "23e+3",
            "'hello'",
            Quote::Literal,
        ))],
        Error::InvalidCharInIdentifier(FmtChar('+'), Pos { line: 0, char: 3 }),
    );
}

#[test]
fn multi_line_literal_string_identifier() {
    check_error(
        "'''abc''' = false",
        [],
        Error::MultilineLiteralStringIdent(Span::from_pos_len(Pos::new(0, 0), 9)),
    );
}

#[test]
fn multi_line_basic_string_identifier() {
    check_error(
        r#""""abc""" = false"#,
        [],
        Error::MultilineBasicStringIdent(Span::from_pos_len(Pos::new(0, 0), 9)),
    );
}

#[test]
fn inline_array() {
    check(
        "array = [0, 1, 2]",
        [Ast::Assignment(ta(
            0,
            "array",
            Value::InlineArray(InlineArray {
                l_par: Pos { line: 0, char: 8 },
                values: vec![
                    InlineArrayValue {
                        val: int(0, 9, "0"),
                        comma: Some(Pos { line: 0, char: 10 }),
                    },
                    InlineArrayValue {
                        val: int(0, 12, "1"),
                        comma: Some(Pos { line: 0, char: 13 }),
                    },
                    InlineArrayValue {
                        val: int(0, 15, "2"),
                        comma: None,
                    },
                ],
                r_par: Some(Pos { line: 0, char: 16 }),
            }),
        ))],
    );
}

#[test]
fn multi_line_inline_array() {
    check(
        "array = [\n  0,\n  1,\n  2,\n]",
        [Ast::Assignment(ta(
            0,
            "array",
            Value::InlineArray(InlineArray {
                l_par: Pos { line: 0, char: 8 },
                values: vec![
                    InlineArrayValue {
                        val: int(1, 2, "0"),
                        comma: Some(Pos { line: 1, char: 3 }),
                    },
                    InlineArrayValue {
                        val: int(2, 2, "1"),
                        comma: Some(Pos { line: 2, char: 3 }),
                    },
                    InlineArrayValue {
                        val: int(3, 2, "2"),
                        comma: Some(Pos { line: 3, char: 3 }),
                    },
                ],
                r_par: Some(Pos { line: 4, char: 0 }),
            }),
        ))],
    );
}

#[test]
fn inline_array_recover_comma() {
    check_error(
        "array = [0, 1  2]",
        [Ast::Assignment(ta(
            0,
            "array",
            Value::InlineArray(InlineArray {
                l_par: Pos { line: 0, char: 8 },
                values: vec![
                    InlineArrayValue {
                        val: int(0, 9, "0"),
                        comma: Some(Pos { line: 0, char: 10 }),
                    },
                    InlineArrayValue {
                        val: int(0, 12, "1"),
                        comma: None,
                    },
                    InlineArrayValue {
                        val: int(0, 15, "2"),
                        comma: None,
                    },
                ],
                r_par: Some(Pos { line: 0, char: 16 }),
            }),
        ))],
        Error::MissingComma(Pos { line: 0, char: 13 }),
    );
}

#[test]
fn inline_array_recover_invalid() {
    check_error(
        "array = [0, 1, 2, =]",
        [Ast::Assignment(ta(
            0,
            "array",
            Value::InlineArray(InlineArray {
                l_par: Pos { line: 0, char: 8 },
                values: vec![
                    InlineArrayValue {
                        val: int(0, 9, "0"),
                        comma: Some(Pos { line: 0, char: 10 }),
                    },
                    InlineArrayValue {
                        val: int(0, 12, "1"),
                        comma: Some(Pos { line: 0, char: 13 }),
                    },
                    InlineArrayValue {
                        val: int(0, 15, "2"),
                        comma: Some(Pos { line: 0, char: 16 }),
                    },
                ],
                r_par: Some(Pos { line: 0, char: 19 }),
            }),
        ))],
        Error::ExpectedValueFound("=".into(), Span::from_pos_len(Pos { line: 0, char: 18 }, 1)),
    );
}

#[test]
fn inline_table() {
    check(
        "table = { a = 3, b = true }",
        [Ast::Assignment(ta(
            0,
            "table",
            Value::InlineTable(InlineTable {
                l_par: Pos { line: 0, char: 8 },
                assignments: vec![
                    InlineTableAssignment {
                        assignment: aint(0, 10, "a", "3"),
                        comma: Some(Pos { line: 0, char: 15 }),
                    },
                    InlineTableAssignment {
                        assignment: abool(0, 17, "b", true),
                        comma: None,
                    },
                ],
                r_par: Some(Pos { line: 0, char: 26 }),
            }),
        ))],
    );
}

#[test]
fn inline_table_recover_missing_comma() {
    check_error(
        "table = { a = 3  b = true }",
        [Ast::Assignment(ta(
            0,
            "table",
            Value::InlineTable(InlineTable {
                l_par: Pos { line: 0, char: 8 },
                assignments: vec![
                    InlineTableAssignment {
                        assignment: aint(0, 10, "a", "3"),
                        comma: None,
                    },
                    InlineTableAssignment {
                        assignment: abool(0, 17, "b", true),
                        comma: None,
                    },
                ],
                r_par: Some(Pos { line: 0, char: 26 }),
            }),
        ))],
        Error::MissingComma(Pos { line: 0, char: 15 }),
    );
}

#[test]
fn inline_table_recover_invalid() {
    check_error(
        "table = { a = 3, b = true, = }",
        [Ast::Assignment(ta(
            0,
            "table",
            Value::InlineTable(InlineTable {
                l_par: Pos { line: 0, char: 8 },
                assignments: vec![
                    InlineTableAssignment {
                        assignment: aint(0, 10, "a", "3"),
                        comma: Some(Pos { line: 0, char: 15 }),
                    },
                    InlineTableAssignment {
                        assignment: abool(0, 17, "b", true),
                        comma: Some(Pos { line: 0, char: 25 }),
                    },
                ],
                r_par: Some(Pos { line: 0, char: 29 }),
            }),
        ))],
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
    check(
        "[my_table]\nentry = false\n",
        [Ast::Table(Table {
            comments: Vec::new(),
            header: TableHeader {
                l_par: Pos { line: 0, char: 0 },
                key: Some(Key::One(Ident::from_plain_lit(
                    "my_table",
                    Span::from_pos_len(Pos { line: 0, char: 1 }, 8),
                ))),
                r_par: Some(Pos { line: 0, char: 9 }),
            },
            assignments: vec![tabool(1, "entry", false)],
        })],
    )
}

#[test]
fn array_header() {
    check(
        "[[my_array]]\nentry = false\n",
        [Ast::Array(ArrayEntry {
            comments: Vec::new(),
            header: ArrayHeader {
                l_pars: (Pos { line: 0, char: 0 }, Pos { line: 0, char: 1 }),
                key: Some(Key::One(Ident::from_plain_lit(
                    "my_array",
                    Span::from_pos_len(Pos { line: 0, char: 2 }, 8),
                ))),
                r_pars: (
                    Some(Pos { line: 0, char: 10 }),
                    Some(Pos { line: 0, char: 11 }),
                ),
            },
            assignments: vec![tabool(1, "entry", false)],
        })],
    )
}

#[test]
fn newline_is_required_after_table_header() {
    check_error(
        "[my_table]entry = false\n",
        [Ast::Table(Table {
            comments: Vec::new(),
            header: TableHeader {
                l_par: Pos { line: 0, char: 0 },
                key: Some(Key::One(Ident::from_plain_lit(
                    "my_table",
                    Span::from_pos_len(Pos { line: 0, char: 1 }, 8),
                ))),
                r_par: Some(Pos { line: 0, char: 9 }),
            },
            assignments: vec![abool(0, 10, "entry", false).into()],
        })],
        Error::MissingNewline(Pos { line: 0, char: 10 }),
    )
}

#[test]
fn newline_is_required_after_assignment() {
    check_error(
        "a = false b = 87",
        [
            Ast::Assignment(abool(0, 0, "a", false).into()),
            Ast::Assignment(aint(0, 10, "b", "87").into()),
        ],
        Error::MissingNewline(Pos { line: 0, char: 10 }),
    )
}

#[test]
fn table_header_with_associated_comment() {
    check(
        "# associated\n[my_table]\n",
        [Ast::Table(Table {
            comments: vec![AssociatedComment {
                pos: AssociatedPos::Above,
                comment: Comment {
                    span: Span::from_pos_len(Pos { line: 0, char: 0 }, 12),
                    text: " associated",
                },
            }],
            header: TableHeader {
                l_par: Pos { line: 1, char: 0 },
                key: Some(Key::One(Ident::from_plain_lit(
                    "my_table",
                    Span::from_pos_len(Pos { line: 1, char: 1 }, 8),
                ))),
                r_par: Some(Pos { line: 1, char: 9 }),
            },
            assignments: Vec::new(),
        })],
    )
}

#[test]
fn non_associated_comment() {
    check(
        "# free standing\n\n[my_table]\n",
        [
            Ast::Comment(Comment {
                span: Span::from_pos_len(Pos { line: 0, char: 0 }, 15),
                text: " free standing",
            }),
            Ast::Table(Table {
                comments: Vec::new(),
                header: TableHeader {
                    l_par: Pos { line: 2, char: 0 },
                    key: Some(Key::One(Ident::from_plain_lit(
                        "my_table",
                        Span::from_pos_len(Pos { line: 2, char: 1 }, 8),
                    ))),
                    r_par: Some(Pos { line: 2, char: 9 }),
                },
                assignments: Vec::new(),
            }),
        ],
    )
}

#[test]
fn comment_after_table_header() {
    check(
        "[my_table] # comment\nentry = false\n",
        [Ast::Table(Table {
            comments: vec![AssociatedComment {
                pos: AssociatedPos::SameLine,
                comment: Comment {
                    span: Span::from_pos_len(Pos { line: 0, char: 11 }, 9),
                    text: " comment",
                },
            }],
            header: TableHeader {
                l_par: Pos { line: 0, char: 0 },
                key: Some(Key::One(Ident::from_plain_lit(
                    "my_table",
                    Span::from_pos_len(Pos { line: 0, char: 1 }, 8),
                ))),
                r_par: Some(Pos { line: 0, char: 9 }),
            },
            assignments: vec![tabool(1, "entry", false)],
        })],
    )
}

#[test]
fn associated_comments_above_assignment() {
    check(
        "# comment 1\n# comment 2\nabc = false",
        [Ast::Assignment(ToplevelAssignment {
            comments: vec![
                AssociatedComment {
                    pos: AssociatedPos::Above,
                    comment: Comment {
                        span: Span::from_pos_len(Pos { line: 0, char: 0 }, 11),
                        text: " comment 1",
                    },
                },
                AssociatedComment {
                    pos: AssociatedPos::Above,
                    comment: Comment {
                        span: Span::from_pos_len(Pos { line: 1, char: 0 }, 11),
                        text: " comment 2",
                    },
                },
            ],
            assignment: abool(2, 0, "abc", false),
        })],
    )
}

#[test]
fn comment_after_assignment() {
    check(
        "abc = false # comment\n",
        [Ast::Assignment(ToplevelAssignment {
            comments: vec![AssociatedComment {
                pos: AssociatedPos::SameLine,
                comment: Comment {
                    span: Span::from_pos_len(Pos { line: 0, char: 12 }, 9),
                    text: " comment",
                },
            }],
            assignment: abool(0, 0, "abc", false),
        })],
    )
}

#[test]
fn comment_separated_by_blank_line_is_not_associated() {
    check(
        "# free standing\n\n# associated\nabc = false",
        [
            Ast::Comment(Comment {
                span: Span::from_pos_len(Pos { line: 0, char: 0 }, 15),
                text: " free standing",
            }),
            Ast::Assignment(ToplevelAssignment {
                comments: vec![AssociatedComment {
                    pos: AssociatedPos::Above,
                    comment: Comment {
                        span: Span::from_pos_len(Pos { line: 2, char: 0 }, 12),
                        text: " associated",
                    },
                }],
                assignment: abool(3, 0, "abc", false),
            }),
        ],
    )
}

#[test]
fn offset_date_time_with_subsec() {
    check(
        "abc = 2023-12-05T10:11:12.3324243-04:30",
        [Ast::Assignment(ta(
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
        ))],
    );
}

#[test]
fn offset_date_time_without_subsec() {
    check(
        "abc = 2023-12-05T10:11:12+04:30",
        [Ast::Assignment(ta(
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
        ))],
    );
}

#[test]
fn offset_date_time_with_z_suffix() {
    check(
        "abc = 2023-12-05T10:11:12Z",
        [Ast::Assignment(ta(
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
        ))],
    );
}

#[test]
fn space_separated_time() {
    check(
        "abc = 2023-12-05 10:11:12",
        [Ast::Assignment(ta(
            0,
            "abc",
            Value::DateTime(DateTimeVal {
                lit: "2023-12-05 10:11:12",
                lit_span: Span::from_pos_len(Pos { line: 0, char: 6 }, 19),
                val: DateTime::LocalDateTime(Date::new(2023, 12, 5), Time::new(10, 11, 12, 0)),
            }),
        ))],
    );
}

#[test]
fn local_date() {
    check(
        "abc = 2023-12-05",
        [Ast::Assignment(ta(
            0,
            "abc",
            Value::DateTime(DateTimeVal {
                lit: "2023-12-05",
                lit_span: Span::from_pos_len(Pos { line: 0, char: 6 }, 10),
                val: DateTime::LocalDate(Date::new(2023, 12, 5)),
            }),
        ))],
    );
}

#[test]
fn local_time() {
    check(
        "abc = 10:11:12",
        [Ast::Assignment(ta(
            0,
            "abc",
            Value::DateTime(DateTimeVal {
                lit: "10:11:12",
                lit_span: Span::from_pos_len(Pos { line: 0, char: 6 }, 8),
                val: DateTime::LocalTime(Time::new(10, 11, 12, 0)),
            }),
        ))],
    );
}

#[test]
fn local_time_hour_out_of_range() {
    check_error(
        "abc = 25:00:00",
        [Ast::Assignment(tainvalid(0, "abc", "25:00:00"))],
        Error::DateTimeOutOfBounds(
            DateTimeField::Hour,
            25,
            Span::from_pos_len(Pos { line: 0, char: 6 }, 2),
        ),
    );
}

// TODO: date time error tests
