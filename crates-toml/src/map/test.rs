use pretty_assertions::assert_eq;

use crate::onevec;
use crate::parse::TableHeader;
use crate::test::*;

use super::*;

impl<'a> MapTableEntry<'a> {
    fn new(node: MapNode<'a>, reprs: OneVec<MapTableEntryRepr<'a>>) -> Self {
        Self { node, reprs }
    }
}

fn check(input: &str, expected: MapTable) {
    let mut ctx = Ctx::default();
    let bump = Bump::new();
    let tokens = ctx.lex(&bump, input);
    let asts = ctx.parse(&bump, &tokens);
    let map = ctx.map(&bump, &asts);
    assert_eq!(
        expected, map,
        "\nerrors: {:#?}\nwarnings: {:#?}",
        ctx.errors, ctx.warnings
    );
    assert_eq!(Vec::<Error>::new(), ctx.errors);
    assert_eq!(Vec::<Warning>::new(), ctx.warnings);
}

fn check_error(input: &str, expected: MapTable, error: Error) {
    let mut ctx = Ctx::default();
    let bump = Bump::new();
    let tokens = ctx.lex(&bump, input);
    let asts = ctx.parse(&bump, &tokens);
    let map = ctx.map(&bump, &asts);
    assert_eq!(
        expected, map,
        "\nerrors: {:#?}\nwarnings: {:#?}",
        ctx.errors, ctx.warnings
    );
    assert_eq!(vec![error], ctx.errors);
    assert_eq!(Vec::<Warning>::new(), ctx.warnings);
}

#[test]
fn dotted_key() {
    let key = [
        DottedIdent {
            ident: Ident::from_plain_lit("a", Span::from_pos_len(Pos::new(0, 0), 1)),
            dot: Some(Pos::new(0, 1)),
        },
        DottedIdent {
            ident: Ident::from_plain_lit("b", Span::from_pos_len(Pos::new(0, 2), 1)),
            dot: Some(Pos::new(0, 3)),
        },
        DottedIdent {
            ident: Ident::from_plain_lit("c", Span::from_pos_len(Pos::new(0, 4), 1)),
            dot: None,
        },
    ];
    let value = IntVal {
        lit: "1",
        lit_span: Span::from_pos_len(Pos::new(0, 8), 1),
        val: 1,
    };
    let assignment = twrap(
        &[],
        Assignment {
            key: Key::Dotted(&key),
            eq: Pos::new(0, 6),
            val: Value::Int(value.clone()),
        },
    );

    #[rustfmt::skip]
    check(
        "a.b.c = 1",
        MapTable::from_pairs([("a", MapTableEntry::from_one(
            MapNode::Table(MapTable::from_pairs([("b", MapTableEntry::from_one(
                MapNode::Table(MapTable::from_pairs([("c", MapTableEntry::from_one(
                    MapNode::Scalar(Scalar::Int(&value)),
                    MapTableEntryRepr::new(
                        MapTableKeyRepr::Dotted(2, &key),
                        MapTableEntryReprKind::ToplevelAssignment(&assignment),
                    ),
                ))])),
                MapTableEntryRepr::new(
                    MapTableKeyRepr::Dotted(1, &key),
                    MapTableEntryReprKind::ToplevelAssignment(&assignment),
                ),
            ))])),
            MapTableEntryRepr::new(
                MapTableKeyRepr::Dotted(0, &key),
                MapTableEntryReprKind::ToplevelAssignment(&assignment),
            ),
        ))]),
    );
}

#[test]
fn dotted_keys_extend() {
    let key1 = [
        DottedIdent {
            ident: Ident::from_plain_lit("a", Span::from_pos_len(Pos::new(0, 0), 1)),
            dot: Some(Pos::new(0, 1)),
        },
        DottedIdent {
            ident: Ident::from_plain_lit("b", Span::from_pos_len(Pos::new(0, 2), 1)),
            dot: Some(Pos::new(0, 3)),
        },
        DottedIdent {
            ident: Ident::from_plain_lit("c", Span::from_pos_len(Pos::new(0, 4), 1)),
            dot: None,
        },
    ];
    let value1 = IntVal {
        lit: "1",
        lit_span: Span::from_pos_len(Pos::new(0, 8), 1),
        val: 1,
    };
    let assignment1 = twrap(
        &[],
        Assignment {
            key: Key::Dotted(&key1),
            eq: Pos::new(0, 6),
            val: Value::Int(value1.clone()),
        },
    );

    let key2 = [
        DottedIdent {
            ident: Ident::from_plain_lit("a", Span::from_pos_len(Pos::new(1, 0), 1)),
            dot: Some(Pos::new(1, 1)),
        },
        DottedIdent {
            ident: Ident::from_plain_lit("b", Span::from_pos_len(Pos::new(1, 2), 1)),
            dot: Some(Pos::new(1, 3)),
        },
        DottedIdent {
            ident: Ident::from_plain_lit("d", Span::from_pos_len(Pos::new(1, 4), 1)),
            dot: None,
        },
    ];
    let value2 = IntVal {
        lit: "2",
        lit_span: Span::from_pos_len(Pos::new(1, 8), 1),
        val: 2,
    };
    let assignment2 = twrap(
        &[],
        Assignment {
            key: Key::Dotted(&key2),
            eq: Pos::new(1, 6),
            val: Value::Int(value2.clone()),
        },
    );

    #[rustfmt::skip]
    check("\
a.b.c = 1
a.b.d = 2
",
        MapTable::from_pairs([("a", MapTableEntry::new(
            MapNode::Table(MapTable::from_pairs([("b", MapTableEntry::new(
                MapNode::Table(MapTable::from_pairs([
                    ("c", MapTableEntry::from_one(
                        MapNode::Scalar(Scalar::Int(&value1)),
                        MapTableEntryRepr::new(
                            MapTableKeyRepr::Dotted(2, &key1),
                            MapTableEntryReprKind::ToplevelAssignment(&assignment1),
                        ),
                    )),
                    ("d", MapTableEntry::from_one(
                        MapNode::Scalar(Scalar::Int(&value2)),
                        MapTableEntryRepr::new(
                            MapTableKeyRepr::Dotted(2, &key2),
                            MapTableEntryReprKind::ToplevelAssignment(&assignment2),
                        ),
                    )),
                ])),
                onevec![
                    MapTableEntryRepr::new(
                        MapTableKeyRepr::Dotted(1, &key1),
                        MapTableEntryReprKind::ToplevelAssignment(&assignment1),
                    ),
                    MapTableEntryRepr::new(
                        MapTableKeyRepr::Dotted(1, &key2),
                        MapTableEntryReprKind::ToplevelAssignment(&assignment2),
                    ),
                ],
            ))])),
            onevec![
                MapTableEntryRepr::new(
                    MapTableKeyRepr::Dotted(0, &key1),
                    MapTableEntryReprKind::ToplevelAssignment(&assignment1),
                ),
                MapTableEntryRepr::new(
                    MapTableKeyRepr::Dotted(0, &key2),
                    MapTableEntryReprKind::ToplevelAssignment(&assignment2),
                ),
            ],
        ))]),
    );
}

#[test]
fn table() {
    let table_key = Ident::from_plain_lit("mytable", Span::from_pos_len(Pos::new(0, 1), 7));
    let bump = Bump::new();

    let key1 = Ident::from_plain_lit("abc", Span::from_pos_len(Pos::new(1, 0), 3));
    let value1 = BoolVal {
        lit_span: Span::from_pos_len(Pos::new(1, 6), 4),
        val: true,
    };
    let assignment1 = ToplevelAssignment::from(twrap(
        &[],
        Assignment {
            key: Key::One(key1.clone()),
            eq: Pos::new(1, 4),
            val: Value::Bool(value1.clone()),
        },
    ));

    let key2 = Ident::from_plain_lit("def", Span::from_pos_len(Pos::new(2, 0), 3));
    let value2 = FloatVal {
        lit: "23.0",
        lit_span: Span::from_pos_len(Pos::new(2, 6), 4),
        val: 23.0,
    };
    let assignment2 = ToplevelAssignment::from(twrap(
        &[],
        Assignment {
            key: Key::One(key2.clone()),
            eq: Pos::new(2, 4),
            val: Value::Float(value2.clone()),
        },
    ));

    let table = Table {
        comments: empty_comments(&[]),
        header: TableHeader {
            l_par: Pos::new(0, 0),
            key: Some(Key::One(table_key.clone())),
            r_par: Some(Pos::new(0, 8)),
        },
        assignments: bvec![in &bump; assignment1.clone(), assignment2.clone()],
    };

    #[rustfmt::skip]
    check(
        "\
[mytable]
abc = true
def = 23.0
",
        MapTable::from_pairs([("mytable", MapTableEntry::from_one(
            MapNode::Table(MapTable::from_pairs([
                (
                    "abc",
                    MapTableEntry::from_one(
                        MapNode::Scalar(Scalar::Bool(&value1)),
                        MapTableEntryRepr::new(
                            MapTableKeyRepr::One(&key1),
                            MapTableEntryReprKind::ToplevelAssignment(&assignment1),
                        ),
                    ),
                ),
                (
                    "def",
                    MapTableEntry::from_one(
                        MapNode::Scalar(Scalar::Float(&value2)),
                        MapTableEntryRepr::new(
                            MapTableKeyRepr::One(&key2),
                            MapTableEntryReprKind::ToplevelAssignment(&assignment2),
                        ),
                    ),
                ),
            ])),
            MapTableEntryRepr::new(
                MapTableKeyRepr::One(&table_key),
                MapTableEntryReprKind::Table(&table),
            ),
        ))]),
    );
}

#[test]
fn inline_array() {
    let value1 = IntVal {
        lit: "4",
        lit_span: Span::from_pos_len(Pos::new(0, 9), 1),
        val: 4,
    };
    let inline_array_value1 = InlineArrayValue {
        comments: empty_comments(&[]),
        val: Value::Int(value1.clone()),
        comma: Some(Pos::new(0, 10)),
    };

    let value2 = IntVal {
        lit: "8",
        lit_span: Span::from_pos_len(Pos::new(0, 12), 1),
        val: 8,
    };
    let inline_array_value2 = InlineArrayValue {
        comments: empty_comments(&[]),
        val: Value::Int(value2.clone()),
        comma: Some(Pos::new(0, 13)),
    };

    let value3 = IntVal {
        lit: "16",
        lit_span: Span::from_pos_len(Pos::new(0, 15), 2),
        val: 16,
    };
    let inline_array_value3 = InlineArrayValue {
        comments: empty_comments(&[]),
        val: Value::Int(value3.clone()),
        comma: None,
    };

    let array_key = Ident::from_plain_lit("array", Span::from_pos_len(Pos::new(0, 0), 5));
    let array = InlineArray {
        comments: empty_comments(&[]),
        l_par: Pos::new(0, 8),
        values: &[
            inline_array_value1.clone(),
            inline_array_value2.clone(),
            inline_array_value3.clone(),
        ],
        r_par: Some(Pos::new(0, 17)),
    };
    let assignment = twrap(
        &[],
        Assignment {
            key: Key::One(array_key.clone()),
            eq: Pos::new(0, 6),
            val: Value::InlineArray(array.clone()),
        },
    );

    #[rustfmt::skip]
    check(
        "array = [4, 8, 16]",
        MapTable::from_pairs([("array", MapTableEntry::from_one(
            MapNode::Array(MapArray::Inline(MapArrayInline::from_iter(&array, [
                MapArrayInlineEntry::new(
                    MapNode::Scalar(Scalar::Int(&value1)),
                    &inline_array_value1,
                ),
                MapArrayInlineEntry::new(
                    MapNode::Scalar(Scalar::Int(&value2)),
                    &inline_array_value2,
                ),
                MapArrayInlineEntry::new(
                    MapNode::Scalar(Scalar::Int(&value3)),
                    &inline_array_value3,
                ),
            ]))),
            MapTableEntryRepr::new(
                MapTableKeyRepr::One(&array_key),
                MapTableEntryReprKind::ToplevelAssignment(&assignment),
            ),
        ))]),
    );
}

#[test]
fn array_of_tables() {
    check_simple(
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
        HashMap::from_iter([(
            "currencies".into(),
            SimpleVal::Array(vec![
                SimpleVal::Table(HashMap::from_iter([
                    ("name".into(), SimpleVal::String("Euro".into())),
                    ("symbol".into(), SimpleVal::String("€".into())),
                ])),
                SimpleVal::Table(HashMap::from_iter([
                    ("name".into(), SimpleVal::String("Dollar".into())),
                    ("symbol".into(), SimpleVal::String("$".into())),
                ])),
                SimpleVal::Table(HashMap::from_iter([
                    ("name".into(), SimpleVal::String("Pound".into())),
                    ("symbol".into(), SimpleVal::String("£".into())),
                ])),
            ]),
        )]),
    )
}

#[test]
fn table_cannot_extend_dotted_key_of_assignment() {
    let key = [
        DottedIdent {
            ident: Ident::from_plain_lit("fruit", Span::from_pos_len(Pos::new(0, 0), 5)),
            dot: Some(Pos::new(0, 5)),
        },
        DottedIdent {
            ident: Ident::from_plain_lit("apple", Span::from_pos_len(Pos::new(0, 6), 5)),
            dot: None,
        },
    ];
    let value = IntVal {
        lit: "3",
        lit_span: Span::from_pos_len(Pos::new(0, 14), 1),
        val: 3,
    };
    let assignment = twrap(
        &[],
        Assignment {
            key: Key::Dotted(&key),
            eq: Pos::new(0, 12),
            val: Value::Int(value.clone()),
        },
    );
    check_error(
        "\
fruit.apple = 3
[fruit]
    ",
        MapTable::from_pairs([(
            "fruit",
            MapTableEntry::from_one(
                MapNode::Table(MapTable::from_pairs([(
                    "apple",
                    MapTableEntry::from_one(
                        MapNode::Scalar(Scalar::Int(&value)),
                        MapTableEntryRepr::new(
                            MapTableKeyRepr::Dotted(1, &key),
                            MapTableEntryReprKind::ToplevelAssignment(&assignment),
                        ),
                    ),
                )])),
                MapTableEntryRepr::new(
                    MapTableKeyRepr::Dotted(0, &key),
                    MapTableEntryReprKind::ToplevelAssignment(&assignment),
                ),
            ),
        )]),
        Error::DuplicateKey {
            path: None,
            key: "fruit".into(),
            orig: Span::from_pos_len(Pos::new(0, 0), 5),
            duplicate: Span::from_pos_len(Pos::new(1, 1), 5),
        },
    );
}

#[test]
fn table_can_share_part_of_assignments_dotted_key() {
    check_simple(
        "\
fruit.berries.strawberry.num = 3

[fruit.berries.raspberry]
num = 8383
    ",
        HashMap::from_iter([(
            "fruit".into(),
            SimpleVal::Table(HashMap::from_iter([(
                "berries".into(),
                SimpleVal::Table(HashMap::from_iter([
                    (
                        "strawberry".into(),
                        SimpleVal::Table(HashMap::from_iter([("num".into(), SimpleVal::Int(3))])),
                    ),
                    (
                        "raspberry".into(),
                        SimpleVal::Table(HashMap::from_iter([(
                            "num".into(),
                            SimpleVal::Int(8383),
                        )])),
                    ),
                ])),
            )])),
        )]),
    );
}

#[test]
fn table_extends_other_table() {
    check_simple(
        "\
[a]
1 = false

[a.b]
2 = true
    ",
        HashMap::from_iter([(
            "a".into(),
            SimpleVal::Table(HashMap::from_iter([
                ("1".into(), SimpleVal::Bool(false)),
                (
                    "b".into(),
                    SimpleVal::Table(HashMap::from_iter([("2".into(), SimpleVal::Bool(true))])),
                ),
            ])),
        )]),
    );
}

#[test]
fn super_table_declared_afterwards() {
    check_simple(
        "\
[a.b]
2 = true

[a]
1 = false
    ",
        HashMap::from_iter([(
            "a".into(),
            SimpleVal::Table(HashMap::from_iter([
                ("1".into(), SimpleVal::Bool(false)),
                (
                    "b".into(),
                    SimpleVal::Table(HashMap::from_iter([("2".into(), SimpleVal::Bool(true))])),
                ),
            ])),
        )]),
    );
}

#[test]
fn table_extends_last_array_entry() {
    check_simple(
        "\
[[a.b]]
1 = 'one'

[a.b.c]
2 = 'two'

[[a.b]]
1 = 'three'

[a.b.c]
2 = 'four'
    ",
        HashMap::from_iter([(
            "a".into(),
            SimpleVal::Table(HashMap::from_iter([(
                "b".into(),
                SimpleVal::Array(vec![
                    SimpleVal::Table(HashMap::from_iter([
                        ("1".into(), SimpleVal::String("one".into())),
                        (
                            "c".into(),
                            SimpleVal::Table(HashMap::from_iter([(
                                "2".into(),
                                SimpleVal::String("two".into()),
                            )])),
                        ),
                    ])),
                    SimpleVal::Table(HashMap::from_iter([
                        ("1".into(), SimpleVal::String("three".into())),
                        (
                            "c".into(),
                            SimpleVal::Table(HashMap::from_iter([(
                                "2".into(),
                                SimpleVal::String("four".into()),
                            )])),
                        ),
                    ])),
                ]),
            )])),
        )]),
    );
}

#[test]
fn array_of_table_of_arrays() {
    check_simple(
        "\
[[a.b]]
1 = false

[[a.b]]
1 = true

[[a.b.c]]
2 = false
    ",
        HashMap::from_iter([(
            "a".into(),
            SimpleVal::Table(HashMap::from_iter([(
                "b".into(),
                SimpleVal::Array(vec![
                    SimpleVal::Table(HashMap::from_iter([("1".into(), SimpleVal::Bool(false))])),
                    SimpleVal::Table(HashMap::from_iter([
                        ("1".into(), SimpleVal::Bool(true)),
                        (
                            "c".into(),
                            SimpleVal::Array(vec![SimpleVal::Table(HashMap::from_iter([(
                                "2".into(),
                                SimpleVal::Bool(false),
                            )]))]),
                        ),
                    ])),
                ]),
            )])),
        )]),
    );
}
