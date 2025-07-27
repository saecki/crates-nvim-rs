use common::Pos;
use pretty_assertions::assert_eq;

use crate::onevec::onevec;
use crate::parse::{End, TableHeader};
use crate::test::*;
use crate::util::SimpleMap;

use super::*;

#[track_caller]
fn check(input: &str, expected: MapTable) {
    let mut ctx = TomlDiagnostics::default();
    let bump = Bump::new();
    let tokens = ctx.lex(&bump, input);
    let ast = ctx.parse(&bump, tokens);
    let map = ctx.map(&ast);
    assert_eq!(
        expected, map,
        "\nerrors: {:#?}\nwarnings: {:#?}",
        ctx.errors, ctx.warnings
    );
    assert_eq!(Vec::<Error>::new(), ctx.errors);
    assert_eq!(Vec::<Warning>::new(), ctx.warnings);
}

#[track_caller]
fn check_error(input: &str, expected: MapTable, error: Error) {
    let mut ctx = TomlDiagnostics::default();
    let bump = Bump::new();
    let tokens = ctx.lex(&bump, input);
    let ast = ctx.parse(&bump, tokens);
    let map = ctx.map(&ast);
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
    let input = "a.b.c = 1";
    let bump = Bump::new();
    let builder = AstBuilder::new(&bump);

    let key = vec![
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
        lit_span: Span::from_pos_len(Pos::new(0, 8), 1),
        val: 1,
    };
    let assignment = twrap(
        builder.empty_comments(1),
        Assignment {
            key: Key::Dotted(&key),
            eq: Pos::new(0, 6),
            val: Value::Int(value.clone()),
        },
    );

    #[rustfmt::skip]
    check(
        input,
        MapTable::from_pairs(
            [("a", MapTableEntry::from_one(
                MapNode::Table(MapTable::from_pairs(
                    [("b", MapTableEntry::from_one(
                        MapNode::Table(MapTable::from_pairs(
                            [("c", MapTableEntry::from_one(
                                MapNode::Scalar(Scalar::Int(&value)),
                                MapTableEntryRepr::new(
                                    ParentId(0),
                                    MapTableKeyRepr::Dotted(2, &key),
                                    MapTableEntryReprKind::ToplevelAssignment(&assignment),
                                ),
                            ))],
                            OneVec::new(MapTableRepr::ToplevelAssignment(&assignment)),
                        )),
                        MapTableEntryRepr::new(
                            ParentId(0),
                            MapTableKeyRepr::Dotted(1, &key),
                            MapTableEntryReprKind::ToplevelAssignment(&assignment),
                        ),
                    ))],
                    OneVec::new(MapTableRepr::ToplevelAssignment(&assignment)),
                )),
                MapTableEntryRepr::new(
                    ROOT_PARENT,
                    MapTableKeyRepr::Dotted(0, &key),
                    MapTableEntryReprKind::ToplevelAssignment(&assignment),
                ),
            ))],
            OneVec::new(MapTableRepr::Root(Span::new(Pos::ZERO, Pos::new(0, 9)))),
        ),
    );
}

#[test]
fn dotted_keys_extend() {
    let input = "\
a.b.c = 1
a.b.d = 2
";
    let bump = Bump::new();
    let builder = AstBuilder::new(&bump);

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
        lit_span: Span::from_pos_len(Pos::new(0, 8), 1),
        val: 1,
    };
    let assignment1 = twrap(
        builder.empty_comments(1),
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
        lit_span: Span::from_pos_len(Pos::new(1, 8), 1),
        val: 2,
    };
    let assignment2 = twrap(
        builder.empty_comments(1),
        Assignment {
            key: Key::Dotted(&key2),
            eq: Pos::new(1, 6),
            val: Value::Int(value2.clone()),
        },
    );

    #[rustfmt::skip]
    check(input,
        MapTable::from_pairs(
            [("a", MapTableEntry::new(
                MapNode::Table(MapTable::from_pairs(
                    [("b", MapTableEntry::new(
                        MapNode::Table(MapTable::from_pairs(
                            [
                                ("c", MapTableEntry::from_one(
                                    MapNode::Scalar(Scalar::Int(&value1)),
                                    MapTableEntryRepr::new(
                                        ParentId(0),
                                        MapTableKeyRepr::Dotted(2, &key1),
                                        MapTableEntryReprKind::ToplevelAssignment(&assignment1),
                                    ),
                                )),
                                ("d", MapTableEntry::from_one(
                                    MapNode::Scalar(Scalar::Int(&value2)),
                                    MapTableEntryRepr::new(
                                        ParentId(1),
                                        MapTableKeyRepr::Dotted(2, &key2),
                                        MapTableEntryReprKind::ToplevelAssignment(&assignment2),
                                    ),
                                )),
                            ],
                            onevec![
                                MapTableRepr::ToplevelAssignment(&assignment1),
                                MapTableRepr::ToplevelAssignment(&assignment2),
                            ],
                        )),
                        onevec![
                            MapTableEntryRepr::new(
                                ParentId(0),
                                MapTableKeyRepr::Dotted(1, &key1),
                                MapTableEntryReprKind::ToplevelAssignment(&assignment1),
                            ),
                            MapTableEntryRepr::new(
                                ParentId(1),
                                MapTableKeyRepr::Dotted(1, &key2),
                                MapTableEntryReprKind::ToplevelAssignment(&assignment2),
                            ),
                        ],
                    ))],
                    onevec![
                        MapTableRepr::ToplevelAssignment(&assignment1),
                        MapTableRepr::ToplevelAssignment(&assignment2),
                    ],
                )),
                onevec![
                    MapTableEntryRepr::new(
                        ROOT_PARENT,
                        MapTableKeyRepr::Dotted(0, &key1),
                        MapTableEntryReprKind::ToplevelAssignment(&assignment1),
                    ),
                    MapTableEntryRepr::new(
                        ROOT_PARENT,
                        MapTableKeyRepr::Dotted(0, &key2),
                        MapTableEntryReprKind::ToplevelAssignment(&assignment2),
                    ),
                ],
            ))],
            OneVec::new(MapTableRepr::Root(Span::new(Pos::ZERO, Pos::new(1, 9))))
        ),
    );
}

#[test]
fn table() {
    let input = "\
[mytable]
abc = true
def = 23.0
";

    let table_key = Ident::from_plain_lit("mytable", Span::from_pos_len(Pos::new(0, 1), 7));
    let bump = Bump::new();
    let builder = AstBuilder::new(&bump);

    let key1 = Ident::from_plain_lit("abc", Span::from_pos_len(Pos::new(1, 0), 3));
    let value1 = BoolVal {
        lit_span: Span::from_pos_len(Pos::new(1, 6), 4),
        val: true,
    };
    let assignment1 = twrap(
        builder.empty_comments(2),
        Assignment {
            key: Key::One(key1.clone()),
            eq: Pos::new(1, 4),
            val: Value::Bool(value1.clone()),
        },
    );

    let key2 = Ident::from_plain_lit("def", Span::from_pos_len(Pos::new(2, 0), 3));
    let value2 = FloatVal {
        lit_span: Span::from_pos_len(Pos::new(2, 6), 4),
        val: 23.0,
    };
    let assignment2 = twrap(
        builder.empty_comments(2),
        Assignment {
            key: Key::One(key2.clone()),
            eq: Pos::new(2, 4),
            val: Value::Float(value2.clone()),
        },
    );

    let table = Table {
        comments: builder.empty_comments(1),
        header: TableHeader::new(
            Pos::new(0, 0),
            Some(Key::One(table_key.clone())),
            Some(Pos::new(0, 8)),
        ),
        assignments: vec![assignment1.clone(), assignment2.clone()],
    };

    #[rustfmt::skip]
    check(
        input,
        MapTable::from_pairs(
            [("mytable", MapTableEntry::from_one(
                MapNode::Table(MapTable::from_pairs(
                    [
                        (
                            "abc",
                            MapTableEntry::from_one(
                                MapNode::Scalar(Scalar::Bool(&value1)),
                                MapTableEntryRepr::new(
                                ParentId(0),
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
                                    ParentId(0),
                                    MapTableKeyRepr::One(&key2),
                                    MapTableEntryReprKind::ToplevelAssignment(&assignment2),
                                ),
                            ),
                        ),
                    ],
                    OneVec::new(MapTableRepr::Table(&table)),
                )),
                MapTableEntryRepr::new(
                    ROOT_PARENT,
                    MapTableKeyRepr::One(&table_key),
                    MapTableEntryReprKind::Table(&table),
                ),
            ))],
            OneVec::new(MapTableRepr::Root(Span::new(Pos::ZERO, Pos::new(2, 10)))),
        ),
    );
}

#[test]
fn inline_array() {
    let input = "array = [4, 8, 16]";
    let bump = Bump::new();
    let builder = AstBuilder::new(&bump);

    let value1 = IntVal {
        lit_span: Span::from_pos_len(Pos::new(0, 9), 1),
        val: 4,
    };
    let inline_array_value1 = InlineArrayValue {
        comments: builder.empty_comments(3),
        val: Value::Int(value1.clone()),
        comma: Some(Pos::new(0, 10)),
    };

    let value2 = IntVal {
        lit_span: Span::from_pos_len(Pos::new(0, 12), 1),
        val: 8,
    };
    let inline_array_value2 = InlineArrayValue {
        comments: builder.empty_comments(3),
        val: Value::Int(value2.clone()),
        comma: Some(Pos::new(0, 13)),
    };

    let value3 = IntVal {
        lit_span: Span::from_pos_len(Pos::new(0, 15), 2),
        val: 16,
    };
    let inline_array_value3 = InlineArrayValue {
        comments: builder.empty_comments(3),
        val: Value::Int(value3.clone()),
        comma: None,
    };

    let array_key = Ident::from_plain_lit("array", Span::from_pos_len(Pos::new(0, 0), 5));
    let array = InlineArray {
        comments: builder.empty_comments(2),
        l_par: Pos::new(0, 8),
        values: vec![
            inline_array_value1.clone(),
            inline_array_value2.clone(),
            inline_array_value3.clone(),
        ],
        end: End::Par(Pos::new(0, 17)),
    };
    let assignment = twrap(
        builder.empty_comments(1),
        Assignment {
            key: Key::One(array_key.clone()),
            eq: Pos::new(0, 6),
            val: Value::InlineArray(array.clone()),
        },
    );

    #[rustfmt::skip]
    check(
        input,
        MapTable::from_pairs(
            [("array", MapTableEntry::from_one(
                MapNode::Array(MapArray::Inline(MapArrayInline::from_iter(ParentId(0), &array, [
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
                    ROOT_PARENT,
                    MapTableKeyRepr::One(&array_key),
                    MapTableEntryReprKind::ToplevelAssignment(&assignment),
                ),
            ))],
            OneVec::new(MapTableRepr::Root(Span::new(Pos::ZERO, Pos::new(0, 18))))
        ),
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
        SimpleMap::from_iter([(
            "currencies".into(),
            SimpleVal::Array(vec![
                SimpleVal::Table(SimpleMap::from_iter([
                    ("name".into(), SimpleVal::String("Euro".into())),
                    ("symbol".into(), SimpleVal::String("€".into())),
                ])),
                SimpleVal::Table(SimpleMap::from_iter([
                    ("name".into(), SimpleVal::String("Dollar".into())),
                    ("symbol".into(), SimpleVal::String("$".into())),
                ])),
                SimpleVal::Table(SimpleMap::from_iter([
                    ("name".into(), SimpleVal::String("Pound".into())),
                    ("symbol".into(), SimpleVal::String("£".into())),
                ])),
            ]),
        )]),
    )
}

#[test]
fn table_cannot_extend_dotted_key_of_assignment() {
    let input = "\
fruit.apple = 3
[fruit]
";
    let bump = Bump::new();
    let builder = AstBuilder::new(&bump);

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
        lit_span: Span::from_pos_len(Pos::new(0, 14), 1),
        val: 3,
    };
    let assignment = twrap(
        builder.empty_comments(1),
        Assignment {
            key: Key::Dotted(&key),
            eq: Pos::new(0, 12),
            val: Value::Int(value.clone()),
        },
    );
    check_error(
        input,
        MapTable::from_pairs(
            [(
                "fruit",
                MapTableEntry::from_one(
                    MapNode::Table(MapTable::from_pairs(
                        [(
                            "apple",
                            MapTableEntry::from_one(
                                MapNode::Scalar(Scalar::Int(&value)),
                                MapTableEntryRepr::new(
                                    ParentId(0),
                                    MapTableKeyRepr::Dotted(1, &key),
                                    MapTableEntryReprKind::ToplevelAssignment(&assignment),
                                ),
                            ),
                        )],
                        OneVec::new(MapTableRepr::ToplevelAssignment(&assignment)),
                    )),
                    MapTableEntryRepr::new(
                        ROOT_PARENT,
                        MapTableKeyRepr::Dotted(0, &key),
                        MapTableEntryReprKind::ToplevelAssignment(&assignment),
                    ),
                ),
            )],
            OneVec::new(MapTableRepr::Root(Span::new(Pos::ZERO, Pos::new(1, 7)))),
        ),
        Error::DuplicateKey {
            lines: Box::new([]),
            path: "fruit".into(),
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
        SimpleMap::from_iter([(
            "fruit".into(),
            SimpleVal::Table(SimpleMap::from_iter([(
                "berries".into(),
                SimpleVal::Table(SimpleMap::from_iter([
                    (
                        "strawberry".into(),
                        SimpleVal::Table(SimpleMap::from_iter([("num".into(), SimpleVal::Int(3))])),
                    ),
                    (
                        "raspberry".into(),
                        SimpleVal::Table(SimpleMap::from_iter([(
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
        SimpleMap::from_iter([(
            "a".into(),
            SimpleVal::Table(SimpleMap::from_iter([
                ("1".into(), SimpleVal::Bool(false)),
                (
                    "b".into(),
                    SimpleVal::Table(SimpleMap::from_iter([("2".into(), SimpleVal::Bool(true))])),
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
        SimpleMap::from_iter([(
            "a".into(),
            SimpleVal::Table(SimpleMap::from_iter([
                ("1".into(), SimpleVal::Bool(false)),
                (
                    "b".into(),
                    SimpleVal::Table(SimpleMap::from_iter([("2".into(), SimpleVal::Bool(true))])),
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
        SimpleMap::from_iter([(
            "a".into(),
            SimpleVal::Table(SimpleMap::from_iter([(
                "b".into(),
                SimpleVal::Array(vec![
                    SimpleVal::Table(SimpleMap::from_iter([
                        ("1".into(), SimpleVal::String("one".into())),
                        (
                            "c".into(),
                            SimpleVal::Table(SimpleMap::from_iter([(
                                "2".into(),
                                SimpleVal::String("two".into()),
                            )])),
                        ),
                    ])),
                    SimpleVal::Table(SimpleMap::from_iter([
                        ("1".into(), SimpleVal::String("three".into())),
                        (
                            "c".into(),
                            SimpleVal::Table(SimpleMap::from_iter([(
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
        SimpleMap::from_iter([(
            "a".into(),
            SimpleVal::Table(SimpleMap::from_iter([(
                "b".into(),
                SimpleVal::Array(vec![
                    SimpleVal::Table(SimpleMap::from_iter([("1".into(), SimpleVal::Bool(false))])),
                    SimpleVal::Table(SimpleMap::from_iter([
                        ("1".into(), SimpleVal::Bool(true)),
                        (
                            "c".into(),
                            SimpleVal::Array(vec![SimpleVal::Table(SimpleMap::from_iter([(
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

#[test]
fn dotted_keys_in_inline_table() {
    check_simple(
        "a = { b.c.d = 1, b.c.e = 2 }",
        SimpleMap::from_iter([(
            "a".into(),
            SimpleVal::Table(SimpleMap::from_iter([(
                "b".into(),
                SimpleVal::Table(SimpleMap::from_iter([(
                    "c".into(),
                    SimpleVal::Table(SimpleMap::from_iter([
                        ("d".into(), SimpleVal::Int(1)),
                        ("e".into(), SimpleVal::Int(2)),
                    ])),
                )])),
            )])),
        )]),
    );
}

#[test]
fn toml_test_repro_open_parent_table() {
    check_simple(
        "\
[[parent-table.arr]]
[[parent-table.arr]]
[parent-table]
not-arr = 1
",
        SimpleMap::from_iter([(
            "parent-table".into(),
            SimpleVal::Table(SimpleMap::from_iter([
                (
                    "arr".into(),
                    SimpleVal::Array(vec![
                        SimpleVal::Table(SimpleMap::new()),
                        SimpleVal::Table(SimpleMap::new()),
                    ]),
                ),
                ("not-arr".into(), SimpleVal::Int(1)),
            ])),
        )]),
    );
}

#[test]
fn toml_test_repro_append_to_array_with_dotted_keys() {
    check_simple_error(
        "\
[[a.b]]

[a]
b.y = 2
",
        SimpleMap::from_iter([(
            "a".into(),
            SimpleVal::Table(SimpleMap::from_iter([(
                "b".into(),
                SimpleVal::Array(vec![SimpleVal::Table(SimpleMap::new())]),
            )])),
        )]),
        Error::CannotExtendArrayWithDottedKey {
            lines: Box::new([0, 2]),
            path: "a.b".into(),
            orig: Span::from_pos_len(Pos { line: 0, char: 0 }, 7),
            new: Span::from_pos_len(Pos { line: 3, char: 0 }, 1),
        },
    );
}

#[test]
fn toml_test_repro_append_with_dotted_keys_1() {
    check_simple_error(
        "\
[a.b.c]
  z = 9

[a]
  b.c.t = \"Using dotted keys to add to [a.b.c] after explicitly defining it above is not allowed\"
",
        SimpleMap::from_iter([(
            "a".into(),
            SimpleVal::Table(SimpleMap::from_iter([(
                "b".into(),
                SimpleVal::Table(SimpleMap::from_iter([(
                    "c".into(),
                    SimpleVal::Table(SimpleMap::from_iter([("z".into(), SimpleVal::Int(9))])),
                )])),
            )])),
        )]),
        Error::CannotExtendTableWithDottedKey {
            lines: Box::new([0, 3]),
            path: "a.b".into(),
            orig: Span::new(Pos { line: 0, char: 0 }, Pos { line: 1, char: 7 }),
            new: Span::from_pos_len(Pos { line: 4, char: 2 }, 1),
        },
    );
}
