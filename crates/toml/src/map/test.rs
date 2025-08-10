use common::Pos;

use crate::test::*;
use crate::util::{SimpleMap, SimpleVal};

use super::*;

#[test]
fn dotted_key() {
    check_simple(
        "a.b.c = 1",
        SimpleMap::from_iter([(
            "a".into(),
            SimpleVal::Table(SimpleMap::from_iter([(
                "b".into(),
                SimpleVal::Table(SimpleMap::from_iter([("c".into(), SimpleVal::Int(1))])),
            )])),
        )]),
    );
}

#[test]
fn dotted_keys_extend() {
    check_simple(
        "\
a.b.c = 1
a.b.d = 2
",
        SimpleMap::from_iter([(
            "a".into(),
            SimpleVal::Table(SimpleMap::from_iter([(
                "b".into(),
                SimpleVal::Table(SimpleMap::from_iter([
                    ("c".into(), SimpleVal::Int(1)),
                    ("d".into(), SimpleVal::Int(2)),
                ])),
            )])),
        )]),
    );
}

#[test]
fn table() {
    check_simple(
        "\
[mytable]
abc = true
def = 23.0
",
        SimpleMap::from_iter([(
            "mytable".into(),
            SimpleVal::Table(SimpleMap::from_iter([
                ("abc".into(), SimpleVal::Bool(true)),
                ("def".into(), SimpleVal::Float(23.0)),
            ])),
        )]),
    );
}

#[test]
fn inline_array() {
    check_simple(
        "array = [4, 8, 16]",
        SimpleMap::from_iter([(
            "array".into(),
            SimpleVal::Array(vec![
                SimpleVal::Int(4),
                SimpleVal::Int(8),
                SimpleVal::Int(16),
            ]),
        )]),
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
    );
}

#[test]
fn table_cannot_extend_dotted_key_of_assignment() {
    check_simple_error(
        "\
fruit.apple = 3
[fruit]
",
        SimpleMap::from_iter([(
            "fruit".into(),
            SimpleVal::Table(SimpleMap::from_iter([("apple".into(), SimpleVal::Int(3))])),
        )]),
        Error::DuplicateKey {
            lines: Box::new([]),
            path: "fruit".into(),
            orig: Span::from_pos_len(Pos::new(0, 0), 5),
            new: Span::from_pos_len(Pos::new(1, 1), 5),
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
            orig: Span::new(Pos { line: 0, char: 0 }, Pos { line: 0, char: 7 }),
            new: Span::from_pos_len(Pos { line: 4, char: 2 }, 1),
        },
    );
}
