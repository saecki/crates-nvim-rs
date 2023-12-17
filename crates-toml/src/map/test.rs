use pretty_assertions::assert_eq;

use crate::{Ctx, Error, MapTable, Warning};

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

fn check_error(input: &str, expected: MapTable, error: Error) {
    let mut ctx = Ctx::default();
    let tokens = ctx.lex(input);
    let asts = ctx.parse(tokens);
    let map = ctx.map(&asts);
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
    check("a.b.c = 1", todo!())
}

#[test]
fn dotted_keys_extend() {
    check(
        "\
a.b.c = 1
a.b.d = 2
",
        todo!(),
    )
}

#[test]
fn table() {
    check(
        "\
[mytable]
abc = true
def = 23.0
",
        todo!(),
    )
}

#[test]
fn inline_array() {
    check("array = [1, 2, 4, 8, 16]", todo!())
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
        todo!(),
    )
}

#[test]
fn table_cannot_extend_dotted_key() {
    check(
        "\
fruit.apple = 3
[fruit]
    ",
        todo!("duplicate key"),
    );
}

#[test]
fn table_extends_other_table() {
    check(
        "\
[a]
1 = false

[a.b]
2 = true
    ",
        todo!(),
    );
}
