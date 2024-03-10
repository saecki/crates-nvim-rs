use bumpalo::Bump;
use pretty_assertions::assert_eq;

use std::collections::HashMap;

use crate::map::simple::SimpleVal;
use crate::{Ctx, Error, Warning};

pub fn check_simple(input: &str, expected: HashMap<String, SimpleVal>) {
    let mut ctx = Ctx::default();
    let bump = Bump::new();
    let tokens = ctx.lex(&bump, input);
    let asts = ctx.parse(&bump, &tokens);
    let map = ctx.map(&bump, &asts);

    let test_table = crate::map::simple::map_table(map);
    assert_eq!(
        expected, test_table,
        "\nerrors: {:#?}\nwarnings: {:#?}",
        ctx.errors, ctx.warnings
    );
    assert_eq!(Vec::<Error>::new(), ctx.errors);
    assert_eq!(Vec::<Warning>::new(), ctx.warnings);
}

pub fn check_simple_error(input: &str, expected: HashMap<String, SimpleVal>, error: Error) {
    let mut ctx = Ctx::default();
    let bump = Bump::new();
    let tokens = ctx.lex(&bump, input);
    let asts = ctx.parse(&bump, &tokens);
    let map = ctx.map(&bump, &asts);

    let test_table = crate::map::simple::map_table(map);
    assert_eq!(
        expected, test_table,
        "\nerrors: {:#?}\nwarnings: {:#?}",
        ctx.errors, ctx.warnings
    );
    assert_eq!(vec![error], ctx.errors);
    assert_eq!(Vec::<Warning>::new(), ctx.warnings);
}
