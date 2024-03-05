pub use bumpalo::collections::Vec as BVec;
pub use bumpalo::vec as bvec;
pub use bumpalo::Bump;
pub use pretty_assertions::assert_eq;

pub use std::collections::HashMap;

pub use crate::map::simple::SimpleVal;
pub use crate::parse::{Assignment, Ident, IdentKind, Key, ToplevelAssignment, Value};
pub use crate::{Ctx, Error, Pos, Quote, Span, Warning};

use crate::parse::{AssociatedComment, BoolVal, CommentId, Comments, FloatVal, IntVal, StringVal};

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

pub fn int<'a>(line: u32, char: u32, lit: &'a str) -> Value<'a> {
    let val_span = Span::from_pos_len(Pos { line, char }, lit.len() as u32);
    let num = lit.replace("_", "").parse::<i64>().unwrap();
    Value::Int(IntVal {
        lit,
        lit_span: val_span,
        val: num,
    })
}

pub fn bool<'a>(line: u32, char: u32, val: bool) -> Value<'a> {
    let val_span = Span::from_pos_len(Pos { line, char }, if val { 4 } else { 5 });
    Value::Bool(BoolVal {
        lit_span: val_span,
        val,
    })
}

pub fn a<'a>(line: u32, char: u32, ident: &'a str, val: Value<'a>) -> Assignment<'a> {
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

pub fn ainvalid<'a>(line: u32, char: u32, ident: &'a str, val: &'a str) -> Assignment<'a> {
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

pub fn aint<'a>(line: u32, char: u32, ident: &'a str, lit: &'a str) -> Assignment<'a> {
    let val = int(line, char + ident.len() as u32 + 3, lit);
    a(line, char, ident, val)
}

pub fn afloat<'a>(line: u32, char: u32, ident: &'a str, val: &'a str) -> Assignment<'a> {
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

pub fn abool<'a>(line: u32, char: u32, ident: &'a str, val: bool) -> Assignment<'a> {
    let val = bool(line, char + ident.len() as u32 + 3, val);
    a(line, char, ident, val)
}

pub fn astring<'a>(
    line: u32,
    char: u32,
    ident: &'a str,
    lit: &'a str,
    quote: Quote,
) -> Assignment<'a> {
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

pub fn wrap<'a>(
    comments: &[AssociatedComment],
    assignment: Assignment<'a>,
) -> ToplevelAssignment<'a> {
    ToplevelAssignment {
        comments: empty_comments(comments),
        assignment,
    }
}

pub fn ta<'a>(
    comments: &[AssociatedComment],
    line: u32,
    ident: &'a str,
    val: Value<'a>,
) -> ToplevelAssignment<'a> {
    wrap(comments, a(line, 0, ident, val))
}

pub fn tainvalid<'a>(
    comments: &[AssociatedComment],
    line: u32,
    ident: &'a str,
    val: &'a str,
) -> ToplevelAssignment<'a> {
    wrap(comments, ainvalid(line, 0, ident, val))
}

pub fn taint<'a>(
    comments: &[AssociatedComment],
    line: u32,
    ident: &'a str,
    val: &'a str,
) -> ToplevelAssignment<'a> {
    wrap(comments, aint(line, 0, ident, val))
}

pub fn tafloat<'a>(
    comments: &[AssociatedComment],
    line: u32,
    ident: &'a str,
    val: &'a str,
) -> ToplevelAssignment<'a> {
    wrap(comments, afloat(line, 0, ident, val))
}

pub fn tabool<'a>(
    comments: &[AssociatedComment],
    line: u32,
    ident: &'a str,
    val: bool,
) -> ToplevelAssignment<'a> {
    wrap(comments, abool(line, 0, ident, val))
}

pub fn tastring<'a>(
    comments: &[AssociatedComment],
    line: u32,
    ident: &'a str,
    lit: &'a str,
    quote: Quote,
) -> ToplevelAssignment<'a> {
    wrap(comments, astring(line, 0, ident, lit, quote))
}

pub fn empty_comments(comments: &[AssociatedComment]) -> Comments {
    Comments::new(CommentId(comments.len() as u32), 0)
}

pub fn build_comments<'a, const SIZE: usize>(
    storage: &mut BVec<'a, AssociatedComment<'a>>,
    comments: [AssociatedComment<'a>; SIZE],
) -> Comments {
    let range = Comments::new(CommentId(storage.len() as u32), comments.len() as u32);
    storage.extend(comments);
    range
}
