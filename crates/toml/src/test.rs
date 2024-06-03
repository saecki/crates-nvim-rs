pub use bumpalo::collections::Vec as BVec;
pub use bumpalo::vec as bvec;
pub use bumpalo::Bump;
use common::{Pos, Span};
pub use pretty_assertions::assert_eq;

use crate::lex::TextOffset;
use crate::map::MapInner;
pub use crate::parse::{Assignment, Ident, Key, ToplevelAssignment, Value};
pub use crate::util::{self, SimpleVal};
pub use crate::{Error, Quote, TomlCtx, TomlDiagnostics, Warning};

use crate::parse::{AssocComment, BoolVal, CommentId, CommentRange, FloatVal, IntVal, StringVal};

mod fuzz;

pub fn expect_float(table: &MapInner<String, SimpleVal>, key: &str) -> f64 {
    let val = table.get(key).unwrap();
    match val {
        SimpleVal::Float(f) => *f,
        _ => unreachable!("{val:?}"),
    }
}

pub fn parse_simple(input: &str) -> (TomlDiagnostics, MapInner<String, SimpleVal>) {
    let mut ctx = TomlDiagnostics::default();
    let bump = Bump::new();
    let tokens = ctx.lex(&bump, input);
    let asts = ctx.parse(&bump, &tokens);
    let map = ctx.map(&asts);
    let table = util::map_simple(map);
    (ctx, table)
}

#[track_caller]
pub fn check_simple(input: &str, expected: MapInner<String, SimpleVal>) {
    let (ctx, table) = parse_simple(input);
    assert_eq!(
        expected, table,
        "\nerrors: {:#?}\nwarnings: {:#?}",
        ctx.errors, ctx.warnings
    );
    assert_eq!(Vec::<Error>::new(), ctx.errors);
    assert_eq!(Vec::<Warning>::new(), ctx.warnings);
}

#[track_caller]
pub fn check_simple_error(input: &str, expected: MapInner<String, SimpleVal>, error: Error) {
    let mut ctx = TomlDiagnostics::default();
    let bump = Bump::new();
    let tokens = ctx.lex(&bump, input);
    let asts = ctx.parse(&bump, &tokens);
    let map = ctx.map(&asts);

    let test_table = util::map_simple(map);
    assert_eq!(
        expected, test_table,
        "\nerrors: {:#?}\nwarnings: {:#?}",
        ctx.errors, ctx.warnings
    );
    assert_eq!(vec![error], ctx.errors);
    assert_eq!(Vec::<Warning>::new(), ctx.warnings);
}

pub fn int(line: u32, char: u32, lit: &str) -> Value<'_> {
    let val_span = Span::from_pos_len(Pos { line, char }, lit.len() as u32);
    let num = lit.replace('_', "").parse::<i64>().unwrap();
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
        key: Key::One(Ident::from_plain_lit(ident, ident_span)),
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
    let num = val.replace('_', "").parse::<f64>().unwrap();
    let val = Value::Float(FloatVal {
        lit: val,
        lit_span: val_span,
        val: num,
    });
    a(line, char, ident, val)
}

pub fn abool(line: u32, char: u32, ident: &str, val: bool) -> Assignment<'_> {
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
    // HACK: only works for strings without escape sequences
    let text = lit.trim_start_matches('\'');
    let start_offset = (lit.len() - text.len()) as u8;
    let text = text.trim_end_matches('\'');
    let end_offset = (lit.len() - text.len()) as u8 - start_offset;
    let text_offset = TextOffset::chars(start_offset, end_offset);
    let val = Value::String(StringVal {
        lit_span,
        lit,
        text,
        text_offset,
        quote,
    });
    a(line, char, ident, val)
}

pub fn twrap<'a>(
    comments: &[AssocComment],
    level: u16,
    assignment: Assignment<'a>,
) -> ToplevelAssignment<'a> {
    ToplevelAssignment {
        comments: empty_comments(comments, level),
        assignment,
    }
}

pub fn ta<'a, 'b>(
    comments: &'b [AssocComment<'b>],
    level: u16,
    line: u32,
    ident: &'a str,
    val: Value<'a>,
) -> ToplevelAssignment<'a> {
    twrap(comments, level, a(line, 0, ident, val))
}

pub fn tainvalid<'a>(
    comments: &[AssocComment],
    level: u16,
    line: u32,
    ident: &'a str,
    val: &'a str,
) -> ToplevelAssignment<'a> {
    twrap(comments, level, ainvalid(line, 0, ident, val))
}

pub fn taint<'a>(
    comments: &[AssocComment],
    level: u16,
    line: u32,
    ident: &'a str,
    val: &'a str,
) -> ToplevelAssignment<'a> {
    twrap(comments, level, aint(line, 0, ident, val))
}

pub fn tafloat<'a>(
    comments: &[AssocComment],
    level: u16,
    line: u32,
    ident: &'a str,
    val: &'a str,
) -> ToplevelAssignment<'a> {
    twrap(comments, level, afloat(line, 0, ident, val))
}

pub fn tabool<'a>(
    comments: &[AssocComment],
    level: u16,
    line: u32,
    ident: &'a str,
    val: bool,
) -> ToplevelAssignment<'a> {
    twrap(comments, level, abool(line, 0, ident, val))
}

pub fn tastring<'a>(
    comments: &[AssocComment],
    level: u16,
    line: u32,
    ident: &'a str,
    lit: &'a str,
    quote: Quote,
) -> ToplevelAssignment<'a> {
    twrap(comments, level, astring(line, 0, ident, lit, quote))
}

pub fn empty_comments(comments: &[AssocComment], level: u16) -> CommentRange {
    CommentRange::new(CommentId(comments.len() as u32), 0, level)
}

pub fn build_comments<'a, const SIZE: usize>(
    storage: &mut BVec<'a, AssocComment<'a>>,
    level: u16,
    comments: [AssocComment<'a>; SIZE],
) -> CommentRange {
    let range = CommentRange::new(
        CommentId(storage.len() as u32),
        comments.len() as u32,
        level,
    );
    storage.extend(comments);
    range
}
