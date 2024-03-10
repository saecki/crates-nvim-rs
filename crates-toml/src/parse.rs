use std::num::NonZeroU32;

use bumpalo::collections::Vec as BVec;
use bumpalo::Bump;

use crate::datetime::{Date, DateTime, Time};
use crate::error::{FmtChar, FmtStr};
use crate::lex::{LiteralId, StringId, StringToken, Token, TokenType, Tokens};
use crate::{Ctx, Error, Pos, Quote, Span};

pub use num::{IntPrefix, Sign};

mod datetime;
mod num;
#[cfg(test)]
mod test;

macro_rules! recover_on {
    ($parser:expr, $($tokens:pat => $recover:stmt),+ $(,)?) => {{
        loop {
            use TokenType::*;
            match $parser.peek().ty {
                $($tokens => {
                    #[allow(redundant_semicolons)]
                    $recover;
                })+
                _ => {
                    $parser.next();
                }
            }
        }
    }};
}

#[derive(Debug, PartialEq)]
pub struct Asts<'a> {
    pub asts: &'a [Ast<'a>],
    pub comments: &'a [AssociatedComment<'a>],
}

#[derive(Debug, PartialEq)]
pub enum Ast<'a> {
    Assignment(ToplevelAssignment<'a>),
    Table(Table<'a>),
    Array(ArrayEntry<'a>),
    Comment(Comment<'a>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CommentId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Comments {
    start: CommentId,
    len: u32,
}

impl Comments {
    pub const fn new(start: CommentId, len: u32) -> Self {
        Self { start, len }
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Append all comments including this one to the range.
    fn append(&mut self, id: CommentId) {
        self.len = id.0 - self.start.0 + 1;
    }

    /// Set the end bound of this comment range. If this happens to be the same as start, this
    /// range remains empty.
    fn extend_to(&mut self, id: CommentId) {
        self.len = id.0 - self.start.0;
    }
}

// TODO: add level to distiguish nested comments in values from comments that belong to the parent.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AssociatedComment<'a> {
    pub pos: AssociatedPos,
    pub comment: Comment<'a>,
}

impl<'a> AssociatedComment<'a> {
    pub fn above(comment: Comment<'a>) -> AssociatedComment<'a> {
        AssociatedComment {
            pos: AssociatedPos::Above,
            comment,
        }
    }

    pub fn line_end(comment: Comment<'a>) -> AssociatedComment<'a> {
        AssociatedComment {
            pos: AssociatedPos::LineEnd,
            comment,
        }
    }

    pub fn contained(comment: Comment<'a>) -> AssociatedComment<'a> {
        AssociatedComment {
            pos: AssociatedPos::Contained,
            comment,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AssociatedPos {
    /// Directly above the associated item, without any blank lines
    Above,
    /// At the end of the same line as the associated item.
    LineEnd,
    /// Contained inside the item.
    Contained,
}

#[derive(Debug, PartialEq)]
pub struct Table<'a> {
    pub comments: Comments,
    pub header: TableHeader<'a>,
    pub assignments: BVec<'a, ToplevelAssignment<'a>>,
}

impl<'a> Table<'a> {
    #[inline]
    pub fn span(&self) -> Span {
        let header_span = self.header.span();
        let start = header_span.start;
        let end = (self.assignments.last())
            .map(|a| a.assignment.val.span().end)
            .unwrap_or(header_span.end);
        Span { start, end }
    }

    pub fn append_comment(&mut self, id: CommentId) {
        match self.assignments.last_mut() {
            Some(a) => a.comments.append(id),
            None => self.comments.append(id),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct TableHeader<'a> {
    pub l_par: Pos,
    pub key: Option<Key<'a>>,
    pub r_par_offset: Option<NonZeroU32>,
}

impl<'a> TableHeader<'a> {
    pub fn new(l_par: Pos, key: Option<Key<'a>>, r_par: Option<Pos>) -> Self {
        Self {
            l_par,
            key,
            r_par_offset: r_par.map(|r_par| {
                NonZeroU32::new(r_par.char - l_par.char)
                    .expect("l_par and r_par can't be at the same position")
            }),
        }
    }

    #[inline]
    pub fn span(&self) -> Span {
        let start = self.l_par;
        let end = self
            .r_par()
            .or_else(|| self.key.as_ref().map(|k| k.span().end))
            .unwrap_or_else(|| self.l_par.plus(1));
        Span { start, end }
    }

    pub fn r_par(&self) -> Option<Pos> {
        self.r_par_offset.map(|o| self.l_par.plus(o.get()))
    }
}

#[derive(Debug, PartialEq)]
pub struct ArrayEntry<'a> {
    pub comments: Comments,
    pub header: ArrayHeader<'a>,
    pub assignments: BVec<'a, ToplevelAssignment<'a>>,
}

impl<'a> ArrayEntry<'a> {
    #[inline]
    pub fn span(&self) -> Span {
        let header_span = self.header.span();
        let start = header_span.start;
        let end = (self.assignments.last())
            .map(|a| a.assignment.val.span().end)
            .unwrap_or(header_span.end);
        Span { start, end }
    }

    /// Comment on the same line as the last item of this table
    pub fn append_comment(&mut self, id: CommentId) {
        match self.assignments.last_mut() {
            Some(a) => a.comments.append(id),
            None => self.comments.append(id),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ArrayHeader<'a> {
    pub l_pars: (Pos, Pos),
    pub key: Option<Key<'a>>,
    pub r_par_offsets: (Option<NonZeroU32>, Option<NonZeroU32>),
}

impl<'a> ArrayHeader<'a> {
    pub fn new(
        l_pars: (Pos, Pos),
        key: Option<Key<'a>>,
        r_pars: (Option<Pos>, Option<Pos>),
    ) -> Self {
        Self {
            l_pars,
            key,
            r_par_offsets: (
                r_pars.0.map(|r_par| {
                    NonZeroU32::new(r_par.char - l_pars.0.char)
                        .expect("l_par and r_par can't be at the same position")
                }),
                r_pars.1.map(|r_par| {
                    NonZeroU32::new(r_par.char - l_pars.0.char)
                        .expect("l_par and r_par can't be at the same position")
                }),
            ),
        }
    }

    #[inline]
    pub fn span(&self) -> Span {
        let start = self.l_pars.0;

        let r_pars = self.r_pars();
        let end = (r_pars.1)
            .or_else(|| r_pars.0)
            .or_else(|| self.key.as_ref().map(|k| k.span().end))
            .unwrap_or_else(|| self.l_pars.1.plus(1));
        Span { start, end }
    }

    #[inline(always)]
    pub fn r_pars(&self) -> (Option<Pos>, Option<Pos>) {
        let a = (self.r_par_offsets.0).map(|o| self.l_pars.0.plus(o.get() + 1));
        let b = (self.r_par_offsets.1).map(|o| self.l_pars.0.plus(o.get() + 1));
        (a, b)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ToplevelAssignment<'a> {
    pub comments: Comments,
    pub assignment: Assignment<'a>,
}

// impl<'a> ToplevelAssignment<'a> {
//     fn from(bump: &'a Bump, assignment: Assignment<'a>) -> Self {
//         Self {
//             comments: Comments::default(),
//             assignment,
//         }
//     }
// }

impl ToplevelAssignment<'_> {
    #[inline]
    pub fn span(&self) -> Span {
        self.assignment.span()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment<'a> {
    pub key: Key<'a>,
    pub eq: Pos,
    pub val: Value<'a>,
}

impl Assignment<'_> {
    #[inline]
    pub fn span(&self) -> Span {
        Span::across(self.key.span(), self.val.span())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Key<'a> {
    One(Ident<'a>),
    Dotted(&'a [DottedIdent<'a>]),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DottedIdent<'a> {
    pub ident: Ident<'a>,
    pub dot: Option<Pos>,
}

impl Key<'_> {
    #[inline]
    pub fn span(&self) -> Span {
        match self {
            Key::One(i) => i.lit_span(),
            Key::Dotted(idents) => {
                let start = idents.first().unwrap().ident.lit_span().start;
                let last = idents.last().unwrap();
                let end = last.dot.map_or(last.ident.lit_span().end, |p| p.plus(1));
                Span { start, end }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ident<'a> {
    pub lit: &'a str,
    pub lit_start: Pos,
    pub text: &'a str,
    pub text_start_offset: u8,
    pub text_end_offset: u8,
    pub kind: IdentKind,
}

impl<'a> Ident<'a> {
    pub fn from_plain_lit(lit: &'a str, span: Span) -> Self {
        Ident {
            lit,
            lit_start: span.start,
            text: lit,
            text_start_offset: 0,
            text_end_offset: 0,
            kind: IdentKind::Plain,
        }
    }

    pub fn from_string(
        lit: &'a str,
        lit_span: Span,
        text: &'a str,
        text_span: Span,
        kind: IdentKind,
    ) -> Self {
        Ident {
            lit,
            lit_start: lit_span.start,
            text,
            text_start_offset: (text_span.start.char - lit_span.start.char) as u8,
            text_end_offset: (lit_span.end.char - text_span.end.char) as u8,
            kind,
        }
    }

    pub fn text(&self) -> &str {
        self.text.as_ref()
    }

    #[inline(always)]
    pub fn lit_span(&self) -> Span {
        Span::from_pos_len(self.lit_start, self.lit.len() as u32)
    }

    #[inline(always)]
    pub fn text_span(&self) -> Span {
        let start = self.lit_start.plus(self.text_start_offset as u32);
        let len = self.lit.len() as u32 - (self.text_start_offset + self.text_end_offset) as u32;
        Span::from_pos_len(start, len)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IdentKind {
    Plain,
    BasicString,
    LiteralString,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'a> {
    String(StringVal<'a>),
    Int(IntVal<'a>),
    Float(FloatVal<'a>),
    Bool(BoolVal),
    DateTime(DateTimeVal<'a>),
    InlineTable(InlineTable<'a>),
    InlineArray(InlineArray<'a>),
    Invalid(&'a str, Span),
}

impl Value<'_> {
    #[inline]
    pub fn span(&self) -> Span {
        match self {
            Value::String(s) => s.lit_span,
            Value::Int(i) => i.lit_span,
            Value::Float(f) => f.lit_span,
            Value::Bool(b) => b.lit_span,
            Value::DateTime(d) => d.lit_span,
            Value::InlineTable(t) => t.span(),
            Value::InlineArray(a) => a.span(),
            Value::Invalid(_, r) => *r,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StringVal<'a> {
    pub lit: &'a str,
    pub lit_span: Span,
    pub text: &'a str,
    pub text_start_offset: u8,
    pub text_end_offset: u8,
    pub quote: Quote,
}

impl<'a> StringVal<'a> {
    pub fn new(lit: &'a str, lit_span: Span, text: &'a str, text_span: Span, quote: Quote) -> Self {
        Self {
            lit,
            lit_span,
            text,
            text_start_offset: (text_span.start.char - lit_span.start.char) as u8,
            text_end_offset: (lit_span.end.char - text_span.end.char) as u8,
            quote,
        }
    }

    pub fn text_span(&self) -> Span {
        let start = self.lit_span.start.plus(self.text_start_offset as u32);
        let end = self.lit_span.end.minus(self.text_end_offset as u32);
        Span { start, end }
    }

    pub fn l_quote(&'a self) -> &'a str {
        let quote_end = self.text_start_offset as usize;
        &self.lit[0..quote_end]
    }

    /// Returns either the right quote or none if the string literal is unclosed.
    pub fn r_quote(&'a self) -> Option<&'a str> {
        if self.text_end_offset == 0 {
            None
        } else {
            let quote_start = self.lit.len() - self.text_end_offset as usize;
            Some(&self.lit[quote_start..])
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntVal<'a> {
    pub lit: &'a str,
    pub lit_span: Span,
    pub val: i64,
}

impl<'a> IntVal<'a> {
    pub fn new(lit: &'a str, lit_span: Span, val: i64) -> Self {
        Self { lit, lit_span, val }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FloatVal<'a> {
    pub lit: &'a str,
    pub lit_span: Span,
    pub val: f64,
}

impl<'a> FloatVal<'a> {
    pub fn new(lit: &'a str, lit_span: Span, val: f64) -> Self {
        Self { lit, lit_span, val }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BoolVal {
    pub lit_span: Span,
    pub val: bool,
}

impl BoolVal {
    pub fn new(lit_span: Span, val: bool) -> Self {
        Self { lit_span, val }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DateTimeVal<'a> {
    pub lit: &'a str,
    pub lit_span: Span,
    pub val: DateTime,
}

impl<'a> DateTimeVal<'a> {
    pub fn new(lit: &'a str, lit_span: Span, val: DateTime) -> Self {
        Self { lit, lit_span, val }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InlineTable<'a> {
    pub l_par: Pos,
    pub assignments: &'a [InlineTableAssignment<'a>],
    pub r_par: Option<Pos>,
}

impl InlineTable<'_> {
    #[inline]
    pub fn span(&self) -> Span {
        let start = self.l_par;
        let end = (self.r_par.map(|p| p.plus(1)))
            .or_else(|| self.assignments.last().map(|a| a.span().end))
            .unwrap_or_else(|| self.l_par.plus(1));
        Span { start, end }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InlineTableAssignment<'a> {
    pub assignment: Assignment<'a>,
    pub comma: Option<Pos>,
}

impl InlineTableAssignment<'_> {
    #[inline]
    pub fn span(&self) -> Span {
        let start = self.assignment.key.span().start;
        let end = self
            .comma
            .map(|c| c.plus(1))
            .unwrap_or_else(|| self.assignment.val.span().end);
        Span { start, end }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InlineArray<'a> {
    pub comments: Comments,
    pub l_par: Pos,
    pub values: &'a [InlineArrayValue<'a>],
    pub r_par: Option<Pos>,
}

impl InlineArray<'_> {
    #[inline]
    pub fn span(&self) -> Span {
        let start = self.l_par;
        let end = (self.r_par.map(|p| p.plus(1)))
            .or_else(|| self.values.last().map(|a| a.span().end))
            .unwrap_or_else(|| self.l_par.plus(1));
        Span { start, end }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InlineArrayValue<'a> {
    pub comments: Comments,
    pub val: Value<'a>,
    pub comma: Option<Pos>,
}

impl InlineArrayValue<'_> {
    #[inline]
    pub fn span(&self) -> Span {
        let start = self.val.span().start;
        let end = self
            .comma
            .map(|c| c.plus(1))
            .unwrap_or_else(|| self.val.span().end);
        Span { start, end }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Comment<'a> {
    pub span: Span,
    pub text: &'a str,
}

// TODO: cursor to peek multiple tokens ahead and revert
// -> use for heuristics to detect unclosed inline arrays
#[derive(Debug)]
struct Parser<'a> {
    strings: &'a [StringToken<'a>],
    literals: &'a [&'a str],
    tokens: &'a [Token],
    cursor: usize,
    eof: Token,
}

impl<'a> Parser<'a> {
    fn new(tokens: &Tokens<'a>) -> Self {
        Self {
            strings: tokens.strings,
            literals: tokens.literals,
            tokens: tokens.tokens,
            cursor: 0,
            eof: tokens.eof,
        }
    }

    fn next(&mut self) -> Token {
        if self.cursor < self.tokens.len() {
            let t = self.tokens[self.cursor];
            self.cursor += 1;
            t
        } else {
            self.eof
        }
    }

    fn peek(&mut self) -> Token {
        if self.cursor < self.tokens.len() {
            self.tokens[self.cursor]
        } else {
            self.eof
        }
    }

    fn eat_comment(&mut self) -> Option<Comment<'a>> {
        let t = self.peek();
        match t.ty {
            TokenType::Comment(id) => {
                let text = self.literal(id);
                let c = Comment { span: t.span, text };
                self.next();
                Some(c)
            }
            _ => None,
        }
    }

    fn eat_comment_and_newlines(&mut self) -> Option<Comment<'a>> {
        loop {
            let t = self.peek();
            match t.ty {
                TokenType::Comment(id) => {
                    let text = self.literal(id);
                    let c = Comment { span: t.span, text };
                    self.next();
                    return Some(c);
                }
                TokenType::Newline => {
                    self.next();
                }
                _ => return None,
            }
        }
    }

    fn string(&self, id: StringId) -> &'a StringToken<'a> {
        &self.strings[id.0 as usize]
    }

    fn literal(&self, id: LiteralId) -> &'a str {
        self.literals[id.0 as usize]
    }

    fn token_to_fmt_str(&self, ty: TokenType) -> FmtStr {
        let mut string = String::new();
        _ = ty.display(&mut string, &self.strings, &self.literals);
        FmtStr::from_string(string)
    }
}

/// A possibly only partially parsed value
enum PartialValue {
    /// An integer that is prefixed by either `0b`, `0o`, or `0x`.
    PrefixedInt(i64),
    /// A valid decimal integer, but could also be the integer part of a float.
    Int(i64),
    /// Possibly the integer part of a float, otherwise an error.
    OverflowOrFloat,
    /// A float with an exponent. There can't be a fractional part after this.
    FloatWithExp,
    /// A complete offset date-time, without the sub second part, but with an offset.
    OffsetDateTime(DateTime),
    /// A date-time without the subsecond part and an offset, might be followed by the subsecond
    /// part.
    PartialDateTime(Date, Time),
    /// Just the date part, might be followed by the time part.
    PartialDate(Date),
    /// A local time without sub second part, might be followed by it.
    PartialTime(Time),
}

/// All errors are stored inside the [`Ctx`]. If a fatal error occurs, a unit error
/// is returned, otherwise the possibly partially invalid ast is returned.
pub fn parse<'a>(ctx: &mut Ctx, bump: &'a Bump, tokens: &'_ Tokens<'a>) -> Asts<'a> {
    let mut parser = Parser::new(tokens);
    let mut asts = BVec::new_in(bump);
    let mut comment_storage = BVec::new_in(bump);
    let mut newline_required = false;
    let mut prev_comments = Vec::new();

    'root: loop {
        if newline_required {
            if let Some(comment) = parser.eat_comment() {
                let comment = AssociatedComment::line_end(comment);
                let comment_id = store_comment(&mut comment_storage, comment);
                match asts.last_mut() {
                    Some(Ast::Table(t)) => t.append_comment(comment_id),
                    Some(Ast::Array(a)) => a.append_comment(comment_id),
                    Some(Ast::Assignment(a)) => a.comments.append(comment_id),
                    Some(Ast::Comment(_)) | None => unreachable!(
                        "newline is only required after table/array headers and assignments"
                    ),
                }
            }
            match parser.peek() {
                t if t.ty == TokenType::Newline => {
                    parser.next();
                }
                t if t.ty == TokenType::EOF => break 'root,
                t => {
                    ctx.error(Error::MissingNewline(t.span.start));
                }
            }

            newline_required = false;
        }

        let token = parser.peek();
        match token.ty {
            TokenType::SquareLeft => {
                let l_table_square = token.span;
                parser.next();

                let l_array_square = match parser.peek() {
                    t if t.ty == TokenType::SquareLeft => Some(parser.next().span),
                    _ => None,
                };

                let key = match parse_key(ctx, bump, &mut parser) {
                    Ok(k) => Some(k),
                    Err(e) => {
                        ctx.error(e);
                        recover_on!(parser, SquareRight | Newline | EOF => break);
                        None
                    }
                };

                let r_array_square = l_array_square.and_then(|_| match parser.peek() {
                    t if t.ty == TokenType::SquareRight => Some(parser.next().span.start),
                    t => {
                        let string = parser.token_to_fmt_str(t.ty);
                        ctx.error(Error::ExpectedRightSquareFound(string, t.span));
                        None
                    }
                });

                let r_table_square = match parser.peek() {
                    t if t.ty == TokenType::SquareRight => Some(parser.next().span.start),
                    t => {
                        let string = parser.token_to_fmt_str(t.ty);
                        ctx.error(Error::ExpectedRightSquareFound(string, t.span));
                        None
                    }
                };

                let pos = find_associated_comments(&prev_comments, l_table_square.start.line);
                asts.extend(prev_comments.drain(..pos).map(Ast::Comment));

                let associated_comments = prev_comments.drain(..).map(AssociatedComment::above);
                let comments = store_comments(&mut comment_storage, associated_comments);
                match l_array_square {
                    Some(l_array_square) => {
                        let header = ArrayHeader::new(
                            (l_table_square.start, l_array_square.start),
                            key,
                            (r_array_square, r_table_square),
                        );
                        asts.push(Ast::Array(ArrayEntry {
                            comments,
                            header,
                            assignments: BVec::new_in(bump),
                        }));
                    }
                    None => {
                        let header = TableHeader::new(l_table_square.start, key, r_table_square);
                        asts.push(Ast::Table(Table {
                            comments,
                            header,
                            assignments: BVec::new_in(bump),
                        }));
                    }
                }

                newline_required = true;
                continue;
            }
            TokenType::Comment(id) => {
                let comment = Comment {
                    span: token.span,
                    text: parser.literal(id),
                };
                prev_comments.push(comment);
                parser.next();
                continue;
            }
            TokenType::Newline => {
                parser.next();
                continue;
            }
            TokenType::EOF => break 'root,
            _ => (),
        }

        let key = match parse_key(ctx, bump, &mut parser) {
            Ok(k) => k,
            Err(e) => {
                ctx.error(e);
                recover_on!(parser, Newline | EOF => continue 'root);
            }
        };

        let eq = match parser.next() {
            t if t.ty == TokenType::Equal => t.span.start,
            t => {
                let string = parser.token_to_fmt_str(t.ty);
                ctx.error(Error::ExpectedEqFound(string, t.span));
                recover_on!(parser, Newline | EOF => continue 'root);
            }
        };

        // store associated comments here so associated comments of the value are added in the correct order
        let pos = find_associated_comments(&prev_comments, eq.line);
        let non_associated_comments = prev_comments.drain(..pos);
        match asts.last_mut() {
            Some(Ast::Table(t)) => {
                let contained_comments = non_associated_comments.map(AssociatedComment::contained);
                add_comments(&mut comment_storage, &mut t.comments, contained_comments);
            }
            Some(Ast::Array(a)) => {
                let contained_comments = non_associated_comments.map(AssociatedComment::contained);
                add_comments(&mut comment_storage, &mut a.comments, contained_comments);
            }
            Some(Ast::Assignment(_) | Ast::Comment(_)) | None => {
                let freestanding_comments = non_associated_comments.map(Ast::Comment);
                asts.extend(freestanding_comments);
            }
        }

        let associated_comments = prev_comments.drain(..).map(AssociatedComment::above);
        let mut comments = store_comments(&mut comment_storage, associated_comments);

        let val = match parse_value(ctx, bump, &mut parser, &mut comment_storage) {
            Ok(v) => v,
            Err(e) => {
                ctx.error(e);
                recover_on!(parser, Newline | EOF => continue 'root);
            }
        };

        // include all associated comments of inner values
        comments.extend_to(next_comment_id(&comment_storage));

        let assignment = Assignment { key, eq, val };
        let assignment = ToplevelAssignment {
            comments,
            assignment,
        };
        match asts.last_mut() {
            Some(Ast::Table(t)) => t.assignments.push(assignment),
            Some(Ast::Array(a)) => a.assignments.push(assignment),
            Some(Ast::Assignment(_) | Ast::Comment(_)) | None => {
                asts.push(Ast::Assignment(assignment))
            }
        }

        newline_required = true;
    }

    asts.extend(prev_comments.into_iter().map(Ast::Comment));

    Asts {
        asts: asts.into_bump_slice(),
        comments: comment_storage.into_bump_slice(),
    }
}

fn find_associated_comments<'a>(comments: &[Comment<'a>], mut line: u32) -> usize {
    let len = comments.iter().rev().position(|c| {
        let contigous = c.span.start.line + 1 == line;
        line -= 1;
        !contigous
    });
    len.map_or(0, |l| comments.len() - l)
}

fn mark_comments_above<'a>(storage: &mut [AssociatedComment<'a>], mut line: u32) -> Comments {
    let len = storage.iter_mut().rev().position(|c| {
        let contigous = c.comment.span.start.line + 1 == line && c.pos == AssociatedPos::Contained;
        line -= 1;
        if contigous {
            c.pos = AssociatedPos::Above;
        }
        !contigous
    });
    let len = len.map_or(0, |l| l as u32);
    let start = CommentId(storage.len() as u32 - len);
    Comments { start, len }
}

fn add_comments<'a>(
    storage: &mut BVec<'a, AssociatedComment<'a>>,
    range: &mut Comments,
    comments: impl Iterator<Item = AssociatedComment<'a>>,
) {
    for c in comments {
        let id = store_comment(storage, c);
        range.append(id);
    }
}

fn add_comment<'a>(
    storage: &mut BVec<'a, AssociatedComment<'a>>,
    range: &mut Comments,
    comment: AssociatedComment<'a>,
) {
    let id = store_comment(storage, comment);
    range.append(id);
}

#[must_use]
fn store_comments<'a>(
    storage: &mut BVec<'a, AssociatedComment<'a>>,
    comments: impl Iterator<Item = AssociatedComment<'a>>,
) -> Comments {
    let mut range = Comments {
        start: next_comment_id(storage),
        len: 0,
    };
    add_comments(storage, &mut range, comments);
    range
}

#[must_use]
fn store_comment<'a>(
    storage: &mut BVec<'a, AssociatedComment<'a>>,
    comment: AssociatedComment<'a>,
) -> CommentId {
    let id = next_comment_id(storage);
    storage.push(comment);
    id
}

#[inline(always)]
fn next_comment_id(storage: &[AssociatedComment<'_>]) -> CommentId {
    CommentId(storage.len() as u32)
}

fn parse_key<'a>(ctx: &mut Ctx, bump: &'a Bump, parser: &mut Parser<'a>) -> Result<Key<'a>, Error> {
    let mut idents = BVec::new_in(bump);
    loop {
        let token = parser.peek();
        let ident = match token.ty {
            TokenType::String(id) => {
                let str = parser.string(id);
                let kind = match str.quote {
                    Quote::Basic => IdentKind::BasicString,
                    Quote::Literal => IdentKind::LiteralString,
                    Quote::BasicMultiline => {
                        return Err(Error::MultilineBasicStringIdent(token.span))
                    }
                    Quote::LiteralMultiline => {
                        return Err(Error::MultilineLiteralStringIdent(token.span))
                    }
                };
                Ident::from_string(str.lit, token.span, str.text, str.text_span, kind)
            }
            TokenType::LiteralOrIdent(id) => {
                let lit = parser.literal(id);
                let invalid_char = lit
                    .char_indices()
                    .find(|(_, c)| !matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'));

                if let Some((i, c)) = invalid_char {
                    let mut pos = token.span.start;
                    pos.char += i as u32;
                    ctx.error(Error::InvalidCharInIdentifier(FmtChar(c), pos));
                }

                Ident::from_plain_lit(lit, token.span)
            }
            TokenType::Comment(_)
            | TokenType::SquareLeft
            | TokenType::SquareRight
            | TokenType::CurlyLeft
            | TokenType::CurlyRight
            | TokenType::Equal
            | TokenType::Comma
            | TokenType::Dot
            | TokenType::Newline
            | TokenType::EOF => {
                let string = parser.token_to_fmt_str(token.ty);
                return Err(Error::ExpectedKeyFound(string, token.span));
            }
        };
        parser.next();

        match parser.peek() {
            t if t.ty == TokenType::Dot => {
                let dot = Some(t.span.start);
                idents.push(DottedIdent { ident, dot });
                parser.next();
            }
            _ => {
                return if idents.is_empty() {
                    Ok(Key::One(ident))
                } else {
                    idents.push(DottedIdent { ident, dot: None });
                    Ok(Key::Dotted(idents.into_bump_slice()))
                };
            }
        }
    }
}

fn parse_value<'a>(
    ctx: &mut Ctx,
    bump: &'a Bump,
    parser: &mut Parser<'a>,
    comment_storage: &mut BVec<'a, AssociatedComment<'a>>,
) -> Result<Value<'a>, Error> {
    let token = parser.peek();
    let value = match token.ty {
        TokenType::String(id) => {
            let token = parser.next();
            let str = parser.string(id);
            Value::String(StringVal::new(
                str.lit,
                token.span,
                str.text,
                str.text_span,
                str.quote,
            ))
        }
        TokenType::LiteralOrIdent(id) => {
            let token = parser.next();
            let lit = parser.literal(id);

            match lit {
                "true" => Value::Bool(BoolVal::new(token.span, true)),
                "false" => Value::Bool(BoolVal::new(token.span, false)),
                "nan" => Value::Float(FloatVal::new(lit, token.span, f64::NAN)),
                "+nan" => Value::Float(FloatVal::new(lit, token.span, f64::NAN)),
                "-nan" => Value::Float(FloatVal::new(lit, token.span, -f64::NAN)),
                "inf" => Value::Float(FloatVal::new(lit, token.span, f64::INFINITY)),
                "+inf" => Value::Float(FloatVal::new(lit, token.span, f64::INFINITY)),
                "-inf" => Value::Float(FloatVal::new(lit, token.span, f64::NEG_INFINITY)),
                _ => match num::parse_num_or_date(lit, token.span) {
                    Ok(PartialValue::PrefixedInt(i)) => Value::Int(IntVal::new(lit, token.span, i)),
                    Ok(PartialValue::Int(i)) => {
                        try_to_parse_fractional_part_of_float(ctx, parser, lit, token.span, Some(i))
                    }
                    Ok(PartialValue::OverflowOrFloat) => {
                        try_to_parse_fractional_part_of_float(ctx, parser, lit, token.span, None)
                    }
                    Ok(PartialValue::FloatWithExp) => match lit.replace('_', "").parse() {
                        Ok(v) => Value::Float(FloatVal::new(lit, token.span, v)),
                        Err(_) => {
                            ctx.error(Error::FloatLiteralOverflow(token.span));
                            Value::Invalid(lit, token.span)
                        }
                    },
                    Ok(PartialValue::OffsetDateTime(val)) => {
                        let date_time = DateTimeVal::new(lit, token.span, val);
                        Value::DateTime(date_time)
                    }
                    Ok(PartialValue::PartialDate(date)) => {
                        try_to_parse_time_part(ctx, parser, lit, token.span, date)
                    }
                    Ok(PartialValue::PartialDateTime(date, time)) => {
                        try_to_parse_subsecs(ctx, parser, lit, token.span, Some(date), time)
                    }
                    Ok(PartialValue::PartialTime(time)) => {
                        try_to_parse_subsecs(ctx, parser, lit, token.span, None, time)
                    }
                    Err(e) => {
                        ctx.error(e);
                        Value::Invalid(lit, token.span)
                    }
                },
            }
        }
        TokenType::SquareLeft => {
            let l_par = token.span.start;
            parser.next();

            let mut array_comments = Comments::new(next_comment_id(comment_storage), 0);
            let mut values = BVec::new_in(bump);

            if let Some(comment) = parser.eat_comment() {
                let comment = AssociatedComment::line_end(comment);
                add_comment(comment_storage, &mut array_comments, comment);
            }

            'inline_array: loop {
                while let Some(comment) = parser.eat_comment_and_newlines() {
                    let comment = AssociatedComment::contained(comment);
                    add_comment(comment_storage, &mut array_comments, comment);
                }

                if matches!(parser.peek().ty, TokenType::SquareRight | TokenType::EOF) {
                    break;
                }

                let val = match parse_value(ctx, bump, parser, comment_storage) {
                    Ok(v) => v,
                    Err(e) => {
                        ctx.error(e);
                        recover_on!(parser,
                            Comma | Newline => continue 'inline_array,
                            SquareRight | EOF => break 'inline_array,
                        );
                    }
                };

                let val_line = val.span().start.line;
                let mut val_comments = mark_comments_above(comment_storage, val_line);
                if let Some(comment) = parser.eat_comment() {
                    let comment = AssociatedComment::line_end(comment);
                    add_comment(comment_storage, &mut val_comments, comment);
                }

                while let Some(comment) = parser.eat_comment_and_newlines() {
                    let comment = AssociatedComment::contained(comment);
                    add_comment(comment_storage, &mut array_comments, comment);
                }
                let comma = match parser.peek() {
                    t if t.ty == TokenType::Comma => {
                        let end = next_comment_id(comment_storage);
                        val_comments.extend_to(end);

                        let comma = parser.next().span.start;
                        if let Some(comment) = parser.eat_comment() {
                            let comment = AssociatedComment::line_end(comment);
                            add_comment(comment_storage, &mut val_comments, comment);
                        }

                        Some(comma)
                    }
                    t if t.ty == TokenType::SquareRight || t.ty == TokenType::EOF => {
                        values.push(InlineArrayValue {
                            comments: val_comments,
                            val,
                            comma: None,
                        });
                        break;
                    }
                    _ => {
                        ctx.error(Error::MissingComma(val.span().end));
                        // try to continue
                        None
                    }
                };

                values.push(InlineArrayValue {
                    comments: val_comments,
                    val,
                    comma,
                });
            }

            let r_par = match parser.peek() {
                t if t.ty == TokenType::SquareRight => Some(parser.next().span.start),
                t => {
                    let string = parser.token_to_fmt_str(t.ty);
                    ctx.error(Error::ExpectedRightSquareFound(string, t.span));
                    None
                }
            };

            let end = next_comment_id(comment_storage);
            array_comments.extend_to(end);

            Value::InlineArray(InlineArray {
                comments: array_comments,
                l_par,
                values: values.into_bump_slice(),
                r_par,
            })
        }
        TokenType::CurlyLeft => {
            let l_par = token.span.start;
            parser.next();

            let mut assignments = BVec::new_in(bump);
            let mut comma = None;
            'inline_table: loop {
                if matches!(parser.peek().ty, TokenType::CurlyRight | TokenType::EOF) {
                    if let Some(pos) = comma {
                        ctx.error(Error::InlineTableTrailingComma(pos));
                    }
                    break;
                }
                let key = match parse_key(ctx, bump, parser) {
                    Ok(k) => k,
                    Err(e) => {
                        ctx.error(e);
                        recover_on!(parser,
                            Comma => continue 'inline_table,
                            Newline | CurlyRight | EOF => break 'inline_table,
                        )
                    }
                };

                let eq = match parser.peek() {
                    t if t.ty == TokenType::Equal => parser.next().span.start,
                    t => {
                        let string = parser.token_to_fmt_str(t.ty);
                        ctx.error(Error::ExpectedEqFound(string, t.span));
                        recover_on!(parser,
                            Comma => continue 'inline_table,
                            Newline | CurlyRight | EOF => break 'inline_table,
                        )
                    }
                };

                let val = match parse_value(ctx, bump, parser, comment_storage) {
                    Ok(v) => v,
                    Err(e) => {
                        ctx.error(e);
                        recover_on!(parser,
                            Comma => continue 'inline_table,
                            Newline | CurlyRight | EOF => break 'inline_table,
                        )
                    }
                };

                let assignment = Assignment { key, eq, val };
                comma = match parser.peek() {
                    t if t.ty == TokenType::Comma => Some(parser.next().span.start),
                    t if t.ty == TokenType::CurlyRight || t.ty == TokenType::EOF => {
                        assignments.push(InlineTableAssignment {
                            assignment,
                            comma: None,
                        });
                        break;
                    }
                    _ => {
                        let pos = assignment.val.span().end;
                        ctx.error(Error::MissingComma(pos));
                        // try to continue
                        None
                    }
                };

                assignments.push(InlineTableAssignment { assignment, comma });
            }

            let r_par = match parser.peek() {
                t if t.ty == TokenType::CurlyRight => Some(parser.next().span.start),
                t => {
                    let string = parser.token_to_fmt_str(t.ty);
                    ctx.error(Error::ExpectedRightCurlyFound(string, t.span));
                    None
                }
            };

            Value::InlineTable(InlineTable {
                l_par,
                assignments: assignments.into_bump_slice(),
                r_par,
            })
        }
        TokenType::Comment(_)
        | TokenType::SquareRight
        | TokenType::CurlyRight
        | TokenType::Equal
        | TokenType::Comma
        | TokenType::Dot
        | TokenType::Newline
        | TokenType::EOF => {
            let string = parser.token_to_fmt_str(token.ty);
            return Err(Error::ExpectedValueFound(string, token.span));
        }
    };

    Ok(value)
}

fn try_to_parse_fractional_part_of_float<'a>(
    ctx: &mut Ctx,
    parser: &mut Parser<'a>,
    int_lit: &'a str,
    int_span: Span,
    int_val: Option<i64>,
) -> Value<'a> {
    // Check this is actually a floating point literal separated by a dot
    match parser.peek() {
        t if t.ty == TokenType::Dot && int_span.end == t.span.start => {
            parser.next();
        }
        _ => match int_val {
            Some(val) => return Value::Int(IntVal::new(int_lit, int_span, val)),
            None => {
                ctx.error(Error::IntLiteralOverflow(int_span));
                return Value::Invalid(int_lit, int_span);
            }
        },
    }

    let mut missing_float_fractional_part_error = || {
        let pos = int_span.end.plus(1);
        ctx.error(Error::MissingFloatFractionalPart(pos));

        // SAFETY: we know there is a dot directly after int_lit.
        let lit = unsafe {
            let ptr = int_lit.as_ptr();
            let len = int_lit.len() + 1;
            let slice = std::slice::from_raw_parts(ptr, len);
            std::str::from_utf8_unchecked(slice)
        };
        let mut span = int_span;
        span.end.char += 1;
        Value::Invalid(lit, span)
    };

    let frac;
    let frac_lit = match parser.peek().ty {
        TokenType::LiteralOrIdent(id) => {
            frac = parser.next();
            let frac_lit = parser.literal(id);

            let int_end = int_lit.as_bytes().as_ptr_range().end;
            let frac_start = frac_lit.as_bytes().as_ptr_range().start;
            // SAFETY: we know there is a dot directly after int_lit.
            let dot_end = unsafe { int_end.add(1) };
            if dot_end != frac_start {
                return missing_float_fractional_part_error();
            }
            frac_lit
        }
        _ => return missing_float_fractional_part_error(),
    };

    // SAFETY: the first and second literal reference the same string and
    // are only separated by a single dot. See above.
    let lit = unsafe {
        let ptr = int_lit.as_ptr();
        let len = int_lit.len() + 1 + frac_lit.len();
        let slice = std::slice::from_raw_parts(ptr, len);
        std::str::from_utf8_unchecked(slice)
    };
    let span = Span::across(int_span, frac.span);

    // validate fractional part
    if let Err(e) = num::validate_float_fractional_part(frac_lit, frac.span) {
        ctx.error(e);
        return Value::Invalid(lit, span);
    }

    let Ok(val) = lit.replace('_', "").parse() else {
        ctx.error(Error::FloatLiteralOverflow(span));
        return Value::Invalid(lit, span);
    };

    Value::Float(FloatVal::new(lit, span, val))
}

/// toml permits using spaces instead of `T` to separate date and time in and rfc3339
/// timestamp, if the previous token just contained the date then check if the next token
/// contains the time.
fn try_to_parse_time_part<'a>(
    ctx: &mut Ctx,
    parser: &mut Parser<'a>,
    date_lit: &'a str,
    date_span: Span,
    date: Date,
) -> Value<'a> {
    let (time_lit, time_span) = match parser.peek().ty {
        TokenType::LiteralOrIdent(id) => {
            let token = parser.next();
            let lit = parser.literal(id);
            (lit, token.span)
        }
        _ => {
            let val = DateTime::LocalDate(date);
            let date_time = DateTimeVal::new(date_lit, date_span, val);
            return Value::DateTime(date_time);
        }
    };

    // only need to compare columns, since we known there is no newline token in between
    if time_span.start.char > date_span.end.char + 1 {
        let span = Span::between(date_span, time_span);
        ctx.error(Error::DateAndTimeTooFarApart(span));
    }

    // SAFETY: the first and second literal reference the same string, are on the same line and
    // are only separated by whitespace. See above.
    let lit = unsafe {
        let ptr = date_lit.as_ptr();
        let len = (time_span.end.char - date_span.start.char) as usize;
        let slice = std::slice::from_raw_parts(ptr, len);
        std::str::from_utf8_unchecked(slice)
    };
    let span = Span::across(date_span, time_span);

    let mut chars = time_lit.char_indices().peekable();
    let (time, offset) = match datetime::parse_time_and_offset(&mut chars, time_span) {
        Ok(v) => v,
        Err(e) => {
            ctx.error(e);
            return Value::Invalid(lit, span);
        }
    };

    if let Some(offset) = offset {
        let val = DateTime::OffsetDateTime(date, time, offset);
        let date_time = DateTimeVal::new(lit, span, val);
        return Value::DateTime(date_time);
    }

    try_to_parse_subsecs(ctx, parser, lit, span, Some(date), time)
}

fn try_to_parse_subsecs<'a>(
    ctx: &mut Ctx,
    parser: &mut Parser<'a>,
    date_time_lit: &'a str,
    date_time_span: Span,
    date: Option<Date>,
    time: Time,
) -> Value<'a> {
    match parser.peek() {
        t if t.ty == TokenType::Dot && date_time_span.end == t.span.start => {
            parser.next();
        }
        _ => {
            let val = match date {
                Some(date) => DateTime::LocalDateTime(date, time),
                None => DateTime::LocalTime(time),
            };
            let date_time = DateTimeVal::new(date_time_lit, date_time_span, val);
            return Value::DateTime(date_time);
        }
    }

    let mut missing_date_time_subsec_part_error = || {
        let pos = date_time_span.end.plus(1);
        ctx.error(Error::DateTimeMissingSubsec(pos));

        // SAFETY: we know there is a dot directly after int_lit.
        let lit = unsafe {
            let ptr = date_time_lit.as_ptr();
            let len = date_time_lit.len() + 1;
            let slice = std::slice::from_raw_parts(ptr, len);
            std::str::from_utf8_unchecked(slice)
        };
        let mut span = date_time_span;
        span.end.char += 1;
        Value::Invalid(lit, span)
    };

    let subsec;
    let subsec_lit = match parser.peek().ty {
        TokenType::LiteralOrIdent(id) => {
            subsec = parser.next();
            let subsec_lit = parser.literal(id);

            let time_end = date_time_lit.as_bytes().as_ptr_range().end;
            let subsec_start = subsec_lit.as_bytes().as_ptr_range().start;
            // SAFETY: we know there is a dot directly after date_time_lit.
            let dot_end = unsafe { time_end.add(1) };
            if dot_end != subsec_start {
                return missing_date_time_subsec_part_error();
            }
            subsec_lit
        }
        _ => return missing_date_time_subsec_part_error(),
    };

    // SAFETY: the first and second literal reference the same string and
    // are only separated by a single dot. See above.
    let lit = unsafe {
        let ptr = date_time_lit.as_ptr();
        let len = date_time_lit.len() + 1 + subsec_lit.len();
        let slice = std::slice::from_raw_parts(ptr, len);
        std::str::from_utf8_unchecked(slice)
    };
    let span = Span {
        start: date_time_span.start,
        end: subsec.span.end,
    };

    // parse subsec part
    match datetime::parse_subsec_part(lit, span, subsec_lit, subsec.span, date, time) {
        Ok(date_time) => Value::DateTime(date_time),
        Err(e) => {
            ctx.error(e);
            Value::Invalid(lit, span)
        }
    }
}
