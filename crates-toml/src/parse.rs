use std::num::NonZeroU32;

use bumpalo::collections::Vec as BVec;
use bumpalo::Bump;
use common::{FmtChar, FmtStr, Pos, Span};

use crate::datetime::{Date, DateTime, Time};
use crate::lex::{LiteralId, StringId, StringToken, TextOffset, Token, TokenType, Tokens};
use crate::{Error, Quote, TomlCtx};

pub use num::{IntPrefix, LitPart, Sign};

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

macro_rules! one_of {
    ($token:expr, $tokens:pat) => {{
        use TokenType::*;
        matches!($token, $tokens)
    }};
}

#[derive(Debug, PartialEq)]
pub struct Asts<'a> {
    pub asts: &'a [Ast<'a>],
    pub comments: &'a [AssocComment<'a>],
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
pub struct CommentRange {
    start: CommentId,
    len: u32,
    level: u16,
}

impl CommentRange {
    pub const fn new(start: CommentId, len: u32, level: u16) -> Self {
        Self { start, len, level }
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AssocComment<'a> {
    pub pos: AssocPos,
    /// level 0 are comments that are associated with items declared directly in root, such as
    /// assignments (that aren't declared inside a table), tables and arrays of tables
    pub level: u16,
    pub comment: Comment<'a>,
}

impl<'a> AssocComment<'a> {
    pub fn above(level: u16, comment: Comment<'a>) -> AssocComment<'a> {
        AssocComment {
            pos: AssocPos::Above,
            level,
            comment,
        }
    }

    pub fn line_end(level: u16, comment: Comment<'a>) -> AssocComment<'a> {
        AssocComment {
            pos: AssocPos::LineEnd,
            level,
            comment,
        }
    }

    pub fn contained(level: u16, comment: Comment<'a>) -> AssocComment<'a> {
        AssocComment {
            pos: AssocPos::Contained,
            level,
            comment,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AssocPos {
    /// Directly above the associated item, without any blank lines
    Above,
    /// At the end of the same line as the associated item.
    LineEnd,
    /// Contained inside the item.
    Contained,
}

#[derive(Debug, PartialEq)]
pub struct Table<'a> {
    pub comments: CommentRange,
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
    pub comments: CommentRange,
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
            .or(r_pars.0)
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
    pub comments: CommentRange,
    pub assignment: Assignment<'a>,
}

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
        text_offset: TextOffset,
        kind: IdentKind,
    ) -> Self {
        Ident {
            lit,
            lit_start: lit_span.start,
            text,
            // multiline strings aren't allowed as identifiers, hence line offsets are zero
            text_start_offset: text_offset.start_char,
            text_end_offset: text_offset.end_char,
            kind,
        }
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
    pub text_offset: TextOffset,
    pub quote: Quote,
}

impl<'a> StringVal<'a> {
    pub fn text_span(&self) -> Span {
        self.text_offset.apply_to(self.lit_span)
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
    pub comments: CommentRange,
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
    pub comments: CommentRange,
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

    fn peek_prev(&mut self) -> Option<Token> {
        let idx = self.cursor.checked_sub(1)?;
        Some(self.tokens[idx])
    }

    fn eat_comment(&mut self) -> Option<Comment<'a>> {
        let t = self.peek();
        match t.ty {
            TokenType::Comment(id) => {
                self.next();
                let c = self.comment(id, t.start);
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
                    self.next();
                    let c = self.comment(id, t.start);
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

    fn comment(&self, id: LiteralId, start: Pos) -> Comment<'a> {
        let text = self.literal(id);
        let span = Span::from_pos_len(start, 1 + text.len() as u32);
        Comment { span, text }
    }

    fn token_fmt_str_and_span(&self, token: Token) -> (FmtStr, Span) {
        fn ascii(char: &str, start: Pos) -> (FmtStr, Span) {
            (FmtStr::from_str(char), Span::ascii_char(start))
        }
        match token.ty {
            TokenType::String(id) => {
                let string = &self.strings[id.0 as usize];
                let span = Span::new(token.start, string.lit_end);
                let lit = FmtStr::from_str(string.lit);
                (lit, span)
            }
            TokenType::LiteralOrIdent(id) => {
                let lit = self.literals[id.0 as usize];
                let span = Span::from_pos_len(token.start, lit.len() as u32);
                let lit = FmtStr::from_str(lit);
                (lit, span)
            }
            TokenType::Comment(id) => {
                let lit = self.literals[id.0 as usize];
                let span = Span::from_pos_len(token.start, 1 + lit.len() as u32);
                let lit = FmtStr::from_string(format!("#{lit}"));
                (lit, span)
            }
            TokenType::SquareLeft => ascii("[", token.start),
            TokenType::SquareRight => ascii("]", token.start),
            TokenType::CurlyLeft => ascii("{", token.start),
            TokenType::CurlyRight => ascii("{", token.start),
            TokenType::Equal => ascii("=", token.start),
            TokenType::Comma => ascii(",", token.start),
            TokenType::Dot => ascii(".", token.start),
            TokenType::Newline => (FmtStr::from_str("\\n"), Span::pos(token.start)),
            TokenType::EOF => (FmtStr::from_str("EOF"), Span::pos(token.start)),
        }
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
pub fn parse<'a>(ctx: &mut impl TomlCtx, bump: &'a Bump, tokens: &'_ Tokens<'a>) -> Asts<'a> {
    let mut parser = Parser::new(tokens);
    let mut asts = Vec::new();
    let mut comment_storage = Vec::new();
    let mut newline_required = false;
    let mut prev_comments = Vec::new();

    'root: loop {
        if newline_required {
            if let Some(comment) = parser.eat_comment() {
                let comment = AssocComment::line_end(0, comment);
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
                    ctx.error(Error::MissingNewline(t.start));
                }
            }

            newline_required = false;
        }

        let token = parser.peek();
        match token.ty {
            TokenType::SquareLeft => {
                let l_table_square = token.start;
                parser.next();

                let l_array_square = match parser.peek() {
                    t if t.ty == TokenType::SquareLeft => {
                        parser.next();

                        if l_table_square.char + 1 != t.start.char {
                            let span = Span::new(l_table_square.plus(1), t.start);
                            ctx.error(Error::SpaceBetweenArrayPars(span));
                        }

                        Some(t.start)
                    }
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
                    t if t.ty == TokenType::SquareRight => Some(parser.next().start),
                    t => {
                        let (string, span) = parser.token_fmt_str_and_span(t);
                        ctx.error(Error::ExpectedRightSquareFound(string, span));
                        None
                    }
                });

                let r_table_square = match parser.peek() {
                    t if t.ty == TokenType::SquareRight => {
                        parser.next();

                        if let Some(a) = r_array_square {
                            if a.char + 1 != t.start.char {
                                let span = Span::new(a.plus(1), t.start);
                                ctx.error(Error::SpaceBetweenArrayPars(span));
                            }
                        }

                        Some(t.start)
                    }
                    t => {
                        let (string, span) = parser.token_fmt_str_and_span(t);
                        ctx.error(Error::ExpectedRightSquareFound(string, span));
                        None
                    }
                };

                let pos = find_associated_comments(&prev_comments, l_table_square.line);
                asts.extend(prev_comments.drain(..pos).map(Ast::Comment));

                let associated_comments = prev_comments.drain(..);
                let comments = store_comments(
                    &mut comment_storage,
                    associated_comments,
                    AssocPos::Above,
                    0,
                );
                match l_array_square {
                    Some(l_array_square) => {
                        let header = ArrayHeader::new(
                            (l_table_square, l_array_square),
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
                        let header = TableHeader::new(l_table_square, key, r_table_square);
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
                let comment = parser.comment(id, token.start);
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
            t if t.ty == TokenType::Equal => t.start,
            _ => {
                let (string, span) = parser.token_fmt_str_and_span(token);
                ctx.error(Error::ExpectedEqFound(string, span));
                recover_on!(parser, Newline | EOF => continue 'root);
            }
        };

        // store associated comments here so associated comments of the value are added in the correct order
        let pos = find_associated_comments(&prev_comments, eq.line);
        let non_associated_comments = prev_comments.drain(..pos);
        let level = match asts.last_mut() {
            Some(Ast::Table(t)) => {
                add_comments(
                    &mut comment_storage,
                    &mut t.comments,
                    non_associated_comments,
                    AssocPos::Contained,
                );
                1
            }
            Some(Ast::Array(a)) => {
                add_comments(
                    &mut comment_storage,
                    &mut a.comments,
                    non_associated_comments,
                    AssocPos::Contained,
                );
                1
            }
            Some(Ast::Assignment(_) | Ast::Comment(_)) | None => {
                let freestanding_comments = non_associated_comments.map(Ast::Comment);
                asts.extend(freestanding_comments);
                0
            }
        };

        let associated_comments = prev_comments.drain(..);
        let mut comments = store_comments(
            &mut comment_storage,
            associated_comments,
            AssocPos::Above,
            level,
        );

        let val = match parse_value(ctx, bump, &mut parser, &mut comment_storage, level) {
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
        asts: bump.alloc_slice_fill_iter(asts),
        comments: bump.alloc_slice_fill_iter(comment_storage),
    }
}

fn find_associated_comments(comments: &[Comment<'_>], mut line: u32) -> usize {
    let len = comments.iter().rev().position(|c| {
        let contigous = c.span.start.line + 1 == line;
        line -= 1;
        !contigous
    });
    len.map_or(0, |l| comments.len() - l)
}

fn mark_comments_above(
    storage: &mut [AssocComment<'_>],
    mut line: u32,
    level: u16,
) -> CommentRange {
    let len = storage.iter_mut().rev().position(|c| {
        let contigous = c.comment.span.start.line + 1 == line && c.pos == AssocPos::Contained;
        if contigous {
            line -= 1;
            c.pos = AssocPos::Above;
            c.level = level;
        }
        !contigous
    });
    let len = len.map_or(0, |l| l as u32);
    let start = CommentId(storage.len() as u32 - len);
    CommentRange { start, len, level }
}

fn mark_contained_comments(storage: &mut [AssocComment<'_>], range: &CommentRange, level: u16) {
    let start = range.start.0 as usize;
    let end = start + range.len as usize;
    for c in storage[start..end].iter_mut() {
        if c.level < level {
            c.level = level;
        }
    }
}

fn add_comments<'a>(
    storage: &mut Vec<AssocComment<'a>>,
    range: &mut CommentRange,
    comments: impl Iterator<Item = Comment<'a>>,
    pos: AssocPos,
) {
    for c in comments {
        let comment = AssocComment {
            comment: c,
            pos,
            level: range.level,
        };
        let id = store_comment(storage, comment);
        range.append(id);
    }
}

fn add_comment<'a>(
    storage: &mut Vec<AssocComment<'a>>,
    range: &mut CommentRange,
    comment: Comment<'a>,
    pos: AssocPos,
) {
    let comment = AssocComment {
        pos,
        level: range.level,
        comment,
    };
    let id = store_comment(storage, comment);
    range.append(id);
}

#[must_use]
fn store_comments<'a>(
    storage: &mut Vec<AssocComment<'a>>,
    comments: impl Iterator<Item = Comment<'a>>,
    pos: AssocPos,
    level: u16,
) -> CommentRange {
    let mut range = CommentRange {
        start: next_comment_id(storage),
        len: 0,
        level,
    };
    add_comments(storage, &mut range, comments, pos);
    range
}

#[must_use]
fn store_comment<'a>(storage: &mut Vec<AssocComment<'a>>, comment: AssocComment<'a>) -> CommentId {
    let id = next_comment_id(storage);
    storage.push(comment);
    id
}

#[inline(always)]
fn next_comment_id(storage: &[AssocComment<'_>]) -> CommentId {
    CommentId(storage.len() as u32)
}

fn parse_key<'a>(
    ctx: &mut impl TomlCtx,
    bump: &'a Bump,
    parser: &mut Parser<'a>,
) -> Result<Key<'a>, Error> {
    let mut idents = BVec::new_in(bump);
    loop {
        let token = parser.peek();
        let ident = match token.ty {
            TokenType::String(id) => {
                let str = parser.string(id);
                let lit_span = Span::new(token.start, str.lit_end);
                let kind = match str.quote {
                    Quote::Basic => IdentKind::BasicString,
                    Quote::Literal => IdentKind::LiteralString,
                    Quote::BasicMultiline => {
                        return Err(Error::MultilineBasicStringIdent(lit_span));
                    }
                    Quote::LiteralMultiline => {
                        return Err(Error::MultilineLiteralStringIdent(lit_span));
                    }
                };
                Ident::from_string(str.lit, lit_span, str.text, str.text_offset, kind)
            }
            TokenType::LiteralOrIdent(id) => {
                let lit = parser.literal(id);
                let invalid_char = lit
                    .char_indices()
                    .find(|(_, c)| !matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'));

                if let Some((i, c)) = invalid_char {
                    let mut pos = token.start;
                    pos.char += i as u32;
                    ctx.error(Error::InvalidCharInIdentifier(FmtChar(c), pos));
                }

                let span = Span::from_pos_len(token.start, lit.len() as u32);
                Ident::from_plain_lit(lit, span)
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
                let (string, span) = parser.token_fmt_str_and_span(token);
                return Err(Error::ExpectedKeyFound(string, span));
            }
        };
        parser.next();

        match parser.peek() {
            t if t.ty == TokenType::Dot => {
                let dot = Some(t.start);
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
    ctx: &mut impl TomlCtx,
    bump: &'a Bump,
    parser: &mut Parser<'a>,
    comment_storage: &mut Vec<AssocComment<'a>>,
    level: u16,
) -> Result<Value<'a>, Error> {
    let token = parser.peek();
    let value = match token.ty {
        TokenType::String(id) => {
            let token = parser.next();
            let str = parser.string(id);
            let lit_span = Span::new(token.start, str.lit_end);

            Value::String(StringVal {
                lit: str.lit,
                lit_span,
                text: str.text,
                text_offset: str.text_offset,
                quote: str.quote,
            })
        }
        TokenType::LiteralOrIdent(id) => {
            let token = parser.next();
            let lit = parser.literal(id);
            let span = Span::from_pos_len(token.start, lit.len() as u32);

            match lit {
                "true" => Value::Bool(BoolVal::new(span, true)),
                "false" => Value::Bool(BoolVal::new(span, false)),
                "nan" => Value::Float(FloatVal::new(lit, span, f64::NAN)),
                "+nan" => Value::Float(FloatVal::new(lit, span, f64::NAN)),
                "-nan" => Value::Float(FloatVal::new(lit, span, -f64::NAN)),
                "inf" => Value::Float(FloatVal::new(lit, span, f64::INFINITY)),
                "+inf" => Value::Float(FloatVal::new(lit, span, f64::INFINITY)),
                "-inf" => Value::Float(FloatVal::new(lit, span, f64::NEG_INFINITY)),
                _ => match num::parse_num_or_date(lit, span) {
                    Ok(PartialValue::PrefixedInt(i)) => Value::Int(IntVal::new(lit, span, i)),
                    Ok(PartialValue::Int(i)) => {
                        try_to_parse_fractional_part_of_float(ctx, parser, lit, span, Some(i))
                    }
                    Ok(PartialValue::OverflowOrFloat) => {
                        try_to_parse_fractional_part_of_float(ctx, parser, lit, span, None)
                    }
                    Ok(PartialValue::FloatWithExp) => match lit.replace('_', "").parse() {
                        Ok(v) => Value::Float(FloatVal::new(lit, span, v)),
                        Err(_) => {
                            ctx.error(Error::FloatLiteralOverflow(span));
                            Value::Invalid(lit, span)
                        }
                    },
                    Ok(PartialValue::OffsetDateTime(val)) => {
                        let date_time = DateTimeVal::new(lit, span, val);
                        Value::DateTime(date_time)
                    }
                    Ok(PartialValue::PartialDate(date)) => {
                        try_to_parse_time_part(ctx, parser, lit, span, date)
                    }
                    Ok(PartialValue::PartialDateTime(date, time)) => {
                        try_to_parse_subsecs(ctx, parser, lit, span, Some(date), time)
                    }
                    Ok(PartialValue::PartialTime(time)) => {
                        try_to_parse_subsecs(ctx, parser, lit, span, None, time)
                    }
                    Err(e) => {
                        ctx.error(e);
                        Value::Invalid(lit, span)
                    }
                },
            }
        }
        TokenType::SquareLeft => {
            let l_par = token.start;
            parser.next();

            let mut array_comments = CommentRange::new(next_comment_id(comment_storage), 0, level);
            let mut values = Vec::new();

            if let Some(comment) = parser.eat_comment() {
                add_comment(
                    comment_storage,
                    &mut array_comments,
                    comment,
                    AssocPos::LineEnd,
                );
            }

            'inline_array: loop {
                while let Some(comment) = parser.eat_comment_and_newlines() {
                    add_comment(
                        comment_storage,
                        &mut array_comments,
                        comment,
                        AssocPos::Contained,
                    );
                }

                if one_of!(parser.peek().ty, SquareRight | EOF) {
                    break;
                }

                let val = match parse_value(ctx, bump, parser, comment_storage, level + 1) {
                    Ok(v) => v,
                    Err(e) => {
                        ctx.error(e);
                        recover_on!(parser,
                            Comma | Newline => {
                                parser.next();
                                continue 'inline_array;
                            },
                            SquareRight | EOF => break 'inline_array,
                        );
                    }
                };

                let val_line = val.span().start.line;
                let mut val_comments = mark_comments_above(comment_storage, val_line, level + 1);
                if let Some(comment) = parser.eat_comment() {
                    add_comment(
                        comment_storage,
                        &mut val_comments,
                        comment,
                        AssocPos::LineEnd,
                    );
                }

                while let Some(comment) = parser.eat_comment_and_newlines() {
                    add_comment(
                        comment_storage,
                        &mut array_comments,
                        comment,
                        AssocPos::Contained,
                    );
                }
                let comma = match parser.peek() {
                    t if t.ty == TokenType::Comma => {
                        val_comments.extend_to(next_comment_id(comment_storage));
                        mark_contained_comments(comment_storage, &val_comments, level + 1);

                        let comma = parser.next().start;
                        if let Some(comment) = parser.eat_comment() {
                            add_comment(
                                comment_storage,
                                &mut val_comments,
                                comment,
                                AssocPos::LineEnd,
                            );
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
                t if t.ty == TokenType::SquareRight => Some(parser.next().start),
                t => {
                    let (string, mut span) = parser.token_fmt_str_and_span(t);
                    if t.ty == TokenType::EOF {
                        // show error on previous line if last line is empty
                        if let Some(t) = parser.peek_prev() {
                            if t.ty == TokenType::Newline {
                                span = Span::pos(t.start);
                            }
                        }
                    }
                    ctx.error(Error::ExpectedRightSquareFound(string, span));
                    None
                }
            };

            array_comments.extend_to(next_comment_id(comment_storage));
            mark_contained_comments(comment_storage, &array_comments, level);

            Value::InlineArray(InlineArray {
                comments: array_comments,
                l_par,
                values: bump.alloc_slice_fill_iter(values),
                r_par,
            })
        }
        TokenType::CurlyLeft => {
            let l_par = token.start;
            parser.next();

            let mut assignments = Vec::new();
            let mut comma = None;
            'inline_table: loop {
                if one_of!(parser.peek().ty, CurlyRight | Newline | EOF) {
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
                            Comma => {
                                parser.next();
                                continue 'inline_table;
                            },
                            CurlyRight | Newline | EOF => break 'inline_table,
                        )
                    }
                };

                let eq = match parser.peek() {
                    t if t.ty == TokenType::Equal => parser.next().start,
                    t => {
                        let (string, span) = parser.token_fmt_str_and_span(t);
                        ctx.error(Error::ExpectedEqFound(string, span));
                        recover_on!(parser,
                            Comma => {
                                parser.next();
                                continue 'inline_table;
                            },
                            CurlyRight | Newline | EOF => break 'inline_table,
                        )
                    }
                };

                let val = match parse_value(ctx, bump, parser, comment_storage, level + 1) {
                    Ok(v) => v,
                    Err(e) => {
                        ctx.error(e);
                        recover_on!(parser,
                            Comma => {
                                parser.next();
                                continue 'inline_table;
                            },
                            CurlyRight | Newline | EOF => break 'inline_table,
                        )
                    }
                };

                let assignment = Assignment { key, eq, val };
                comma = match parser.peek() {
                    t if t.ty == TokenType::Comma => Some(parser.next().start),
                    t if one_of!(t.ty, CurlyRight | Newline | EOF) => {
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
                t if t.ty == TokenType::CurlyRight => Some(parser.next().start),
                t => {
                    let (string, span) = parser.token_fmt_str_and_span(t);
                    ctx.error(Error::ExpectedRightCurlyFound(string, span));
                    None
                }
            };

            Value::InlineTable(InlineTable {
                l_par,
                assignments: bump.alloc_slice_fill_iter(assignments),
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
            let (string, span) = parser.token_fmt_str_and_span(token);
            return Err(Error::ExpectedValueFound(string, span));
        }
    };

    Ok(value)
}

fn try_to_parse_fractional_part_of_float<'a>(
    ctx: &mut impl TomlCtx,
    parser: &mut Parser<'a>,
    int_lit: &'a str,
    int_span: Span,
    int_val: Option<i64>,
) -> Value<'a> {
    // Check this is actually a floating point literal separated by a dot
    match parser.peek() {
        t if t.ty == TokenType::Dot && int_span.end == t.start => {
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

    let frac_span;
    let frac_lit = match parser.peek().ty {
        TokenType::LiteralOrIdent(id) => {
            let frac = parser.next();
            let frac_lit = parser.literal(id);
            frac_span = Span::from_pos_len(frac.start, frac_lit.len() as u32);

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
    let span = Span::across(int_span, frac_span);

    // validate fractional part
    if let Err(e) = num::validate_float_fractional_part(frac_lit, frac_span) {
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
    ctx: &mut impl TomlCtx,
    parser: &mut Parser<'a>,
    date_lit: &'a str,
    date_span: Span,
    date: Date,
) -> Value<'a> {
    let (time_lit, time_span) = match parser.peek().ty {
        TokenType::LiteralOrIdent(id) => {
            let token = parser.next();
            let lit = parser.literal(id);
            let span = Span::from_pos_len(token.start, lit.len() as u32);
            (lit, span)
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
    ctx: &mut impl TomlCtx,
    parser: &mut Parser<'a>,
    date_time_lit: &'a str,
    date_time_span: Span,
    date: Option<Date>,
    time: Time,
) -> Value<'a> {
    match parser.peek() {
        t if t.ty == TokenType::Dot && date_time_span.end == t.start => {
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

    let subsec_span;
    let subsec_lit = match parser.peek().ty {
        TokenType::LiteralOrIdent(id) => {
            let subsec = parser.next();
            let subsec_lit = parser.literal(id);
            subsec_span = Span::from_pos_len(subsec.start, subsec_lit.len() as u32);

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
    let span = Span::across(date_time_span, subsec_span);

    // parse subsec part
    match datetime::parse_subsec_part(lit, span, subsec_lit, subsec_span, date, time) {
        Ok(date_time) => Value::DateTime(date_time),
        Err(e) => {
            ctx.error(e);
            Value::Invalid(lit, span)
        }
    }
}
