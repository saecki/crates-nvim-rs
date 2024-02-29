use std::borrow::Borrow;

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
pub enum Ast<'a> {
    Assignment(Assignment<'a>),
    Table(Table<'a>),
    Array(ArrayEntry<'a>),
    Comment(Comment<'a>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct AssociatedComment<'a> {
    pub pos: AssociatedPos,
    pub comment: Comment<'a>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AssociatedPos {
    /// Directly above the associated item, without any blank lines
    Above,
    /// On the same line as the associated item.
    SameLine,
}

#[derive(Debug, PartialEq)]
pub struct Table<'a> {
    pub comments: Vec<AssociatedComment<'a>>,
    pub header: TableHeader<'a>,
    pub assignments: Vec<Assignment<'a>>,
}

impl<'a> Table<'a> {
    #[inline]
    pub fn span(&self) -> Span {
        let header_span = self.header.span();
        let start = header_span.start;
        let end = (self.assignments.last())
            .map(|a| a.val.span().end)
            .unwrap_or(header_span.end);
        Span { start, end }
    }

    pub fn append_comment(&mut self, comment: Comment<'a>) {
        if self.header.l_par.line == comment.span.start.line {
            self.comments.push(AssociatedComment {
                pos: AssociatedPos::SameLine,
                comment,
            });
        } else {
            todo!("add {comment:?} to last assignment");
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct TableHeader<'a> {
    pub l_par: Pos,
    pub key: Option<Key<'a>>,
    pub r_par: Option<Pos>,
}

impl TableHeader<'_> {
    #[inline]
    pub fn span(&self) -> Span {
        let start = self.l_par;
        let end = (self.r_par.map(|p| p.plus(1)))
            .or_else(|| self.key.as_ref().map(|k| k.span().end))
            .unwrap_or_else(|| self.l_par.plus(1));
        Span { start, end }
    }
}

#[derive(Debug, PartialEq)]
pub struct ArrayEntry<'a> {
    pub comments: Vec<AssociatedComment<'a>>,
    pub header: ArrayHeader<'a>,
    pub assignments: Vec<Assignment<'a>>,
}

impl<'a> ArrayEntry<'a> {
    #[inline]
    pub fn span(&self) -> Span {
        let header_span = self.header.span();
        let start = header_span.start;
        let end = (self.assignments.last())
            .map(|a| a.val.span().end)
            .unwrap_or(header_span.end);
        Span { start, end }
    }

    /// Comment on the same line as the last item of this table
    pub fn append_comment(&mut self, comment: Comment<'a>) {
        if self.header.l_pars.0.line == comment.span.start.line {
            self.comments.push(AssociatedComment {
                pos: AssociatedPos::SameLine,
                comment,
            });
        } else {
            todo!("add {comment:?} to last assignment");
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ArrayHeader<'a> {
    pub l_pars: (Pos, Pos),
    pub key: Option<Key<'a>>,
    pub r_pars: (Option<Pos>, Option<Pos>),
}

impl ArrayHeader<'_> {
    #[inline]
    pub fn span(&self) -> Span {
        let start = self.l_pars.0;
        let end = (self.r_pars.1.map(|p| p.plus(1)))
            .or_else(|| self.r_pars.0.map(|p| p.plus(1)))
            .or_else(|| self.key.as_ref().map(|k| k.span().end))
            .unwrap_or_else(|| self.l_pars.1.plus(1));
        Span { start, end }
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
    Dotted(Vec<DottedIdent<'a>>),
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
            Key::One(i) => i.lit_span,
            Key::Dotted(idents) => {
                let start = idents.first().unwrap().ident.lit_span.start;
                let last = idents.last().unwrap();
                let end = last.dot.map_or(last.ident.lit_span.end, |p| p.plus(1));
                Span { start, end }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ident<'a> {
    pub lit: &'a str,
    pub lit_span: Span,
    pub text: &'a str,
    pub text_span: Span,
    pub kind: IdentKind,
}

impl<'a> Ident<'a> {
    pub fn from_plain_lit(lit: &'a str, span: Span) -> Self {
        Ident {
            lit,
            lit_span: span,
            text: lit,
            text_span: span,
            kind: IdentKind::Plain,
        }
    }

    pub fn text(&self) -> &str {
        self.text.as_ref()
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
    pub text_span: Span,
    pub quote: Quote,
}

impl<'a> StringVal<'a> {
    pub fn l_quote(&'a self) -> &'a str {
        let quote_end = self.text_span.start.char - self.lit_span.start.char;
        &self.lit[0..quote_end as usize]
    }

    /// Returns either the right quote or none if the string literal is unclosed.
    pub fn r_quote(&'a self) -> Option<&'a str> {
        let quote_len = self.lit_span.end.char - self.text_span.end.char;
        if quote_len == 0 {
            None
        } else {
            let quote_end = self.lit.len();
            let quote_start = quote_end - quote_len as usize;
            Some(&self.lit[quote_start..quote_end])
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
    pub assignments: Vec<InlineTableAssignment<'a>>,
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
    pub l_par: Pos,
    pub values: Vec<InlineArrayValue<'a>>,
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
    fn new(tokens: &'a Tokens<'a>) -> Self {
        Self {
            strings: &tokens.strings,
            literals: &tokens.literals,
            tokens: &tokens.tokens,
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
pub fn parse<'a>(ctx: &mut Ctx, tokens: &'a Tokens<'a>) -> Vec<Ast<'a>> {
    let mut parser = Parser::new(tokens);
    let mut asts = Vec::new();
    let mut newline_required = false;
    let mut prev_comments = Vec::new();

    'root: loop {
        if newline_required {
            while let Some(comment) = parser.eat_comment() {
                // comment on the same line as the last item
                match asts.last_mut() {
                    Some(Ast::Table(t)) => t.append_comment(comment),
                    Some(Ast::Array(a)) => a.append_comment(comment),
                    _ => todo!("append {comment:?} to last assignment"),
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

                let key = match parse_key(ctx, &mut parser) {
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

                match l_array_square {
                    Some(l_array_square) => {
                        let comments = find_associated_comments(
                            &mut asts,
                            &mut prev_comments,
                            l_table_square.start.line,
                        );
                        let header = ArrayHeader {
                            l_pars: (l_table_square.start, l_array_square.start),
                            key,
                            r_pars: (r_array_square, r_table_square),
                        };
                        asts.push(Ast::Array(ArrayEntry {
                            comments,
                            header,
                            assignments: Vec::new(),
                        }));
                    }
                    None => {
                        let associated_comments = find_associated_comments(
                            &mut asts,
                            &mut prev_comments,
                            l_table_square.start.line,
                        );
                        let header = TableHeader {
                            l_par: l_table_square.start,
                            key,
                            r_par: r_table_square,
                        };
                        asts.push(Ast::Table(Table {
                            comments: associated_comments,
                            header,
                            assignments: Vec::new(),
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

        let key = match parse_key(ctx, &mut parser) {
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

        let val = match parse_value(ctx, &mut parser) {
            Ok(v) => v,
            Err(e) => {
                ctx.error(e);
                recover_on!(parser, Newline | EOF => continue 'root);
            }
        };

        let assignment = Assignment { key, eq, val };
        match asts.last_mut() {
            Some(Ast::Table(t)) => t.assignments.push(assignment),
            Some(Ast::Array(a)) => a.assignments.push(assignment),
            _ => asts.push(Ast::Assignment(assignment)),
        }

        newline_required = true;
    }

    asts
}

fn find_associated_comments<'a>(
    asts: &mut Vec<Ast<'a>>,
    comments: &mut Vec<Comment<'a>>,
    mut line: u32,
) -> Vec<AssociatedComment<'a>> {
    let i = comments.iter().rev().position(|c| {
        let contigous = c.span.start.line + 1 == line;
        if contigous {
            line -= 1;
        }
        !contigous
    });
    if let Some(i) = i {
        for c in comments.drain(0..=i) {
            asts.push(Ast::Comment(c));
        }
    }
    comments
        .drain(..)
        .map(|comment| AssociatedComment {
            pos: AssociatedPos::Above,
            comment,
        })
        .collect()
}

fn parse_key<'a>(ctx: &mut Ctx, parser: &mut Parser<'a>) -> Result<Key<'a>, Error> {
    let mut idents = Vec::new();
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
                Ident {
                    lit_span: token.span,
                    lit: str.lit,
                    text: str.text.borrow(),
                    text_span: str.text_span,
                    kind,
                }
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
                    Ok(Key::Dotted(idents))
                };
            }
        }
    }
}

fn parse_value<'a>(ctx: &mut Ctx, parser: &mut Parser<'a>) -> Result<Value<'a>, Error> {
    let token = parser.peek();
    let value = match token.ty {
        TokenType::String(id) => {
            let token = parser.next();
            let str = parser.string(id);
            Value::String(StringVal {
                lit: str.lit,
                lit_span: token.span,
                text: str.text.borrow(),
                text_span: str.text_span,
                quote: str.quote,
            })
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

            let mut values = Vec::new();
            'inline_array: loop {
                while let Some(comment) = parser.eat_comment_and_newlines() {
                    todo!("store comment inside the array: {comment:?}");
                }

                if matches!(parser.peek().ty, TokenType::SquareRight | TokenType::EOF) {
                    break;
                }

                let val = match parse_value(ctx, parser) {
                    Ok(v) => v,
                    Err(e) => {
                        ctx.error(e);
                        recover_on!(parser,
                            Comma | Newline => continue 'inline_array,
                            SquareRight | EOF => break 'inline_array,
                        );
                    }
                };

                while let Some(comment) = parser.eat_comment_and_newlines() {
                    todo!("store comment inside the array: {comment:?}");
                }
                let comma = match parser.peek() {
                    t if t.ty == TokenType::Comma => Some(parser.next().span.start),
                    t if t.ty == TokenType::SquareRight || t.ty == TokenType::EOF => {
                        values.push(InlineArrayValue { val, comma: None });
                        break;
                    }
                    _ => {
                        ctx.error(Error::MissingComma(val.span().end));
                        // try to continue
                        None
                    }
                };

                values.push(InlineArrayValue { val, comma });
            }

            let r_par = match parser.peek() {
                t if t.ty == TokenType::SquareRight => Some(parser.next().span.start),
                t => {
                    let string = parser.token_to_fmt_str(t.ty);
                    ctx.error(Error::ExpectedRightSquareFound(string, t.span));
                    None
                }
            };

            Value::InlineArray(InlineArray {
                l_par,
                values,
                r_par,
            })
        }
        TokenType::CurlyLeft => {
            let l_par = token.span.start;
            parser.next();

            let mut assignments = Vec::new();
            let mut comma = None;
            'inline_table: loop {
                if matches!(parser.peek().ty, TokenType::CurlyRight | TokenType::EOF) {
                    if let Some(pos) = comma {
                        ctx.error(Error::InlineTableTrailingComma(pos));
                    }
                    break;
                }
                let key = match parse_key(ctx, parser) {
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

                let val = match parse_value(ctx, parser) {
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
                assignments,
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
