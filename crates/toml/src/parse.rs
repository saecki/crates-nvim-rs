use std::num::NonZeroU32;
use std::ops::ControlFlow;

use bumpalo::collections::Vec as BVec;
use bumpalo::Bump;
use common::{DiagnosticMark, FmtChar, FmtStr, Pos, Span};

use crate::datetime::{Date, DateTime};
use crate::lex::{LiteralId, StringId, StringToken, Token, TokenType, Tokens};
use crate::parse::lit::PartialValue;
use crate::{Error, Quote, TomlCtx};

pub use ast::*;
pub use lit::LitPart;
pub use num::{IntPrefix, Sign};

mod ast;
mod datetime;
mod lit;
mod num;
#[cfg(test)]
mod test;

pub const RECURSION_LIMIT: u16 = 100;
pub const START_FUEL: u8 = 8;

macro_rules! recover_on {
    ($parser:expr, $tokens:pat) => {{
        recover_on!($parser, $tokens => break)
    }};
    ($parser:expr, $($tokens:pat => $recover:stmt),+ $(,)?) => {{
        recover_on!($parser, __token, $($tokens => $recover),+);
    }};
    ($parser:expr, $token:ident, $($tokens:pat => $recover:stmt),+ $(,)?) => {{
        loop {
            use TokenType::*;
            let $token = $parser.peek();
            match $token.ty {
                $($tokens => {
                    #[allow(redundant_semicolons)]
                    $recover;
                })+
                #[allow(unreachable_patterns)]
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

macro_rules! unexpected_char {
    ($part:expr, $char:expr, $pos: expr) => {{
        use crate::parse::LitPart::*;
        Err(Error::UnexpectedLiteralChar($part, FmtChar($char), $pos))
    }};
}
use unexpected_char;

#[derive(Debug)]
struct Parser<'a> {
    strings: &'a [StringToken<'a>],
    literals: &'a [&'a str],
    tokens: &'a [Token],
    cursor: usize,
    eof: Token,
    newline_required: bool,
}

impl<'a> Parser<'a> {
    fn new(tokens: &Tokens<'a>) -> Self {
        Self {
            strings: tokens.strings,
            literals: tokens.literals,
            tokens: tokens.tokens,
            cursor: 0,
            eof: tokens.eof,
            newline_required: false,
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

    fn jump_to_end(&mut self) {
        self.cursor = self.tokens.len();
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
                    self.newline_required = false;
                    self.next();
                }
                _ => return None,
            }
        }
    }

    fn mark(&self) -> ParserMark {
        ParserMark {
            cursor: self.cursor as u32,
        }
    }

    fn reset(&mut self, mark: ParserMark) {
        self.cursor = mark.cursor as usize;
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

    fn token_fmt_str(&self, token: Token) -> FmtStr {
        match token.ty {
            TokenType::String(id) => {
                let string = &self.strings[id.0 as usize];
                FmtStr::from_string(format!("`{}`", FmtStr::from_str(string.lit)))
            }
            TokenType::LiteralOrIdent(id) => {
                let lit = self.literals[id.0 as usize];
                FmtStr::from_string(format!("`{}`", FmtStr::from_str(lit)))
            }
            TokenType::Comment(_) => FmtStr::from_str("comment"),
            TokenType::SquareLeft(_) => FmtStr::from_str("`[`"),
            TokenType::SquareRight => FmtStr::from_str("`]`"),
            TokenType::CurlyLeft(_) => FmtStr::from_str("`{`"),
            TokenType::CurlyRight => FmtStr::from_str("`{`"),
            TokenType::Equal => FmtStr::from_str("`=`"),
            TokenType::Comma => FmtStr::from_str("`,`"),
            TokenType::Dot => FmtStr::from_str("`.`"),
            TokenType::Newline => FmtStr::from_str("`\\n`"),
            TokenType::EOF => FmtStr::from_str("`EOF`"),
        }
    }

    fn token_span(&self, token: Token) -> Span {
        match token.ty {
            TokenType::String(id) => {
                let string = &self.strings[id.0 as usize];
                Span::new(token.start, string.lit_end)
            }
            TokenType::LiteralOrIdent(id) => {
                let lit = self.literals[id.0 as usize];
                Span::from_pos_len(token.start, lit.len() as u32)
            }
            TokenType::Comment(id) => {
                let lit = self.literals[id.0 as usize];
                Span::from_pos_len(token.start, 1 + lit.len() as u32)
            }
            TokenType::SquareLeft(_) => Span::ascii_char(token.start),
            TokenType::SquareRight => Span::ascii_char(token.start),
            TokenType::CurlyLeft(_) => Span::ascii_char(token.start),
            TokenType::CurlyRight => Span::ascii_char(token.start),
            TokenType::Equal => Span::ascii_char(token.start),
            TokenType::Comma => Span::ascii_char(token.start),
            TokenType::Dot => Span::ascii_char(token.start),
            TokenType::Newline => Span::pos(token.start),
            TokenType::EOF => Span::pos(token.start),
        }
    }

    fn token_fmt_str_and_span(&self, token: Token) -> (FmtStr, Span) {
        (self.token_fmt_str(token), self.token_span(token))
    }
}

#[derive(Clone, Copy)]
struct ParserMark {
    cursor: u32,
}

#[derive(Clone, Copy)]
struct Mark {
    ctx: DiagnosticMark,
    parser: ParserMark,
    comments: u32,
    values: u32,
}

fn mark<T>(
    ctx: &impl TomlCtx,
    parser: &Parser,
    comment_storage: &[AssocComment],
    values: &[T],
) -> Mark {
    Mark {
        ctx: ctx.mark(),
        parser: parser.mark(),
        comments: comment_storage.len() as u32,
        values: values.len() as u32,
    }
}

fn reset<T>(
    ctx: &mut impl TomlCtx,
    parser: &mut Parser,
    comment_storage: &mut Vec<AssocComment>,
    values: &mut Vec<T>,
    mark: Mark,
) {
    ctx.reset(mark.ctx);
    parser.reset(mark.parser);
    comment_storage.truncate(mark.comments as usize);
    values.truncate(mark.values as usize);
}

/// All errors are stored inside the [`Ctx`]. If an error is encountered this won't stop parsing
/// and will try to recover. If the [`Ctx`] contains no errors, the returned [`Asts`] are
/// completely valid, otherwise they might be incomplete or partially/completely invalid.
pub fn parse<'a>(ctx: &mut impl TomlCtx, bump: &'a Bump, tokens: &'_ Tokens<'a>) -> Asts<'a> {
    let mut parser = Parser::new(tokens);
    let mut asts = Vec::new();
    let mut comment_storage = Vec::new();
    let mut prev_comments = Vec::new();

    'root: loop {
        let token = parser.peek();
        match token.ty {
            TokenType::SquareLeft(_) => {
                let mark = ctx.mark();
                let l_table_square = parser.next().start;

                let l_array_square = match parser.peek() {
                    t if matches!(t.ty, TokenType::SquareLeft(_)) => {
                        parser.next();

                        if l_table_square.char + 1 != t.start.char {
                            let span = Span::new(l_table_square.plus(1), t.start);
                            ctx.error(Error::SpaceBetweenArrayPars(span));
                        }

                        Some(t.start)
                    }
                    _ => None,
                };

                let mut skip_brackets = false;
                let key = match parse_key(ctx, bump, &mut parser) {
                    KeyResult::Ok(k) => Some(k),
                    KeyResult::UnterminatedStr(k) => {
                        skip_brackets = true;
                        Some(k)
                    }
                    KeyResult::Err(e) => {
                        ctx.error(e);
                        recover_on!(parser, SquareRight | Newline | Comment(_) | EOF);
                        None
                    }
                };

                let mut r_array_square = None;
                let mut r_table_square = None;

                if !skip_brackets {
                    r_array_square = l_array_square.and_then(|l_par| match parser.peek() {
                        t if t.ty == TokenType::SquareRight => Some(parser.next().start),
                        t => {
                            let (string, span) = parser.token_fmt_str_and_span(t);
                            ctx.error(Error::ExpectedDotOrRightSquareFound(string, l_par, span));
                            None
                        }
                    });

                    r_table_square = match parser.peek() {
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
                            let err = if r_array_square.is_some() {
                                Error::ExpectedRightSquareFound
                            } else {
                                Error::ExpectedDotOrRightSquareFound
                            };
                            ctx.error(err(string, l_table_square, span));
                            None
                        }
                    };
                }

                if parser.newline_required {
                    if ctx.mark() == mark {
                        // continue if there is just a missing newline
                        ctx.error(Error::MissingNewline(token.start));
                    } else {
                        // avoid excessive error messages
                        ctx.reset(mark);
                        recover_on!(parser, Newline | Comment(_) | EOF);
                        let string = parser.token_fmt_str(token);
                        let end = parser.peek().start;
                        let span = Span::new(token.start, end);
                        ctx.error(Error::ExpectedNewlineFound(string, span));
                        continue 'root;
                    }
                }

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

                parser.newline_required = true;
            }
            TokenType::Comment(id) => {
                parser.next();
                let comment = parser.comment(id, token.start);
                if parser.newline_required {
                    let comment = AssocComment::line_end(0, comment);
                    let comment_id = store_comment(&mut comment_storage, comment);
                    match asts.last_mut() {
                        Some(Ast::Table(t)) => t.append_comment(comment_id),
                        Some(Ast::Array(a)) => a.append_comment(comment_id),
                        Some(Ast::Assignment(a)) => a.comments.append(comment_id),
                        Some(Ast::Comment(_)) | None => unreachable!(
                            "a comment has to be the last item in a line -> there can't be two comments in a line"
                        ),
                    }
                } else {
                    prev_comments.push(comment);
                }
            }
            TokenType::Newline => {
                parser.next();
                parser.newline_required = false;
            }
            TokenType::EOF => break 'root,
            _ => {
                parse_assignment(
                    ctx,
                    bump,
                    &mut parser,
                    &mut asts,
                    &mut comment_storage,
                    &mut prev_comments,
                );
            }
        }
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

enum KeyResult<'a> {
    Ok(Key<'a>),
    UnterminatedStr(Key<'a>),
    Err(Error),
}

fn parse_assignment<'a>(
    ctx: &mut impl TomlCtx,
    bump: &'a Bump,
    parser: &mut Parser<'a>,
    asts: &mut Vec<Ast<'a>>,
    comment_storage: &mut Vec<AssocComment<'a>>,
    prev_comments: &mut Vec<Comment<'a>>,
) {
    let token = parser.peek();
    let mark = ctx.mark();

    let key = match parse_key(ctx, bump, parser) {
        KeyResult::Ok(k) => k,
        KeyResult::UnterminatedStr(_) => {
            if parser.newline_required {
                // avoid excessive error messages
                ctx.reset(mark);
                let string = parser.token_fmt_str(token);
                let end = parser.peek().start;
                let span = Span::new(token.start, end);
                ctx.error(Error::ExpectedNewlineFound(string, span));
            }
            return;
        }
        KeyResult::Err(e) => {
            recover_on!(parser, Newline | Comment(_) | EOF);
            if parser.newline_required {
                // avoid excessive error messages
                ctx.reset(mark);
                let string = parser.token_fmt_str(token);
                let end = parser.peek().start;
                let span = Span::new(token.start, end);
                ctx.error(Error::ExpectedNewlineFound(string, span));
            } else {
                ctx.error(e);
            }
            return;
        }
    };

    let eq = match parser.peek() {
        t if t.ty == TokenType::Equal => {
            parser.next();
            t.start
        }
        t => {
            recover_on!(parser, Newline | Comment(_) | EOF);
            if parser.newline_required {
                // avoid excessive error messages
                ctx.reset(mark);
                let string = parser.token_fmt_str(token);
                let end = parser.peek().start;
                let span = Span::new(token.start, end);
                ctx.error(Error::ExpectedNewlineFound(string, span));
            } else {
                let (string, span) = parser.token_fmt_str_and_span(t);
                ctx.error(Error::ExpectedEqOrDotFound(string, span));
            }
            return;
        }
    };

    // Only generate missing newline error when a suffciently complete assignment has been parsed.
    if parser.newline_required {
        if ctx.mark() == mark {
            // continue if there is just a missing newline
            ctx.error(Error::MissingNewline(token.start));
        } else {
            // avoid excessive error messages
            ctx.reset(mark);
            recover_on!(parser, Newline | Comment(_) | EOF);
            let string = parser.token_fmt_str(token);
            let end = parser.peek().start;
            let span = Span::new(token.start, end);
            ctx.error(Error::ExpectedNewlineFound(string, span));
            return;
        }
    }

    // Store associated comments here so associated comments of the value are added in the correct order.
    let pos = find_associated_comments(&prev_comments, eq.line);
    let non_associated_comments = prev_comments.drain(..pos);
    let level = match asts.last_mut() {
        Some(Ast::Table(t)) => {
            add_comments(
                comment_storage,
                &mut t.comments,
                non_associated_comments,
                AssocPos::Contained,
            );
            1
        }
        Some(Ast::Array(a)) => {
            add_comments(
                comment_storage,
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
    let mut comments = store_comments(comment_storage, associated_comments, AssocPos::Above, level);

    parser.newline_required = true;
    let val = match parse_value(ctx, bump, parser, comment_storage, level) {
        Ok(v) => v,
        Err(e) => {
            ctx.error(e);
            parser.newline_required = false;
            recover_on!(parser, Newline | Comment(_) | EOF => return);
        }
    };

    // Include all associated comments of inner values.
    comments.extend_to(next_comment_id(&comment_storage));

    let assignment = Assignment { key, eq, val };
    let assignment = ToplevelAssignment {
        comments,
        assignment,
    };

    match asts.last_mut() {
        Some(Ast::Table(t)) => t.assignments.push(assignment),
        Some(Ast::Array(a)) => a.assignments.push(assignment),
        Some(Ast::Assignment(_) | Ast::Comment(_)) | None => asts.push(Ast::Assignment(assignment)),
    }
}

fn parse_key<'a>(ctx: &mut impl TomlCtx, bump: &'a Bump, parser: &mut Parser<'a>) -> KeyResult<'a> {
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
                        return KeyResult::Err(Error::MultilineBasicStringIdent(lit_span));
                    }
                    Quote::LiteralMultiline => {
                        return KeyResult::Err(Error::MultilineLiteralStringIdent(lit_span));
                    }
                };
                let ident = Ident::from_string(str.lit, lit_span, str.text, str.text_offset, kind);
                if str.text_offset.end_line == 0 && str.text_offset.end_char == 0 {
                    parser.next();
                    return KeyResult::UnterminatedStr(Key::One(ident));
                }
                ident
            }
            TokenType::LiteralOrIdent(id) => {
                let lit = parser.literal(id);
                let invalid_char = lit
                    .char_indices()
                    .find(|(_, c)| !matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'));

                if let Some((i, c)) = invalid_char {
                    let pos = token.start.plus(i as u32);
                    let error = Error::InvalidCharInIdentifier(FmtChar(c), pos);

                    if lit.chars().all(|c| matches!(c, '\x00'..='\x1f' | '\x7f')) {
                        return KeyResult::Err(error);
                    }

                    ctx.error(error);
                }

                let span = Span::from_pos_len(token.start, lit.len() as u32);
                Ident::from_plain_lit(lit, span)
            }
            TokenType::Comment(_)
            | TokenType::SquareLeft(_)
            | TokenType::SquareRight
            | TokenType::CurlyLeft(_)
            | TokenType::CurlyRight
            | TokenType::Equal
            | TokenType::Comma
            | TokenType::Dot
            | TokenType::Newline
            | TokenType::EOF => {
                let (string, span) = parser.token_fmt_str_and_span(token);
                return KeyResult::Err(Error::ExpectedKeyFound(string, span));
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
                    KeyResult::Ok(Key::One(ident))
                } else {
                    idents.push(DottedIdent { ident, dot: None });
                    KeyResult::Ok(Key::Dotted(idents.into_bump_slice()))
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
            let (lit, span) = combine_adjacent_dot_and_lit(parser, lit, span);

            match lit::parse_literal(lit, span) {
                Ok(PartialValue::Float(f)) => Value::Float(FloatVal::new(lit, span, f)),
                Ok(PartialValue::Int(i)) => Value::Int(IntVal::new(lit, span, i)),
                Ok(PartialValue::Bool(b)) => Value::Bool(BoolVal::new(span, b)),
                Ok(PartialValue::DateTime(d)) => Value::DateTime(DateTimeVal::new(lit, span, d)),
                Ok(PartialValue::PartialDate(date)) => {
                    try_to_parse_time_part(ctx, parser, lit, span, date)
                }
                Ok(PartialValue::InvalidDateTime(e)) => {
                    ctx.error(e);
                    if !lit.contains(['T', 't', ':']) {
                        try_combine_time_part(parser, lit, span)
                    } else {
                        Value::Invalid(lit, span)
                    }
                }
                Err(e) => {
                    ctx.error(e);
                    Value::Invalid(lit, span)
                }
            }
        }
        TokenType::Dot => {
            let token = parser.next();

            ctx.error(Error::UnexpectedLiteralStart(FmtChar('.'), token.start));

            let lit;
            let span;
            let t = parser.peek();
            let dot_end = token.start.plus(1);
            match t.ty {
                TokenType::LiteralOrIdent(id) if t.start == dot_end => {
                    parser.next();
                    let l = parser.literal(id);
                    span = Span::from_pos_len(token.start, l.len() as u32 + 1);
                    // SAFETY: we know there is a dot directly before the literal
                    lit = unsafe { lit::extend_str_front(l, 1) };
                }
                _ => {
                    let (string, span) = parser.token_fmt_str_and_span(token);
                    return Err(Error::ExpectedValueFound(string, span));
                }
            }
            let (lit, span) = combine_adjacent_dot_and_lit(parser, lit, span);

            Value::Invalid(lit, span)
        }
        // TODO: Possibly pass down information when parsing stopped to avoid the same error recovery
        // and rewind process in nested inline arrays/tables.
        TokenType::SquareLeft(close) => {
            let array = parse_inline_array(ctx, bump, parser, comment_storage, level, close)?;
            Value::InlineArray(array)
        }
        TokenType::CurlyLeft(close) => {
            let table = parse_inline_table(ctx, bump, parser, comment_storage, level, close)?;
            Value::InlineTable(table)
        }
        TokenType::Comment(_)
        | TokenType::SquareRight
        | TokenType::CurlyRight
        | TokenType::Equal
        | TokenType::Comma
        | TokenType::Newline
        | TokenType::EOF => {
            let (string, span) = parser.token_fmt_str_and_span(token);
            return Err(Error::ExpectedValueFound(string, span));
        }
    };

    Ok(value)
}

fn parse_inline_array<'a>(
    ctx: &mut impl TomlCtx,
    bump: &'a Bump,
    parser: &mut Parser<'a>,
    comment_storage: &mut Vec<AssocComment<'a>>,
    level: u16,
    close: Option<NonZeroU32>,
) -> Result<InlineArray<'a>, Error> {
    let token = parser.next();
    let l_par = token.start;

    if level >= RECURSION_LIMIT {
        parser.jump_to_end();
        return Err(Error::RecursionLimitExceeded(l_par));
    }

    let mut values: Vec<InlineArrayValue<'_>> = Vec::new();
    let mut array_comments = CommentRange::new(next_comment_id(comment_storage), 0, level);
    let mut fuel = START_FUEL;
    let mut valid_mark = mark(ctx, parser, comment_storage, &values);
    'inline_array: loop {
        while let Some(comment) = parser.eat_comment_and_newlines() {
            // Avoid extending the comment range so it doesn't have to be reset when the array is
            // unclosed and too many errors are encountered.
            _ = store_comment(comment_storage, AssocComment::contained(level, comment));
        }

        if one_of!(parser.peek().ty, SquareRight | EOF) {
            break 'inline_array;
        }

        let val = match parse_value(ctx, bump, parser, comment_storage, level + 1) {
            Ok(v) => v,
            Err(e @ Error::RecursionLimitExceeded(_)) => return Err(e),
            Err(e) => {
                ctx.error(e);
                fuel = fuel.saturating_sub(1);

                recover_on!(parser,
                    Comma | Newline | Comment(_) => {
                        parser.next();
                        continue 'inline_array;
                    },
                    SquareRight | EOF => break 'inline_array,
                    t => {
                        fuel = match t {
                            TokenType::Equal => fuel.saturating_sub(8),
                            _ => fuel.saturating_sub(1),
                        };
                        if fuel == 0 && close.is_none() {
                            reset(ctx, parser, comment_storage, &mut values, valid_mark);
                            break 'inline_array;
                        }
                        parser.next();
                    }
                );
            }
        };

        // Only generate missing comma error once another value is found. This avoids missing
        // comma errors for unclosed inline-arrays
        if let Some(prev) = values.last() {
            if prev.comma.is_none() {
                ctx.error(Error::MissingComma(prev.end()));
            }
        }

        let val_line = val.start().line;
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
            _ = store_comment(comment_storage, AssocComment::contained(level, comment));
        }

        let token = parser.peek();
        let comma = match token.ty {
            TokenType::Comma => {
                parser.next();
                val_comments.extend_to(next_comment_id(comment_storage));
                mark_contained_comments(comment_storage, &val_comments, level + 1);

                if let Some(comment) = parser.eat_comment() {
                    add_comment(
                        comment_storage,
                        &mut val_comments,
                        comment,
                        AssocPos::LineEnd,
                    );
                }

                Some(token.start)
            }
            TokenType::SquareRight | TokenType::EOF => {
                values.push(InlineArrayValue {
                    comments: val_comments,
                    val,
                    comma: None,
                });
                break 'inline_array;
            }
            _ => {
                // Missing a comma, continue for now...
                fuel = fuel.saturating_sub(1);
                None
            }
        };

        let is_valid = val.is_valid();
        values.push(InlineArrayValue {
            comments: val_comments,
            val,
            comma,
        });

        if is_valid {
            fuel = u8::max(fuel + 1, START_FUEL);
            valid_mark = mark(ctx, parser, comment_storage, &values);
        }
    }

    let r_par = match parser.peek() {
        t if t.ty == TokenType::SquareRight => Some(parser.next().start),
        t => {
            let (string, span) = parser.token_fmt_str_and_span(t);
            ctx.error(Error::ExpectedRightSquareFound(string, l_par, span));
            None
        }
    };

    array_comments.extend_to(next_comment_id(comment_storage));
    mark_contained_comments(comment_storage, &array_comments, level);

    let end = match r_par {
        Some(p) => End::Par(p),
        None => {
            let end = values
                .last()
                .map(|v| v.end())
                .unwrap_or_else(|| l_par.plus(1));
            End::None(end)
        }
    };
    Ok(InlineArray {
        comments: array_comments,
        l_par,
        values: bump.alloc_slice_fill_iter(values),
        end,
    })
}

fn parse_inline_table<'a>(
    ctx: &mut impl TomlCtx,
    bump: &'a Bump,
    parser: &mut Parser<'a>,
    comment_storage: &mut Vec<AssocComment<'a>>,
    level: u16,
    close: Option<NonZeroU32>,
) -> Result<InlineTable<'a>, Error> {
    let token = parser.next();
    let l_par = token.start;

    if level >= RECURSION_LIMIT {
        parser.jump_to_end();
        return Err(Error::RecursionLimitExceeded(l_par));
    }

    let mut assignments = Vec::new();
    let mut table_comments = CommentRange::new(next_comment_id(comment_storage), 0, level);
    let mut fuel = START_FUEL;
    let mut valid_mark = mark(ctx, parser, comment_storage, &assignments);
    'inline_table: loop {
        while let Some(comment) = parser.eat_comment_and_newlines() {
            // Avoid extending the comment range so it doesn't have to be reset when the array is
            // unclosed and too many errors are encountered.
            _ = store_comment(comment_storage, AssocComment::contained(level, comment));
        }

        if one_of!(parser.peek().ty, CurlyRight | EOF) {
            let prev = parser
                .peek_prev()
                .expect("there must be at least the opening brace");
            if prev.ty == TokenType::Comma {
                ctx.error(Error::InlineTableTrailingComma(prev.start));
            }
            break 'inline_table;
        }

        let key = match parse_key(ctx, bump, parser) {
            KeyResult::Ok(k) => k,
            KeyResult::UnterminatedStr(_) => {
                // TODO:
                // No token other than newline can come after an unterminated string literal
                break 'inline_table;
            }
            KeyResult::Err(e @ Error::RecursionLimitExceeded(_)) => return Err(e),
            KeyResult::Err(e) => {
                ctx.error(e);
                let cf = recover_inline_table(
                    ctx,
                    parser,
                    comment_storage,
                    &mut assignments,
                    valid_mark,
                    close,
                    &mut fuel,
                );
                match cf {
                    ControlFlow::Continue(_) => continue 'inline_table,
                    ControlFlow::Break(_) => break 'inline_table,
                }
            }
        };

        // Only generate missing comma error once another assignment is found. This avoids missing
        // comma errors for unclosed inline-tables.
        if let Some(prev) = assignments.last() {
            if prev.comma.is_none() {
                ctx.error(Error::MissingComma(prev.end()));
            }
        }

        let eq = match parser.peek() {
            t if t.ty == TokenType::Equal => parser.next().start,
            t => {
                let (string, span) = parser.token_fmt_str_and_span(t);
                ctx.error(Error::ExpectedEqOrDotFound(string, span));
                let cf = recover_inline_table(
                    ctx,
                    parser,
                    comment_storage,
                    &mut assignments,
                    valid_mark,
                    close,
                    &mut fuel,
                );
                match cf {
                    ControlFlow::Continue(_) => continue 'inline_table,
                    ControlFlow::Break(_) => break 'inline_table,
                }
            }
        };

        let val = match parse_value(ctx, bump, parser, comment_storage, level + 1) {
            Ok(v) => v,
            Err(e @ Error::RecursionLimitExceeded(_)) => return Err(e),
            Err(e) => {
                ctx.error(e);
                let cf = recover_inline_table(
                    ctx,
                    parser,
                    comment_storage,
                    &mut assignments,
                    valid_mark,
                    close,
                    &mut fuel,
                );
                match cf {
                    ControlFlow::Continue(_) => continue 'inline_table,
                    ControlFlow::Break(_) => break 'inline_table,
                }
            }
        };

        let assignment = Assignment { key, eq, val };
        let comma = match parser.peek() {
            t if t.ty == TokenType::Comma => Some(parser.next().start),
            t if one_of!(t.ty, CurlyRight | EOF) => {
                assignments.push(InlineTableAssignment {
                    assignment,
                    comma: None,
                });
                break 'inline_table;
            }
            _ => {
                // Missing a comma, continue, for now..
                fuel = fuel.saturating_sub(1);
                None
            }
        };

        let is_valid = assignment.val.is_valid();
        assignments.push(InlineTableAssignment { assignment, comma });

        if is_valid {
            fuel = u8::max(fuel + 1, START_FUEL);
            valid_mark = mark(ctx, parser, comment_storage, &assignments);
        }
    }

    let r_par = match parser.peek() {
        t if t.ty == TokenType::CurlyRight => Some(parser.next().start),
        t => {
            let (string, span) = parser.token_fmt_str_and_span(t);
            ctx.error(Error::ExpectedRightCurlyFound(string, l_par, span));
            None
        }
    };

    table_comments.extend_to(next_comment_id(comment_storage));
    mark_contained_comments(comment_storage, &table_comments, level);

    let end = match r_par {
        Some(p) => End::Par(p),
        None => {
            let end = assignments
                .last()
                .map(|a| a.end())
                .unwrap_or_else(|| l_par.plus(1));
            End::None(end)
        }
    };

    Ok(InlineTable {
        l_par,
        assignments: bump.alloc_slice_fill_iter(assignments),
        end,
    })
}

fn recover_inline_table(
    ctx: &mut impl TomlCtx,
    parser: &mut Parser<'_>,
    comment_storage: &mut Vec<AssocComment<'_>>,
    assignments: &mut Vec<InlineTableAssignment<'_>>,
    valid_mark: Mark,
    close: Option<NonZeroU32>,
    fuel: &mut u8,
) -> ControlFlow<()> {
    recover_on!(parser, token,
        Comma => {
            parser.next();
            return ControlFlow::Continue(());
        },
        CurlyRight | EOF => {
            return ControlFlow::Break(());
        },
        t => {
            *fuel = match t {
                TokenType::Newline => {
                    ctx.error(Error::InlineTableNewline(token.start));
                    fuel.saturating_sub(1)
                }
                _ => fuel.saturating_sub(1),
            };
            if *fuel == 0 && close.is_none() {
                reset(ctx, parser, comment_storage, assignments, valid_mark);
                return ControlFlow::Break(());
            }
            parser.next();
        },
    );
}

/// toml permits using spaces instead of `T` to separate date and time in an rfc3339
/// timestamp, if the previous token just contained the date then check if the next token
/// contains the time.
fn try_to_parse_time_part<'a>(
    ctx: &mut impl TomlCtx,
    parser: &mut Parser<'a>,
    date_lit: &'a str,
    date_span: Span,
    date: Date,
) -> Value<'a> {
    let time_lit;
    let time_span;
    match parser.peek().ty {
        TokenType::LiteralOrIdent(id) => {
            let token = parser.next();
            let lit = parser.literal(id);
            let span = Span::from_pos_len(token.start, lit.len() as u32);
            (time_lit, time_span) = combine_adjacent_dot_and_lit(parser, lit, span)
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
    let lit = unsafe { lit::concat_strs(date_lit, time_lit) };
    let span = Span::across(date_span, time_span);

    let mut chars = time_lit.char_indices().peekable();
    let (time, offset) = match datetime::parse_time_and_offset(&mut chars, time_span) {
        Ok(v) => v,
        Err(e) => {
            ctx.error(e);
            return Value::Invalid(lit, span);
        }
    };

    let val = DateTime::from_optional_offset(date, time, offset);
    let date_time = DateTimeVal::new(lit, span, val);
    Value::DateTime(date_time)
}

fn try_combine_time_part<'a>(
    parser: &mut Parser<'a>,
    date_lit: &'a str,
    date_span: Span,
) -> Value<'a> {
    let time_lit;
    let time_span;
    let time = parser.peek();
    match time.ty {
        TokenType::LiteralOrIdent(id) => {
            time_lit = parser.literal(id);
            time_span = Span::from_pos_len(time.start, time_lit.len() as u32);
        }
        _ => return Value::Invalid(date_lit, date_span),
    }

    // only assum these literals belong together if they are reasonably close together
    if time_span.start.char > date_span.end.char + 5 {
        return Value::Invalid(date_lit, date_span);
    }
    parser.next();

    // SAFETY: the first and second literal reference the same string, are on the same line and
    // are only separated by whitespace. See above.
    let lit = unsafe { lit::concat_strs(date_lit, time_lit) };
    let span = Span::across(date_span, time_span);
    let (lit, span) = combine_adjacent_dot_and_lit(parser, lit, span);

    Value::Invalid(lit, span)
}

fn combine_adjacent_dot_and_lit<'a>(
    parser: &mut Parser<'a>,
    mut prev_lit: &'a str,
    mut prev_span: Span,
) -> (&'a str, Span) {
    loop {
        let t = parser.peek();
        if t.start != prev_span.end {
            return (prev_lit, prev_span);
        }

        match t.ty {
            TokenType::Dot => {
                // SAFETY: we know there is a dot directly after prev_lit
                prev_lit = unsafe { lit::extend_str_back(prev_lit, 1) };
                prev_span.end.char += 1;
            }
            TokenType::LiteralOrIdent(id) => {
                let next_lit = parser.literal(id);
                // SAFETY: the first and second literal are directly adjacent
                prev_lit = unsafe { lit::concat_strs(prev_lit, next_lit) };
                prev_span.end.char += next_lit.len() as u32;
            }
            _ => return (prev_lit, prev_span),
        }
        parser.next();
    }
}
