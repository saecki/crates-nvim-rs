use std::borrow::Cow;

use crate::datetime::{Date, DateTime, Time};
use crate::lex::{LiteralId, StringId, StringToken, Token, TokenType, Tokens};
use crate::{Ctx, Error, Pos, Quote, Span};

mod datetime;
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

// TODO: cursor to peek multiple tokens ahead and revert
// -> use for heuristics to detect unclosed inline arrays
#[derive(Debug)]
struct Parser<'a> {
    strings: Vec<StringToken<'a>>,
    literals: Vec<&'a str>,
    tokens: std::iter::Peekable<std::vec::IntoIter<Token>>,
    eof: Token,
}

impl<'a> Parser<'a> {
    fn new(tokens: Tokens<'a>) -> Self {
        Self {
            strings: tokens.strings,
            literals: tokens.literals,
            tokens: tokens.tokens.into_iter().peekable(),
            eof: tokens.eof,
        }
    }

    fn next(&mut self) -> Token {
        self.tokens.next().unwrap_or(self.eof)
    }

    fn peek(&mut self) -> Token {
        match self.tokens.peek() {
            Some(t) => *t,
            None => self.eof,
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

    fn string_mut(&mut self, id: StringId) -> &mut StringToken<'a> {
        &mut self.strings[id.0 as usize]
    }

    fn literal(&self, id: LiteralId) -> &'a str {
        self.literals[id.0 as usize]
    }

    fn token_to_string(&self, ty: TokenType) -> String {
        let mut string = String::new();
        _ = ty.display(&mut string, &self.strings, &self.literals);
        string
    }
}

#[derive(Debug, PartialEq)]
pub enum Ast<'a> {
    Assignment(Assignment<'a>),
    Table(Table<'a>),
    Array(ArrayEntry<'a>),
    Comment(Comment<'a>),
}

#[derive(Debug, PartialEq)]
pub struct Table<'a> {
    pub header: TableHeader<'a>,
    pub assignments: Vec<Assignment<'a>>,
}

impl Table<'_> {
    #[inline]
    pub fn span(&self) -> Span {
        let header_span = self.header.span();
        let start = header_span.start;
        let end = (self.assignments.last())
            .map(|a| a.val.span().end)
            .unwrap_or(header_span.end);
        Span { start, end }
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
    pub header: ArrayHeader<'a>,
    pub assignments: Vec<Assignment<'a>>,
}

impl ArrayEntry<'_> {
    #[inline]
    pub fn span(&self) -> Span {
        let header_span = self.header.span();
        let start = header_span.start;
        let end = (self.assignments.last())
            .map(|a| a.val.span().end)
            .unwrap_or(header_span.end);
        Span { start, end }
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
    pub text: Cow<'a, str>,
    pub text_span: Span,
    pub kind: IdentKind,
}

impl<'a> Ident<'a> {
    pub fn from_plain_lit(lit: &'a str, span: Span) -> Self {
        Ident {
            lit,
            lit_span: span,
            text: Cow::Borrowed(lit),
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
    pub text: Cow<'a, str>,
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
        let end = self
            .r_par
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
        let end = self
            .r_par
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

#[derive(Debug, PartialEq, Eq)]
pub struct Comment<'a> {
    pub span: Span,
    pub text: &'a str,
}

enum Header<'a> {
    Table(Table<'a>),
    Array(ArrayEntry<'a>),
}

impl<'a> Header<'a> {
    fn into_ast(self) -> Ast<'a> {
        match self {
            Header::Table(t) => Ast::Table(t),
            Header::Array(a) => Ast::Array(a),
        }
    }
}

impl Ctx {
    /// All errors are stored inside the [`Ctx`]. If a fatal error occurs, a unit error
    /// is returned, otherwise the possibly partially invalid ast is returned.
    pub fn parse<'a>(&mut self, tokens: Tokens<'a>) -> Vec<Ast<'a>> {
        let mut parser = Parser::new(tokens);
        let mut asts = Vec::new();
        let mut last_header = None;
        let mut newline_required = false;

        'root: loop {
            if newline_required {
                while let Some(comment) = parser.eat_comment() {
                    asts.push(Ast::Comment(comment));
                }
                match parser.peek() {
                    t if t.ty == TokenType::Newline => {
                        parser.next();
                    }
                    t if t.ty == TokenType::EOF => break 'root,
                    t => {
                        self.error(Error::MissingNewline(t.span.start));
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

                    let key = match self.parse_key(&mut parser) {
                        Ok(k) => Some(k),
                        Err(e) => {
                            // TODO: push some sort of unused hint for the body of the table
                            self.error(e);
                            loop {
                                match parser.peek().ty {
                                    TokenType::SquareRight
                                    | TokenType::Newline
                                    | TokenType::EOF => break,
                                    _ => {
                                        parser.next();
                                    }
                                }
                            }
                            None
                        }
                    };

                    let r_array_square = l_array_square.and_then(|_| match parser.peek() {
                        t if t.ty == TokenType::SquareRight => Some(parser.next().span.start),
                        t => {
                            let string = parser.token_to_string(t.ty);
                            self.error(Error::ExpectedRightSquareFound(string, t.span));
                            None
                        }
                    });

                    let r_table_square = match parser.peek() {
                        t if t.ty == TokenType::SquareRight => Some(parser.next().span.start),
                        t => {
                            let string = parser.token_to_string(t.ty);
                            self.error(Error::ExpectedRightSquareFound(string, t.span));
                            None
                        }
                    };

                    let header = match l_array_square {
                        Some(l_array_square) => {
                            let header = ArrayHeader {
                                l_pars: (l_table_square.start, l_array_square.start),
                                key,
                                r_pars: (r_array_square, r_table_square),
                            };
                            Header::Array(ArrayEntry {
                                header,
                                assignments: Vec::new(),
                            })
                        }
                        None => {
                            let header = TableHeader {
                                l_par: l_table_square.start,
                                key,
                                r_par: r_table_square,
                            };
                            Header::Table(Table {
                                header,
                                assignments: Vec::new(),
                            })
                        }
                    };

                    match &mut last_header {
                        Some(last) => {
                            let last = std::mem::replace(last, header);
                            asts.push(last.into_ast());
                        }
                        None => last_header = Some(header),
                    }

                    newline_required = true;
                    continue;
                }
                TokenType::Comment(id) => {
                    let text = parser.literal(id);
                    asts.push(Ast::Comment(Comment {
                        span: token.span,
                        text,
                    }));
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

            let key = match self.parse_key(&mut parser) {
                Ok(k) => k,
                Err(e) => {
                    self.error(e);
                    recover_on!(parser, Newline | EOF => continue 'root);
                }
            };

            let eq = match parser.next() {
                t if t.ty == TokenType::Equal => t.span.start,
                t => {
                    let string = parser.token_to_string(t.ty);
                    self.error(Error::ExpectedEqFound(string, t.span));
                    recover_on!(parser, Newline | EOF => continue 'root);
                }
            };

            let val = match self.parse_value(&mut parser) {
                Ok(v) => v,
                Err(e) => {
                    self.error(e);
                    recover_on!(parser, Newline | EOF => continue 'root);
                }
            };

            let assignment = Assignment { key, eq, val };
            match &mut last_header {
                Some(Header::Table(t)) => t.assignments.push(assignment),
                Some(Header::Array(a)) => a.assignments.push(assignment),
                None => asts.push(Ast::Assignment(assignment)),
            }

            newline_required = true;
        }

        if let Some(last) = last_header {
            asts.push(last.into_ast());
        }

        asts
    }

    fn parse_key<'a>(&mut self, parser: &mut Parser<'a>) -> Result<Key<'a>, Error> {
        let mut idents = Vec::new();
        loop {
            let token = parser.peek();
            let ident = match token.ty {
                TokenType::String(id) => {
                    let str = parser.string_mut(id);
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
                        text: std::mem::take(&mut str.text),
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
                        self.error(Error::InvalidCharInIdentifier(c, pos));
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
                    let string = parser.token_to_string(token.ty);
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
                    if idents.is_empty() {
                        return Ok(Key::One(ident));
                    } else {
                        idents.push(DottedIdent { ident, dot: None });
                        return Ok(Key::Dotted(idents));
                    }
                }
            }
        }
    }

    fn parse_value<'a>(&mut self, parser: &mut Parser<'a>) -> Result<Value<'a>, Error> {
        let token = parser.peek();
        let value = match token.ty {
            TokenType::String(id) => {
                let token = parser.next();
                let str = parser.string_mut(id);
                Value::String(StringVal {
                    lit: str.lit,
                    lit_span: token.span,
                    text: std::mem::take(&mut str.text),
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
                    _ => match parse_num_or_date(lit, token.span) {
                        Ok(PartialValue::PrefixedInt(i)) => {
                            Value::Int(IntVal::new(lit, token.span, i))
                        }
                        Ok(PartialValue::Int(i)) => self.try_to_parse_fractional_part_of_float(
                            parser,
                            lit,
                            token.span,
                            Some(i),
                        ),
                        Ok(PartialValue::OverflowOrFloat) => self
                            .try_to_parse_fractional_part_of_float(parser, lit, token.span, None),
                        Ok(PartialValue::FloatWithExp) => match lit.replace('_', "").parse() {
                            Ok(v) => Value::Float(FloatVal::new(lit, token.span, v)),
                            Err(_) => {
                                self.error(Error::FloatLiteralOverflow(token.span));
                                Value::Invalid(lit, token.span)
                            }
                        },
                        Ok(PartialValue::OffsetDateTime(val)) => {
                            let date_time = DateTimeVal::new(lit, token.span, val);
                            Value::DateTime(date_time)
                        }
                        Ok(PartialValue::PartialDate(date)) => {
                            self.try_to_parse_time_part(parser, lit, token.span, date)
                        }
                        Ok(PartialValue::PartialDateTime(date, time)) => {
                            self.try_to_parse_subsecs(parser, lit, token.span, Some(date), time)
                        }
                        Ok(PartialValue::PartialTime(time)) => {
                            self.try_to_parse_subsecs(parser, lit, token.span, None, time)
                        }
                        Err(e) => {
                            self.error(e);
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
                    if matches!(parser.peek().ty, TokenType::SquareRight | TokenType::EOF) {
                        break;
                    }

                    while let Some(comment) = parser.eat_comment_and_newlines() {
                        todo!("store comment inside the array: {comment:?}");
                    }
                    let val = match self.parse_value(parser) {
                        Ok(v) => v,
                        Err(e) => {
                            self.error(e);
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
                            self.error(Error::MissingComma(val.span().end));
                            // try to continue
                            None
                        }
                    };

                    while let Some(comment) = parser.eat_comment_and_newlines() {
                        todo!("store comment inside the array: {comment:?}");
                    }

                    values.push(InlineArrayValue { val, comma });
                }

                let r_par = match parser.peek() {
                    t if t.ty == TokenType::SquareRight => Some(parser.next().span.start),
                    t => {
                        let string = parser.token_to_string(t.ty);
                        self.error(Error::ExpectedRightSquareFound(string, t.span));
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
                'inline_table: loop {
                    if matches!(parser.peek().ty, TokenType::CurlyRight | TokenType::EOF) {
                        break;
                    }
                    let key = match self.parse_key(parser) {
                        Ok(k) => k,
                        Err(e) => {
                            self.error(e);
                            recover_on!(parser,
                                Comma => continue 'inline_table,
                                Newline | CurlyRight | EOF => break 'inline_table,
                            )
                        }
                    };

                    let eq = match parser.peek() {
                        t if t.ty == TokenType::Equal => parser.next().span.start,
                        t => {
                            let string = parser.token_to_string(t.ty);
                            self.error(Error::ExpectedEqFound(string, t.span));
                            recover_on!(parser,
                                Comma => continue 'inline_table,
                                Newline | CurlyRight | EOF => break 'inline_table,
                            )
                        }
                    };

                    let val = match self.parse_value(parser) {
                        Ok(v) => v,
                        Err(e) => {
                            self.error(e);
                            recover_on!(parser,
                                Comma => continue 'inline_table,
                                Newline | CurlyRight | EOF => break 'inline_table,
                            )
                        }
                    };

                    let mut assignment = InlineTableAssignment {
                        assignment: Assignment { key, eq, val },
                        comma: None,
                    };
                    match parser.peek() {
                        t if t.ty == TokenType::Comma => {
                            assignment.comma = Some(parser.next().span.start);
                        }
                        t if t.ty == TokenType::CurlyRight || t.ty == TokenType::EOF => {
                            assignments.push(assignment);
                            break;
                        }
                        _ => {
                            let pos = assignment.assignment.val.span().end;
                            self.error(Error::MissingComma(pos));
                            // try to continue
                        }
                    };

                    assignments.push(assignment);
                }

                let r_par = match parser.peek() {
                    t if t.ty == TokenType::CurlyRight => Some(parser.next().span.start),
                    t => {
                        let string = parser.token_to_string(t.ty);
                        self.error(Error::ExpectedRightCurlyFound(string, t.span));
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
                let string = parser.token_to_string(token.ty);
                return Err(Error::ExpectedValueFound(string, token.span));
            }
        };

        Ok(value)
    }

    fn try_to_parse_fractional_part_of_float<'a>(
        &mut self,
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
                    self.error(Error::IntLiteralOverflow(int_span));
                    return Value::Invalid(int_lit, int_span);
                }
            },
        }

        let mut missing_float_fractional_part_error = || {
            let pos = int_span.end.plus(1);
            self.error(Error::MissingFloatFractionalPart(pos));

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
        if let Err(e) = validate_float_fractional_part(frac_lit, frac.span) {
            self.error(e);
            return Value::Invalid(lit, span);
        }

        let Ok(val) = lit.replace('_', "").parse() else {
            self.error(Error::FloatLiteralOverflow(span));
            return Value::Invalid(lit, span);
        };

        Value::Float(FloatVal::new(lit, span, val))
    }

    /// toml permits using spaces instead of `T` to separate date and time in and rfc3339
    /// timestamp, if the previous token just contained the date then check if the next token
    /// contains the time.
    fn try_to_parse_time_part<'a>(
        &mut self,
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
            self.error(Error::DateAndTimeTooFarApart(span));
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
                self.error(e);
                return Value::Invalid(lit, span);
            }
        };

        if let Some(offset) = offset {
            let val = DateTime::OffsetDateTime(date, time, offset);
            let date_time = DateTimeVal::new(lit, span, val);
            return Value::DateTime(date_time);
        }

        self.try_to_parse_subsecs(parser, lit, span, Some(date), time)
    }

    fn try_to_parse_subsecs<'a>(
        &mut self,
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
            self.error(Error::DateTimeMissingSubsec(pos));

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
        match parse_date_time_subsec_part(lit, span, subsec_lit, subsec.span, date, time) {
            Ok(date_time) => Value::DateTime(date_time),
            Err(e) => {
                self.error(e);
                Value::Invalid(lit, span)
            }
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

/// Parse all integers adhering to the toml spec, the integer part of a float and it's exponent or
/// a date-time adhering to the RFC 3339 spec. The toml spec allows using a space instead of `T` to
/// separate the date and time parts, in that case only the date is parsed since the time part is
/// inside the next token.
fn parse_num_or_date(literal: &str, span: Span) -> Result<PartialValue, Error> {
    #[derive(PartialEq, Eq)]
    enum NumParseState {
        Int,
        OverflowOrFloat,
    }

    let mut chars = literal.char_indices().peekable();
    let c = match chars.next() {
        None => unreachable!("value literal should never be emtpy"),
        Some((_, c)) => c,
    };

    let mut parse_state = NumParseState::Int;
    let mut int_accum;

    // TODO: support signs
    // TODO: better errors for uppercase radices
    match c {
        '0' => match chars.next() {
            Some((_, 'b')) => {
                let val = parse_prefixed_int_literal(IntBits::Binary, chars, span)?;
                return Ok(PartialValue::PrefixedInt(val));
            }
            Some((_, 'o')) => {
                let val = parse_prefixed_int_literal(IntBits::Octal, chars, span)?;
                return Ok(PartialValue::PrefixedInt(val));
            }
            Some((_, 'x')) => {
                let val = parse_prefixed_int_literal(IntBits::Hexadecimal, chars, span)?;
                return Ok(PartialValue::PrefixedInt(val));
            }
            Some((_, c @ ('0'..='9'))) => {
                let two_digits = c as u16 - '0' as u16;
                return datetime::continue_parsing_date_time(&mut chars, span, two_digits);
            }
            Some((i, radix)) => {
                return Err(Error::InvalidIntRadix(radix, span.start.plus(i as u32)));
            }
            None => {
                return Ok(PartialValue::Int(0));
            }
        },
        '1'..='9' => {
            int_accum = (c as u32 - '0' as u32) as i64;
        }
        '_' => return Err(Error::NumOrDateLiteralStartsWithUnderscore(span.start)),
        _ => return Err(Error::InvalidNumOrDateLiteralStart(c, span.start)),
    }

    let mut last_underscore = false;
    loop {
        let Some((i, c)) = chars.next() else {
            break;
        };

        last_underscore = false;

        // TODO: better errors for hex digits
        match c {
            '0'..='9' => {
                match parse_state {
                    // the literal seems to be an integer
                    NumParseState::Int => {
                        let digit = (c as u32) - ('0' as u32);
                        let (val, overflow) = int_accum.overflowing_mul(10);
                        if overflow {
                            parse_state = NumParseState::OverflowOrFloat;
                        } else {
                            int_accum = val;
                            int_accum += digit as i64;
                        }
                    }
                    // The literal would overflow if it was an int, but it could be a float.
                    NumParseState::OverflowOrFloat => {}
                }
            }
            'e' | 'E' => {
                if let Some((_, '-' | '+')) = chars.peek() {
                    chars.next();
                }

                let mut last_underscore = false;
                for j in 0.. {
                    let Some((i, c)) = chars.next() else {
                        break;
                    };

                    last_underscore = false;

                    match c {
                        '0'..='9' => {}
                        '_' => {
                            if j == 0 {
                                let pos = span.start.plus(i as u32);
                                return Err(Error::FloatExponentStartsWithUnderscore(pos));
                            }
                            last_underscore = true;
                            continue;
                        }
                        _ => {
                            let pos = span.start.plus(i as u32);
                            return Err(Error::InvalidCharInFloatExponent(c, pos));
                        }
                    }
                }

                if last_underscore {
                    let pos = span.end.minus(1);
                    return Err(Error::FloatExponentEndsWithUnderscore(pos));
                }

                return Ok(PartialValue::FloatWithExp);
            }
            ':' if i == 2 => {
                let hour = int_accum as u8;
                return datetime::continue_parsing_local_time(&mut chars, span, hour)
                    .map(PartialValue::PartialTime);
            }
            '-' if i == 4 => {
                let year = int_accum as u16;
                return datetime::continue_parsing_date_time_after_year(&mut chars, span, year);
            }
            '_' => {
                last_underscore = true;
                continue;
            }
            _ => {
                let pos = span.start.plus(i as u32);
                return Err(Error::InvalidCharInNumLiteral(c, pos));
            }
        }
    }

    if last_underscore {
        let pos = span.end.minus(1);
        return Err(Error::NumLiteralEndsWithUnderscore(pos));
    }

    match parse_state {
        NumParseState::Int => Ok(PartialValue::Int(int_accum)),
        NumParseState::OverflowOrFloat => Ok(PartialValue::OverflowOrFloat),
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntBits {
    Binary = 1,
    Octal = 3,
    Hexadecimal = 4,
}

fn parse_prefixed_int_literal(
    bits: IntBits,
    mut chars: impl Iterator<Item = (usize, char)>,
    span: Span,
) -> Result<i64, Error> {
    let max_value: u32 = 1 << bits as u32;
    let mut accum: i64 = 0;
    let mut last_underscore = false;

    for j in 0.. {
        let Some((i, c)) = chars.next() else {
            if j == 0 {
                return Err(Error::EmptyPrefixedIntValue(span.end));
            } else {
                break;
            }
        };

        last_underscore = false;

        let digit = match c {
            '0'..='9' => {
                let n = (c as u32) - ('0' as u32);
                if n >= max_value {
                    let pos = span.start.plus(i as u32);
                    return Err(Error::IntDigitTooBig(bits, c, pos));
                }
                n
            }
            'a'..='f' => {
                let n = 10 + (c as u32) - ('a' as u32);
                if n >= max_value {
                    let pos = span.start.plus(i as u32);
                    return Err(Error::IntDigitTooBig(bits, c, pos));
                }
                n
            }
            'A'..='F' => {
                let n = 10 + (c as u32) - ('A' as u32);
                if n >= max_value {
                    let pos = span.start.plus(i as u32);
                    return Err(Error::IntDigitTooBig(bits, c, pos));
                }
                n
            }
            '_' => {
                if j == 0 {
                    let pos = span.start.plus(i as u32);
                    return Err(Error::PrefixedIntValueStartsWithUnderscore(pos));
                }
                last_underscore = true;
                continue;
            }
            _ => {
                let pos = span.start.plus(i as u32);
                return Err(Error::InvalidCharInPrefixedInt(c, pos));
            }
        };

        let (val, overflow) = accum.overflowing_shl(bits as u32);
        if overflow {
            return Err(Error::IntLiteralOverflow(span));
        }

        accum = val;
        accum += digit as i64;
    }

    if last_underscore {
        let pos = span.end.minus(1);
        return Err(Error::PrefixedIntValueEndsWithUnderscore(pos));
    }

    Ok(accum)
}

fn validate_float_fractional_part(literal: &str, span: Span) -> Result<(), Error> {
    let mut chars = literal.char_indices().peekable();
    let mut last_underscore = false;
    loop {
        let Some((i, c)) = chars.next() else { break };

        match c {
            '0'..='9' => {}
            'e' | 'E' => {
                if last_underscore {
                    let pos = span.start.plus(i as u32 - 1);
                    return Err(Error::FloatFractEndsWithUnderscore(pos));
                }

                if let Some((_, '-' | '+')) = chars.peek() {
                    chars.next();
                }

                for j in 0.. {
                    let Some((i, c)) = chars.next() else { break };

                    last_underscore = false;

                    match c {
                        '0'..='9' => {}
                        '_' => {
                            if j == 0 {
                                let pos = span.start.plus(i as u32);
                                return Err(Error::FloatExponentStartsWithUnderscore(pos));
                            }
                            last_underscore = true;
                            continue;
                        }
                        _ => {
                            let pos = span.start.plus(i as u32);
                            return Err(Error::InvalidCharInFloatExponent(c, pos));
                        }
                    }
                }

                if last_underscore {
                    let pos = span.end.minus(1);
                    return Err(Error::FloatExponentEndsWithUnderscore(pos));
                }

                return Ok(());
            }
            '_' => (),
            _ => {
                let pos = span.start.plus(i as u32);
                return Err(Error::InvalidCharInFloatLiteral(c, pos));
            }
        }

        last_underscore = c == '_';
    }

    if last_underscore {
        let pos = span.end.minus(1);
        return Err(Error::FloatEndsWithUnderscore(pos));
    }

    Ok(())
}

fn parse_date_time_subsec_part<'a>(
    lit: &'a str,
    span: Span,
    subsec_lit: &str,
    subsec_span: Span,
    date: Option<Date>,
    mut time: Time,
) -> Result<DateTimeVal<'a>, Error> {
    let mut chars = subsec_lit.char_indices().peekable();
    let val = match date {
        Some(date) => {
            let (nanos, offset) = datetime::parse_subsec_and_offset(&mut chars, subsec_span)?;
            time.nanos = nanos;
            DateTime::from_optional_offset(date, time, offset)
        }
        None => {
            let nanos = datetime::parse_subsec_without_offset(&mut chars, subsec_span)?;
            time.nanos = nanos;
            DateTime::LocalTime(time)
        }
    };

    Ok(DateTimeVal::new(lit, span, val))
}
