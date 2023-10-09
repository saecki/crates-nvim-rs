use std::borrow::Cow;

use crate::toml::{lex, Ctx, Error, Pos, Quote, Range, Token, TokenType};

#[cfg(test)]
mod test;

macro_rules! recover_on {
    ($parser:expr, $tokens:pat, $label:lifetime) => {{
        loop {
            match $parser.peek().ty {
                $tokens => {
                    break $label;
                }
                _ => {
                    $parser.next();
                }
            }
        }
    }};
}

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: std::iter::Peekable<std::vec::IntoIter<Token<'a>>>,
    last: Token<'a>,
}

impl<'a> Parser<'a> {
    fn new(tokens: Vec<Token<'a>>) -> Self {
        let last = tokens.last().expect("there has to be an EOF token").clone();
        let tokens = tokens.into_iter().peekable();
        Self { tokens, last }
    }

    fn next(&mut self) -> Token<'a> {
        self.tokens.next().unwrap_or(self.last.clone())
    }

    fn peek(&mut self) -> &Token<'a> {
        self.tokens.peek().unwrap_or(&self.last)
    }

    fn peek_mut(&mut self) -> &mut Token<'a> {
        self.tokens.peek_mut().unwrap_or(&mut self.last)
    }

    fn eat_newlines(&mut self) {
        while self.peek().ty == TokenType::Newline {
            self.next();
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Ast<'a> {
    Assignment(Assignment<'a>),
    Table(Table<'a>),
    Array(Array<'a>),
}

#[derive(Debug, PartialEq)]
pub struct Table<'a> {
    pub header: TableHeader<'a>,
    pub assignments: Vec<Assignment<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct TableHeader<'a> {
    l_par: Pos,
    key: Option<Key<'a>>,
    r_par: Option<Pos>,
}

#[derive(Debug, PartialEq)]
pub struct Array<'a> {
    pub header: ArrayHeader<'a>,
    pub assignments: Vec<Assignment<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct ArrayHeader<'a> {
    l_pars: (Pos, Pos),
    key: Option<Key<'a>>,
    r_pars: (Option<Pos>, Option<Pos>),
}

#[derive(Debug, PartialEq)]
pub struct Assignment<'a> {
    pub key: Key<'a>,
    pub eq: Pos,
    pub val: Value<'a>,
}

#[derive(Debug, PartialEq)]
pub enum Key<'a> {
    One(Ident<'a>),
    Dotted(Vec<DottedIdent<'a>>),
}

#[derive(Debug, PartialEq)]
pub struct DottedIdent<'a> {
    pub ident: Ident<'a>,
    pub dot: Option<Pos>,
}

impl<'a> Key<'a> {
    fn from_plain_lit(lit: &'a str, range: Range) -> Self {
        Self::One(Ident {
            lit,
            lit_range: range,
            text: Cow::Borrowed(lit),
            text_range: range,
            kind: IdentKind::Plain,
        })
    }

    pub fn range(&self) -> Range {
        match self {
            Key::One(i) => i.lit_range,
            Key::Dotted(_) => todo!(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Ident<'a> {
    pub lit: &'a str,
    pub lit_range: Range,
    pub text: Cow<'a, str>,
    pub text_range: Range,
    pub kind: IdentKind,
}

#[derive(Debug, PartialEq)]
pub enum IdentKind {
    Plain,
    String(Quote),
}

// TODO: date and time
#[derive(Debug, PartialEq)]
pub enum Value<'a> {
    String(StringVal<'a>),
    Int(IntVal<'a>),
    Float(FloatVal<'a>),
    Bool(BoolVal),
    InlineTable(InlineTable<'a>),
    InlineArray(InlineArray<'a>),
}

impl Value<'_> {
    pub fn range(&self) -> Range {
        match self {
            Value::String(s) => s.lit_range,
            Value::Int(i) => i.lit_range,
            Value::Float(f) => f.lit_range,
            Value::Bool(b) => b.lit_range,
            Value::InlineTable(t) => t.range,
            Value::InlineArray(a) => a.range,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct StringVal<'a> {
    pub lit: &'a str,
    pub lit_range: Range,
    pub text: Cow<'a, str>,
    pub text_range: Range,
    pub quote: Quote,
}

#[derive(Debug, PartialEq)]
pub struct IntVal<'a> {
    pub lit: &'a str,
    pub lit_range: Range,
    pub val: i64,
}

#[derive(Debug, PartialEq)]
pub struct FloatVal<'a> {
    pub lit: &'a str,
    pub lit_range: Range,
    pub val: f64,
}

#[derive(Debug, PartialEq)]
pub struct BoolVal {
    pub lit_range: Range,
    pub val: bool,
}

#[derive(Debug, PartialEq)]
pub struct InlineTable<'a> {
    pub range: Range,
    pub assignments: Vec<InlineTableAssignment<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct InlineTableAssignment<'a> {
    pub key: Key<'a>,
    pub eq: Pos,
    pub val: Value<'a>,
    pub comma: Option<Pos>,
}

impl InlineTableAssignment<'_> {
    pub fn range(&self) -> Range {
        let start = self.key.range().start;
        let end = self
            .comma
            .map(|c| c.plus(1))
            .unwrap_or_else(|| self.val.range().end);
        Range { start, end }
    }
}

#[derive(Debug, PartialEq)]
pub struct InlineArray<'a> {
    pub range: Range,
    pub values: Vec<InlineArrayValue<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct InlineArrayValue<'a> {
    pub val: Value<'a>,
    pub comma: Option<Pos>,
}

enum Header<'a> {
    Table(Table<'a>),
    Array(Array<'a>),
}

impl<'a> Header<'a> {
    fn into_ast(self) -> Ast<'a> {
        match self {
            Header::Table(t) => Ast::Table(t),
            Header::Array(a) => Ast::Array(a),
        }
    }
}

impl InlineArrayValue<'_> {
    pub fn range(&self) -> Range {
        let start = self.val.range().start;
        let end = self
            .comma
            .map(|c| c.plus(1))
            .unwrap_or_else(|| self.val.range().end);
        Range { start, end }
    }
}

impl Ctx {
    // TODO: require newlines after toplevel assignments, array- and table headers.
    pub fn parse<'a>(&mut self, tokens: Vec<Token<'a>>) -> Result<Vec<Ast<'a>>, Error> {
        let mut parser = Parser::new(tokens);
        let mut asts = Vec::new();
        let mut last_header = None;

        loop {
            let token = parser.next();
            let key = match token.ty {
                TokenType::Ident(lit) => Key::One(Ident {
                    lit_range: token.range.clone(),
                    lit,
                    text: Cow::Borrowed(lit),
                    text_range: token.range,
                    kind: IdentKind::Plain,
                }),
                TokenType::String {
                    quote,
                    lit,
                    text,
                    text_range,
                } => Key::One(Ident {
                    lit_range: token.range,
                    lit,
                    text,
                    text_range,
                    kind: IdentKind::String(quote),
                }),
                TokenType::Int(_, lit) => {
                    if let Err((i, c)) = lex::validate_literal(lit) {
                        let mut pos = token.range.start;
                        pos.char += i as u32;
                        self.errors.push(Error::InvalidCharInIdentifier(c, pos));
                    }

                    Key::from_plain_lit(lit, token.range)
                }
                TokenType::Float(_, lit) => {
                    if let Err((i, c)) = lex::validate_literal(lit) {
                        let mut pos = token.range.start;
                        pos.char += i as u32;
                        self.errors.push(Error::InvalidCharInIdentifier(c, pos));
                    }

                    Key::from_plain_lit(lit, token.range)
                }
                TokenType::Bool(_, lit) => Key::from_plain_lit(lit, token.range),
                TokenType::SquareLeft => {
                    let l_table_square = token.range;

                    let l_array_square = match parser.peek() {
                        t if t.ty == TokenType::SquareLeft => Some(parser.next().range),
                        _ => None,
                    };

                    let key = match self.parse_key(&mut parser) {
                        Ok(k) => Some(k),
                        Err(e) => {
                            self.errors.push(e);
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
                        t if t.ty == TokenType::SquareRight => Some(parser.next().range.start),
                        t => {
                            self.errors
                                .push(Error::ExpectedRightSquareFound(t.ty.to_string(), t.range));
                            None
                        }
                    });

                    let r_table_square = match parser.peek() {
                        t if t.ty == TokenType::SquareRight => Some(parser.next().range.start),
                        t => {
                            self.errors
                                .push(Error::ExpectedRightSquareFound(t.ty.to_string(), t.range));
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
                            Header::Array(Array {
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
                    continue;
                }
                TokenType::SquareRight => todo!(),
                TokenType::CurlyLeft => todo!(),
                TokenType::CurlyRight => todo!(),
                TokenType::Equal => todo!(),
                TokenType::Comma => todo!(),
                TokenType::Dot => todo!(),
                TokenType::Newline => continue,
                TokenType::Invalid(_) => todo!(),
                TokenType::EOF => break,
            };

            // TODO: somehow try to recover, probably on newline
            let eq = match parser.next() {
                t if t.ty == TokenType::Equal => t.range.start,
                _ => todo!("error"),
            };

            let val = match self.parse_value(&mut parser) {
                Ok(v) => v,
                Err(e) => {
                    self.errors.push(e);
                    break;
                }
            };

            let assignment = Assignment { key, eq, val };
            match &mut last_header {
                Some(Header::Table(t)) => t.assignments.push(assignment),
                Some(Header::Array(a)) => a.assignments.push(assignment),
                None => asts.push(Ast::Assignment(assignment)),
            }
        }

        if let Some(last) = last_header {
            asts.push(last.into_ast());
        }

        Ok(asts)
    }

    fn parse_key<'a>(&mut self, parser: &mut Parser<'a>) -> Result<Key<'a>, Error> {
        // TODO: parse dotted keys

        let token = parser.peek_mut();
        let key = match &mut token.ty {
            TokenType::Ident(lit) => Key::One(Ident {
                lit_range: token.range.clone(),
                lit,
                text: Cow::Borrowed(lit),
                text_range: token.range,
                kind: IdentKind::Plain,
            }),
            TokenType::String {
                quote,
                lit,
                text,
                text_range,
            } => Key::One(Ident {
                lit_range: token.range,
                lit,
                text: std::mem::take(text),
                text_range: *text_range,
                kind: IdentKind::String(*quote),
            }),
            TokenType::Int(_, lit) => {
                if let Err((i, c)) = lex::validate_literal(lit) {
                    let mut pos = token.range.start;
                    pos.char += i as u32;
                    self.errors.push(Error::InvalidCharInIdentifier(c, pos));
                }

                Key::from_plain_lit(lit, token.range)
            }
            TokenType::Float(_, lit) => {
                if let Err((i, c)) = lex::validate_literal(lit) {
                    let mut pos = token.range.start;
                    pos.char += i as u32;
                    self.errors.push(Error::InvalidCharInIdentifier(c, pos));
                }

                Key::from_plain_lit(lit, token.range)
            }
            TokenType::Bool(_, lit) => Key::from_plain_lit(lit, token.range),
            TokenType::SquareLeft => todo!(),
            TokenType::SquareRight => todo!(),
            TokenType::CurlyLeft => todo!(),
            TokenType::CurlyRight => todo!(),
            TokenType::Equal => return Err(Error::ExpectedKey(token.ty.to_string(), token.range)),
            TokenType::Comma => todo!(),
            TokenType::Dot => todo!(),
            TokenType::Newline => todo!(),
            TokenType::Invalid(_) => todo!(),
            TokenType::EOF => todo!(),
        };

        parser.next();

        Ok(key)
    }

    fn parse_value<'a>(&mut self, parser: &mut Parser<'a>) -> Result<Value<'a>, Error> {
        let token = parser.peek_mut();
        let value = match &mut token.ty {
            TokenType::Ident(_) => todo!(),
            TokenType::String {
                quote,
                lit,
                text,
                text_range,
            } => Value::String(StringVal {
                lit,
                lit_range: token.range,
                text: std::mem::take(text),
                text_range: *text_range,
                quote: *quote,
            }),
            TokenType::Int(val, lit) => Value::Int(IntVal {
                lit,
                lit_range: token.range,
                val: *val,
            }),
            TokenType::Float(val, lit) => Value::Float(FloatVal {
                lit,
                lit_range: token.range,
                val: *val,
            }),
            TokenType::Bool(val, _lit) => Value::Bool(BoolVal {
                lit_range: token.range,
                val: *val,
            }),
            TokenType::SquareLeft => {
                let l_square_range = token.range;
                parser.next();

                let mut values = Vec::new();
                'inline_array: loop {
                    if matches!(parser.peek().ty, TokenType::SquareRight | TokenType::EOF) {
                        break;
                    }

                    parser.eat_newlines();
                    let val = match self.parse_value(parser) {
                        Ok(v) => v,
                        Err(e) => {
                            self.errors.push(e);

                            recover_on!(parser, TokenType::SquareRight | TokenType::EOF, 'inline_array)
                        }
                    };

                    parser.eat_newlines();
                    let mut value = InlineArrayValue { val, comma: None };
                    match parser.peek() {
                        t if t.ty == TokenType::Comma => {
                            value.comma = Some(parser.next().range.start);
                        }
                        t if t.ty == TokenType::SquareRight || t.ty == TokenType::EOF => {
                            values.push(value);
                            break;
                        }
                        _ => {
                            // TODO: maybe try to eat dot or similar character
                            let pos = value.val.range().end;
                            self.errors.push(Error::ExpectedComma(pos));
                            // try to continue
                        }
                    };

                    values.push(value);
                }

                parser.eat_newlines();
                let end = match parser.peek() {
                    t if t.ty == TokenType::SquareRight => parser.next().range.end,
                    t => {
                        self.errors
                            .push(Error::ExpectedRightSquareFound(t.ty.to_string(), t.range));

                        values.last().map_or(l_square_range.end, |v| v.range().end)
                    }
                };

                let range = Range {
                    start: l_square_range.start,
                    end,
                };

                return Ok(Value::InlineArray(InlineArray { range, values }));
            }
            TokenType::SquareRight => todo!(),
            TokenType::CurlyLeft => {
                let l_curly_range = token.range;
                parser.next();

                let mut assignments = Vec::new();
                'inline_table: loop {
                    if matches!(parser.peek().ty, TokenType::CurlyRight | TokenType::EOF) {
                        break;
                    }
                    let key = match self.parse_key(parser) {
                        Ok(k) => k,
                        Err(e) => {
                            self.errors.push(e);
                            recover_on!(parser, TokenType::CurlyRight | TokenType::Newline | TokenType::EOF, 'inline_table)
                        }
                    };

                    // TODO: somehow try to recover, probably on newline
                    let eq = match parser.peek() {
                        t if t.ty == TokenType::Equal => parser.next().range.start,
                        t => {
                            self.errors
                                .push(Error::ExpectedEqFound(t.ty.to_string(), t.range));
                            recover_on!(parser, TokenType::CurlyRight | TokenType::Newline | TokenType::EOF, 'inline_table)
                        }
                    };

                    let val = match self.parse_value(parser) {
                        Ok(v) => v,
                        Err(e) => {
                            self.errors.push(e);
                            recover_on!(parser, TokenType::CurlyRight | TokenType::Newline | TokenType::EOF, 'inline_table)
                        }
                    };

                    let mut assignment = InlineTableAssignment {
                        key,
                        eq,
                        val,
                        comma: None,
                    };
                    match parser.peek() {
                        t if t.ty == TokenType::Comma => {
                            assignment.comma = Some(parser.next().range.start);
                        }
                        t if t.ty == TokenType::CurlyRight || t.ty == TokenType::EOF => {
                            assignments.push(assignment);
                            break;
                        }
                        _ => {
                            // TODO: maybe try to eat dot or similar character
                            let pos = assignment.val.range().end;
                            self.errors.push(Error::ExpectedComma(pos));
                            // try to continue
                        }
                    };

                    assignments.push(assignment);
                }

                let end = match parser.peek() {
                    t if t.ty == TokenType::CurlyRight => parser.next().range.end,
                    t => {
                        self.errors
                            .push(Error::ExpectedRightCurlyFound(t.ty.to_string(), t.range));

                        assignments
                            .last()
                            .map_or(l_curly_range.end, |a| a.range().end)
                    }
                };

                let range = Range {
                    start: l_curly_range.start,
                    end,
                };

                return Ok(Value::InlineTable(InlineTable { range, assignments }));
            }
            TokenType::CurlyRight => todo!(),
            TokenType::Equal => {
                return Err(Error::ExpectedValue(token.ty.to_string(), token.range))
            }
            TokenType::Comma => todo!(),
            TokenType::Dot => todo!(),
            TokenType::Newline => todo!(),
            TokenType::Invalid(_) => todo!(),
            TokenType::EOF => todo!(),
        };

        parser.next();

        Ok(value)
    }
}
