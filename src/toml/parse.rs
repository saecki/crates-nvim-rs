use std::borrow::Cow;

use crate::toml::{Ctx, Error, Pos, Quote, Range, Token, TokenType};

#[cfg(test)]
mod test;

#[derive(Debug)]
pub struct Parser<'a> {
    rhs: bool,
    tokens: std::iter::Peekable<std::vec::IntoIter<Token<'a>>>,
}

impl<'a> Parser<'a> {
    fn new(tokens: std::iter::Peekable<std::vec::IntoIter<Token<'a>>>) -> Self {
        Self { rhs: false, tokens }
    }

    fn next(&mut self) -> Option<Token<'a>> {
        self.tokens.next()
    }

    fn peek(&mut self) -> Option<&Token<'a>> {
        self.tokens.peek()
    }
}

#[derive(Debug, PartialEq)]
pub enum Ast<'a> {
    TableHeader(TableHeader<'a>),
    Assignment(Key<'a>, Pos, Value<'a>),
}

#[derive(Debug, PartialEq)]
pub enum Key<'a> {
    One(Ident<'a>),
    Dotted(Vec<Ident<'a>>),
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
    Array(Array<'a>),
    InlineTable(InlineTable<'a>),
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
pub struct Array<'a> {
    pub range: Range,
    pub val: Vec<Value<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct InlineTable<'a> {
    pub range: Range,
    pub val: Vec<(Key<'a>, Value<'a>)>,
}

#[derive(Debug, PartialEq)]
pub struct TableHeader<'a> {
    l_par: Option<Pos>,
    key: Key<'a>,
    r_par: Option<Pos>,
}

impl Ctx {
    pub fn parse<'a>(&mut self, tokens: Vec<Token<'a>>) -> Result<Vec<Ast<'a>>, Error> {
        let mut parser = Parser::new(tokens.into_iter().peekable());
        let mut asts = Vec::new();

        loop {
            let Some(token) = parser.next() else { break };
            let lhs = match token.ty {
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
                TokenType::Int(_, lit) => Key::One(Ident {
                    lit_range: token.range,
                    lit,
                    text: Cow::Borrowed(lit),
                    text_range: token.range,
                    kind: IdentKind::Plain,
                }),
                TokenType::Float(_, lit) => {
                    let invalid_char = lit
                        .char_indices()
                        .find(|(_, c)| matches!(c, 'a'..='z' | 'A'..='Z' | '-' | '_'));

                    if let Some((i, c)) = invalid_char {
                        let mut pos = token.range.start;
                        pos.char += i as u32;
                        self.errors.push(Error::InvalidCharInIdentifier(c, pos));
                    }

                    Key::One(Ident {
                        lit_range: token.range,
                        lit,
                        text: Cow::Borrowed(lit),
                        text_range: token.range,
                        kind: IdentKind::Plain,
                    })
                }
                TokenType::Bool(_, lit) => Key::One(Ident {
                    lit_range: token.range,
                    lit,
                    text: Cow::Borrowed(lit),
                    text_range: token.range,
                    kind: IdentKind::Plain,
                }),
                TokenType::SquareLeft => todo!(),
                TokenType::SquareRight => todo!(),
                TokenType::CurlyLeft => todo!(),
                TokenType::CurlyRight => todo!(),
                TokenType::Equal => todo!(),
                TokenType::Comma => todo!(),
                TokenType::Dot => todo!(),
                TokenType::Newline => continue,
                TokenType::Invalid(_) => todo!(),
            };

            // TODO: somehow try to recover, probably on newline
            let eq = match parser.next() {
                Some(t) if t.ty == TokenType::Equal => t,
                Some(_) => todo!("error"),
                None => todo!("error"),
            };

            let Some(token) = parser.next() else {
                todo!("error");
                break;
            };
            let rhs = match token.ty {
                TokenType::Ident(_) => todo!(),
                TokenType::String {
                    quote,
                    lit,
                    text,
                    text_range,
                } => Value::String(StringVal {
                    lit,
                    lit_range: token.range,
                    text,
                    text_range,
                    quote,
                }),
                TokenType::Int(val, lit) => Value::Int(IntVal {
                    lit,
                    lit_range: token.range,
                    val,
                }),
                TokenType::Float(val, lit) => Value::Float(FloatVal {
                    lit,
                    lit_range: token.range,
                    val,
                }),
                TokenType::Bool(val, _lit) => Value::Bool(BoolVal {
                    lit_range: token.range,
                    val,
                }),
                TokenType::SquareLeft => todo!("parse array"),
                TokenType::SquareRight => todo!(),
                TokenType::CurlyLeft => todo!("parse inline table"),
                TokenType::CurlyRight => todo!(),
                TokenType::Equal => todo!(),
                TokenType::Comma => todo!(),
                TokenType::Dot => todo!(),
                TokenType::Newline => todo!(),
                TokenType::Invalid(_) => todo!(),
            };

            asts.push(Ast::Assignment(lhs, eq.range.start, rhs));
        }

        Ok(asts)
    }
}
