use std::borrow::Cow;

use crate::toml::{Ctx, Error, Pos, Quote, Range, Token, TokenType};

#[derive(Debug)]
pub struct Parser<'a> {
    rhs: bool,
    tokens: std::iter::Peekable<std::vec::IntoIter<Token<'a>>>,
}

impl<'a> Parser<'a> {
    fn new(tokens: std::iter::Peekable<std::vec::IntoIter<Token<'a>>>) -> Self {
        Self { rhs: false, tokens }
    }

    fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }
}

pub enum Ast<'a> {
    TableHeader(TableHeader<'a>),
    Assignment(Key<'a>),
}

pub enum Key<'a> {
    One(Ident<'a>),
    Dotted(Vec<Ident<'a>>),
}

pub enum Value {
    String,
}

pub struct Ident<'a> {
    pub lit: &'a str,
    pub lit_range: Range,
    pub text: Cow<'a, str>,
    pub text_range: Range,
    pub kind: IdentKind,
}

pub enum IdentKind {
    Plain,
    String(Quote),
}

pub struct TableHeader<'a> {
    l_par: Option<Pos>,
    key: Key<'a>,
    r_par: Option<Pos>,
}

impl Ctx {
    pub fn parse(&mut self, tokens: Vec<Token>) -> Result<Vec<Ast>, Error> {
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
                TokenType::Newline => todo!(),
                TokenType::Invalid(l) => todo!(),
            };
        }

        Ok(asts)
    }

    pub fn parse_table_header<'a>(&mut self, parser: &mut Parser<'a>) -> TableHeader<'a> {
        if let Ast:: parser.peek()
    }
}
