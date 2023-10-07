use std::borrow::Cow;

use crate::toml::{lex, Ctx, Error, Pos, Quote, Range, Token, TokenType};

#[cfg(test)]
mod test;

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: std::iter::Peekable<std::vec::IntoIter<Token<'a>>>,
}

impl<'a> Parser<'a> {
    fn new(tokens: std::iter::Peekable<std::vec::IntoIter<Token<'a>>>) -> Self {
        Self { tokens }
    }

    fn next(&mut self) -> Option<Token<'a>> {
        self.tokens.next()
    }

    fn peek(&mut self) -> Option<&Token<'a>> {
        self.tokens.peek()
    }
}

#[derive(Clone, Copy)]
struct StopOn(u16);

#[rustfmt::skip]
// TODO: remove later
#[allow(unused)]
impl StopOn {
    const NOTHING: Self      = Self(0x00_00);
    const IDENT: Self        = Self(0x00_01);
    const STRING: Self       = Self(0x00_02);
    const INT: Self          = Self(0x00_04);
    const FLOAT: Self        = Self(0x00_08);
    const BOOL: Self         = Self(0x00_10);
    const SQUARE_LEFT: Self  = Self(0x00_20);
    const SQUARE_RIGHT: Self = Self(0x00_40);
    const CURLY_LEFT: Self   = Self(0x00_80);
    const CURLY_RIGHT: Self  = Self(0x01_00);
    const EQUAL: Self        = Self(0x02_00);
    const COMMA: Self        = Self(0x04_00);
    const DOT: Self          = Self(0x08_00);
    const NEWLINE: Self      = Self(0x10_00);
    const INVALID: Self      = Self(0x20_00);
}

impl StopOn {
    fn contains(&self, ty: &TokenType) -> bool {
        let stop_on = match ty {
            TokenType::Ident(_) => Self::IDENT,
            TokenType::String { .. } => Self::STRING,
            TokenType::Int(_, _) => Self::INT,
            TokenType::Float(_, _) => Self::FLOAT,
            TokenType::Bool(_, _) => Self::BOOL,
            TokenType::SquareLeft => Self::SQUARE_LEFT,
            TokenType::SquareRight => Self::SQUARE_RIGHT,
            TokenType::CurlyLeft => Self::CURLY_LEFT,
            TokenType::CurlyRight => Self::CURLY_RIGHT,
            TokenType::Equal => Self::EQUAL,
            TokenType::Comma => Self::COMMA,
            TokenType::Dot => Self::DOT,
            TokenType::Newline => Self::NEWLINE,
            TokenType::Invalid(_) => Self::INVALID,
        };
        (self.0 & stop_on.0) != 0
    }
}

impl std::ops::BitOr for StopOn {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

#[derive(Debug, PartialEq)]
pub enum Ast<'a> {
    Assignment(Assignment<'a>),
    Table(TableHeader<'a>, Vec<Assignment<'a>>),
    Array(ArrayHeader<'a>, Vec<Assignment<'a>>),
}

#[derive(Debug, PartialEq)]
pub struct TableHeader<'a> {
    l_par: Option<Pos>,
    key: Key<'a>,
    r_par: Option<Pos>,
}

#[derive(Debug, PartialEq)]
pub struct ArrayHeader<'a> {
    l_pars: (Option<Pos>, Option<Pos>),
    key: Key<'a>,
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
    InlineArray(InlineArray<'a>),
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
pub struct InlineArray<'a> {
    pub range: Range,
    pub values: Vec<Value<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct InlineTable<'a> {
    pub range: Range,
    pub assignments: Vec<Assignment<'a>>,
}

impl Ctx {
    pub fn parse<'a>(&mut self, tokens: Vec<Token<'a>>) -> Result<Vec<Ast<'a>>, Error> {
        let mut parser = Parser::new(tokens.into_iter().peekable());
        let mut asts = Vec::new();

        loop {
            // eat newlines
            while let Some(t) = parser.peek() {
                if t.ty == TokenType::Newline {
                    parser.next();
                } else {
                    break;
                }
            }

            let Some(key) = self.parse_key(&mut parser, StopOn::NOTHING)? else {
                break;
            };

            // TODO: somehow try to recover, probably on newline
            let eq = match parser.next() {
                Some(t) if t.ty == TokenType::Equal => t.range.start,
                Some(_) => todo!("error"),
                None => todo!("error"),
            };

            let Some(val) = self.parse_value(&mut parser, StopOn::NOTHING)? else {
                todo!("error");
                break;
            };

            asts.push(Ast::Assignment(Assignment { key, eq, val }));
        }

        Ok(asts)
    }

    fn parse_key<'a>(
        &mut self,
        parser: &mut Parser<'a>,
        stop_on: StopOn,
    ) -> Result<Option<Key<'a>>, Error> {
        let Some(token) = parser.peek() else {
            return Ok(None);
        };

        if stop_on.contains(&token.ty) {
            return Ok(None);
        }

        let token = parser.next().unwrap();

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

                Key::One(Ident {
                    lit_range: token.range,
                    lit,
                    text: Cow::Borrowed(lit),
                    text_range: token.range,
                    kind: IdentKind::Plain,
                })
            }
            TokenType::Float(_, lit) => {
                if let Err((i, c)) = lex::validate_literal(lit) {
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
            TokenType::Invalid(_) => todo!(),
        };

        Ok(Some(key))
    }

    fn parse_value<'a>(
        &mut self,
        parser: &mut Parser<'a>,
        stop_on: StopOn,
    ) -> Result<Option<Value<'a>>, Error> {
        let Some(token) = parser.peek() else {
            return Ok(None);
        };

        if stop_on.contains(&token.ty) {
            return Ok(None);
        }

        let token = parser.next().unwrap();

        let value = match token.ty {
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
            TokenType::SquareLeft => {
                let mut values = Vec::new();
                while let Some(value) = self.parse_value(parser, StopOn::SQUARE_RIGHT)? {
                    values.push(value);

                    match parser.peek() {
                        Some(t) if t.ty == TokenType::Comma => {
                            parser.next();
                        }
                        Some(t) if t.ty == TokenType::SquareRight => break,
                        Some(_) => todo!("push error and continue"),
                        None => break,
                    }
                }

                let right_par = match parser.next() {
                    Some(t) if t.ty == TokenType::SquareRight => t,
                    Some(_) => todo!("error"),
                    None => todo!("push error"),
                };

                let range = Range {
                    start: token.range.start,
                    end: right_par.range.end,
                };

                Value::InlineArray(InlineArray { range, values })
            }
            TokenType::SquareRight => todo!(),
            TokenType::CurlyLeft => {
                let mut assignments = Vec::new();
                loop {
                    let Some(key) = self.parse_key(parser, StopOn::CURLY_RIGHT)? else {
                        break;
                    };

                    // TODO: somehow try to recover, probably on newline
                    let eq = match parser.next() {
                        Some(t) if t.ty == TokenType::Equal => t.range.start,
                        Some(_) => todo!("error"),
                        None => todo!("error"),
                    };

                    let Some(val) = self.parse_value(parser, StopOn::CURLY_RIGHT)? else {
                        todo!("error");
                        break;
                    };

                    assignments.push(Assignment { key, eq, val });

                    match parser.peek() {
                        Some(t) if t.ty == TokenType::Comma => {
                            parser.next();
                        }
                        Some(t) if t.ty == TokenType::CurlyRight => break,
                        Some(_) => todo!("push error and continue"),
                        None => break,
                    }
                }

                let right_par = match parser.next() {
                    Some(t) if t.ty == TokenType::CurlyRight => t,
                    Some(_) => todo!("error"),
                    None => todo!("push error"),
                };

                let range = Range {
                    start: token.range.start,
                    end: right_par.range.end,
                };

                Value::InlineTable(InlineTable { range, assignments })
            }
            TokenType::CurlyRight => todo!(),
            TokenType::Equal => todo!(),
            TokenType::Comma => todo!(),
            TokenType::Dot => todo!(),
            TokenType::Newline => todo!(),
            TokenType::Invalid(_) => todo!(),
        };

        Ok(Some(value))
    }
}
