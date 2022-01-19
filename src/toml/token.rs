use serde::{Deserialize, Serialize};

use crate::toml::{Ctx, Error};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Token {
    range: Range,
    typ: TokenType,
    text: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Range {
    start: Pos,
    end: Pos,
}

impl Range {
    pub fn char(pos: Pos) -> Self {
        Self {
            start: pos,
            end: pos.moved(0, 1),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct Pos {
    line: usize,
    char: usize,
}

impl Pos {
    pub fn moved(&self, line: usize, char: usize) -> Self {
        Self {
            line: self.line + line,
            char: self.char + char,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub enum TokenType {
    Ident,
    String(Quote),
    Int(i64),
    Float(f64),
    Bool(bool),
    Par(Par),
    Equal,
    Comma,
    Dot,
    Invalid,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Par {
    HeaderLeft,
    HeaderRight,
    ArrayLeft,
    ArrayRight,
    CurlyLeft,
    CurlyRight,
}

impl Par {
    fn matches(self, other: Par) -> bool {
        match self {
            Self::HeaderLeft => matches!(other, Self::ArrayRight),
            Self::HeaderRight => matches!(other, Self::ArrayLeft),
            Self::ArrayLeft => matches!(other, Self::ArrayRight),
            Self::ArrayRight => matches!(other, Self::ArrayLeft),
            Self::CurlyLeft => matches!(other, Self::CurlyRight),
            Self::CurlyRight => matches!(other, Self::CurlyLeft),
        }
    }

    pub fn is_curly(&self) -> bool {
        matches!(self, Self::CurlyLeft | Self::CurlyRight)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Quote {
    Single,
    Double,
}

impl Quote {
    fn matches(self, char: char) -> bool {
        match self {
            Self::Single => char == '\'',
            Self::Double => char == '"',
        }
    }

    /// Returns `true` if the quote is [`Double`].
    ///
    /// [`Double`]: Quote::Double
    pub fn is_double(&self) -> bool {
        matches!(self, Self::Double)
    }
}

#[derive(Debug, Default)]
struct State {
    pos: Pos,
    rhs: bool,
    stack: Vec<Struct>,
    tokens: Vec<Token>,
    str: Option<StrState>,
    lit_start: Pos,
    lit: String,
}

#[derive(Clone, Copy, Debug)]
pub enum Struct {
    TableHeader,
    InlineTable,
    Array,
}

impl Struct {
    pub fn supports_multiline(&self) -> bool {
        match self {
            Struct::TableHeader => false,
            Struct::InlineTable => false,
            Struct::Array => true,
        }
    }
}

#[derive(Debug)]
struct StrState {
    esc: bool,
    esc_unicode: Option<UnicodeState>,
    quote: Quote,
}

#[derive(Debug)]
struct UnicodeState {
    start: Pos,
    count: u8,
    cp: u32,
}

impl Ctx {
    pub fn tokenize(&mut self, lines: &[impl AsRef<str>]) -> Result<Vec<Token>, Error> {
        let mut state = State::default();

        for (li, l) in lines.iter().enumerate() {
            state.rhs = false; // TODO: if not in array
            self.pop_inline_table_pars(&mut state);

            for (ci, c) in l.as_ref().chars().enumerate() {
                state.pos.line = li;
                state.pos.char = ci;

                if let Some(str) = &mut state.str {
                    if !str.esc && str.quote.matches(c) {
                        let quote = str.quote;
                        self.finish_string(&mut state, quote);
                        state.str = None;
                    } else if str.quote.is_double() && !str.esc && c == '\\' {
                        str.esc = true;
                    } else if str.esc {
                        if let Some(esc) = &mut str.esc_unicode {
                            esc.count -= 1;

                            let offset = esc.count * 4;
                            match c {
                                '0'..='9' => {
                                    esc.cp += (c as u32 - '0' as u32) << offset;
                                }
                                'a'..='f' => {
                                    esc.cp += (c as u32 - 'a' as u32 + 10) << offset;
                                }
                                'A'..='F' => {
                                    esc.cp += (c as u32 - 'A' as u32 + 10) << offset;
                                }
                                _ => {
                                    self.errors.push(Error::InvalidUnicodeEscapeChar(
                                        c,
                                        Pos { line: li, char: ci },
                                    ));

                                    if str.quote.matches(c) {
                                        let quote = str.quote;
                                        self.finish_string(&mut state, quote);
                                        state.str = None;
                                        continue;
                                    }
                                }
                            }

                            if esc.count == 0 {
                                match char::from_u32(esc.cp) {
                                    Some(char) => state.lit.push(char),
                                    None => self.errors.push(Error::InvalidUnicodeScalar(
                                        esc.cp,
                                        Range {
                                            start: esc.start,
                                            end: Pos {
                                                line: li,
                                                char: ci + 1,
                                            },
                                        },
                                    )),
                                }
                                str.esc = false;
                                str.esc_unicode = None;
                            }
                        } else if c == 'u' {
                            str.esc_unicode = Some(UnicodeState {
                                start: Pos { line: li, char: ci },
                                count: 4,
                                cp: 0,
                            });
                        } else if c == 'U' {
                            str.esc_unicode = Some(UnicodeState {
                                start: Pos { line: li, char: ci },
                                count: 8,
                                cp: 0,
                            });
                        } else {
                            match c {
                                'b' => state.lit.push('\u{8}'),
                                't' => state.lit.push('\t'),
                                'n' => state.lit.push('\n'),
                                'f' => state.lit.push('\u{D}'),
                                'r' => state.lit.push('\r'),
                                '"' => state.lit.push('"'),
                                '\\' => state.lit.push('\\'),
                                _ => self
                                    .errors
                                    .push(Error::InvalidEscapeChar(c, Pos { line: li, char: ci })),
                            }
                            str.esc = false;
                        }
                    } else {
                        state.lit.push(c);
                    }
                    continue;
                }

                match c {
                    '\t' | ' ' => self.finish_literal(&mut state),
                    '"' => {
                        self.finish_literal(&mut state);
                        state.str = Some(StrState {
                            esc: false,
                            esc_unicode: None,
                            quote: Quote::Double,
                        });
                        state.lit_start = state.pos;
                    }
                    '\'' => {
                        self.finish_literal(&mut state);
                        state.str = Some(StrState {
                            esc: false,
                            esc_unicode: None,
                            quote: Quote::Single,
                        });
                        state.lit_start = state.pos;
                    }
                    '[' => {
                        self.char_token(&mut state, c, TokenType::Par(Par::ArrayLeft));
                        if state.rhs {
                            state.stack.push(Struct::Array);
                        } else {
                            state.stack.push(Struct::TableHeader);
                        }
                    }
                    ']' => {
                        self.char_token(&mut state, c, TokenType::Par(Par::ArrayRight));
                        self.close_par(&mut state, Par::ArrayRight);
                    }
                    '{' => {
                        self.char_token(&mut state, c, TokenType::Par(Par::CurlyLeft));
                        state.stack.push(Struct::InlineTable);
                        state.rhs = false;
                    }
                    '}' => {
                        self.char_token(&mut state, c, TokenType::Par(Par::CurlyRight));
                        self.close_par(&mut state, Par::CurlyRight);
                    }
                    '=' => {
                        self.char_token(&mut state, c, TokenType::Equal);
                        state.rhs = true;
                    }
                    '.' => {
                        if state.rhs {
                            self.push_literal(&mut state, c);
                        } else {
                            self.char_token(&mut state, c, TokenType::Dot);
                        }
                    }
                    ',' => {
                        self.char_token(&mut state, c, TokenType::Comma);
                    }
                    '#' => todo!("comment"),
                    _ => self.push_literal(&mut state, c),
                }
            }
        }

        state.pos.char += 1;

        if let Some(str) = &mut state.str {
            if str.esc {
                self.errors.push(Error::UnfinishedEscapeSequence(state.pos));
            }
            let quote = str.quote;
            self.errors.push(Error::MissingQuote(quote, state.pos));

            self.finish_string(&mut state, quote);
        } else {
            self.finish_literal(&mut state);
        }

        Ok(state.tokens)
    }

    fn push_literal(&mut self, state: &mut State, char: char) {
        if state.lit.is_empty() {
            state.lit_start = state.pos;
        }
        state.lit.push(char);
    }

    fn finish_literal(&mut self, state: &mut State) {
        if !state.lit.is_empty() {
            let text = state.lit.clone();
            state.lit.clear();

            let range = Range {
                start: state.lit_start,
                end: state.pos,
            };

            let typ = if state.rhs {
                if let Ok(i) = text.parse::<i64>() {
                    TokenType::Int(i)
                } else if let Ok(f) = text.parse::<f64>() {
                    TokenType::Float(f)
                } else {
                    match text.as_str() {
                        "true" => TokenType::Bool(true),
                        "false" => TokenType::Bool(false),
                        "nan" => TokenType::Float(f64::NAN),
                        "+nan" => TokenType::Float(f64::NAN),
                        "-nan" => TokenType::Float(-f64::NAN),
                        "inf" => TokenType::Float(f64::INFINITY),
                        "+inf" => TokenType::Float(f64::INFINITY),
                        "-inf" => TokenType::Float(-f64::NEG_INFINITY),
                        _ => {
                            // TODO date and time
                            self.errors
                                .push(Error::InvalidRhsLiteral(text.clone(), range.clone()));
                            TokenType::Invalid
                        }
                    }
                }
            } else {
                TokenType::Ident
            };

            let token = Token { range, typ, text };
            state.tokens.push(token);
        }
    }

    fn finish_string(&mut self, state: &mut State, quote: Quote) {
        let lit = state.lit.clone();
        state.lit.clear();

        let token = Token {
            range: Range {
                start: state.lit_start,
                end: state.pos.moved(0, 1),
            },
            typ: TokenType::String(quote),
            text: lit,
        };
        state.tokens.push(token);

        state.str = None;
    }

    fn char_token(&mut self, state: &mut State, char: char, typ: TokenType) {
        self.finish_literal(state);

        state.tokens.push(Token {
            range: Range::char(state.pos),
            typ,
            text: char.to_string(),
        });
    }

    fn close_par(&mut self, state: &mut State, par: Par) {
        let mut pars = state.stack.iter().rev();
        while let Some(&p) = pars.next() {
            todo!("close par")
        }
    }

    fn pop_inline_table_pars(&mut self, state: &mut State) {
        let i = state.stack.iter().position(|s| s.supports_multiline());
        if let Some(i) = i {
            for _ in 0..(state.stack.len() - i) {
                match state.stack.pop().expect("") {
                    s @ Struct::TableHeader => {
                        self.errors
                            .push(Error::MissingRightPar(s, state.pos.moved(0, 1)));
                    }
                    s @ Struct::InlineTable => {
                        self.errors
                            .push(Error::MissingRightPar(s, state.pos.moved(0, 1)));
                    }
                    s @ Struct::Array => {
                        self.errors
                            .push(Error::MissingRightPar(s, state.pos.moved(0, 1)));
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn assign_int() {
        let mut ctx = Ctx::default();

        let tokens = ctx.tokenize(&["my_int = 98742"]).unwrap();
        assert_eq!(
            tokens,
            [
                Token {
                    range: Range {
                        start: Pos { line: 0, char: 0 },
                        end: Pos { line: 0, char: 6 },
                    },
                    typ: TokenType::Ident,
                    text: "my_int".to_string(),
                },
                Token {
                    range: Range {
                        start: Pos { line: 0, char: 7 },
                        end: Pos { line: 0, char: 8 },
                    },
                    typ: TokenType::Equal,
                    text: "=".to_string(),
                },
                Token {
                    range: Range {
                        start: Pos { line: 0, char: 9 },
                        end: Pos { line: 0, char: 14 },
                    },
                    typ: TokenType::Int(98742),
                    text: "98742".to_string(),
                }
            ],
        );
    }

    #[test]
    fn assign_float() {
        let mut ctx = Ctx::default();

        let tokens = ctx.tokenize(&["my_float=0.23"]).unwrap();
        assert_eq!(
            tokens,
            [
                Token {
                    range: Range {
                        start: Pos { line: 0, char: 0 },
                        end: Pos { line: 0, char: 8 },
                    },
                    typ: TokenType::Ident,
                    text: "my_float".to_string(),
                },
                Token {
                    range: Range {
                        start: Pos { line: 0, char: 8 },
                        end: Pos { line: 0, char: 9 },
                    },
                    typ: TokenType::Equal,
                    text: "=".to_string(),
                },
                Token {
                    range: Range {
                        start: Pos { line: 0, char: 9 },
                        end: Pos { line: 0, char: 13 },
                    },
                    typ: TokenType::Float(0.23),
                    text: "0.23".to_string(),
                }
            ],
        );
    }

    #[test]
    fn assign_literal_string() {
        let mut ctx = Ctx::default();

        let tokens = ctx.tokenize(&["my.string = 'yeet\\'"]).unwrap();
        assert_eq!(
            tokens,
            [
                Token {
                    range: Range {
                        start: Pos { line: 0, char: 0 },
                        end: Pos { line: 0, char: 2 },
                    },
                    typ: TokenType::Ident,
                    text: "my".to_string(),
                },
                Token {
                    range: Range {
                        start: Pos { line: 0, char: 2 },
                        end: Pos { line: 0, char: 3 },
                    },
                    typ: TokenType::Dot,
                    text: ".".to_string(),
                },
                Token {
                    range: Range {
                        start: Pos { line: 0, char: 3 },
                        end: Pos { line: 0, char: 9 },
                    },
                    typ: TokenType::Ident,
                    text: "string".to_string(),
                },
                Token {
                    range: Range {
                        start: Pos { line: 0, char: 10 },
                        end: Pos { line: 0, char: 11 },
                    },
                    typ: TokenType::Equal,
                    text: "=".to_string(),
                },
                Token {
                    range: Range {
                        start: Pos { line: 0, char: 12 },
                        end: Pos { line: 0, char: 19 },
                    },
                    typ: TokenType::String(Quote::Single),
                    text: "yeet\\".to_string(),
                }
            ],
        );
    }

    #[test]
    fn assign_escaped_string() {
        let mut ctx = Ctx::default();

        let tokens = ctx
            .tokenize(&["my.escaped.string = \"a\\u93f2nope\""])
            .unwrap();
        assert_eq!(
            tokens,
            [
                Token {
                    range: Range {
                        start: Pos { line: 0, char: 0 },
                        end: Pos { line: 0, char: 2 },
                    },
                    typ: TokenType::Ident,
                    text: "my".to_string(),
                },
                Token {
                    range: Range {
                        start: Pos { line: 0, char: 2 },
                        end: Pos { line: 0, char: 3 },
                    },
                    typ: TokenType::Dot,
                    text: ".".to_string(),
                },
                Token {
                    range: Range {
                        start: Pos { line: 0, char: 3 },
                        end: Pos { line: 0, char: 10 },
                    },
                    typ: TokenType::Ident,
                    text: "escaped".to_string(),
                },
                Token {
                    range: Range {
                        start: Pos { line: 0, char: 10 },
                        end: Pos { line: 0, char: 11 },
                    },
                    typ: TokenType::Dot,
                    text: ".".to_string(),
                },
                Token {
                    range: Range {
                        start: Pos { line: 0, char: 11 },
                        end: Pos { line: 0, char: 17 },
                    },
                    typ: TokenType::Ident,
                    text: "string".to_string(),
                },
                Token {
                    range: Range {
                        start: Pos { line: 0, char: 18 },
                        end: Pos { line: 0, char: 19 },
                    },
                    typ: TokenType::Equal,
                    text: "=".to_string(),
                },
                Token {
                    range: Range {
                        start: Pos { line: 0, char: 20 },
                        end: Pos { line: 0, char: 33 },
                    },
                    typ: TokenType::String(Quote::Double),
                    text: "a\u{93f2}nope".to_string(),
                }
            ],
        );
    }
}
