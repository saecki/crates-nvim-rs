use serde::{Deserialize, Serialize};

use crate::toml::{Ctx, Error};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Token {
    range: Range,
    typ: TokenType,
    text: String,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
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
    Dot,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Par {
    SqareLeft,
    SqareRight,
    CurlyLeft,
    CurlyRight,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Quote {
    Singe,
    Double,
}

impl Quote {
    fn matches(self, char: char) -> bool {
        match self {
            Self::Singe => char == '\'',
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

#[derive(Default)]
struct State {
    pos: Pos,
    tokens: Vec<Token>,
    str: Option<StrState>,
    lit_start: Pos,
    lit_is_num: bool,
    lit: String,
}

struct StrState {
    esc: bool,
    esc_unicode: Option<UnicodeState>,
    quote: Quote,
}

struct UnicodeState {
    start: Pos,
    count: u8,
    cp: u32,
}

impl Ctx {
    pub fn tokenize(&mut self, lines: &[impl AsRef<str>]) -> Result<Vec<Token>, Error> {
        let mut state = State::default();

        for (li, l) in lines.iter().enumerate() {
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
                    '[' => self.char_token(&mut state, c, TokenType::Par(Par::SqareLeft)),
                    ']' => self.char_token(&mut state, c, TokenType::Par(Par::SqareRight)),
                    '{' => self.char_token(&mut state, c, TokenType::Par(Par::CurlyLeft)),
                    '}' => self.char_token(&mut state, c, TokenType::Par(Par::CurlyRight)),
                    '=' => self.char_token(&mut state, c, TokenType::Equal),
                    '.' => {
                        if state.lit_is_num {
                            self.push_literal(&mut state, c);
                        } else {
                            self.char_token(&mut state, c, TokenType::Dot);
                        }
                    }
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
            self.finish_string(&mut state, quote);
        } else {
            self.finish_literal(&mut state);
        }

        Ok(state.tokens)
    }

    fn push_literal(&mut self, state: &mut State, char: char) {
        if state.lit.is_empty() {
            state.lit_start = state.pos;
            state.lit_is_num = ('0'..='9').contains(&char);
        }
        state.lit.push(char);
    }

    fn finish_literal(&mut self, state: &mut State) {
        if !state.lit.is_empty() {
            let text = state.lit.clone();
            state.lit.clear();

            let typ = if let Ok(i) = text.parse::<i64>() {
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
                    _ => TokenType::Ident,
                }
            };

            let token = Token {
                range: Range {
                    start: state.lit_start,
                    end: state.pos,
                },
                typ,
                text,
            };
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
}
