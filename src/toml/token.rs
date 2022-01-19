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
    pub fn char(line: usize, char: usize) -> Self {
        Self {
            start: Pos { line, char },
            end: Pos {
                line,
                char: char + 1,
            },
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct Pos {
    line: usize,
    char: usize,
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
}

#[derive(Default)]
struct State {
    tokens: Vec<Token>,
    str: Option<StrState>,
    lit_start: Pos,
    lit: String,
}

struct StrState {
    start: Pos,
    esc: bool,
    quote: Quote,
}

impl Ctx {
    pub fn tokenize(&mut self, lines: &[impl AsRef<str>]) -> Result<Vec<Token>, Error> {
        let mut state = State::default();
        let mut _li = 0;
        let mut _ci = 0;

        for (li, l) in lines.iter().enumerate() {
            for (ci, c) in l.as_ref().chars().enumerate() {
                _li = li;
                _ci = ci; // TODO: cleanup

                if let Some(str) = &mut state.str {
                    if !str.esc && str.quote.matches(c) {
                        let lit = state.lit.clone();
                        state.lit.clear();

                        let token = Token {
                            range: Range {
                                start: str.start,
                                end: Pos {
                                    line: li,
                                    char: ci + 1,
                                },
                            },
                            typ: TokenType::String(str.quote),
                            text: lit,
                        };
                        state.tokens.push(token);

                        state.str = None;
                    } else {
                        if !str.esc && c == '\\' {
                            str.esc = true;
                        } else {
                            str.esc = false;
                        }
                        state.lit.push(c);
                    }
                    continue;
                }

                match c {
                    '\t' | ' ' => self.finish_literal(&mut state, li, ci),
                    '[' => self.char_token(&mut state, c, TokenType::Par(Par::SqareLeft), li, ci),
                    ']' => self.char_token(&mut state, c, TokenType::Par(Par::SqareRight), li, ci),
                    '{' => self.char_token(&mut state, c, TokenType::Par(Par::CurlyLeft), li, ci),
                    '}' => self.char_token(&mut state, c, TokenType::Par(Par::CurlyRight), li, ci),
                    '=' => self.char_token(&mut state, c, TokenType::Equal, li, ci),
                    '.' => self.char_token(&mut state, c, TokenType::Dot, li, ci),
                    _ => self.push_literal(&mut state, c, li, ci),
                }
            }
        }

        self.finish_literal(&mut state, _li, _ci);

        return Ok(state.tokens);
    }

    fn push_literal(&mut self, state: &mut State, char: char, li: usize, ci: usize) {
        if state.lit.is_empty() {
            state.lit_start = Pos { line: li, char: ci };
        }
        state.lit.push(char);
    }

    fn finish_literal(&mut self, state: &mut State, li: usize, ci: usize) {
        if !state.lit.is_empty() {
            let lit = state.lit.clone();
            state.lit.clear();

            if let Ok(i) = lit.parse::<i64>() {

            }

            let token = Token {
                range: Range {
                    start: state.lit_start,
                    end: Pos { line: li, char: ci },
                },
                typ: TokenType::Ident,
                text: lit,
            };
            state.tokens.push(token);
        }
    }

    fn char_token(&mut self, state: &mut State, char: char, typ: TokenType, li: usize, ci: usize) {
        self.finish_literal(state, li, ci);

        state.tokens.push(Token {
            range: Range::char(li, ci),
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

        let tokens = ctx.tokenize(&["my_num = 98742"]).unwrap();
        assert_eq!(
            tokens,
            [
                Token {
                    range: Range {
                        start: Pos { line: 0, char: 0 },
                        end: Pos { line: 0, char: 6 },
                    },
                    typ: TokenType::Ident,
                    text: "my_num".to_string(),
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
}
