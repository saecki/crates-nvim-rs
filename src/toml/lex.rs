use serde::{Deserialize, Serialize};

use crate::toml::{Ctx, Error};

#[cfg(test)]
mod test;

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
    fn char(pos: Pos, c: char) -> Self {
        Self {
            start: pos,
            end: pos.after(c),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct Pos {
    /// 0-based index of line
    line: usize,
    /// utf-8 byte index of line
    char: usize,
}

impl Pos {
    fn after(&self, c: char) -> Self {
        Self {
            line: self.line,
            char: self.char + c.len_utf8(),
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
    SquareLeft,
    SquareRight,
    CurlyLeft,
    CurlyRight,
    Equal,
    Comma,
    Dot,
    Invalid,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Par {}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Quote {
    /// "
    Basic,
    /// """
    BasicMultiline,
    /// '
    Literal,
    /// '''
    LiteralMultiline,
}

impl Quote {
    fn is_basic(&self) -> bool {
        matches!(self, Self::Basic | Self::BasicMultiline)
    }

    fn is_multiline(&self) -> bool {
        matches!(self, Self::BasicMultiline | Self::LiteralMultiline)
    }

    fn matches(&self, c: char) -> bool {
        match (c, self) {
            ('\"', Self::Basic | Self::BasicMultiline) => true,
            ('\'', Self::Literal | Self::LiteralMultiline) => true,
            _ => false,
        }
    }

    fn char(&self) -> char {
        match self {
            Quote::Basic | Quote::BasicMultiline => '"',
            Quote::Literal | Quote::LiteralMultiline => '\'',
        }
    }
}

#[derive(Debug, Default)]
struct State {
    /// Byte index of line start
    line_start: usize,
    pos: Pos,
    tokens: Vec<Token>,
    str: Option<StrState>,
    lit_start: Pos,
    lit: String,
}

#[derive(Debug)]
struct StrState {
    esc: Option<EscState>,
    quote: Quote,
}

#[derive(Debug)]
struct EscState {
    start: Pos,
    unicode: Option<UnicodeState>,
}

#[derive(Debug)]
struct UnicodeState {
    count: u8,
    cp: u32,
}

impl Ctx {
    pub fn lex(&mut self, input: &str) -> Result<Vec<Token>, Error> {
        let mut state = State::default();

        let mut chars = input.char_indices().peekable();
        while let Some((ci, c)) = chars.next() {
            state.pos.char = ci - state.line_start;

            if let Some(str) = &mut state.str {
                if let Some(esc) = &mut str.esc {
                    if let Some(unicode) = &mut esc.unicode {
                        unicode.count -= 1;

                        let offset = unicode.count * 4;
                        match c {
                            '0'..='9' => {
                                unicode.cp += (c as u32 - '0' as u32) << offset;
                            }
                            'a'..='f' => {
                                unicode.cp += (c as u32 - 'a' as u32 + 10) << offset;
                            }
                            'A'..='F' => {
                                unicode.cp += (c as u32 - 'A' as u32 + 10) << offset;
                            }
                            '\n' => {
                                self.errors.push(Error::UnfinishedEscapeSequence(Range {
                                    start: esc.start,
                                    end: state.pos.after(c),
                                }));

                                if str.quote.is_multiline() {
                                    state.line_start = ci + 1;
                                    state.pos.line += 1;
                                    state.pos.char = 0;

                                    // eat whitespace
                                    while let Some((_, ' ' | '\t')) = chars.peek() {
                                        chars.next();
                                        state.line_start += 1;
                                    }
                                } else {
                                    // Recover state
                                    let quote = str.quote;
                                    self.errors.push(Error::MissingQuote(quote, state.pos));
                                    self.finish_string(&mut state, quote, false);
                                    state.str = None;

                                    state.line_start = ci + 1;
                                    state.pos.line += 1;
                                    state.pos.char = 0;
                                }
                                continue;
                            }
                            _ => {
                                self.errors
                                    .push(Error::InvalidUnicodeEscapeChar(c, state.pos));

                                if str.quote.matches(c) {
                                    if str.quote.is_multiline() {
                                        let mut missing = false;
                                        if Some(str.quote.char()) != chars.peek().map(|(_, c)| *c) {
                                            missing = true;
                                        } else {
                                            chars.next();
                                            state.pos.char += 1;
                                        }

                                        if Some(str.quote.char()) != chars.peek().map(|(_, c)| *c) {
                                            missing = true;
                                        } else {
                                            chars.next();
                                            state.pos.char += 1;
                                        }

                                        if missing {
                                            self.errors
                                                .push(Error::MissingQuote(str.quote, state.pos));
                                        }
                                    }

                                    // Recover state
                                    let quote = str.quote;
                                    self.finish_string(&mut state, quote, true);
                                    state.str = None;
                                    continue;
                                }
                            }
                        }

                        if unicode.count == 0 {
                            match char::from_u32(unicode.cp) {
                                Some(char) => state.lit.push(char),
                                None => self.errors.push(Error::InvalidUnicodeScalar(
                                    unicode.cp,
                                    Range {
                                        start: esc.start,
                                        end: state.pos.after(c),
                                    },
                                )),
                            }
                            str.esc = None;
                        }
                    } else if c == 'u' {
                        esc.unicode = Some(UnicodeState { count: 4, cp: 0 });
                    } else if c == 'U' {
                        esc.unicode = Some(UnicodeState { count: 8, cp: 0 });
                    } else {
                        match c {
                            'b' => state.lit.push('\u{8}'),
                            't' => state.lit.push('\t'),
                            'n' => state.lit.push('\n'),
                            'f' => state.lit.push('\u{D}'),
                            'r' => state.lit.push('\r'),
                            '"' => state.lit.push('"'),
                            '\\' => state.lit.push('\\'),
                            '\n' => {
                                if !str.quote.is_multiline() {
                                    // Recover state
                                    let quote = str.quote;
                                    self.errors.push(Error::MissingQuote(quote, state.pos));
                                    self.finish_string(&mut state, quote, false);
                                    state.str = None;
                                    continue;
                                }

                                // Newline was escaped
                                state.line_start = ci + 1;
                                state.pos.line += 1;
                                state.pos.char = 0;

                                // eat whitespace
                                while let Some((_, ' ' | '\t')) = chars.peek() {
                                    chars.next();
                                    state.line_start += 1;
                                }
                            }
                            _ => self.errors.push(Error::InvalidEscapeChar(c, state.pos)),
                        }
                        str.esc = None;
                    }
                } else if str.quote.matches(c) {
                    if str.quote.is_multiline() {
                        if Some(str.quote.char()) == chars.peek().map(|(_, c)| *c) {
                            chars.next();
                            state.pos.char += 1;
                        } else {
                            state.lit.push(c);
                            continue;
                        }

                        if Some(str.quote.char()) == chars.peek().map(|(_, c)| *c) {
                            chars.next();
                            state.pos.char += 1;
                        } else {
                            state.lit.push(c);
                            state.lit.push(c);
                            continue;
                        }
                    }

                    let quote = str.quote;
                    self.finish_string(&mut state, quote, true);
                    state.str = None;
                } else if c == '\n' {
                    if !str.quote.is_multiline() {
                        // Recover state
                        self.errors.push(Error::MissingQuote(str.quote, state.pos));
                        let quote = str.quote;
                        self.finish_string(&mut state, quote, false);
                        state.str = None;

                        state.line_start = ci + 1;
                        state.pos.line += 1;
                        state.pos.char = 0;
                        continue;
                    } else {
                        state.line_start = ci + 1;
                        state.pos.line += 1;
                        state.pos.char = 0;
                        state.lit.push(c);
                    }
                } else if str.quote.is_basic() && c == '\\' {
                    str.esc = Some(EscState {
                        start: state.pos,
                        unicode: None,
                    });
                } else {
                    state.lit.push(c);
                }
                continue;
            }

            match c {
                '\n' => {
                    state.line_start = ci + 1;
                    state.pos.line += 1;
                    state.pos.char = 0;
                }
                '\t' | ' ' => self.finish_literal(&mut state),
                '"' => {
                    self.finish_literal(&mut state);

                    let start = state.pos;
                    let mut quote = Quote::Basic;
                    if Some('"') == chars.peek().map(|(_, c)| *c) {
                        chars.next();
                        state.pos.char += 1;

                        if Some('"') == chars.peek().map(|(_, c)| *c) {
                            // It's a multiline string
                            chars.next();
                            state.pos.char += 1;

                            quote = Quote::BasicMultiline;
                        } else {
                            // It's just an empty string
                            let token = Token {
                                range: Range {
                                    start,
                                    end: Pos {
                                        line: start.line,
                                        char: start.char + 2,
                                    },
                                },
                                typ: TokenType::String(quote),
                                text: String::new(),
                            };
                            state.tokens.push(token);
                            continue;
                        }
                    }

                    state.str = Some(StrState { esc: None, quote });
                    state.lit_start = start;
                }
                '\'' => {
                    self.finish_literal(&mut state);

                    let start = state.pos;
                    let mut quote = Quote::Literal;
                    if Some('\'') == chars.peek().map(|(_, c)| *c) {
                        chars.next();
                        state.pos.char += 1;

                        if Some('\'') == chars.peek().map(|(_, c)| *c) {
                            // It's a multiline string
                            chars.next();
                            state.pos.char += 1;

                            quote = Quote::LiteralMultiline;
                        } else {
                            // It's just an empty string
                            let token = Token {
                                range: Range {
                                    start,
                                    end: Pos {
                                        line: start.line,
                                        char: start.char + 2,
                                    },
                                },
                                typ: TokenType::String(quote),
                                text: String::new(),
                            };
                            state.tokens.push(token);
                            continue;
                        }
                    }

                    state.str = Some(StrState { esc: None, quote });
                    state.lit_start = start;
                }
                '[' => {
                    self.char_token(&mut state, c, TokenType::SquareLeft);
                }
                ']' => {
                    self.char_token(&mut state, c, TokenType::SquareRight);
                }
                '{' => {
                    self.char_token(&mut state, c, TokenType::CurlyLeft);
                }
                '}' => {
                    self.char_token(&mut state, c, TokenType::CurlyRight);
                }
                '=' => {
                    self.char_token(&mut state, c, TokenType::Equal);
                }
                '.' => {
                    self.char_token(&mut state, c, TokenType::Dot);
                }
                ',' => {
                    self.char_token(&mut state, c, TokenType::Comma);
                }
                '#' => todo!("comment"),
                _ => self.push_literal(&mut state, c),
            }
        }

        if let Some(str) = &mut state.str {
            state.pos.char = input.len() - state.line_start;

            if let Some(esc) = &mut str.esc {
                self.errors.push(Error::UnfinishedEscapeSequence(Range {
                    start: esc.start,
                    end: state.pos,
                }));
            }
            let quote = str.quote;
            self.errors.push(Error::MissingQuote(quote, state.pos));

            self.finish_string(&mut state, quote, false);
        } else {
            // Set the position to the end of the last char
            state.pos.char = input.len() - state.line_start;
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
        if state.lit.is_empty() {
            return;
        }
        let text = state.lit.clone();
        state.lit.clear();

        let range = Range {
            start: state.lit_start,
            end: state.pos,
        };

        // TODO: custom integer parser to support binary, oct, and hex literals, and adhere to the
        // toml spec
        let typ = if let Ok(i) = text.parse::<i64>() {
            TokenType::Int(i)
        // TODO: recognize an exponnent while parsing an integer and then store that exponent
        // separately to compute the precise float later
        } else if let Ok(f) = text.parse::<f64>() {
            // Since dots won't be included in literals, floats are only identified if they
            // contain an exponent. Otherwise they are identified as a later float during parsing
            // when these kinds of tokens are found, when parsing a rhs expression:
            // int dot int|float
            //
            // The following would imply that the first token contains an exponent that would
            // preceed the fractional part which is invalid:
            // float dot int|float
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
                // TODO date and time
                _ => {
                    let is_valid_ident = text
                        .chars()
                        .all(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'));

                    if is_valid_ident {
                        TokenType::Ident
                    } else {
                        self.errors
                            .push(Error::InvalidLiteral(text.clone(), range.clone()));
                        TokenType::Invalid
                    }
                }
            }
        };

        let token = Token { range, typ, text };
        state.tokens.push(token);
    }

    fn finish_string(&mut self, state: &mut State, quote: Quote, end_after_current_pos: bool) {
        let lit = state.lit.clone();
        state.lit.clear();

        let mut token = Token {
            range: Range {
                start: state.lit_start,
                end: state.pos,
            },
            typ: TokenType::String(quote),
            text: lit,
        };
        if end_after_current_pos {
            token.range.end.char += 1;
        }
        state.tokens.push(token);

        state.str = None;
    }

    fn char_token(&mut self, state: &mut State, char: char, typ: TokenType) {
        self.finish_literal(state);

        state.tokens.push(Token {
            range: Range::char(state.pos, char),
            typ,
            text: char.to_string(),
        });
    }
}
