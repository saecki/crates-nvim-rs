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
    rhs: bool,
    stack: Vec<Struct>,
    tokens: Vec<Token>,
    str: Option<StrState>,
    lit_start: Pos,
    lit: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
    start: Pos,
    count: u8,
    cp: u32,
}

impl Ctx {
    pub fn tokenize(&mut self, input: &str) -> Result<Vec<Token>, Error> {
        let mut state = State::default();

        let mut chars = input.char_indices().peekable();
        while let Some((ci, c)) = chars.next() {
            state.pos.char = ci - state.line_start;

            if let Some(str) = &mut state.str {
                if let Some(esc) = &mut str.esc {
                    if let Some(esc) = &mut esc.unicode {
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

                        if esc.count == 0 {
                            match char::from_u32(esc.cp) {
                                Some(char) => state.lit.push(char),
                                None => self.errors.push(Error::InvalidUnicodeScalar(
                                    esc.cp,
                                    Range {
                                        start: esc.start,
                                        end: state.pos.after(c),
                                    },
                                )),
                            }
                            str.esc = None;
                        }
                    } else if c == 'u' {
                        esc.unicode = Some(UnicodeState {
                            start: state.pos,
                            count: 4,
                            cp: 0,
                        });
                    } else if c == 'U' {
                        esc.unicode = Some(UnicodeState {
                            start: state.pos,
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

                    // TODO: if not in array
                    state.rhs = false;
                    self.pop_inline_table_pars(&mut state, c);
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

    fn close_par(&mut self, state: &mut State, par: Par) {
        let mut pars = state.stack.iter().rev();
        while let Some(&p) = pars.next() {
            todo!("close par")
        }
    }

    fn pop_inline_table_pars(&mut self, state: &mut State, c: char) {
        let i = state.stack.iter().position(|s| s.supports_multiline());
        if let Some(i) = i {
            let num_unclosed_structs = state.stack.len() - i;
            for _ in 0..num_unclosed_structs {
                let s = state.stack.pop().expect("");
                match s {
                    Struct::TableHeader => {
                        self.errors
                            .push(Error::MissingRightPar(s, state.pos.after(c)));
                    }
                    Struct::InlineTable => {
                        self.errors
                            .push(Error::MissingRightPar(s, state.pos.after(c)));
                    }
                    Struct::Array => unreachable!("Arrays support multiple lines"),
                }
            }
        }
    }
}
