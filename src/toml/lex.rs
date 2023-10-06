use std::borrow::Cow;

use serde::{Deserialize, Serialize};

use crate::toml::{Ctx, Error};

#[cfg(test)]
mod test;

#[derive(Debug, PartialEq)]
pub struct Token<'a> {
    pub ty: TokenType<'a>,
    pub range: Range,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum TokenType<'a> {
    Ident(&'a str),
    String {
        quote: Quote,
        /// The literal exactly as it is written in the toml file.
        lit: &'a str,
        /// The text with escape sequences evaluated
        text: Cow<'a, str>,
        /// The range of the text without quotes
        text_range: Range,
    },
    Int(i64, &'a str),
    Float(f64, &'a str),
    Bool(bool, &'a str),
    SquareLeft,
    SquareRight,
    CurlyLeft,
    CurlyRight,
    Equal,
    Comma,
    Dot,
    Newline,
    Invalid(&'a str),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Range {
    pub start: Pos,
    pub end: Pos,
}

impl Range {
    fn ascii_char(pos: Pos) -> Self {
        Self {
            start: pos,
            end: pos.plus(1),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct Pos {
    /// 0-based index of line
    pub line: u32,
    /// utf-8 byte index of line
    pub char: u32,
}

impl Pos {
    fn after(&self, c: char) -> Self {
        Self {
            line: self.line,
            char: self.char + c.len_utf8() as u32,
        }
    }

    fn plus(&self, n: u32) -> Self {
        Self {
            line: self.line,
            char: self.char + n,
        }
    }
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

#[derive(Debug)]
struct Lexer<'a> {
    input: &'a str,
    /// Byte index of line start
    line_start: usize,
    pos: Pos,
    tokens: Vec<Token<'a>>,
    str: Option<StrState>,
    in_lit: bool,
    lit_start: Pos,
    lit_byte_start: usize,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            line_start: 0,
            pos: Pos::default(),
            tokens: Vec::new(),
            str: None,
            in_lit: false,
            lit_start: Pos::default(),
            lit_byte_start: 0,
        }
    }

    fn newline(&mut self, char_index: usize) {
        self.line_start = char_index + 1;
        self.pos.line += 1;
        self.pos.char = 0;
    }
}

#[derive(Debug)]
struct StrState {
    /// Only used when there are escapes so we can't reference the original string,
    text: Option<String>,
    text_start: Pos,
    text_byte_start: usize,
    esc: Option<EscState>,
    quote: Quote,
}

impl StrState {
    fn push_char(&mut self, c: char) {
        if let Some(text) = &mut self.text {
            text.push(c);
        }
    }
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
    pub fn lex<'a>(&mut self, input: &'a str) -> Result<Vec<Token<'a>>, Error> {
        let mut lexer = Lexer::new(input);

        let mut chars = input.char_indices().peekable();
        while let Some((ci, c)) = chars.next() {
            lexer.pos.char = (ci - lexer.line_start) as u32;

            if let Some(str) = &mut lexer.str {
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
                                    end: lexer.pos.after(c),
                                }));

                                if str.quote.is_multiline() {
                                    lexer.newline(ci);

                                    // eat whitespace
                                    while let Some((_, ' ' | '\t')) = chars.peek() {
                                        chars.next();
                                        lexer.pos.char += 1;
                                    }
                                } else {
                                    // Recover state
                                    let quote = str.quote;
                                    self.errors.push(Error::MissingQuote(quote, lexer.pos));
                                    self.finish_string(&mut lexer, quote, ci, ci);
                                    lexer.str = None;

                                    self.newline_token(&mut lexer, ci);
                                    lexer.newline(ci);
                                }
                                continue;
                            }
                            _ => {
                                self.errors
                                    .push(Error::InvalidUnicodeEscapeChar(c, lexer.pos));

                                if str.quote.matches(c) {
                                    let text_end = ci;
                                    if str.quote.is_multiline() {
                                        if Some(str.quote.char()) == chars.peek().map(|(_, c)| *c) {
                                            chars.next();
                                            lexer.pos.char += 1;
                                        } else {
                                            str.push_char(c);
                                            continue;
                                        }

                                        if Some(str.quote.char()) == chars.peek().map(|(_, c)| *c) {
                                            chars.next();
                                            lexer.pos.char += 1;
                                        } else {
                                            str.push_char(c);
                                            str.push_char(c);
                                            continue;
                                        }
                                    }

                                    // Recover state
                                    let quote = str.quote;
                                    let lit_end = lexer.line_start + lexer.pos.char as usize + 1;
                                    self.finish_string(&mut lexer, quote, text_end, lit_end);
                                    lexer.str = None;
                                    continue;
                                }
                            }
                        }

                        if unicode.count == 0 {
                            match char::from_u32(unicode.cp) {
                                Some(char) => str.push_char(char),
                                None => self.errors.push(Error::InvalidUnicodeScalar(
                                    unicode.cp,
                                    Range {
                                        start: esc.start,
                                        end: lexer.pos.after(c),
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
                        str.esc = None;
                        match c {
                            'b' => str.push_char('\u{8}'),
                            't' => str.push_char('\t'),
                            'n' => str.push_char('\n'),
                            'f' => str.push_char('\u{C}'),
                            'r' => str.push_char('\r'),
                            '"' => str.push_char('"'),
                            '\\' => str.push_char('\\'),
                            '\n' => {
                                if !str.quote.is_multiline() {
                                    // Recover state
                                    let quote = str.quote;
                                    self.errors.push(Error::MissingQuote(quote, lexer.pos));
                                    self.finish_string(&mut lexer, quote, ci, ci);

                                    self.newline_token(&mut lexer, ci);
                                    lexer.newline(ci);
                                    continue;
                                }

                                // Newline was escaped
                                lexer.newline(ci);

                                // eat whitespace
                                while let Some((_, ' ' | '\t')) = chars.peek() {
                                    chars.next();
                                    lexer.pos.char += 1;
                                }
                            }
                            _ => self.errors.push(Error::InvalidEscapeChar(c, lexer.pos)),
                        }
                    }
                } else if str.quote.matches(c) {
                    let text_end = ci;
                    if str.quote.is_multiline() {
                        if Some(str.quote.char()) == chars.peek().map(|(_, c)| *c) {
                            chars.next();
                            lexer.pos.char += 1;
                        } else {
                            str.push_char(c);
                            continue;
                        }

                        if Some(str.quote.char()) == chars.peek().map(|(_, c)| *c) {
                            chars.next();
                            lexer.pos.char += 1;
                        } else {
                            str.push_char(c);
                            str.push_char(c);
                            continue;
                        }
                    }

                    let quote = str.quote;
                    let lit_end = lexer.line_start + lexer.pos.char as usize + 1;
                    self.finish_string(&mut lexer, quote, text_end, lit_end);
                    lexer.str = None;
                } else if c == '\n' {
                    if !str.quote.is_multiline() {
                        // Recover state
                        self.errors.push(Error::MissingQuote(str.quote, lexer.pos));
                        let quote = str.quote;
                        self.finish_string(&mut lexer, quote, ci, ci);
                        lexer.str = None;

                        self.newline_token(&mut lexer, ci);
                        lexer.newline(ci);
                        continue;
                    }

                    str.push_char(c);
                    lexer.newline(ci);
                } else if str.quote.is_basic() && c == '\\' {
                    if str.text.is_none() {
                        str.text = Some(String::from(&input[str.text_byte_start..ci]));
                    }
                    str.esc = Some(EscState {
                        start: lexer.pos,
                        unicode: None,
                    });
                } else {
                    str.push_char(c);
                }
                continue;
            }

            match c {
                '\n' => {
                    self.newline_token(&mut lexer, ci);
                    lexer.newline(ci);
                }
                '\t' | ' ' => self.finish_literal(&mut lexer, ci),
                '"' => {
                    self.finish_literal(&mut lexer, ci);

                    lexer.lit_byte_start = ci;
                    let lit_start = lexer.pos;
                    let mut quote = Quote::Basic;
                    if Some('"') == chars.peek().map(|(_, c)| *c) {
                        chars.next();
                        lexer.pos.char += 1;

                        if Some('"') == chars.peek().map(|(_, c)| *c) {
                            // It's a multiline string
                            chars.next();
                            lexer.pos.char += 1;

                            quote = Quote::BasicMultiline;
                        } else {
                            // It's just an empty string
                            let text_range = Range {
                                start: lexer.pos,
                                end: lexer.pos,
                            };
                            let token = Token {
                                range: Range {
                                    start: lit_start,
                                    end: Pos {
                                        line: lit_start.line,
                                        char: lit_start.char + 2,
                                    },
                                },
                                ty: TokenType::String {
                                    quote,
                                    lit: &input[ci..ci + 2],
                                    text: Cow::Borrowed(&input[ci + 1..ci + 1]),
                                    text_range,
                                },
                            };
                            lexer.tokens.push(token);
                            continue;
                        }
                    }

                    let mut text_start = lexer.pos;
                    text_start.char += 1;
                    let text_byte_start = lexer.line_start + text_start.char as usize;
                    lexer.str = Some(StrState {
                        text: None,
                        text_start,
                        text_byte_start,
                        esc: None,
                        quote,
                    });
                    lexer.lit_start = lit_start;
                }
                '\'' => {
                    self.finish_literal(&mut lexer, ci);

                    lexer.lit_byte_start = ci;
                    let lit_start = lexer.pos;
                    let mut quote = Quote::Literal;
                    if Some('\'') == chars.peek().map(|(_, c)| *c) {
                        chars.next();
                        lexer.pos.char += 1;

                        if Some('\'') == chars.peek().map(|(_, c)| *c) {
                            // It's a multiline string
                            chars.next();
                            lexer.pos.char += 1;

                            quote = Quote::LiteralMultiline;
                        } else {
                            // It's just an empty string
                            let text_range = Range {
                                start: lexer.pos,
                                end: lexer.pos,
                            };
                            let token = Token {
                                range: Range {
                                    start: lit_start,
                                    end: Pos {
                                        line: lit_start.line,
                                        char: lit_start.char + 2,
                                    },
                                },
                                ty: TokenType::String {
                                    quote,
                                    lit: &input[ci..ci + 2],
                                    text: Cow::Borrowed(&input[ci + 1..ci + 1]),
                                    text_range,
                                },
                            };
                            lexer.tokens.push(token);
                            continue;
                        }
                    }

                    let mut text_start = lexer.pos;
                    text_start.char += 1;
                    let text_byte_start = lexer.line_start + text_start.char as usize;
                    lexer.str = Some(StrState {
                        text: None,
                        text_start,
                        text_byte_start,
                        esc: None,
                        quote,
                    });
                    lexer.lit_start = lit_start;
                }
                '[' => self.char_token(&mut lexer, TokenType::SquareLeft, ci),
                ']' => self.char_token(&mut lexer, TokenType::SquareRight, ci),
                '{' => self.char_token(&mut lexer, TokenType::CurlyLeft, ci),
                '}' => self.char_token(&mut lexer, TokenType::CurlyRight, ci),
                '=' => self.char_token(&mut lexer, TokenType::Equal, ci),
                '.' => self.char_token(&mut lexer, TokenType::Dot, ci),
                ',' => self.char_token(&mut lexer, TokenType::Comma, ci),
                '#' => todo!("comment"),
                _ => self.push_literal(&mut lexer, ci),
            }
        }

        // Set the position to the end of the last char
        let end = input.len();
        lexer.pos.char = (end - lexer.line_start) as u32;
        if let Some(str) = &mut lexer.str {
            if let Some(esc) = &mut str.esc {
                self.errors.push(Error::UnfinishedEscapeSequence(Range {
                    start: esc.start,
                    end: lexer.pos,
                }));
            }
            let quote = str.quote;
            self.errors.push(Error::MissingQuote(quote, lexer.pos));

            self.finish_string(&mut lexer, quote, end, end);
        } else {
            self.finish_literal(&mut lexer, end);
        }

        Ok(lexer.tokens)
    }

    fn push_literal(&mut self, lexer: &mut Lexer, char_index: usize) {
        if !lexer.in_lit {
            lexer.lit_start = lexer.pos;
            lexer.lit_byte_start = char_index;
            lexer.in_lit = true;
        }
    }

    fn finish_literal(&mut self, lexer: &mut Lexer, lit_end: usize) {
        if !lexer.in_lit {
            return;
        }
        let lit = &lexer.input[lexer.lit_byte_start..lit_end];

        let range = Range {
            start: lexer.lit_start,
            end: lexer.pos,
        };

        // TODO: custom integer parser to support binary, oct, and hex literals, and adhere to the
        // toml spec
        let ty = if let Ok(i) = lit.parse::<i64>() {
            TokenType::Int(i, lit)
        // TODO: recognize an exponnent while parsing an integer and then store that exponent
        // separately to compute the precise float later
        } else if let Ok(f) = lit.parse::<f64>() {
            // Since dots won't be included in literals, floats are only identified if they
            // contain an exponent. Otherwise they are identified as a later float during parsing
            // when these kinds of tokens are found, when parsing a rhs expression:
            // int dot int|float
            //
            // The following would imply that the first token contains an exponent that would
            // preceed the fractional part which is invalid:
            // float dot int|float
            TokenType::Float(f, lit)
        } else {
            match lit {
                "true" => TokenType::Bool(true, lit),
                "false" => TokenType::Bool(false, lit),
                "nan" => TokenType::Float(f64::NAN, lit),
                "+nan" => TokenType::Float(f64::NAN, lit),
                "-nan" => TokenType::Float(-f64::NAN, lit),
                "inf" => TokenType::Float(f64::INFINITY, lit),
                "+inf" => TokenType::Float(f64::INFINITY, lit),
                "-inf" => TokenType::Float(-f64::NEG_INFINITY, lit),
                // TODO date and time
                _ => {
                    let is_valid_ident = lit
                        .chars()
                        .all(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'));

                    if is_valid_ident {
                        TokenType::Ident(lit)
                    } else {
                        self.errors
                            .push(Error::InvalidLiteral(lit.to_string(), range.clone()));
                        TokenType::Invalid(lit)
                    }
                }
            }
        };

        let token = Token { range, ty };
        lexer.tokens.push(token);

        lexer.in_lit = false;
    }

    fn finish_string(
        &mut self,
        lexer: &mut Lexer,
        quote: Quote,
        text_byte_end: usize,
        lit_byte_end: usize,
    ) {
        let lit = &lexer.input[lexer.lit_byte_start..lit_byte_end];
        let Some(str) = &mut lexer.str else { return };

        let text = match str.text.take() {
            Some(text) => Cow::Owned(text),
            None => Cow::Borrowed(&lexer.input[str.text_byte_start..text_byte_end]),
        };

        let lit_range = Range {
            start: lexer.lit_start,
            end: Pos {
                line: lexer.pos.line,
                char: (lit_byte_end - lexer.line_start) as u32,
            },
        };
        let text_range = Range {
            start: str.text_start,
            end: Pos {
                line: lexer.pos.line,
                char: (text_byte_end - lexer.line_start) as u32,
            },
        };
        let token = Token {
            range: lit_range,
            ty: TokenType::String {
                quote,
                lit,
                text,
                text_range,
            },
        };
        lexer.tokens.push(token);

        lexer.str = None;
        lexer.in_lit = false;
    }

    fn char_token(&mut self, lexer: &mut Lexer, ty: TokenType<'static>, char_index: usize) {
        self.finish_literal(lexer, char_index);

        lexer.tokens.push(Token {
            range: Range::ascii_char(lexer.pos),
            ty,
        });
    }

    fn newline_token(&mut self, lexer: &mut Lexer, char_index: usize) {
        self.finish_literal(lexer, char_index);

        lexer.tokens.push(Token {
            range: Range {
                start: lexer.pos,
                end: Pos {
                    line: lexer.pos.line + 1,
                    char: 0,
                },
            },
            ty: TokenType::Newline,
        });
    }
}
