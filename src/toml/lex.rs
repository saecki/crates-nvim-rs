use std::borrow::Cow;
use std::fmt::Write;
use std::iter::Peekable;

use serde::{Deserialize, Serialize};

use crate::toml::{Ctx, Error};

#[cfg(test)]
mod test;

#[derive(Clone, Debug, PartialEq)]
pub struct Token<'a> {
    pub ty: TokenType<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum TokenType<'a> {
    String {
        quote: Quote,
        /// The literal exactly as it is written in the toml file.
        lit: &'a str,
        /// The text with escape sequences evaluated
        text: Cow<'a, str>,
        /// The span of the text without quotes
        text_span: Span,
    },
    LiteralOrIdent(&'a str),
    /// Contains all the text following a `#` excluding the next newline.
    Comment(&'a str),
    SquareLeft,
    SquareRight,
    CurlyLeft,
    CurlyRight,
    Equal,
    Comma,
    Dot,
    Newline,
    EOF,
}

impl std::fmt::Display for TokenType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::String { lit, .. } => f.write_str(lit),
            TokenType::LiteralOrIdent(lit) => f.write_str(lit),
            TokenType::Comment(text) => write!(f, "#{text}"),
            TokenType::SquareLeft => f.write_char('['),
            TokenType::SquareRight => f.write_char(']'),
            TokenType::CurlyLeft => f.write_char('{'),
            TokenType::CurlyRight => f.write_char('{'),
            TokenType::Equal => f.write_char('='),
            TokenType::Comma => f.write_char(','),
            TokenType::Dot => f.write_char('.'),
            TokenType::Newline => f.write_str("\\n"),
            TokenType::EOF => f.write_str("EOF"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Span {
    #[inline(always)]
    pub fn new(start: Pos, end: Pos) -> Self {
        Self { start, end }
    }

    #[inline(always)]
    pub fn from_pos_len(start: Pos, len: u32) -> Self {
        Self {
            start,
            end: start.plus(len),
        }
    }

    #[inline(always)]
    pub fn pos(pos: Pos) -> Self {
        Self {
            start: pos,
            end: pos,
        }
    }

    #[inline(always)]
    fn ascii_char(pos: Pos) -> Self {
        Self {
            start: pos,
            end: pos.plus(1),
        }
    }

    #[inline(always)]
    pub fn across(a: Self, b: Self) -> Self {
        Self {
            start: a.start,
            end: b.end,
        }
    }

    #[inline(always)]
    pub fn between(a: Self, b: Self) -> Self {
        Self {
            start: a.end,
            end: b.start,
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
    #[inline(always)]
    fn after(&self, c: char) -> Self {
        Self {
            line: self.line,
            char: self.char + c.len_utf8() as u32,
        }
    }

    #[inline(always)]
    pub fn plus(&self, n: u32) -> Self {
        Self {
            line: self.line,
            char: self.char + n,
        }
    }

    #[inline(always)]
    pub fn minus(&self, n: u32) -> Self {
        Self {
            line: self.line,
            char: self.char - n,
        }
    }
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
        self.char() == c
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
    /// All errors are stored inside the [`Ctx`]. If a fatal error occurs, a unit error
    /// is returned, otherwise the possibly partially invalid token stream is returned.
    pub fn lex<'a>(&mut self, input: &'a str) -> Vec<Token<'a>> {
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
                                self.errors.push(Error::UnfinishedEscapeSequence(Span {
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
                                    Span::new(esc.start, lexer.pos.after(c)),
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
                            let text_span = Span::pos(lexer.pos);
                            let token = Token {
                                span: Span::from_pos_len(lit_start, 2),
                                ty: TokenType::String {
                                    quote,
                                    lit: &input[ci..ci + 2],
                                    text: Cow::Borrowed(&input[ci + 1..ci + 1]),
                                    text_span,
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
                            let text_span = Span::pos(lexer.pos);
                            let token = Token {
                                span: Span::from_pos_len(lit_start, 2),
                                ty: TokenType::String {
                                    quote,
                                    lit: &input[ci..ci + 2],
                                    text: Cow::Borrowed(&input[ci + 1..ci + 1]),
                                    text_span,
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
                '#' => self.comment(&mut chars, &mut lexer, ci),
                _ => self.push_literal(&mut lexer, ci),
            }
        }

        // Set the position to the end of the last char
        let end = input.len();
        lexer.pos.char = (end - lexer.line_start) as u32;
        if let Some(str) = &mut lexer.str {
            if let Some(esc) = &mut str.esc {
                self.errors.push(Error::UnfinishedEscapeSequence(Span {
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

        lexer.tokens.push(Token {
            ty: TokenType::EOF,
            span: Span::pos(lexer.pos),
        });

        lexer.tokens
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
        let span = Span::new(lexer.lit_start, lexer.pos);
        let ty = TokenType::LiteralOrIdent(lit);
        let token = Token { span, ty };
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

        let lit_span = Span {
            start: lexer.lit_start,
            end: Pos {
                line: lexer.pos.line,
                char: (lit_byte_end - lexer.line_start) as u32,
            },
        };
        let text_span = Span {
            start: str.text_start,
            end: Pos {
                line: lexer.pos.line,
                char: (text_byte_end - lexer.line_start) as u32,
            },
        };
        let token = Token {
            span: lit_span,
            ty: TokenType::String {
                quote,
                lit,
                text,
                text_span,
            },
        };
        lexer.tokens.push(token);

        lexer.str = None;
        lexer.in_lit = false;
    }

    fn char_token(&mut self, lexer: &mut Lexer, ty: TokenType<'static>, char_index: usize) {
        self.finish_literal(lexer, char_index);

        lexer.tokens.push(Token {
            span: Span::ascii_char(lexer.pos),
            ty,
        });
    }

    fn newline_token(&mut self, lexer: &mut Lexer, char_index: usize) {
        self.finish_literal(lexer, char_index);

        lexer.tokens.push(Token {
            span: Span {
                start: lexer.pos,
                end: Pos {
                    line: lexer.pos.line + 1,
                    char: 0,
                },
            },
            ty: TokenType::Newline,
        });
    }

    fn comment<C>(&mut self, chars: &mut Peekable<C>, lexer: &mut Lexer, char_index: usize)
    where
        C: Iterator<Item = (usize, char)>,
    {
        self.finish_literal(lexer, char_index);

        let text_start = char_index + 1;
        let mut text_end = text_start;
        while let Some((ci, c)) = chars.peek() {
            match c {
                '\n' => break,
                _ => {
                    text_end = ci + c.len_utf8();
                    chars.next();
                }
            }
        }

        let text = &lexer.input[text_start..text_end];
        let end_pos = lexer.pos.plus(1 + text.len() as u32);
        lexer.tokens.push(Token {
            span: Span::new(lexer.pos, end_pos),
            ty: TokenType::Comment(text),
        });

        lexer.pos = end_pos;
    }
}
