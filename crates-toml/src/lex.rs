use std::borrow::Cow;

use crate::{Ctx, Error};

#[cfg(test)]
mod test;

pub(crate) type CharIter<'a> = std::iter::Peekable<std::str::CharIndices<'a>>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Tokens<'a> {
    pub tokens: Vec<Token>,
    pub strings: Vec<StringToken<'a>>,
    pub literals: Vec<&'a str>,
    pub eof: Token,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Token {
    pub ty: TokenType,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenType {
    String(StringId),
    LiteralOrIdent(LiteralId),
    /// Contains all the text following a `#` excluding the next newline.
    Comment(LiteralId),
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct StringId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LiteralId(pub u32);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringToken<'a> {
    pub quote: Quote,
    /// The literal exactly as it is written in the toml file.
    pub lit: &'a str,
    /// The text with escape sequences evaluated
    pub text: Cow<'a, str>,
    /// The span of the text without quotes
    pub text_span: Span,
}

impl TokenType {
    pub fn display(
        &self,
        f: &mut impl std::fmt::Write,
        strings: &[StringToken<'_>],
        literals: &[&str],
    ) -> std::fmt::Result {
        match self {
            TokenType::String(id) => {
                let string = &strings[id.0 as usize];
                f.write_str(string.lit)
            }
            TokenType::LiteralOrIdent(id) => {
                let lit = &literals[id.0 as usize];
                f.write_str(lit)
            }
            TokenType::Comment(id) => {
                let lit = &literals[id.0 as usize];
                write!(f, "#{lit}")
            }
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pos {
    /// 0-based index of line
    pub line: u32,
    /// utf-8 byte index of line
    pub char: u32,
}

impl Pos {
    #[inline(always)]
    pub fn new(line: u32, char: u32) -> Self {
        Self { line, char }
    }

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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

impl std::fmt::Display for Quote {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Quote::Basic => f.write_str("\""),
            Quote::BasicMultiline => f.write_str("\"\"\""),
            Quote::Literal => f.write_str("'"),
            Quote::LiteralMultiline => f.write_str("'''"),
        }
    }
}

impl Quote {
    pub fn len(&self) -> u32 {
        match self {
            Quote::Basic | Quote::Literal => 1,
            Quote::BasicMultiline | Quote::LiteralMultiline => 3,
        }
    }

    pub fn is_basic(&self) -> bool {
        matches!(self, Self::Basic | Self::BasicMultiline)
    }

    pub fn is_multiline(&self) -> bool {
        matches!(self, Self::BasicMultiline | Self::LiteralMultiline)
    }

    pub fn matches(&self, c: char) -> bool {
        self.char() == c
    }

    pub fn char(&self) -> char {
        match self {
            Quote::Basic | Quote::BasicMultiline => '"',
            Quote::Literal | Quote::LiteralMultiline => '\'',
        }
    }

    fn multiline(&self) -> Self {
        match self {
            Quote::Basic | Quote::BasicMultiline => Self::BasicMultiline,
            Quote::Literal | Quote::LiteralMultiline => Self::LiteralMultiline,
        }
    }
}

#[derive(Debug)]
struct Lexer<'a> {
    input: &'a str,
    chars: CharIter<'a>,

    line_idx: u32,
    line_byte_start: usize,
    byte_pos: usize,

    in_lit: bool,
    lit_start: Pos,
    lit_byte_start: usize,

    tokens: Vec<Token>,
    strings: Vec<StringToken<'a>>,
    literals: Vec<&'a str>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            chars: input.char_indices().peekable(),
            line_idx: 0,
            line_byte_start: 0,
            byte_pos: 0,
            tokens: Vec::new(),
            strings: Vec::new(),
            literals: Vec::new(),
            in_lit: false,
            lit_start: Pos::default(),
            lit_byte_start: 0,
        }
    }

    fn newline(&mut self) {
        self.line_idx += 1;
        self.byte_pos += 1;
        self.line_byte_start = self.byte_pos;
    }

    fn store_string(&mut self, string: StringToken<'a>) -> StringId {
        let id = self.strings.len();
        self.strings.push(string);
        StringId(id as u32)
    }

    fn store_literal(&mut self, lit: &'a str) -> LiteralId {
        let id = self.literals.len();
        self.literals.push(lit);
        LiteralId(id as u32)
    }

    fn next(&mut self) -> Option<char> {
        match self.chars.next() {
            Some((ci, c)) => {
                self.byte_pos = ci;
                Some(c)
            }
            None => {
                self.byte_pos = self.input.len();
                None
            }
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, c)| *c)
    }

    fn pos(&self) -> Pos {
        Pos {
            line: self.line_idx,
            char: (self.byte_pos - self.line_byte_start) as u32,
        }
    }
}

#[derive(Debug)]
struct StrState {
    /// Only used when there are escapes so we can't reference the original string,
    text: Option<String>,
    text_start: Pos,
    text_byte_start: usize,
    quote: Quote,
}

impl StrState {
    fn push_char(&mut self, c: char) {
        if let Some(text) = &mut self.text {
            text.push(c);
        }
    }
}

enum StringResult {
    Continue,
    Ended,
}

impl Ctx {
    /// All errors are stored inside the [`Ctx`]. If a fatal error occurs, a unit error
    /// is returned, otherwise the possibly partially invalid token stream is returned.
    pub fn lex<'a>(&mut self, input: &'a str) -> Tokens<'a> {
        let mut lexer = Lexer::new(input);
        while let Some(c) = lexer.next() {
            match c {
                '\n' => {
                    self.newline_token(&mut lexer);
                    lexer.newline();
                }
                '\t' | ' ' => self.end_literal(&mut lexer),
                '"' | '\'' => {
                    self.end_literal(&mut lexer);

                    lexer.lit_byte_start = lexer.byte_pos;
                    lexer.lit_start = lexer.pos();
                    let mut quote = match c {
                        '"' => Quote::Basic,
                        '\'' => Quote::Literal,
                        _ => unsafe { core::hint::unreachable_unchecked() },
                    };
                    if Some(c) == lexer.peek() {
                        lexer.next();

                        if Some(c) == lexer.peek() {
                            // It's a multiline string
                            lexer.next();
                            quote = quote.multiline();
                        } else {
                            // It's just an empty string
                            let text_span = Span::pos(lexer.pos());
                            let id = lexer.store_string(StringToken {
                                quote,
                                lit: &input[lexer.byte_pos..lexer.byte_pos + 2],
                                text: Cow::Borrowed(&input[lexer.byte_pos + 1..lexer.byte_pos + 1]),
                                text_span,
                            });
                            let token = Token {
                                span: Span::from_pos_len(lexer.lit_start, 2),
                                ty: TokenType::String(id),
                            };
                            lexer.tokens.push(token);
                            continue;
                        }
                    }

                    let text_start = lexer.pos().plus(1);
                    let text_byte_start = lexer.byte_pos + 1;
                    let mut str_state = StrState {
                        text: None,
                        text_start,
                        text_byte_start,
                        quote,
                    };
                    self.string(&mut lexer, &mut str_state);
                }
                '[' => self.char_token(&mut lexer, TokenType::SquareLeft),
                ']' => self.char_token(&mut lexer, TokenType::SquareRight),
                '{' => self.char_token(&mut lexer, TokenType::CurlyLeft),
                '}' => self.char_token(&mut lexer, TokenType::CurlyRight),
                '=' => self.char_token(&mut lexer, TokenType::Equal),
                '.' => self.char_token(&mut lexer, TokenType::Dot),
                ',' => self.char_token(&mut lexer, TokenType::Comma),
                '#' => self.comment(&mut lexer),
                _ => self.start_literal(&mut lexer),
            }
        }

        // Set the position to the end of the last char
        self.end_literal(&mut lexer);

        let eof = Token {
            ty: TokenType::EOF,
            span: Span::pos(lexer.pos()),
        };
        Tokens {
            tokens: lexer.tokens,
            strings: lexer.strings,
            literals: lexer.literals,
            eof,
        }
    }

    fn string(&mut self, lexer: &mut Lexer<'_>, str: &mut StrState) {
        loop {
            let Some(c) = lexer.next() else {
                self.error(Error::MissingQuote(str.quote, lexer.lit_start, lexer.pos()));

                let end = lexer.byte_pos;
                self.end_string(lexer, str, end, end);
                return;
            };

            if str.quote.matches(c) {
                let text_end = lexer.byte_pos;
                if str.quote.is_multiline() {
                    if Some(str.quote.char()) == lexer.peek() {
                        lexer.next();
                    } else {
                        str.push_char(c);
                        continue;
                    }

                    if Some(str.quote.char()) == lexer.peek() {
                        lexer.next();
                    } else {
                        str.push_char(c);
                        str.push_char(c);
                        continue;
                    }
                }

                let lit_end = lexer.byte_pos + 1;
                self.end_string(lexer, str, text_end, lit_end);
                return;
            } else if c == '\n' {
                if !str.quote.is_multiline() {
                    // Recover state
                    self.error(Error::MissingQuote(str.quote, lexer.lit_start, lexer.pos()));
                    self.end_string(lexer, str, lexer.byte_pos, lexer.byte_pos);
                    self.newline_token(lexer);
                    lexer.newline();
                    return;
                }

                str.push_char(c);
                lexer.newline();
            } else if str.quote.is_basic() && c == '\\' {
                if str.text.is_none() {
                    let text = &lexer.input[str.text_byte_start..lexer.byte_pos];
                    str.text = Some(String::from(text));
                }

                let res = self.string_escape(lexer, str, lexer.pos());
                match res {
                    StringResult::Continue => continue,
                    StringResult::Ended => return,
                }
            } else {
                str.push_char(c);
            }
        }
    }

    fn string_escape(
        &mut self,
        lexer: &mut Lexer<'_>,
        str: &mut StrState,
        esc_start: Pos,
    ) -> StringResult {
        let Some(c) = lexer.next() else {
            self.error(Error::UnfinishedEscapeSequence(Span::new(
                esc_start,
                lexer.pos(),
            )));
            return StringResult::Continue;
        };

        match c {
            'u' => {
                return self.string_escape_unicode(lexer, str, esc_start, 4);
            }
            'U' => {
                return self.string_escape_unicode(lexer, str, esc_start, 8);
            }
            'b' => str.push_char('\u{8}'),
            't' => str.push_char('\t'),
            'n' => str.push_char('\n'),
            'f' => str.push_char('\u{C}'),
            'r' => str.push_char('\r'),
            '"' => str.push_char('"'),
            '\\' => str.push_char('\\'),
            ' ' => {
                self.error(Error::UnfinishedEscapeSequence(Span::new(
                    esc_start,
                    lexer.pos(),
                )));
                str.push_char(c);
            }
            '\n' => {
                if !str.quote.is_multiline() {
                    // Recover state
                    self.error(Error::MissingQuote(str.quote, lexer.lit_start, lexer.pos()));
                    self.end_string(lexer, str, lexer.byte_pos, lexer.byte_pos);
                    self.newline_token(lexer);
                    lexer.newline();
                    return StringResult::Ended;
                }

                // Newline was escaped
                lexer.newline();

                // eat whitespace
                while let Some(' ' | '\t') = lexer.peek() {
                    lexer.next();
                }
            }
            _ => self.error(Error::InvalidEscapeChar(c, lexer.pos())),
        }

        StringResult::Continue
    }

    fn string_escape_unicode(
        &mut self,
        lexer: &mut Lexer<'_>,
        str: &mut StrState,
        esc_start: Pos,
        num_chars: u8,
    ) -> StringResult {
        let mut remaining = num_chars;
        let mut unicode_cp = 0;
        loop {
            let Some(c) = lexer.next() else {
                self.error(Error::UnfinishedEscapeSequence(Span {
                    start: esc_start,
                    end: lexer.pos(),
                }));
                return StringResult::Continue;
            };
            remaining -= 1;

            let offset = remaining * 4;
            match c {
                '0'..='9' => {
                    unicode_cp += (c as u32 - '0' as u32) << offset;
                }
                'a'..='f' => {
                    unicode_cp += (c as u32 - 'a' as u32 + 10) << offset;
                }
                'A'..='F' => {
                    unicode_cp += (c as u32 - 'A' as u32 + 10) << offset;
                }
                ' ' => {
                    self.error(Error::UnfinishedEscapeSequence(Span::new(
                        esc_start,
                        lexer.pos(),
                    )));
                    str.push_char(c);
                }
                '\n' => {
                    self.error(Error::UnfinishedEscapeSequence(Span::new(
                        esc_start,
                        lexer.pos(),
                    )));

                    if str.quote.is_multiline() {
                        str.push_char(c);
                        lexer.newline();
                        return StringResult::Continue;
                    } else {
                        // Recover state
                        self.error(Error::MissingQuote(str.quote, lexer.lit_start, lexer.pos()));
                        self.end_string(lexer, str, lexer.byte_pos, lexer.byte_pos);
                        self.newline_token(lexer);
                        lexer.newline();
                        return StringResult::Ended;
                    }
                }
                _ => {
                    self.error(Error::InvalidUnicodeEscapeChar(c, lexer.pos()));

                    if str.quote.matches(c) {
                        let text_end = lexer.byte_pos;
                        if str.quote.is_multiline() {
                            if Some(str.quote.char()) == lexer.peek() {
                                lexer.next();
                            } else {
                                str.push_char(c);
                                return StringResult::Continue;
                            }

                            if Some(str.quote.char()) == lexer.peek() {
                                lexer.next();
                            } else {
                                str.push_char(c);
                                str.push_char(c);
                                return StringResult::Continue;
                            }
                        }

                        // Recover state
                        let lit_end = lexer.byte_pos + 1;
                        self.end_string(lexer, str, text_end, lit_end);
                        return StringResult::Ended;
                    }
                }
            }

            if remaining == 0 {
                match char::from_u32(unicode_cp) {
                    Some(char) => str.push_char(char),
                    None => self.error(Error::InvalidUnicodeCodepoint(
                        num_chars,
                        unicode_cp,
                        Span::new(esc_start, lexer.pos().after(c)),
                    )),
                }

                return StringResult::Continue;
            }
        }
    }

    fn start_literal(&mut self, lexer: &mut Lexer) {
        if !lexer.in_lit {
            lexer.lit_byte_start = lexer.byte_pos;
            lexer.lit_start = lexer.pos();
            lexer.in_lit = true;
        }
    }

    fn end_literal(&mut self, lexer: &mut Lexer) {
        if !lexer.in_lit {
            return;
        }
        let lit = &lexer.input[lexer.lit_byte_start..lexer.byte_pos];
        let span = Span::new(lexer.lit_start, lexer.pos());
        let id = lexer.store_literal(lit);
        let ty = TokenType::LiteralOrIdent(id);
        let token = Token { span, ty };
        lexer.tokens.push(token);

        lexer.in_lit = false;
    }

    fn end_string(
        &mut self,
        lexer: &mut Lexer,
        str: &mut StrState,
        text_byte_end: usize,
        lit_byte_end: usize,
    ) {
        let lit = &lexer.input[lexer.lit_byte_start..lit_byte_end];

        let text = match str.text.take() {
            Some(text) => Cow::Owned(text),
            None => Cow::Borrowed(&lexer.input[str.text_byte_start..text_byte_end]),
        };

        let lit_span = Span {
            start: lexer.lit_start,
            end: Pos {
                line: lexer.line_idx,
                char: (lit_byte_end - lexer.line_byte_start) as u32,
            },
        };
        let text_span = Span {
            start: str.text_start,
            end: Pos {
                line: lexer.line_idx,
                char: (text_byte_end - lexer.line_byte_start) as u32,
            },
        };

        let id = lexer.store_string(StringToken {
            quote: str.quote,
            lit,
            text,
            text_span,
        });
        let token = Token {
            span: lit_span,
            ty: TokenType::String(id),
        };
        lexer.tokens.push(token);

        lexer.in_lit = false;
    }

    fn char_token(&mut self, lexer: &mut Lexer, ty: TokenType) {
        self.end_literal(lexer);

        lexer.tokens.push(Token {
            span: Span::ascii_char(lexer.pos()),
            ty,
        });
    }

    fn newline_token(&mut self, lexer: &mut Lexer) {
        self.end_literal(lexer);

        let start = lexer.pos();
        let end = Pos {
            line: start.line + 1,
            char: 0,
        };
        lexer.tokens.push(Token {
            span: Span { start, end },
            ty: TokenType::Newline,
        });
    }

    fn comment(&mut self, lexer: &mut Lexer) {
        self.end_literal(lexer);

        let start_pos = lexer.pos();
        let text_start = lexer.byte_pos + 1;
        let newline = loop {
            match lexer.next() {
                None => break false,
                Some('\n') => break true,
                Some(_) => (),
            }
        };
        let end_pos = lexer.pos();
        let text_end = lexer.byte_pos;

        let lit = &lexer.input[text_start..text_end];
        let id = lexer.store_literal(lit);
        lexer.tokens.push(Token {
            span: Span::new(start_pos, end_pos),
            ty: TokenType::Comment(id),
        });

        if newline {
            self.newline_token(lexer);
            lexer.newline();
        }
    }
}
