use std::ops::ControlFlow;
use std::str::Chars;

use bumpalo::collections::String as BString;
use bumpalo::Bump;
use common::FmtChar;

use crate::{Ctx, Error};

#[cfg(test)]
mod test;

pub(crate) type CharIter<'a> = std::iter::Peekable<std::str::CharIndices<'a>>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Tokens<'a> {
    pub tokens: &'a [Token],
    pub strings: &'a [StringToken<'a>],
    pub literals: &'a [&'a str],
    pub eof: Token,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Token {
    pub ty: TokenType,
    pub start: Pos,
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
    pub lit_end: Pos,
    /// The text with escape sequences evaluated
    pub text: &'a str,
    pub text_start_offset: u8,
    pub text_end_offset: u8,
}

impl<'a> StringToken<'a> {
    pub fn new(quote: Quote, lit: &'a str, lit_span: Span, text: &'a str, text_span: Span) -> Self {
        Self {
            quote,
            lit,
            lit_end: lit_span.end,
            text,
            text_start_offset: (text_span.start.char - lit_span.start.char) as u8,
            text_end_offset: (lit_span.end.char - text_span.end.char) as u8,
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
    pub fn ascii_char(pos: Pos) -> Self {
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

    pub fn char(&self) -> char {
        match self {
            Quote::Basic | Quote::BasicMultiline => '"',
            Quote::Literal | Quote::LiteralMultiline => '\'',
        }
    }

    pub fn byte(&self) -> u8 {
        match self {
            Quote::Basic | Quote::BasicMultiline => b'"',
            Quote::Literal | Quote::LiteralMultiline => b'\'',
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
    bump: &'a Bump,
    input: &'a str,
    chars: Chars<'a>,

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
    fn new(bump: &'a Bump, input: &'a str) -> Self {
        Self {
            bump,
            input,
            chars: input.chars(),

            line_idx: 0,
            line_byte_start: 0,
            byte_pos: 0,

            in_lit: false,
            lit_start: Pos::default(),
            lit_byte_start: 0,

            tokens: Vec::new(),
            strings: Vec::new(),
            literals: Vec::new(),
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
        self.byte_pos = self.input.len() - self.chars.as_str().len();
        self.chars.next()
    }

    fn skip_until(&mut self, a: u8) -> Option<char> {
        let substr = self.chars.as_str();
        let pos = self.input.len() - substr.len();
        let offset = memchr::memchr(a, substr.as_bytes());
        self.byte_pos = match offset {
            Some(o) => pos + o,
            None => self.input.len(),
        };
        self.chars = self.input[self.byte_pos..].chars();
        self.chars.next()
    }

    fn skip_until3(&mut self, a: u8, b: u8, c: u8) -> Option<char> {
        let substr = self.chars.as_str();
        let pos = self.input.len() - substr.len();
        let offset = memchr::memchr3(a, b, c, substr.as_bytes());
        self.byte_pos = match offset {
            Some(o) => pos + o,
            None => self.input.len(),
        };
        self.chars = self.input[self.byte_pos..].chars();
        self.chars.next()
    }

    fn peek(&self) -> Option<char> {
        self.chars.as_str().chars().next()
    }

    fn peek_prev(&self) -> Option<char> {
        self.input[..self.byte_pos].chars().next_back()
    }

    fn pos(&self) -> Pos {
        Pos {
            line: self.line_idx,
            char: (self.byte_pos - self.line_byte_start) as u32,
        }
    }
}

#[derive(Debug)]
struct StrState<'a> {
    /// Only used when there are escapes so we can't reference the original string,
    text: Option<BString<'a>>,
    text_start: Pos,
    text_byte_start: usize,
    quote: Quote,
}

impl<'a> StrState<'a> {
    fn push_char(&mut self, c: char) {
        if let Some(text) = &mut self.text {
            text.push(c);
        }
    }
}

pub fn lex<'a>(ctx: &mut Ctx, bump: &'a Bump, input: &'a str) -> Tokens<'a> {
    let mut lexer = Lexer::new(bump, input);
    while let Some(c) = lexer.next() {
        match c {
            '\r' if lexer.peek() == Some('\n') => {
                newline_token(&mut lexer);
                lexer.next();
                lexer.newline();
            }
            '\n' => {
                newline_token(&mut lexer);
                lexer.newline();
            }
            '\t' | ' ' => end_literal(&mut lexer),
            '"' | '\'' => {
                end_literal(&mut lexer);

                lexer.lit_byte_start = lexer.byte_pos;
                lexer.lit_start = lexer.pos();
                let mut quote = match c {
                    '"' => Quote::Basic,
                    '\'' => Quote::Literal,
                    _ => unsafe { core::hint::unreachable_unchecked() },
                };
                if lexer.peek() == Some(c) {
                    lexer.next();

                    if lexer.peek() == Some(c) {
                        // It's a multiline string
                        lexer.next();
                        quote = quote.multiline();
                    } else {
                        // It's just an empty string
                        let lit_span = Span::from_pos_len(lexer.lit_start, 2);
                        let text_span = Span::pos(lexer.pos());
                        let str_start = lexer.lit_byte_start;
                        let id = lexer.store_string(StringToken::new(
                            quote,
                            &input[str_start..str_start + 2],
                            lit_span,
                            &input[str_start + 1..str_start + 1],
                            text_span,
                        ));
                        let token = Token {
                            start: lit_span.start,
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
                string(ctx, &mut lexer, &mut str_state);
            }
            '[' => char_token(&mut lexer, TokenType::SquareLeft),
            ']' => char_token(&mut lexer, TokenType::SquareRight),
            '{' => char_token(&mut lexer, TokenType::CurlyLeft),
            '}' => char_token(&mut lexer, TokenType::CurlyRight),
            '=' => char_token(&mut lexer, TokenType::Equal),
            '.' => char_token(&mut lexer, TokenType::Dot),
            ',' => char_token(&mut lexer, TokenType::Comma),
            '#' => comment(&mut lexer),
            _ => start_literal(&mut lexer),
        }
    }

    // Set the position to the end of the last char
    end_literal(&mut lexer);

    let eof = Token {
        ty: TokenType::EOF,
        start: lexer.pos(),
    };
    Tokens {
        tokens: bump.alloc_slice_fill_iter(lexer.tokens),
        strings: bump.alloc_slice_fill_iter(lexer.strings),
        literals: bump.alloc_slice_fill_iter(lexer.literals),
        eof,
    }
}

fn string<'a>(ctx: &mut Ctx, lexer: &mut Lexer<'a>, str: &mut StrState<'a>) {
    loop {
        let start = lexer.input.len() - lexer.chars.as_str().len();
        let c = lexer.skip_until3(str.quote.byte(), b'\n', b'\\');
        if let Some(text) = &mut str.text {
            let substr = &lexer.input[start..lexer.byte_pos];
            text.push_str(substr);
        }

        let Some(c) = c else {
            ctx.error(Error::MissingQuote(str.quote, lexer.lit_start, lexer.pos()));

            let end = lexer.byte_pos;
            end_string(lexer, str, end, end);
            return;
        };

        if c == str.quote.char() {
            let text_end = lexer.byte_pos;
            if str.quote.is_multiline() {
                if lexer.peek() == Some(str.quote.char()) {
                    lexer.next();
                } else {
                    str.push_char(c);
                    continue;
                }

                if lexer.peek() == Some(str.quote.char()) {
                    lexer.next();
                } else {
                    str.push_char(c);
                    str.push_char(c);
                    continue;
                }
            }

            let lit_end = lexer.byte_pos + 1;
            end_string(lexer, str, text_end, lit_end);
            return;
        }

        if c == '\n' {
            let cr = lexer.peek_prev() == Some('\r');
            let line_end = lexer.byte_pos - cr as usize;

            if !str.quote.is_multiline() {
                let line_end_pos = Pos {
                    line: lexer.line_idx,
                    char: (line_end - lexer.line_byte_start) as u32,
                };

                // Recover state
                ctx.error(Error::MissingQuote(
                    str.quote,
                    lexer.lit_start,
                    line_end_pos,
                ));

                if let Some(text) = &mut str.text {
                    text.pop();
                }
                end_string(lexer, str, line_end, line_end);

                lexer.tokens.push(Token {
                    start: line_end_pos,
                    ty: TokenType::Newline,
                });

                lexer.newline();
                return;
            }

            if cr {
                match &mut str.text {
                    Some(text) => {
                        text.pop();
                    }
                    _ => {
                        let text = &lexer.input[str.text_byte_start..line_end];
                        str.text = Some(BString::from_str_in(text, lexer.bump));
                    }
                }
            }

            str.push_char(c);
            lexer.newline();
        } else if str.quote.is_basic() && c == '\\' {
            if str.text.is_none() {
                let text = &lexer.input[str.text_byte_start..lexer.byte_pos];
                str.text = Some(BString::from_str_in(text, lexer.bump));
            }

            let res = string_escape(ctx, lexer, str, lexer.pos());
            match res {
                ControlFlow::Continue(()) => continue,
                ControlFlow::Break(()) => return,
            }
        } else {
            str.push_char(c);
        }
    }
}

fn string_escape<'a>(
    ctx: &mut Ctx,
    lexer: &mut Lexer<'a>,
    str: &mut StrState<'a>,
    esc_start: Pos,
) -> ControlFlow<()> {
    let Some(c) = lexer.next() else {
        ctx.error(Error::UnfinishedEscapeSequence(Span::new(
            esc_start,
            lexer.pos(),
        )));
        return ControlFlow::Continue(());
    };

    match c {
        'u' => {
            return string_escape_unicode(ctx, lexer, str, esc_start, 4);
        }
        'U' => {
            return string_escape_unicode(ctx, lexer, str, esc_start, 8);
        }
        'b' => str.push_char('\u{8}'),
        't' => str.push_char('\t'),
        'n' => str.push_char('\n'),
        'f' => str.push_char('\u{C}'),
        'r' => str.push_char('\r'),
        '"' => str.push_char('"'),
        '\\' => str.push_char('\\'),
        ' ' => {
            ctx.error(Error::UnfinishedEscapeSequence(Span::new(
                esc_start,
                lexer.pos(),
            )));
            str.push_char(c);
        }
        '\n' => {
            if !str.quote.is_multiline() {
                // Recover state
                ctx.error(Error::MissingQuote(str.quote, lexer.lit_start, lexer.pos()));
                end_string(lexer, str, lexer.byte_pos, lexer.byte_pos);
                newline_token(lexer);
                lexer.newline();
                return ControlFlow::Break(());
            }

            // Newline was escaped
            lexer.newline();

            // eat whitespace
            while let Some(' ' | '\t') = lexer.peek() {
                lexer.next();
            }
        }
        _ => ctx.error(Error::InvalidEscapeChar(FmtChar(c), lexer.pos())),
    }

    ControlFlow::Continue(())
}

fn string_escape_unicode<'a>(
    ctx: &mut Ctx,
    lexer: &mut Lexer<'a>,
    str: &mut StrState<'a>,
    esc_start: Pos,
    num_chars: u8,
) -> ControlFlow<()> {
    let mut remaining = num_chars;
    let mut unicode_cp = 0;
    loop {
        let Some(c) = lexer.next() else {
            ctx.error(Error::UnfinishedEscapeSequence(Span {
                start: esc_start,
                end: lexer.pos(),
            }));
            return ControlFlow::Continue(());
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
                ctx.error(Error::UnfinishedEscapeSequence(Span::new(
                    esc_start,
                    lexer.pos(),
                )));
                str.push_char(c);
            }
            '\n' => {
                ctx.error(Error::UnfinishedEscapeSequence(Span::new(
                    esc_start,
                    lexer.pos(),
                )));

                if !str.quote.is_multiline() {
                    // Recover state
                    ctx.error(Error::MissingQuote(str.quote, lexer.lit_start, lexer.pos()));
                    end_string(lexer, str, lexer.byte_pos, lexer.byte_pos);
                    newline_token(lexer);
                    lexer.newline();
                    return ControlFlow::Break(());
                }

                str.push_char(c);
                lexer.newline();
                return ControlFlow::Continue(());
            }
            _ => {
                ctx.error(Error::InvalidUnicodeEscapeChar(FmtChar(c), lexer.pos()));

                if c == str.quote.char() {
                    let text_end = lexer.byte_pos;
                    if str.quote.is_multiline() {
                        if lexer.peek() == Some(str.quote.char()) {
                            lexer.next();
                        } else {
                            str.push_char(c);
                            return ControlFlow::Continue(());
                        }

                        if lexer.peek() == Some(str.quote.char()) {
                            lexer.next();
                        } else {
                            str.push_char(c);
                            str.push_char(c);
                            return ControlFlow::Continue(());
                        }
                    }

                    // Recover state
                    let lit_end = lexer.byte_pos + 1;
                    end_string(lexer, str, text_end, lit_end);
                    return ControlFlow::Break(());
                }
            }
        }

        if remaining == 0 {
            match char::from_u32(unicode_cp) {
                Some(char) => str.push_char(char),
                None => ctx.error(Error::InvalidUnicodeCodepoint(
                    num_chars,
                    unicode_cp,
                    Span::new(esc_start, lexer.pos().after(c)),
                )),
            }

            return ControlFlow::Continue(());
        }
    }
}

fn start_literal(lexer: &mut Lexer) {
    if !lexer.in_lit {
        lexer.lit_byte_start = lexer.byte_pos;
        lexer.lit_start = lexer.pos();
        lexer.in_lit = true;
    }
}

fn end_literal(lexer: &mut Lexer) {
    if !lexer.in_lit {
        return;
    }
    let lit = &lexer.input[lexer.lit_byte_start..lexer.byte_pos];
    let start = lexer.lit_start;
    let id = lexer.store_literal(lit);
    let ty = TokenType::LiteralOrIdent(id);
    let token = Token { start, ty };
    lexer.tokens.push(token);

    lexer.in_lit = false;
}

fn end_string<'a>(
    lexer: &mut Lexer<'a>,
    str: &mut StrState<'a>,
    text_byte_end: usize,
    lit_byte_end: usize,
) {
    let lit = &lexer.input[lexer.lit_byte_start..lit_byte_end];

    let text = match str.text.take() {
        Some(text) => text.into_bump_str(),
        None => &lexer.input[str.text_byte_start..text_byte_end],
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

    let id = lexer.store_string(StringToken::new(str.quote, lit, lit_span, text, text_span));
    let token = Token {
        start: lit_span.start,
        ty: TokenType::String(id),
    };
    lexer.tokens.push(token);

    lexer.in_lit = false;
}

fn char_token(lexer: &mut Lexer, ty: TokenType) {
    end_literal(lexer);

    lexer.tokens.push(Token {
        start: lexer.pos(),
        ty,
    });
}

fn newline_token(lexer: &mut Lexer) {
    end_literal(lexer);

    lexer.tokens.push(Token {
        start: lexer.pos(),
        ty: TokenType::Newline,
    });
}

fn comment(lexer: &mut Lexer) {
    end_literal(lexer);

    let start_pos = lexer.pos();
    let text_start = lexer.byte_pos + 1;

    let newline = lexer.skip_until(b'\n').is_some();
    let cr = newline && lexer.peek_prev() == Some('\r');
    let text_end = lexer.byte_pos - cr as usize;

    let lit = &lexer.input[text_start..text_end];
    let id = lexer.store_literal(lit);
    lexer.tokens.push(Token {
        start: start_pos,
        ty: TokenType::Comment(id),
    });

    if newline {
        let line_end_pos = Pos {
            line: lexer.line_idx,
            char: (text_end - lexer.line_byte_start) as u32,
        };
        lexer.tokens.push(Token {
            start: line_end_pos,
            ty: TokenType::Newline,
        });
        lexer.newline();
    }
}
