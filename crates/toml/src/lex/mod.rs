use std::num::NonZeroU32;
use std::ops::ControlFlow;
use std::str::Chars;

use bumpalo::Bump;
use bumpalo::collections::String as BString;
use common::OneVec;
use common::Source;
use common::{FmtChar, Pos, Span};

use crate::{Error, TomlCtx};

#[cfg(test)]
mod test;

pub(crate) type CharIter<'a> = std::iter::Peekable<std::str::CharIndices<'a>>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Tokens<'a> {
    pub source: Source<'a>,
    pub tokens: Vec<Token>,
    pub strings: Vec<StringToken<'a>>,
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
    LiteralOrIdent {
        len: u32,
    },
    Comment {
        len: u32,
    },
    /// Contains the token index of the close delimiter.
    SquareLeft(Option<NonZeroU32>),
    SquareRight,
    /// Contains the token index of the close delimiter.
    CurlyLeft(Option<NonZeroU32>),
    CurlyRight,
    Equal,
    Comma,
    Dot,
    Newline,
    EOF,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct StringId(pub u32);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringToken<'a> {
    pub quote: Quote,
    pub lit_end: Pos,
    /// The text with escape sequences evaluated. If there are no escape sequences this references
    /// the input text directly, otherwise it is bump allocated.
    pub text: &'a str,
    pub text_offset: TextOffset,
}

impl<'a> StringToken<'a> {
    pub fn new(quote: Quote, lit_span: Span, text: &'a str, text_span: Span) -> Self {
        let start_line = (text_span.start.line - lit_span.start.line) as u8;
        let end_line = (lit_span.end.line - text_span.end.line) as u8;
        let text_offset = TextOffset {
            start_line,
            start_char: if start_line == 0 {
                (text_span.start.char - lit_span.start.char) as u8
            } else {
                0
            },
            end_line,
            end_char: if end_line == 0 {
                (lit_span.end.char - text_span.end.char) as u8
            } else {
                0
            },
        };
        Self {
            quote,
            lit_end: lit_span.end,
            text,
            text_offset,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TextOffset {
    pub start_line: u8,
    pub start_char: u8,
    pub end_line: u8,
    pub end_char: u8,
}

impl TextOffset {
    pub const ZERO: Self = Self {
        start_line: 0,
        start_char: 0,
        end_line: 0,
        end_char: 0,
    };

    pub fn chars(start_char: u8, end_char: u8) -> Self {
        Self {
            start_line: 0,
            start_char,
            end_line: 0,
            end_char,
        }
    }

    pub fn apply_to(&self, lit_span: Span) -> Span {
        let start = Pos {
            line: lit_span.start.line + self.start_line as u32,
            char: lit_span.start.char + self.start_char as u32,
        };
        let end = Pos {
            line: lit_span.end.line - self.end_line as u32,
            char: lit_span.end.char - self.end_char as u32,
        };
        Span { start, end }
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
    #[allow(clippy::len_without_is_empty)]
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

    pub fn singleline(&self) -> Self {
        match self {
            Quote::Basic | Quote::BasicMultiline => Self::Basic,
            Quote::Literal | Quote::LiteralMultiline => Self::Literal,
        }
    }

    pub fn multiline(&self) -> Self {
        match self {
            Quote::Basic | Quote::BasicMultiline => Self::BasicMultiline,
            Quote::Literal | Quote::LiteralMultiline => Self::LiteralMultiline,
        }
    }

    pub fn kind_str(&self) -> &'static str {
        match self {
            Quote::Basic => "basic",
            Quote::BasicMultiline => "multi-line basic",
            Quote::Literal => "literal",
            Quote::LiteralMultiline => "multi-line literal",
        }
    }
}

#[derive(Debug)]
struct Lexer<'a> {
    bump: &'a Bump,
    source: Source<'a>,
    chars: Chars<'a>,
    byte_pos: usize,

    in_lit: bool,
    lit_start: Pos,

    tokens: Vec<Token>,
    strings: Vec<StringToken<'a>>,

    // Delimiter stack to determine unclosed/unopened delimiters inside the lexer.
    delimiters: Vec<Delim>,
}

impl<'a> Lexer<'a> {
    fn new(bump: &'a Bump, path: &'a str, text: &'a str) -> Self {
        Self {
            bump,
            source: Source {
                path,
                text,
                lines: OneVec::new(0),
            },
            chars: text.chars(),
            byte_pos: 0,

            in_lit: false,
            lit_start: Pos::default(),

            tokens: Vec::new(),
            strings: Vec::new(),

            delimiters: Vec::new(),
        }
    }

    #[inline(always)]
    fn newline(&mut self) {
        self.byte_pos += 1;
        self.source.lines.push(self.byte_pos as u32);
    }

    fn store_string(&mut self, string: StringToken<'a>) -> StringId {
        let id = self.strings.len();
        self.strings.push(string);
        StringId(id as u32)
    }

    #[inline(always)]
    fn next(&mut self) -> Option<char> {
        self.byte_pos = self.source.text.len() - self.chars.as_str().len();
        self.chars.next()
    }

    #[inline(always)]
    fn peek(&self) -> Option<char> {
        self.chars.as_str().chars().next()
    }

    #[inline(always)]
    fn peek2(&self) -> Option<char> {
        let mut iter = self.chars.as_str().chars();
        iter.next();
        iter.next()
    }

    #[inline(always)]
    fn peek_prev(&self) -> Option<char> {
        self.source.text[..self.byte_pos].chars().next_back()
    }

    #[inline(always)]
    fn pos(&self) -> Pos {
        self.pos_in_line(self.byte_pos)
    }

    #[inline(always)]
    fn next_byte_pos(&self) -> usize {
        self.source.text.len() - self.chars.as_str().len()
    }

    #[inline(always)]
    fn next_pos(&self) -> Pos {
        self.pos_in_line(self.next_byte_pos())
    }

    #[inline(always)]
    fn pos_in_line(&self, byte_pos: usize) -> Pos {
        Pos {
            line: self.source.lines.len() as u32 - 1,
            char: byte_pos as u32 - self.source.lines.last(),
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

impl StrState<'_> {
    fn push_char(&mut self, c: char) {
        if let Some(text) = &mut self.text {
            text.push(c);
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Delim {
    token_idx: u32,
    kind: DelimKind,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum DelimKind {
    Square,
    Curly,
}

pub fn lex<'a>(ctx: &mut impl TomlCtx, bump: &'a Bump, path: &'a str, text: &'a str) -> Tokens<'a> {
    let mut lexer = Lexer::new(bump, path, text);
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

                        // > A newline immediately following the opening delimiter will be trimmed
                        match lexer.peek() {
                            Some('\n') => {
                                lexer.next();
                                lexer.newline();
                            }
                            Some('\r') if lexer.peek2() == Some('\n') => {
                                lexer.next();
                                lexer.next();
                                lexer.newline();
                            }
                            _ => (),
                        }
                    } else {
                        // It's just an empty string
                        let lit_span = Span::from_pos_len(lexer.lit_start, 2);
                        let text_span = Span::pos(lexer.pos());
                        let text = lexer.source.spanned_str(text_span);
                        let id =
                            lexer.store_string(StringToken::new(quote, lit_span, text, text_span));
                        let token = Token {
                            start: lit_span.start,
                            ty: TokenType::String(id),
                        };
                        lexer.tokens.push(token);
                        continue;
                    }
                }

                let text_byte_start = lexer.next_byte_pos();
                let text_start = lexer.pos_in_line(text_byte_start);
                let mut str_state = StrState {
                    text: None,
                    text_start,
                    text_byte_start,
                    quote,
                };
                string(ctx, &mut lexer, &mut str_state);
            }
            '[' => push_delimiter(&mut lexer, DelimKind::Square),
            ']' => pop_delimiter(&mut lexer, DelimKind::Square),
            '{' => push_delimiter(&mut lexer, DelimKind::Curly),
            '}' => pop_delimiter(&mut lexer, DelimKind::Curly),
            '=' => char_token(&mut lexer, TokenType::Equal),
            '.' => char_token(&mut lexer, TokenType::Dot),
            ',' => char_token(&mut lexer, TokenType::Comma),
            '#' => comment(ctx, &mut lexer),
            _ => start_literal(&mut lexer),
        }
    }

    // Set the position to the end of the last char
    end_literal(&mut lexer);

    // If last line is empty (trailing newline), set the EOF position to the previous line end.
    let mut eof_pos = lexer.pos();
    if let Some(token) = lexer.tokens.last()
        && token.ty == TokenType::Newline
    {
        eof_pos = token.start;
    }
    let eof = Token {
        ty: TokenType::EOF,
        start: eof_pos,
    };
    Tokens {
        source: lexer.source,
        tokens: lexer.tokens,
        strings: lexer.strings,
        eof,
    }
}

fn push_delimiter(lexer: &mut Lexer, kind: DelimKind) {
    let idx = lexer.tokens.len() as u32 + lexer.in_lit as u32;
    let ty = match kind {
        DelimKind::Square => TokenType::SquareLeft(None),
        DelimKind::Curly => TokenType::CurlyLeft(None),
    };
    char_token(lexer, ty);
    lexer.delimiters.push(Delim {
        token_idx: idx,
        kind,
    });
}

fn pop_delimiter(lexer: &mut Lexer, kind: DelimKind) {
    const MAX_UNCLOSED_DEPTH: usize = 3;
    let matching = (lexer.delimiters.iter())
        .enumerate()
        .rev()
        .take(MAX_UNCLOSED_DEPTH + 1)
        .find_map(|(i, d)| (d.kind == kind).then_some(i));

    if let Some(delim_idx) = matching {
        // mark delimiter as closed
        let close_token_idx = lexer.tokens.len() as u32 + lexer.in_lit as u32;
        // SAFETY: The index can't be 0, since there must be an open delimiter on the stack.
        let close_token_idx = unsafe { NonZeroU32::new_unchecked(close_token_idx) };
        let delim = &mut lexer.delimiters[delim_idx];
        match &mut lexer.tokens[delim.token_idx as usize].ty {
            TokenType::SquareLeft(close) => *close = Some(close_token_idx),
            TokenType::CurlyLeft(close) => *close = Some(close_token_idx),
            _ => unreachable!(),
            // _ => unsafe { std::hint::unreachable_unchecked() },
        }
        lexer.delimiters.drain(delim_idx..);
    }

    let ty = match kind {
        DelimKind::Square => TokenType::SquareRight,
        DelimKind::Curly => TokenType::CurlyRight,
    };
    char_token(lexer, ty);
}

fn string<'a>(ctx: &mut impl TomlCtx, lexer: &mut Lexer<'a>, str: &mut StrState<'a>) {
    loop {
        let start = lexer.next_byte_pos();
        let c = loop {
            let Some(c) = lexer.next() else {
                let mut pos = lexer.pos();
                let mut chars = lexer.source.text.chars();
                if chars.next_back() == Some('\n') {
                    let cr = chars.next_back() == Some('\r');
                    let line_end = lexer.source.text.len() - (1 + cr as usize);
                    let text = &lexer.source.text.as_bytes()[..line_end];
                    let line_len = text
                        .iter()
                        .rev()
                        .position(|b| *b == b'\n')
                        .unwrap_or(text.len());

                    pos.line -= 1;
                    pos.char = line_len as u32;
                }
                let span = Span::new(lexer.lit_start, pos);
                ctx.error(Error::MissingQuote(str.quote, span));

                let end = lexer.byte_pos;
                end_string(lexer, str, end, end);
                return;
            };

            match c {
                _ if c == str.quote.char() => break c,
                '\n' => break c,
                '\\' if str.quote.is_basic() => break c,
                '\t' => (),
                '\r' if str.quote.is_multiline() && lexer.peek() == Some('\n') => (),
                '\x00'..='\x1f' | '\x7f' => {
                    let span = Span::ascii_char(lexer.pos());
                    ctx.error(Error::InvalidStringChar(FmtChar(c), span));
                }
                _ => (),
            }
        };
        if let Some(text) = &mut str.text {
            let substr = &lexer.source.text[start..lexer.byte_pos];
            text.push_str(substr);
        }

        if c == str.quote.char() {
            match string_closing_quote(ctx, lexer, str) {
                ControlFlow::Continue(_) => continue,
                ControlFlow::Break(_) => break,
            }
        }

        if c == '\n' {
            let cr = lexer.peek_prev() == Some('\r');
            let line_end = lexer.byte_pos - cr as usize;

            if !str.quote.is_multiline() {
                let line_end_pos = lexer.pos_in_line(line_end);

                // Recover state
                let span = Span::new(lexer.lit_start, line_end_pos);
                ctx.error(Error::MissingQuote(str.quote, span));

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
                        let text = &lexer.source.text[str.text_byte_start..line_end];
                        str.text = Some(BString::from_str_in(text, lexer.bump));
                    }
                }
            }

            str.push_char(c);
            lexer.newline();
        } else if str.quote.is_basic() && c == '\\' {
            if str.text.is_none() {
                let text = &lexer.source.text[str.text_byte_start..lexer.byte_pos];
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
    ctx: &mut impl TomlCtx,
    lexer: &mut Lexer<'a>,
    str: &mut StrState<'a>,
    esc_start: Pos,
) -> ControlFlow<()> {
    let Some(c) = lexer.next() else {
        let span = Span::new(esc_start, lexer.pos());
        ctx.error(Error::UnfinishedEscapeSequence(span));
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
        ' ' | '\r' | '\n' | '\t' => {
            let mut has_newline = c == '\n';
            if !str.quote.is_multiline() {
                let span = Span::new(esc_start, lexer.pos());
                ctx.error(Error::UnfinishedEscapeSequence(span));

                return if has_newline {
                    // Recover state
                    let span = Span::new(lexer.lit_start, lexer.pos());
                    ctx.error(Error::MissingQuote(str.quote, span));
                    end_string(lexer, str, lexer.byte_pos, lexer.byte_pos);
                    newline_token(lexer);
                    lexer.newline();
                    ControlFlow::Break(())
                } else {
                    ControlFlow::Continue(())
                };
            }

            if has_newline {
                lexer.newline();
            }

            // eat whitespace
            while let Some(c) = lexer.peek() {
                match c {
                    ' ' | '\t' | '\r' => {
                        lexer.next();
                    }
                    '\n' => {
                        lexer.next();
                        lexer.newline();
                        has_newline = true;
                    }
                    _ => break,
                }
            }

            if !has_newline {
                let end = lexer.next_pos();
                let span = Span::new(esc_start, end);
                ctx.error(Error::InvalidLineEndingEscape(span));
            }
        }
        _ => ctx.error(Error::InvalidEscapeChar(FmtChar(c), lexer.pos())),
    }

    ControlFlow::Continue(())
}

fn string_escape_unicode<'a>(
    ctx: &mut impl TomlCtx,
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
                    let span = Span::new(lexer.lit_start, lexer.pos());
                    ctx.error(Error::MissingQuote(str.quote, span));
                    end_string(lexer, str, lexer.byte_pos, lexer.byte_pos);
                    newline_token(lexer);
                    lexer.newline();
                    return ControlFlow::Break(());
                }

                str.push_char(c);
                lexer.newline();
                return ControlFlow::Continue(());
            }
            '"' => {
                // escapes are only permitted in basic strings
                ctx.error(Error::UnfinishedEscapeSequence(Span::new(
                    esc_start,
                    lexer.pos(),
                )));

                return string_closing_quote(ctx, lexer, str);
            }
            _ => {
                ctx.error(Error::InvalidUnicodeEscapeChar(FmtChar(c), lexer.pos()));
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

#[inline(always)]
fn string_closing_quote<'a>(
    ctx: &mut impl TomlCtx,
    lexer: &mut Lexer<'a>,
    str: &mut StrState<'a>,
) -> ControlFlow<()> {
    let mut text_end = lexer.byte_pos;
    if str.quote.is_multiline() {
        if lexer.peek() == Some(str.quote.char()) {
            lexer.next();
        } else {
            str.push_char(str.quote.char());
            return ControlFlow::Continue(());
        }

        if lexer.peek() == Some(str.quote.char()) {
            lexer.next();
        } else {
            str.push_char(str.quote.char());
            str.push_char(str.quote.char());
            return ControlFlow::Continue(());
        }

        // up to 2 quotes are allowed at the end of multi-line strings
        if lexer.peek() == Some(str.quote.char()) {
            lexer.next();
            str.push_char(str.quote.char());
            text_end += 1;
        }
        if lexer.peek() == Some(str.quote.char()) {
            lexer.next();
            str.push_char(str.quote.char());
            text_end += 1;
        }

        if lexer.peek() == Some(str.quote.char()) {
            let start = lexer.pos();
            lexer.next();

            while lexer.peek() == Some(str.quote.char()) {
                lexer.next();
            }

            let end = lexer.pos().plus(1);
            ctx.error(Error::ExcessiveQuotes(str.quote, Span::new(start, end)));
        }
    }

    // Recover state
    let lit_end = lexer.byte_pos + 1;
    end_string(lexer, str, text_end, lit_end);
    ControlFlow::Break(())
}

fn start_literal(lexer: &mut Lexer) {
    if !lexer.in_lit {
        lexer.lit_start = lexer.pos();
        lexer.in_lit = true;
    }
}

fn end_literal(lexer: &mut Lexer) {
    if !lexer.in_lit {
        return;
    }
    let start = lexer.lit_start;
    // literals cannot span multiple lines
    let len = lexer.pos().char - start.char;
    let ty = TokenType::LiteralOrIdent { len };
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
    let text = match str.text.take() {
        Some(text) => text.into_bump_str(),
        None => &lexer.source.text[str.text_byte_start..text_byte_end],
    };

    let lit_span = Span {
        start: lexer.lit_start,
        end: lexer.pos_in_line(lit_byte_end),
    };
    let text_span = Span {
        start: str.text_start,
        end: lexer.pos_in_line(text_byte_end),
    };

    let id = lexer.store_string(StringToken::new(str.quote, lit_span, text, text_span));
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

fn comment(ctx: &mut impl TomlCtx, lexer: &mut Lexer) {
    end_literal(lexer);

    let start_byte_pos = lexer.byte_pos;
    let start = lexer.pos();

    while let Some(c) = lexer.peek() {
        match c {
            '\n' => break,
            '\t' => (),
            '\r' if lexer.peek2() == Some('\n') => (),
            '\x00'..='\x1f' | '\x7f' => {
                let span = Span::ascii_char(lexer.pos());
                ctx.error(Error::InvalidCommentChar(FmtChar(c), span));
            }
            _ => (),
        }
        lexer.next();
    }
    let newline = lexer.next().is_some();
    let cr = newline && lexer.peek_prev() == Some('\r');
    let end = lexer.byte_pos - cr as usize;

    let len = (end - start_byte_pos) as u32;
    lexer.tokens.push(Token {
        start,
        ty: TokenType::Comment { len },
    });

    if newline {
        let line_end_pos = lexer.pos_in_line(end);
        lexer.tokens.push(Token {
            start: line_end_pos,
            ty: TokenType::Newline,
        });
        lexer.newline();
    }
}
