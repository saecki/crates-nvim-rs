use std::ops::ControlFlow;
use std::str::Chars;

use bumpalo::collections::String as BString;
use bumpalo::Bump;
use common::{FmtChar, Pos, Span};

use crate::{Error, TomlCtx};

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
    pub text_offset: TextOffset,
}

impl<'a> StringToken<'a> {
    pub fn new(quote: Quote, lit: &'a str, lit_span: Span, text: &'a str, text_span: Span) -> Self {
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
            lit,
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

    #[inline(always)]
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

    #[inline(always)]
    fn next(&mut self) -> Option<char> {
        self.byte_pos = self.input.len() - self.chars.as_str().len();
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
        self.input[..self.byte_pos].chars().next_back()
    }

    #[inline(always)]
    fn pos(&self) -> Pos {
        self.pos_in_line(self.byte_pos)
    }

    #[inline(always)]
    fn next_byte_pos(&self) -> usize {
        self.input.len() - self.chars.as_str().len()
    }

    #[inline(always)]
    fn next_pos(&self) -> Pos {
        self.pos_in_line(self.next_byte_pos())
    }

    #[inline(always)]
    fn pos_in_line(&self, byte_pos: usize) -> Pos {
        Pos {
            line: self.line_idx,
            char: (byte_pos - self.line_byte_start) as u32,
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

pub fn lex<'a>(ctx: &mut impl TomlCtx, bump: &'a Bump, input: &'a str) -> Tokens<'a> {
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
            '[' => char_token(&mut lexer, TokenType::SquareLeft),
            ']' => char_token(&mut lexer, TokenType::SquareRight),
            '{' => char_token(&mut lexer, TokenType::CurlyLeft),
            '}' => char_token(&mut lexer, TokenType::CurlyRight),
            '=' => char_token(&mut lexer, TokenType::Equal),
            '.' => char_token(&mut lexer, TokenType::Dot),
            ',' => char_token(&mut lexer, TokenType::Comma),
            '#' => comment(ctx, &mut lexer),
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

fn string<'a>(ctx: &mut impl TomlCtx, lexer: &mut Lexer<'a>, str: &mut StrState<'a>) {
    loop {
        let start = lexer.next_byte_pos();
        let c = loop {
            let Some(c) = lexer.next() else {
                let mut pos = lexer.pos();
                let mut chars = lexer.input.chars();
                if chars.next_back() == Some('\n') {
                    let cr = chars.next_back() == Some('\r');
                    let line_end = lexer.input.len() - (1 + cr as usize);
                    let text = &lexer.input.as_bytes()[..line_end];
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
            let substr = &lexer.input[start..lexer.byte_pos];
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
        end: lexer.pos_in_line(lit_byte_end),
    };
    let text_span = Span {
        start: str.text_start,
        end: lexer.pos_in_line(text_byte_end),
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

fn comment(ctx: &mut impl TomlCtx, lexer: &mut Lexer) {
    end_literal(lexer);

    let start_pos = lexer.pos();
    let text_start = lexer.byte_pos + 1;

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
    let text_end = lexer.byte_pos - cr as usize;

    let lit = &lexer.input[text_start..text_end];
    let id = lexer.store_literal(lit);
    lexer.tokens.push(Token {
        start: start_pos,
        ty: TokenType::Comment(id),
    });

    if newline {
        let line_end_pos = lexer.pos_in_line(text_end);
        lexer.tokens.push(Token {
            start: line_end_pos,
            ty: TokenType::Newline,
        });
        lexer.newline();
    }
}
