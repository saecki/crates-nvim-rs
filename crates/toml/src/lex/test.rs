use crate::{TomlDiagnostics, Warning};

use super::*;

use pretty_assertions::assert_eq;

struct TokenBuilder<'a> {
    strings: Vec<StringToken<'a>>,
}

impl<'a> TokenBuilder<'a> {
    fn new() -> Self {
        Self {
            strings: Vec::new(),
        }
    }

    fn string(&mut self, start: Pos, str: StringToken<'a>) -> Token {
        let id = StringId(self.strings.len() as u32);
        self.strings.push(str);
        Token {
            start,
            ty: TokenType::String(id),
        }
    }
}

#[track_caller]
fn check<const SIZE: usize>(text: &str, expected: [Token; SIZE]) {
    let mut ctx = TomlDiagnostics::default();
    let bump = Bump::new();
    let tokens = lex(&mut ctx, &bump, "<test>", text);
    let (expected_eof, expected_tokens) = expected.split_last().unwrap();
    assert_eq!(expected_tokens, tokens.tokens);
    assert_eq!(*expected_eof, tokens.eof);
    assert_eq!(Vec::<Error>::new(), ctx.errors);
    assert_eq!(Vec::<Warning>::new(), ctx.warnings);
}

#[track_caller]
fn check_builder<const SIZE: usize>(
    text: &str,
    expected_builder: impl Fn(&mut TokenBuilder<'_>) -> [Token; SIZE],
) {
    let mut builder = TokenBuilder::new();
    let expected = expected_builder(&mut builder);
    check(text, expected);
}

#[track_caller]
fn check_error<const SIZE: usize>(text: &str, expected: [Token; SIZE], error: Error) {
    let mut ctx = TomlDiagnostics::default();
    let bump = Bump::new();
    let tokens = lex(&mut ctx, &bump, "<test>", text);
    let (expected_eof, expected_tokens) = expected.split_last().unwrap();

    assert_eq!(
        expected_tokens, tokens.tokens,
        "\nerrors: {:#?}\nwarnings: {:#?}",
        ctx.errors, ctx.warnings,
    );
    assert_eq!(*expected_eof, tokens.eof);
    assert_eq!(vec![error], ctx.errors);
    assert_eq!(std::vec::Vec::<Warning>::new(), ctx.warnings);
}

#[track_caller]
fn check_builder_error<const SIZE: usize>(
    text: &str,
    expected_builder: impl Fn(&mut TokenBuilder<'_>) -> [Token; SIZE],
    error: Error,
) {
    let mut builder = TokenBuilder::new();
    let expected = expected_builder(&mut builder);
    check_error(text, expected, error);
}

#[track_caller]
fn check_str(text: &str, expected_lit: &str, expected_text: &str) {
    let mut ctx = TomlDiagnostics::default();
    let bump = Bump::new();
    let tokens = lex(&mut ctx, &bump, "<test>", text);
    assert_eq!(
        1,
        tokens.tokens.len(),
        "\ntokens: {:#?}\nerrors: {:#?}\nwarnings: {:#?}",
        tokens,
        ctx.errors,
        ctx.warnings,
    );
    assert_eq!(std::vec::Vec::<Error>::new(), ctx.errors);
    assert_eq!(std::vec::Vec::<Warning>::new(), ctx.warnings);

    let token = tokens.tokens.first().unwrap();
    match token.ty {
        TokenType::String(id) => {
            let str = &tokens.strings[id.0 as usize];
            let lit_span = Span::new(token.start, str.lit_end);
            let lit = tokens.source.spanned_str(lit_span);
            assert_eq!(lit, expected_lit, "literals don't match");
            assert_eq!(str.text, expected_text, "text doesn't match");
        }
        t => panic!("Found tokentyp: {t:?}, expected string"),
    }
}

#[track_caller]
fn check_str_error(text: &str, expected_lit: &str, expected_text: &str, error: Error) {
    let mut ctx = TomlDiagnostics::default();
    let bump = Bump::new();
    let tokens = lex(&mut ctx, &bump, "<test>", text);
    assert_eq!(
        1,
        tokens.tokens.len(),
        "\ntokens: {:#?}\nerrors: {:#?}\nwarnings: {:#?}",
        tokens,
        ctx.errors,
        ctx.warnings,
    );
    assert_eq!(vec![error], ctx.errors);
    assert_eq!(std::vec::Vec::<Warning>::new(), ctx.warnings);

    let token = tokens.tokens.first().unwrap();
    match token.ty {
        TokenType::String(id) => {
            let str = &tokens.strings[id.0 as usize];
            let lit_span = Span::new(token.start, str.lit_end);
            let lit = tokens.source.spanned_str(lit_span);
            assert_eq!(lit, expected_lit, "literals don't match");
            assert_eq!(str.text, expected_text, "text doesn't match");
        }
        t => panic!("Found tokentyp: {t:?}, expected string"),
    }
}

#[test]
fn assign_int() {
    check(
        "my_int = 98742",
        [
            Token {
                ty: TokenType::LiteralOrIdent { len: 6 },
                start: Pos { line: 0, char: 0 },
            },
            Token {
                ty: TokenType::Equal,
                start: Pos { line: 0, char: 7 },
            },
            Token {
                ty: TokenType::LiteralOrIdent { len: 5 },
                start: Pos { line: 0, char: 9 },
            },
            Token {
                ty: TokenType::EOF,
                start: Pos { line: 0, char: 14 },
            },
        ],
    );
}

#[test]
fn assign_float() {
    check(
        "my_float=0.23",
        [
            Token {
                ty: TokenType::LiteralOrIdent { len: 8 },
                start: Pos { line: 0, char: 0 },
            },
            Token {
                ty: TokenType::Equal,
                start: Pos { line: 0, char: 8 },
            },
            Token {
                ty: TokenType::LiteralOrIdent { len: 1 },
                start: Pos { line: 0, char: 9 },
            },
            Token {
                ty: TokenType::Dot,
                start: Pos { line: 0, char: 10 },
            },
            Token {
                ty: TokenType::LiteralOrIdent { len: 2 },
                start: Pos { line: 0, char: 11 },
            },
            Token {
                ty: TokenType::EOF,
                start: Pos { line: 0, char: 13 },
            },
        ],
    );
}

#[test]
fn assign_literal_string() {
    check_builder("my.string = 'yeet\\'", |builder| {
        [
            Token {
                ty: TokenType::LiteralOrIdent { len: 2 },
                start: Pos { line: 0, char: 0 },
            },
            Token {
                ty: TokenType::Dot,
                start: Pos { line: 0, char: 2 },
            },
            Token {
                ty: TokenType::LiteralOrIdent { len: 6 },
                start: Pos { line: 0, char: 3 },
            },
            Token {
                ty: TokenType::Equal,
                start: Pos { line: 0, char: 10 },
            },
            builder.string(
                Pos { line: 0, char: 12 },
                StringToken {
                    quote: Quote::Literal,
                    lit_end: Pos { line: 0, char: 19 },
                    text: "yeet\\",
                    text_offset: TextOffset::chars(1, 1),
                },
            ),
            Token {
                ty: TokenType::EOF,
                start: Pos { line: 0, char: 19 },
            },
        ]
    });
}

#[test]
fn assign_escaped_string() {
    check_builder("my.escaped.string = \"a\\u93f2nope\"", |builder| {
        [
            Token {
                ty: TokenType::LiteralOrIdent { len: 2 },
                start: Pos { line: 0, char: 0 },
            },
            Token {
                ty: TokenType::Dot,
                start: Pos { line: 0, char: 2 },
            },
            Token {
                ty: TokenType::LiteralOrIdent { len: 7 },
                start: Pos { line: 0, char: 3 },
            },
            Token {
                ty: TokenType::Dot,
                start: Pos { line: 0, char: 10 },
            },
            Token {
                ty: TokenType::LiteralOrIdent { len: 6 },
                start: Pos { line: 0, char: 11 },
            },
            Token {
                ty: TokenType::Equal,
                start: Pos { line: 0, char: 18 },
            },
            builder.string(
                Pos { line: 0, char: 20 },
                StringToken {
                    quote: Quote::Basic,
                    lit_end: Pos { line: 0, char: 33 },
                    text: "a\u{93f2}nope",
                    text_offset: TextOffset::chars(1, 1),
                },
            ),
            Token {
                ty: TokenType::EOF,
                start: Pos { line: 0, char: 33 },
            },
        ]
    });
}

// TODO: escape error tests

#[test]
fn string_escapes() {
    check_str(r#"   "\b""#, r#""\b""#, "\x08");
    check_str(r#""\t""#, r#""\t""#, "\t");
    check_str(r#""\n""#, r#""\n""#, "\n");
    check_str(r#""\f""#, r#""\f""#, "\u{C}");
    check_str(r#""\r""#, r#""\r""#, "\r");
}

#[test]
fn unicode_escapes() {
    check_str(r#""\u001b""#, r#""\u001b""#, "\x1b");
    check_str(r#""\u001a""#, r#""\u001a""#, "\u{1a}");
    check_str(r#""\u03a0""#, r#""\u03a0""#, "\u{03a0}");
    check_str(r#""\U00102230""#, r#""\U00102230""#, "\u{102230}");
    check_str(r#"  "\u03c0"  "#, r#""\u03c0""#, "\u{03c0}");
}
#[test]
fn empty_strings() {
    check_str(r#" '' "#, r#"''"#, "");
    check_str(r#" "" "#, r#""""#, "");
}

#[test]
fn multiline_string_unfinished_escape_sequence_on_newline() {
    check_str_error(
        r#""""\t abc \u324
this should be on a new line""""#,
        r#""""\t abc \u324
this should be on a new line""""#,
        "\t abc \nthis should be on a new line",
        Error::UnfinishedEscapeSequence(Span::from_pos_len(Pos::new(0, 10), 5)),
    );
}

#[test]
fn multiline_string_escaped_newline() {
    check_builder(
        "\"\"\"look \\\n    the final string \\\n    is just one \\\n    line\\\n\"\"\"",
        |builder| {
            [
                builder.string(
                    Pos { line: 0, char: 0 },
                    StringToken {
                        quote: Quote::BasicMultiline,
                        lit_end: Pos { line: 4, char: 3 },
                        text: "look the final string is just one line",
                        text_offset: TextOffset::chars(3, 3),
                    },
                ),
                Token {
                    ty: TokenType::EOF,
                    start: Pos { line: 4, char: 3 },
                },
            ]
        },
    );
}

#[test]
fn multiline_string_contains_up_to_two_quotes() {
    check_builder(
        "'''this doesn't end the string: '' but this does: '''",
        |builder| {
            [
                builder.string(
                    Pos { line: 0, char: 0 },
                    StringToken {
                        quote: Quote::LiteralMultiline,
                        lit_end: Pos { line: 0, char: 53 },
                        text: "this doesn't end the string: '' but this does: ",
                        text_offset: TextOffset::chars(3, 3),
                    },
                ),
                Token {
                    ty: TokenType::EOF,
                    start: Pos { line: 0, char: 53 },
                },
            ]
        },
    );
}

#[test]
fn assign_basic_multiline_string() {
    check_builder(
        "m_string = \"\"\"\\\neach\nword\nis\non\na\nnew\nline\n\"\"\"",
        |builder| {
            [
                Token {
                    ty: TokenType::LiteralOrIdent { len: 8 },
                    start: Pos { line: 0, char: 0 },
                },
                Token {
                    ty: TokenType::Equal,
                    start: Pos { line: 0, char: 9 },
                },
                builder.string(
                    Pos { line: 0, char: 11 },
                    StringToken {
                        quote: Quote::BasicMultiline,
                        lit_end: Pos { line: 8, char: 3 },
                        text: "each\nword\nis\non\na\nnew\nline\n",
                        text_offset: TextOffset::chars(3, 3),
                    },
                ),
                Token {
                    ty: TokenType::EOF,
                    start: Pos { line: 8, char: 3 },
                },
            ]
        },
    );
}

#[test]
fn assign_literal_multiline_string() {
    check_builder(
        "m_string = '''\\\neach\nword\nis\non\na\nnew\nline\n'''",
        |builder| {
            [
                Token {
                    ty: TokenType::LiteralOrIdent { len: 8 },
                    start: Pos { line: 0, char: 0 },
                },
                Token {
                    ty: TokenType::Equal,
                    start: Pos { line: 0, char: 9 },
                },
                builder.string(
                    Pos { line: 0, char: 11 },
                    StringToken {
                        quote: Quote::LiteralMultiline,
                        lit_end: Pos { line: 8, char: 3 },
                        text: "\\\neach\nword\nis\non\na\nnew\nline\n",
                        text_offset: TextOffset::chars(3, 3),
                    },
                ),
                Token {
                    ty: TokenType::EOF,
                    start: Pos { line: 8, char: 3 },
                },
            ]
        },
    );
}

#[test]
fn unclosed_basic_single_line_string() {
    check_builder_error(
        "\"some unclosed string\n",
        |builder| {
            [
                builder.string(
                    Pos { line: 0, char: 0 },
                    StringToken {
                        quote: Quote::Basic,
                        lit_end: Pos { line: 0, char: 21 },
                        text: "some unclosed string",
                        text_offset: TextOffset::chars(1, 0),
                    },
                ),
                Token {
                    ty: TokenType::Newline,
                    start: Pos { line: 0, char: 21 },
                },
                Token {
                    ty: TokenType::EOF,
                    start: Pos { line: 0, char: 21 },
                },
            ]
        },
        Error::MissingQuote(
            Quote::Basic,
            Span::from_pos_len(Pos { line: 0, char: 0 }, 21),
        ),
    );
}

#[test]
fn unclosed_basic_multi_line_string() {
    check_builder_error(
        "\"\"\"some unclosed string\nthis is a new line",
        |builder| {
            [
                builder.string(
                    Pos { line: 0, char: 0 },
                    StringToken {
                        quote: Quote::BasicMultiline,
                        lit_end: Pos { line: 1, char: 18 },
                        text: "some unclosed string\nthis is a new line",
                        text_offset: TextOffset::chars(3, 0),
                    },
                ),
                Token {
                    ty: TokenType::EOF,
                    start: Pos { line: 1, char: 18 },
                },
            ]
        },
        Error::MissingQuote(
            Quote::BasicMultiline,
            Span::new(Pos { line: 0, char: 0 }, Pos { line: 1, char: 18 }),
        ),
    );
}

#[test]
fn not_fully_closed_basic_multi_line_string_1() {
    check_builder_error(
        "\"\"\"some unclosed string\"",
        |builder| {
            [
                builder.string(
                    Pos { line: 0, char: 0 },
                    StringToken {
                        quote: Quote::BasicMultiline,
                        lit_end: Pos { line: 0, char: 24 },
                        text: "some unclosed string\"",
                        text_offset: TextOffset::chars(3, 0),
                    },
                ),
                Token {
                    ty: TokenType::EOF,
                    start: Pos { line: 0, char: 24 },
                },
            ]
        },
        Error::MissingQuote(
            Quote::BasicMultiline,
            Span::from_pos_len(Pos { line: 0, char: 0 }, 24),
        ),
    );
}

#[test]
fn not_fully_closed_basic_multi_line_string_2() {
    check_builder_error(
        "\"\"\"some unclosed string\"\"",
        |builder| {
            [
                builder.string(
                    Pos { line: 0, char: 0 },
                    StringToken {
                        quote: Quote::BasicMultiline,
                        lit_end: Pos { line: 0, char: 25 },
                        text: "some unclosed string\"\"",
                        text_offset: TextOffset::chars(3, 0),
                    },
                ),
                Token {
                    ty: TokenType::EOF,
                    start: Pos { line: 0, char: 25 },
                },
            ]
        },
        Error::MissingQuote(
            Quote::BasicMultiline,
            Span::from_pos_len(Pos { line: 0, char: 0 }, 25),
        ),
    );
}

#[test]
fn unclosed_literal_single_line_string() {
    check_builder_error(
        "'some unclosed string\n",
        |builder| {
            [
                builder.string(
                    Pos { line: 0, char: 0 },
                    StringToken {
                        quote: Quote::Literal,
                        lit_end: Pos { line: 0, char: 21 },
                        text: "some unclosed string",
                        text_offset: TextOffset::chars(1, 0),
                    },
                ),
                Token {
                    ty: TokenType::Newline,
                    start: Pos { line: 0, char: 21 },
                },
                Token {
                    ty: TokenType::EOF,
                    start: Pos { line: 0, char: 21 },
                },
            ]
        },
        Error::MissingQuote(
            Quote::Literal,
            Span::from_pos_len(Pos { line: 0, char: 0 }, 21),
        ),
    );
}

#[test]
fn unclosed_literal_multi_line_string() {
    check_builder_error(
        "'''some unclosed string\nthis is a new line",
        |builder| {
            [
                builder.string(
                    Pos { line: 0, char: 0 },
                    StringToken {
                        quote: Quote::LiteralMultiline,
                        lit_end: Pos { line: 1, char: 18 },
                        text: "some unclosed string\nthis is a new line",
                        text_offset: TextOffset::chars(3, 0),
                    },
                ),
                Token {
                    ty: TokenType::EOF,
                    start: Pos { line: 1, char: 18 },
                },
            ]
        },
        Error::MissingQuote(
            Quote::LiteralMultiline,
            Span::new(Pos { line: 0, char: 0 }, Pos { line: 1, char: 18 }),
        ),
    );
}

#[test]
fn not_fully_closed_literal_multi_line_string_1() {
    check_builder_error(
        "'''some unclosed string'",
        |builder| {
            [
                builder.string(
                    Pos { line: 0, char: 0 },
                    StringToken {
                        quote: Quote::LiteralMultiline,
                        lit_end: Pos { line: 0, char: 24 },
                        text: "some unclosed string'",
                        text_offset: TextOffset::chars(3, 0),
                    },
                ),
                Token {
                    ty: TokenType::EOF,
                    start: Pos { line: 0, char: 24 },
                },
            ]
        },
        Error::MissingQuote(
            Quote::LiteralMultiline,
            Span::from_pos_len(Pos { line: 0, char: 0 }, 24),
        ),
    );
}

#[test]
fn not_fully_closed_literal_multi_line_string_2() {
    check_builder_error(
        "'''some unclosed string''",
        |builder| {
            [
                builder.string(
                    Pos { line: 0, char: 0 },
                    StringToken {
                        quote: Quote::LiteralMultiline,
                        lit_end: Pos { line: 0, char: 25 },
                        text: "some unclosed string''",
                        text_offset: TextOffset::chars(3, 0),
                    },
                ),
                Token {
                    ty: TokenType::EOF,
                    start: Pos { line: 0, char: 25 },
                },
            ]
        },
        Error::MissingQuote(
            Quote::LiteralMultiline,
            Span::from_pos_len(Pos { line: 0, char: 0 }, 25),
        ),
    );
}

#[test]
fn unclosed_multi_line_string_error_on_last_line() {
    let cases = [
        (Quote::BasicMultiline, "\"\"\"some unclosed string\n"),
        (Quote::LiteralMultiline, "'''some unclosed string\n"),
    ];
    for (quote, text) in cases {
        check_builder_error(
            text,
            |builder| {
                [
                    builder.string(
                        Pos { line: 0, char: 0 },
                        StringToken {
                            quote,
                            lit_end: Pos { line: 1, char: 0 },
                            text: &text[3..],
                            text_offset: TextOffset::chars(3, 0),
                        },
                    ),
                    Token {
                        ty: TokenType::EOF,
                        start: Pos { line: 1, char: 0 },
                    },
                ]
            },
            Error::MissingQuote(quote, Span::from_pos_len(Pos { line: 0, char: 0 }, 23)),
        );
    }
}

#[test]
fn comment_without_newline() {
    check(
        "# hello there",
        [
            Token {
                ty: TokenType::Comment { len: 13 },
                start: Pos { line: 0, char: 0 },
            },
            Token {
                ty: TokenType::EOF,
                start: Pos { line: 0, char: 13 },
            },
        ],
    )
}

#[test]
fn comment_with_newline() {
    check(
        "# hello there\n",
        [
            Token {
                ty: TokenType::Comment { len: 13 },
                start: Pos { line: 0, char: 0 },
            },
            Token {
                ty: TokenType::Newline,
                start: Pos { line: 0, char: 13 },
            },
            Token {
                ty: TokenType::EOF,
                start: Pos { line: 0, char: 13 },
            },
        ],
    )
}

#[test]
fn crlf() {
    check_builder(
        "\
        [project]\r\n\
        \r\n\
        name = \"splay\"\r\n\
        version = \"0.1.0\"\r\n\
        # comment\r\n\
        [[lib]]\r\n\
        \r\n\
        description = \"\"\"\
        A Rust implementation of a TAR file reader and writer. This library does not\r\n\
        currently handle compression, but it is abstract over all I/O readers and\r\n\
        writers. Additionally, great lengths are taken to ensure that the entire\r\n\
        contents are never required to be entirely resident in memory all at once.\r\n\
        \"\"\"\
        ",
        |builder| {
            [
                Token {
                    ty: TokenType::SquareLeft(Some(NonZeroU32::new(2).unwrap())),
                    start: Pos { line: 0, char: 0 },
                },
                Token {
                    ty: TokenType::LiteralOrIdent { len: 7 },
                    start: Pos { line: 0, char: 1 },
                },
                Token {
                    ty: TokenType::SquareRight,
                    start: Pos { line: 0, char: 8 },
                },
                Token {
                    ty: TokenType::Newline,
                    start: Pos { line: 0, char: 9 },
                },
                //
                Token {
                    ty: TokenType::Newline,
                    start: Pos { line: 1, char: 0 },
                },
                //
                Token {
                    ty: TokenType::LiteralOrIdent { len: 4 },
                    start: Pos { line: 2, char: 0 },
                },
                Token {
                    ty: TokenType::Equal,
                    start: Pos { line: 2, char: 5 },
                },
                builder.string(
                    Pos { line: 2, char: 7 },
                    StringToken {
                        quote: Quote::Basic,
                        lit_end: Pos { line: 2, char: 14 },
                        text: "splay",
                        text_offset: TextOffset::chars(1, 1),
                    },
                ),
                Token {
                    ty: TokenType::Newline,
                    start: Pos { line: 2, char: 14 },
                },
                //
                Token {
                    ty: TokenType::LiteralOrIdent { len: 7 },
                    start: Pos { line: 3, char: 0 },
                },
                Token {
                    ty: TokenType::Equal,
                    start: Pos { line: 3, char: 8 },
                },
                builder.string(
                    Pos { line: 3, char: 10 },
                    StringToken {
                        quote: Quote::Basic,
                        lit_end: Pos { line: 3, char: 17 },
                        text: "0.1.0",
                        text_offset: TextOffset::chars(1, 1),
                    },
                ),
                Token {
                    ty: TokenType::Newline,
                    start: Pos { line: 3, char: 17 },
                },
                //
                Token {
                    ty: TokenType::Comment { len: 9 },
                    start: Pos { line: 4, char: 0 },
                },
                Token {
                    ty: TokenType::Newline,
                    start: Pos { line: 4, char: 9 },
                },
                //
                Token {
                    ty: TokenType::SquareLeft(Some(NonZeroU32::new(19).unwrap())),
                    start: Pos { line: 5, char: 0 },
                },
                Token {
                    ty: TokenType::SquareLeft(Some(NonZeroU32::new(18).unwrap())),
                    start: Pos { line: 5, char: 1 },
                },
                Token {
                    ty: TokenType::LiteralOrIdent { len: 3 },
                    start: Pos { line: 5, char: 2 },
                },
                Token {
                    ty: TokenType::SquareRight,
                    start: Pos { line: 5, char: 5 },
                },
                Token {
                    ty: TokenType::SquareRight,
                    start: Pos { line: 5, char: 6 },
                },
                Token {
                    ty: TokenType::Newline,
                    start: Pos { line: 5, char: 7 },
                },
                //
                Token {
                    ty: TokenType::Newline,
                    start: Pos { line: 6, char: 0 },
                },
                //
                Token {
                    ty: TokenType::LiteralOrIdent { len: 11 },
                    start: Pos { line: 7, char: 0 },
                },
                Token {
                    ty: TokenType::Equal,
                    start: Pos { line: 7, char: 12 },
                },
                builder.string(
Pos { line: 7, char: 14 },
                StringToken {
                    quote: Quote::BasicMultiline,
                    lit_end: Pos { line: 11, char: 3 },
                    text: "\
                        A Rust implementation of a TAR file reader and writer. This library does not\n\
                        currently handle compression, but it is abstract over all I/O readers and\n\
                        writers. Additionally, great lengths are taken to ensure that the entire\n\
                        contents are never required to be entirely resident in memory all at once.\n\
                    ",
                    text_offset: TextOffset::chars(3, 3),
                },
                ),
                Token {
                    ty: TokenType::EOF,
                    start: Pos { line: 11, char: 3 },
                },
            ]
        },
    );
}
