use super::*;

use pretty_assertions::assert_eq;

fn check(input: &str, expected: Tokens<'_>) {
    let mut ctx = Ctx::default();
    let tokens = ctx.lex(input);
    assert_eq!(tokens, expected);
    assert_eq!(ctx.errors, []);
    assert_eq!(ctx.warnings, []);
}

fn check_err<const ERR_SIZE: usize>(
    input: &str,
    expected: Tokens<'_>,
    expected_errors: [Error; ERR_SIZE],
) {
    let mut ctx = Ctx::default();
    let tokens = ctx.lex(input);
    assert_eq!(tokens, expected);
    assert_eq!(ctx.errors, expected_errors);
    assert_eq!(ctx.warnings, []);
}

fn check_str(input: &str, expected_lit: &str, expected_text: &str) {
    let mut ctx = Ctx::default();
    let tokens = ctx.lex(input);
    assert_eq!(tokens.tokens.len(), 1, "{:#?}", tokens);
    assert_eq!(ctx.errors, []);
    assert_eq!(ctx.warnings, []);

    let token = tokens.tokens.into_iter().next().unwrap();
    match token.ty {
        TokenType::String(id) => {
            let str = &tokens.strings[id.0 as usize];
            assert_eq!(str.lit, expected_lit, "literals don't match");
            assert_eq!(str.text, expected_text, "text doesn't match");
        }
        t => panic!("Found tokentyp: {t:?}, expected string"),
    }
}

fn check_str_error(input: &str, expected_lit: &str, expected_text: &str, error: Error) {
    let mut ctx = Ctx::default();
    let tokens = ctx.lex(input);
    assert_eq!(tokens.tokens.len(), 1, "{:#?}", tokens);
    assert_eq!(ctx.errors, [error]);
    assert_eq!(ctx.warnings, []);

    let token = tokens.tokens.into_iter().next().unwrap();
    match token.ty {
        TokenType::String(id) => {
            let str = &tokens.strings[id.0 as usize];
            assert_eq!(str.lit, expected_lit, "literals don't match");
            assert_eq!(str.text, expected_text, "text doesn't match");
        }
        t => panic!("Found tokentyp: {t:?}, expected string"),
    }
}

#[test]
fn assign_int() {
    check(
        "my_int = 98742",
        Tokens {
            tokens: vec![
                Token {
                    ty: TokenType::LiteralOrIdent(LiteralId(0)),
                    span: Span {
                        start: Pos { line: 0, char: 0 },
                        end: Pos { line: 0, char: 6 },
                    },
                },
                Token {
                    ty: TokenType::Equal,
                    span: Span {
                        start: Pos { line: 0, char: 7 },
                        end: Pos { line: 0, char: 8 },
                    },
                },
                Token {
                    ty: TokenType::LiteralOrIdent(LiteralId(1)),
                    span: Span {
                        start: Pos { line: 0, char: 9 },
                        end: Pos { line: 0, char: 14 },
                    },
                },
            ],
            strings: vec![],
            literals: vec!["my_int", "98742"],
            eof: Token {
                ty: TokenType::EOF,
                span: Span::pos(Pos { line: 0, char: 14 }),
            },
        },
    );
}

#[test]
fn assign_float() {
    check(
        "my_float=0.23",
        Tokens {
            tokens: vec![
                Token {
                    ty: TokenType::LiteralOrIdent(LiteralId(0)),
                    span: Span {
                        start: Pos { line: 0, char: 0 },
                        end: Pos { line: 0, char: 8 },
                    },
                },
                Token {
                    ty: TokenType::Equal,
                    span: Span {
                        start: Pos { line: 0, char: 8 },
                        end: Pos { line: 0, char: 9 },
                    },
                },
                Token {
                    ty: TokenType::LiteralOrIdent(LiteralId(1)),
                    span: Span {
                        start: Pos { line: 0, char: 9 },
                        end: Pos { line: 0, char: 10 },
                    },
                },
                Token {
                    ty: TokenType::Dot,
                    span: Span {
                        start: Pos { line: 0, char: 10 },
                        end: Pos { line: 0, char: 11 },
                    },
                },
                Token {
                    ty: TokenType::LiteralOrIdent(LiteralId(2)),
                    span: Span {
                        start: Pos { line: 0, char: 11 },
                        end: Pos { line: 0, char: 13 },
                    },
                },
            ],
            strings: vec![],
            literals: vec!["my_float", "0", "23"],
            eof: Token {
                ty: TokenType::EOF,
                span: Span::pos(Pos { line: 0, char: 13 }),
            },
        },
    );
}

#[test]
fn assign_literal_string() {
    check(
        "my.string = 'yeet\\'",
        Tokens {
            tokens: vec![
                Token {
                    ty: TokenType::LiteralOrIdent(LiteralId(0)),
                    span: Span {
                        start: Pos { line: 0, char: 0 },
                        end: Pos { line: 0, char: 2 },
                    },
                },
                Token {
                    ty: TokenType::Dot,
                    span: Span {
                        start: Pos { line: 0, char: 2 },
                        end: Pos { line: 0, char: 3 },
                    },
                },
                Token {
                    ty: TokenType::LiteralOrIdent(LiteralId(1)),
                    span: Span {
                        start: Pos { line: 0, char: 3 },
                        end: Pos { line: 0, char: 9 },
                    },
                },
                Token {
                    ty: TokenType::Equal,
                    span: Span {
                        start: Pos { line: 0, char: 10 },
                        end: Pos { line: 0, char: 11 },
                    },
                },
                Token {
                    ty: TokenType::String(StringId(0)),
                    span: Span {
                        start: Pos { line: 0, char: 12 },
                        end: Pos { line: 0, char: 19 },
                    },
                },
            ],
            strings: vec![StringToken {
                quote: Quote::Literal,
                lit: "'yeet\\'",
                text: Cow::Borrowed("yeet\\"),
                text_span: Span {
                    start: Pos { line: 0, char: 13 },
                    end: Pos { line: 0, char: 18 },
                },
            }],
            literals: vec!["my", "string"],
            eof: Token {
                ty: TokenType::EOF,
                span: Span::pos(Pos { line: 0, char: 19 }),
            },
        },
    );
}

#[test]
fn assign_escaped_string() {
    check(
        "my.escaped.string = \"a\\u93f2nope\"",
        Tokens {
            tokens: vec![
                Token {
                    ty: TokenType::LiteralOrIdent(LiteralId(0)),
                    span: Span {
                        start: Pos { line: 0, char: 0 },
                        end: Pos { line: 0, char: 2 },
                    },
                },
                Token {
                    ty: TokenType::Dot,
                    span: Span {
                        start: Pos { line: 0, char: 2 },
                        end: Pos { line: 0, char: 3 },
                    },
                },
                Token {
                    ty: TokenType::LiteralOrIdent(LiteralId(1)),
                    span: Span {
                        start: Pos { line: 0, char: 3 },
                        end: Pos { line: 0, char: 10 },
                    },
                },
                Token {
                    ty: TokenType::Dot,
                    span: Span {
                        start: Pos { line: 0, char: 10 },
                        end: Pos { line: 0, char: 11 },
                    },
                },
                Token {
                    ty: TokenType::LiteralOrIdent(LiteralId(2)),
                    span: Span {
                        start: Pos { line: 0, char: 11 },
                        end: Pos { line: 0, char: 17 },
                    },
                },
                Token {
                    ty: TokenType::Equal,
                    span: Span {
                        start: Pos { line: 0, char: 18 },
                        end: Pos { line: 0, char: 19 },
                    },
                },
                Token {
                    ty: TokenType::String(StringId(0)),
                    span: Span {
                        start: Pos { line: 0, char: 20 },
                        end: Pos { line: 0, char: 33 },
                    },
                },
            ],
            strings: vec![StringToken {
                quote: Quote::Basic,
                lit: "\"a\\u93f2nope\"",
                text: Cow::Borrowed("a\u{93f2}nope"),
                text_span: Span {
                    start: Pos { line: 0, char: 21 },
                    end: Pos { line: 0, char: 32 },
                },
            }],
            literals: vec!["my", "escaped", "string"],
            eof: Token {
                ty: TokenType::EOF,
                span: Span::pos(Pos { line: 0, char: 33 }),
            },
        },
    );
}

// TODO: escape error tests

#[test]
fn string_escapes() {
    check_str(r#""\b""#, r#""\b""#, "\x08");
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
    check(
        "\"\"\"look \\\n    the final string \\\n    is just one \\\n    line\\\n\"\"\"",
        Tokens {
            tokens: vec![Token {
                ty: TokenType::String(StringId(0)),
                span: Span {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 4, char: 3 },
                },
            }],
            strings: vec![StringToken {
                quote: Quote::BasicMultiline,
                lit:
                    "\"\"\"look \\\n    the final string \\\n    is just one \\\n    line\\\n\"\"\"",
                text: Cow::Borrowed("look the final string is just one line"),
                text_span: Span {
                    start: Pos { line: 0, char: 3 },
                    end: Pos { line: 4, char: 0 },
                },
            }],
            literals: vec![],
            eof: Token {
                ty: TokenType::EOF,
                span: Span::pos(Pos { line: 4, char: 3 }),
            },
        },
    );
}

#[test]
fn multiline_string_contains_up_to_two_quotes() {
    check(
        "'''this doesn't end the string: '' but this does: '''",
        Tokens {
            tokens: vec![Token {
                ty: TokenType::String(StringId(0)),
                span: Span {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 53 },
                },
            }],
            strings: vec![StringToken {
                quote: Quote::LiteralMultiline,
                lit: "'''this doesn't end the string: '' but this does: '''",
                text: Cow::Borrowed("this doesn't end the string: '' but this does: "),
                text_span: Span {
                    start: Pos { line: 0, char: 3 },
                    end: Pos { line: 0, char: 50 },
                },
            }],
            literals: vec![],
            eof: Token {
                ty: TokenType::EOF,
                span: Span::pos(Pos { line: 0, char: 53 }),
            },
        },
    );
}

#[test]
fn assign_basic_multiline_string() {
    check(
        "m_string = \"\"\"\\\neach\nword\nis\non\na\nnew\nline\n\"\"\"",
        Tokens {
            tokens: vec![
                Token {
                    ty: TokenType::LiteralOrIdent(LiteralId(0)),
                    span: Span {
                        start: Pos { line: 0, char: 0 },
                        end: Pos { line: 0, char: 8 },
                    },
                },
                Token {
                    ty: TokenType::Equal,
                    span: Span {
                        start: Pos { line: 0, char: 9 },
                        end: Pos { line: 0, char: 10 },
                    },
                },
                Token {
                    ty: TokenType::String(StringId(0)),
                    span: Span {
                        start: Pos { line: 0, char: 11 },
                        end: Pos { line: 8, char: 3 },
                    },
                },
            ],
            strings: vec![StringToken {
                quote: Quote::BasicMultiline,
                lit: "\"\"\"\\\neach\nword\nis\non\na\nnew\nline\n\"\"\"",
                text: Cow::Borrowed("each\nword\nis\non\na\nnew\nline\n"),
                text_span: Span {
                    start: Pos { line: 0, char: 14 },
                    end: Pos { line: 8, char: 0 },
                },
            }],
            literals: vec!["m_string"],
            eof: Token {
                ty: TokenType::EOF,
                span: Span::pos(Pos { line: 8, char: 3 }),
            },
        },
    );
}

#[test]
fn assign_literal_multiline_string() {
    check(
        "m_string = '''\\\neach\nword\nis\non\na\nnew\nline\n'''",
        Tokens {
            tokens: vec![
                Token {
                    ty: TokenType::LiteralOrIdent(LiteralId(0)),
                    span: Span {
                        start: Pos { line: 0, char: 0 },
                        end: Pos { line: 0, char: 8 },
                    },
                },
                Token {
                    ty: TokenType::Equal,
                    span: Span {
                        start: Pos { line: 0, char: 9 },
                        end: Pos { line: 0, char: 10 },
                    },
                },
                Token {
                    ty: TokenType::String(StringId(0)),
                    span: Span {
                        start: Pos { line: 0, char: 11 },
                        end: Pos { line: 8, char: 3 },
                    },
                },
            ],
            strings: vec![StringToken {
                quote: Quote::LiteralMultiline,
                lit: "'''\\\neach\nword\nis\non\na\nnew\nline\n'''",
                text: Cow::Borrowed("\\\neach\nword\nis\non\na\nnew\nline\n"),
                text_span: Span {
                    start: Pos { line: 0, char: 14 },
                    end: Pos { line: 8, char: 0 },
                },
            }],
            literals: vec!["m_string"],
            eof: Token {
                ty: TokenType::EOF,
                span: Span::pos(Pos { line: 8, char: 3 }),
            },
        },
    );
}

#[test]
fn unclosed_basic_single_line_string() {
    check_err(
        "\"some unclosed string\n",
        Tokens {
            tokens: vec![
                Token {
                    ty: TokenType::String(StringId(0)),
                    span: Span {
                        start: Pos { line: 0, char: 0 },
                        end: Pos { line: 0, char: 21 },
                    },
                },
                Token {
                    ty: TokenType::Newline,
                    span: Span {
                        start: Pos { line: 0, char: 21 },
                        end: Pos { line: 1, char: 0 },
                    },
                },
            ],
            strings: vec![StringToken {
                quote: Quote::Basic,
                lit: "\"some unclosed string",
                text: Cow::Borrowed("some unclosed string"),
                text_span: Span {
                    start: Pos { line: 0, char: 1 },
                    end: Pos { line: 0, char: 21 },
                },
            }],
            literals: vec![],
            eof: Token {
                ty: TokenType::EOF,
                span: Span::pos(Pos { line: 1, char: 0 }),
            },
        },
        [Error::MissingQuote(
            Quote::Basic,
            Pos { line: 0, char: 0 },
            Pos { line: 0, char: 21 },
        )],
    );
}

#[test]
fn unclosed_basic_multi_line_string() {
    check_err(
        "\"\"\"some unclosed string\nthis is a new line",
        Tokens {
            tokens: vec![Token {
                ty: TokenType::String(StringId(0)),
                span: Span {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 1, char: 18 },
                },
            }],
            strings: vec![StringToken {
                quote: Quote::BasicMultiline,
                lit: "\"\"\"some unclosed string\nthis is a new line",
                text: Cow::Borrowed("some unclosed string\nthis is a new line"),
                text_span: Span {
                    start: Pos { line: 0, char: 3 },
                    end: Pos { line: 1, char: 18 },
                },
            }],
            literals: vec![],
            eof: Token {
                ty: TokenType::EOF,
                span: Span::pos(Pos { line: 1, char: 18 }),
            },
        },
        [Error::MissingQuote(
            Quote::BasicMultiline,
            Pos { line: 0, char: 0 },
            Pos { line: 1, char: 18 },
        )],
    );
}

#[test]
fn not_fully_closed_basic_multi_line_string_1() {
    check_err(
        "\"\"\"some unclosed string\"",
        Tokens {
            tokens: vec![Token {
                ty: TokenType::String(StringId(0)),
                span: Span {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 24 },
                },
            }],
            strings: vec![StringToken {
                quote: Quote::BasicMultiline,
                lit: "\"\"\"some unclosed string\"",
                text: Cow::Borrowed("some unclosed string\""),
                text_span: Span {
                    start: Pos { line: 0, char: 3 },
                    end: Pos { line: 0, char: 24 },
                },
            }],
            literals: vec![],
            eof: Token {
                ty: TokenType::EOF,
                span: Span::pos(Pos { line: 0, char: 24 }),
            },
        },
        [Error::MissingQuote(
            Quote::BasicMultiline,
            Pos { line: 0, char: 0 },
            Pos { line: 0, char: 24 },
        )],
    );
}

#[test]
fn not_fully_closed_basic_multi_line_string_2() {
    check_err(
        "\"\"\"some unclosed string\"\"",
        Tokens {
            tokens: vec![Token {
                ty: TokenType::String(StringId(0)),
                span: Span {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 25 },
                },
            }],
            strings: vec![StringToken {
                quote: Quote::BasicMultiline,
                lit: "\"\"\"some unclosed string\"\"",
                text: Cow::Borrowed("some unclosed string\"\""),
                text_span: Span {
                    start: Pos { line: 0, char: 3 },
                    end: Pos { line: 0, char: 25 },
                },
            }],
            literals: vec![],
            eof: Token {
                ty: TokenType::EOF,
                span: Span::pos(Pos { line: 0, char: 25 }),
            },
        },
        [Error::MissingQuote(
            Quote::BasicMultiline,
            Pos { line: 0, char: 0 },
            Pos { line: 0, char: 25 },
        )],
    );
}

#[test]
fn unclosed_literal_single_line_string() {
    check_err(
        "'some unclosed string\n",
        Tokens {
            tokens: vec![
                Token {
                    ty: TokenType::String(StringId(0)),
                    span: Span {
                        start: Pos { line: 0, char: 0 },
                        end: Pos { line: 0, char: 21 },
                    },
                },
                Token {
                    ty: TokenType::Newline,
                    span: Span {
                        start: Pos { line: 0, char: 21 },
                        end: Pos { line: 1, char: 0 },
                    },
                },
            ],
            strings: vec![StringToken {
                quote: Quote::Literal,
                lit: "'some unclosed string",
                text: Cow::Borrowed("some unclosed string"),
                text_span: Span {
                    start: Pos { line: 0, char: 1 },
                    end: Pos { line: 0, char: 21 },
                },
            }],
            literals: vec![],
            eof: Token {
                ty: TokenType::EOF,
                span: Span::pos(Pos { line: 1, char: 0 }),
            },
        },
        [Error::MissingQuote(
            Quote::Literal,
            Pos { line: 0, char: 0 },
            Pos { line: 0, char: 21 },
        )],
    );
}

#[test]
fn unclosed_literal_multi_line_string() {
    check_err(
        "'''some unclosed string\nthis is a new line",
        Tokens {
            tokens: vec![Token {
                ty: TokenType::String(StringId(0)),
                span: Span {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 1, char: 18 },
                },
            }],
            strings: vec![StringToken {
                quote: Quote::LiteralMultiline,
                lit: "'''some unclosed string\nthis is a new line",
                text: Cow::Borrowed("some unclosed string\nthis is a new line"),
                text_span: Span {
                    start: Pos { line: 0, char: 3 },
                    end: Pos { line: 1, char: 18 },
                },
            }],
            literals: vec![],
            eof: Token {
                ty: TokenType::EOF,
                span: Span::pos(Pos { line: 1, char: 18 }),
            },
        },
        [Error::MissingQuote(
            Quote::LiteralMultiline,
            Pos { line: 0, char: 0 },
            Pos { line: 1, char: 18 },
        )],
    );
}

#[test]
fn not_fully_closed_literal_multi_line_string_1() {
    check_err(
        "'''some unclosed string'",
        Tokens {
            tokens: vec![Token {
                ty: TokenType::String(StringId(0)),
                span: Span {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 24 },
                },
            }],
            strings: vec![StringToken {
                quote: Quote::LiteralMultiline,
                lit: "'''some unclosed string'",
                text: Cow::Borrowed("some unclosed string'"),
                text_span: Span {
                    start: Pos { line: 0, char: 3 },
                    end: Pos { line: 0, char: 24 },
                },
            }],
            literals: vec![],
            eof: Token {
                ty: TokenType::EOF,
                span: Span::pos(Pos { line: 0, char: 24 }),
            },
        },
        [Error::MissingQuote(
            Quote::LiteralMultiline,
            Pos { line: 0, char: 0 },
            Pos { line: 0, char: 24 },
        )],
    );
}

#[test]
fn not_fully_closed_literal_multi_line_string_2() {
    check_err(
        "'''some unclosed string''",
        Tokens {
            tokens: vec![Token {
                ty: TokenType::String(StringId(0)),
                span: Span {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 25 },
                },
            }],
            strings: vec![StringToken {
                quote: Quote::LiteralMultiline,
                lit: "'''some unclosed string''",
                text: Cow::Borrowed("some unclosed string''"),
                text_span: Span {
                    start: Pos { line: 0, char: 3 },
                    end: Pos { line: 0, char: 25 },
                },
            }],
            literals: vec![],
            eof: Token {
                ty: TokenType::EOF,
                span: Span::pos(Pos { line: 0, char: 25 }),
            },
        },
        [Error::MissingQuote(
            Quote::LiteralMultiline,
            Pos { line: 0, char: 0 },
            Pos { line: 0, char: 25 },
        )],
    );
}

#[test]
fn comment_without_newline() {
    check(
        "# hello there",
        Tokens {
            tokens: vec![Token {
                ty: TokenType::Comment(LiteralId(0)),
                span: Span {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 13 },
                },
            }],
            strings: vec![],
            literals: vec![" hello there"],
            eof: Token {
                ty: TokenType::EOF,
                span: Span::pos(Pos { line: 0, char: 13 }),
            },
        },
    )
}

#[test]
fn comment_with_newline() {
    check(
        "# hello there\n",
        Tokens {
            tokens: vec![
                Token {
                    ty: TokenType::Comment(LiteralId(0)),
                    span: Span {
                        start: Pos { line: 0, char: 0 },
                        end: Pos { line: 0, char: 13 },
                    },
                },
                Token {
                    ty: TokenType::Newline,
                    span: Span {
                        start: Pos { line: 0, char: 13 },
                        end: Pos { line: 1, char: 0 },
                    },
                },
            ],
            literals: vec![" hello there"],
            strings: vec![],
            eof: Token {
                ty: TokenType::EOF,
                span: Span::pos(Pos { line: 1, char: 0 }),
            },
        },
    )
}
