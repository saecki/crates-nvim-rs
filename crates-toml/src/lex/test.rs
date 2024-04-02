use crate::Warning;

use super::*;

use pretty_assertions::assert_eq;

fn check(input: &str, expected: Tokens<'_>) {
    let mut ctx = Ctx::default();
    let bump = Bump::new();
    let tokens = ctx.lex(&bump, input);
    assert_eq!(expected, tokens);
    assert_eq!(std::vec::Vec::<Error>::new(), ctx.errors);
    assert_eq!(std::vec::Vec::<Warning>::new(), ctx.warnings);
}

fn check_error(input: &str, expected: Tokens<'_>, error: Error) {
    let mut ctx = Ctx::default();
    let bump = Bump::new();
    let tokens = ctx.lex(&bump, input);
    assert_eq!(
        tokens, expected,
        "\nerrors: {:#?}\nwarnings: {:#?}",
        ctx.errors, ctx.warnings,
    );
    assert_eq!(vec![error], ctx.errors);
    assert_eq!(std::vec::Vec::<Warning>::new(), ctx.warnings);
}

fn check_str(input: &str, expected_lit: &str, expected_text: &str) {
    let mut ctx = Ctx::default();
    let bump = Bump::new();
    let tokens = ctx.lex(&bump, input);
    assert_eq!(
        tokens.tokens.len(),
        1,
        "\ntokens: {:#?}\nerrors: {:#?}\nwarnings: {:#?}",
        tokens,
        ctx.errors,
        ctx.warnings,
    );
    assert_eq!(std::vec::Vec::<Error>::new(), ctx.errors);
    assert_eq!(std::vec::Vec::<Warning>::new(), ctx.warnings);

    let token = tokens.tokens.iter().next().unwrap();
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
    let bump = Bump::new();
    let tokens = ctx.lex(&bump, input);
    assert_eq!(
        tokens.tokens.len(),
        1,
        "\ntokens: {:#?}\nerrors: {:#?}\nwarnings: {:#?}",
        tokens,
        ctx.errors,
        ctx.warnings,
    );
    assert_eq!(vec![error], ctx.errors);
    assert_eq!(std::vec::Vec::<Warning>::new(), ctx.warnings);

    let token = tokens.tokens.iter().next().unwrap();
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
            tokens: &[
                Token {
                    ty: TokenType::LiteralOrIdent(LiteralId(0)),
                    start: Pos { line: 0, char: 0 },
                },
                Token {
                    ty: TokenType::Equal,
                    start: Pos { line: 0, char: 7 },
                },
                Token {
                    ty: TokenType::LiteralOrIdent(LiteralId(1)),
                    start: Pos { line: 0, char: 9 },
                },
            ],
            strings: &[],
            literals: &["my_int", "98742"],
            eof: Token {
                ty: TokenType::EOF,
                start: Pos { line: 0, char: 14 },
            },
        },
    );
}

#[test]
fn assign_float() {
    check(
        "my_float=0.23",
        Tokens {
            tokens: &[
                Token {
                    ty: TokenType::LiteralOrIdent(LiteralId(0)),
                    start: Pos { line: 0, char: 0 },
                },
                Token {
                    ty: TokenType::Equal,
                    start: Pos { line: 0, char: 8 },
                },
                Token {
                    ty: TokenType::LiteralOrIdent(LiteralId(1)),
                    start: Pos { line: 0, char: 9 },
                },
                Token {
                    ty: TokenType::Dot,
                    start: Pos { line: 0, char: 10 },
                },
                Token {
                    ty: TokenType::LiteralOrIdent(LiteralId(2)),
                    start: Pos { line: 0, char: 11 },
                },
            ],
            strings: &[],
            literals: &["my_float", "0", "23"],
            eof: Token {
                ty: TokenType::EOF,
                start: Pos { line: 0, char: 13 },
            },
        },
    );
}

#[test]
fn assign_literal_string() {
    check(
        "my.string = 'yeet\\'",
        Tokens {
            tokens: &[
                Token {
                    ty: TokenType::LiteralOrIdent(LiteralId(0)),
                    start: Pos { line: 0, char: 0 },
                },
                Token {
                    ty: TokenType::Dot,
                    start: Pos { line: 0, char: 2 },
                },
                Token {
                    ty: TokenType::LiteralOrIdent(LiteralId(1)),
                    start: Pos { line: 0, char: 3 },
                },
                Token {
                    ty: TokenType::Equal,
                    start: Pos { line: 0, char: 10 },
                },
                Token {
                    ty: TokenType::String(StringId(0)),
                    start: Pos { line: 0, char: 12 },
                },
            ],
            strings: &[StringToken {
                quote: Quote::Literal,
                lit: "'yeet\\'",
                lit_end: Pos { line: 0, char: 19 },
                text: "yeet\\",
                text_start_offset: 1,
                text_end_offset: 1,
            }],
            literals: &["my", "string"],
            eof: Token {
                ty: TokenType::EOF,
                start: Pos { line: 0, char: 19 },
            },
        },
    );
}

#[test]
fn assign_escaped_string() {
    check(
        "my.escaped.string = \"a\\u93f2nope\"",
        Tokens {
            tokens: &[
                Token {
                    ty: TokenType::LiteralOrIdent(LiteralId(0)),
                    start: Pos { line: 0, char: 0 },
                },
                Token {
                    ty: TokenType::Dot,
                    start: Pos { line: 0, char: 2 },
                },
                Token {
                    ty: TokenType::LiteralOrIdent(LiteralId(1)),
                    start: Pos { line: 0, char: 3 },
                },
                Token {
                    ty: TokenType::Dot,
                    start: Pos { line: 0, char: 10 },
                },
                Token {
                    ty: TokenType::LiteralOrIdent(LiteralId(2)),
                    start: Pos { line: 0, char: 11 },
                },
                Token {
                    ty: TokenType::Equal,
                    start: Pos { line: 0, char: 18 },
                },
                Token {
                    ty: TokenType::String(StringId(0)),
                    start: Pos { line: 0, char: 20 },
                },
            ],
            strings: &[StringToken {
                quote: Quote::Basic,
                lit: "\"a\\u93f2nope\"",
                lit_end: Pos { line: 0, char: 33 },
                text: "a\u{93f2}nope",
                text_start_offset: 1,
                text_end_offset: 1,
            }],
            literals: &["my", "escaped", "string"],
            eof: Token {
                ty: TokenType::EOF,
                start: Pos { line: 0, char: 33 },
            },
        },
    );
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
    check(
        "\"\"\"look \\\n    the final string \\\n    is just one \\\n    line\\\n\"\"\"",
        Tokens {
            tokens: &[Token {
                ty: TokenType::String(StringId(0)),
                start: Pos { line: 0, char: 0 },
            }],
            strings: &[StringToken {
                quote: Quote::BasicMultiline,
                lit:
                    "\"\"\"look \\\n    the final string \\\n    is just one \\\n    line\\\n\"\"\"",
                lit_end: Pos { line: 4, char: 3 },
                text: "look the final string is just one line",
                text_start_offset: 3,
                text_end_offset: 3,
            }],
            literals: &[],
            eof: Token {
                ty: TokenType::EOF,
                start: Pos { line: 4, char: 3 },
            },
        },
    );
}

#[test]
fn multiline_string_contains_up_to_two_quotes() {
    check(
        "'''this doesn't end the string: '' but this does: '''",
        Tokens {
            tokens: &[Token {
                ty: TokenType::String(StringId(0)),
                start: Pos { line: 0, char: 0 },
            }],
            strings: &[StringToken {
                quote: Quote::LiteralMultiline,
                lit: "'''this doesn't end the string: '' but this does: '''",
                lit_end: Pos { line: 0, char: 53 },
                text: "this doesn't end the string: '' but this does: ",
                text_start_offset: 3,
                text_end_offset: 3,
            }],
            literals: &[],
            eof: Token {
                ty: TokenType::EOF,
                start: Pos { line: 0, char: 53 },
            },
        },
    );
}

#[test]
fn assign_basic_multiline_string() {
    check(
        "m_string = \"\"\"\\\neach\nword\nis\non\na\nnew\nline\n\"\"\"",
        Tokens {
            tokens: &[
                Token {
                    ty: TokenType::LiteralOrIdent(LiteralId(0)),
                    start: Pos { line: 0, char: 0 },
                },
                Token {
                    ty: TokenType::Equal,
                    start: Pos { line: 0, char: 9 },
                },
                Token {
                    ty: TokenType::String(StringId(0)),
                    start: Pos { line: 0, char: 11 },
                },
            ],
            strings: &[StringToken {
                quote: Quote::BasicMultiline,
                lit: "\"\"\"\\\neach\nword\nis\non\na\nnew\nline\n\"\"\"",
                lit_end: Pos { line: 8, char: 3 },
                text: "each\nword\nis\non\na\nnew\nline\n",
                text_start_offset: 3,
                text_end_offset: 3,
            }],
            literals: &["m_string"],
            eof: Token {
                ty: TokenType::EOF,
                start: Pos { line: 8, char: 3 },
            },
        },
    );
}

#[test]
fn assign_literal_multiline_string() {
    check(
        "m_string = '''\\\neach\nword\nis\non\na\nnew\nline\n'''",
        Tokens {
            tokens: &[
                Token {
                    ty: TokenType::LiteralOrIdent(LiteralId(0)),
                    start: Pos { line: 0, char: 0 },
                },
                Token {
                    ty: TokenType::Equal,
                    start: Pos { line: 0, char: 9 },
                },
                Token {
                    ty: TokenType::String(StringId(0)),
                    start: Pos { line: 0, char: 11 },
                },
            ],
            strings: &[StringToken {
                quote: Quote::LiteralMultiline,
                lit: "'''\\\neach\nword\nis\non\na\nnew\nline\n'''",
                lit_end: Pos { line: 8, char: 3 },
                text: "\\\neach\nword\nis\non\na\nnew\nline\n",
                text_start_offset: 3,
                text_end_offset: 3,
            }],
            literals: &["m_string"],
            eof: Token {
                ty: TokenType::EOF,
                start: Pos { line: 8, char: 3 },
            },
        },
    );
}

#[test]
fn unclosed_basic_single_line_string() {
    check_error(
        "\"some unclosed string\n",
        Tokens {
            tokens: &[
                Token {
                    ty: TokenType::String(StringId(0)),
                    start: Pos { line: 0, char: 0 },
                },
                Token {
                    ty: TokenType::Newline,
                    start: Pos { line: 0, char: 21 },
                },
            ],
            strings: &[StringToken {
                quote: Quote::Basic,
                lit: "\"some unclosed string",
                lit_end: Pos { line: 0, char: 21 },
                text: "some unclosed string",
                text_start_offset: 1,
                text_end_offset: 0,
            }],
            literals: &[],
            eof: Token {
                ty: TokenType::EOF,
                start: Pos { line: 1, char: 0 },
            },
        },
        Error::MissingQuote(
            Quote::Basic,
            Pos { line: 0, char: 0 },
            Pos { line: 0, char: 21 },
        ),
    );
}

#[test]
fn unclosed_basic_multi_line_string() {
    check_error(
        "\"\"\"some unclosed string\nthis is a new line",
        Tokens {
            tokens: &[Token {
                ty: TokenType::String(StringId(0)),
                start: Pos { line: 0, char: 0 },
            }],
            strings: &[StringToken {
                quote: Quote::BasicMultiline,
                lit: "\"\"\"some unclosed string\nthis is a new line",
                lit_end: Pos { line: 1, char: 18 },
                text: "some unclosed string\nthis is a new line",
                text_start_offset: 3,
                text_end_offset: 0,
            }],
            literals: &[],
            eof: Token {
                ty: TokenType::EOF,
                start: Pos { line: 1, char: 18 },
            },
        },
        Error::MissingQuote(
            Quote::BasicMultiline,
            Pos { line: 0, char: 0 },
            Pos { line: 1, char: 18 },
        ),
    );
}

#[test]
fn not_fully_closed_basic_multi_line_string_1() {
    check_error(
        "\"\"\"some unclosed string\"",
        Tokens {
            tokens: &[Token {
                ty: TokenType::String(StringId(0)),
                start: Pos { line: 0, char: 0 },
            }],
            strings: &[StringToken {
                quote: Quote::BasicMultiline,
                lit: "\"\"\"some unclosed string\"",
                lit_end: Pos { line: 0, char: 24 },
                text: "some unclosed string\"",
                text_start_offset: 3,
                text_end_offset: 0,
            }],
            literals: &[],
            eof: Token {
                ty: TokenType::EOF,
                start: Pos { line: 0, char: 24 },
            },
        },
        Error::MissingQuote(
            Quote::BasicMultiline,
            Pos { line: 0, char: 0 },
            Pos { line: 0, char: 24 },
        ),
    );
}

#[test]
fn not_fully_closed_basic_multi_line_string_2() {
    check_error(
        "\"\"\"some unclosed string\"\"",
        Tokens {
            tokens: &[Token {
                ty: TokenType::String(StringId(0)),
                start: Pos { line: 0, char: 0 },
            }],
            strings: &[StringToken {
                quote: Quote::BasicMultiline,
                lit: "\"\"\"some unclosed string\"\"",
                lit_end: Pos { line: 0, char: 25 },
                text: "some unclosed string\"\"",
                text_start_offset: 3,
                text_end_offset: 0,
            }],
            literals: &[],
            eof: Token {
                ty: TokenType::EOF,
                start: Pos { line: 0, char: 25 },
            },
        },
        Error::MissingQuote(
            Quote::BasicMultiline,
            Pos { line: 0, char: 0 },
            Pos { line: 0, char: 25 },
        ),
    );
}

#[test]
fn unclosed_literal_single_line_string() {
    check_error(
        "'some unclosed string\n",
        Tokens {
            tokens: &[
                Token {
                    ty: TokenType::String(StringId(0)),
                    start: Pos { line: 0, char: 0 },
                },
                Token {
                    ty: TokenType::Newline,
                    start: Pos { line: 0, char: 21 },
                },
            ],
            strings: &[StringToken {
                quote: Quote::Literal,
                lit: "'some unclosed string",
                lit_end: Pos { line: 0, char: 21 },
                text: "some unclosed string",
                text_start_offset: 1,
                text_end_offset: 0,
            }],
            literals: &[],
            eof: Token {
                ty: TokenType::EOF,
                start: Pos { line: 1, char: 0 },
            },
        },
        Error::MissingQuote(
            Quote::Literal,
            Pos { line: 0, char: 0 },
            Pos { line: 0, char: 21 },
        ),
    );
}

#[test]
fn unclosed_literal_multi_line_string() {
    check_error(
        "'''some unclosed string\nthis is a new line",
        Tokens {
            tokens: &[Token {
                ty: TokenType::String(StringId(0)),
                start: Pos { line: 0, char: 0 },
            }],
            strings: &[StringToken {
                quote: Quote::LiteralMultiline,
                lit: "'''some unclosed string\nthis is a new line",
                lit_end: Pos { line: 1, char: 18 },
                text: "some unclosed string\nthis is a new line",
                text_start_offset: 3,
                text_end_offset: 0,
            }],
            literals: &[],
            eof: Token {
                ty: TokenType::EOF,
                start: Pos { line: 1, char: 18 },
            },
        },
        Error::MissingQuote(
            Quote::LiteralMultiline,
            Pos { line: 0, char: 0 },
            Pos { line: 1, char: 18 },
        ),
    );
}

#[test]
fn not_fully_closed_literal_multi_line_string_1() {
    check_error(
        "'''some unclosed string'",
        Tokens {
            tokens: &[Token {
                ty: TokenType::String(StringId(0)),
                start: Pos { line: 0, char: 0 },
            }],
            strings: &[StringToken {
                quote: Quote::LiteralMultiline,
                lit: "'''some unclosed string'",
                lit_end: Pos { line: 0, char: 24 },
                text: "some unclosed string'",
                text_start_offset: 3,
                text_end_offset: 0,
            }],
            literals: &[],
            eof: Token {
                ty: TokenType::EOF,
                start: Pos { line: 0, char: 24 },
            },
        },
        Error::MissingQuote(
            Quote::LiteralMultiline,
            Pos { line: 0, char: 0 },
            Pos { line: 0, char: 24 },
        ),
    );
}

#[test]
fn not_fully_closed_literal_multi_line_string_2() {
    check_error(
        "'''some unclosed string''",
        Tokens {
            tokens: &[Token {
                ty: TokenType::String(StringId(0)),
                start: Pos { line: 0, char: 0 },
            }],
            strings: &[StringToken {
                quote: Quote::LiteralMultiline,
                lit: "'''some unclosed string''",
                lit_end: Pos { line: 0, char: 25 },
                text: "some unclosed string''",
                text_start_offset: 3,
                text_end_offset: 0,
            }],
            literals: &[],
            eof: Token {
                ty: TokenType::EOF,
                start: Pos { line: 0, char: 25 },
            },
        },
        Error::MissingQuote(
            Quote::LiteralMultiline,
            Pos { line: 0, char: 0 },
            Pos { line: 0, char: 25 },
        ),
    );
}

#[test]
fn comment_without_newline() {
    check(
        "# hello there",
        Tokens {
            tokens: &[Token {
                ty: TokenType::Comment(LiteralId(0)),
                start: Pos { line: 0, char: 0 },
            }],
            strings: &[],
            literals: &[" hello there"],
            eof: Token {
                ty: TokenType::EOF,
                start: Pos { line: 0, char: 13 },
            },
        },
    )
}

#[test]
fn comment_with_newline() {
    check(
        "# hello there\n",
        Tokens {
            tokens: &[
                Token {
                    ty: TokenType::Comment(LiteralId(0)),
                    start: Pos { line: 0, char: 0 },
                },
                Token {
                    ty: TokenType::Newline,
                    start: Pos { line: 0, char: 13 },
                },
            ],
            literals: &[" hello there"],
            strings: &[],
            eof: Token {
                ty: TokenType::EOF,
                start: Pos { line: 1, char: 0 },
            },
        },
    )
}
