use super::*;

use pretty_assertions::assert_eq;

fn check<const SIZE: usize>(input: &str, expected: [Token; SIZE]) {
    let mut ctx = Ctx::default();
    let tokens = ctx.lex(input).unwrap();
    assert_eq!(tokens, expected);
}

fn check_err<const SIZE: usize, const ERR_SIZE: usize>(
    input: &str,
    expected: [Token; SIZE],
    expected_errors: [Error; ERR_SIZE],
) {
    let mut ctx = Ctx::default();
    let tokens = ctx.lex(input).unwrap();
    assert_eq!(tokens, expected);
    assert_eq!(ctx.errors, expected_errors);
}

fn check_str(input: &str, expected_lit: &str, expected_text: &str) {
    let mut ctx = Ctx::default();
    let tokens = ctx.lex(input).unwrap();
    assert_eq!(tokens.len(), 2);
    if ctx.errors != [] {
        assert_eq!(ctx.errors, []);
    }
    assert_eq!(ctx.warnings, []);

    let token = tokens.into_iter().next().unwrap();
    match token.ty {
        TokenType::String { lit, text, .. } => {
            assert_eq!(lit, expected_lit, "literals don't match");
            assert_eq!(text, expected_text, "text doesn't match");
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
                ty: TokenType::LiteralOrIdent("my_int"),
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 6 },
                },
            },
            Token {
                ty: TokenType::Equal,
                range: Range {
                    start: Pos { line: 0, char: 7 },
                    end: Pos { line: 0, char: 8 },
                },
            },
            Token {
                ty: TokenType::LiteralOrIdent("98742"),
                range: Range {
                    start: Pos { line: 0, char: 9 },
                    end: Pos { line: 0, char: 14 },
                },
            },
            Token {
                ty: TokenType::EOF,
                range: Range::pos(Pos { line: 0, char: 14 }),
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
                ty: TokenType::LiteralOrIdent("my_float"),
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 8 },
                },
            },
            Token {
                ty: TokenType::Equal,
                range: Range {
                    start: Pos { line: 0, char: 8 },
                    end: Pos { line: 0, char: 9 },
                },
            },
            Token {
                ty: TokenType::LiteralOrIdent("0"),
                range: Range {
                    start: Pos { line: 0, char: 9 },
                    end: Pos { line: 0, char: 10 },
                },
            },
            Token {
                ty: TokenType::Dot,
                range: Range {
                    start: Pos { line: 0, char: 10 },
                    end: Pos { line: 0, char: 11 },
                },
            },
            Token {
                ty: TokenType::LiteralOrIdent("23"),
                range: Range {
                    start: Pos { line: 0, char: 11 },
                    end: Pos { line: 0, char: 13 },
                },
            },
            Token {
                ty: TokenType::EOF,
                range: Range::pos(Pos { line: 0, char: 13 }),
            },
        ],
    );
}

#[test]
fn assign_literal_string() {
    check(
        "my.string = 'yeet\\'",
        [
            Token {
                ty: TokenType::LiteralOrIdent("my"),
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 2 },
                },
            },
            Token {
                ty: TokenType::Dot,
                range: Range {
                    start: Pos { line: 0, char: 2 },
                    end: Pos { line: 0, char: 3 },
                },
            },
            Token {
                ty: TokenType::LiteralOrIdent("string"),
                range: Range {
                    start: Pos { line: 0, char: 3 },
                    end: Pos { line: 0, char: 9 },
                },
            },
            Token {
                ty: TokenType::Equal,
                range: Range {
                    start: Pos { line: 0, char: 10 },
                    end: Pos { line: 0, char: 11 },
                },
            },
            Token {
                ty: TokenType::String {
                    quote: Quote::Literal,
                    lit: "'yeet\\'",
                    text: Cow::Borrowed("yeet\\"),
                    text_range: Range {
                        start: Pos { line: 0, char: 13 },
                        end: Pos { line: 0, char: 18 },
                    },
                },
                range: Range {
                    start: Pos { line: 0, char: 12 },
                    end: Pos { line: 0, char: 19 },
                },
            },
            Token {
                ty: TokenType::EOF,
                range: Range::pos(Pos { line: 0, char: 19 }),
            },
        ],
    );
}

#[test]
fn assign_escaped_string() {
    check(
        "my.escaped.string = \"a\\u93f2nope\"",
        [
            Token {
                ty: TokenType::LiteralOrIdent("my"),
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 2 },
                },
            },
            Token {
                ty: TokenType::Dot,
                range: Range {
                    start: Pos { line: 0, char: 2 },
                    end: Pos { line: 0, char: 3 },
                },
            },
            Token {
                ty: TokenType::LiteralOrIdent("escaped"),
                range: Range {
                    start: Pos { line: 0, char: 3 },
                    end: Pos { line: 0, char: 10 },
                },
            },
            Token {
                ty: TokenType::Dot,
                range: Range {
                    start: Pos { line: 0, char: 10 },
                    end: Pos { line: 0, char: 11 },
                },
            },
            Token {
                ty: TokenType::LiteralOrIdent("string"),
                range: Range {
                    start: Pos { line: 0, char: 11 },
                    end: Pos { line: 0, char: 17 },
                },
            },
            Token {
                ty: TokenType::Equal,
                range: Range {
                    start: Pos { line: 0, char: 18 },
                    end: Pos { line: 0, char: 19 },
                },
            },
            Token {
                ty: TokenType::String {
                    quote: Quote::Basic,
                    lit: "\"a\\u93f2nope\"",
                    text: Cow::Borrowed("a\u{93f2}nope"),
                    text_range: Range {
                        start: Pos { line: 0, char: 21 },
                        end: Pos { line: 0, char: 32 },
                    },
                },
                range: Range {
                    start: Pos { line: 0, char: 20 },
                    end: Pos { line: 0, char: 33 },
                },
            },
            Token {
                ty: TokenType::EOF,
                range: Range::pos(Pos { line: 0, char: 33 }),
            },
        ],
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
fn multiline_string_escaped_newline() {
    check(
        "\"\"\"look \\\n    the final string \\\n    is just one \\\n    line\\\n\"\"\"",
        [
            Token {
                ty: TokenType::String {
                    quote: Quote::BasicMultiline,
                    lit:
                        "\"\"\"look \\\n    the final string \\\n    is just one \\\n    line\\\n\"\"\"",
                    text: Cow::Borrowed("look the final string is just one line"),
                    text_range: Range {
                        start: Pos { line: 0, char: 3 },
                        end: Pos { line: 4, char: 0 },
                    },
                },
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 4, char: 3 },
                },
            },
            Token {
                ty: TokenType::EOF,
                range: Range::pos(Pos { line: 4, char: 3 }),
            },
        ],
    );
}

#[test]
fn multiline_string_contains_up_to_two_quotes() {
    check(
        "'''this doesn't end the string: '' but this does: '''",
        [
            Token {
                ty: TokenType::String {
                    quote: Quote::LiteralMultiline,
                    lit: "'''this doesn't end the string: '' but this does: '''",
                    text: Cow::Borrowed("this doesn't end the string: '' but this does: "),
                    text_range: Range {
                        start: Pos { line: 0, char: 3 },
                        end: Pos { line: 0, char: 50 },
                    },
                },
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 53 },
                },
            },
            Token {
                ty: TokenType::EOF,
                range: Range::pos(Pos { line: 0, char: 53 }),
            },
        ],
    );
}

#[test]
fn assign_basic_multiline_string() {
    check(
        "m_string = \"\"\"\\\neach\nword\nis\non\na\nnew\nline\n\"\"\"",
        [
            Token {
                ty: TokenType::LiteralOrIdent("m_string"),
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 8 },
                },
            },
            Token {
                ty: TokenType::Equal,
                range: Range {
                    start: Pos { line: 0, char: 9 },
                    end: Pos { line: 0, char: 10 },
                },
            },
            Token {
                ty: TokenType::String {
                    quote: Quote::BasicMultiline,
                    lit: "\"\"\"\\\neach\nword\nis\non\na\nnew\nline\n\"\"\"",
                    text: Cow::Borrowed("each\nword\nis\non\na\nnew\nline\n"),
                    text_range: Range {
                        start: Pos { line: 0, char: 14 },
                        end: Pos { line: 8, char: 0 },
                    },
                },
                range: Range {
                    start: Pos { line: 0, char: 11 },
                    end: Pos { line: 8, char: 3 },
                },
            },
            Token {
                ty: TokenType::EOF,
                range: Range::pos(Pos { line: 8, char: 3 }),
            },
        ],
    );
}

#[test]
fn assign_literal_multiline_string() {
    check(
        "m_string = '''\\\neach\nword\nis\non\na\nnew\nline\n'''",
        [
            Token {
                ty: TokenType::LiteralOrIdent("m_string"),
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 8 },
                },
            },
            Token {
                ty: TokenType::Equal,
                range: Range {
                    start: Pos { line: 0, char: 9 },
                    end: Pos { line: 0, char: 10 },
                },
            },
            Token {
                ty: TokenType::String {
                    quote: Quote::LiteralMultiline,
                    lit: "'''\\\neach\nword\nis\non\na\nnew\nline\n'''",
                    text: Cow::Borrowed("\\\neach\nword\nis\non\na\nnew\nline\n"),
                    text_range: Range {
                        start: Pos { line: 0, char: 14 },
                        end: Pos { line: 8, char: 0 },
                    },
                },
                range: Range {
                    start: Pos { line: 0, char: 11 },
                    end: Pos { line: 8, char: 3 },
                },
            },
            Token {
                ty: TokenType::EOF,
                range: Range::pos(Pos { line: 8, char: 3 }),
            },
        ],
    );
}

#[test]
fn unclosed_basic_single_line_string() {
    check_err(
        "\"some unclosed string\n",
        [
            Token {
                ty: TokenType::String {
                    quote: Quote::Basic,
                    lit: "\"some unclosed string",
                    text: Cow::Borrowed("some unclosed string"),
                    text_range: Range {
                        start: Pos { line: 0, char: 1 },
                        end: Pos { line: 0, char: 21 },
                    },
                },
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 21 },
                },
            },
            Token {
                ty: TokenType::Newline,
                range: Range {
                    start: Pos { line: 0, char: 21 },
                    end: Pos { line: 1, char: 0 },
                },
            },
            Token {
                ty: TokenType::EOF,
                range: Range::pos(Pos { line: 1, char: 0 }),
            },
        ],
        [Error::MissingQuote(Quote::Basic, Pos { line: 0, char: 21 })],
    );
}

#[test]
fn unclosed_basic_multi_line_string() {
    check_err(
        "\"\"\"some unclosed string\nthis is a new line",
        [
            Token {
                ty: TokenType::String {
                    quote: Quote::BasicMultiline,
                    lit: "\"\"\"some unclosed string\nthis is a new line",
                    text: Cow::Borrowed("some unclosed string\nthis is a new line"),
                    text_range: Range {
                        start: Pos { line: 0, char: 3 },
                        end: Pos { line: 1, char: 18 },
                    },
                },
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 1, char: 18 },
                },
            },
            Token {
                ty: TokenType::EOF,
                range: Range::pos(Pos { line: 1, char: 18 }),
            },
        ],
        [Error::MissingQuote(
            Quote::BasicMultiline,
            Pos { line: 1, char: 18 },
        )],
    );
}

#[test]
fn not_fully_closed_basic_multi_line_string_1() {
    check_err(
        "\"\"\"some unclosed string\"",
        [
            Token {
                ty: TokenType::String {
                    quote: Quote::BasicMultiline,
                    lit: "\"\"\"some unclosed string\"",
                    text: Cow::Borrowed("some unclosed string\""),
                    text_range: Range {
                        start: Pos { line: 0, char: 3 },
                        end: Pos { line: 0, char: 24 },
                    },
                },
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 24 },
                },
            },
            Token {
                ty: TokenType::EOF,
                range: Range::pos(Pos { line: 0, char: 24 }),
            },
        ],
        [Error::MissingQuote(
            Quote::BasicMultiline,
            Pos { line: 0, char: 24 },
        )],
    );
}

#[test]
fn not_fully_closed_basic_multi_line_string_2() {
    check_err(
        "\"\"\"some unclosed string\"\"",
        [
            Token {
                ty: TokenType::String {
                    quote: Quote::BasicMultiline,
                    lit: "\"\"\"some unclosed string\"\"",
                    text: Cow::Borrowed("some unclosed string\"\""),
                    text_range: Range {
                        start: Pos { line: 0, char: 3 },
                        end: Pos { line: 0, char: 25 },
                    },
                },
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 25 },
                },
            },
            Token {
                ty: TokenType::EOF,
                range: Range::pos(Pos { line: 0, char: 25 }),
            },
        ],
        [Error::MissingQuote(
            Quote::BasicMultiline,
            Pos { line: 0, char: 25 },
        )],
    );
}

#[test]
fn unclosed_literal_single_line_string() {
    check_err(
        "'some unclosed string\n",
        [
            Token {
                ty: TokenType::String {
                    quote: Quote::Literal,
                    lit: "'some unclosed string",
                    text: Cow::Borrowed("some unclosed string"),
                    text_range: Range {
                        start: Pos { line: 0, char: 1 },
                        end: Pos { line: 0, char: 21 },
                    },
                },
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 21 },
                },
            },
            Token {
                ty: TokenType::Newline,
                range: Range {
                    start: Pos { line: 0, char: 21 },
                    end: Pos { line: 1, char: 0 },
                },
            },
            Token {
                ty: TokenType::EOF,
                range: Range::pos(Pos { line: 1, char: 0 }),
            },
        ],
        [Error::MissingQuote(
            Quote::Literal,
            Pos { line: 0, char: 21 },
        )],
    );
}

#[test]
fn unclosed_literal_multi_line_string() {
    check_err(
        "'''some unclosed string\nthis is a new line",
        [
            Token {
                ty: TokenType::String {
                    quote: Quote::LiteralMultiline,
                    lit: "'''some unclosed string\nthis is a new line",
                    text: Cow::Borrowed("some unclosed string\nthis is a new line"),
                    text_range: Range {
                        start: Pos { line: 0, char: 3 },
                        end: Pos { line: 1, char: 18 },
                    },
                },
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 1, char: 18 },
                },
            },
            Token {
                ty: TokenType::EOF,
                range: Range::pos(Pos { line: 1, char: 18 }),
            },
        ],
        [Error::MissingQuote(
            Quote::LiteralMultiline,
            Pos { line: 1, char: 18 },
        )],
    );
}

#[test]
fn not_fully_closed_literal_multi_line_string_1() {
    check_err(
        "'''some unclosed string'",
        [
            Token {
                ty: TokenType::String {
                    quote: Quote::LiteralMultiline,
                    lit: "'''some unclosed string'",
                    text: Cow::Borrowed("some unclosed string'"),
                    text_range: Range {
                        start: Pos { line: 0, char: 3 },
                        end: Pos { line: 0, char: 24 },
                    },
                },
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 24 },
                },
            },
            Token {
                ty: TokenType::EOF,
                range: Range::pos(Pos { line: 0, char: 24 }),
            },
        ],
        [Error::MissingQuote(
            Quote::LiteralMultiline,
            Pos { line: 0, char: 24 },
        )],
    );
}

#[test]
fn not_fully_closed_literal_multi_line_string_2() {
    check_err(
        "'''some unclosed string''",
        [
            Token {
                ty: TokenType::String {
                    quote: Quote::LiteralMultiline,
                    lit: "'''some unclosed string''",
                    text: Cow::Borrowed("some unclosed string''"),
                    text_range: Range {
                        start: Pos { line: 0, char: 3 },
                        end: Pos { line: 0, char: 25 },
                    },
                },
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 25 },
                },
            },
            Token {
                ty: TokenType::EOF,
                range: Range::pos(Pos { line: 0, char: 25 }),
            },
        ],
        [Error::MissingQuote(
            Quote::LiteralMultiline,
            Pos { line: 0, char: 25 },
        )],
    );
}

#[test]
fn comment_without_newline() {
    check(
        "# hello there",
        [
            Token {
                ty: TokenType::Comment(" hello there"),
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 13 },
                },
            },
            Token {
                ty: TokenType::EOF,
                range: Range::pos(Pos { line: 0, char: 13 }),
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
                ty: TokenType::Comment(" hello there"),
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 13 },
                },
            },
            Token {
                ty: TokenType::Newline,
                range: Range {
                    start: Pos { line: 0, char: 13 },
                    end: Pos { line: 1, char: 0 },
                },
            },
            Token {
                ty: TokenType::EOF,
                range: Range::pos(Pos { line: 1, char: 0 }),
            },
        ],
    )
}
