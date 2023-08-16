use super::*;

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

fn check_str(input: &str, expected_str: &str) {
    let mut ctx = Ctx::default();
    let tokens = ctx.lex(input).unwrap();
    assert_eq!(tokens.len(), 1);
    if ctx.errors != [] {
        assert_eq!(ctx.errors, []);
    }
    assert_eq!(ctx.warnings, []);

    let token = tokens.into_iter().next().unwrap();
    assert_eq!(token.ty, TokenType::String(Quote::Basic));
    assert_eq!(token.text, expected_str);
}

#[test]
fn assign_int() {
    check(
        "my_int = 98742",
        [
            Token {
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 6 },
                },
                ty: TokenType::Ident,
                text: "my_int".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 7 },
                    end: Pos { line: 0, char: 8 },
                },
                ty: TokenType::Equal,
                text: "=".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 9 },
                    end: Pos { line: 0, char: 14 },
                },
                ty: TokenType::Int(98742),
                text: "98742".to_string(),
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
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 8 },
                },
                ty: TokenType::Ident,
                text: "my_float".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 8 },
                    end: Pos { line: 0, char: 9 },
                },
                ty: TokenType::Equal,
                text: "=".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 9 },
                    end: Pos { line: 0, char: 10 },
                },
                ty: TokenType::Int(0),
                text: "0".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 10 },
                    end: Pos { line: 0, char: 11 },
                },
                ty: TokenType::Dot,
                text: ".".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 11 },
                    end: Pos { line: 0, char: 13 },
                },
                ty: TokenType::Int(23),
                text: "23".to_string(),
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
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 2 },
                },
                ty: TokenType::Ident,
                text: "my".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 2 },
                    end: Pos { line: 0, char: 3 },
                },
                ty: TokenType::Dot,
                text: ".".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 3 },
                    end: Pos { line: 0, char: 9 },
                },
                ty: TokenType::Ident,
                text: "string".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 10 },
                    end: Pos { line: 0, char: 11 },
                },
                ty: TokenType::Equal,
                text: "=".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 12 },
                    end: Pos { line: 0, char: 19 },
                },
                ty: TokenType::String(Quote::Literal),
                text: "yeet\\".to_string(),
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
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 2 },
                },
                ty: TokenType::Ident,
                text: "my".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 2 },
                    end: Pos { line: 0, char: 3 },
                },
                ty: TokenType::Dot,
                text: ".".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 3 },
                    end: Pos { line: 0, char: 10 },
                },
                ty: TokenType::Ident,
                text: "escaped".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 10 },
                    end: Pos { line: 0, char: 11 },
                },
                ty: TokenType::Dot,
                text: ".".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 11 },
                    end: Pos { line: 0, char: 17 },
                },
                ty: TokenType::Ident,
                text: "string".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 18 },
                    end: Pos { line: 0, char: 19 },
                },
                ty: TokenType::Equal,
                text: "=".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 20 },
                    end: Pos { line: 0, char: 33 },
                },
                ty: TokenType::String(Quote::Basic),
                text: "a\u{93f2}nope".to_string(),
            },
        ],
    );
}

#[test]
fn string_escapes() {
    check_str(r#""\b""#, "\x08");
    check_str(r#""\t""#, "\t");
    check_str(r#""\n""#, "\n");
    check_str(r#""\f""#, "\u{C}");
    check_str(r#""\r""#, "\r");
}

#[test]
fn unicode_escapes() {
    check_str(r#""\u001b""#, "\x1b");
    check_str(r#""\u001a""#, "\u{1a}");
    check_str(r#""\u03a0""#, "\u{03a0}");
    check_str(r#""\U00102230""#, "\u{102230}");
    check_str(r#"  "\u03c0"  "#, "\u{03c0}");
}

#[test]
fn multiline_string_escaped_newline() {
    check(
        "\"\"\"look \\\n    the final string \\\n    is just one \\\n    line\\\n\"\"\"",
        [Token {
            range: Range {
                start: Pos { line: 0, char: 0 },
                end: Pos { line: 4, char: 3 },
            },
            ty: TokenType::String(Quote::BasicMultiline),
            text: "look the final string is just one line".to_string(),
        }],
    );
}

#[test]
fn multiline_string_contains_up_to_two_quotes() {
    check(
        "'''this doesn't end the string: '' but this does: '''",
        [Token {
            range: Range {
                start: Pos { line: 0, char: 0 },
                end: Pos { line: 0, char: 53 },
            },
            ty: TokenType::String(Quote::LiteralMultiline),
            text: "this doesn't end the string: '' but this does: ".to_string(),
        }],
    );
}

#[test]
fn assign_multiline_string() {
    check(
        "m_string = '''each\nword\nis\non\na\nnew\nline'''",
        [
            Token {
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 8 },
                },
                ty: TokenType::Ident,
                text: "m_string".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 9 },
                    end: Pos { line: 0, char: 10 },
                },
                ty: TokenType::Equal,
                text: "=".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 11 },
                    end: Pos { line: 6, char: 7 },
                },
                ty: TokenType::String(Quote::LiteralMultiline),
                text: "each\nword\nis\non\na\nnew\nline".to_string(),
            },
        ],
    );
}

#[test]
fn unclosed_basic_single_line_string() {
    check_err(
        "\"some unclosed string\n",
        [Token {
            range: Range {
                start: Pos { line: 0, char: 0 },
                end: Pos { line: 0, char: 21 },
            },
            ty: TokenType::String(Quote::Basic),
            text: "some unclosed string".to_string(),
        }],
        [Error::MissingQuote(Quote::Basic, Pos { line: 0, char: 21 })],
    );
}

#[test]
fn unclosed_basic_multi_line_string() {
    check_err(
        "\"\"\"some unclosed string\nthis is a new line",
        [Token {
            range: Range {
                start: Pos { line: 0, char: 0 },
                end: Pos { line: 1, char: 18 },
            },
            ty: TokenType::String(Quote::BasicMultiline),
            text: "some unclosed string\nthis is a new line".to_string(),
        }],
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
        [Token {
            range: Range {
                start: Pos { line: 0, char: 0 },
                end: Pos { line: 0, char: 24 },
            },
            ty: TokenType::String(Quote::BasicMultiline),
            text: "some unclosed string\"".to_string(),
        }],
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
        [Token {
            range: Range {
                start: Pos { line: 0, char: 0 },
                end: Pos { line: 0, char: 25 },
            },
            ty: TokenType::String(Quote::BasicMultiline),
            text: "some unclosed string\"\"".to_string(),
        }],
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
        [Token {
            range: Range {
                start: Pos { line: 0, char: 0 },
                end: Pos { line: 0, char: 21 },
            },
            ty: TokenType::String(Quote::Literal),
            text: "some unclosed string".to_string(),
        }],
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
        [Token {
            range: Range {
                start: Pos { line: 0, char: 0 },
                end: Pos { line: 1, char: 18 },
            },
            ty: TokenType::String(Quote::LiteralMultiline),
            text: "some unclosed string\nthis is a new line".to_string(),
        }],
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
        [Token {
            range: Range {
                start: Pos { line: 0, char: 0 },
                end: Pos { line: 0, char: 24 },
            },
            ty: TokenType::String(Quote::LiteralMultiline),
            text: "some unclosed string'".to_string(),
        }],
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
        [Token {
            range: Range {
                start: Pos { line: 0, char: 0 },
                end: Pos { line: 0, char: 25 },
            },
            ty: TokenType::String(Quote::LiteralMultiline),
            text: "some unclosed string''".to_string(),
        }],
        [Error::MissingQuote(
            Quote::LiteralMultiline,
            Pos { line: 0, char: 25 },
        )],
    );
}
