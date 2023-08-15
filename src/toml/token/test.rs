use super::*;

fn check<const SIZE: usize>(input: &str, expected: [Token; SIZE]) {
    let mut ctx = Ctx::default();
    let tokens = ctx.tokenize(input).unwrap();
    assert_eq!(tokens, expected);
}

fn check_err<const SIZE: usize, const ERR_SIZE: usize>(
    input: &str,
    expected: [Token; SIZE],
    expected_errors: [Error; ERR_SIZE],
) {
    let mut ctx = Ctx::default();
    let tokens = ctx.tokenize(input).unwrap();
    assert_eq!(tokens, expected);
    assert_eq!(ctx.errors, expected_errors);
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
                typ: TokenType::Ident,
                text: "my_int".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 7 },
                    end: Pos { line: 0, char: 8 },
                },
                typ: TokenType::Equal,
                text: "=".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 9 },
                    end: Pos { line: 0, char: 14 },
                },
                typ: TokenType::Int(98742),
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
                typ: TokenType::Ident,
                text: "my_float".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 8 },
                    end: Pos { line: 0, char: 9 },
                },
                typ: TokenType::Equal,
                text: "=".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 9 },
                    end: Pos { line: 0, char: 13 },
                },
                typ: TokenType::Float(0.23),
                text: "0.23".to_string(),
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
                typ: TokenType::Ident,
                text: "my".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 2 },
                    end: Pos { line: 0, char: 3 },
                },
                typ: TokenType::Dot,
                text: ".".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 3 },
                    end: Pos { line: 0, char: 9 },
                },
                typ: TokenType::Ident,
                text: "string".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 10 },
                    end: Pos { line: 0, char: 11 },
                },
                typ: TokenType::Equal,
                text: "=".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 12 },
                    end: Pos { line: 0, char: 19 },
                },
                typ: TokenType::String(Quote::Literal),
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
                typ: TokenType::Ident,
                text: "my".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 2 },
                    end: Pos { line: 0, char: 3 },
                },
                typ: TokenType::Dot,
                text: ".".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 3 },
                    end: Pos { line: 0, char: 10 },
                },
                typ: TokenType::Ident,
                text: "escaped".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 10 },
                    end: Pos { line: 0, char: 11 },
                },
                typ: TokenType::Dot,
                text: ".".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 11 },
                    end: Pos { line: 0, char: 17 },
                },
                typ: TokenType::Ident,
                text: "string".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 18 },
                    end: Pos { line: 0, char: 19 },
                },
                typ: TokenType::Equal,
                text: "=".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 20 },
                    end: Pos { line: 0, char: 33 },
                },
                typ: TokenType::String(Quote::Basic),
                text: "a\u{93f2}nope".to_string(),
            },
        ],
    );
}

#[test]
fn multiline_string_escaped_newline() {
    check(
        "\"\"\"look \\\n    the final string \\\n    is just one \\\n    line\\\n\"\"\"",
        [
            Token {
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 4, char: 3 },
                },
                typ: TokenType::String(Quote::BasicMultiline),
                text: "look the final string is just one line".to_string(),
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
                range: Range {
                    start: Pos { line: 0, char: 0 },
                    end: Pos { line: 0, char: 53 },
                },
                typ: TokenType::String(Quote::LiteralMultiline),
                text: "this doesn't end the string: '' but this does: ".to_string(),
            },
        ],
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
                typ: TokenType::Ident,
                text: "m_string".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 9 },
                    end: Pos { line: 0, char: 10 },
                },
                typ: TokenType::Equal,
                text: "=".to_string(),
            },
            Token {
                range: Range {
                    start: Pos { line: 0, char: 11 },
                    end: Pos { line: 6, char: 7 },
                },
                typ: TokenType::String(Quote::LiteralMultiline),
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
            typ: TokenType::String(Quote::Basic),
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
            typ: TokenType::String(Quote::BasicMultiline),
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
            typ: TokenType::String(Quote::BasicMultiline),
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
            typ: TokenType::String(Quote::BasicMultiline),
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
            typ: TokenType::String(Quote::Literal),
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
            typ: TokenType::String(Quote::LiteralMultiline),
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
            typ: TokenType::String(Quote::LiteralMultiline),
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
            typ: TokenType::String(Quote::LiteralMultiline),
            text: "some unclosed string''".to_string(),
        }],
        [Error::MissingQuote(
            Quote::LiteralMultiline,
            Pos { line: 0, char: 25 },
        )],
    );
}
