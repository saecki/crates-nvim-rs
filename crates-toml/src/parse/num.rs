use common::{FmtChar, Span};

use crate::lex::CharIter;
use crate::parse::{datetime, PartialValue};
use crate::Error;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Sign {
    Positive,
    Negative,
}

impl std::fmt::Display for Sign {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Write;

        match self {
            Sign::Positive => f.write_char('+'),
            Sign::Negative => f.write_char('-'),
        }
    }
}

impl Sign {
    #[inline(always)]
    pub fn val(&self) -> i64 {
        match self {
            Sign::Positive => 1,
            Sign::Negative => -1,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntPrefix {
    Binary = 1,
    Octal = 3,
    Hexadecimal = 4,
}

impl IntPrefix {
    #[inline(always)]
    pub fn bits(&self) -> u32 {
        *self as u32
    }
}

/// Parse all integers adhering to the toml spec, the integer part of a float and it's exponent or
/// a date-time adhering to the RFC 3339 spec. The toml spec allows using a space instead of `T` to
/// separate the date and time parts, in that case only the date is parsed since the time part is
/// inside the next token.
pub fn parse_num_or_date(literal: &str, span: Span) -> Result<PartialValue, Error> {
    let mut chars = literal.char_indices().peekable();
    let c = match chars.next() {
        None => unreachable!("value literal should never be emtpy"),
        Some((_, c)) => c,
    };

    match c {
        '+' | '-' => {
            let sign = match c {
                '+' => Sign::Positive,
                '-' => Sign::Negative,
                _ => unsafe { core::hint::unreachable_unchecked() },
            };

            match chars.next() {
                Some((_, '0')) => parse_prefixed_int_or_date(chars, span, Some(sign)),
                Some((_, c @ '1'..='9')) => {
                    let num = (c as u32 - '0' as u32) as i64;
                    parse_decimal_int_float_or_date(chars, span, num, Some(sign))
                }
                Some((i, c)) => {
                    let pos = span.start.plus(i as u32);
                    Err(Error::InvalidCharInNumLiteral(FmtChar(c), pos))
                }
                None => Err(Error::MissingNumDigitsAfterSign(sign, span.end)),
            }
        }
        '0' => parse_prefixed_int_or_date(chars, span, None),
        '1'..='9' => {
            let num = (c as u32 - '0' as u32) as i64;
            parse_decimal_int_float_or_date(chars, span, num, None)
        }
        '_' => Err(Error::NumOrDateLiteralStartsWithUnderscore(span.start)),
        _ => Err(Error::InvalidNumOrDateLiteralStart(FmtChar(c), span.start)),
    }
}

fn parse_decimal_int_float_or_date(
    mut chars: CharIter,
    span: Span,
    mut int_accum: i64,
    sign_char: Option<Sign>,
) -> Result<PartialValue, Error> {
    #[derive(PartialEq, Eq)]
    enum NumParseState {
        Int,
        OverflowOrFloat,
    }

    let mut parse_state = NumParseState::Int;
    let mut last_underscore = false;
    loop {
        let Some((i, c)) = chars.next() else { break };

        match c {
            '0'..='9' => {
                match parse_state {
                    // the literal seems to be an integer
                    NumParseState::Int => {
                        let digit = (c as u32) - ('0' as u32);
                        let (val, overflow) = int_accum.overflowing_mul(10);
                        if overflow {
                            parse_state = NumParseState::OverflowOrFloat;
                        } else {
                            int_accum = val;
                            int_accum += digit as i64;
                        }
                    }
                    // The literal would overflow if it was an int, but it could be a float.
                    NumParseState::OverflowOrFloat => {}
                }
            }
            'e' | 'E' => {
                if last_underscore {
                    let pos = span.start.plus(i as u32);
                    return Err(Error::FloatIntegralEndsWithUnderscore(pos));
                }

                return validate_float_exponent(chars, span)
            }
            ':' if sign_char.is_none() && i == 2 => {
                let hour = int_accum as u8;
                return datetime::continue_parsing_local_time(&mut chars, span, hour)
                    .map(PartialValue::PartialTime);
            }
            '-' if sign_char.is_none() && i == 4 => {
                let year = int_accum as u16;
                return datetime::continue_parsing_date_time_after_year(&mut chars, span, year);
            }
            '_' => {
                last_underscore = true;
                continue;
            }
            _ => {
                let pos = span.start.plus(i as u32);
                return Err(Error::InvalidCharInNumLiteral(FmtChar(c), pos));
            }
        }

        last_underscore = c == '_';
    }

    if last_underscore {
        let pos = span.end.minus(1);
        return Err(Error::NumLiteralEndsWithUnderscore(pos));
    }

    match parse_state {
        NumParseState::Int => {
            let sign = sign_char.map_or(1, |s| s.val());
            Ok(PartialValue::Int(sign * int_accum))
        }
        NumParseState::OverflowOrFloat => Ok(PartialValue::OverflowOrFloat),
    }
}

fn parse_prefixed_int_or_date(
    mut chars: CharIter,
    span: Span,
    sign_char: Option<Sign>,
) -> Result<PartialValue, Error> {
    let Some((i, c)) = chars.next() else {
        return Ok(PartialValue::Int(0));
    };
    match c {
        'b' | 'B' | 'o' | 'O' | 'x' | 'X' => {
            let sign = match sign_char {
                Some(Sign::Positive) => {
                    return Err(Error::PrefixedIntPositiveSignNotAllowed(span.start));
                }
                Some(Sign::Negative) => -1,
                None => 1,
            };

            let prefix = match c {
                'b' | 'B' => IntPrefix::Binary,
                'o' | 'O' => IntPrefix::Octal,
                'x' | 'X' => IntPrefix::Hexadecimal,
                _ => unsafe { core::hint::unreachable_unchecked() },
            };
            if c.is_uppercase() {
                let pos = span.start.plus(i as u32);
                return Err(Error::UppercaseIntRadix(prefix, pos));
            }
            let val = parse_prefixed_int_literal(chars, span, prefix)?;
            Ok(PartialValue::PrefixedInt(sign * val))
        }
        'e' | 'E' => validate_float_exponent(chars, span),
        '0'..='9' if sign_char.is_none() => {
            let two_digits = c as u16 - '0' as u16;
            datetime::continue_parsing_date_time(&mut chars, span, two_digits)
        }
        _ => {
            let pos = span.start.plus(i as u32);
            Err(Error::InvalidIntRadix(FmtChar(c), pos))
        }
    }
}

fn parse_prefixed_int_literal(
    mut chars: CharIter,
    span: Span,
    prefix: IntPrefix,
) -> Result<i64, Error> {
    let max_value: u32 = 1 << prefix.bits();
    let mut accum: i64 = 0;
    let mut last_underscore = false;

    for j in 0.. {
        let Some((i, c)) = chars.next() else {
            if j == 0 {
                return Err(Error::EmptyPrefixedIntValue(span.end));
            }

            break;
        };

        last_underscore = false;

        let digit = match c {
            '0'..='9' => {
                let n = c as u32 - '0' as u32;
                if n >= max_value {
                    let pos = span.start.plus(i as u32);
                    return Err(Error::IntDigitTooBig(prefix, FmtChar(c), pos));
                }
                n
            }
            'a'..='f' => {
                let n = 10 + c as u32 - 'a' as u32;
                if n >= max_value {
                    let pos = span.start.plus(i as u32);
                    return Err(Error::IntDigitTooBig(prefix, FmtChar(c), pos));
                }
                n
            }
            'A'..='F' => {
                let n = 10 + (c as u32 - 'A' as u32);
                if n >= max_value {
                    let pos = span.start.plus(i as u32);
                    return Err(Error::IntDigitTooBig(prefix, FmtChar(c), pos));
                }
                n
            }
            '_' => {
                if j == 0 {
                    let pos = span.start.plus(i as u32);
                    return Err(Error::PrefixedIntValueStartsWithUnderscore(pos));
                }
                last_underscore = true;
                continue;
            }
            _ => {
                let pos = span.start.plus(i as u32);
                return Err(Error::InvalidCharInPrefixedInt(FmtChar(c), pos));
            }
        };

        let (val, overflow) = accum.overflowing_shl(prefix.bits());
        if overflow {
            return Err(Error::IntLiteralOverflow(span));
        }

        accum = val;
        accum += digit as i64;
    }

    if last_underscore {
        let pos = span.end.minus(1);
        return Err(Error::PrefixedIntValueEndsWithUnderscore(pos));
    }

    Ok(accum)
}

fn validate_float_exponent(mut chars: CharIter, span: Span) -> Result<PartialValue, Error> {
    if let Some((_, '-' | '+')) = chars.peek() {
        chars.next();
    }

    let mut last_underscore = false;
    for j in 0.. {
        let Some((i, c)) = chars.next() else {
            break;
        };

        last_underscore = false;

        match c {
            '0'..='9' => {}
            '_' => {
                if j == 0 {
                    let pos = span.start.plus(i as u32);
                    return Err(Error::FloatExponentStartsWithUnderscore(pos));
                }
                last_underscore = true;
                continue;
            }
            _ => {
                let pos = span.start.plus(i as u32);
                return Err(Error::InvalidCharInFloatExponent(FmtChar(c), pos));
            }
        }
    }

    if last_underscore {
        let pos = span.end.minus(1);
        return Err(Error::FloatExponentEndsWithUnderscore(pos));
    }

    Ok(PartialValue::FloatWithExp)
}

pub fn validate_float_fractional_part(literal: &str, span: Span) -> Result<(), Error> {
    let mut chars = literal.char_indices().peekable();
    let mut last_underscore = false;
    loop {
        let Some((i, c)) = chars.next() else { break };

        match c {
            '0'..='9' => {}
            'e' | 'E' => {
                if i == 0 {
                    return Err(Error::MissingFloatFractionalPart(span.start));
                }

                if last_underscore {
                    let pos = span.start.plus(i as u32 - 1);
                    return Err(Error::FloatFractEndsWithUnderscore(pos));
                }

                if let Some((_, '-' | '+')) = chars.peek() {
                    chars.next();
                }

                for j in 0.. {
                    let Some((i, c)) = chars.next() else { break };

                    last_underscore = false;

                    match c {
                        '0'..='9' => {}
                        '_' => {
                            if j == 0 {
                                let pos = span.start.plus(i as u32);
                                return Err(Error::FloatExponentStartsWithUnderscore(pos));
                            }
                            last_underscore = true;
                            continue;
                        }
                        _ => {
                            let pos = span.start.plus(i as u32);
                            return Err(Error::InvalidCharInFloatExponent(FmtChar(c), pos));
                        }
                    }
                }

                if last_underscore {
                    let pos = span.end.minus(1);
                    return Err(Error::FloatExponentEndsWithUnderscore(pos));
                }

                return Ok(());
            }
            '_' => {
                if i == 0 {
                    let pos = span.start.plus(i as u32);
                    return Err(Error::FloatFractStartsWithUnderscore(pos));
                }
            }
            _ => {
                let pos = span.start.plus(i as u32);
                return Err(Error::InvalidCharInFloatLiteral(FmtChar(c), pos));
            }
        }

        last_underscore = c == '_';
    }

    if last_underscore {
        let pos = span.end.minus(1);
        return Err(Error::FloatIntegralEndsWithUnderscore(pos));
    }

    Ok(())
}
