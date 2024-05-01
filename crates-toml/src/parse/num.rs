use common::{FmtChar, Pos, Span};

use crate::datetime::DateTimeField;
use crate::lex::CharIter;
use crate::parse::{datetime, unexpected_char, PartialValue};
use crate::Error;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LitPart {
    Generic,
    IntOrFloat,
    PrefixedInt(IntPrefix),
    FloatIntegral,
    FloatFract,
    FloatExp,
}

impl LitPart {
    pub fn to_str(&self) -> &'static str {
        match self {
            LitPart::Generic => "literal",
            LitPart::IntOrFloat => "integer or float",
            LitPart::PrefixedInt(IntPrefix::Binary) => "binary integer",
            LitPart::PrefixedInt(IntPrefix::Octal) => "octal integer",
            LitPart::PrefixedInt(IntPrefix::Hexadecimal) => "hexadecimal integer",
            LitPart::FloatIntegral => "float integral",
            LitPart::FloatFract => "float fractional part",
            LitPart::FloatExp => "float exponent",
        }
    }
}

impl std::fmt::Display for LitPart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_str())
    }
}

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

pub fn parse_decimal_int_float_or_date(
    mut chars: CharIter,
    literal: &str,
    span: Span,
    mut int_accum: i64,
    sign_char: Option<Sign>,
) -> Result<PartialValue, Error> {
    #[derive(PartialEq, Eq)]
    enum NumParseState {
        Int,
        OverflowOrFloat,
    }

    let sign = sign_char.map_or(1, |s| s.val());
    int_accum *= sign;
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
                        let val = int_accum
                            .checked_mul(10)
                            .and_then(|v| v.checked_add(sign * digit as i64));
                        match val {
                            Some(val) => int_accum = val,
                            None => parse_state = NumParseState::OverflowOrFloat,
                        }
                    }
                    // The literal would overflow if it was an int, but it could be a float.
                    NumParseState::OverflowOrFloat => {}
                }
            }
            'e' | 'E' => {
                if last_underscore {
                    let pos = span.start.plus(i as u32).minus(1);
                    return Err(Error::LitEndsWithUnderscore(LitPart::FloatIntegral, pos));
                }

                return validate_float_exponent(chars, span);
            }
            ':' if sign_char.is_none() && i == 2 => {
                let hour = int_accum as u8;
                return match datetime::continue_parsing_local_time(&mut chars, span, hour) {
                    Ok(t) => Ok(PartialValue::PartialTime(t)),
                    Err(e) => Ok(PartialValue::InvalidTime(e)),
                };
            }
            '-' if sign_char.is_none() && i == 4 => {
                let year = int_accum as u16;
                return match datetime::continue_parsing_date_time_after_year(&mut chars, span, year)
                {
                    Ok(v) => Ok(v),
                    Err(e) => Ok(PartialValue::InvalidDateTime(e)),
                };
            }
            ':' if sign_char.is_none() && i <= 3 => {
                let err = match i {
                    ..=3 => {
                        Error::DateTimeIncomplete(DateTimeField::Hour, span.start.plus(i as u32))
                    }
                    4.. => {
                        Error::DateTimeExpectedCharFound {
                            after: DateTimeField::Hour,
                            expected: FmtChar(':'),
                            // valid becasuse all characters before were ascii chars
                            found: FmtChar(literal.as_bytes()[2] as char),
                            pos: span.start.plus(2),
                        }
                    }
                };
                return Ok(PartialValue::InvalidDateTime(err));
            }
            '-' if sign_char.is_none() && i <= 6 => {
                let err = match i {
                    ..=3 => {
                        Error::DateTimeIncomplete(DateTimeField::Year, span.start.plus(i as u32))
                    }
                    4.. => {
                        Error::DateTimeExpectedCharFound {
                            after: DateTimeField::Year,
                            expected: FmtChar('-'),
                            // valid becasuse all characters before were ascii chars
                            found: FmtChar(literal.as_bytes()[4] as char),
                            pos: span.start.plus(4),
                        }
                    }
                };
                return Ok(PartialValue::InvalidDateTime(err));
            }
            '_' => {
                if last_underscore {
                    let start = span.start.plus(i as u32 - 1);
                    return consecutive_underscore_error(chars, start);
                }
            }
            _ => {
                let pos = span.start.plus(i as u32);
                return unexpected_char!(IntOrFloat, c, pos);
            }
        }

        last_underscore = c == '_';
    }

    if last_underscore {
        let pos = span.end.minus(1);
        return Err(Error::LitEndsWithUnderscore(LitPart::IntOrFloat, pos));
    }

    match parse_state {
        NumParseState::Int => Ok(PartialValue::Int(int_accum)),
        NumParseState::OverflowOrFloat => Ok(PartialValue::OverflowOrFloat),
    }
}

pub fn parse_prefixed_int_or_date(
    mut chars: CharIter,
    span: Span,
    sign_char: Option<Sign>,
) -> Result<PartialValue, Error> {
    let Some((i, c)) = chars.next() else {
        return Ok(PartialValue::Int(0));
    };
    match c {
        'b' | 'B' | 'o' | 'O' | 'x' | 'X' => {
            if sign_char.is_some() {
                return Err(Error::PrefixedIntSignNotAllowed(span.start));
            }

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
            Ok(PartialValue::PrefixedInt(val))
        }
        'e' | 'E' => validate_float_exponent(chars, span),
        '0'..='9' if sign_char.is_none() => {
            let two_digits = c as u16 - '0' as u16;
            match datetime::continue_parsing_date_time(&mut chars, span, two_digits) {
                Ok(v) => Ok(v),
                Err(e) => Ok(PartialValue::InvalidDateTime(e)),
            }
        }
        '0'..='9' => {
            let pos = span.start.plus(1);
            Err(Error::InvalidLeadingZero(pos))
        }
        '_' => {
            let pos = span.start.plus(sign_char.is_some() as u32);
            Err(Error::InvalidLeadingZero(pos))
        }
        _ => {
            let pos = span.start.plus(i as u32);
            if sign_char.is_some() {
                Err(Error::UnexpectedCharSignedLeadingZeroNum(FmtChar(c), pos))
            } else {
                Err(Error::ExpectedRadixOrDateTime(FmtChar(c), pos))
            }
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
            'g'..='z' | 'G'..='Z' if prefix == IntPrefix::Hexadecimal => {
                let pos = span.start.plus(i as u32);
                return Err(Error::IntDigitTooBig(prefix, FmtChar(c), pos));
            }
            '_' => {
                if j == 0 {
                    let pos = span.start.plus(i as u32);
                    return Err(Error::PrefixedIntValueStartsWithUnderscore(pos));
                } else if last_underscore {
                    let start = span.start.plus(i as u32 - 1);
                    return consecutive_underscore_error(chars, start);
                }
                last_underscore = true;
                continue;
            }
            _ => {
                let pos = span.start.plus(i as u32);
                return unexpected_char!(PrefixedInt(prefix), c, pos);
            }
        };

        let (val, overflow) = accum.overflowing_shl(prefix.bits());
        if overflow {
            return Err(Error::IntLiteralOverflow(span));
        }

        accum = val;
        accum += digit as i64;

        last_underscore = c == '_';
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

        match c {
            '0'..='9' => {}
            '_' => {
                if j == 0 {
                    let pos = span.start.plus(i as u32);
                    return Err(Error::LitStartsWithUnderscore(LitPart::FloatExp, pos));
                } else if last_underscore {
                    let start = span.start.plus(i as u32 - 1);
                    return consecutive_underscore_error(chars, start);
                }
            }
            _ => {
                let pos = span.start.plus(i as u32);
                return unexpected_char!(FloatExp, c, pos);
            }
        }

        last_underscore = c == '_';
    }

    if last_underscore {
        let pos = span.end.minus(1);
        return Err(Error::LitEndsWithUnderscore(LitPart::FloatExp, pos));
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
                    return Err(Error::LitEndsWithUnderscore(LitPart::FloatFract, pos));
                }

                if let Some((_, '-' | '+')) = chars.peek() {
                    chars.next();
                }

                for j in 0.. {
                    let Some((i, c)) = chars.next() else { break };

                    match c {
                        '0'..='9' => {}
                        '_' => {
                            if j == 0 {
                                let pos = span.start.plus(i as u32);
                                return Err(Error::LitStartsWithUnderscore(LitPart::FloatExp, pos));
                            } else if last_underscore {
                                let start = span.start.plus(i as u32 - 1);
                                return consecutive_underscore_error(chars, start);
                            }
                        }
                        _ => {
                            let pos = span.start.plus(i as u32);
                            return unexpected_char!(FloatExp, c, pos);
                        }
                    }

                    last_underscore = c == '_';
                }

                if last_underscore {
                    let pos = span.end.minus(1);
                    return Err(Error::LitEndsWithUnderscore(LitPart::FloatExp, pos));
                }

                return Ok(());
            }
            '_' => {
                if i == 0 {
                    let pos = span.start.plus(i as u32);
                    return Err(Error::LitStartsWithUnderscore(LitPart::FloatFract, pos));
                } else if last_underscore {
                    let start = span.start.plus(i as u32 - 1);
                    return consecutive_underscore_error(chars, start);
                }
            }
            _ => {
                let pos = span.start.plus(i as u32);
                return unexpected_char!(FloatFract, c, pos);
            }
        }

        last_underscore = c == '_';
    }

    if last_underscore {
        let pos = span.end.minus(1);
        return Err(Error::LitEndsWithUnderscore(LitPart::FloatFract, pos));
    }

    Ok(())
}

fn consecutive_underscore_error<T>(mut chars: CharIter, start: Pos) -> Result<T, Error> {
    let mut end = start.plus(2);
    while let Some((_, '_')) = chars.peek() {
        chars.next();
        end.char += 1;
    }

    Err(Error::ConsecutiveUnderscoresInLiteral(Span::new(
        start, end,
    )))
}
