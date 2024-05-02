use common::{FmtChar, FmtStr, Span};

use crate::datetime::{Date, DateTime};
use crate::lex::CharIter;
use crate::parse::num::IntPrefix;
use crate::parse::{num, unexpected_char, Sign};
use crate::Error;

/// A possibly only partially parsed value
pub enum PartialValue {
    Float(f64),
    Int(i64),
    Bool(bool),
    /// A complete date-time with `T` separating date and time.
    DateTime(DateTime),
    /// Just the date part, might be followed by the time part. The toml spec allows using a space
    /// instead of `T` to separate the date and time parts, in that case only the date is parsed
    /// since the time part is inside the next token.
    PartialDate(Date),
    /// An invalid datetime, parsed up to a certain point, this is used to possibly consume
    /// the next tokens in order to avoid excessive errors.
    InvalidDateTime(Error),
}

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

pub fn parse_literal(lit: &str, span: Span) -> Result<PartialValue, Error> {
    let mut chars = lit.char_indices().peekable();
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
                Some((_, '0')) => {
                    num::parse_prefixed_int_float_or_date(chars, lit, span, Some(sign))
                }
                Some((_, c @ '1'..='9')) => {
                    let num = (c as u32 - '0' as u32) as i64;
                    num::parse_decimal_int_float_or_date(chars, lit, span, num, Some(sign))
                }
                Some((i, 'i' | 'I')) => {
                    parse_bare_literal(chars, lit, span, i, c, "inf")?;
                    let val = sign.val() as f64 * f64::INFINITY;
                    Ok(PartialValue::Float(val))
                }
                Some((i, 'n' | 'N')) => {
                    parse_bare_literal(chars, lit, span, i, c, "nan")?;
                    let val = match sign {
                        Sign::Positive => f64::NAN,
                        Sign::Negative => -f64::NAN,
                    };
                    Ok(PartialValue::Float(val))
                }
                Some((i, c)) => {
                    let pos = span.start.plus(i as u32);
                    match c {
                        '.' => Err(Error::MissingNumDigitsAfterSign(sign, pos)),
                        _ => unexpected_char!(IntOrFloat, c, pos),
                    }
                }
                None => Err(Error::MissingNumDigitsAfterSign(sign, span.end)),
            }
        }
        '0' => num::parse_prefixed_int_float_or_date(chars, lit, span, None),
        '1'..='9' => {
            let num = (c as u32 - '0' as u32) as i64;
            num::parse_decimal_int_float_or_date(chars, lit, span, num, None)
        }
        'f' | 'F' => {
            parse_bare_literal(chars, lit, span, 0, c, "false")?;
            Ok(PartialValue::Bool(false))
        }
        't' | 'T' => {
            parse_bare_literal(chars, lit, span, 0, c, "true")?;
            Ok(PartialValue::Bool(true))
        }
        'i' | 'I' => {
            parse_bare_literal(chars, lit, span, 0, c, "inf")?;
            Ok(PartialValue::Float(f64::INFINITY))
        }
        'n' | 'N' => {
            parse_bare_literal(chars, lit, span, 0, c, "nan")?;
            Ok(PartialValue::Float(f64::NAN))
        }
        '_' => Err(Error::LitStartsWithUnderscore(LitPart::Generic, span.start)),
        _ => Err(Error::UnexpectedLiteralStart(FmtChar(c), span.start)),
    }
}

fn parse_bare_literal(
    mut chars: CharIter,
    lit: &str,
    span: Span,
    i: usize,
    first: char,
    expected: &'static str,
) -> Result<(), Error> {
    let mut expected_iter = expected.chars().skip(1);

    if first.is_uppercase() {
        let pos = span.start.plus(i as u32);
        return Err(Error::UppercaseBareLitChar(FmtChar(first), expected, pos));
    }

    while let Some((i, c)) = chars.next() {
        let Some(e) = expected_iter.next() else {
            let span = Span::new(span.start.plus(i as u32), span.end);
            let trailing = FmtStr::from_str(&lit[i..]);
            return Err(Error::BareLitTrailingChars(trailing, expected, span));
        };

        if c != e {
            let pos = span.start.plus(i as u32);
            if c.to_ascii_lowercase() == e {
                return Err(Error::UppercaseBareLitChar(FmtChar(c), expected, pos));
            } else {
                return Err(Error::UnexpectedBareLitChar(FmtChar(c), expected, pos));
            }
        }
    }

    if expected_iter.next().is_some() {
        return Err(Error::BareLitMissingChars(expected, span.end));
    }

    Ok(())
}

/// # SAFETY
/// The literals have to reference the same string
pub unsafe fn concat_strs<'a>(left: &'a str, right: &'a str) -> &'a str {
    let ptr = left.as_ptr();
    let len = (right.as_ptr() as usize - left.as_ptr() as usize) + right.len();
    let slice = std::slice::from_raw_parts(ptr, len);
    std::str::from_utf8_unchecked(slice)
}

/// # SAFETY
/// At least `additional` bytes are required inside the source string after the `lit` slice.
pub unsafe fn extend_str_back<'a>(lit: &'a str, additional: usize) -> &'a str {
    let ptr = lit.as_ptr();
    let len = lit.len() + additional;
    let slice = std::slice::from_raw_parts(ptr, len);
    std::str::from_utf8_unchecked(slice)
}

/// # SAFETY
/// At least `additional` bytes are required inside the source string before the `lit` slice.
pub unsafe fn extend_str_front<'a>(lit: &'a str, additional: usize) -> &'a str {
    let ptr = lit.as_ptr().sub(additional);
    let len = lit.len() + additional;
    let slice = std::slice::from_raw_parts(ptr, len);
    std::str::from_utf8_unchecked(slice)
}
