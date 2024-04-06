use common::FmtStr;

use crate::inlinestr::InlineStr;
use crate::{
    BuildMetadata, CompVersion, Comparator, Error, IdentField, NumField, Offset, Op, Prerelease,
    Version, VersionReq, WlChar,
};

#[cfg(test)]
mod test;

struct CharIter<'a> {
    str: &'a str,
    idx: usize,
}

impl<'a> CharIter<'a> {
    fn new(str: &'a str) -> Self {
        Self { str, idx: 0 }
    }

    fn peek_byte(&self) -> Option<u8> {
        self.str.as_bytes().get(self.idx).copied()
    }

    fn next_byte(&mut self) {
        if self.idx < self.str.len() {
            self.idx += 1;
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.str[self.idx..].chars().next()
    }

    fn remainder(&self) -> &'a str {
        &self.str[self.idx..]
    }

    fn offset(&self) -> Offset {
        Offset {
            char: self.idx as u32,
        }
    }
}

pub fn parse_requirement(input: &str) -> Result<VersionReq, Error> {
    let mut chars = CharIter::new(input);
    let num_commas = memchr::memchr_iter(b',', input.as_bytes()).count();
    // If there is no comma, there are probably no comparators or one. If it's only one the push
    // won't have to reallocate anyway, but if there are 0 we saved an allocation. For 1 or more
    // commas this might allocate space for one extra item, but will never have to reallocate.
    let capacity = num_commas + (num_commas != 0) as usize;
    let mut comparators = Vec::with_capacity(capacity as usize);
    let mut last_comma = None;

    loop {
        eat_whitespace(&mut chars);

        let Some(b) = chars.peek_byte() else {
            break;
        };

        let op_offset = chars.offset();
        let mut op = match b {
            // Blank version requirement, equivalent to caret
            b'0'..=b'9' => Op::Bl,
            b'*' | b'x' | b'X' => {
                chars.next_byte();
                if !comparators.is_empty() || !chars.remainder().trim().is_empty() {
                    // TODO: store error and continue
                    let offset = chars.offset().minus(1);
                    return Err(Error::WildcardNotTheSoleComparator(offset));
                }

                eat_whitespace(&mut chars);
                last_comma = expect_comma_or_end(&mut chars)?;

                let wl = match b {
                    b'*' => WlChar::Star,
                    b'x' => WlChar::LowerX,
                    b'X' => WlChar::UpperX,
                    _ => unsafe { std::hint::unreachable_unchecked() },
                };
                let comparator = Comparator {
                    op_offset,
                    op: Op::Wl,
                    version_offset: op_offset,
                    version: CompVersion::Wl(wl),
                    comma: last_comma,
                };
                comparators.push(comparator);

                continue;
            }
            b'=' => {
                chars.next_byte();
                Op::Eq
            }
            b'<' => {
                chars.next_byte();
                if chars.peek_byte() == Some(b'=') {
                    chars.next_byte();
                    Op::Le
                } else {
                    Op::Lt
                }
            }
            b'>' => {
                chars.next_byte();
                if chars.peek_byte() == Some(b'=') {
                    chars.next_byte();
                    Op::Ge
                } else {
                    Op::Gt
                }
            }
            b'^' => {
                chars.next_byte();
                Op::Cr
            }
            b'~' => {
                chars.next_byte();
                Op::Tl
            }
            _ => {
                let char = chars.peek_char().unwrap();
                let offset = chars.offset();
                return Err(Error::InvalidOp(char, offset));
            }
        };

        eat_whitespace(&mut chars);
        let version_offset = chars.offset();
        let version = comp_version(&mut chars, &mut op)?;

        eat_whitespace(&mut chars);
        last_comma = expect_comma_or_end(&mut chars)?;

        let comparator = Comparator {
            op_offset,
            op,
            version_offset,
            version,
            comma: last_comma,
        };
        comparators.push(comparator);
    }

    if let Some(offset) = last_comma {
        // TODO: store error and continue
        return Err(Error::TrailingComma(offset));
    }

    Ok(VersionReq { comparators })
}

fn comp_version(chars: &mut CharIter, op: &mut Op) -> Result<CompVersion, Error> {
    let major = parse_int(chars, NumField::Major)?;
    if !eat_dot(chars) {
        return Ok(CompVersion::Major(major));
    }

    let minor = if let Some(wl) = eat_wildcard(chars) {
        if *op == Op::Bl {
            *op = Op::Wl;
        }
        return Ok(CompVersion::MajorWl(major, wl));
    } else {
        parse_int(chars, NumField::Minor)?
    };
    if !eat_dot(chars) {
        return Ok(CompVersion::Minor(major, minor));
    }

    let patch = if let Some(wl) = eat_wildcard(chars) {
        if *op == Op::Bl {
            *op = Op::Wl;
        }
        return Ok(CompVersion::MinorWl(major, minor, wl));
    } else {
        parse_int(chars, NumField::Patch)?
    };
    if !eat_hyphen(chars) {
        return Ok(CompVersion::Patch(major, minor, patch));
    }

    let ident = parse_ident(chars, IdentField::Prerelease)?;
    let str = unsafe { InlineStr::new_unchecked(ident) };
    let pre = Prerelease { str };

    Ok(CompVersion::Pre(major, minor, patch, pre))
}

pub fn parse_version(input: &str) -> Result<Version, Error> {
    let mut chars = CharIter::new(input);

    eat_whitespace(&mut chars);

    let major = parse_int(&mut chars, NumField::Major)?;
    expect_dot(&mut chars, NumField::Major)?;
    let minor = parse_int(&mut chars, NumField::Minor)?;
    expect_dot(&mut chars, NumField::Minor)?;
    let patch = parse_int(&mut chars, NumField::Patch)?;

    let pre = if eat_hyphen(&mut chars) {
        let ident = parse_ident(&mut chars, IdentField::Prerelease)?;
        let str = unsafe { InlineStr::new_unchecked(ident) };
        Prerelease { str }
    } else {
        Prerelease::EMPTY
    };

    let meta = if eat_plus(&mut chars) {
        let ident = parse_ident(&mut chars, IdentField::BuildMetadata)?;
        let str = unsafe { InlineStr::new_unchecked(ident) };
        BuildMetadata { str }
    } else {
        BuildMetadata::EMPTY
    };

    eat_whitespace(&mut chars);

    if chars.peek_byte().is_some() {
        let trailing = FmtStr::from_str(chars.remainder().trim_end());
        let offset = chars.offset();
        return Err(Error::TrailingCharacters(trailing, offset));
    }

    Ok(Version {
        major,
        minor,
        patch,
        pre,
        meta,
    })
}

fn parse_int(chars: &mut CharIter, field: NumField) -> Result<u32, Error> {
    let start = chars.offset();

    let Some(c) = chars.peek_byte() else {
        let offset = chars.offset();
        return Err(Error::MissingField(field, offset));
    };
    let accum = match c {
        b'0' => {
            chars.next_byte();
            return match chars.peek_byte() {
                Some(b'0'..=b'9') => {
                    let offset = chars.offset().minus(1);
                    Err(Error::LeadingZeroNum(field, offset))
                }
                _ => Ok(0),
            };
        }
        b'1'..=b'9' => {
            chars.next_byte();
            c as u32 - b'0' as u32
        }
        _ => {
            let offset = chars.offset();
            let char = chars.peek_char().expect("remainder shouldn't be empty");
            return Err(Error::InvalidIntChar(char, field, offset));
        }
    };

    enum State {
        Ok(u32),
        Overflow,
    }

    let mut accum = State::Ok(accum);

    while let Some(c) = chars.peek_byte() {
        let digit = match c {
            b'0'..=b'9' => {
                chars.next_byte();
                c as u32 - '0' as u32
            }
            _ => break,
        };

        match &mut accum {
            State::Ok(a) => match a.checked_mul(10) {
                Some(val) => *a = val + digit,
                None => accum = State::Overflow,
            },
            State::Overflow => (),
        }
    }

    match accum {
        State::Ok(v) => Ok(v),
        State::Overflow => {
            let len = chars.idx as u32 - start.char;
            Err(Error::IntOverflow(field, start, len))
        }
    }
}

fn parse_ident<'a>(chars: &mut CharIter<'a>, field: IdentField) -> Result<&'a str, Error> {
    let start = chars.idx;
    let mut segment_start = chars.idx;
    let mut segment_has_nondigit = false;

    loop {
        match chars.peek_byte() {
            Some(b'a'..=b'z' | b'A'..=b'Z' | b'-') => {
                chars.next_byte();
                segment_has_nondigit = true;
            }
            Some(b'0'..=b'9') => chars.next_byte(),
            boundary => {
                if segment_start == chars.idx {
                    // TODO: consider reading up to a `+` and returning an invalid character error instead
                    let offset = chars.offset();
                    if start == chars.idx && boundary != Some(b'.') {
                        return Err(Error::EmptyIdentifier(field, offset));
                    } else {
                        return Err(Error::EmptyIdentifierSegment(field, offset));
                    }
                }

                if field == IdentField::Prerelease
                    && chars.idx - segment_start > 1
                    && !segment_has_nondigit
                    && chars.str[segment_start..].starts_with('0')
                {
                    let offset = Offset::new(segment_start as u32);
                    return Err(Error::LeadingZeroSegment(field, offset));
                }

                if boundary == Some(b'.') {
                    chars.next_byte();
                    segment_start = chars.idx;
                    segment_has_nondigit = false;
                } else {
                    let end = chars.idx;
                    let str = &chars.str[start..end];
                    return Ok(str);
                }
            }
        }
    }
}

fn expect_dot(chars: &mut CharIter, field: NumField) -> Result<(), Error> {
    match chars.peek_byte() {
        Some(b'.') => {
            chars.next_byte();
            Ok(())
        }
        Some(_) => {
            let char = chars.peek_char().expect("remainder shouldn't be empty");
            let offset = chars.offset();
            Err(Error::ExpectedDot(char, field, offset))
        }
        None => {
            let offset = chars.offset();
            Err(Error::MissingDot(field, offset))
        }
    }
}

fn expect_comma_or_end(chars: &mut CharIter) -> Result<Option<Offset>, Error> {
    match chars.peek_byte() {
        Some(b',') => {
            let offset = chars.offset();
            chars.next_byte();
            Ok(Some(offset))
        }
        Some(_) => {
            Err(Error::MissingComma(chars.offset()))
            // TODO: store error and continue
            // None
        }
        None => Ok(None),
    }
}

fn eat_dot(chars: &mut CharIter) -> bool {
    match chars.peek_byte() {
        Some(b'.') => {
            chars.next_byte();
            true
        }
        _ => false,
    }
}

fn eat_hyphen(chars: &mut CharIter) -> bool {
    match chars.peek_byte() {
        Some(b'-') => {
            chars.next_byte();
            true
        }
        _ => false,
    }
}

fn eat_plus(chars: &mut CharIter) -> bool {
    match chars.peek_byte() {
        Some(b'+') => {
            chars.next_byte();
            true
        }
        _ => false,
    }
}

fn eat_whitespace(chars: &mut CharIter) {
    while let Some(b' ') = chars.peek_byte() {
        chars.next_byte();
    }
}

fn eat_wildcard(chars: &mut CharIter) -> Option<WlChar> {
    let wl = match chars.peek_byte() {
        Some(b'*') => WlChar::Star,
        Some(b'x') => WlChar::LowerX,
        Some(b'X') => WlChar::UpperX,
        _ => return None,
    };
    chars.next_byte();
    Some(wl)
}
