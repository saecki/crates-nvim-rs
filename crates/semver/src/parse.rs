use common::{FmtChar, FmtStr, Pos};

use crate::inlinestr::InlineStr;
use crate::{
    BuildMetadata, CompVersion, Comparator, Error, IdentField, NumField, Offset, Op, Prerelease,
    Version, VersionReq, WlChar,
};

#[cfg(test)]
mod test;

pub(crate) const MAX_COMPARATORS: usize = 32;

struct CharIter<'a> {
    start: Pos,
    str: &'a str,
    idx: usize,
}

impl<'a> CharIter<'a> {
    fn new(start: Pos, str: &'a str) -> Self {
        Self { start, str, idx: 0 }
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
        Offset::new(self.idx as u32)
    }

    fn pos(&self) -> Pos {
        self.start.plus(self.idx as u32)
    }
}

pub fn parse_requirement(input: &str, pos: Pos) -> Result<VersionReq, Error> {
    let mut chars = CharIter::new(pos, input);
    let num_commas = input.as_bytes().iter().filter(|b| **b == b',').count();
    // If there is no comma, there are probably no comparators or one. If it's only one the push
    // won't have to reallocate anyway, but if there are 0 we saved an allocation. For 1 or more
    // commas this might allocate space for one extra item, but will never have to reallocate.
    let capacity = num_commas + (num_commas != 0) as usize;
    let capacity = capacity.max(MAX_COMPARATORS);
    let mut comparators = Vec::with_capacity(capacity);
    let mut last_comma = None;

    loop {
        eat_whitespace(&mut chars);

        let Some(b) = chars.peek_byte() else {
            break;
        };

        if comparators.len() >= MAX_COMPARATORS {
            let pos = chars.pos();
            let len = chars.remainder().len() as u32;
            return Err(Error::ExcessiveComparators(pos, len));
        }

        let op_offset = chars.offset();
        let mut op = match b {
            // Blank version requirement, equivalent to caret
            b'0'..=b'9' => Op::Bl,
            b'*' | b'x' | b'X' => {
                let wl = match b {
                    b'*' => WlChar::Star,
                    b'x' => WlChar::LowerX,
                    b'X' => WlChar::UpperX,
                    _ => unsafe { std::hint::unreachable_unchecked() },
                };

                if !comparators.is_empty() {
                    let pos = pos.plus(op_offset.char);
                    return Err(Error::WildcardNotTheSoleComparator(wl, pos));
                }

                chars.next_byte();

                match chars.peek_byte() {
                    Some(b',' | b' ') => (),
                    Some(_) => {
                        let c = FmtChar(chars.peek_char().unwrap());
                        let pos = chars.pos();
                        return Err(Error::UnexpectedAfterWildcard(c, NumField::Major, pos));
                    }
                    None => (),
                }

                eat_whitespace(&mut chars);

                if let Some(c) = chars.peek_byte() {
                    if c == b',' {
                        let pos = chars.pos();
                        chars.next_byte();
                        eat_whitespace(&mut chars);
                        if chars.peek_byte().is_none() {
                            return Err(Error::TrailingComma(pos));
                        }
                    }

                    let pos = pos.plus(op_offset.char);
                    return Err(Error::WildcardNotTheSoleComparator(wl, pos));
                }

                last_comma = expect_comma_or_end(&mut chars)?;

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
                let char = FmtChar(chars.peek_char().unwrap());
                let pos = chars.pos();
                return Err(Error::InvalidOp(char, pos));
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
        let pos = pos.plus(offset.char);
        return Err(Error::TrailingComma(pos));
    }

    if comparators.is_empty() {
        return Err(Error::EmptyVersionReq(pos));
    }

    let len = chars.str.len() as u32;
    Ok(VersionReq {
        pos,
        len,
        comparators,
    })
}

fn comp_version(chars: &mut CharIter, op: &mut Op) -> Result<CompVersion, Error> {
    let major = parse_int(chars, NumField::Major)?;
    if !eat_dot(chars) {
        return Ok(CompVersion::Major(major, None));
    }

    if let Some(wl1) = eat_wildcard(chars) {
        if *op == Op::Bl {
            *op = Op::Wl;
        }
        if !eat_dot(chars) {
            return Ok(CompVersion::Major(major, Some((wl1, None))));
        }

        if let Some(wl2) = eat_wildcard(chars) {
            return Ok(CompVersion::Major(major, Some((wl1, Some(wl2)))));
        }
        let pos = chars.pos();
        return match chars.peek_char() {
            Some(c) => Err(Error::UnexpectedAfterWildcard(
                FmtChar(c),
                NumField::Minor,
                pos,
            )),
            None => Err(Error::MissingField(NumField::Patch, pos)),
        };
    }
    let minor = parse_int(chars, NumField::Minor)?;
    if !eat_dot(chars) {
        return Ok(CompVersion::Minor(major, minor, None));
    }

    if let Some(wl) = eat_wildcard(chars) {
        if *op == Op::Bl {
            *op = Op::Wl;
        }
        return Ok(CompVersion::Minor(major, minor, Some(wl)));
    }
    let patch = parse_int(chars, NumField::Patch)?;

    let mut pre = None;
    let mut meta = None;
    if eat_hyphen(chars) {
        let ident = parse_ident(chars, IdentField::Prerelease)?;
        let str = unsafe { InlineStr::new_unchecked(ident) };
        pre = Some(Prerelease { str });
    }
    if eat_plus(chars) {
        let ident = parse_ident(chars, IdentField::BuildMetadata)?;
        let str = unsafe { InlineStr::new_unchecked(ident) };
        meta = Some(BuildMetadata { str });
    }

    match pre {
        None => Ok(CompVersion::Patch(major, minor, patch, meta)),
        Some(pre) => Ok(CompVersion::Pre(major, minor, patch, pre, meta)),
    }
}

pub fn parse_version(input: &str, pos: Pos) -> Result<Version, Error> {
    let mut chars = CharIter::new(pos, input);

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
        let trailing = FmtStr::from_str(chars.remainder().trim_end_matches(' '));
        let pos = chars.pos();
        let field = if !meta.is_empty() {
            Some(IdentField::BuildMetadata)
        } else if pre.is_empty() {
            Some(IdentField::Prerelease)
        } else {
            None
        };
        return Err(Error::TrailingCharacters(trailing, field, pos));
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
    let start_idx = chars.idx as u32;

    let Some(c) = chars.peek_byte() else {
        let pos = chars.pos();
        return Err(Error::MissingField(field, pos));
    };
    let accum = match c {
        b'0' => {
            chars.next_byte();
            return match chars.peek_byte() {
                Some(b'0'..=b'9') => {
                    let pos = chars.pos().minus(1);
                    Err(Error::LeadingZeroNum(field, pos))
                }
                _ => Ok(0),
            };
        }
        b'1'..=b'9' => {
            chars.next_byte();
            c as u32 - b'0' as u32
        }
        _ => {
            let pos = chars.pos();
            let char = FmtChar(chars.peek_char().unwrap());
            return Err(Error::InvalidIntChar(char, field, pos));
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
            let pos = chars.start.plus(start_idx);
            let len = chars.idx as u32 - start_idx;
            Err(Error::IntOverflow(field, pos, len))
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
                    let pos = chars.pos();
                    if start == chars.idx && boundary != Some(b'.') {
                        return Err(Error::EmptyIdentifier(field, pos));
                    } else {
                        return Err(Error::EmptyIdentifierSegment(field, pos));
                    }
                }

                if field == IdentField::Prerelease
                    && chars.idx - segment_start > 1
                    && !segment_has_nondigit
                    && chars.str[segment_start..].starts_with('0')
                {
                    let pos = chars.start.plus(segment_start as u32);
                    return Err(Error::LeadingZeroSegment(field, pos));
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
            let char = FmtChar(chars.peek_char().unwrap());
            let pos = chars.pos();
            Err(Error::ExpectedDot(char, field, pos))
        }
        None => {
            let pos = chars.pos();
            Err(Error::MissingDot(field, pos))
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
            Err(Error::MissingComma(chars.pos()))
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
