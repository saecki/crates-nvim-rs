use common::FmtStr;

use crate::inlinestr::InlineStr;
use crate::{
    BuildMetadata, Comparator, Error, IdentField, NumField, Offset, Prerelease, Version, VersionReq,
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
}

pub fn parse_requirement(input: &str) -> Result<VersionReq, Error> {
    let mut chars = CharIter::new(input);
    let mut comparators = Vec::new();

    loop {
        eat_whitespace(&mut chars);

        let op = todo!("parse op");

        let major = parse_int(&mut chars, NumField::Major)?;
        let mut minor = None;
        let mut patch = None;

        if eat_dot(&mut chars) {
            let num = parse_int(&mut chars, NumField::Minor)?;
            minor = Some(num);

            if eat_dot(&mut chars) {
                let num = parse_int(&mut chars, NumField::Patch)?;
                patch = Some(num);
            }
        }

        let pre = if eat_hyphen(&mut chars) {
            let ident = parse_ident(&mut chars, IdentField::Prerelease)?;
            let str = unsafe { InlineStr::new_unchecked(ident) };
            Prerelease { str }
        } else {
            Prerelease::EMPTY
        };

        // TODO: eat comma

        let comparator = Comparator {
            op,
            major: Some(major),
            minor,
            patch,
            pre,
        };
        comparators.push(comparator);
    }

    Ok(VersionReq { comparators })
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
        let offset = Offset::new(chars.idx as u32);
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
    let start = chars.idx as u32;

    let Some(c) = chars.peek_byte() else {
        let offset = Offset::new(chars.str.len() as u32);
        return Err(Error::MissingField(field, offset));
    };
    let accum = match c {
        b'0' => {
            chars.next_byte();
            return match chars.peek_byte() {
                Some(b'0'..=b'9') => {
                    let i = chars.idx - 1;
                    let offset = Offset::new(i as u32);
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
            let offset = Offset::new(chars.idx as u32);
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
            let offset = Offset::new(start);
            let len = chars.idx as u32 - start;
            Err(Error::IntOverflow(field, offset, len))
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
                    if start == chars.idx && boundary != Some(b'.') {
                        let offset = Offset::new(chars.idx as u32);
                        return Err(Error::EmptyIdentifier(field, offset));
                    } else {
                        let offset = Offset::new(chars.idx as u32);
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
            let offset = Offset::new(chars.idx as u32);
            Err(Error::ExpectedDot(char, field, offset))
        }
        None => {
            let offset = Offset::new(chars.idx as u32);
            Err(Error::MissingDot(field, offset))
        }
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
