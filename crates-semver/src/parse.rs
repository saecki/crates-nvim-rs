use crate::{BuildMetadata, Error, Prerelease, Version, VersionReq};

pub fn parse_requirement(input: &str) -> VersionReq {
    todo!()
}

pub fn parse_version(input: &str) -> Result<Version, Error> {
    let mut chars = input.char_indices().peekable();

    let major = parse_int(&mut chars)?;
    expect_dot(&mut chars)?;
    let minor = parse_int(&mut chars)?;
    expect_dot(&mut chars)?;
    let patch = parse_int(&mut chars)?;

    let pre = if hyphen(&mut chars) {
        todo!("pre-release");
    } else {
        Prerelease::EMPTY
    };

    let meta = if plus(&mut chars) {
        todo!("build metadata");
    } else {
        BuildMetadata::EMPTY
    };

    if let Some((i, c)) = chars.next() {
        todo!("error {i} {c}")
    }

    Ok(Version {
        major,
        minor,
        patch,
        pre,
        meta,
    })
}

type CharIter<'a> = std::iter::Peekable<std::str::CharIndices<'a>>;

fn parse_int(chars: &mut CharIter) -> Result<u32, Error> {
    let Some(&(i, c)) = chars.peek() else {
        todo!("error");
    };
    let mut accum = match c {
        '0' => todo!("error"),
        '1'..='9' => {
            chars.next();
            c as u32 - '0' as u32
        }
        _ => todo!("error {i}"),
    };

    while let Some(&(i, c)) = chars.peek() {
        let digit = match c {
            '0'..='9' => {
                chars.next();
                c as u32 - '0' as u32
            }
            _ => break,
        };

        let Some(val) = accum.checked_mul(10) else {
            todo!("overflow error {i}")
        };

        accum = val + digit
    }

    Ok(accum)
}

fn expect_dot(chars: &mut CharIter) -> Result<(), Error> {
    match chars.peek() {
        Some((_, '.')) => {
            chars.next();
            Ok(())
        }
        Some((i, c)) => todo!("error {i} {c}"),
        None => todo!("error"),
    }
}

fn hyphen(chars: &mut CharIter) -> bool {
    match chars.peek() {
        Some((_, '-')) => {
            chars.next();
            true
        }
        _ => false,
    }
}

fn plus(chars: &mut CharIter) -> bool {
    match chars.peek() {
        Some((_, '+')) => {
            chars.next();
            true
        }
        _ => false,
    }
}
