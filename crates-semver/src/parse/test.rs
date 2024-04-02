use pretty_assertions::assert_eq;

use super::*;

fn check(input: &str, expected: Version) {
    let version = parse_version(input).unwrap();
    assert_eq!(version, expected);
}

fn check_error(input: &str, expected: Error) {
    let version = parse_version(input).unwrap_err();
    assert_eq!(version, expected);
}

macro_rules! v {
    ($major:literal,$minor:literal,$patch:literal) => {
        Version {
            major: $major,
            minor: $minor,
            patch: $patch,
            pre: Prerelease::EMPTY,
            meta: BuildMetadata::EMPTY,
        }
    };
    ($major:literal,$minor:literal,$patch:literal-$pre:literal) => {
        Version {
            major: $major,
            minor: $minor,
            patch: $patch,
            pre: Prerelease {
                str: unsafe { InlineStr::new_unchecked($pre) },
            },
            meta: BuildMetadata::EMPTY,
        }
    };
    ($major:literal,$minor:literal,$patch:literal+$meta:literal) => {
        Version {
            major: $major,
            minor: $minor,
            patch: $patch,
            pre: Prerelease::EMPTY,
            meta: BuildMetadata {
                str: unsafe { InlineStr::new_unchecked($meta) },
            },
        }
    };
    ($major:literal,$minor:literal,$patch:literal-$pre:literal+$meta:literal) => {
        Version {
            major: $major,
            minor: $minor,
            patch: $patch,
            pre: Prerelease {
                str: unsafe { InlineStr::new_unchecked($pre) },
            },
            meta: BuildMetadata {
                str: unsafe { InlineStr::new_unchecked($meta) },
            },
        }
    };
}

#[test]
fn version_parsing() {
    check("0.1.2", v!(0, 1, 2));

    check("0.1.2-alpha.1", v!(0, 1, 2 - "alpha.1"));
    check("2.0.8-beta.0", v!(2, 0, 8 - "beta.0"));

    check("1.0.0+sdfsd-32423", v!(1, 0, 0 + "sdfsd-32423"));
    check("0.8.3+32432.dsfds", v!(0, 8, 3 + "32432.dsfds"));

    check(
        "1.0.0-alpha.7+sdfsd-32423",
        v!(1, 0, 0 - "alpha.7" + "sdfsd-32423"),
    );
    check(
        "0.8.3-pre.48+32432.dsfds",
        v!(0, 8, 3 - "pre.48" + "32432.dsfds"),
    );
}

#[test]
fn version_missing_num() {
    check_error("", Error::MissingField(NumField::Major, Offset::new(0)));
    check_error("1", Error::MissingDot(NumField::Major, Offset::new(1)));
    check_error("1 ", Error::ExpectedDot(' ', NumField::Major, Offset::new(1)));
    check_error("1.", Error::MissingField(NumField::Minor, Offset::new(2)));
    check_error("1.0", Error::MissingDot(NumField::Minor, Offset::new(3)));
    check_error("1.0 ", Error::ExpectedDot(' ', NumField::Minor, Offset::new(3)));
    check_error("1.0.", Error::MissingField(NumField::Patch, Offset::new(4)));
}

#[test]
fn version_invalid_num() {
    check_error(
        "01.2.3",
        Error::LeadingZeroNum(NumField::Major, Offset::new(0)),
    );
    check_error(
        "1.02.3",
        Error::LeadingZeroNum(NumField::Minor, Offset::new(2)),
    );
    check_error(
        "1.2.03",
        Error::LeadingZeroNum(NumField::Patch, Offset::new(4)),
    );
}

#[test]
fn version_empty_prerelease() {
    check_error(
        "1.0.0-",
        Error::EmptyIdentifier(IdentField::Prerelease, Offset::new(6)),
    );
    check_error(
        "1.0.0-+meta123",
        Error::EmptyIdentifier(IdentField::Prerelease, Offset::new(6)),
    );
}

#[test]
fn version_empty_buildmetadata() {
    check_error(
        "1.0.0+",
        Error::EmptyIdentifier(IdentField::BuildMetadata, Offset::new(6)),
    );
    check_error(
        "1.0.0-beta.5+",
        Error::EmptyIdentifier(IdentField::BuildMetadata, Offset::new(13)),
    );
}
