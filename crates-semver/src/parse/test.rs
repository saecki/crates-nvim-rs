use pretty_assertions::assert_eq;

use super::*;

fn check_version(input: &str, expected: Version) {
    let version = parse_version(input).unwrap();
    assert_eq!(version, expected);
}

fn check_version_display(input: &str) {
    let version = parse_version(input).unwrap();
    let display = version.to_string();
    assert_eq!(input.trim(), display);
}

fn check_version_error(input: &str, expected: Error) {
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
    check_version("0.1.2", v!(0, 1, 2));

    check_version("0.1.2-alpha.1", v!(0, 1, 2 - "alpha.1"));
    check_version("2.0.8-beta.0", v!(2, 0, 8 - "beta.0"));

    check_version("1.0.0+sdfsd-32423", v!(1, 0, 0 + "sdfsd-32423"));
    check_version("0.8.3+32432.dsfds", v!(0, 8, 3 + "32432.dsfds"));

    check_version(
        "1.0.0-alpha.7+sdfsd-32423",
        v!(1, 0, 0 - "alpha.7" + "sdfsd-32423"),
    );
    check_version(
        "0.8.3-pre.48+32432.dsfds",
        v!(0, 8, 3 - "pre.48" + "32432.dsfds"),
    );
}

#[test]
fn version_display() {
    check_version_display("0.1.2");

    check_version_display("0.1.2-alpha.1");
    check_version_display("2.0.8-beta.0");

    check_version_display("1.0.0+sdfsd-32423");
    check_version_display("0.8.3+32432.dsfds");

    check_version_display("1.0.0-alpha.7+sdfsd-32423");
    check_version_display("0.8.3-pre.48+32432.dsfds");

    check_version_display("    0.8.3-pre.48+32432.dsfds    ");
}

#[test]
fn version_missing_num() {
    check_version_error("", Error::MissingField(NumField::Major, Offset::new(0)));
    check_version_error("1", Error::MissingDot(NumField::Major, Offset::new(1)));
    check_version_error(
        "1 ",
        Error::ExpectedDot(' ', NumField::Major, Offset::new(1)),
    );
    check_version_error("1.", Error::MissingField(NumField::Minor, Offset::new(2)));
    check_version_error("1.0", Error::MissingDot(NumField::Minor, Offset::new(3)));
    check_version_error(
        "1.0 ",
        Error::ExpectedDot(' ', NumField::Minor, Offset::new(3)),
    );
    check_version_error("1.0.", Error::MissingField(NumField::Patch, Offset::new(4)));
}

#[test]
fn version_invalid_num() {
    check_version_error(
        "01.2.3",
        Error::LeadingZeroNum(NumField::Major, Offset::new(0)),
    );
    check_version_error(
        "1.02.3",
        Error::LeadingZeroNum(NumField::Minor, Offset::new(2)),
    );
    check_version_error(
        "1.2.03",
        Error::LeadingZeroNum(NumField::Patch, Offset::new(4)),
    );
}

#[test]
fn version_empty_prerelease() {
    check_version_error(
        "1.0.0-",
        Error::EmptyIdentifier(IdentField::Prerelease, Offset::new(6)),
    );
    check_version_error(
        "1.0.0-+meta123",
        Error::EmptyIdentifier(IdentField::Prerelease, Offset::new(6)),
    );
}

#[test]
fn version_empty_buildmetadata() {
    check_version_error(
        "1.0.0+",
        Error::EmptyIdentifier(IdentField::BuildMetadata, Offset::new(6)),
    );
    check_version_error(
        "1.0.0-beta.5+",
        Error::EmptyIdentifier(IdentField::BuildMetadata, Offset::new(13)),
    );
}

fn check_req(input: &str, expected: VersionReq) {
    let req = parse_requirement(input).unwrap();
    assert_eq!(req, expected);
}

fn check_req_display(input: &str) {
    let req = parse_requirement(input).unwrap();
    let display = req.to_string();
    assert_eq!(input.trim(), display);
}

#[test]
fn req_parsing() {
    check_req("", VersionReq::EMPTY);

    check_req(
        "*",
        VersionReq::new(vec![Comparator {
            op_offset: Offset::new(0),
            op: Op::Wl,
            version_offset: Offset::new(0),
            version: CompVersion::Wl(WlChar::Star),
            comma: None,
        }]),
    );
    check_req(
        "1.*",
        VersionReq::new(vec![Comparator {
            op_offset: Offset::new(0),
            op: Op::Wl,
            version_offset: Offset::new(0),
            version: CompVersion::MajorWl(1, WlChar::Star),
            comma: None,
        }]),
    );
    check_req(
        "7.2.*",
        VersionReq::new(vec![Comparator {
            op_offset: Offset::new(0),
            op: Op::Wl,
            version_offset: Offset::new(0),
            version: CompVersion::MinorWl(7, 2, WlChar::Star),
            comma: None,
        }]),
    );

    check_req(
        "<0.9.*",
        VersionReq::new(vec![Comparator {
            op_offset: Offset::new(0),
            op: Op::Lt,
            version_offset: Offset::new(1),
            version: CompVersion::MinorWl(0, 9, WlChar::Star),
            comma: None,
        }]),
    );

    check_req(
        "2.4.1",
        VersionReq::new(vec![Comparator {
            op_offset: Offset::new(0),
            op: Op::Bl,
            version_offset: Offset::new(0),
            version: CompVersion::Patch(2, 4, 1),
            comma: None,
        }]),
    );
    check_req(
        "=0.1.0",
        VersionReq::new(vec![Comparator {
            op_offset: Offset::new(0),
            op: Op::Eq,
            version_offset: Offset::new(1),
            version: CompVersion::Patch(0, 1, 0),
            comma: None,
        }]),
    );
    check_req(
        "^0.1.0",
        VersionReq::new(vec![Comparator {
            op_offset: Offset::new(0),
            op: Op::Cr,
            version_offset: Offset::new(1),
            version: CompVersion::Patch(0, 1, 0),
            comma: None,
        }]),
    );

    check_req(
        "  >= 0.3.8 , < 0.4.2  ",
        VersionReq::new(vec![
            Comparator {
                op_offset: Offset::new(2),
                op: Op::Ge,
                version_offset: Offset::new(5),
                version: CompVersion::Patch(0, 3, 8),
                comma: Some(Offset::new(11)),
            },
            Comparator {
                op_offset: Offset::new(13),
                op: Op::Lt,
                version_offset: Offset::new(15),
                version: CompVersion::Patch(0, 4, 2),
                comma: None,
            },
        ]),
    );
}

#[test]
fn req_display() {
    check_req_display("1");
    check_req_display("1.*");
    check_req_display("1.0");
    check_req_display("1.0.*");
    check_req_display("1.0.0");
    check_req_display("1.0.0-alpha.1");

    check_req_display("   > 1.* ,    < 5.2.4-alpha.2   ");
}
