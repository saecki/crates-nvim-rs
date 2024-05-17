use pretty_assertions::assert_eq;

use super::*;
use crate::test::*;

fn pos(char: u32) -> Pos {
    Pos { line: 0, char }
}

#[track_caller]
fn check_version(input: &str, expected: Version) {
    let version = parse_version(input, Pos::ZERO).unwrap();
    assert_eq!(expected, version);
}

#[track_caller]
fn check_version_display(input: &str) {
    let version = parse_version(input, Pos::ZERO).unwrap();
    let display = version.to_string();
    assert_eq!(input.trim(), display);
}

#[track_caller]
fn check_version_error(input: &str, expected: Error) {
    let error = parse_version(input, Pos::ZERO).unwrap_err();
    assert_eq!(expected, error);
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
    check_version_error("", Error::MissingField(NumField::Major, pos(0)));
    check_version_error("1", Error::MissingDot(NumField::Major, pos(1)));
    check_version_error(
        "1 ",
        Error::ExpectedDot(' '.into(), NumField::Major, pos(1)),
    );
    check_version_error("1.", Error::MissingField(NumField::Minor, pos(2)));
    check_version_error("1.0", Error::MissingDot(NumField::Minor, pos(3)));
    check_version_error(
        "1.0 ",
        Error::ExpectedDot(' '.into(), NumField::Minor, pos(3)),
    );
    check_version_error("1.0.", Error::MissingField(NumField::Patch, pos(4)));
}

#[test]
fn version_invalid_num() {
    check_version_error("01.2.3", Error::LeadingZeroNum(NumField::Major, pos(0)));
    check_version_error("1.02.3", Error::LeadingZeroNum(NumField::Minor, pos(2)));
    check_version_error("1.2.03", Error::LeadingZeroNum(NumField::Patch, pos(4)));
}

#[test]
fn version_empty_prerelease() {
    check_version_error(
        "1.0.0-",
        Error::EmptyIdentifier(IdentField::Prerelease, pos(6)),
    );
    check_version_error(
        "1.0.0-+meta123",
        Error::EmptyIdentifier(IdentField::Prerelease, pos(6)),
    );
}

#[test]
fn version_empty_buildmetadata() {
    check_version_error(
        "1.0.0+",
        Error::EmptyIdentifier(IdentField::BuildMetadata, pos(6)),
    );
    check_version_error(
        "1.0.0-beta.5+",
        Error::EmptyIdentifier(IdentField::BuildMetadata, pos(13)),
    );
}

#[track_caller]
fn check_req(input: &str, expected_comparators: Vec<Comparator>) {
    let req = parse_requirement(input, Pos::ZERO).unwrap();
    let expected = VersionReq {
        pos: Pos::ZERO,
        len: input.len() as u32,
        comparators: expected_comparators,
    };
    assert_eq!(expected, req);
}

#[track_caller]
fn check_req_display(input: &str) {
    let req = parse_requirement(input, Pos::ZERO).unwrap();
    let identical = format!("{req}");
    let trimmed = format!("{req:#}");
    assert_eq!(input, identical);
    assert_eq!(input.trim(), trimmed);
}

#[track_caller]
fn check_req_error(input: &str, expected: Error) {
    let error = parse_requirement(input, Pos::ZERO).unwrap_err();
    assert_eq!(expected, error);
}

#[test]
fn req_parsing() {
    check_req(
        "*",
        vec![Comparator {
            op_offset: Offset::new(0),
            op: Op::Wl,
            version_offset: Offset::new(0),
            version: CompVersion::Wl(WlChar::Star),
            comma: None,
        }],
    );
    check_req(
        "1.*",
        vec![Comparator {
            op_offset: Offset::new(0),
            op: Op::Wl,
            version_offset: Offset::new(0),
            version: CompVersion::Major(1, Some((WlChar::Star, None))),
            comma: None,
        }],
    );
    check_req(
        "2.*.*",
        vec![Comparator {
            op_offset: Offset::new(0),
            op: Op::Wl,
            version_offset: Offset::new(0),
            version: CompVersion::Major(2, Some((WlChar::Star, Some(WlChar::Star)))),
            comma: None,
        }],
    );
    check_req(
        "7.2.*",
        vec![Comparator {
            op_offset: Offset::new(0),
            op: Op::Wl,
            version_offset: Offset::new(0),
            version: CompVersion::Minor(7, 2, Some(WlChar::Star)),
            comma: None,
        }],
    );

    check_req(
        "<0.9.*",
        vec![Comparator {
            op_offset: Offset::new(0),
            op: Op::Lt,
            version_offset: Offset::new(1),
            version: CompVersion::Minor(0, 9, Some(WlChar::Star)),
            comma: None,
        }],
    );

    check_req(
        "2.4.1",
        vec![Comparator {
            op_offset: Offset::new(0),
            op: Op::Bl,
            version_offset: Offset::new(0),
            version: CompVersion::Patch(2, 4, 1, None),
            comma: None,
        }],
    );
    check_req(
        "=0.1.0",
        vec![Comparator {
            op_offset: Offset::new(0),
            op: Op::Eq,
            version_offset: Offset::new(1),
            version: CompVersion::Patch(0, 1, 0, None),
            comma: None,
        }],
    );
    check_req(
        "^0.1.0",
        vec![Comparator {
            op_offset: Offset::new(0),
            op: Op::Cr,
            version_offset: Offset::new(1),
            version: CompVersion::Patch(0, 1, 0, None),
            comma: None,
        }],
    );

    check_req(
        "  >= 0.3.8 , < 0.4.2  ",
        vec![
            Comparator {
                op_offset: Offset::new(2),
                op: Op::Ge,
                version_offset: Offset::new(5),
                version: CompVersion::Patch(0, 3, 8, None),
                comma: Some(Offset::new(11)),
            },
            Comparator {
                op_offset: Offset::new(13),
                op: Op::Lt,
                version_offset: Offset::new(15),
                version: CompVersion::Patch(0, 4, 2, None),
                comma: None,
            },
        ],
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

#[test]
fn req_wildcard_invalid_after_operator() {
    let ops = ["=", "<", "<=", ">", ">=", "^", "~"];
    for op in ops {
        let req = format!("{op}*");
        let offset = pos(op.len() as u32);
        check_req_error(
            &req,
            Error::InvalidIntChar('*'.into(), NumField::Major, offset),
        );
    }
}

#[test]
fn req_trailing_comma() {
    check_req_error("> 0.1.0,", Error::TrailingComma(pos(7)));
    check_req_error("> 0.3.0, ,", Error::InvalidOp(','.into(), pos(9)));
}

#[test]
fn req_invalid_separator() {
    check_req_error("1.2.3 - 2.3.4", Error::MissingComma(pos(6)));
}

#[test]
fn req_excessive_comparators() {
    check_req_error(
        ">1, >2, >3, >4, >5, >6, >7, >8, >9, >10, >11, >12, >13, >14, >15, >16, >17, >18, >19, >20, >21, >22, >23, >24, >25, >26, >27, >28, >29, >30, >31, >32, >33",
        Error::ExcessiveComparators(pos(151), 3),
    );
}

#[test]
fn req_whitespace_delimited_comparator_sets() {
    check_req_error("> 0.0.9 <= 2.5.3", Error::MissingComma(pos(8)));
}

#[test]
fn req_empty() {
    check_req_error("", Error::EmptyVersionReq(pos(0)));
}

#[test]
fn req_invalid_logical_or_separator() {
    check_req_error("=1.2.3 || =2.3.4", Error::MissingComma(pos(7)));
    check_req_error("1.1 || =1.2.3", Error::MissingComma(pos(4)));
    check_req_error("6.* || 8.* || >= 10.*", Error::MissingComma(pos(4)));
}

#[test]
fn req_invalid_char() {
    check_req_error("\0", Error::InvalidOp('\0'.into(), pos(0)));
}

#[test]
fn req_duplicate_operator() {
    check_req_error(
        ">= >= 0.0.2",
        Error::InvalidIntChar('>'.into(), NumField::Major, pos(3)),
    );
}

#[test]
fn req_too_long_operator() {
    check_req_error(
        ">== 0.0.2",
        Error::InvalidIntChar('='.into(), NumField::Major, pos(2)),
    );
}

#[test]
fn req_non_numeric_major_version() {
    check_req_error("a.0.0", Error::InvalidOp('a'.into(), pos(0)));
}

#[test]
fn req_empty_prerelease() {
    check_req_error(
        "1.0.0-",
        Error::EmptyIdentifier(IdentField::Prerelease, pos(6)),
    );
    check_req_error(
        "1.0.0- ",
        Error::EmptyIdentifier(IdentField::Prerelease, pos(6)),
    );
}

#[test]
fn req_missing_version() {
    check_req_error(">=", Error::MissingField(NumField::Major, pos(2)));
    check_req_error(">= ", Error::MissingField(NumField::Major, pos(3)));
}

#[test]
fn req_prerelease_leading_zero() {
    check_req_error(
        "1.2.3-01",
        Error::LeadingZeroSegment(IdentField::Prerelease, pos(6)),
    );
}

#[test]
fn req_empty_identifier_segment() {
    check_req_error(
        "1.2.3+4.",
        Error::EmptyIdentifierSegment(IdentField::BuildMetadata, pos(8)),
    );
}

#[test]
fn req_missing_minor() {
    check_req_error("1.", Error::MissingField(NumField::Minor, pos(2)));
}

#[test]
fn req_missing_patch_after_wildcard() {
    check_req_error("1.*.", Error::MissingField(NumField::Patch, pos(4)));
}

#[test]
fn req_invalid_char_in_buildmetadata() {
    check_req_error("1.2.3+4ÿ", Error::MissingComma(pos(7)));
}

#[test]
fn req_digit_after_wildcard() {
    check_req_error(
        "*.1",
        Error::UnexpectedAfterWildcard('.'.into(), NumField::Major, pos(1)),
    );
    check_req_error(
        "1.*.1",
        Error::UnexpectedAfterWildcard('1'.into(), NumField::Minor, pos(4)),
    );
    check_req_error(
        ">=1.*.1",
        Error::UnexpectedAfterWildcard('1'.into(), NumField::Minor, pos(6)),
    );
}

#[test]
fn req_wildcard_and_another() {
    check_req_error(
        "*, 0.20.0-any",
        Error::WildcardNotTheSoleComparator(WlChar::Star, pos(0)),
    );
    check_req_error(
        "0.20.0-any, *",
        Error::WildcardNotTheSoleComparator(WlChar::Star, pos(12)),
    );
    check_req_error(
        "0.20.0-any, *, 1.0",
        Error::WildcardNotTheSoleComparator(WlChar::Star, pos(12)),
    );
}

#[test]
fn req_leading_digit_in_pre_and_build() {
    for op in &["=", ">", ">=", "<", "<=", "~", "^"] {
        // digit then alpha
        req(&format!("{} 1.2.3-1a", op));
        req(&format!("{} 1.2.3+1a", op));

        // digit then alpha (leading zero)
        req(&format!("{} 1.2.3-01a", op));
        req(&format!("{} 1.2.3+01", op));

        // multiple
        req(&format!("{} 1.2.3-1+1", op));
        req(&format!("{} 1.2.3-1-1+1-1-1", op));
        req(&format!("{} 1.2.3-1a+1a", op));
        req(&format!("{} 1.2.3-1a-1a+1a-1a-1a", op));
    }
}
