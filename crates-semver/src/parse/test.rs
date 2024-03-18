use super::*;

fn check(input: &str, expected: Version) {
    let version = parse_version(input).unwrap();
    assert_eq!(version, expected);
}

#[test]
fn simple_version() {
    check(
        "0.1.2",
        Version {
            major: 0,
            minor: 1,
            patch: 2,
            pre: Prerelease::EMPTY,
            meta: BuildMetadata::EMPTY,
        },
    )
}
