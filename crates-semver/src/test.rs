use common::Pos;

use crate::{parse_requirement, parse_version, Version, VersionReq};

#[track_caller]
pub fn version(str: &str) -> Version {
    parse_version(str, Pos::ZERO).unwrap()
}

#[track_caller]
pub fn req(str: &str) -> VersionReq {
    parse_requirement(str, Pos::ZERO).unwrap()
}

#[track_caller]
fn assert_match_all(req: &VersionReq, versions: &[&str]) {
    for string in versions {
        let parsed = version(string);
        assert!(req.matches(&parsed), "did not match {}", string);
    }
}

#[track_caller]
fn assert_match_none(req: &VersionReq, versions: &[&str]) {
    for string in versions {
        let parsed = version(string);
        assert!(!req.matches(&parsed), "matched {}", string);
    }
}

#[test]
fn basic() {
    let ref r = req("1.0.0");
    assert_match_all(r, &["1.0.0", "1.1.0", "1.0.1"]);
    assert_match_none(r, &["0.9.9", "0.10.0", "0.1.0", "1.0.0-pre", "1.0.1-pre"]);
}

#[test]
fn exact() {
    let ref r = req("=1.0.0");
    assert_match_all(r, &["1.0.0"]);
    assert_match_none(r, &["1.0.1", "0.9.9", "0.10.0", "0.1.0", "1.0.0-pre"]);

    let ref r = req("=0.9.0");
    assert_match_all(r, &["0.9.0"]);
    assert_match_none(r, &["0.9.1", "1.9.0", "0.0.9", "0.9.0-pre"]);

    let ref r = req("=0.0.2");
    assert_match_all(r, &["0.0.2"]);
    assert_match_none(r, &["0.0.1", "0.0.3", "0.0.2-pre"]);

    let ref r = req("=0.1.0-beta2.a");
    assert_match_all(r, &["0.1.0-beta2.a"]);
    assert_match_none(r, &["0.9.1", "0.1.0", "0.1.1-beta2.a", "0.1.0-beta2"]);

    let ref r = req("=0.1.0+meta");
    assert_match_all(r, &["0.1.0", "0.1.0+meta", "0.1.0+any"]);
}

#[test]
fn greater_less() {
    let ref r = req(">= 1.0.0");
    assert_match_all(r, &["1.0.0", "2.0.0"]);
    assert_match_none(r, &["0.1.0", "0.0.1", "1.0.0-pre", "2.0.0-pre"]);

    let ref r = req(">= 2.1.0-alpha2");
    assert_match_all(r, &["2.1.0-alpha2", "2.1.0-alpha3", "2.1.0", "3.0.0"]);
    assert_match_none(
        r,
        &["2.0.0", "2.1.0-alpha1", "2.0.0-alpha2", "3.0.0-alpha2"],
    );

    let ref r = req("< 1.0.0");
    assert_match_all(r, &["0.1.0", "0.0.1"]);
    assert_match_none(r, &["1.0.0", "1.0.0-beta", "1.0.1", "0.9.9-alpha"]);

    let ref r = req("<= 2.1.0-alpha2");
    assert_match_all(r, &["2.1.0-alpha2", "2.1.0-alpha1", "2.0.0", "1.0.0"]);
    assert_match_none(
        r,
        &["2.1.0", "2.2.0-alpha1", "2.0.0-alpha2", "1.0.0-alpha2"],
    );

    let ref r = req(">1.0.0-alpha, <1.0.0");
    assert_match_all(r, &["1.0.0-beta"]);

    let ref r = req(">1.0.0-alpha, <1.0");
    assert_match_none(r, &["1.0.0-beta"]);

    let ref r = req(">1.0.0-alpha, <1");
    assert_match_none(r, &["1.0.0-beta"]);

    let ref r = req("> 0.0.9, <= 2.5.3");
    assert_match_all(r, &["0.0.10", "1.0.0", "2.5.3"]);
    assert_match_none(r, &["0.0.8", "2.5.4"]);

    let ref r = req("0.3.0, 0.4.0");
    assert_match_none(r, &["0.0.8", "0.3.0", "0.4.0"]);

    let ref r = req("<= 0.2.0, >= 0.5.0");
    assert_match_none(r, &["0.0.8", "0.3.0", "0.5.1"]);

    let ref r = req("0.1.0, 0.1.4, 0.1.6");
    assert_match_all(r, &["0.1.6", "0.1.9"]);
    assert_match_none(r, &["0.1.0", "0.1.4", "0.2.0"]);

    let ref r = req(">=0.5.1-alpha3, <0.6");
    assert_match_all(
        r,
        &[
            "0.5.1-alpha3",
            "0.5.1-alpha4",
            "0.5.1-beta",
            "0.5.1",
            "0.5.5",
        ],
    );
    assert_match_none(
        r,
        &["0.5.1-alpha1", "0.5.2-alpha3", "0.5.5-pre", "0.5.0-pre"],
    );
    assert_match_none(r, &["0.6.0", "0.6.0-pre"]);
}

#[test]
fn tilde() {
    let ref r = req("~1");
    assert_match_all(r, &["1.0.0", "1.0.1", "1.1.1"]);
    assert_match_none(r, &["0.9.1", "2.9.0", "0.0.9"]);

    let ref r = req("~1.2");
    assert_match_all(r, &["1.2.0", "1.2.1"]);
    assert_match_none(r, &["1.1.1", "1.3.0", "0.0.9"]);

    let ref r = req("~1.2.2");
    assert_match_all(r, &["1.2.2", "1.2.4"]);
    assert_match_none(r, &["1.2.1", "1.9.0", "1.0.9", "2.0.1", "0.1.3"]);

    let ref r = req("~1.2.3-beta.2");
    assert_match_all(r, &["1.2.3", "1.2.4", "1.2.3-beta.2", "1.2.3-beta.4"]);
    assert_match_none(r, &["1.3.3", "1.1.4", "1.2.3-beta.1", "1.2.4-beta.2"]);
}

#[test]
fn caret() {
    let ref r = req("^1");
    assert_match_all(r, &["1.1.2", "1.1.0", "1.2.1", "1.0.1"]);
    assert_match_none(r, &["0.9.1", "2.9.0", "0.1.4"]);
    assert_match_none(r, &["1.0.0-beta1", "0.1.0-alpha", "1.0.1-pre"]);

    let ref r = req("^1.1");
    assert_match_all(r, &["1.1.2", "1.1.0", "1.2.1"]);
    assert_match_none(r, &["0.9.1", "2.9.0", "1.0.1", "0.1.4"]);

    let ref r = req("^1.1.2");
    assert_match_all(r, &["1.1.2", "1.1.4", "1.2.1"]);
    assert_match_none(r, &["0.9.1", "2.9.0", "1.1.1", "0.0.1"]);
    assert_match_none(r, &["1.1.2-alpha1", "1.1.3-alpha1", "2.9.0-alpha1"]);

    let ref r = req("^0.1.2");
    assert_match_all(r, &["0.1.2", "0.1.4"]);
    assert_match_none(r, &["0.9.1", "2.9.0", "1.1.1", "0.0.1"]);
    assert_match_none(r, &["0.1.2-beta", "0.1.3-alpha", "0.2.0-pre"]);

    let ref r = req("^0.5.1-alpha3");
    assert_match_all(
        r,
        &[
            "0.5.1-alpha3",
            "0.5.1-alpha4",
            "0.5.1-beta",
            "0.5.1",
            "0.5.5",
        ],
    );
    assert_match_none(
        r,
        &[
            "0.5.1-alpha1",
            "0.5.2-alpha3",
            "0.5.5-pre",
            "0.5.0-pre",
            "0.6.0",
        ],
    );

    let ref r = req("^0.0.2");
    assert_match_all(r, &["0.0.2"]);
    assert_match_none(r, &["0.9.1", "2.9.0", "1.1.1", "0.0.1", "0.1.4"]);

    let ref r = req("^0.0");
    assert_match_all(r, &["0.0.2", "0.0.0"]);
    assert_match_none(r, &["0.9.1", "2.9.0", "1.1.1", "0.1.4"]);

    let ref r = req("^0");
    assert_match_all(r, &["0.9.1", "0.0.2", "0.0.0"]);
    assert_match_none(r, &["2.9.0", "1.1.1"]);

    let ref r = req("^1.4.2-beta.5");
    assert_match_all(
        r,
        &["1.4.2", "1.4.3", "1.4.2-beta.5", "1.4.2-beta.6", "1.4.2-c"],
    );
    assert_match_none(
        r,
        &[
            "0.9.9",
            "2.0.0",
            "1.4.2-alpha",
            "1.4.2-beta.4",
            "1.4.3-beta.5",
        ],
    );
}

#[test]
fn wildcard() {
    for s in ["*", "x", "X"] {
        let ref r = req(s);
        assert_match_all(r, &["0.9.1", "2.9.0", "0.0.9", "1.0.1", "1.1.1"]);
        assert_match_none(r, &["1.0.0-pre"]);
    }

    for s in &["1.*", "1.x", "1.X", "1.*.*"] {
        let ref r = req(s);
        assert_match_all(r, &["1.2.0", "1.2.1", "1.1.1", "1.3.0"]);
        assert_match_none(r, &["0.0.9", "1.2.0-pre"]);
    }

    for s in ["1.2.*", "1.2.x", "1.2.X"] {
        let ref r = req(s);
        assert_match_all(r, &["1.2.0", "1.2.2", "1.2.4"]);
        assert_match_none(r, &["1.9.0", "1.0.9", "2.0.1", "0.1.3", "1.2.2-pre"]);
    }
}

#[test]
fn any() {
    // TODO
    //let ref r = VersionReq::STAR;
    //assert_match_all(r, &["0.0.1", "0.1.0", "1.0.0"]);
}

#[test]
fn pre() {
    let ref r = req("=2.1.1-really.0");
    assert_match_all(r, &["2.1.1-really.0"]);
}
