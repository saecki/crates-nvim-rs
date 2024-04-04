use super::*;

#[test]
fn fuzz1() {
    // [48, 57, 61, 91, 61, 96, 61, 91, 46, 44, 0, 0]
    _ = parse_simple("09=[=`=[.,\0\0");
}

#[test]
fn fuzz2() {
    // [43, 105, 110, 102, 122, 61, 91, 0, 91, 91, 35, 0, 93, 61, 91]
    _ = parse_simple("+infz=[\0[[#\0]=[");
}

#[test]
fn fuzz3() {
    // [0, 0, 61, 123, 4, 34, 34, 44, 34, 34, 34]
    _ = parse_simple("\0\0={\u{4}\"\",\"\"\"");
}
