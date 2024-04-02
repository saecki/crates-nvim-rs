use pretty_assertions::assert_eq;

use crate::inlinestr::{InlineStr, PTR_SIZE};

#[test]
fn empty_str() {
    let str = unsafe { InlineStr::new_unchecked("") };
    assert!(str.is_empty());
    assert_eq!(str.as_str(), "");
}

#[test]
fn inline_str() {
    let mut string = String::new();
    for _ in 0..PTR_SIZE {
        string.push('a');
        let str = unsafe { InlineStr::new_unchecked(&string) };
        assert!(!str.is_empty());
        assert!(str.is_inline());
        assert_eq!(str.as_str(), string);
    }
}

#[test]
fn allocated_str() {
    let mut string = "a".repeat(PTR_SIZE);
    for _ in 0..300 {
        string.push('a');
        let str = unsafe { InlineStr::new_unchecked(&string) };
        assert!(!str.is_empty());
        assert!(!str.is_inline());
        assert_eq!(str.as_str(), string);
    }
}
