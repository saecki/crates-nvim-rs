use pretty_assertions::assert_eq;

use super::*;

#[test]
fn remove_last_line_without_newline() {
    let doc = "abc\ndef";
    let changes = vec![lsp_types::TextDocumentContentChangeEvent {
        range: Some(lsp_types::Range {
            start: lsp_types::Position::new(1, 0),
            end: lsp_types::Position::new(2, 0),
        }),
        range_length: None,
        text: "".into(),
    }];
    let doc = apply_document_changes(doc, changes, OffsetEncoding::Utf8).unwrap();
    assert_eq!("abc\n", doc);
}

#[test]
fn compute_encoded_offsets() {
    #[track_caller]
    fn check(str: &str, utf8: u32, utf16: u32, utf32: u32) {
        assert_eq!(
            encoded_offset(str, OffsetEncoding::Utf8),
            utf8,
            "utf8 encoding"
        );
        assert_eq!(
            encoded_offset(str, OffsetEncoding::Utf16),
            utf16,
            "utf16 encoding"
        );
        assert_eq!(
            encoded_offset(str, OffsetEncoding::Utf32),
            utf32,
            "utf32 encoding"
        );

        assert_eq!(
            utf8_offset(str, utf8, OffsetEncoding::Utf8).expect("utf8 decoding"),
            str.len(),
            "utf8 decoding"
        );
        assert_eq!(
            utf8_offset(str, utf16, OffsetEncoding::Utf16).expect("utf16 decoding"),
            str.len(),
            "utf16 decoding"
        );
        assert_eq!(
            utf8_offset(str, utf32, OffsetEncoding::Utf32).expect("utf32 decoding"),
            str.len(),
            "utf32 decoding"
        );
    }

    check("abc", 3, 3, 3);
    check("äd", 3, 2, 2);
    check("\u{2049}", 3, 1, 1);
    check("\u{1F642}", 4, 2, 1);
}

#[test]
fn remove_blank_lines_utf16() {
    let doc = "statement;\n\n\n";
    let changes = vec![
        lsp_types::TextDocumentContentChangeEvent {
            range: Some(lsp_types::Range {
                start: lsp_types::Position {
                    line: 1,
                    character: 0,
                },
                end: lsp_types::Position {
                    line: 2,
                    character: 0,
                },
            }),
            range_length: Some(1),
            text: "".into(),
        },
        lsp_types::TextDocumentContentChangeEvent {
            range: Some(lsp_types::Range {
                start: lsp_types::Position {
                    line: 1,
                    character: 0,
                },
                end: lsp_types::Position {
                    line: 2,
                    character: 0,
                },
            }),
            range_length: Some(1),
            text: "".into(),
        },
    ];
    let doc = apply_document_changes(&doc, changes, OffsetEncoding::Utf16).unwrap();
    assert_eq!("statement;\n", doc);
}
