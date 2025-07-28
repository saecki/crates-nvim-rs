use common::{Pos, Source, Span};

#[cfg(test)]
mod test;

pub trait SpanExt {
    fn to_lsp_range(&self, source: &Source<'_>, encoding: OffsetEncoding) -> lsp_types::Range;
}

impl SpanExt for Span {
    fn to_lsp_range(&self, source: &Source<'_>, encoding: OffsetEncoding) -> lsp_types::Range {
        lsp_types::Range {
            start: self.start.to_lsp_pos(source, encoding),
            end: self.end.to_lsp_pos(source, encoding),
        }
    }
}

pub trait LocationExt {
    fn to_lsp_pos(&self, text: &Source<'_>, encoding: OffsetEncoding) -> lsp_types::Position;
}

impl LocationExt for Pos {
    fn to_lsp_pos(&self, source: &Source<'_>, encoding: OffsetEncoding) -> lsp_types::Position {
        let line_text = source.line_str(self.line);
        let character = encoded_offset(line_text, encoding);
        lsp_types::Position::new(self.line, character)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OffsetEncoding {
    Utf8,
    /// The default and required encoding by the lsp spec.
    Utf16,
    Utf32,
}

impl OffsetEncoding {
    pub fn position_encoding(&self) -> lsp_types::PositionEncodingKind {
        match self {
            OffsetEncoding::Utf8 => lsp_types::PositionEncodingKind::UTF8,
            OffsetEncoding::Utf16 => lsp_types::PositionEncodingKind::UTF16,
            OffsetEncoding::Utf32 => lsp_types::PositionEncodingKind::UTF32,
        }
    }
}

pub fn negotiated_encoding(capabilities: &lsp_types::ClientCapabilities) -> OffsetEncoding {
    let mut negotiated_encoding = OffsetEncoding::Utf16;
    let Some(general) = &capabilities.general else {
        return negotiated_encoding;
    };
    let Some(position_encodings) = &general.position_encodings else {
        return negotiated_encoding;
    };

    for encoding in position_encodings.iter() {
        if encoding == &lsp_types::PositionEncodingKind::UTF8 {
            return OffsetEncoding::Utf8;
        } else if encoding == &lsp_types::PositionEncodingKind::UTF32 {
            negotiated_encoding = OffsetEncoding::Utf32
        }
    }

    negotiated_encoding
}

pub fn source_span(
    source: &Source<'_>,
    range: lsp_types::Range,
    encoding: OffsetEncoding,
) -> anyhow::Result<Span> {
    let start = source_pos(source, range.start, encoding)?;
    let end = source_pos(source, range.start, encoding)?;
    Ok(Span::new(start, end))
}

pub fn source_pos(
    source: &Source<'_>,
    pos: lsp_types::Position,
    encoding: OffsetEncoding,
) -> anyhow::Result<Pos> {
    if pos.line as usize >= source.lines.len() {
        anyhow::bail!("position line out of bounds for text document: {pos:?}");
    }
    let line_text = source.line_str(pos.line);
    let char_offset = utf8_offset(line_text, pos.character, encoding)? as u32;
    Ok(Pos::new(pos.line, char_offset))
}

/// Compute the offset of this text in the given offset encoding.
pub fn encoded_offset(text: &str, encoding: OffsetEncoding) -> u32 {
    match encoding {
        OffsetEncoding::Utf8 => text.len() as u32,
        OffsetEncoding::Utf16 => text.encode_utf16().count() as u32,
        OffsetEncoding::Utf32 => text.chars().count() as u32,
    }
}

/// Compute the utf-8 offset from an existing char offset in the given offset encoding.
pub fn utf8_offset(
    text: &str,
    char_offset: u32,
    encoding: OffsetEncoding,
) -> anyhow::Result<usize> {
    if char_offset == 0 {
        return Ok(0);
    };

    match encoding {
        OffsetEncoding::Utf8 => Ok(char_offset as usize),
        OffsetEncoding::Utf16 => {
            let mut utf16_offset = 0;
            let mut char_iter = text.chars();
            while let Some(c) = char_iter.next() {
                utf16_offset += c.len_utf16() as u32;
                if utf16_offset >= char_offset {
                    return Ok(text.len() - char_iter.as_str().len());
                }
            }

            anyhow::bail!("character position out of bounds for text document");
        }
        OffsetEncoding::Utf32 => {
            let mut utf32_offset = 0;
            let mut char_iter = text.chars();
            while char_iter.next().is_some() {
                utf32_offset += 1;
                if utf32_offset >= char_offset {
                    return Ok(text.len() - char_iter.as_str().len());
                }
            }

            anyhow::bail!("character position out of bounds for text document");
        }
    }
}

/// Finds the utf-8 byte range.
pub fn text_byte_range(
    text: &[u8],
    range: lsp_types::Range,
    encoding: OffsetEncoding,
) -> anyhow::Result<std::ops::Range<usize>> {
    let [start, end] = text_range(text, range, encoding)?;
    Ok(start.byte_offset..end.byte_offset)
}

struct TextPos {
    byte_offset: usize,
    line: u32,
    char: u32,
}
fn text_range(
    text: &[u8],
    range: lsp_types::Range,
    encoding: OffsetEncoding,
) -> anyhow::Result<[TextPos; 2]> {
    // PERF: cache line byte offsets, see rust-analyzer's `LineIndex`
    let mut start_line_offset = 0;
    let mut line_iter = memchr::memchr_iter(b'\n', text);
    for _ in 0..range.start.line {
        let Some(o) = line_iter.next() else {
            anyhow::bail!("range start line out of bounds for text document: {range:?}");
        };
        start_line_offset = o + 1;
    }

    let mut end_line_offset = start_line_offset;
    for i in range.start.line..range.end.line {
        end_line_offset = match line_iter.next() {
            Some(o) => o + 1,
            // even if the final newline is missing allow the end range to be on the next "missing" line
            None if i == range.end.line - 1 => text.len(),
            None => anyhow::bail!("range end line out of bounds for text document: {range:?}"),
        };
    }

    let line_text = std::str::from_utf8(&text[start_line_offset..])?;
    let start_char_offset = utf8_offset(line_text, range.start.character, encoding)?;
    let start_offset = start_line_offset + start_char_offset;
    if start_offset > text.len() {
        anyhow::bail!("range start position out of bounds for text document: {range:?}");
    }

    let line_text = std::str::from_utf8(&text[end_line_offset..])?;
    let end_char_offset = utf8_offset(line_text, range.end.character, encoding)?;
    let end_offset = end_line_offset + end_char_offset;
    if end_offset > text.len() {
        anyhow::bail!("range end position out of bounds for text document: {range:?}");
    }

    let start = TextPos {
        byte_offset: start_offset,
        line: range.start.line,
        char: start_char_offset as u32,
    };
    let end = TextPos {
        byte_offset: end_offset,
        line: range.end.line,
        char: end_char_offset as u32,
    };
    Ok([start, end])
}

pub fn apply_document_changes(
    text: &str,
    mut changes: Vec<lsp_types::TextDocumentContentChangeEvent>,
    offset_encoding: OffsetEncoding,
) -> anyhow::Result<String> {
    // find last complete change, and dismiss redundant previous changes
    let (doc, changes) = if let Some(i) = changes.iter().rposition(|c| c.range.is_none()) {
        let text = std::mem::take(&mut changes[i].text);
        (text, &changes[i + 1..])
    } else {
        (text.to_string(), changes.as_slice())
    };

    let mut doc = doc.into_bytes();
    for change in changes {
        if let Some(range) = change.range {
            let text_range = text_byte_range(&doc, range, offset_encoding)?;
            doc.splice(text_range, change.text.bytes());
        }
    }

    let text = String::from_utf8(doc)?;
    Ok(text)
}

pub fn trim_last_line(text: &str) -> Option<&str> {
    let offset = text.bytes().rev().position(|b| b == b'\n')?;
    Some(&text[..text.len() - offset - 1])
}
