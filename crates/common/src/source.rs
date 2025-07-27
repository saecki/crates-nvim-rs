use crate::onevec::OneVec;
use crate::{Pos, Span};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Source<'a> {
    pub text: &'a str,
    /// Line start indices. The first one is guaranteed to be 0, and all others
    /// immediately follow a newline (`'\n'`) character.
    pub lines: OneVec<u32>,
}

impl<'a> From<&'a str> for Source<'a> {
    fn from(input: &'a str) -> Self {
        let mut lines = OneVec::new(0);
        lines.extend(memchr::memchr_iter(b'\n', input.as_bytes()).map(|i| i as u32 + 1));
        Self { text: input, lines }
    }
}

impl<'a> Source<'a> {
    pub fn spanned_str(&self, span: Span) -> &'a str {
        let start = self.lines[span.start.line as usize] + span.start.char;
        let end = self.lines[span.end.line as usize] + span.end.char;
        &self.text[start as usize..end as usize]
    }

    /// The line corresponding to the index with newlines (`'\n'`) and carriage
    /// returns (`'\r'`) trimmed.
    pub fn line_str(&self, line: u32) -> &str {
        let start = self.lines[line as usize] as usize;
        let end = (self.lines.get(line as usize + 1))
            .map(|i| {
                // Trim off the newline.
                let mut end = *i as usize - 1;

                // Trim off the carriage return.
                if let [_, b'\r'] = &self.text.as_bytes()[..end] {
                    end -= 1;
                }
                end
            })
            .unwrap_or(self.text.len());

        &self.text[start..end]
    }

    pub fn range(&self, span: Span) -> std::ops::Range<usize> {
        self.index(span.start)..self.index(span.end)
    }

    pub fn index(&self, pos: Pos) -> usize {
        self.lines[pos.line as usize] as usize + pos.char as usize
    }
}
