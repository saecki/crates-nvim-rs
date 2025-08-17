use common::{FmtStr, Pos, Span};

use crate::Error;
use crate::map::{MapArrayInlineEntry, MapArrayToplevelEntry, MapTableEntry, NodeEntry};

#[derive(Debug)]
pub struct SerdeError<'a> {
    /// The parent entry, which allows reconstructing the path.
    pub parent: Option<NodeEntry<'a>>,
    pub span: Option<Span>,
    pub msg: String,
}

impl std::error::Error for SerdeError<'_> {}

impl std::fmt::Display for SerdeError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.msg)
    }
}

impl From<SerdeError<'_>> for Error {
    fn from(error: SerdeError) -> Self {
        let lines = error.parent.map(|p| p.context_lines());
        let path = error.parent.map(|p| p.fmt_path());
        Error::Serde {
            lines: lines.unwrap_or_default(),
            path,
            msg: FmtStr::from_string(error.msg),
            span: error.span.unwrap_or(Span::pos(Pos::ZERO)),
        }
    }
}

impl<'a> SerdeError<'a> {
    pub fn new(msg: impl Into<String>) -> Self {
        Self {
            span: None,
            parent: None,
            msg: msg.into(),
        }
    }

    pub fn spanned(msg: impl Into<String>, span: Span) -> Self {
        Self {
            span: Some(span),
            parent: None,
            msg: msg.into(),
        }
    }

    pub fn with_table_parent(mut self, entry: &'a MapTableEntry<'a>) -> Self {
        self.parent.get_or_insert(NodeEntry::Table(entry));
        self
    }

    pub fn with_toplevel_array_parent(mut self, entry: &'a MapArrayToplevelEntry<'a>) -> Self {
        self.parent.get_or_insert(NodeEntry::ToplevelArray(entry));
        self
    }

    pub fn with_inline_array_parent(mut self, entry: &'a MapArrayInlineEntry<'a>) -> Self {
        self.parent.get_or_insert(NodeEntry::InlineArray(entry));
        self
    }

    pub fn with_span(mut self, span: Span) -> Self {
        if self.span.is_none() {
            self.span = Some(span);
        }
        self
    }
}

impl serde::de::Error for SerdeError<'_> {
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        SerdeError {
            span: None,
            parent: None,
            msg: msg.to_string(),
        }
    }
}
