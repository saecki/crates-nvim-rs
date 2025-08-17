use common::{FmtStr, Pos, Span};

use crate::map::{MapArrayInlineEntry, MapArrayToplevelEntry, MapTableEntry};
use crate::{Error, map};

#[derive(Debug)]
pub struct SerdeError<'a> {
    /// The parent entry, which allows reconstructing the path.
    pub parent: Option<ErrorParent<'a>>,
    pub span: Option<Span>,
    pub msg: String,
}

#[derive(Clone, Copy, Debug)]
pub enum ErrorParent<'a> {
    Table(&'a MapTableEntry<'a>),
    ToplevelArray(&'a MapArrayToplevelEntry<'a>),
    InlineArray(&'a MapArrayInlineEntry<'a>),
}

impl<'a> ErrorParent<'a> {
    pub fn context_lines(&self) -> Box<[u32]> {
        let mut lines = Vec::new();
        match self {
            ErrorParent::Table(entry) => {
                for repr in entry.reprs.iter() {
                    lines.push(repr.key.repr_ident().lit_start.line);
                    if let Some(parent_entry) = repr.parent.repr().parent_entry() {
                        map::collect_lines(&mut lines, parent_entry);
                    }
                }
            }
            ErrorParent::ToplevelArray(entry) => {
                lines.push(entry.definition.start().line);
                map::collect_lines(&mut lines, entry.parent_entry);
            }
            ErrorParent::InlineArray(entry) => {
                lines.push(entry.repr.start().line);
                map::collect_lines(&mut lines, entry.parent.parent);
            }
        }
        lines.sort();
        lines.dedup();
        lines.into_boxed_slice()
    }

    pub fn fmt_path(&self) -> FmtStr {
        match self {
            ErrorParent::Table(entry) => {
                let repr = entry.reprs.first();
                map::joined_path(repr.parent, repr.key.repr_ident())
            }
            ErrorParent::ToplevelArray(entry) => {
                let mut buf = map::fmt_path(entry.parent_entry);
                map::fmt_array_idx(&mut buf, entry.idx).ok();
                FmtStr::from_string(buf)
            }
            ErrorParent::InlineArray(entry) => {
                let mut buf = map::fmt_path(entry.parent.parent);
                map::fmt_array_idx(&mut buf, entry.idx).ok();
                FmtStr::from_string(buf)
            }
        }
    }
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
        self.parent.get_or_insert(ErrorParent::Table(entry));
        self
    }

    pub fn with_toplevel_array_parent(mut self, entry: &'a MapArrayToplevelEntry<'a>) -> Self {
        self.parent.get_or_insert(ErrorParent::ToplevelArray(entry));
        self
    }

    pub fn with_inline_array_parent(mut self, entry: &'a MapArrayInlineEntry<'a>) -> Self {
        self.parent.get_or_insert(ErrorParent::InlineArray(entry));
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
