use std::fmt::Write as _;

use common::{FmtStr, OneVec, Pos, Span};

use crate::Error;
use crate::map::{MapTableEntryRepr, ParentThingy, PathSegment, fmt_ident};

#[derive(Debug)]
pub struct SerdeError<'a> {
    /// The reverse access path. The innermost table keys are stored first, and
    /// the outermost ones last.
    pub path: Vec<PathSegment<'a, 'a>>,
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
        // Build a string representation of the path.
        let mut path: Option<String> = None;
        for segment in error.path.iter().rev() {
            match segment {
                PathSegment::Table(reprs) => {
                    let buf = match &mut path {
                        Some(path) => {
                            path.push('.');
                            path
                        }
                        None => path.insert(String::new()),
                    };
                    fmt_ident(buf, reprs.first().key.repr_ident()).unwrap();
                }
                PathSegment::Array(_, idx) => {
                    let buf = path.get_or_insert(String::new());
                    write!(buf, "[{idx}]").unwrap();
                }
            }
        }

        // Collect all contextual lines from parent tables.
        let mut lines = Vec::new();
        let mut path_iter = error.path.iter();
        if let Some(segment) = path_iter.next() {
            match segment {
                PathSegment::Table(reprs) => {
                    let remaining = path_iter.as_slice();
                    for repr in reprs.iter() {
                        lines.push(repr.key.repr_ident().lit_start.line);
                        collect_lines(&mut lines, remaining, repr.parent);
                    }
                }
                PathSegment::Array(parent, _) => {
                    let remaining = path_iter.as_slice();
                    collect_lines(&mut lines, remaining, *parent);
                }
            }
            lines.sort();
            lines.dedup();
        }

        Error::Serde {
            lines: lines.into_boxed_slice(),
            path: path.map(FmtStr::from_string),
            msg: FmtStr::from_string(error.msg),
            span: error.span.unwrap_or(Span::pos(Pos::ZERO)),
        }
    }
}

fn collect_lines(lines: &mut Vec<u32>, path_segments: &[PathSegment], mut parent: ParentThingy) {
    for segment in path_segments.iter() {
        match segment {
            PathSegment::Table(reprs) => {
                let repr = &reprs[parent.0 as usize];
                lines.push(repr.key.repr_ident().lit_start.line);
                parent = repr.parent;
            }
            PathSegment::Array(p, _) => parent = *p,
        }
    }
}

impl<'a> SerdeError<'a> {
    pub fn new(msg: impl Into<String>) -> Self {
        Self {
            span: None,
            path: Vec::new(),
            msg: msg.into(),
        }
    }

    pub fn spanned(msg: impl Into<String>, span: Span) -> Self {
        Self {
            span: Some(span),
            path: Vec::new(),
            msg: msg.into(),
        }
    }

    pub fn with_table_path(mut self, reprs: &'a OneVec<MapTableEntryRepr<'a>>) -> Self {
        self.path.push(PathSegment::Table(reprs));
        self
    }

    pub fn with_array_path(mut self, parent: ParentThingy, idx: usize) -> Self {
        self.path.push(PathSegment::Array(parent, idx));
        self
    }

    pub fn with_path(mut self, segment: PathSegment<'a, 'a>) -> Self {
        self.path.push(segment);
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
            path: Vec::new(),
            msg: msg.to_string(),
        }
    }
}
