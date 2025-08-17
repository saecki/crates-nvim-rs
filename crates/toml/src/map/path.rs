use common::{FmtChar, FmtStr};

use crate::map::parent::{ParentEntry, ParentTable};
use crate::map::{MapArrayInlineEntry, MapArrayToplevelEntry, MapTableEntry};
use crate::parse::Ident;

#[derive(Clone, Copy, Debug)]
pub enum NodeEntry<'a> {
    Table(&'a MapTableEntry<'a>),
    ToplevelArray(&'a MapArrayToplevelEntry<'a>),
    InlineArray(&'a MapArrayInlineEntry<'a>),
}

impl<'a> From<&'a MapTableEntry<'a>> for NodeEntry<'a> {
    fn from(value: &'a MapTableEntry<'a>) -> Self {
        Self::Table(value)
    }
}

impl<'a> From<&'a MapArrayToplevelEntry<'a>> for NodeEntry<'a> {
    fn from(value: &'a MapArrayToplevelEntry<'a>) -> Self {
        Self::ToplevelArray(value)
    }
}

impl<'a> From<&'a MapArrayInlineEntry<'a>> for NodeEntry<'a> {
    fn from(value: &'a MapArrayInlineEntry<'a>) -> Self {
        Self::InlineArray(value)
    }
}

impl<'a> NodeEntry<'a> {
    pub fn context_lines(&self) -> Box<[u32]> {
        let mut lines = Vec::new();
        match self {
            NodeEntry::Table(entry) => {
                for repr in entry.reprs.iter() {
                    lines.push(repr.key.repr_ident().lit_start.line);
                    if let Some(parent_entry) = repr.parent.repr().parent_entry() {
                        collect_lines(&mut lines, parent_entry);
                    }
                }
            }
            NodeEntry::ToplevelArray(entry) => {
                lines.push(entry.definition.start().line);
                collect_lines(&mut lines, entry.parent_entry);
            }
            NodeEntry::InlineArray(entry) => {
                lines.push(entry.repr.start().line);
                collect_lines(&mut lines, entry.parent.parent);
            }
        }
        lines.sort();
        lines.dedup();
        lines.into_boxed_slice()
    }

    pub fn fmt_path(&self) -> FmtStr {
        match self {
            NodeEntry::Table(entry) => {
                let repr = entry.reprs.first();
                joined_path(repr.parent, repr.key.repr_ident())
            }
            NodeEntry::ToplevelArray(entry) => {
                let mut buf = fmt_path(entry.parent_entry);
                fmt_array_idx(&mut buf, entry.idx).ok();
                FmtStr::from_string(buf)
            }
            NodeEntry::InlineArray(entry) => {
                let mut buf = fmt_path(entry.parent.parent);
                fmt_array_idx(&mut buf, entry.idx).ok();
                FmtStr::from_string(buf)
            }
        }
    }
}

pub fn joined_path(parent: ParentTable, ident: &Ident) -> FmtStr {
    let parent_entry = parent.repr().parent_entry();
    let mut buf = parent_entry.map(fmt_path).unwrap_or_default();
    if parent_entry.is_some() {
        buf.push('.');
    }
    fmt_ident(&mut buf, ident).unwrap();
    FmtStr::from_string(buf)
}

pub fn fmt_path(parent_entry: ParentEntry) -> String {
    match parent_entry {
        ParentEntry::Table(entry) => {
            let repr = entry.repr();
            let mut buf = (repr.parent.repr())
                .parent_entry()
                .map(fmt_path)
                .unwrap_or_default();
            if !buf.is_empty() {
                buf.push('.');
            }
            let ident = repr.key.repr_ident();
            fmt_ident(&mut buf, ident).unwrap();
            buf
        }
        ParentEntry::ToplevelArray(entry) => {
            let array_entry = entry.get();
            let mut buf = fmt_path(array_entry.parent_entry);
            fmt_array_idx(&mut buf, array_entry.idx).ok();
            buf
        }
        ParentEntry::ToplevelArrayExtension(entry) => {
            let mut buf = fmt_path(entry.parent_table_entry().wrap());
            fmt_array_idx(&mut buf, entry.get().idx).ok();
            buf
        }
        ParentEntry::InlineArray(entry) => {
            let array_entry = entry.get();
            let mut buf = fmt_path(array_entry.parent.get().parent);
            fmt_array_idx(&mut buf, array_entry.idx).ok();
            buf
        }
    }
}

pub struct FmtIdent<'a>(pub &'a str);

impl std::fmt::Display for FmtIdent<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt_ident_str(f, self.0)
    }
}

pub fn fmt_ident(f: &mut impl std::fmt::Write, key: &Ident) -> std::fmt::Result {
    fmt_ident_str(f, key.text)
}

fn fmt_ident_str(f: &mut impl std::fmt::Write, key: &str) -> std::fmt::Result {
    if key.is_empty() {
        f.write_str("''")?;
    } else {
        let is_invalid_plain_ident =
            (key.chars()).any(|c| !matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-'));
        if is_invalid_plain_ident {
            f.write_char('\'')?;
            for c in key.chars() {
                write!(f, "{}", FmtChar(c))?;
            }
            f.write_char('\'')?;
        } else {
            f.write_str(key)?;
        }
    }
    Ok(())
}

pub fn fmt_array_idx(f: &mut impl std::fmt::Write, idx: u32) -> std::fmt::Result {
    write!(f, "[{idx}]")
}

pub fn context_lines<'a>(parents: impl IntoIterator<Item = ParentEntry<'a>>) -> Box<[u32]> {
    let mut lines = Vec::new();
    for parent in parents.into_iter() {
        collect_lines(&mut lines, parent);
    }
    lines.sort();
    lines.dedup();
    lines.into_boxed_slice()
}

pub fn collect_lines(lines: &mut Vec<u32>, parent_entry: ParentEntry) {
    match parent_entry {
        ParentEntry::Table(entry) => {
            let repr = entry.repr();
            if let Some(parent) = repr.parent.repr().parent_entry() {
                collect_lines(lines, parent);
            }
            lines.push(repr.key.repr_ident().lit_start.line);
        }
        ParentEntry::ToplevelArray(entry) => {
            let array_entry = entry.get();
            collect_lines(lines, array_entry.parent_entry);
            lines.push(array_entry.definition.start().line);
        }
        ParentEntry::ToplevelArrayExtension(entry) => {
            collect_lines(lines, ParentEntry::Table(entry.parent_table_entry()));
            lines.push(entry.get().definition.start().line);
        }
        ParentEntry::InlineArray(entry) => {
            let array_entry = entry.get();
            collect_lines(lines, array_entry.parent.get().parent);
            lines.push(array_entry.repr.start().line);
        }
    }
}
