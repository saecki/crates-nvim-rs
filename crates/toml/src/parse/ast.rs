use std::num::NonZeroU32;

use common::{Pos, Span};

use crate::Quote;
use crate::datetime::DateTime;
use crate::lex::{Source, TextOffset};

#[derive(Debug, PartialEq)]
pub struct Ast<'a> {
    pub source: Source<'a>,
    pub toplevel: &'a [Toplevel<'a>],
    pub comments: &'a [AssocComment],
}

#[derive(Debug, PartialEq)]
pub enum Toplevel<'a> {
    Assignment(ToplevelAssignment<'a>),
    Table(Table<'a>),
    Array(ArrayEntry<'a>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CommentId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CommentRange {
    pub start: CommentId,
    pub len: u32,
    pub level: u16,
}

impl CommentRange {
    pub const fn new(start: CommentId, len: u32, level: u16) -> Self {
        Self { start, len, level }
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Append all comments including this one to the range.
    pub(in crate::parse) fn append(&mut self, id: CommentId) {
        self.len = id.0 - self.start.0 + 1;
    }

    /// Set the end bound of this comment range. If this happens to be the same as start, this
    /// range remains empty.
    pub(in crate::parse) fn extend_to(&mut self, id: CommentId) {
        self.len = id.0 - self.start.0;
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AssocComment {
    pub pos: AssocPos,
    /// level 0 are comments that are associated with items declared directly in root, such as
    /// assignments (that aren't declared inside a table), tables and arrays of tables
    pub level: u16,
    pub comment: Comment,
}

impl AssocComment {
    pub fn above(level: u16, comment: Comment) -> AssocComment {
        AssocComment {
            pos: AssocPos::Above,
            level,
            comment,
        }
    }

    pub fn line_end(level: u16, comment: Comment) -> AssocComment {
        AssocComment {
            pos: AssocPos::LineEnd,
            level,
            comment,
        }
    }

    pub fn contained(level: u16, comment: Comment) -> AssocComment {
        AssocComment {
            pos: AssocPos::Contained,
            level,
            comment,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AssocPos {
    /// Directly above the associated item, without any blank lines
    Above,
    /// At the end of the same line as the associated item.
    LineEnd,
    /// Contained inside the item, or at file root, if level is 0.
    Contained,
}

#[derive(Debug, PartialEq)]
pub struct Table<'a> {
    pub comments: CommentRange,
    pub header: TableHeader<'a>,
    pub assignments: Vec<ToplevelAssignment<'a>>,
}

impl Table<'_> {
    #[inline]
    pub fn span(&self) -> Span {
        Span::new(self.start(), self.end())
    }

    #[inline]
    pub fn start(&self) -> Pos {
        self.header.start()
    }

    #[inline]
    pub fn end(&self) -> Pos {
        (self.assignments.last())
            .map(|a| a.assignment.val.end())
            .unwrap_or_else(|| self.header.end())
    }

    pub fn append_comment_range(&mut self) -> &mut CommentRange {
        match self.assignments.last_mut() {
            Some(a) => &mut a.comments,
            None => &mut self.comments,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct TableHeader<'a> {
    pub l_par: Pos,
    pub key: Option<Key<'a>>,
    pub r_par_offset: Option<NonZeroU32>,
}

impl<'a> TableHeader<'a> {
    pub fn new(l_par: Pos, key: Option<Key<'a>>, r_par: Option<Pos>) -> Self {
        Self {
            l_par,
            key,
            r_par_offset: r_par.map(|r_par| {
                NonZeroU32::new(r_par.char - l_par.char)
                    .expect("l_par and r_par can't be at the same position")
            }),
        }
    }

    #[inline]
    pub fn span(&self) -> Span {
        Span::new(self.start(), self.end())
    }

    #[inline]
    pub fn start(&self) -> Pos {
        self.l_par
    }

    #[inline]
    pub fn end(&self) -> Pos {
        (self.r_par().map(|p| p.plus(1)))
            .or_else(|| self.key.as_ref().map(|k| k.end()))
            .unwrap_or_else(|| self.l_par.plus(1))
    }

    pub fn r_par(&self) -> Option<Pos> {
        self.r_par_offset.map(|o| self.l_par.plus(o.get()))
    }
}

#[derive(Debug, PartialEq)]
pub struct ArrayEntry<'a> {
    pub comments: CommentRange,
    pub header: ArrayHeader<'a>,
    pub assignments: Vec<ToplevelAssignment<'a>>,
}

impl ArrayEntry<'_> {
    #[inline]
    pub fn span(&self) -> Span {
        Span::new(self.start(), self.end())
    }

    #[inline]
    pub fn start(&self) -> Pos {
        self.header.start()
    }

    #[inline]
    pub fn end(&self) -> Pos {
        (self.assignments.last())
            .map(|a| a.assignment.val.end())
            .unwrap_or_else(|| self.header.end())
    }

    /// Comment on the same line as the last item of this table
    pub fn append_comment_range(&mut self) -> &mut CommentRange {
        match self.assignments.last_mut() {
            Some(a) => &mut a.comments,
            None => &mut self.comments,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ArrayHeader<'a> {
    pub l_pars: (Pos, Pos),
    pub key: Option<Key<'a>>,
    pub r_par_offsets: (Option<NonZeroU32>, Option<NonZeroU32>),
}

impl<'a> ArrayHeader<'a> {
    pub fn new(
        l_pars: (Pos, Pos),
        key: Option<Key<'a>>,
        r_pars: (Option<Pos>, Option<Pos>),
    ) -> Self {
        Self {
            l_pars,
            key,
            r_par_offsets: (
                r_pars.0.map(|r_par| {
                    NonZeroU32::new(r_par.char - l_pars.0.char)
                        .expect("l_par and r_par can't be at the same position")
                }),
                r_pars.1.map(|r_par| {
                    NonZeroU32::new(r_par.char - l_pars.0.char)
                        .expect("l_par and r_par can't be at the same position")
                }),
            ),
        }
    }

    #[inline]
    pub fn span(&self) -> Span {
        Span::new(self.start(), self.end())
    }

    #[inline]
    pub fn start(&self) -> Pos {
        self.l_pars.0
    }

    #[inline]
    pub fn end(&self) -> Pos {
        let r_pars = self.r_pars();
        (r_pars.1.or(r_pars.0).map(|p| p.plus(1)))
            .or_else(|| self.key.as_ref().map(|k| k.end()))
            .unwrap_or_else(|| self.l_pars.1.plus(1))
    }

    #[inline(always)]
    pub fn r_pars(&self) -> (Option<Pos>, Option<Pos>) {
        let a = (self.r_par_offsets.0).map(|o| self.l_pars.0.plus(o.get()));
        let b = (self.r_par_offsets.1).map(|o| self.l_pars.0.plus(o.get()));
        (a, b)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ToplevelAssignment<'a> {
    pub comments: CommentRange,
    pub assignment: Assignment<'a>,
}

impl ToplevelAssignment<'_> {
    #[inline(always)]
    pub fn span(&self) -> Span {
        self.assignment.span()
    }

    #[inline(always)]
    pub fn start(&self) -> Pos {
        self.assignment.start()
    }

    #[inline(always)]
    pub fn end(&self) -> Pos {
        self.assignment.end()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment<'a> {
    pub key: Key<'a>,
    pub eq: Pos,
    pub val: Value<'a>,
}

impl Assignment<'_> {
    #[inline]
    pub fn span(&self) -> Span {
        Span::new(self.start(), self.end())
    }

    #[inline]
    pub fn start(&self) -> Pos {
        self.key.start()
    }

    #[inline]
    pub fn end(&self) -> Pos {
        self.val.end()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Key<'a> {
    One(Ident<'a>),
    Dotted(&'a [DottedIdent<'a>]),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DottedIdent<'a> {
    pub ident: Ident<'a>,
    pub dot: Option<Pos>,
}

impl Key<'_> {
    #[inline]
    pub fn span(&self) -> Span {
        Span::new(self.start(), self.end())
    }

    #[inline]
    pub fn start(&self) -> Pos {
        match self {
            Key::One(i) => i.lit_start,
            Key::Dotted(idents) => idents.first().unwrap().ident.lit_start,
        }
    }

    #[inline]
    pub fn end(&self) -> Pos {
        match self {
            Key::One(i) => i.lit_end(),
            Key::Dotted(idents) => {
                let last = idents.last().unwrap();
                last.dot
                    .map(|p| p.plus(1))
                    .unwrap_or_else(|| last.ident.lit_end())
            }
        }
    }
}

/// Identifiers cannot contain line breaks.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ident<'a> {
    pub lit_start: Pos,
    pub lit_len: u32,
    pub text: &'a str,
    pub text_start_offset: u8,
    pub text_end_offset: u8,
    pub kind: IdentKind,
}

impl<'a> Ident<'a> {
    pub fn from_plain_lit(lit: &'a str, span: Span) -> Self {
        Ident {
            lit_start: span.start,
            lit_len: lit.len() as u32,
            text: lit,
            text_start_offset: 0,
            text_end_offset: 0,
            kind: IdentKind::Plain,
        }
    }

    pub fn from_string(
        lit_span: Span,
        text: &'a str,
        text_offset: TextOffset,
        kind: IdentKind,
    ) -> Self {
        let lit_len = lit_span.end.char - lit_span.start.char;
        Ident {
            lit_start: lit_span.start,
            lit_len,
            text,
            // multiline strings aren't allowed as identifiers, hence line offsets are zero
            text_start_offset: text_offset.start_char,
            text_end_offset: text_offset.end_char,
            kind,
        }
    }

    #[inline(always)]
    pub fn lit_span(&self) -> Span {
        Span::from_pos_len(self.lit_start, self.lit_len)
    }

    #[inline(always)]
    pub fn lit_end(&self) -> Pos {
        self.lit_start.plus(self.lit_len)
    }

    #[inline(always)]
    pub fn text_span(&self) -> Span {
        let start = self.lit_start.plus(self.text_start_offset as u32);
        let len = self.lit_len - (self.text_start_offset + self.text_end_offset) as u32;
        Span::from_pos_len(start, len)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IdentKind {
    Plain,
    BasicString,
    LiteralString,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'a> {
    String(StringVal<'a>),
    Int(IntVal),
    Float(FloatVal),
    Bool(BoolVal),
    DateTime(DateTimeVal),
    InlineTable(InlineTable<'a>),
    InlineArray(InlineArray<'a>),
    Invalid(Span),
}

impl Value<'_> {
    #[inline]
    pub fn span(&self) -> Span {
        match self {
            Value::String(s) => s.lit_span,
            Value::Int(i) => i.lit_span,
            Value::Float(f) => f.lit_span,
            Value::Bool(b) => b.lit_span,
            Value::DateTime(d) => d.lit_span,
            Value::InlineTable(t) => t.span(),
            Value::InlineArray(a) => a.span(),
            Value::Invalid(span) => *span,
        }
    }

    #[inline]
    pub fn start(&self) -> Pos {
        match self {
            Value::String(s) => s.lit_span.start,
            Value::Int(i) => i.lit_span.start,
            Value::Float(f) => f.lit_span.start,
            Value::Bool(b) => b.lit_span.start,
            Value::DateTime(d) => d.lit_span.start,
            Value::InlineTable(t) => t.start(),
            Value::InlineArray(a) => a.start(),
            Value::Invalid(span) => span.start,
        }
    }

    #[inline]
    pub fn end(&self) -> Pos {
        match self {
            Value::String(s) => s.lit_span.end,
            Value::Int(i) => i.lit_span.end,
            Value::Float(f) => f.lit_span.end,
            Value::Bool(b) => b.lit_span.end,
            Value::DateTime(d) => d.lit_span.end,
            Value::InlineTable(t) => t.end(),
            Value::InlineArray(a) => a.end(),
            Value::Invalid(span) => span.end,
        }
    }

    pub fn is_valid(&self) -> bool {
        !matches!(self, Value::Invalid(..))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringVal<'a> {
    pub lit_span: Span,
    pub text: &'a str,
    pub text_offset: TextOffset,
    pub quote: Quote,
}

impl StringVal<'_> {
    pub fn text_span(&self) -> Span {
        self.text_offset.apply_to(self.lit_span)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IntVal {
    pub lit_span: Span,
    pub val: i64,
}

impl IntVal {
    pub fn new(lit_span: Span, val: i64) -> Self {
        Self { lit_span, val }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FloatVal {
    pub lit_span: Span,
    pub val: f64,
}

impl FloatVal {
    pub fn new(lit_span: Span, val: f64) -> Self {
        Self { lit_span, val }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BoolVal {
    pub lit_span: Span,
    pub val: bool,
}

impl BoolVal {
    pub fn new(lit_span: Span, val: bool) -> Self {
        Self { lit_span, val }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DateTimeVal {
    pub lit_span: Span,
    pub val: DateTime,
}

impl DateTimeVal {
    pub fn new(lit_span: Span, val: DateTime) -> Self {
        Self { lit_span, val }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InlineTable<'a> {
    pub l_par: Pos,
    pub assignments: Vec<InlineTableAssignment<'a>>,
    pub end: End,
}

impl InlineTable<'_> {
    #[inline]
    pub fn span(&self) -> Span {
        Span::new(self.start(), self.end())
    }

    #[inline]
    pub fn start(&self) -> Pos {
        self.l_par
    }

    #[inline]
    pub fn end(&self) -> Pos {
        self.end.end_pos()
    }

    #[inline]
    pub fn r_par(&self) -> Option<Pos> {
        match self.end {
            End::Par(p) => Some(p),
            End::None(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InlineTableAssignment<'a> {
    pub assignment: Assignment<'a>,
    pub comma: Option<Pos>,
}

impl InlineTableAssignment<'_> {
    #[inline]
    pub fn span(&self) -> Span {
        Span::new(self.start(), self.end())
    }

    #[inline]
    pub fn start(&self) -> Pos {
        self.assignment.key.start()
    }

    #[inline]
    pub fn end(&self) -> Pos {
        self.comma
            .map(|c| c.plus(1))
            .unwrap_or_else(|| self.assignment.val.end())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InlineArray<'a> {
    pub comments: CommentRange,
    pub l_par: Pos,
    pub values: Vec<InlineArrayValue<'a>>,
    pub end: End,
}

impl InlineArray<'_> {
    #[inline]
    pub fn span(&self) -> Span {
        Span {
            start: self.start(),
            end: self.end(),
        }
    }

    #[inline]
    pub fn start(&self) -> Pos {
        self.l_par
    }

    #[inline]
    pub fn end(&self) -> Pos {
        self.end.end_pos()
    }

    #[inline]
    pub fn r_par(&self) -> Option<Pos> {
        match self.end {
            End::Par(p) => Some(p),
            End::None(_) => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum End {
    /// Position of the closing parenthesis.
    Par(Pos),
    /// End of the parent value,
    None(Pos),
}

impl End {
    #[inline]
    pub fn end_pos(&self) -> Pos {
        match self {
            End::Par(p) => p.plus(1),
            End::None(p) => *p,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InlineArrayValue<'a> {
    pub comments: CommentRange,
    pub val: Value<'a>,
    pub comma: Option<Pos>,
}

impl InlineArrayValue<'_> {
    #[inline]
    pub fn span(&self) -> Span {
        Span::new(self.start(), self.end())
    }

    #[inline]
    pub fn start(&self) -> Pos {
        self.val.start()
    }

    #[inline]
    pub fn end(&self) -> Pos {
        self.comma
            .map(|c| c.plus(1))
            .unwrap_or_else(|| self.val.end())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Comment {
    pub span: Span,
}

impl Comment {
    pub fn from_pos_len(pos: Pos, len: u32) -> Self {
        let span = Span::from_pos_len(pos, len);
        Comment { span }
    }
}
