use unicode_width::UnicodeWidthStr;

use crate::{Source, Span};

pub fn cmp<D: Diagnostic>(a: &D, b: &D) -> std::cmp::Ordering {
    span_cmp(a.span(), b.span())
}

pub fn span_cmp(a: Span, b: Span) -> std::cmp::Ordering {
    a.start.cmp(&b.start)
}

pub trait Diagnostic {
    type Hint: DiagnosticHint;

    const SEVERITY: Severity;

    /// The span of text that is highlighted by this diagnostic.
    fn span(&self) -> Span;

    /// A complete error description.
    fn description(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result;

    /// A shorter description shown inline next to the spanned text.
    ///
    /// This should not contain names that are spanned to reduce clutter.
    fn annotation(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result;

    /// A supporting hint providing further information related to this diagnostic.
    fn hint(&self) -> Option<Self::Hint> {
        None
    }

    /// Other relevant lines for this diagnostic. These may overlap with spans defined by this
    /// diagnostic or its hint.
    ///
    /// The line numbers are 0-based.
    fn context_lines(&self) -> Option<&[u32]> {
        None
    }
}

pub trait DiagnosticHint {
    /// The span of text that is highlighted by this diagnostic.
    fn span(&self) -> Span;

    /// A shorter description shown inline next to the spanned text.
    ///
    /// This should not contain names that are spanned to reduce clutter.
    fn annotation(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Info,
    Hint,
}

impl std::fmt::Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Severity::Error => f.write_str("error"),
            Severity::Warning => f.write_str("warning"),
            Severity::Info => f.write_str("info"),
            Severity::Hint => f.write_str("hint"),
        }
    }
}

pub trait DisplayDiagnostic: Diagnostic {
    fn display<'a>(&'a self, source: &'a Source<'a>) -> FmtDiagnostic<'a, Self>;
}

impl<D: Diagnostic> DisplayDiagnostic for D {
    fn display<'a>(&'a self, source: &'a Source<'a>) -> FmtDiagnostic<'a, D> {
        FmtDiagnostic {
            diagnostic: self,
            source,
        }
    }
}

pub struct FmtDiagnostic<'a, D: Diagnostic + ?Sized> {
    diagnostic: &'a D,
    source: &'a Source<'a>,
}

impl<D: Diagnostic> std::fmt::Display for FmtDiagnostic<'_, D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        display(f, self.diagnostic, self.source)
    }
}

pub fn display(
    f: &mut impl std::fmt::Write,
    diagnostic: &impl Diagnostic,
    source: &Source,
) -> std::fmt::Result {
    fn display_context_lines(
        f: &mut impl std::fmt::Write,
        source: &Source,
        context_lines: &[u32],
        range: std::ops::Range<u32>,
    ) -> std::fmt::Result {
        for &l in context_lines.iter() {
            if l < range.start {
                continue;
            }
            if l < range.end {
                display_line(f, l, source.line_str(l))?;
            } else {
                break;
            }
        }
        Ok(())
    }

    writeln!(f, "{}", diagnostic.header(source))?;
    writeln!(f, "     {ANSII_COLOR_BLUE}|{ANSII_CLEAR}")?;

    let context_lines = diagnostic.context_lines().unwrap_or(&[]);
    let main_span = diagnostic.span();
    let hint = diagnostic.hint();
    let mut current_line = 0;

    if let Some(hint) = &hint {
        let hint_span = hint.span();
        if hint_span.start < main_span.start {
            display_context_lines(f, source, context_lines, current_line..hint_span.start.line)?;
            write!(f, "{}", hint.body(source))?;
            current_line = hint_span.end.line + 1;
        }
    }

    display_context_lines(f, source, context_lines, current_line..main_span.start.line)?;
    write!(f, "{}", diagnostic.body(source))?;
    current_line = main_span.end.line + 1;

    if let Some(hint) = &hint {
        let hint_span = hint.span();
        if hint_span.start >= main_span.start {
            display_context_lines(f, source, context_lines, current_line..hint_span.start.line)?;
            write!(f, "{}", hint.body(source))?;
        }
    }

    Ok(())
}

pub trait DisplayDiagnosticHeader: Diagnostic {
    fn header<'a>(&'a self, source: &'a Source<'a>) -> FmtDiagnosticHeader<'a, Self>;
}

impl<D: Diagnostic> DisplayDiagnosticHeader for D {
    fn header<'a>(&'a self, source: &'a Source<'a>) -> FmtDiagnosticHeader<'a, D> {
        FmtDiagnosticHeader {
            diagnostic: self,
            source,
        }
    }
}

pub struct FmtDiagnosticHeader<'a, D: Diagnostic + ?Sized> {
    diagnostic: &'a D,
    source: &'a Source<'a>,
}

impl<D: Diagnostic> std::fmt::Display for FmtDiagnosticHeader<'_, D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        display_header(f, self.diagnostic, self.source)
    }
}

pub fn display_header<D: Diagnostic>(
    f: &mut impl std::fmt::Write,
    diagnostic: &D,
    source: &Source,
) -> std::fmt::Result {
    let severity = D::SEVERITY;
    let color = ansii_esc_color(severity);
    write!(f, "{color}{severity}{ANSII_CLEAR}: ")?;
    diagnostic.description(f)?;
    f.write_char('\n')?;
    let pos = diagnostic.span().start;
    let line_nr = pos.line + 1;
    let char = source.line_str(pos.line)[..pos.char as usize]
        .chars()
        .count();
    let path = source.path;
    write!(f, "    {ANSII_COLOR_BLUE}-->{ANSII_CLEAR} ")?;
    write!(f, "{path}:{line_nr}:{char}")?;
    Ok(())
}

pub trait DisplayDiagnosticBody: Diagnostic {
    fn body<'a>(&'a self, source: &'a Source<'a>) -> FmtDiagnosticBody<'a, Self>;
}

impl<D: Diagnostic> DisplayDiagnosticBody for D {
    fn body<'a>(&'a self, source: &'a Source<'a>) -> FmtDiagnosticBody<'a, D> {
        FmtDiagnosticBody {
            diagnostic: self,
            source,
        }
    }
}

pub struct FmtDiagnosticBody<'a, D: Diagnostic + ?Sized> {
    diagnostic: &'a D,
    source: &'a Source<'a>,
}

impl<'a, D: Diagnostic> std::fmt::Display for FmtDiagnosticBody<'a, D> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        display_body(
            f,
            |f| self.diagnostic.annotation(f),
            D::SEVERITY,
            self.diagnostic.span(),
            self.source,
        )
    }
}

pub trait DisplayDiagnosticHintBody: DiagnosticHint {
    fn body<'a>(&'a self, source: &'a Source<'a>) -> FmtDiagnosticHintBody<'a, Self>;
}

impl<D: DiagnosticHint> DisplayDiagnosticHintBody for D {
    fn body<'a>(&'a self, source: &'a Source<'a>) -> FmtDiagnosticHintBody<'a, D> {
        FmtDiagnosticHintBody {
            diagnostic: self,
            source,
        }
    }
}

pub struct FmtDiagnosticHintBody<'a, D: DiagnosticHint + ?Sized> {
    diagnostic: &'a D,
    source: &'a Source<'a>,
}

impl<'a, D: DiagnosticHint> std::fmt::Display for FmtDiagnosticHintBody<'a, D> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        display_body(
            f,
            |f| self.diagnostic.annotation(f),
            Severity::Hint,
            self.diagnostic.span(),
            self.source,
        )
    }
}

fn display_body<F: std::fmt::Write>(
    f: &mut F,
    annotation: impl Fn(&mut F) -> std::fmt::Result,
    severity: Severity,
    span: Span,
    source: &Source,
) -> std::fmt::Result {
    let start_line = span.start.line;
    let end_line = span.end.line;
    let color = ansii_esc_color(severity);
    let underline_char = underline_char(severity);

    for line_idx in start_line..=end_line {
        let line = source.line_str(line_idx);
        display_line(f, line_idx, line)?;

        let char_start = if line_idx == start_line {
            span.start.char as usize
        } else {
            0
        };
        let char_end = if line_idx == end_line {
            span.end.char as usize
        } else {
            line.len()
        };
        let num_spaces = calc_width(&line[0..char_start]);
        write!(f, "     {ANSII_COLOR_BLUE}|{ANSII_CLEAR} ")?;
        write!(f, "{:num_spaces$}{color}", "")?;

        let spanned_text = &line[char_start..char_end];
        let num_carets = spanned_text.width().max(1);
        for _ in 0..num_carets {
            f.write_char(underline_char)?;
        }
        if line_idx == end_line {
            f.write_char(' ')?;
            annotation(f)?;
        }
        writeln!(f, "{ANSII_CLEAR}")?;
    }

    Ok(())
}

/// `line_idx` is 0-based
pub fn display_line(f: &mut impl std::fmt::Write, line_idx: u32, line: &str) -> std::fmt::Result {
    let line_nr = line_idx + 1;
    write!(f, "{ANSII_COLOR_BLUE}{line_nr:4} |{ANSII_CLEAR} ")?;

    let mut next_start = 0;
    for (j, c) in line.char_indices() {
        match c {
            // print tab ('\x09')
            '\t' => (),
            // ignore all other control characters
            '\x00'..='\x1f' | '\x7f' => {
                f.write_str(&line[next_start..j])?;
                next_start = j + 1;
            }
            _ => (),
        }
    }
    f.write_str(&line[next_start..])?;
    f.write_char('\n')?;
    Ok(())
}

pub fn calc_width(line: &str) -> usize {
    let mut width = 0;
    let mut next_start = 0;
    for (j, c) in line.char_indices() {
        match c {
            '\t' => (),
            // backspace
            '\x00'..='\x1f' | '\x7f' => {
                width += line[next_start..j].width();
                next_start = j + 1;
            }
            _ => (),
        }
    }
    width += line[next_start..].width();
    width
}

pub const ANSII_CLEAR: &str = "\x1b[0m";
pub const ANSII_UNDERLINED: &str = "\x1b[4m";
pub const ANSII_COLOR_RED: &str = "\x1b[91m";
pub const ANSII_COLOR_YELLOW: &str = "\x1b[93m";
pub const ANSII_COLOR_BLUE: &str = "\x1b[94m";
pub const ANSII_COLOR_CYAN: &str = "\x1b[96m";

pub const fn ansii_esc_color(severity: Severity) -> &'static str {
    match severity {
        Severity::Error => ANSII_COLOR_RED,
        Severity::Warning => ANSII_COLOR_YELLOW,
        Severity::Info => ANSII_COLOR_CYAN,
        Severity::Hint => ANSII_COLOR_BLUE,
    }
}

pub const fn underline_char(severity: Severity) -> char {
    match severity {
        Severity::Error => '^',
        Severity::Warning => '^',
        Severity::Info => '^',
        Severity::Hint => '-',
    }
}
