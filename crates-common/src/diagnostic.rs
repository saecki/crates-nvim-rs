use unicode_width::UnicodeWidthStr;

use crate::Span;

pub trait Diagnostic {
    const SEVERITY: Severity;

    fn span(&self) -> Span;
    fn description(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result;
    fn annotation(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Hint,
}

impl std::fmt::Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Severity::Error => f.write_str("error"),
            Severity::Warning => f.write_str("warning"),
            Severity::Hint => f.write_str("hint"),
        }
    }
}

pub trait DisplayDiagnostic: Diagnostic + Sized {
    fn header<'a, T>(&'a self, text: &'a [T]) -> DiagnosticHeader<'a, Self, T>;
    fn body<'a, T>(&'a self, text: &'a [T]) -> DiagnosticBody<'a, Self, T>;
}

impl<D: Diagnostic> DisplayDiagnostic for D {
    fn header<'a, T>(&'a self, text: &'a [T]) -> DiagnosticHeader<'a, D, T> {
        DiagnosticHeader {
            diagnostic: self,
            text,
        }
    }

    fn body<'a, T>(&'a self, text: &'a [T]) -> DiagnosticBody<'a, D, T> {
        DiagnosticBody {
            diagnostic: self,
            text,
        }
    }
}

pub struct DiagnosticHeader<'a, D: Diagnostic, T> {
    diagnostic: &'a D,
    text: &'a [T],
}

impl<'a, D: Diagnostic, T: AsRef<str>> std::fmt::Display for DiagnosticHeader<'a, D, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        display_header(f, self.diagnostic, self.text)
    }
}

pub struct DiagnosticBody<'a, D: Diagnostic, T> {
    diagnostic: &'a D,
    text: &'a [T],
}

impl<'a, D: Diagnostic, T: AsRef<str> + 'a> std::fmt::Display for DiagnosticBody<'a, D, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        display_body(f, self.diagnostic, self.text)
    }
}

fn display_header<D: Diagnostic>(
    f: &mut impl std::fmt::Write,
    diagnostic: &D,
    text: &[impl AsRef<str>],
) -> std::fmt::Result {
    let severity = D::SEVERITY;
    let color = ansii_esc_color(severity);
    write!(f, "{color}{severity}{ANSII_CLEAR}: ")?;
    diagnostic.description(f)?;
    f.write_char('\n')?;
    let pos = diagnostic.span().start;
    let line_nr = pos.line + 1;
    let char = text[pos.line as usize].as_ref()[0..pos.char as usize]
        .chars()
        .count();
    write!(f, " {ANSII_COLOR_BLUE}-->{ANSII_CLEAR} {line_nr}:{char}")
}

fn display_body<D: Diagnostic>(
    f: &mut impl std::fmt::Write,
    diagnostic: &D,
    text: &[impl AsRef<str>],
) -> std::fmt::Result {
    let span = diagnostic.span();
    let start_line = span.start.line as usize;
    let end_line = span.end.line as usize + 1;
    let num_lines = end_line - start_line;
    let color = ansii_esc_color(D::SEVERITY);

    for (i, line) in text[start_line..end_line].iter().enumerate() {
        let line_nr = start_line + i + 1;
        write!(f, "{ANSII_COLOR_BLUE}{line_nr:4} |{ANSII_CLEAR} ")?;

        let line = line.as_ref().trim_end_matches('\r');
        let mut next_start = 0;
        for (j, c) in line.char_indices() {
            match c {
                // backspace
                '\u{8}' |
                // vertical tab
                '\u{B}' |
                // form feed
                '\u{C}' |
                // carriage return
                '\r' |
                // delete
                '\x7f' => {
                    f.write_str(&line[next_start..j])?;
                    next_start = j + 1;
                }
                _ => (),
            }
        }
        f.write_str(&line[next_start..])?;
        f.write_char('\n')?;

        let col_start = if i == 0 { span.start.char as usize } else { 0 };
        let col_end = if i == num_lines - 1 {
            span.end.char as usize
        } else {
            line.len()
        };
        let num_spaces = line[0..col_start].width();
        write!(
            f,
            "     {ANSII_COLOR_BLUE}|{ANSII_CLEAR} {:num_spaces$}",
            ""
        )?;

        let spanned_text = &line[col_start..col_end];
        let num_carets = spanned_text.width().max(1);
        write!(f, "{color}{:^<num_carets$}", "")?;
        if i == num_lines - 1 {
            f.write_char(' ')?;
            diagnostic.annotation(f)?;
        }
        writeln!(f, "{ANSII_CLEAR}")?;
    }

    Ok(())
}

const ANSII_CLEAR: &str = "\x1b[0m";
const ANSII_COLOR_RED: &str = "\x1b[91m";
const ANSII_COLOR_YELLOW: &str = "\x1b[93m";
const ANSII_COLOR_BLUE: &str = "\x1b[94m";

fn ansii_esc_color(severity: Severity) -> &'static str {
    match severity {
        Severity::Error => ANSII_COLOR_RED,
        Severity::Warning => ANSII_COLOR_YELLOW,
        Severity::Hint => ANSII_COLOR_BLUE,
    }
}
