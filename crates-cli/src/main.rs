use std::process::ExitCode;

use toml::error::{Diagnostic, Severity};
use unicode_width::UnicodeWidthStr;

fn main() -> ExitCode {
    let Some(path) = std::env::args().nth(1) else {
        eprintln!("Missing argument <file>");
        return ExitCode::FAILURE;
    };

    let text = match std::fs::read_to_string(path) {
        Ok(text) => text,
        _ => {
            eprintln!("Error reading from file");
            return ExitCode::FAILURE;
        }
    };
    let lines = text.split('\n').collect::<Vec<_>>();

    let start = std::time::SystemTime::now();
    let mut ctx = toml::Ctx::default();
    let tokens = ctx.lex(&text);
    let lexing = std::time::SystemTime::now();
    let asts = ctx.parse(&tokens);
    let parsing = std::time::SystemTime::now();
    let map = ctx.map(&asts);
    let mapping = std::time::SystemTime::now();
    let simple = toml::map::simple::map_table(map);
    let end = std::time::SystemTime::now();

    println!("{:#?}", simple);
    for error in ctx.errors.iter() {
        println!("{}", error.header(&lines));
        if let Some(hint) = error.hint() {
            print!("{}", hint.display(&lines));
        }
        println!("{}", error.display(&lines));
    }

    let us_lexing = lexing.duration_since(start).unwrap().as_micros();
    let us_parsing = parsing.duration_since(lexing).unwrap().as_micros();
    let us_mapping = mapping.duration_since(parsing).unwrap().as_micros();
    let us_simple = end.duration_since(mapping).unwrap().as_micros();
    let us_total = end.duration_since(start).unwrap().as_micros();

    {
        println!("lexing {}us", us_lexing);
        println!("parsing {}us", us_parsing);
        println!("mapping {}us", us_mapping);
        println!("simple {}us", us_simple);
        println!("total {}us", us_total);
    };

    ExitCode::SUCCESS
}

trait DisplayDiagnostic: Diagnostic + Sized {
    fn header<'a, T>(&'a self, text: &'a [T]) -> DiagnosticHeader<'a, Self, T>;
    fn display<'a, T>(&'a self, text: &'a [T]) -> FmtDiagnostic<'a, Self, T>;
}

impl<D: Diagnostic> DisplayDiagnostic for D {
    fn header<'a, T>(&'a self, text: &'a [T]) -> DiagnosticHeader<'a, D, T> {
        DiagnosticHeader {
            diagnostic: self,
            text,
        }
    }

    fn display<'a, T>(&'a self, text: &'a [T]) -> FmtDiagnostic<'a, D, T> {
        FmtDiagnostic {
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

pub struct FmtDiagnostic<'a, D: Diagnostic, T> {
    diagnostic: &'a D,
    text: &'a [T],
}

impl<'a, D: Diagnostic, T: AsRef<str> + 'a> std::fmt::Display for FmtDiagnostic<'a, D, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        display_in_text(f, self.diagnostic, self.text)
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

fn display_in_text<D: Diagnostic>(
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
        let line = line.as_ref();
        writeln!(f, "{ANSII_COLOR_BLUE}{line_nr:4} |{ANSII_CLEAR} {line}")?;

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
        let num_carrets = spanned_text.width().max(1);
        write!(f, "{color}{:^<num_carrets$}", "")?;
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
