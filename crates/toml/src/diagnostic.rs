use common::{diagnostic, Diagnostic, DisplayDiagnostic};

use crate::Error;

// TODO: add `lines` method to `common::Diagnostic` and move this into there
pub fn display_error(
    f: &mut impl std::fmt::Write,
    error: &Error,
    lines: &[&str],
) -> std::fmt::Result {
    writeln!(f, "{}", error.header(&lines))?;
    let context_lines = error.lines().unwrap_or(&[]);

    let mut start_line = 0;
    if let Some(hint) = error.hint() {
        let span = hint.span();
        for &l in context_lines {
            if l < span.start.line {
                diagnostic::display_line(f, l as usize, &lines[l as usize])?;
            } else {
                break;
            }
        }
        write!(f, "{}", hint.body(&lines))?;
        start_line = span.end.line + 1;
    }

    let span = error.span();
    for &l in context_lines {
        if l < start_line {
            continue;
        }
        if l < span.start.line {
            diagnostic::display_line(f, l as usize, &lines[l as usize])?;
        } else {
            break;
        }
    }
    write!(f, "{}", error.body(&lines))?;
    Ok(())
}
