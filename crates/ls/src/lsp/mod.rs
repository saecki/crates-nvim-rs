use common::diagnostic::{Diagnostic, Severity};
use ide::IdeDiagnostics;
use toml::Toml;

use crate::edit::{OffsetEncoding, SpanExt};

pub fn generate_diagnostics(
    toml: &Toml,
    diagnostics: &IdeDiagnostics,
    encoding: OffsetEncoding,
) -> Vec<lsp_types::Diagnostic> {
    let mut acc = Vec::with_capacity(diagnostics.len());
    generate(&mut acc, toml, &diagnostics.errors, encoding);
    generate(&mut acc, toml, &diagnostics.warnings, encoding);
    generate(&mut acc, toml, &diagnostics.infos, encoding);
    acc
}

fn generate<D: Diagnostic>(
    acc: &mut Vec<lsp_types::Diagnostic>,
    toml: &Toml,
    diagnostics: &[D],
    encoding: OffsetEncoding,
) {
    for d in diagnostics {
        acc.push(lsp_types::Diagnostic {
            range: d.span().to_lsp_range(&toml.ast.source, encoding),
            severity: Some(lsp_severity(D::SEVERITY)),
            code: None,
            code_description: None,
            source: None,
            message: {
                let mut buf = String::new();
                d.description(&mut buf);
                buf
            },
            related_information: d.hint().map(|hint| {
                vec![lsp_types::DiagnosticRelatedInformation {
                    location: todo!("store file id or similar"),
                    message: {
                        let mut buf = String::new();
                        d.annotation(&mut buf);
                        buf
                    },
                }]
            }),
            tags: todo!(),
            data: todo!(),
        });
    }
}

fn lsp_severity(severity: Severity) -> lsp_types::DiagnosticSeverity {
    match severity {
        Severity::Error => lsp_types::DiagnosticSeverity::ERROR,
        Severity::Warning => lsp_types::DiagnosticSeverity::WARNING,
        Severity::Info => lsp_types::DiagnosticSeverity::INFORMATION,
        Severity::Hint => lsp_types::DiagnosticSeverity::HINT,
    }
}
