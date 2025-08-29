use std::str::FromStr;

use common::Source;
use common::diagnostic::{Diagnostic, DiagnosticHint, Severity};
use ide::IdeDiagnostics;

use crate::edit::{OffsetEncoding, SpanExt};

pub mod refs;

pub fn generate_diagnostics(
    source: &Source<'_>,
    diagnostics: &IdeDiagnostics,
    encoding: OffsetEncoding,
) -> Vec<lsp_types::Diagnostic> {
    let mut acc = Vec::with_capacity(diagnostics.len());
    generate(&mut acc, source, &diagnostics.errors, encoding);
    generate(&mut acc, source, &diagnostics.warnings, encoding);
    generate(&mut acc, source, &diagnostics.infos, encoding);
    acc
}

fn generate<D: Diagnostic>(
    acc: &mut Vec<lsp_types::Diagnostic>,
    source: &Source<'_>,
    diagnostics: &[D],
    encoding: OffsetEncoding,
) {
    for d in diagnostics {
        acc.push(lsp_types::Diagnostic {
            range: d.span().to_lsp_range(source, encoding),
            severity: Some(lsp_severity(D::SEVERITY)),
            code: None,
            code_description: None,
            source: None,
            message: {
                let mut buf = String::new();
                d.description(&mut buf).unwrap();
                buf
            },
            related_information: d.hint().map(|hint| {
                vec![lsp_types::DiagnosticRelatedInformation {
                    location: lsp_types::Location::new(
                        {
                            // FIXME: store VfsPath in source
                            let uri = format!("file://{}", source.path);
                            lsp_types::Url::from_str(&uri).expect("source path to be valid")
                        },
                        hint.span().to_lsp_range(source, encoding),
                    ),
                    message: {
                        let mut buf = String::new();
                        hint.annotation(&mut buf).unwrap();
                        buf
                    },
                }]
            }),
            tags: None,
            data: None,
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
