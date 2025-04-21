use common::Source;
use lsp_server::{Message, Notification};
use lsp_types::notification::{self as notif, Notification as _, PublishDiagnostics};
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    PublishDiagnosticsParams,
};
use toml::TomlDiagnostics;

use crate::{State, VfsDocumentData, VfsPath, edit};

pub enum NotificationError {
    /// Exit the server, fatal error.
    Fatal(anyhow::Error),
    /// Log the error and continue.
    Ignored(anyhow::Error),
}

pub(crate) fn handle_notification(
    state: &mut State,
    method: String,
    params: serde_json::Value,
) -> Result<(), NotificationError> {
    #[rustfmt::skip]
    let res = match method.as_str() {
        notif::DidOpenTextDocument::METHOD => {
            wrap_notification::<notif::DidOpenTextDocument>(state, params, handle_did_open_text_document)
        }
        notif::DidChangeTextDocument::METHOD => {
            wrap_notification::<notif::DidChangeTextDocument>(state, params, handle_did_change_text_document)
        }
        notif::DidCloseTextDocument::METHOD => {
            wrap_notification::<notif::DidCloseTextDocument>(state, params, handle_did_close_text_document)
        }
        notif::WillSaveTextDocument::METHOD => Ok(()),
        notif::DidSaveTextDocument::METHOD => Ok(()),
        notif::DidChangeWatchedFiles::METHOD => Ok(()),
        // TODO: watch workspace files
        notif::DidCreateFiles::METHOD => Ok(()),
        notif::DidRenameFiles::METHOD => Ok(()),
        notif::DidDeleteFiles::METHOD => Ok(()),
        notif::DidChangeWorkspaceFolders::METHOD => Ok(()),

        _ if method.starts_with("$/") => {
            eprintln!("implementation specific notification: `{method}`");
            Ok(())
        }

        _ => {
            eprintln!("unhandled notification method: `{method}`");
            Ok(())
        }
    };
    res
}

fn wrap_notification<N>(
    state: &mut State,
    value: serde_json::Value,
    handler: fn(&mut State, N::Params) -> Result<(), NotificationError>,
) -> Result<(), NotificationError>
where
    N: lsp_types::notification::Notification,
{
    let params = serde_json::from_value(value).map_err(|e| {
        NotificationError::Ignored(anyhow::anyhow!(
            "invalid notification params for `{}`: {e}",
            N::METHOD
        ))
    })?;
    handler(state, params)
}

fn handle_did_open_text_document(
    state: &mut State,
    params: DidOpenTextDocumentParams,
) -> Result<(), NotificationError> {
    let text_doc = params.text_document;

    let path = to_path(&text_doc.uri)?;
    let mut ctx = TomlDiagnostics::default();
    let toml = toml::Container::parse(&mut ctx, &params.text_document.text);
    let doc = VfsDocumentData::new(text_doc.version, text_doc.text, toml);

    state.mem_docs.insert(path.clone(), doc);

    run_analysis(state)?;

    Ok(())
}

fn handle_did_change_text_document(
    state: &mut State,
    params: DidChangeTextDocumentParams,
) -> Result<(), NotificationError> {
    let text_doc = params.text_document;

    let path = to_path(&text_doc.uri)?;
    let Some(doc) = state.mem_docs.get_mut(&path) else {
        return Err(NotificationError::Ignored(anyhow::anyhow!(
            "text document not found"
        )));
    };

    let changes = params.content_changes;
    let text = edit::apply_document_changes(&doc.text, changes, state.offset_encoding)
        .map_err(NotificationError::Ignored)?;

    let source = Source::new(path.as_str(), &text);
    let ast = libvvm_parser::parse(&source);
    *doc = VfsDocumentData::new(text_doc.version, text, ast);

    run_analysis(state)?;

    Ok(())
}

fn handle_did_close_text_document(
    state: &mut State,
    params: DidCloseTextDocumentParams,
) -> Result<(), NotificationError> {
    let path = to_path(&params.text_document.uri)?;
    state.mem_docs.remove(&path);
    Ok(())
}

fn run_analysis(state: &mut State) -> Result<(), NotificationError> {
    // TODO: Maybe run in worker thread.
    // NOTE: If so, make sure ordering of requests is consistent with global analysis state.

    let sources = state.mem_docs.iter().map(|(path, doc)| {
        let name = {
            let idx = path.as_str().rfind('/').expect("path to be absolute");
            let filename = &path.as_str()[idx + 1..];
            filename.strip_suffix(".vvm").expect("a vvm file").into()
        };
        let source = OwnedSource {
            path: path.clone(),
            name,
            text: doc.text.as_str().into(),
            version: doc.version,
        };
        (source, &doc.ast)
    });
    let analysis = analysis::analyze(sources);
    let diagnostics = lsp::diagnostic::generate_diagnostics(&analysis, state.offset_encoding);

    for (module, diagnostics) in diagnostics {
        let source = &analysis.modules.get(module).source;
        state
            .connection
            .sender
            .send(Message::Notification(Notification::new(
                PublishDiagnostics::METHOD.into(),
                PublishDiagnosticsParams {
                    uri: source.path.to_uri(),
                    version: Some(source.version),
                    diagnostics,
                },
            )))
            .unwrap();
    }

    state.analysis = Some(analysis);

    Ok(())
}

fn to_path(uri: &lsp_types::Uri) -> Result<VfsPath, NotificationError> {
    VfsPath::try_from(uri).map_err(NotificationError::Ignored)
}
