use std::collections::HashMap;
use std::str::FromStr;

use common::Source;
use ide::IdeDiagnostics;
use lsp_server::{Connection, Message, Response};
use lsp_types::notification::Notification as _;
use lsp_types::{
    InitializeParams, InitializeResult, PublishDiagnosticsClientCapabilities, ServerCapabilities,
    ServerInfo, TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    TextDocumentSyncSaveOptions,
};

use crate::edit::OffsetEncoding;
use crate::notif::NotificationError;
use crate::request::RequestError;

pub mod edit;
pub mod lsp;
pub mod notif;
pub mod request;

struct State {
    connection: Connection,
    mem_docs: HashMap<VfsPath, VfsDocumentData>,
    offset_encoding: OffsetEncoding,
    shutdown: bool,
}

impl State {
    pub fn new(connection: Connection, offset_encoding: OffsetEncoding) -> Self {
        Self {
            connection,
            mem_docs: HashMap::new(),
            offset_encoding,
            shutdown: false,
        }
    }
}

/// An absolute path that must be valid utf-8.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct VfsPath(Box<str>);

impl std::fmt::Display for VfsPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl TryFrom<&lsp_types::Url> for VfsPath {
    type Error = anyhow::Error;

    fn try_from(value: &lsp_types::Url) -> Result<Self, Self::Error> {
        let uri = value.as_str();
        let path = uri
            .strip_prefix("file://")
            .ok_or_else(|| anyhow::anyhow!("expected a uri prefixed with `file://`: `{uri}`"))?;
        if !path.starts_with("/") {
            anyhow::bail!("expected an absolute path: `{uri}`");
        }

        Ok(VfsPath(path.into()))
    }
}

impl VfsPath {
    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn to_uri(&self) -> lsp_types::Url {
        let uri = format!("file://{}", self.as_str());
        lsp_types::Url::from_str(&uri).expect("VfsPath to be a valid path")
    }
}

pub struct VfsDocumentData {
    pub version: i32,
    pub diagnostics: IdeDiagnostics,
    pub toml: toml::Container,
}

impl VfsDocumentData {
    pub fn new(version: i32, diagnostics: IdeDiagnostics, toml: toml::Container) -> Self {
        Self {
            version,
            diagnostics,
            toml,
        }
    }

    pub fn source(&self) -> &Source<'_> {
        &self.toml.toml().ast.source
    }
}

pub fn run() -> anyhow::Result<()> {
    let (connection, io_threads) = Connection::stdio();

    let (id, params) = connection.initialize_start()?;

    let InitializeParams {
        capabilities: client_capabilities,
        ..
    } = serde_json::from_value(params)?;

    let offset_encoding = edit::negotiated_encoding(&client_capabilities);

    // FIXME: not a hard requirement, just check before publishing diagnostics
    assert!(matches!(
        client_capabilities
            .text_document
            .unwrap()
            .publish_diagnostics
            .unwrap(),
        PublishDiagnosticsClientCapabilities {
            version_support: _,
            related_information: Some(true),
            tag_support: _,
            code_description_support: _,
            data_support: _,
        }
    ));

    let server_capabilities = ServerCapabilities {
        position_encoding: Some(offset_encoding.position_encoding()),
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::INCREMENTAL),
                will_save: Some(false),
                will_save_wait_until: Some(false),
                save: Some(TextDocumentSyncSaveOptions::Supported(false)),
            },
        )),
        ..Default::default()
    };

    let initialize_result = InitializeResult {
        capabilities: server_capabilities,
        server_info: Some(ServerInfo {
            name: "vvm-ls".into(),
            version: Some("0.1.0".into()),
        }),
    };
    let value = serde_json::to_value(initialize_result)?;
    connection.initialize_finish(id, value)?;

    let mut state = State::new(connection, offset_encoding);

    loop {
        let msg = state.connection.receiver.recv()?;
        if matches!(
            &msg,
            Message::Notification(lsp_server::Notification { method, ..  })
            if method == lsp_types::notification::Exit::METHOD
        ) {
            break;
        }
        handle_message(&mut state, msg)?;
    }

    drop(state);
    io_threads.join()?;

    Ok(())
}

fn handle_message(state: &mut State, msg: Message) -> anyhow::Result<()> {
    match msg {
        Message::Request(req) if state.shutdown => {
            let resp = Response::new_err(
                req.id,
                lsp_server::ErrorCode::InvalidRequest as i32,
                "Already shutting down".into(),
            );
            state.connection.sender.send(Message::Response(resp))?;
            Ok(())
        }
        Message::Request(req) => {
            let res = request::handle_request(state, req.method, req.params);
            match res {
                Ok(result) => {
                    state.connection.sender.send(Message::Response(Response {
                        id: req.id,
                        result: Some(result),
                        error: None,
                    }))?;
                    Ok(())
                }
                Err(RequestError::Response(err)) => {
                    state.connection.sender.send(Message::Response(Response {
                        id: req.id,
                        result: None,
                        error: Some(err),
                    }))?;
                    Ok(())
                }
                Err(RequestError::Fatal(err)) => Err(err),
                Err(RequestError::Ignored(err)) => {
                    // TODO: log
                    eprintln!("ignored error: {err}");
                    Ok(())
                }
            }
        }
        Message::Response(_resp) => {
            // TODO: store request ids and look up request
            todo!()
        }
        Message::Notification(n) => {
            let res = notif::handle_notification(state, n.method, n.params);
            match res {
                Ok(()) => Ok(()),
                Err(NotificationError::Fatal(err)) => Err(err),
                Err(NotificationError::Ignored(err)) => {
                    // TODO: log
                    eprintln!("ignored error: {err}");
                    Ok(())
                }
            }
        }
    }
}
