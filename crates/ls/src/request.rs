use common::Pos;
use lsp_server::ResponseError;
use lsp_types::request::{self, Request as _};
use toml::Toml;

use crate::edit::{self, OffsetEncoding};
use crate::{State, VfsPath, lsp};

pub enum RequestError {
    /// Respond to the client with a response error.
    Response(lsp_server::ResponseError),
    /// Exit the server, fatal error.
    Fatal(anyhow::Error),
    /// Log the error and continue.
    Ignored(anyhow::Error),
}

impl From<lsp_server::ResponseError> for RequestError {
    fn from(value: lsp_server::ResponseError) -> Self {
        Self::Response(value)
    }
}

pub(crate) fn handle_request(
    state: &mut State,
    method: String,
    params: serde_json::Value,
) -> Result<serde_json::Value, RequestError> {
    #[rustfmt::skip]
    let res = match method.as_str() {
        request::Shutdown::METHOD => {
            state.shutdown = true;
            Ok(serde_json::Value::Null)
        }

        request::DocumentHighlightRequest::METHOD => {
            wrap_request::<request::DocumentHighlightRequest>(state, params, handle_document_highlight)
        }

        _ if method.starts_with("$/") => {
            eprintln!("implementation specific request method: `{method}`");
            Err(RequestError::Response(
                lsp_server::ResponseError {
                    code: lsp_server::ErrorCode::MethodNotFound as i32,
                    message: String::new(),
                    data: None,
                }
            ))
        }

        _ => todo!("handle request method: `{}`", method),
    };
    res
}

fn wrap_request<R: lsp_types::request::Request>(
    state: &mut State,
    value: serde_json::Value,
    handler: fn(&mut State, R::Params) -> Result<R::Result, RequestError>,
) -> Result<serde_json::Value, RequestError> {
    let params = serde_json::from_value(value).map_err(|e| {
        invalid_params_response(format!("invalid request params for `{}`: {e}", R::METHOD))
    })?;
    handler(state, params).map(|result| serde_json::to_value(result).unwrap())
}

fn handle_document_highlight(
    state: &mut State,
    params: lsp_types::DocumentHighlightParams,
) -> Result<Option<Vec<lsp_types::DocumentHighlight>>, RequestError> {
    let (toml, _path, pos) = try_from_pos_params(state, params.text_document_position_params)?;
    Ok(lsp::refs::document_highlight(
        toml,
        pos,
        state.offset_encoding,
    ))
}

fn try_from_pos_params(
    state: &State,
    text_document_position: lsp_types::TextDocumentPositionParams,
) -> Result<(&Toml<'_>, VfsPath, Pos), RequestError> {
    let uri = &text_document_position.text_document.uri;
    let (toml, path) = get_module(state, uri)?;

    let pos = get_text_pos(toml, text_document_position.position, state.offset_encoding)?;

    Ok((toml, path, pos))
}

fn get_module<'a>(
    state: &'a State,
    uri: &lsp_types::Url,
) -> Result<(&'a Toml<'a>, VfsPath), RequestError> {
    let path = to_path(uri)?;
    let Some(doc) = state.mem_docs.get(&path) else {
        return Err(invalid_params_response(format!("text document not found `{path}`")).into());
    };

    Ok((doc.toml.toml(), path))
}

fn get_text_pos(
    toml: &Toml,
    pos: lsp_types::Position,
    encoding: OffsetEncoding,
) -> Result<Pos, RequestError> {
    edit::source_pos(&toml.ast.source, pos, encoding).map_err(invalid_params_error)
}

fn to_path(uri: &lsp_types::Url) -> Result<VfsPath, RequestError> {
    VfsPath::try_from(uri).map_err(invalid_params_error)
}

fn invalid_params_response(message: String) -> ResponseError {
    lsp_server::ResponseError {
        code: lsp_server::ErrorCode::InvalidParams as i32,
        message,
        data: None,
    }
}

fn invalid_params_error(error: anyhow::Error) -> RequestError {
    RequestError::Response(invalid_params_response(error.to_string()))
}
