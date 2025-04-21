use lsp_server::ResponseError;
use lsp_types::request::{self, Request as _};

use crate::{State, VfsDocumentData, VfsPath};

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
    _params: serde_json::Value,
) -> Result<serde_json::Value, RequestError> {
    #[rustfmt::skip]
    let res = match method.as_str() {
        request::Shutdown::METHOD => {
            state.shutdown = true;
            Ok(serde_json::Value::Null)
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

fn wrap_request<R>(
    state: &mut State,
    value: serde_json::Value,
    handler: fn(&mut State, R::Params) -> Result<R::Result, RequestError>,
) -> Result<serde_json::Value, RequestError>
where
    R: lsp_types::request::Request,
{
    let params = serde_json::from_value(value).map_err(|e| {
        invalid_params_response(format!("invalid request params for `{}`: {e}", R::METHOD))
    })?;
    handler(state, params).map(|result| serde_json::to_value(result).unwrap())
}

fn to_path(uri: &lsp_types::Url) -> Result<VfsPath, RequestError> {
    VfsPath::try_from(uri).map_err(invalid_params_error)
}

fn get_document<'a>(state: &'a State, path: &VfsPath) -> Result<&'a VfsDocumentData, RequestError> {
    state
        .mem_docs
        .get(path)
        .ok_or_else(|| invalid_params_response(format!("text document not found `{path}`")).into())
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
