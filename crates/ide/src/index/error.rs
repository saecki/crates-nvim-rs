use common::FmtStr;
use http_req::response::StatusCode;

#[derive(Debug)]
pub struct Error {
    pub name: FmtStr,
    pub url: FmtStr,
    pub kind: ErrorKind,
}

#[derive(Debug)]
pub enum ErrorKind {
    Request(http_req::error::Error),
    NotFound,
    Status(StatusCode),
    Utf8(std::string::FromUtf8Error),
    Json(serde_json::Error),
    Parse(ParseError),
}

#[derive(Debug)]
pub enum ParseError {
    NoVersion,
    MismatchedNames,
}
