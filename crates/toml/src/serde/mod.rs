use bumpalo::Bump;
use common::diagnostic::Diagnostic;
use serde::Deserialize;
use serde::de::DeserializeOwned;

use crate::container::Toml;
use crate::serde::table::TableDeserializer;
use crate::{TomlCtx, TomlDiagnostics};

pub use error::*;

mod array;
mod datetime;
mod error;
mod key;
mod table;
mod table_enum;
mod value;

// TODO: add some tests
pub fn deserialize<'de, T: Deserialize<'de>>(toml: &Toml<'de>) -> Result<T, SerdeError<'de>> {
    let deserializer = TableDeserializer::new(toml.map);
    T::deserialize(deserializer)
}

/// Deserialize a owned value.
pub fn deserialize_owned<T: DeserializeOwned>(text: &str) -> Result<T, String> {
    let mut ctx = TomlDiagnostics::default();
    let bump = Bump::new();
    let text = bump.alloc(text);
    let toml = ctx.parse(&bump, "<serde>", text);
    if !ctx.errors.is_empty() {
        let error = ctx.errors.remove(0);
        let mut msg = String::new();
        error.description(&mut msg).unwrap();
        return Err(msg);
    }
    let deserializer = TableDeserializer::new(toml.map);
    T::deserialize(deserializer).map_err(|error| error.to_string())
}
