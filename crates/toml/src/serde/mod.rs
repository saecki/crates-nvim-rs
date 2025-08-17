use serde::Deserialize;

use crate::container::Toml;
use crate::serde::table::TableDeserializer;

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
