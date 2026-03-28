use crate::MapTable;
use crate::map::{MapArrayInline, MapArrayToplevel};
use crate::serde::SerdeError;
use crate::serde::table::TableDeserializer;
use crate::serde::value::ValueDeserializer;

pub struct ToplevelArrayDeserializer<'a> {
    pub array: &'a MapArrayToplevel<'a>,
    pub idx: usize,
}

impl<'a> ToplevelArrayDeserializer<'a> {
    pub fn new(array: &'a MapArrayToplevel<'a>) -> Self {
        Self { array, idx: 0 }
    }
}

impl<'de> serde::Deserializer<'de> for ToplevelArrayDeserializer<'de> {
    type Error = SerdeError<'de>;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_seq(self)
    }

    serde::forward_to_deserialize_any! {
        bool u8 u16 u32 u64 i8 i16 i32 i64 f32 f64 char str string seq
        bytes byte_buf map option unit newtype_struct struct
        ignored_any unit_struct tuple_struct tuple enum identifier
    }
}

impl<'de> serde::de::SeqAccess<'de> for ToplevelArrayDeserializer<'de> {
    type Error = SerdeError<'de>;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: serde::de::DeserializeSeed<'de>,
    {
        let Some(entry) = self.array.as_slice().get(self.idx) else {
            return Ok(None);
        };
        self.idx += 1;
        seed.deserialize(TableDeserializer::new(entry.node.get()))
            .map_err(|e| e.with_toplevel_array_parent(entry))
            .map(Some)
    }
}

pub struct InlineArrayDeserializer<'a> {
    pub array: &'a MapArrayInline<'a>,
    pub idx: usize,
}

impl<'a> InlineArrayDeserializer<'a> {
    pub fn new(array: &'a MapArrayInline<'a>) -> Self {
        Self { array, idx: 0 }
    }
}

impl<'de> serde::Deserializer<'de> for InlineArrayDeserializer<'de> {
    type Error = SerdeError<'de>;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_seq(self)
    }

    serde::forward_to_deserialize_any! {
        bool u8 u16 u32 u64 i8 i16 i32 i64 f32 f64 char str string seq
        bytes byte_buf map option unit newtype_struct struct
        ignored_any unit_struct tuple_struct tuple enum identifier
    }
}

impl<'de> serde::de::SeqAccess<'de> for InlineArrayDeserializer<'de> {
    type Error = SerdeError<'de>;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: serde::de::DeserializeSeed<'de>,
    {
        let Some(entry) = self.array.as_slice().get(self.idx) else {
            return Ok(None);
        };
        self.idx += 1;
        seed.deserialize(ValueDeserializer::new(&entry.node))
            .map_err(|e| e.with_inline_array_parent(entry))
            .map(Some)
    }
}

pub struct TableTupleDeserializer<'a> {
    pub tuple: &'a MapTable<'a>,
    pub idx: usize,
}

impl<'a> TableTupleDeserializer<'a> {
    pub fn new(tuple: &'a MapTable<'a>) -> Self {
        Self { tuple, idx: 0 }
    }
}

impl<'de> serde::Deserializer<'de> for TableTupleDeserializer<'de> {
    type Error = SerdeError<'de>;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_seq(self)
    }

    serde::forward_to_deserialize_any! {
        bool u8 u16 u32 u64 i8 i16 i32 i64 f32 f64 char str string seq
        bytes byte_buf map option unit newtype_struct struct
        ignored_any unit_struct tuple_struct tuple enum identifier
    }
}

impl<'de> serde::de::SeqAccess<'de> for TableTupleDeserializer<'de> {
    type Error = SerdeError<'de>;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: serde::de::DeserializeSeed<'de>,
    {
        let Some((_, entry)) = self.tuple.as_slice().get_index(self.idx) else {
            return Ok(None);
        };
        self.idx += 1;
        seed.deserialize(ValueDeserializer::new(&entry.node))
            .map_err(|e| e.with_table_parent(entry))
            .map(Some)
    }
}
