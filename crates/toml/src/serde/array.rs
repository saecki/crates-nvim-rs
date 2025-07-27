use crate::map::{MapArrayToplevelEntry, MapNode, PathSegment};
use crate::serde::SerdeError;
use crate::serde::table::TableDeserializer;
use crate::serde::value::ValueDeserializer;

pub struct ArrayDeserializer<I, N>
where
    I: Iterator<Item = N>,
{
    pub iter: I,
}

impl<I, N> ArrayDeserializer<I, N>
where
    I: Iterator<Item = N>,
{
    pub fn new(iter: I) -> Self {
        Self { iter }
    }
}

// impls for `MapNode`
type ArrayItem<'a> = (PathSegment<'a, 'a>, &'a MapNode<'a>);

impl<'de, I> serde::Deserializer<'de> for ArrayDeserializer<I, ArrayItem<'de>>
where
    I: Iterator<Item = ArrayItem<'de>>,
{
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

impl<'de, I> serde::de::SeqAccess<'de> for ArrayDeserializer<I, ArrayItem<'de>>
where
    I: Iterator<Item = ArrayItem<'de>>,
{
    type Error = SerdeError<'de>;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: serde::de::DeserializeSeed<'de>,
    {
        match self.iter.next() {
            Some((segment, node)) => seed
                .deserialize(ValueDeserializer::new(node))
                .map_err(|e| e.with_path(segment))
                .map(Some),
            None => Ok(None),
        }
    }
}

// impls for `MapTable`
type ArrayTableItem<'a> = (usize, &'a MapArrayToplevelEntry<'a>);

impl<'de, I> serde::Deserializer<'de> for ArrayDeserializer<I, ArrayTableItem<'de>>
where
    I: Iterator<Item = ArrayTableItem<'de>>,
{
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

impl<'de, I> serde::de::SeqAccess<'de> for ArrayDeserializer<I, ArrayTableItem<'de>>
where
    I: Iterator<Item = ArrayTableItem<'de>>,
{
    type Error = SerdeError<'de>;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: serde::de::DeserializeSeed<'de>,
    {
        match self.iter.next() {
            Some((idx, entry)) => seed
                .deserialize(TableDeserializer::new(&entry.node))
                .map_err(|e| e.with_array_path(entry.parent, idx))
                .map(Some),
            None => Ok(None),
        }
    }
}
