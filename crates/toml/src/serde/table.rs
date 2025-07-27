use common::Span;

use crate::map::{MapIter, MapTable, MapTableEntry};
use crate::serde::SerdeError;
use crate::serde::key::KeyDeserializer;
use crate::serde::table_enum::TableEnumDeserializer;
use crate::serde::value::ValueDeserializer;

pub struct TableDeserializer<'a> {
    pub table: &'a MapTable<'a>,
}

impl<'a> TableDeserializer<'a> {
    pub fn new(table: &'a MapTable<'a>) -> Self {
        Self { table }
    }
}

impl<'de> serde::Deserializer<'de> for TableDeserializer<'de> {
    type Error = SerdeError<'de>;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        let table = self.table;
        visitor
            .visit_map(TableMapAccess::new(self.table))
            .map_err(|e| {
                let span = Span::across(table.reprs.first().span(), table.reprs.last().span());
                e.with_span(span)
            })
    }

    // `None` is interpreted as a missing field so be sure to implement `Some`
    // as a present field.
    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_some(self)
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    // Called when the type to deserialize is an enum, as opposed to a field in the type.
    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        if self.table.is_empty() {
            let msg = "expected exactly 1 element, found 0 elements";
            Err(SerdeError::spanned(msg, self.table.reprs.first().span()))
        } else if self.table.len() != 1 {
            let msg = "expected exactly 1 element, found more than 1 element";
            let span = Span::across(
                self.table.reprs.first().span(),
                self.table.reprs.last().span(),
            );
            Err(SerdeError::spanned(msg, span))
        } else {
            let table = self.table;
            visitor
                .visit_enum(TableMapAccess::new(self.table))
                .map_err(|e| {
                    let span = Span::across(table.reprs.first().span(), table.reprs.last().span());
                    e.with_span(span)
                })
        }
    }

    serde::forward_to_deserialize_any! {
        bool u8 u16 u32 u64 i8 i16 i32 i64 f32 f64 char str string unit seq
        bytes byte_buf map unit_struct tuple_struct struct
        tuple ignored_any identifier
    }
}

pub struct TableMapAccess<'a> {
    table: &'a MapTable<'a>,
    iter: MapIter<'a, 'a>,
    entry: Option<&'a MapTableEntry<'a>>,
}

impl<'a> TableMapAccess<'a> {
    pub fn new(table: &'a MapTable<'a>) -> Self {
        Self {
            table,
            iter: table.iter(),
            entry: None,
        }
    }
}

impl<'de> serde::de::MapAccess<'de> for TableMapAccess<'de> {
    type Error = SerdeError<'de>;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: serde::de::DeserializeSeed<'de>,
    {
        let Some((key, entry)) = self.iter.next() else {
            return Ok(None);
        };

        self.entry = Some(entry);

        let key = seed.deserialize(KeyDeserializer::new(key)).map_err(|e| {
            let span = entry.reprs.first().key.repr_ident().lit_span();
            e.with_span(span).with_table_path(&entry.reprs)
        })?;
        Ok(Some(key))
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::DeserializeSeed<'de>,
    {
        let Some(entry) = self.entry.take() else {
            unreachable!("no more values in next_value_seed");
        };
        seed.deserialize(ValueDeserializer::new(&entry.node))
            .map_err(|e| {
                let span = Span::across(
                    entry.reprs.first().repr_span(),
                    entry.reprs.last().repr_span(),
                );
                e.with_span(span).with_table_path(&entry.reprs)
            })
    }
}

impl<'de> serde::de::EnumAccess<'de> for TableMapAccess<'de> {
    type Error = SerdeError<'de>;

    type Variant = TableEnumDeserializer<'de>;

    fn variant_seed<V>(mut self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: serde::de::DeserializeSeed<'de>,
    {
        let Some((key, entry)) = self.iter.next() else {
            let msg = "expected table with exactly 1 entry, found empty table";
            return Err(SerdeError::spanned(msg, self.table.reprs.first().span()));
        };

        let key = seed.deserialize(KeyDeserializer::new(key)).map_err(|e| {
            let span = entry.reprs.first().key.repr_ident().lit_span();
            e.with_span(span).with_table_path(&entry.reprs)
        })?;

        let variant = TableEnumDeserializer::new(entry);

        Ok((key, variant))
    }
}
