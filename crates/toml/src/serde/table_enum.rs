use common::{FmtStr, Span};

use crate::map::{FmtIdent, MapArray, MapNode, MapTableEntry, PathSegment};
use crate::serde::SerdeError;
use crate::serde::array::ArrayDeserializer;
use crate::serde::value::ValueDeserializer;

/// Deserializes table values into enum variants.
pub struct TableEnumDeserializer<'a> {
    pub entry: &'a MapTableEntry<'a>,
}

impl<'a> TableEnumDeserializer<'a> {
    pub(crate) fn new(entry: &'a MapTableEntry<'a>) -> Self {
        TableEnumDeserializer { entry }
    }
}

impl<'de> serde::de::VariantAccess<'de> for TableEnumDeserializer<'de> {
    type Error = SerdeError<'de>;

    fn unit_variant(self) -> Result<(), Self::Error> {
        match &self.entry.node {
            MapNode::Table(table) => {
                if table.is_empty() {
                    Ok(())
                } else {
                    let span = self.entry.reprs.first().repr_span();
                    Err(SerdeError::spanned("expected empty table", span))
                }
            }
            MapNode::Array(MapArray::Toplevel(_)) => {
                let span = self.entry.reprs.first().repr_span();
                Err(SerdeError::spanned("expected empty array", span))
            }
            MapNode::Array(MapArray::Inline(array)) => {
                if array.is_empty() {
                    Ok(())
                } else {
                    let span = self.entry.reprs.first().repr_span();
                    Err(SerdeError::spanned("expected empty array", span))
                }
            }
            MapNode::Scalar(scalar) => {
                let msg = format!("expected table, found {}", scalar.datatype());
                let span = self.entry.reprs.first().repr_span();
                Err(SerdeError::spanned(FmtStr::from_string(msg), span))
            }
        }
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: serde::de::DeserializeSeed<'de>,
    {
        seed.deserialize(ValueDeserializer::new(&self.entry.node))
    }

    fn tuple_variant<V>(self, len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match &self.entry.node {
            MapNode::Table(table) => {
                for (idx, (key, entry)) in table.iter().enumerate() {
                    if key.parse::<usize>().is_ok_and(|i| i == idx) {
                        continue;
                    }

                    let msg = format!("expected table key `{idx}`, but was `{}`", FmtIdent(key));
                    let span = entry.reprs.first().key.repr_ident().lit_span();
                    return Err(SerdeError::spanned(FmtStr::from_string(msg), span));
                }

                if table.len() == len {
                    let iter = table
                        .iter()
                        .map(|(_, e)| (PathSegment::Table(&e.reprs), &e.node));
                    serde::de::Deserializer::deserialize_seq(ArrayDeserializer::new(iter), visitor)
                } else {
                    let msg = format!("expected tuple with length {len}");
                    let span = Span::across(table.reprs.first().span(), table.reprs.last().span());
                    Err(SerdeError::spanned(FmtStr::from_string(msg), span))
                }
            }
            MapNode::Array(MapArray::Toplevel(array)) => {
                if array.len() == len {
                    serde::de::Deserializer::deserialize_seq(
                        ArrayDeserializer::new(array.iter().enumerate()),
                        visitor,
                    )
                } else {
                    let msg = format!("expected tuple with length {len}");
                    let span = Span::across(array.first().repr.span(), array.last().repr.span());
                    Err(SerdeError::spanned(FmtStr::from_string(msg), span))
                }
            }
            MapNode::Array(MapArray::Inline(array)) => {
                if array.len() == len {
                    let iter = (array.iter().enumerate())
                        .map(|(idx, e)| (PathSegment::Array(array.parent, idx), &e.node));
                    serde::de::Deserializer::deserialize_seq(ArrayDeserializer::new(iter), visitor)
                } else {
                    let msg = format!("expected tuple with length {len}");
                    let span = array.repr.span();
                    Err(SerdeError::spanned(FmtStr::from_string(msg), span))
                }
            }
            MapNode::Scalar(scalar) => {
                let msg = format!("expected table, found {}", scalar.datatype());
                Err(SerdeError::spanned(FmtStr::from_string(msg), scalar.span()))
            }
        }
    }

    fn struct_variant<V>(
        self,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        serde::de::Deserializer::deserialize_struct(
            ValueDeserializer::new(&self.entry.node).with_struct_key_validation(),
            "",
            fields,
            visitor,
        )
    }
}
