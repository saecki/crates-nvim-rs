use common::Span;
use serde::de::IntoDeserializer as _;

use crate::MapTable;
use crate::map::{MapArray, MapNode, PathSegment, Scalar};
use crate::serde::SerdeError;
use crate::serde::array::ArrayDeserializer;
use crate::serde::datetime::DateTimeDeserializer;
use crate::serde::table::TableDeserializer;

pub struct ValueDeserializer<'a> {
    node: &'a MapNode<'a>,
    validate_struct_keys: bool,
}

impl<'a> ValueDeserializer<'a> {
    pub fn new(node: &'a MapNode<'a>) -> Self {
        Self {
            node,
            validate_struct_keys: false,
        }
    }

    pub fn with_struct_key_validation(mut self) -> Self {
        self.validate_struct_keys = true;
        self
    }
}

impl<'de> serde::de::Deserializer<'de> for ValueDeserializer<'de> {
    type Error = SerdeError<'de>;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.node {
            MapNode::Table(table) => TableDeserializer::new(table).deserialize_any(visitor),
            MapNode::Array(MapArray::Inline(array)) => {
                let iter = (array.iter().enumerate())
                    .map(|(idx, e)| (PathSegment::Array(array.parent, idx), &e.node));
                ArrayDeserializer::new(iter).deserialize_any(visitor)
            }
            MapNode::Array(MapArray::Toplevel(array)) => {
                ArrayDeserializer::new(array.iter().enumerate()).deserialize_any(visitor)
            }
            MapNode::Scalar(Scalar::String(string)) => visitor
                .visit_borrowed_str(string.text)
                .map_err(|e| SerdeError::with_span(e, string.lit_span)),
            MapNode::Scalar(Scalar::Int(int)) => visitor
                .visit_i64(int.val)
                .map_err(|e| SerdeError::with_span(e, int.lit_span)),
            MapNode::Scalar(Scalar::Float(float)) => visitor
                .visit_f64(float.val)
                .map_err(|e| SerdeError::with_span(e, float.lit_span)),
            MapNode::Scalar(Scalar::Bool(bool)) => visitor
                .visit_bool(bool.val)
                .map_err(|e| SerdeError::with_span(e, bool.lit_span)),
            MapNode::Scalar(Scalar::DateTime(date)) => visitor
                .visit_map(DateTimeDeserializer::new(date))
                .map_err(|mut e| {
                    const BAD_MSG: &str = "invalid type: map";
                    const GOOD_MSG: &str = "invalid type: date-time";
                    if e.msg.starts_with(BAD_MSG) {
                        e.msg.replace_range(0..BAD_MSG.len(), GOOD_MSG);
                    }
                    SerdeError::with_span(e, date.lit_span)
                }),
            MapNode::Scalar(Scalar::Invalid(span)) => {
                Err(SerdeError::spanned("encountered invalid value", **span))
            }
        }
    }

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

    fn deserialize_enum<V>(
        self,
        name: &'static str,
        variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        let span = match self.node {
            MapNode::Table(table) => {
                return TableDeserializer::new(table).deserialize_enum(name, variants, visitor);
            }
            MapNode::Scalar(Scalar::String(str)) => {
                return visitor.visit_enum(str.text.into_deserializer());
            }
            MapNode::Array(MapArray::Toplevel(array)) => array.first().repr.span(),
            MapNode::Array(MapArray::Inline(array)) => array.repr.span(),
            MapNode::Scalar(scalar) => scalar.span(),
        };
        Err(SerdeError::spanned("expected string or table", span))
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        if self.validate_struct_keys
            && let MapNode::Table(table) = self.node
        {
            // TODO: possibly use name in error message if serde doesn't provide
            // that context on its own.
            validate_struct_keys(table, fields)?;
        }

        self.deserialize_any(visitor)
    }

    serde::forward_to_deserialize_any! {
        bool u8 u16 u32 u64 i8 i16 i32 i64 f32 f64 char str string unit seq
        bytes byte_buf map unit_struct tuple_struct
        tuple ignored_any identifier
    }
}

fn validate_struct_keys<'a>(
    table: &'a MapTable<'a>,
    fields: &[&str],
) -> Result<(), SerdeError<'a>> {
    let extra_fields = table
        .iter()
        .filter(|(key, _)| !fields.contains(key))
        .map(|(key, _)| *key)
        .collect::<Vec<_>>();

    if extra_fields.is_empty() {
        return Ok(());
    }

    let msg = format!(
        "unexpected keys in table: {}, available keys: {}",
        extra_fields.join(", "),
        fields.join(", "),
    );
    let span = Span::across(table.reprs.first().span(), table.reprs.last().span());
    Err(SerdeError::spanned(msg, span))
}
