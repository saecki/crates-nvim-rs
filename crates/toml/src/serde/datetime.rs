use common::FmtStr;
use serde::de::value::BorrowedStrDeserializer;

use crate::datetime::DateTime;
use crate::parse::DateTimeVal;
use crate::serde::SerdeError;

pub(crate) struct DateTimeDeserializer<'a> {
    pub datetime: &'a DateTimeVal,
}

impl<'a> DateTimeDeserializer<'a> {
    pub fn new(datetime: &'a DateTimeVal) -> Self {
        Self { datetime }
    }
}

impl<'de> serde::de::EnumAccess<'de> for DateTimeDeserializer<'de> {
    type Error = SerdeError<'de>;

    type Variant = DateTimeEnumDeserializer<'de>;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: serde::de::DeserializeSeed<'de>,
    {
        let key_str = match self.datetime.val {
            DateTime::OffsetDateTime(..) => "OffsetDateTime",
            DateTime::LocalDateTime(..) => "LocalDateTime",
            DateTime::LocalDate(..) => "LocalDate",
            DateTime::LocalTime(..) => "LocalTime",
        };
        let key = seed.deserialize(BorrowedStrDeserializer::new(key_str))?;

        let val = DateTimeEnumDeserializer::new(self.datetime);

        Ok((key, val))
    }
}

pub(crate) struct DateTimeEnumDeserializer<'a> {
    pub datetime: &'a DateTimeVal,
}

impl<'a> DateTimeEnumDeserializer<'a> {
    pub fn new(datetime: &'a DateTimeVal) -> Self {
        Self { datetime }
    }
}

#[macro_export]
macro_rules! const_assert {
    ($x:expr $(,)?) => {
        #[allow(unknown_lints, clippy::eq_op)]
        const _: [(); 0 - !{
            const ASSERT: bool = $x;
            ASSERT
        } as usize] = [];
    };
}

impl<'de> serde::de::VariantAccess<'de> for DateTimeEnumDeserializer<'de> {
    type Error = SerdeError<'de>;

    fn unit_variant(self) -> Result<(), Self::Error> {
        Err(SerdeError::spanned(
            "expected empty table, found TOML date-time",
            self.datetime.lit_span,
        ))
    }

    fn newtype_variant_seed<T>(self, _seed: T) -> Result<T::Value, Self::Error>
    where
        T: serde::de::DeserializeSeed<'de>,
    {
        Err(SerdeError::spanned(
            "expected newtype, found TOML date-time",
            self.datetime.lit_span,
        ))
    }

    fn tuple_variant<V>(self, len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        if len != 1 {
            let msg = format!("expected tuple with length {len}, found TOML date-time");
            return Err(SerdeError::spanned(
                FmtStr::from_string(msg),
                self.datetime.lit_span,
            ));
        }

        let mut buf: u128 = 0;

        // SAFETY: The transmute below guarantees, that `u128` and `DateTime`
        // are the same size (16 bytes). And `u128` has an alignment of 16,
        // which is at least that of `DateTime`.
        let ptr = &raw mut buf as *mut DateTime;
        unsafe { *ptr = self.datetime.val };

        visitor.visit_u128(buf)
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(SerdeError::spanned(
            "expected struct variant, found TOML date-time",
            self.datetime.lit_span,
        ))
    }
}

impl<'de> serde::de::Deserialize<'de> for DateTime {
    fn deserialize<D>(deserializer: D) -> Result<DateTime, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        struct DateTimeVisitor;

        impl<'de> serde::de::Visitor<'de> for DateTimeVisitor {
            type Value = DateTime;

            fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                formatter.write_str("a TOML date-time")
            }

            fn visit_enum<A>(self, visitor: A) -> Result<DateTime, A::Error>
            where
                A: serde::de::EnumAccess<'de>,
            {
                let (d, _): (DateTimeFromU128, _) = visitor.variant()?;
                Ok(d.value)
            }
        }

        deserializer.deserialize_str(DateTimeVisitor)
    }
}

#[doc(hidden)]
#[cfg(feature = "serde")]
struct DateTimeFromU128 {
    pub value: DateTime,
}

#[cfg(feature = "serde")]
impl<'de> serde::de::Deserialize<'de> for DateTimeFromU128 {
    fn deserialize<D>(deserializer: D) -> Result<DateTimeFromU128, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        struct Visitor;

        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = DateTimeFromU128;

            fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                formatter.write_str("u128 that represents a TOML date-time")
            }

            fn visit_u128<E>(self, s: u128) -> Result<DateTimeFromU128, E>
            where
                E: serde::de::Error,
            {
                // FIXME: safety
                // SAFETY: This isn't safe :(
                // But if we could somehow forbid using this deserialize
                // implementation with any other deserializer this would be.
                // Because TOML doesn't have support for 128 bit integers, so
                // our deserializer will never call the visit_u128 method of
                // any visitor, if it's not a DateTime.
                let value = unsafe { std::mem::transmute::<u128, DateTime>(s) };

                Ok(DateTimeFromU128 { value })
            }
        }

        deserializer.deserialize_str(Visitor)
    }
}
