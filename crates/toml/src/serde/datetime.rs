use serde::de::IntoDeserializer;
use serde::de::value::BorrowedStrDeserializer;

use crate::datetime::DateTime;
use crate::parse::DateTimeVal;
use crate::serde::SerdeError;

pub(crate) struct DateTimeDeserializer<'a> {
    pub datetime: &'a DateTimeVal,
    pub taken: bool,
}

impl<'a> DateTimeDeserializer<'a> {
    pub fn new(datetime: &'a DateTimeVal) -> Self {
        Self {
            datetime,
            taken: false,
        }
    }
}

impl<'de> serde::de::MapAccess<'de> for DateTimeDeserializer<'de> {
    type Error = SerdeError<'de>;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: serde::de::DeserializeSeed<'de>,
    {
        if self.taken {
            return Ok(None);
        }
        self.taken = true;

        let key_str = match self.datetime.val {
            DateTime::OffsetDateTime(..) => "OffsetDateTime",
            DateTime::LocalDateTime(..) => "LocalDateTime",
            DateTime::LocalDate(..) => "LocalDate",
            DateTime::LocalTime(..) => "LocalTime",
        };
        seed.deserialize(BorrowedStrDeserializer::new(key_str))
            .map(Some)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::DeserializeSeed<'de>,
    {
        let mut buf: u128 = 0;

        // SAFETY: The transmute below guarantees, that `u128` and `DateTime`
        // are the same size (16 bytes). And `u128` has an alignment of 16,
        // which is at least that of `DateTime`.
        let ptr = &raw mut buf as *mut DateTime;
        unsafe { *ptr = self.datetime.val };

        seed.deserialize(buf.into_deserializer())
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
                formatter.write_str("a date-time")
            }

            fn visit_map<A>(self, mut visitor: A) -> Result<DateTime, A::Error>
            where
                A: serde::de::MapAccess<'de>,
            {
                let key = visitor.next_key::<DateTimeKey>()?;
                if key.is_none() {
                    return Err(serde::de::Error::custom("date-time key not found"));
                }
                let d: DateTimeFromU128 = visitor.next_value()?;
                Ok(d.value)
            }
        }

        deserializer.deserialize_map(DateTimeVisitor)
    }
}

struct DateTimeKey;

#[cfg(feature = "serde")]
impl<'de> serde::de::Deserialize<'de> for DateTimeKey {
    fn deserialize<D>(deserializer: D) -> Result<DateTimeKey, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        struct FieldVisitor;

        impl<'de> serde::de::Visitor<'de> for FieldVisitor {
            type Value = ();

            fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                formatter.write_str("a valid datetime field")
            }

            fn visit_str<E>(self, s: &str) -> Result<(), E>
            where
                E: serde::de::Error,
            {
                const VARIANTS: [&str; 4] =
                    ["OffsetDateTime", "LocalDateTime", "LocalDate", "LocalTime"];
                if VARIANTS.contains(&s) {
                    Ok(())
                } else {
                    Err(serde::de::Error::custom("expected field with custom name"))
                }
            }
        }

        deserializer.deserialize_identifier(FieldVisitor)?;
        Ok(DateTimeKey)
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
                formatter.write_str("u128 that represents a date-time")
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

        dbg!("here");
        deserializer.deserialize_u128(Visitor)
    }
}
