use serde::{Deserialize, Serialize};
use std::ops::Sub;
use tezos_data_encoding::nom::NomReader;
use tezos_data_encoding::{
    enc::BinWriter,
    encoding::{Encoding, HasEncoding},
};
use time::{error::ComponentRange, format_description::well_known::Rfc3339, OffsetDateTime};

/// Helper function to format UNIX (integral) timestamp to RFC3339 string timestamp
pub fn ts_to_rfc3339(ts: i64) -> Result<String, TimestampOutOfRangeError> {
    Ok(OffsetDateTime::from_unix_timestamp(ts)?
        .format(&Rfc3339)
        .unwrap_or_else(|_| String::from("invalid timestamp")))
}

#[derive(Debug, thiserror::Error)]
#[error("invalid or out-of-range datetime")]
pub struct TimestampOutOfRangeError;

impl From<ComponentRange> for TimestampOutOfRangeError {
    fn from(_error: ComponentRange) -> Self {
        Self
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Timestamp(i64);

impl Timestamp {
    pub fn i64(self) -> i64 {
        self.0
    }

    pub fn as_u64(self) -> u64 {
        self.0 as u64
    }

    pub fn to_rfc3339(&self) -> Result<String, TimestampOutOfRangeError> {
        ts_to_rfc3339(self.0)
    }
}

impl From<i64> for Timestamp {
    fn from(source: i64) -> Self {
        Self(source)
    }
}

impl std::fmt::Debug for Timestamp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

impl std::fmt::Display for Timestamp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.to_rfc3339() {
            Ok(ts) => ts.fmt(f),
            Err(err) => write!(f, "<invalid timestamp: {err}>"),
        }
    }
}

impl From<Timestamp> for i64 {
    fn from(source: Timestamp) -> Self {
        source.0
    }
}

impl NomReader<'_> for Timestamp {
    fn nom_read(input: &[u8]) -> tezos_data_encoding::nom::NomResult<Self> {
        nom::combinator::map(
            nom::number::complete::i64(nom::number::Endianness::Big),
            Timestamp,
        )(input)
    }
}

impl BinWriter for Timestamp {
    fn bin_write(&self, output: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
        tezos_data_encoding::enc::i64(&self.0, output)
    }
}

impl HasEncoding for Timestamp {
    fn encoding() -> Encoding {
        Encoding::Timestamp
    }
}

impl Serialize for Timestamp {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if serializer.is_human_readable() {
            match self.to_rfc3339() {
                Ok(ts) => ts.serialize(serializer),
                Err(err) => Err(serde::ser::Error::custom(format!(
                    "cannot convert timestamp to rfc3339: {err}"
                ))),
            }
        } else {
            self.0.serialize(serializer)
        }
    }
}

impl<'de> Deserialize<'de> for Timestamp {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[allow(dead_code)]
        struct TimestampVisitor;

        impl<'de> serde::de::Visitor<'de> for TimestampVisitor {
            type Value = Timestamp;

            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                f.write_str("timestamp either as i64 or Rfc3339 is expected")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                let parsed: OffsetDateTime = OffsetDateTime::parse(v, &Rfc3339)
                    .map_err(|e| E::custom(format!("error parsing Rfc3339 timestamp: {e}")))?;
                let timestamp = parsed.unix_timestamp();
                Ok(Timestamp(timestamp))
            }

            fn visit_newtype_struct<E>(self, e: E) -> Result<Self::Value, E::Error>
            where
                E: serde::Deserializer<'de>,
            {
                let field0 = match <i64 as serde::Deserialize>::deserialize(e) {
                    Ok(val) => val,
                    Err(err) => {
                        return Err(err);
                    }
                };
                Ok(Timestamp(field0))
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                let timestamp = match seq.next_element::<i64>() {
                    Ok(Some(val)) => val,
                    Ok(None) => return Err(serde::de::Error::custom("no timestamp".to_string())),
                    Err(err) => return Err(err),
                };
                Ok(Timestamp(timestamp))
            }
        }

        if deserializer.is_human_readable() {
            let string: String = serde::Deserialize::deserialize(deserializer)?;
            let parsed: OffsetDateTime = OffsetDateTime::parse(&string, &Rfc3339).map_err(|e| {
                serde::de::Error::custom(format!("error parsing Rfc3339 timestamp: {e}"))
            })?;
            Ok(Self(parsed.unix_timestamp()))
        } else {
            Ok(Self(serde::Deserialize::deserialize(deserializer)?))
        }
    }
}

impl Sub for Timestamp {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        Self::from(self.0.saturating_sub(other.i64()))
    }
}
