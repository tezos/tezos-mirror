// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2021-2023 draganrakita
//
// SPDX-License-Identifier: MIT

use bytes::Bytes;
use primitive_types::{H160, H256, U256};
use serde::{
    de::{self, Error},
    Deserialize,
};
use std::collections::HashMap;
use std::str::FromStr;

pub fn deserialize_str_as_u64<'de, D>(deserializer: D) -> Result<u64, D::Error>
where
    D: de::Deserializer<'de>,
{
    let string = String::deserialize(deserializer)?;

    let output = if let Some(stripped) = string.strip_prefix("0x") {
        u64::from_str_radix(stripped, 16).unwrap()
    } else {
        string.parse().unwrap()
    };

    Ok(output)
}

pub fn deserialize_opt_str_as_u64<'de, D>(
    deserializer: D,
) -> Result<Option<u64>, D::Error>
where
    D: de::Deserializer<'de>,
{
    #[derive(Debug, Deserialize)]
    struct WrappedValue(#[serde(deserialize_with = "deserialize_str_as_u64")] u64);

    Option::<WrappedValue>::deserialize(deserializer).map(
        |opt_wrapped: Option<WrappedValue>| {
            opt_wrapped.map(|wrapped: WrappedValue| wrapped.0)
        },
    )
}

pub fn deserialize_vec_as_vec_bytes<'de, D>(
    deserializer: D,
) -> Result<Vec<Bytes>, D::Error>
where
    D: de::Deserializer<'de>,
{
    let strings: Vec<String> = Vec::<String>::deserialize(deserializer)?;

    let mut out = Vec::new();
    for string in strings {
        out.push(
            hex::decode(string.strip_prefix("0x").unwrap_or(&string))
                .map_err(D::Error::custom)?
                .into(),
        )
    }
    Ok(out)
}

pub fn deserialize_maybe_empty<'de, D>(deserializer: D) -> Result<Option<H160>, D::Error>
where
    D: de::Deserializer<'de>,
{
    let string: String = String::deserialize(deserializer)?;
    if string.is_empty() {
        return Ok(None);
    }
    Ok(Some(H160::from_str(&string).map_err(D::Error::custom)?))
}

pub fn deserialize_str_as_bytes<'de, D>(deserializer: D) -> Result<Bytes, D::Error>
where
    D: de::Deserializer<'de>,
{
    let s = String::deserialize(deserializer)?;

    Ok(hex::decode(s.strip_prefix("0x").unwrap_or(&s))
        .map_err(D::Error::custom)?
        .into())
}

pub fn deserialize_opt_str_as_bytes<'de, D>(
    deserializer: D,
) -> Result<Option<Bytes>, D::Error>
where
    D: de::Deserializer<'de>,
{
    #[derive(Debug, Deserialize)]
    struct WrappedValue(#[serde(deserialize_with = "deserialize_str_as_bytes")] Bytes);

    Option::<WrappedValue>::deserialize(deserializer).map(
        |opt_wrapped: Option<WrappedValue>| {
            opt_wrapped.map(|wrapped: WrappedValue| wrapped.0)
        },
    )
}

pub fn deserialize_u256_as_h256<'de, D>(deserializer: D) -> Result<H256, D::Error>
where
    D: de::Deserializer<'de>,
{
    let value = U256::deserialize(deserializer)?;

    let mut h256 = H256::zero();
    value.to_big_endian(h256.as_bytes_mut());

    Ok(h256)
}

pub fn deserialize_h256_hashmap<'de, D>(
    deserializer: D,
) -> Result<HashMap<H256, H256>, D::Error>
where
    D: de::Deserializer<'de>,
{
    #[derive(Debug, Deserialize, PartialEq, Eq, Hash)]
    struct WrappedValue(#[serde(deserialize_with = "deserialize_u256_as_h256")] H256);

    HashMap::<WrappedValue, WrappedValue>::deserialize(deserializer).map(
        |map_wrapped: HashMap<WrappedValue, WrappedValue>| {
            map_wrapped.iter().map(|(w1, w2)| (w1.0, w2.0)).collect()
        },
    )
}

pub fn deserialize_opt_h256_hashmap<'de, D>(
    deserializer: D,
) -> Result<Option<HashMap<H256, H256>>, D::Error>
where
    D: de::Deserializer<'de>,
{
    #[derive(Debug, Deserialize, PartialEq, Eq)]
    struct WrappedValue(
        #[serde(deserialize_with = "deserialize_h256_hashmap")] HashMap<H256, H256>,
    );

    Option::<WrappedValue>::deserialize(deserializer).map(
        |opt_wrapped: Option<WrappedValue>| {
            opt_wrapped.map(|wrapped: WrappedValue| wrapped.0)
        },
    )
}
