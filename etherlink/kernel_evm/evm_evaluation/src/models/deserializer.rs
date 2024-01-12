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
use serde_yaml::Value;
use std::collections::HashMap;
use std::fmt::Debug;
use std::str::FromStr;

use super::IndexKind;

const H256_RAW_SIZE: usize = 64;

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

pub fn deserialize_str_as_u256<'de, D>(deserializer: D) -> Result<U256, D::Error>
where
    D: de::Deserializer<'de>,
{
    let string = String::deserialize(deserializer)?;

    let output = if let Some(stripped) = string.strip_prefix("0x") {
        U256::from_str_radix(stripped, 16).unwrap()
    } else {
        U256::from_dec_str(&string).unwrap()
    };

    Ok(output)
}

pub fn deserialize_opt_str_as_u256<'de, D>(
    deserializer: D,
) -> Result<Option<U256>, D::Error>
where
    D: de::Deserializer<'de>,
{
    #[derive(Debug, Deserialize)]
    struct WrappedValue(#[serde(deserialize_with = "deserialize_str_as_u256")] U256);

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
        let decoded = hex::decode(string.strip_prefix("0x").unwrap_or(&string))
            .map_err(D::Error::custom)?
            .into();
        out.push(decoded)
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

fn deserialize_as_h256<'de, D>(deserializer: D) -> Result<H256, D::Error>
where
    D: de::Deserializer<'de>,
{
    let mut value = String::deserialize(deserializer)?;

    if let Some(stripped) = value.strip_prefix("0x") {
        // Some values or indexes are already in hexadecimal.
        value = stripped.to_owned();
    } else if value
        .find(|c: char| ('a'..='f').contains(&c) || ('A'..='F').contains(&c))
        .is_some()
    {
        // We need this case to filter out odd formatting from people
        // mixing up decimal and hexadecimal values...
    } else {
        // Some are not so we have to convert the decimal values
        // into hexadecimal ones.
        let dec_to_hex = u64::from_str(&value).unwrap();
        value = format!("{:x}", dec_to_hex);
    }

    let filling_zeroes = (0..(H256_RAW_SIZE - value.len()))
        .map(|_| "0")
        .collect::<String>();

    value = filling_zeroes + &value;

    let h256 = H256::from_str(&value).unwrap();
    Ok(h256)
}

pub fn deserialize_h256_hashmap<'de, D>(
    deserializer: D,
) -> Result<HashMap<H256, H256>, D::Error>
where
    D: de::Deserializer<'de>,
{
    #[derive(Debug, Deserialize, PartialEq, Eq, Hash)]
    struct WrappedValue(#[serde(deserialize_with = "deserialize_as_h256")] H256);

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

enum Either {
    Constraint(IndexKind),
    Wildcard,
}

fn deserialize_index(v: &Value) -> Option<Either> {
    match v {
        Value::Number(n) => {
            if let Some(n) = n.as_i64() {
                if n == -1 {
                    Some(Either::Wildcard)
                } else {
                    Some(Either::Constraint(IndexKind::Range(n, n)))
                }
            } else {
                None
            }
        }
        Value::String(string) => {
            if let Some(label) = regex::Regex::new(r":label ([\w-]+)")
                .unwrap()
                .captures(string)
            {
                Some(Either::Constraint(IndexKind::Label(String::from(
                    label.get(1).unwrap().as_str(),
                ))))
            } else if let Some(range) = regex::Regex::new(r"([0-9]+)-([0-9]+)")
                .unwrap()
                .captures(string)
            {
                let start: i64 = i64::from_str(range.get(1)?.as_str()).unwrap();
                let end: i64 = i64::from_str(range.get(2)?.as_str()).unwrap();
                Some(Either::Constraint(IndexKind::Range(start, end)))
            } else {
                None
            }
        }
        _ => None,
    }
}

pub fn deserialize_indices<'de, D>(deserializer: D) -> Result<Vec<IndexKind>, D::Error>
where
    D: de::Deserializer<'de>,
{
    let err = |v| D::Error::custom(format!("Unexpected index {:?}", v));
    match Value::deserialize(deserializer)? {
        Value::Sequence(values) => {
            let mut out = Vec::new();
            for value in values {
                if let Either::Constraint(c) =
                    deserialize_index(&value).ok_or_else(|| err(value))?
                {
                    out.push(c)
                }
            }
            Ok(out)
        }
        v => match deserialize_index(&v) {
            Some(Either::Constraint(c)) => Ok(vec![c]),
            Some(Either::Wildcard) => Ok(vec![]),
            None => Err(err(v)),
        },
    }
}
