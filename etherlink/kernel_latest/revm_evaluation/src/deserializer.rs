// SPDX-FileCopyrightText: 2023-2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm::context::transaction::{
    AccessList, AccessListItem, Authorization, SignedAuthorization,
};
use revm::primitives::{hex, Address, B256, U256};
use serde::de::Error as DeError;
use serde::{Deserialize, Deserializer};

fn from_hex_u8<'de, D>(deserializer: D) -> Result<u8, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let s: &str = Deserialize::deserialize(deserializer)?;
    u8::from_str_radix(s.trim_start_matches("0x"), 16).map_err(DeError::custom)
}

pub fn from_hex_u64<'de, D>(deserializer: D) -> Result<u64, D::Error>
where
    D: Deserializer<'de>,
{
    let s: &str = Deserialize::deserialize(deserializer)?;
    u64::from_str_radix(s.trim_start_matches("0x"), 16).map_err(DeError::custom)
}

pub fn from_hex_opt_u128<'de, D>(deserializer: D) -> Result<Option<u128>, D::Error>
where
    D: Deserializer<'de>,
{
    let s: &str = Deserialize::deserialize(deserializer)?;
    Ok(Some(
        u128::from_str_radix(s.trim_start_matches("0x"), 16).map_err(DeError::custom)?,
    ))
}

pub fn from_hex_u256<'de, D>(deserializer: D) -> Result<U256, D::Error>
where
    D: Deserializer<'de>,
{
    let s: &str = Deserialize::deserialize(deserializer)?;
    U256::from_str_radix(s.trim_start_matches("0x"), 16).map_err(DeError::custom)
}

pub fn from_hex_b256<'de, D>(deserializer: D) -> Result<B256, D::Error>
where
    D: Deserializer<'de>,
{
    let s: &str = Deserialize::deserialize(deserializer)?;
    let bytes = hex::decode(s.trim_start_matches("0x")).map_err(DeError::custom)?;
    Ok(B256::from_slice(&bytes))
}

pub fn from_hex_opt_address<'de, D>(deserializer: D) -> Result<Option<Address>, D::Error>
where
    D: Deserializer<'de>,
{
    let s: String = Deserialize::deserialize(deserializer)?;
    if s.is_empty() {
        Ok(None)
    } else {
        s.parse::<Address>().map(Some).map_err(DeError::custom)
    }
}

fn vec_from_hex<'de, D, T, F>(deserializer: D, f: F) -> Result<Vec<T>, D::Error>
where
    D: Deserializer<'de>,
    F: Fn(&str) -> Result<T, D::Error>,
{
    let vec: Vec<String> = Deserialize::deserialize(deserializer)?;
    vec.into_iter().map(|s| f(&s)).collect()
}

pub fn from_hex_vec_u64<'de, D>(deserializer: D) -> Result<Vec<u64>, D::Error>
where
    D: Deserializer<'de>,
{
    vec_from_hex(deserializer, |s| {
        u64::from_str_radix(s.trim_start_matches("0x"), 16).map_err(DeError::custom)
    })
}

pub fn from_hex_vec_u256<'de, D>(deserializer: D) -> Result<Vec<U256>, D::Error>
where
    D: Deserializer<'de>,
{
    vec_from_hex(deserializer, |s| {
        U256::from_str_radix(s.trim_start_matches("0x"), 16).map_err(DeError::custom)
    })
}

fn from_hex_vec_b256<'de, D>(deserializer: D) -> Result<Vec<B256>, D::Error>
where
    D: Deserializer<'de>,
{
    vec_from_hex(deserializer, |s| {
        let bytes = hex::decode(s.trim_start_matches("0x")).map_err(DeError::custom)?;
        Ok(B256::from_slice(&bytes))
    })
}

#[derive(Deserialize)]
struct RawAccessListItem {
    address: Address,
    #[serde(rename = "storageKeys", deserialize_with = "from_hex_vec_b256")]
    storage_keys: Vec<B256>,
}

impl From<RawAccessListItem> for AccessListItem {
    fn from(raw: RawAccessListItem) -> Self {
        AccessListItem {
            address: raw.address,
            storage_keys: raw.storage_keys,
        }
    }
}

pub fn from_fixture_access_list<'de, D>(
    deserializer: D,
) -> Result<Option<Vec<AccessList>>, D::Error>
where
    D: Deserializer<'de>,
{
    let raw: Option<Vec<Vec<RawAccessListItem>>> = Option::deserialize(deserializer)?;

    Ok(raw.map(|raw_groups| {
        raw_groups
            .into_iter()
            .map(|group| {
                let items = group.into_iter().map(AccessListItem::from).collect();
                AccessList(items)
            })
            .collect()
    }))
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct RawAuth {
    #[serde(deserialize_with = "from_hex_u256")]
    chain_id: U256,
    address: Address,
    #[serde(deserialize_with = "from_hex_u64")]
    nonce: u64,
    #[serde(default, deserialize_with = "from_hex_u8")]
    y_parity: u8,
    #[serde(deserialize_with = "from_hex_u256")]
    r: U256,
    #[serde(deserialize_with = "from_hex_u256")]
    s: U256,
}

impl From<RawAuth> for SignedAuthorization {
    fn from(raw: RawAuth) -> Self {
        SignedAuthorization::new_unchecked(
            Authorization {
                chain_id: raw.chain_id,
                address: raw.address,
                nonce: raw.nonce,
            },
            raw.y_parity,
            raw.r,
            raw.s,
        )
    }
}

pub fn from_fixture_authorization_list<'de, D>(
    deserializer: D,
) -> Result<Option<Vec<SignedAuthorization>>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let raws: Option<Vec<RawAuth>> = Option::deserialize(deserializer)?;

    match raws {
        None => Ok(None),
        Some(raws) => Ok(Some(
            raws.into_iter().map(SignedAuthorization::from).collect(),
        )),
    }
}
