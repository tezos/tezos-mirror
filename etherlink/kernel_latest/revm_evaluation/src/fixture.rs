// SPDX-FileCopyrightText: 2023-2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::deserializer::{
    from_fixture_access_list, from_fixture_authorization_list, from_hex_b256,
    from_hex_opt_address, from_hex_opt_u128, from_hex_u256, from_hex_u64,
    from_hex_vec_u256, from_hex_vec_u64,
};
use revm::{
    context::transaction::{AccessList, SignedAuthorization},
    primitives::{Address, Bytes, HashMap, StorageKey, StorageValue, B256, U256},
};
use serde::Deserialize;

pub type Fixtures = HashMap<String, TestCase>;

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct TestCase {
    pub env: Env,
    pub pre: HashMap<Address, Account>,
    pub transaction: Transaction,
    pub post: HashMap<SpecName, Vec<PostEntry>>,
    pub config: Config,
    #[serde(rename = "_info")]
    pub info: Info,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Env {
    pub current_coinbase: Address,
    #[serde(deserialize_with = "from_hex_u64")]
    pub current_gas_limit: u64,
    #[serde(deserialize_with = "from_hex_u256")]
    pub current_number: U256,
    #[serde(deserialize_with = "from_hex_u256")]
    pub current_timestamp: U256,
    #[serde(deserialize_with = "from_hex_u256")]
    pub current_difficulty: U256,
    #[serde(deserialize_with = "from_hex_u64")]
    pub current_base_fee: u64,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct Account {
    #[serde(deserialize_with = "from_hex_u64")]
    pub nonce: u64,
    #[serde(deserialize_with = "from_hex_u256")]
    pub balance: U256,
    pub code: Bytes,
    pub storage: HashMap<StorageKey, StorageValue>,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Transaction {
    #[serde(deserialize_with = "from_hex_u64")]
    pub nonce: u64,
    #[serde(default, deserialize_with = "from_hex_opt_u128")]
    pub gas_price: Option<u128>,
    #[serde(deserialize_with = "from_hex_vec_u64")]
    pub gas_limit: Vec<u64>,
    #[serde(deserialize_with = "from_hex_opt_address")]
    pub to: Option<Address>,
    #[serde(deserialize_with = "from_hex_vec_u256")]
    pub value: Vec<U256>,
    pub data: Vec<Bytes>,
    #[serde(default, deserialize_with = "from_fixture_access_list")]
    pub access_lists: Option<Vec<AccessList>>,
    #[serde(default, deserialize_with = "from_fixture_authorization_list")]
    pub authorization_list: Option<Vec<SignedAuthorization>>,
    pub sender: Address,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash, Ord, Deserialize)]
pub enum SpecName {
    Shanghai,
    Cancun,
    Prague,
    #[serde(other)]
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct PostEntry {
    #[serde(deserialize_with = "from_hex_b256")]
    pub hash: B256,
    #[serde(deserialize_with = "from_hex_b256")]
    pub logs: B256,
    pub txbytes: Bytes,
    pub indexes: Indexes,
    pub state: HashMap<Address, Account>,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct Indexes {
    pub data: u64,
    pub gas: u64,
    pub value: u64,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct Config {
    #[serde(deserialize_with = "from_hex_u64")]
    pub chainid: u64,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct Info {
    pub hash: String,
    pub comment: String,
    #[serde(rename = "filling-transition-tool")]
    pub filling_transition_tool: String,
    pub description: String,
    pub url: String,
    #[serde(rename = "reference-spec")]
    pub reference_spec: String,
    #[serde(rename = "reference-spec-version")]
    pub reference_spec_version: String,
}
