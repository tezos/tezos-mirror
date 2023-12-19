// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2021-2023 draganrakita
//
// SPDX-License-Identifier: MIT

mod deserializer;
pub mod spec;

use bytes::Bytes;
use primitive_types::{H160, H256, U256};
use serde::Deserialize;
use std::collections::{BTreeMap, HashMap};

use crate::models::deserializer::*;

#[derive(Debug, PartialEq, Eq, PartialOrd, Hash, Ord, Deserialize)]
pub enum SpecName {
    Shanghai,
    Cancun,
    #[serde(other)]
    Unknown,
}

impl SpecName {
    // Custom `to_string`
    pub fn to_str(&self) -> String {
        let spec_str = match self {
            Self::Shanghai => "Shanghai",
            Self::Cancun => "Cancun",
            Self::Unknown => panic!("Unknown spec"),
        };
        spec_str.to_owned()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize)]
#[serde(deny_unknown_fields)]
#[serde(rename_all = "camelCase")]
pub struct AccountInfo {
    #[serde(deserialize_with = "deserialize_str_as_u256")]
    pub balance: U256,
    #[serde(deserialize_with = "deserialize_str_as_bytes")]
    pub code: Bytes,
    #[serde(deserialize_with = "deserialize_str_as_u64")]
    pub nonce: u64,
    #[serde(deserialize_with = "deserialize_h256_hashmap")]
    pub storage: HashMap<H256, H256>,
}

#[derive(Debug, PartialEq, Eq, Deserialize)]
pub struct TestSuite(pub BTreeMap<String, TestUnit>);

#[derive(Debug, PartialEq, Eq, Deserialize)]
pub struct TestUnit {
    pub _info: Info,
    pub env: UnitEnv,
    pub pre: HashMap<H160, AccountInfo>,
    pub post: BTreeMap<SpecName, Vec<Test>>,
    pub transaction: TransactionParts,
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize)]
pub struct FillerSource(pub BTreeMap<String, Fillers>);

#[derive(Clone, Debug, PartialEq, Eq, Deserialize)]
pub struct Fillers {
    pub expect: Vec<FillerResult>,
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Default)]
pub struct FillerResult {
    #[serde(default)]
    pub indexes: FillerResultIndexes,
    pub network: Vec<String>,
    pub result: HashMap<String, AccountInfoFiller>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IndexKind {
    Label(String),
    Range(i64, i64),
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Default)]
pub struct FillerResultIndexes {
    #[serde(default)]
    #[serde(deserialize_with = "deserialize_indices")]
    pub data: Vec<IndexKind>,
    #[serde(default)]
    #[serde(deserialize_with = "deserialize_indices")]
    pub gas: Vec<IndexKind>,
    #[serde(default)]
    #[serde(deserialize_with = "deserialize_indices")]
    pub value: Vec<IndexKind>,
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AccountInfoFiller {
    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(deserialize_with = "deserialize_opt_str_as_u256")]
    pub balance: Option<U256>,
    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(deserialize_with = "deserialize_opt_str_as_bytes")]
    pub code: Option<Bytes>,
    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(deserialize_with = "deserialize_opt_str_as_u64")]
    pub nonce: Option<u64>,
    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(deserialize_with = "deserialize_opt_h256_hashmap")]
    pub storage: Option<HashMap<H256, H256>>,
    pub shouldnotexist: Option<String>,
}

#[derive(Debug, PartialEq, Eq, Deserialize)]
pub struct Info {
    pub source: String,
    #[serde(default)]
    pub labels: HashMap<usize, String>,
}

/// State test indexed state result deserialization.
#[derive(Debug, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Test {
    pub expect_exception: Option<String>,
    /// Post state hash
    pub hash: H256,
    /// Indexes
    pub indexes: TxPartIndices,
    // logs
    pub logs: H256,
    #[serde(default)]
    #[serde(deserialize_with = "deserialize_opt_str_as_bytes")]
    pub txbytes: Option<Bytes>,
}

#[derive(Debug, PartialEq, Eq, Deserialize)]
pub struct TxPartIndices {
    pub data: usize,
    pub gas: usize,
    pub value: usize,
}

#[derive(Debug, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UnitEnv {
    pub current_coinbase: H160,
    pub current_gas_limit: U256,
    pub current_number: U256,
    pub current_timestamp: U256,
    pub current_base_fee: Option<U256>,
}

#[derive(Debug, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TransactionParts {
    #[serde(deserialize_with = "deserialize_vec_as_vec_bytes")]
    pub data: Vec<Bytes>,
    pub gas_limit: Vec<U256>,
    pub gas_price: Option<U256>,
    pub secret_key: Option<H256>,
    #[serde(deserialize_with = "deserialize_maybe_empty")]
    pub to: Option<H160>,
    pub value: Vec<U256>,
    pub max_fee_per_gas: Option<U256>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct BlockEnv {
    pub number: U256,
    /// Coinbase or miner or address that created and signed the block.
    /// This is the receiver address of all the gas spent in the block.
    pub coinbase: H160,
    pub timestamp: U256,
    pub basefee: U256, // EIP1559
    pub gas_limit: U256,
}

impl Default for BlockEnv {
    fn default() -> BlockEnv {
        BlockEnv {
            gas_limit: U256::MAX,
            number: U256::zero(),
            coinbase: H160::zero(),
            timestamp: U256::from(1),
            basefee: U256::zero(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TxEnv {
    /// Caller or Author or tx signer
    pub caller: H160,
    pub gas_limit: u64,
    pub gas_price: U256,
    pub transact_to: Option<H160>,
    pub value: U256,
    #[cfg_attr(feature = "serde", serde(with = "crate::utilities::serde_hex_bytes"))]
    pub data: Bytes,
}

impl Default for TxEnv {
    fn default() -> TxEnv {
        TxEnv {
            caller: H160::zero(),
            gas_limit: u64::MAX,
            gas_price: U256::zero(),
            transact_to: Some(H160::zero()), // will do nothing
            value: U256::zero(),
            data: Bytes::new(),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Env {
    pub block: BlockEnv,
    pub tx: TxEnv,
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::Error;

    #[test]
    pub fn serialize_u256() -> Result<(), Error> {
        let json = r#"{"_item":"0x10"}"#;

        #[derive(Deserialize, Debug)]
        pub struct Test {
            _item: Option<U256>,
        }

        let out: Test = serde_json::from_str(json)?;
        println!("out:{out:?}");
        Ok(())
    }

    #[test]
    pub fn serialize_h160() -> Result<(), Error> {
        let json = r#"{"_item":"0x2adc25665018aa1fe0e6bc666dac8fc2697ff9ba"}"#;

        #[derive(Deserialize, Debug)]
        pub struct Test {
            _item: H160,
        }

        let out: Test = serde_json::from_str(json)?;
        println!("out:{out:?}");
        Ok(())
    }
}
