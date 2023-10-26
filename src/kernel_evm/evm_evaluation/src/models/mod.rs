// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2021-2023 draganrakita
//
// SPDX-License-Identifier: MIT

mod deserializer;

use bytes::Bytes;
use primitive_types::U256;
use primitives::{HashMap, SpecId, B160, B256};
use serde::Deserialize;
use std::collections::BTreeMap;

use crate::models::deserializer::*;

#[derive(Debug, PartialEq, Eq, PartialOrd, Hash, Ord, Deserialize)]
pub enum SpecName {
    Frontier,
    FrontierToHomesteadAt5,
    Homestead,
    HomesteadToDaoAt5,
    HomesteadToEIP150At5,
    EIP150,
    EIP158, // EIP-161: State trie clearing
    EIP158ToByzantiumAt5,
    Byzantium,
    ByzantiumToConstantinopleAt5, // SKIPPED
    ByzantiumToConstantinopleFixAt5,
    Constantinople, // SKIPPED
    ConstantinopleFix,
    Istanbul,
    Berlin,
    BerlinToLondonAt5,
    London,
    Merge,
    Shanghai,
    Cancun,
    #[serde(other)]
    Unknown,
}

impl SpecName {
    pub fn to_spec_id(&self) -> SpecId {
        match self {
            Self::Frontier => SpecId::FRONTIER,
            Self::Homestead | Self::FrontierToHomesteadAt5 => SpecId::HOMESTEAD,
            Self::EIP150 | Self::HomesteadToDaoAt5 | Self::HomesteadToEIP150At5 => {
                SpecId::TANGERINE
            }
            Self::EIP158 => SpecId::SPURIOUS_DRAGON,
            Self::Byzantium | Self::EIP158ToByzantiumAt5 => SpecId::BYZANTIUM,
            Self::ConstantinopleFix | Self::ByzantiumToConstantinopleFixAt5 => {
                SpecId::PETERSBURG
            }
            Self::Istanbul => SpecId::ISTANBUL,
            Self::Berlin => SpecId::BERLIN,
            Self::London | Self::BerlinToLondonAt5 => SpecId::LONDON,
            Self::Merge => SpecId::MERGE,
            Self::Shanghai => SpecId::SHANGHAI,
            Self::Cancun => SpecId::CANCUN,
            Self::ByzantiumToConstantinopleAt5 | Self::Constantinople => {
                panic!("Overridden with PETERSBURG")
            }
            Self::Unknown => panic!("Unknown spec"),
        }
    }

    // Custom `to_string`
    pub fn to_str(&self) -> String {
        let spec_str = match self {
            Self::Frontier => "Frontier",
            Self::Homestead | Self::FrontierToHomesteadAt5 => "Homestead",
            Self::EIP150 | Self::HomesteadToDaoAt5 | Self::HomesteadToEIP150At5 => {
                "EIP150"
            }
            Self::EIP158 => "EIP158",
            Self::Byzantium | Self::EIP158ToByzantiumAt5 => "Byzantium",
            Self::ConstantinopleFix | Self::ByzantiumToConstantinopleFixAt5 => {
                "ConstantinopleFix"
            }
            Self::Istanbul => "Istanbul",
            Self::Berlin => "Berlin",
            Self::London | Self::BerlinToLondonAt5 => "London",
            Self::Merge => "Merge",
            Self::Shanghai => "Shanghai",
            Self::Cancun => "Cancun",
            Self::ByzantiumToConstantinopleAt5 | Self::Constantinople => {
                panic!("Overridden with PETERSBURG")
            }
            Self::Unknown => panic!("Unknown spec"),
        };
        spec_str.to_owned()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize)]
#[serde(deny_unknown_fields)]
#[serde(rename_all = "camelCase")]
pub struct AccountInfo {
    pub balance: U256,
    #[serde(deserialize_with = "deserialize_str_as_bytes")]
    pub code: Bytes,
    #[serde(deserialize_with = "deserialize_str_as_u64")]
    pub nonce: u64,
    pub storage: HashMap<U256, U256>,
}

#[derive(Debug, PartialEq, Eq, Deserialize)]
pub struct TestSuite(pub BTreeMap<String, TestUnit>);

#[derive(Debug, PartialEq, Eq, Deserialize)]
pub struct TestUnit {
    pub _info: Info,
    pub env: UnitEnv,
    pub pre: HashMap<B160, AccountInfo>,
    pub post: BTreeMap<SpecName, Vec<Test>>,
    pub transaction: TransactionParts,
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize)]
pub struct FillerSource(pub BTreeMap<String, Fillers>);

#[derive(Clone, Debug, PartialEq, Eq, Deserialize)]
pub struct Fillers {
    pub expect: Vec<FillerResult>,
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize)]
pub struct FillerResult {
    pub network: Vec<String>,
    pub result: HashMap<String, AccountInfoFiller>,
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AccountInfoFiller {
    pub balance: Option<U256>,
    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(deserialize_with = "deserialize_opt_str_as_bytes")]
    pub code: Option<Bytes>,
    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(deserialize_with = "deserialize_opt_str_as_u64")]
    pub nonce: Option<u64>,
    pub storage: Option<HashMap<U256, U256>>,
    pub shouldnotexist: Option<String>,
}

#[derive(Debug, PartialEq, Eq, Deserialize)]
pub struct Info {
    pub source: String,
}

/// State test indexed state result deserialization.
#[derive(Debug, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Test {
    pub expect_exception: Option<String>,
    /// Post state hash
    pub hash: B256,
    /// Indexes
    pub indexes: TxPartIndices,
    // logs
    pub logs: B256,
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
    pub current_coinbase: B160,
    #[serde(default, deserialize_with = "deserialize_str_as_u256")]
    pub current_difficulty: U256,
    #[serde(deserialize_with = "deserialize_str_as_u256")]
    pub current_gas_limit: U256,
    #[serde(deserialize_with = "deserialize_str_as_u256")]
    pub current_number: U256,
    #[serde(deserialize_with = "deserialize_str_as_u256")]
    pub current_timestamp: U256,
    pub current_base_fee: Option<U256>,
    pub previous_hash: B256,
}

#[derive(Debug, PartialEq, Eq, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct AccessListItem {
    pub address: B160,
    pub storage_keys: Vec<B256>,
}

// TODO: replace this by tezos_ethereum::access_list::AccessList
// (make it "serde compatible")
pub type AccessList = Vec<AccessListItem>;

#[derive(Debug, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TransactionParts {
    #[serde(deserialize_with = "deserialize_vec_as_vec_bytes")]
    pub data: Vec<Bytes>,
    pub access_lists: Option<Vec<Option<AccessList>>>,
    pub gas_limit: Vec<U256>,
    pub gas_price: Option<U256>,
    pub nonce: U256,
    pub secret_key: Option<B256>,
    #[serde(deserialize_with = "deserialize_maybe_empty")]
    pub to: Option<B160>,
    pub value: Vec<U256>,
    pub max_fee_per_gas: Option<U256>,
    pub max_priority_fee_per_gas: Option<U256>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct BlockEnv {
    pub number: U256,
    /// Coinbase or miner or address that created and signed the block.
    /// Address where we are going to send gas spend
    pub coinbase: B160,
    pub timestamp: U256,
    /// Difficulty is removed and not used after Paris (aka TheMerge).
    /// Value is replaced with prevrandao.
    pub difficulty: U256,
    /// Prevrandao is used after Paris (aka TheMerge) instead of the difficulty value.
    /// NOTE: prevrandao can be found in block in place of mix_hash.
    pub prevrandao: Option<B256>,
    /// basefee is added in EIP1559 London upgrade
    pub basefee: U256,
    pub gas_limit: U256,
}

impl Default for BlockEnv {
    fn default() -> BlockEnv {
        BlockEnv {
            gas_limit: U256::MAX,
            number: U256::zero(),
            coinbase: B160::zero(),
            timestamp: U256::from(1),
            difficulty: U256::zero(),
            prevrandao: Some(B256::zero()),
            basefee: U256::zero(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TxEnv {
    /// Caller or Author or tx signer
    pub caller: B160,
    pub gas_limit: u64,
    pub gas_price: U256,
    pub gas_priority_fee: Option<U256>,
    pub transact_to: Option<B160>,
    pub value: U256,
    #[cfg_attr(feature = "serde", serde(with = "crate::utilities::serde_hex_bytes"))]
    pub data: Bytes,
    pub chain_id: Option<u64>,
    pub nonce: Option<u64>,
    pub access_list: Vec<(B160, Vec<U256>)>,
}

impl Default for TxEnv {
    fn default() -> TxEnv {
        TxEnv {
            caller: B160::zero(),
            gas_limit: u64::MAX,
            gas_price: U256::zero(),
            gas_priority_fee: None,
            transact_to: Some(B160::zero()), // will do nothing
            value: U256::zero(),
            data: Bytes::new(),
            chain_id: None,
            nonce: None,
            access_list: Vec::new(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub struct CfgEnv {
    pub chain_id: u64,
    pub spec_id: SpecId,
}

impl Default for CfgEnv {
    fn default() -> CfgEnv {
        CfgEnv {
            chain_id: 1,
            spec_id: SpecId::LATEST,
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Env {
    pub cfg: CfgEnv,
    pub block: BlockEnv,
    pub tx: TxEnv,
}

#[cfg(test)]
mod tests {
    use super::*;
    use primitives::B160;
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
    pub fn serialize_b160() -> Result<(), Error> {
        let json = r#"{"_item":"0x2adc25665018aa1fe0e6bc666dac8fc2697ff9ba"}"#;

        #[derive(Deserialize, Debug)]
        pub struct Test {
            _item: B160,
        }

        let out: Test = serde_json::from_str(json)?;
        println!("out:{out:?}");
        Ok(())
    }
}
