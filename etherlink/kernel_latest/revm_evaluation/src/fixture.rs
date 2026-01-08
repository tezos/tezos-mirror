// SPDX-FileCopyrightText: 2023-2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    deserializer::{
        from_fixture_access_list, from_fixture_authorization_list, from_hex_b256,
        from_hex_opt_address, from_hex_opt_u128, from_hex_u256, from_hex_u64,
        from_hex_vec_u256, from_hex_vec_u64,
    },
    helpers::bytes_hash,
};
use revm::{
    context::transaction::{AccessList, SignedAuthorization},
    primitives::{
        hardfork::SpecId, Address, Bytes, HashMap, StorageKey, StorageValue, B256,
        KECCAK_EMPTY, U256,
    },
    state::{AccountInfo, Bytecode},
};
use serde::Deserialize;
use std::{cmp::min, path::PathBuf};

pub type Fixtures = Vec<NamedFixture>;

#[derive(Debug)]
pub struct NamedFixture {
    pub path: PathBuf,
    pub fixtures: HashMap<String, TestCase>,
}

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

impl From<Account> for AccountInfo {
    fn from(account: Account) -> Self {
        let (code_hash, code) = if account.code.is_empty() {
            (KECCAK_EMPTY, None)
        } else {
            (
                bytes_hash(account.code.as_ref()),
                Some(Bytecode::new_raw_checked(account.code).unwrap()),
            )
        };
        AccountInfo {
            balance: account.balance,
            nonce: account.nonce,
            code_hash,
            code,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Transaction {
    #[serde(deserialize_with = "from_hex_u64")]
    pub nonce: u64,
    #[serde(default, deserialize_with = "from_hex_opt_u128")]
    pub gas_price: Option<u128>,
    #[serde(default, deserialize_with = "from_hex_opt_u128")]
    pub max_priority_fee_per_gas: Option<u128>,
    #[serde(default, deserialize_with = "from_hex_opt_u128")]
    pub max_fee_per_gas: Option<u128>,
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

impl Transaction {
    pub fn derive_gas_price(&self, base_fee: u128) -> u128 {
        match self.gas_price {
            Some(gas_price) => gas_price,
            None => {
                let max_fee_per_gas = self.max_fee_per_gas.unwrap();
                let max_priority_fee_per_gas = self.max_priority_fee_per_gas.unwrap();
                min(max_fee_per_gas, base_fee + max_priority_fee_per_gas)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash, Ord, Deserialize)]
pub enum SpecName {
    Shanghai,
    Cancun,
    Prague,
    Osaka,
    #[serde(other)]
    Unknown,
}

impl From<SpecName> for SpecId {
    fn from(spec: SpecName) -> Self {
        match spec {
            SpecName::Shanghai => SpecId::SHANGHAI,
            SpecName::Cancun => SpecId::CANCUN,
            SpecName::Prague => SpecId::PRAGUE,
            SpecName::Osaka => SpecId::OSAKA,
            SpecName::Unknown => panic!("Unknown EVM spec id"),
        }
    }
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
    pub data: usize,
    pub gas: usize,
    pub value: usize,
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

#[cfg(test)]
mod test {
    use crate::fixture::TestCase;
    use revm::primitives::HashMap;

    #[test]
    fn test_parse_execution_spec_fixture() {
        let data = r#"
        {
            "tests/cancun/eip6780_selfdestruct/test_dynamic_create2_selfdestruct_collision.py::test_dynamic_create2_selfdestruct_collision[fork_Cancun-state_test-call_create2_contract_in_between_False-call_create2_contract_at_the_end_True-create2_dest_already_in_state_False]": {
                "env": {
                    "currentCoinbase": "0x2adc25665018aa1fe0e6bc666dac8fc2697ff9ba",
                    "currentGasLimit": "0x055d4a80",
                    "currentNumber": "0x01",
                    "currentTimestamp": "0x03e8",
                    "currentRandom": "0x0000000000000000000000000000000000000000000000000000000000000000",
                    "currentDifficulty": "0x00",
                    "currentBaseFee": "0x07",
                    "currentExcessBlobGas": "0x00"
                },
                "pre": {
                    "0x1000000000000000000000000000000000001000": {
                        "nonce": "0x01",
                        "balance": "0x6124fee993bc0000",
                        "code": "0x6001600155",
                        "storage": {}
                    },
                    "0x1000000000000000000000000000000000001100": {
                        "nonce": "0x01",
                        "balance": "0x00",
                        "code": "0x366000600037600136600047f560005260206000f3",
                        "storage": {}
                    },
                    "0x1000000000000000000000000000000000001200": {
                        "nonce": "0x01",
                        "balance": "0x05f5e100",
                        "code": "0x5b36600060003760206000366000600a731000000000000000000000000000000000001100620186a0f160005160025560006000600060006000731000000000000000000000000000000000001100620186a0f1600060006000600060646000620186a0f1366000600037602060003660006103e8731000000000000000000000000000000000001100620186a0f1600051600355600060006000600061271073a61134aea8e2ab3454c770e1bd797dd52f9deaba620186a0f16001600455",
                        "storage": {
                            "0x02": "0xff",
                            "0x03": "0xff"
                        }
                    },
                    "0x8a0a19589531694250d570040a0c4b74576919b8": {
                        "nonce": "0x00",
                        "balance": "0x6124fee993bc0000",
                        "code": "0x",
                        "storage": {}
                    }
                },
                "transaction": {
                    "nonce": "0x00",
                    "gasPrice": "0x0a",
                    "gasLimit": [
                        "0x4c4b40"
                    ],
                    "to": "0x1000000000000000000000000000000000001200",
                    "value": [
                        "0x00"
                    ],
                    "data": [
                        "0x6001600155600060006000600060007310000000000000000000000000000000000010005af161001660008160318239f373a94f5374fce5edbc8e2a8697c15331677e6ebf0bff"
                    ],
                    "accessLists": [
                        [
                              {
                                  "address": "0x0000000000000000000000000000000000000064",
                                  "storageKeys": [
                                    "0x0000000000000000000000000000000000000000000000000000000000000064",
                                    "0x00000000000000000000000000000000000000000000000000000000000000c8"
                                ]
                            }
                        ]
                    ],
                    "authorizationList": [
                        {
                            "chainId": "0x00",
                            "address": "0x0000000000000000000000000000000000000001",
                            "nonce": "0x00",
                            "v": "0x00",
                            "r": "0x33a629f82b5aaff5b4bfe6b9ab53bb3646b92cda403d7c834f72c9a5073aec20",
                            "s": "0x16902e4a3c089e05d649cab6bd74b6f424fae0eefad3a4944fa452ff7413b5d3",
                            "signer": "0x8a0a19589531694250d570040a0c4b74576919b8",
                            "yParity": "0x00"
                        }
                    ],
                    "sender": "0x8a0a19589531694250d570040a0c4b74576919b8",
                    "secretKey": "0x9e7645d0cfd9c3a04eb7a9db59a4eb7d359f2e75c9164a9d6b9a7d54e1b6a36f"
                },
                "post": {
                    "Cancun": [
                        {
                            "hash": "0x038ac222550acdaedd37fa2536de9d20dc92916c0ae525a821e105e09b935ac0",
                            "logs": "0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347",
                            "txbytes": "0xf8a8800a834c4b4094100000000000000000000000000000000000120080b8476001600155600060006000600060007310000000000000000000000000000000000010005af161001660008160318239f373a94f5374fce5edbc8e2a8697c15331677e6ebf0bff26a0c88b2ae53949cb1202442058f75cabc0d9253852c200813e311474370cc3c4fda06af6c791394bfb24769e493d4fc8f0c42848b82c0cc55921c087a4dc94268e51",
                            "indexes": {
                                "data": 0,
                                "gas": 0,
                                "value": 0
                            },
                            "state": {
                                "0x1000000000000000000000000000000000001000": {
                                    "nonce": "0x01",
                                    "balance": "0x6124fee993bc0000",
                                    "code": "0x6001600155",
                                    "storage": {
                                        "0x01": "0x01"
                                    }
                                },
                                "0x1000000000000000000000000000000000001100": {
                                    "nonce": "0x04",
                                    "balance": "0x03e8",
                                    "code": "0x366000600037600136600047f560005260206000f3",
                                    "storage": {}
                                },
                                "0x1000000000000000000000000000000000001200": {
                                    "nonce": "0x01",
                                    "balance": "0x05f5b59a",
                                    "code": "0x5b36600060003760206000366000600a731000000000000000000000000000000000001100620186a0f160005160025560006000600060006000731000000000000000000000000000000000001100620186a0f1600060006000600060646000620186a0f1366000600037602060003660006103e8731000000000000000000000000000000000001100620186a0f1600051600355600060006000600061271073a61134aea8e2ab3454c770e1bd797dd52f9deaba620186a0f16001600455",
                                    "storage": {
                                        "0x02": "0xa61134aea8e2ab3454c770e1bd797dd52f9deaba",
                                        "0x04": "0x01"
                                    }
                                },
                                "0x8a0a19589531694250d570040a0c4b74576919b8": {
                                    "nonce": "0x01",
                                    "balance": "0x6124fee99385b568",
                                    "code": "0x",
                                    "storage": {}
                                },
                                "0x13dc12ab3efcbea5b070d4b2d5d74e5c2cf06686": {
                                    "nonce": "0x01",
                                    "balance": "0x00",
                                    "code": "0x",
                                    "storage": {}
                                },
                                "0x0000000000000000000000000000000000000000": {
                                    "nonce": "0x00",
                                    "balance": "0x64",
                                    "code": "0x",
                                    "storage": {}
                                },
                                "0xa94f5374fce5edbc8e2a8697c15331677e6ebf0b": {
                                    "nonce": "0x00",
                                    "balance": "0x271a",
                                    "code": "0x",
                                    "storage": {}
                                },
                                "0x2adc25665018aa1fe0e6bc666dac8fc2697ff9ba": {
                                    "nonce": "0x00",
                                    "balance": "0x104994",
                                    "code": "0x",
                                    "storage": {}
                                }
                            }
                        }
                    ]
                },
                "config": {
                    "blobSchedule": {
                        "Cancun": {
                            "target": "0x03",
                            "max": "0x06",
                            "baseFeeUpdateFraction": "0x32f0ed"
                        }
                    },
                    "chainid": "0x01"
                },
                "_info": {
                    "hash": "0xaadeb38f8d489a0b2773fd427106b9f5b286565c113f1c0c1b0b68c805c8961f",
                    "comment": "`execution-spec-tests` generated test",
                    "filling-transition-tool": "ethereum-spec-evm-resolver 0.0.5",
                    "description": "Dynamic Create2->Suicide->Create2 collision scenario.\n\n    Perform a CREATE2, make sure that the initcode sets at least a couple of storage keys,\n    then on a different call, in the same tx, perform a self-destruct.\n    Then:\n        a) on the same tx, attempt to recreate the contract   <=== Covered in this test\n            1) and create2 contract already in the state\n            2) and create2 contract is not in the state\n        b) on a different tx, attempt to recreate the contract\n    Perform a CREATE2, make sure that the initcode sets at least a couple of storage keys,\n    then in a different tx, perform a self-destruct.\n    Then:\n        a) on the same tx, attempt to recreate the contract\n        b) on a different tx, attempt to recreate the contract\n    Verify that the test case described in\n    https://lf-hyperledger.atlassian.net/wiki/spaces/BESU/pages/22156575/2024-01-06+Mainnet+Halting+Event",
                    "url": "https://github.com/ethereum/execution-spec-tests/blob/20770a694f440aeb4ea3ec86a3b9bc04d40146a3/tests/cancun/eip6780_selfdestruct/test_dynamic_create2_selfdestruct_collision.py#L30",
                    "fixture-format": "state_test",
                    "reference-spec": "https://github.com/ethereum/EIPs/blob/master/EIPS/eip-6780.md",
                    "reference-spec-version": "1b6a0e94cc47e859b9866e570391cf37dc55059a",
                    "eels-resolution": {
                        "git-url": "https://github.com/marioevz/execution-specs.git",
                        "branch": "forks/bpo1",
                        "commit": "3387e5f4aedfe99becfdc39b444d6371e25e0924"
                    }
                }
            },
            "tests/cancun/eip6780_selfdestruct/test_dynamic_create2_selfdestruct_collision.py::test_dynamic_create2_selfdestruct_collision[fork_Cancun-state_test-call_create2_contract_in_between_False-call_create2_contract_at_the_end_True-create2_dest_already_in_state_True]": {
                "env": {
                    "currentCoinbase": "0x2adc25665018aa1fe0e6bc666dac8fc2697ff9ba",
                    "currentGasLimit": "0x055d4a80",
                    "currentNumber": "0x01",
                    "currentTimestamp": "0x03e8",
                    "currentRandom": "0x0000000000000000000000000000000000000000000000000000000000000000",
                    "currentDifficulty": "0x00",
                    "currentBaseFee": "0x07",
                    "currentExcessBlobGas": "0x00"
                },
                "pre": {
                    "0x1000000000000000000000000000000000001000": {
                        "nonce": "0x01",
                        "balance": "0x6124fee993bc0000",
                        "code": "0x6001600155",
                        "storage": {}
                    },
                    "0x1000000000000000000000000000000000001100": {
                        "nonce": "0x01",
                        "balance": "0x00",
                        "code": "0x366000600037600136600047f560005260206000f3",
                        "storage": {}
                    },
                    "0x1000000000000000000000000000000000001200": {
                        "nonce": "0x01",
                        "balance": "0x05f5e100",
                        "code": "0x5b36600060003760206000366000600a731000000000000000000000000000000000001100620186a0f160005160025560006000600060006000731000000000000000000000000000000000001100620186a0f1600060006000600060646000620186a0f1366000600037602060003660006103e8731000000000000000000000000000000000001100620186a0f1600051600355600060006000600061271073a61134aea8e2ab3454c770e1bd797dd52f9deaba620186a0f16001600455",
                        "storage": {
                            "0x02": "0xff",
                            "0x03": "0xff"
                        }
                    },
                    "0x8a0a19589531694250d570040a0c4b74576919b8": {
                        "nonce": "0x00",
                        "balance": "0x6124fee993bc0000",
                        "code": "0x",
                        "storage": {}
                    },
                    "0xa61134aea8e2ab3454c770e1bd797dd52f9deaba": {
                        "nonce": "0x01",
                        "balance": "0x01",
                        "code": "0x73a94f5374fce5edbc8e2a8697c15331677e6ebf0bff",
                        "storage": {}
                    }
                },
                "transaction": {
                    "nonce": "0x00",
                    "maxPriorityFeePerGas": "0x00",
                    "maxFeePerGas": "0x0e",
                    "gasLimit": [
                        "0x4c4b40"
                    ],
                    "to": "",
                    "value": [
                        "0x00"
                    ],
                    "data": [
                        "0x6001600155600060006000600060007310000000000000000000000000000000000010005af161001660008160318239f373a94f5374fce5edbc8e2a8697c15331677e6ebf0bff"
                    ],
                    "sender": "0x8a0a19589531694250d570040a0c4b74576919b8",
                    "secretKey": "0x9e7645d0cfd9c3a04eb7a9db59a4eb7d359f2e75c9164a9d6b9a7d54e1b6a36f"
                },
                "post": {
                    "Cancun": [
                        {
                            "hash": "0x8fe74ede49e33c1d5081f4936f02a34c0c04fd530de0daf7fb7250e5b716b04e",
                            "logs": "0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347",
                            "txbytes": "0xf8a8800a834c4b4094100000000000000000000000000000000000120080b8476001600155600060006000600060007310000000000000000000000000000000000010005af161001660008160318239f373a94f5374fce5edbc8e2a8697c15331677e6ebf0bff26a0c88b2ae53949cb1202442058f75cabc0d9253852c200813e311474370cc3c4fda06af6c791394bfb24769e493d4fc8f0c42848b82c0cc55921c087a4dc94268e51",
                            "indexes": {
                                "data": 0,
                                "gas": 0,
                                "value": 0
                            },
                            "state": {
                                "0x1000000000000000000000000000000000001000": {
                                    "nonce": "0x01",
                                    "balance": "0x6124fee993bc0000",
                                    "code": "0x6001600155",
                                    "storage": {}
                                },
                                "0x1000000000000000000000000000000000001100": {
                                    "nonce": "0x04",
                                    "balance": "0x03e8",
                                    "code": "0x366000600037600136600047f560005260206000f3",
                                    "storage": {}
                                },
                                "0x1000000000000000000000000000000000001200": {
                                    "nonce": "0x01",
                                    "balance": "0x05f5b59a",
                                    "code": "0x5b36600060003760206000366000600a731000000000000000000000000000000000001100620186a0f160005160025560006000600060006000731000000000000000000000000000000000001100620186a0f1600060006000600060646000620186a0f1366000600037602060003660006103e8731000000000000000000000000000000000001100620186a0f1600051600355600060006000600061271073a61134aea8e2ab3454c770e1bd797dd52f9deaba620186a0f16001600455",
                                    "storage": {
                                        "0x04": "0x01"
                                    }
                                },
                                "0x8a0a19589531694250d570040a0c4b74576919b8": {
                                    "nonce": "0x01",
                                    "balance": "0x6124fee99383b66e",
                                    "code": "0x",
                                    "storage": {}
                                },
                                "0xa61134aea8e2ab3454c770e1bd797dd52f9deaba": {
                                    "nonce": "0x01",
                                    "balance": "0x00",
                                    "code": "0x73a94f5374fce5edbc8e2a8697c15331677e6ebf0bff",
                                    "storage": {}
                                },
                                "0x13dc12ab3efcbea5b070d4b2d5d74e5c2cf06686": {
                                    "nonce": "0x01",
                                    "balance": "0x0a",
                                    "code": "0x",
                                    "storage": {}
                                },
                                "0x0000000000000000000000000000000000000000": {
                                    "nonce": "0x00",
                                    "balance": "0x64",
                                    "code": "0x",
                                    "storage": {}
                                },
                                "0xa94f5374fce5edbc8e2a8697c15331677e6ebf0b": {
                                    "nonce": "0x00",
                                    "balance": "0x2711",
                                    "code": "0x",
                                    "storage": {}
                                },
                                "0x2adc25665018aa1fe0e6bc666dac8fc2697ff9ba": {
                                    "nonce": "0x00",
                                    "balance": "0x10e2df",
                                    "code": "0x",
                                    "storage": {}
                                }
                            }
                        }
                    ]
                },
                "config": {
                    "blobSchedule": {
                        "Cancun": {
                            "target": "0x03",
                            "max": "0x06",
                            "baseFeeUpdateFraction": "0x32f0ed"
                        }
                    },
                    "chainid": "0x01"
                },
                "_info": {
                    "hash": "0x9c3a733f357d61086cdae51a0ddbd9cf268bcb4f8272ad54c8bddb415f413e8c",
                    "comment": "`execution-spec-tests` generated test",
                    "filling-transition-tool": "ethereum-spec-evm-resolver 0.0.5",
                    "description": "Dynamic Create2->Suicide->Create2 collision scenario.\n\n    Perform a CREATE2, make sure that the initcode sets at least a couple of storage keys,\n    then on a different call, in the same tx, perform a self-destruct.\n    Then:\n        a) on the same tx, attempt to recreate the contract   <=== Covered in this test\n            1) and create2 contract already in the state\n            2) and create2 contract is not in the state\n        b) on a different tx, attempt to recreate the contract\n    Perform a CREATE2, make sure that the initcode sets at least a couple of storage keys,\n    then in a different tx, perform a self-destruct.\n    Then:\n        a) on the same tx, attempt to recreate the contract\n        b) on a different tx, attempt to recreate the contract\n    Verify that the test case described in\n    https://lf-hyperledger.atlassian.net/wiki/spaces/BESU/pages/22156575/2024-01-06+Mainnet+Halting+Event",
                    "url": "https://github.com/ethereum/execution-spec-tests/blob/20770a694f440aeb4ea3ec86a3b9bc04d40146a3/tests/cancun/eip6780_selfdestruct/test_dynamic_create2_selfdestruct_collision.py#L30",
                    "fixture-format": "state_test",
                    "reference-spec": "https://github.com/ethereum/EIPs/blob/master/EIPS/eip-6780.md",
                    "reference-spec-version": "1b6a0e94cc47e859b9866e570391cf37dc55059a",
                    "eels-resolution": {
                        "git-url": "https://github.com/marioevz/execution-specs.git",
                        "branch": "forks/bpo1",
                        "commit": "3387e5f4aedfe99becfdc39b444d6371e25e0924"
                    }
                }
            }
        }
        "#;

        // The deserialization should be enough to validate the test.
        let parsed: HashMap<String, TestCase> =
            serde_json::from_str(data).expect("Parsing should succeed.");

        // Here's a couple sanity checks just in case:
        assert_eq!(parsed.len(), 2);
        let (_, test_case) = parsed.into_iter().next().unwrap();
        assert_eq!(test_case.env.current_gas_limit, 0x055d4a80);
        assert_eq!(test_case.transaction.nonce, 0);
        assert_eq!(test_case.config.chainid, 1);
    }
}
