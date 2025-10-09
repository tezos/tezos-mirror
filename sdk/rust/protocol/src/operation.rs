// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Tezos operations
/// The whole module is inspired of `src/proto_alpha/lib_protocol/operation_repr.ml` to represent the operation
use crate::contract::Contract;
use crate::entrypoint::Entrypoint;
use tezos_crypto_rs::{
    hash::{
        SmartRollupCommitmentHash, SmartRollupHash, SmartRollupStateHash, {BlockHash, BlsSignature},
    },
    public_key::PublicKey,
    public_key_hash::PublicKeyHash,
};
use tezos_data_encoding::{enc::BinWriter, nom::NomReader, types::Narith};

#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
pub struct UnsignedOperation {
    pub branch: BlockHash,
    pub content_list: OperationContentList,
}

#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
pub struct OperationContentList {
    pub contents: Vec<OperationContent>,
}

#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
#[encoding(tags = "u8")]
pub enum OperationContent {
    #[encoding(tag = 107)]
    Reveal(ManagerOperationContent<RevealContent>),
    #[encoding(tag = 108)]
    Transaction(ManagerOperationContent<TransactionContent>),
    #[encoding(tag = 109)]
    Origination(ManagerOperationContent<OriginationContent>),
    #[encoding(tag = 110)]
    Delegation(ManagerOperationContent<DelegationContent>),
    // SMART_ROLLUP_OPERATION_TAG_OFFSET = 200;
    #[encoding(tag = 202)] // SMART_ROLLUP_OPERATION_TAG_OFFSET + 2
    SmartRollupCement(ManagerOperationContent<SmartRollupCementContent>),
    #[encoding(tag = 203)] // SMART_ROLLUP_OPERATION_TAG_OFFSET + 3
    SmartRollupPublish(ManagerOperationContent<SmartRollupPublishContent>),
}

#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
pub struct ManagerOperationContent<Op> {
    pub source: PublicKeyHash,
    pub fee: Narith,
    pub counter: Narith,
    pub gas_limit: Narith,
    pub storage_limit: Narith,
    pub operation: Op,
}

#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
pub struct RevealContent {
    pub pk: PublicKey,
    #[encoding(dynamic)]
    pub proof: Option<BlsSignature>,
}

mod internal {
    use super::*;

    /// Encoded representation of the `TransactionContent` type used for binary
    /// serialization and deserialization via the `NomReader` and `BinWriter` traits.
    ///
    /// The `parameters` field is an `Option` to optimize encoding: `None` represents
    /// the default `Parameters` value. During decoding, `None` is restored as the
    /// default value.
    #[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
    pub struct EncodedTransactionContent<Amount, Destination, Param> {
        amount: Amount,
        destination: Destination,
        parameters: Option<Param>,
    }

    impl<'a> From<&'a TransactionContent>
        for EncodedTransactionContent<&'a Narith, &'a Contract, &'a Parameters>
    {
        fn from(c: &'a TransactionContent) -> Self {
            let TransactionContent {
                amount,
                destination,
                parameters,
            } = c;
            let parameters = if parameters == &Parameters::default() {
                None
            } else {
                Some(parameters)
            };
            Self {
                amount,
                destination,
                parameters,
            }
        }
    }

    impl From<EncodedTransactionContent<Narith, Contract, Parameters>> for TransactionContent {
        fn from(c: EncodedTransactionContent<Narith, Contract, Parameters>) -> Self {
            let EncodedTransactionContent {
                amount,
                destination,
                parameters,
            } = c;
            let parameters = parameters.unwrap_or_default();
            Self {
                amount,
                destination,
                parameters,
            }
        }
    }

    impl NomReader<'_> for TransactionContent {
        fn nom_read(input: &[u8]) -> tezos_data_encoding::nom::NomResult<Self> {
            nom::combinator::map(
                EncodedTransactionContent::nom_read,
                TransactionContent::from,
            )(input)
        }
    }

    impl BinWriter for TransactionContent {
        fn bin_write(&self, out: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
            EncodedTransactionContent::from(self).bin_write(out)
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct TransactionContent {
    pub amount: Narith,
    pub destination: Contract,
    pub parameters: Parameters,
}

#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
pub struct Parameters {
    pub entrypoint: Entrypoint,
    #[encoding(dynamic, bytes)]
    pub value: Vec<u8>,
}

impl Default for Parameters {
    fn default() -> Self {
        Parameters {
            entrypoint: Entrypoint::default(),
            // This is the binary representation of the Michelson "Unit" value as produced by
            // octez-client convert data "Unit" from Michelson to binary
            value: vec![0x03, 0x0b],
        }
    }
}

#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
pub struct OriginationContent {
    pub balance: Narith,
    pub delegate: Option<PublicKeyHash>,
    pub script: Script,
}

#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
pub struct Script {
    #[encoding(dynamic, bytes)]
    pub code: Vec<u8>,
    #[encoding(dynamic, bytes)]
    pub storage: Vec<u8>,
}

#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
pub struct DelegationContent {
    pub delegate: Option<PublicKeyHash>,
}

#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
pub struct SmartRollupCementContent {
    pub address: SmartRollupHash,
}

#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
pub struct SmartRollupCommitment {
    pub compressed_state: SmartRollupStateHash,
    pub inbox_level: i32,
    pub predecessor: SmartRollupCommitmentHash,
    pub number_of_ticks: i64,
}

#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
pub struct SmartRollupPublishContent {
    pub address: SmartRollupHash,
    pub commitment: SmartRollupCommitment,
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_crypto_rs::hash::HashTrait;

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "reveal",
      "source": "tz2WU9XW86EdgVQZrbPphjUZiRfXXssY9wEP",
      "fee": "31",
      "counter": "1005",
      "gas_limit": "89",
      "storage_limit": "7",
      "public_key": "sppk7bo7kcRyjajZaAqEfqdtCNx3wgizhJPFqaEuisncbDFMgn6v4iP"
    }'
    */
    #[test]
    fn reveal_encoding() {
        let pk =
            PublicKey::from_b58check("sppk7bo7kcRyjajZaAqEfqdtCNx3wgizhJPFqaEuisncbDFMgn6v4iP")
                .unwrap();
        let pkh = PublicKeyHash::from_b58check("tz2WU9XW86EdgVQZrbPphjUZiRfXXssY9wEP").unwrap();
        let fee = 31.into();
        let counter = 1005.into();
        let gas_limit = 89.into();
        let storage_limit = 7.into();
        let operation = OperationContent::Reveal(ManagerOperationContent {
            source: pkh,
            fee,
            counter,
            gas_limit,
            storage_limit,
            operation: RevealContent { pk, proof: None },
        });

        let encoded_operation = operation.to_bytes().unwrap();

        let bytes = hex::decode("6b01f3023970264e14502daa1db4324527bc464fe0fd1fed0759070103480fcf4241d5903bd5b9a71db63fc6784dc9e686acf0dac9b4305d54cb64294600").unwrap();
        assert_eq!(bytes, encoded_operation);

        let (bytes, decoded_operation) = OperationContent::nom_read(&encoded_operation).unwrap();
        assert_eq!(operation, decoded_operation);
        assert!(bytes.is_empty());
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "reveal",
      "source": "tz4N581kB4vkDANA7WTN6tcnBeCDawv1k4Cy",
      "fee": "127",
      "counter": "91",
      "gas_limit": "3250",
      "storage_limit": "0",
      "public_key": "BLpk1qUhA8WwcwKavKJb3nkPVeKj5FGCsruWQXyRpwypxe7ocbq6Npe4cjA2TLfqzgBKfRAtBajV",
      "proof": "BLsigAKrfg6QLipEvWQrhPRHbWo7x8YLFvteATg65gbvpfo9qKSabb5j92nozhecDkxvnvocv6q2WUUFvoKxkZuCaTNUUGpJPbc7igdf7MUi8fSovaLbWzZEDVGLjon7ZNPZPDpwf6h9jT"
    }'
    */
    #[test]
    fn reveal_with_proof_encoding() {
        let pk = PublicKey::from_b58check(
            "BLpk1qUhA8WwcwKavKJb3nkPVeKj5FGCsruWQXyRpwypxe7ocbq6Npe4cjA2TLfqzgBKfRAtBajV",
        )
        .unwrap();
        let pkh = PublicKeyHash::from_b58check("tz4N581kB4vkDANA7WTN6tcnBeCDawv1k4Cy").unwrap();
        let proof = BlsSignature::from_b58check("BLsigAKrfg6QLipEvWQrhPRHbWo7x8YLFvteATg65gbvpfo9qKSabb5j92nozhecDkxvnvocv6q2WUUFvoKxkZuCaTNUUGpJPbc7igdf7MUi8fSovaLbWzZEDVGLjon7ZNPZPDpwf6h9jT").unwrap();
        let fee = 127.into();
        let counter = 91.into();
        let gas_limit = 3250.into();
        let storage_limit = 0.into();
        let operation = OperationContent::Reveal(ManagerOperationContent {
            source: pkh,
            fee,
            counter,
            gas_limit,
            storage_limit,
            operation: RevealContent {
                pk,
                proof: Some(proof),
            },
        });

        let encoded_operation = operation.to_bytes().unwrap();

        let bytes = hex::decode("6b038f54fa9607db6d164b42bd23f3bfedc99460d4bc7f5bb21900039423b322ccff9b945501d28f743a0369583c1552d47dc69fa5d624bde1034df571103c62a969d5998d4682be60ca3e83ff000000609517ce0faabbf099b46a3b786f3ae0ef81cfdf4d89a8b0d1ff9216bf092a8e90987549cbc3234f21af4b29fe0a5f217c16d56336cdc26d5c364c961ca1f7ac44e9a778f9bab5695dd88f0a7cfbfa0582cb8cd7b5ef158b6b59abd576eb181ddc").unwrap();
        assert_eq!(bytes, encoded_operation);

        let (bytes, decoded_operation) = OperationContent::nom_read(&encoded_operation).unwrap();
        assert_eq!(operation, decoded_operation);
        assert!(bytes.is_empty());
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "transaction",
      "source": "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
      "fee": "405",
      "counter": "2",
      "gas_limit": "1380",
      "storage_limit": "0",
      "amount": "1000000",
      "destination": "KT1EY9XA4Z5tybQN5zmVUL5cntku1zTCBLTv",
      "parameters": {
        "entrypoint": "B",
        "value": {
          "string": "Hello"
        }
      }
    }'
    */
    #[test]
    fn transaction_encoding() {
        let operation = OperationContent::Transaction(ManagerOperationContent {
            source: PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap(),
            fee: 405_u64.into(),
            counter: 2_u64.into(),
            operation: TransactionContent {
                amount: 1_000_000_u64.into(),
                destination: Contract::from_b58check("KT1EY9XA4Z5tybQN5zmVUL5cntku1zTCBLTv")
                    .unwrap(),
                parameters: Parameters {
                    entrypoint: Entrypoint::try_from("B").unwrap(),
                    // octez-client convert data '"Hello"' from Michelson to binary
                    value: hex::decode("010000000548656c6c6f").unwrap(),
                },
            },
            gas_limit: 1380_u64.into(),
            storage_limit: 0_u64.into(),
        });

        let encoded_operation = operation.to_bytes().unwrap();

        let bytes = hex::decode("6c0002298c03ed7d454a101eb7022bc95f7e5f41ac78950302e40a00c0843d014151d57ddff98da8cd49f0f2cbf89465bcf267a400ffff01420000000a010000000548656c6c6f").unwrap();
        assert_eq!(bytes, encoded_operation);

        let (bytes, decoded_operation) = OperationContent::nom_read(&encoded_operation).unwrap();
        assert_eq!(operation, decoded_operation);
        assert!(bytes.is_empty());
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "transaction",
      "source": "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN",
      "fee": "987",
      "counter": "456",
      "gas_limit": "0",
      "storage_limit": "1405",
      "amount": "10",
      "destination": "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"
    }'
    */
    #[test]
    fn transaction_transfer_encoding() {
        let operation = OperationContent::Transaction(ManagerOperationContent {
            source: PublicKeyHash::from_b58check("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN").unwrap(),
            fee: 987.into(),
            counter: 456.into(),
            operation: TransactionContent {
                amount: 10.into(),
                destination: Contract::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
                    .unwrap(),
                parameters: Parameters::default(),
            },
            gas_limit: 0.into(),
            storage_limit: 1405.into(),
        });

        let encoded_operation = operation.to_bytes().unwrap();

        let bytes = hex::decode("6c00e7670f32038107a59a2b9cfefae36ea21f5aa63cdb07c80300fd0a0a000002298c03ed7d454a101eb7022bc95f7e5f41ac7800").unwrap();
        assert_eq!(bytes, encoded_operation);

        let (bytes, decoded_operation) = OperationContent::nom_read(&encoded_operation).unwrap();
        assert_eq!(operation, decoded_operation);
        assert!(bytes.is_empty());
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "transaction",
      "source": "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN",
      "fee": "987",
      "counter": "456",
      "gas_limit": "0",
      "storage_limit": "1405",
      "amount": "10",
      "destination": "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
      "parameters": {
        "entrypoint": "default",
        "value": {
          "prim": "Unit"
        }
      }
    }'
    */
    #[test]
    fn transaction_transfer_with_parameters_encoding() {
        let operation = OperationContent::Transaction(ManagerOperationContent {
            source: PublicKeyHash::from_b58check("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN").unwrap(),
            fee: 987.into(),
            counter: 456.into(),
            operation: TransactionContent {
                amount: 10.into(),
                destination: Contract::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
                    .unwrap(),
                parameters: Parameters::default(),
            },
            gas_limit: 0.into(),
            storage_limit: 1405.into(),
        });

        let encoded_operation = operation.to_bytes().unwrap();

        let bytes = hex::decode("6c00e7670f32038107a59a2b9cfefae36ea21f5aa63cdb07c80300fd0a0a000002298c03ed7d454a101eb7022bc95f7e5f41ac7800").unwrap();
        assert_eq!(bytes, encoded_operation);

        let (bytes, decoded_operation) = OperationContent::nom_read(&encoded_operation).unwrap();
        assert_eq!(operation, decoded_operation);
        assert!(bytes.is_empty());
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "transaction",
      "source": "tz3hqqamVC1G22LACFoMgcJeFKZgoGMFSfSn",
      "fee": "7",
      "counter": "4223",
      "gas_limit": "0",
      "storage_limit": "0",
      "amount": "0",
      "destination": "tz4Uzyxg26DJyM4pc1V2pUvLpdsR5jdyzYsZ",
      "parameters": {
        "entrypoint": "remove_delegate",
        "value": {
          "prim": "Unit"
        }
      }
    }'
    */
    #[test]
    fn transaction_fixed_entrypoint_encoding() {
        let operation = OperationContent::Transaction(ManagerOperationContent {
            source: PublicKeyHash::from_b58check("tz3hqqamVC1G22LACFoMgcJeFKZgoGMFSfSn").unwrap(),
            fee: 7.into(),
            counter: 4223.into(),
            operation: TransactionContent {
                amount: 0.into(),
                destination: Contract::from_b58check("tz4Uzyxg26DJyM4pc1V2pUvLpdsR5jdyzYsZ")
                    .unwrap(),
                parameters: Parameters {
                    entrypoint: Entrypoint::try_from("remove_delegate").unwrap(),
                    // octez-client convert data "Unit" from Michelson to binary
                    value: hex::decode("030b").unwrap(),
                },
            },
            gas_limit: 0.into(),
            storage_limit: 0.into(),
        });

        let encoded_operation = operation.to_bytes().unwrap();

        let bytes = hex::decode("6c02ebfd1371b542831b4be730161d08885c5312e44207ff200000000003db557924e5a295652eff2c1f141d5a5b72b9cc91ff0400000002030b").unwrap();
        assert_eq!(bytes, encoded_operation);

        let (bytes, decoded_operation) = OperationContent::nom_read(&encoded_operation).unwrap();
        assert_eq!(operation, decoded_operation);
        assert!(bytes.is_empty());
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "origination",
      "source": "tz1WQGXKa2h5edfELL1XMBrDmcb4qLHQye4U",
      "fee": "52",
      "counter": "703",
      "gas_limit": "5",
      "storage_limit": "36",
      "balance": "1924",
      "script": {
        "code": [
          { "prim": "parameter", "args": [ { "prim": "nat" } ] },
          { "prim": "storage", "args": [ { "prim": "nat" } ] },
          {
            "prim": "code",
            "args": [
              [
                { "prim": "UNPAIR" },
                { "prim": "ADD" },
                { "prim": "NIL", "args": [ { "prim": "operation" } ] },
                { "prim": "PAIR" }
              ]
            ]
          }
        ],
        "storage": { "int": "0" }
      }
    }'
    */
    #[test]
    fn origination_encoding() {
        let operation = OperationContent::Origination(ManagerOperationContent {
            source: PublicKeyHash::from_b58check("tz1WQGXKa2h5edfELL1XMBrDmcb4qLHQye4U").unwrap(),
            fee: 52.into(),
            counter: 703.into(),
            operation: OriginationContent {
                balance: 1924.into(),
                delegate: None,
                script: Script {
                    /*
                    octez-client convert script "
                              parameter nat;
                              storage nat;
                              code { UNPAIR; ADD; NIL operation; PAIR }
                            " from Michelson to binary
                     */
                    code: hex::decode(
                        "020000001905000362050103620502020000000a037a0312053d036d0342",
                    )
                    .unwrap(),
                    // octez-client convert data "0" from Michelson to binary
                    storage: hex::decode("0000").unwrap(),
                },
            },
            gas_limit: 5.into(),
            storage_limit: 36.into(),
        });

        let encoded_operation = operation.to_bytes().unwrap();

        let bytes = hex::decode("6d00760f1b125362fdf15f0e1093b1c6555b2dfdb8e434bf050524840f000000001e020000001905000362050103620502020000000a037a0312053d036d0342000000020000").unwrap();
        assert_eq!(bytes, encoded_operation);

        let (bytes, decoded_operation) = OperationContent::nom_read(&encoded_operation).unwrap();
        assert_eq!(operation, decoded_operation);
        assert!(bytes.is_empty());
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "origination",
      "source": "tz3dvgoUnnXoAP9MDqAwiZwRtvTFHcuDMxTR",
      "fee": "537",
      "counter": "63",
      "gas_limit": "2491",
      "storage_limit": "2",
      "balance": "25",
      "delegate": "tz4KAEXbRNNbgK2UqYrAsHXPmwFponv1mnXC",
      "script": {
        "code": [
          { "prim": "parameter", "args": [ { "prim": "unit" } ] },
          { "prim": "storage", "args": [ { "prim": "bool" } ] },
          {
            "prim": "code",
            "args": [
              [
                { "prim": "DROP" },
                { "prim": "SELF_ADDRESS" },
                { "prim": "SELF" },
                { "prim": "ADDRESS" },
                { "prim": "COMPARE" },
                { "prim": "EQ" },
                { "prim": "NIL", "args": [ { "prim": "operation" } ] },
                { "prim": "PAIR" }
              ]
            ]
          }
        ],
        "storage": { "prim": "False" }
      }
    }'
    */
    #[test]
    fn origination_delegated_contract_encoding() {
        let operation = OperationContent::Origination(ManagerOperationContent {
            source: PublicKeyHash::from_b58check("tz3dvgoUnnXoAP9MDqAwiZwRtvTFHcuDMxTR").unwrap(),
            fee: 537.into(),
            counter: 63_u64.into(),
            operation: OriginationContent {
                balance: 25_u64.into(),
                delegate: Some(
                    PublicKeyHash::from_b58check("tz4KAEXbRNNbgK2UqYrAsHXPmwFponv1mnXC").unwrap(),
                ),
                script: Script {
                    /*
                    octez-client convert script "
                              parameter unit;
                              storage bool;
                              code { DROP; SELF_ADDRESS; SELF; ADDRESS; COMPARE; EQ ; NIL operation; PAIR }
                            " from Michelson to binary
                     */
                    code: hex::decode("02000000210500036c0501035905020200000012032003770349035403190325053d036d0342").unwrap(),
                    // octez-client convert data "False" from Michelson to binary
                    storage: hex::decode("0303").unwrap(),
                },
            },
            gas_limit: 2491_u64.into(),
            storage_limit: 2_u64.into(),
        });

        let encoded_operation = operation.to_bytes().unwrap();

        let bytes = hex::decode("6d02c10752a0a4ac42262999b3f7300ae60ee62f963499043fbb130219ff036f641e94b212a840099872f3982315f2961a2d380000002602000000210500036c0501035905020200000012032003770349035403190325053d036d0342000000020303").unwrap();
        assert_eq!(bytes, encoded_operation);

        let (bytes, decoded_operation) = OperationContent::nom_read(&encoded_operation).unwrap();
        assert_eq!(operation, decoded_operation);
        assert!(bytes.is_empty());
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "delegation",
      "source": "tz4Quq6VcCeJVmCknjzTX5kcrhUzcMruoavF",
      "fee": "9000",
      "counter": "59",
      "gas_limit": "302",
      "storage_limit": "1024",
      "delegate": "tz1SUWNMC3hUdBRzzrbTbiuGPH1KFVifTQw7"
    }'
    */
    #[test]
    fn delegation_encoding() {
        let delegate =
            Some(PublicKeyHash::from_b58check("tz1SUWNMC3hUdBRzzrbTbiuGPH1KFVifTQw7").unwrap());
        let pkh = PublicKeyHash::from_b58check("tz4Quq6VcCeJVmCknjzTX5kcrhUzcMruoavF").unwrap();
        let fee = 9000.into();
        let counter = 59.into();
        let gas_limit = 302.into();
        let storage_limit = 1024.into();
        let operation = OperationContent::Delegation(ManagerOperationContent {
            source: pkh,
            fee,
            counter,
            gas_limit,
            storage_limit,
            operation: DelegationContent { delegate },
        });

        let mut encoded_operation = Vec::new();
        operation.bin_write(&mut encoded_operation).unwrap();

        let bytes = hex::decode("6e03ae7b7d713977a27ec643969f0c2e665ba9ad9aa1a8463bae028008ff004afbd2ede908e6700eb9b54352c1d2dceee8d0fe").unwrap();
        assert_eq!(bytes, encoded_operation);

        let (bytes, decoded_operation) = OperationContent::nom_read(&encoded_operation).unwrap();
        assert_eq!(operation, decoded_operation);
        assert!(bytes.is_empty());
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents" from '{
      "kind": "delegation",
      "source": "tz3S6P2LccJNrejt27KvJRb3BcuS5vGghkP8",
      "fee": "0",
      "counter": "1",
      "gas_limit": "0",
      "storage_limit": "0"
    }'
    */
    #[test]
    fn delegation_without_delegate_encoding() {
        let delegate = None;
        let pkh = PublicKeyHash::from_b58check("tz3S6P2LccJNrejt27KvJRb3BcuS5vGghkP8").unwrap();
        let fee = 0.into();
        let counter = 1.into();
        let gas_limit = 0.into();
        let storage_limit = 0.into();
        let operation = OperationContent::Delegation(ManagerOperationContent {
            source: pkh,
            fee,
            counter,
            gas_limit,
            storage_limit,
            operation: DelegationContent { delegate },
        });

        let mut encoded_operation = Vec::new();
        operation.bin_write(&mut encoded_operation).unwrap();

        let bytes = hex::decode("6e023f3b21e54e98db3845a2a5033d96877f7f9cea870001000000").unwrap();
        assert_eq!(bytes, encoded_operation);

        let (bytes, decoded_operation) = OperationContent::nom_read(&encoded_operation).unwrap();
        assert_eq!(operation, decoded_operation);
        assert!(bytes.is_empty());
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents_list" from '[
      {
        "kind": "reveal",
        "source": "tz1i4tRMvnyb672Wm7HGTi21MHyW4DSxSqvd",
        "fee": "264",
        "counter": "756631",
        "gas_limit": "155",
        "storage_limit": "0",
        "public_key": "edpkuwYfBr7zWiUmzwppY75v8ut7Xfg73qht3Hpbwfa9GVAmcFDUwT"
      },
      {
        "kind": "transaction",
        "source": "tz1i4tRMvnyb672Wm7HGTi21MHyW4DSxSqvd",
        "fee": "472",
        "counter": "756632",
        "gas_limit": "2169",
        "storage_limit": "277",
        "amount": "20000",
        "destination": "tz1Rc6wtS349fFTyuUhDXTXoBUZ9j7XiN61o"
      }
    ]'
    */
    #[test]
    fn basic_content_list_encoding() {
        let reveal = OperationContent::Reveal(ManagerOperationContent {
            source: PublicKeyHash::from_b58check("tz1i4tRMvnyb672Wm7HGTi21MHyW4DSxSqvd").unwrap(),
            fee: 264.into(),
            counter: 756631.into(),
            gas_limit: 155.into(),
            storage_limit: 0.into(),
            operation: RevealContent {
                pk: PublicKey::from_b58check(
                    "edpkuwYfBr7zWiUmzwppY75v8ut7Xfg73qht3Hpbwfa9GVAmcFDUwT",
                )
                .unwrap(),
                proof: None,
            },
        });

        let transaction = OperationContent::Transaction(ManagerOperationContent {
            source: PublicKeyHash::from_b58check("tz1i4tRMvnyb672Wm7HGTi21MHyW4DSxSqvd").unwrap(),
            fee: 472.into(),
            counter: 756632.into(),
            gas_limit: 2169.into(),
            storage_limit: 277.into(),
            operation: TransactionContent {
                amount: 20000.into(),
                destination: Contract::from_b58check("tz1Rc6wtS349fFTyuUhDXTXoBUZ9j7XiN61o")
                    .unwrap(),
                parameters: Parameters::default(),
            },
        });

        let content_list = OperationContentList {
            contents: vec![reveal, transaction],
        };

        let encoded_content_list = content_list.to_bytes().unwrap();

        let bytes = hex::decode("6b00f60642ed6e39e3a5ecf6e8313cd044323a315c03880297972e9b010000ab0795c320972960ffe5e222a163494294a8d9259acab890f96fcbd18fd158f8006c00f60642ed6e39e3a5ecf6e8313cd044323a315c03d80398972ef9109502a09c0100004173796fda78e7ca2512edbb87124834f358bca200").unwrap();
        assert_eq!(bytes, encoded_content_list);

        let (bytes, decoded_content_list) =
            OperationContentList::nom_read(&encoded_content_list).unwrap();
        assert_eq!(content_list, decoded_content_list);
        assert!(bytes.is_empty());
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.contents_list" from '[
      {
        "kind": "reveal",
        "source": "tz2DdCuWDUQffJxQLJQb6fk1MDAzKcP9LtMy",
        "fee": "469",
        "counter": "1",
        "gas_limit": "2169",
        "storage_limit": "277",
        "public_key": "sppk7aDbxVasHyAqzGXYKRHBXCq4xjs1qYD8MsCfgDRb2kb3Wqkssxf"
      },
      {
        "kind": "transaction",
        "source": "tz2DdCuWDUQffJxQLJQb6fk1MDAzKcP9LtMy",
        "fee": "733",
        "counter": "2",
        "gas_limit": "3250",
        "storage_limit": "0",
        "amount": "10000000",
        "destination": "KT1Cdu5oK8anYaDnXoDJMDnF9AKZS7fQ4fah",
        "parameters": {
          "entrypoint": "balance_of",
          "value": { "string": "tz3j873xGK219DrCKYL4usxM8EQgHUmDdbJB" }
        }
      },
      {
        "kind": "origination",
        "source": "tz2DdCuWDUQffJxQLJQb6fk1MDAzKcP9LtMy",
        "fee": "5871",
        "counter": "3",
        "gas_limit": "1728",
        "storage_limit": "1000000",
        "balance": "0",
        "script": {
          "code": [
            {
              "prim": "parameter",
              "args": [
                { "prim": "list", "args": [ { "prim": "bytes" } ] }
              ]
            },
            { "prim": "storage", "args": [ { "prim": "bytes" } ] },
            { "prim": "code",
              "args": [
                [
                  { "prim": "UNPAIR" },
                  { "prim": "SWAP" },
                  { "prim": "CONS" },
                  { "prim": "CONCAT" },
                  { "prim": "NIL", "args": [ { "prim": "operation" } ] },
                  { "prim": "PAIR" }
                ]
              ]
            }
          ],
          "storage": { "bytes": "dead" }
        }
      },
      {
        "kind": "delegation",
        "source": "tz2DdCuWDUQffJxQLJQb6fk1MDAzKcP9LtMy",
        "fee": "572",
        "counter": "4",
        "gas_limit": "928",
        "storage_limit": "0",
        "delegate": "tz1QfAJGaWpTnrDg8KMGkAtUxAavYnJ7pn13"
      }
    ]'
    */
    #[test]
    fn all_operations_content_list_encoding() {
        let reveal = OperationContent::Reveal(ManagerOperationContent {
            source: PublicKeyHash::from_b58check("tz2DdCuWDUQffJxQLJQb6fk1MDAzKcP9LtMy").unwrap(),
            fee: 469.into(),
            counter: 1.into(),
            gas_limit: 2169.into(),
            storage_limit: 277.into(),
            operation: RevealContent {
                pk: PublicKey::from_b58check(
                    "sppk7aDbxVasHyAqzGXYKRHBXCq4xjs1qYD8MsCfgDRb2kb3Wqkssxf",
                )
                .unwrap(),
                proof: None,
            },
        });

        let transaction = OperationContent::Transaction(ManagerOperationContent {
            source: PublicKeyHash::from_b58check("tz2DdCuWDUQffJxQLJQb6fk1MDAzKcP9LtMy").unwrap(),
            fee: 733.into(),
            counter: 2.into(),
            gas_limit: 3250.into(),
            storage_limit: 0.into(),
            operation: TransactionContent {
                amount: 10000000.into(),
                destination: Contract::from_b58check("KT1Cdu5oK8anYaDnXoDJMDnF9AKZS7fQ4fah")
                    .unwrap(),
                parameters: Parameters {
                    entrypoint: Entrypoint::try_from("balance_of").unwrap(),
                    // octez-client convert data '"tz3j873xGK219DrCKYL4usxM8EQgHUmDdbJB"' from Michelson to binary
                    value: hex::decode("0100000024747a336a38373378474b3231394472434b594c347573784d3845516748556d4464624a42").unwrap(),
                },
            },
        });

        let origination = OperationContent::Origination(ManagerOperationContent {
            source: PublicKeyHash::from_b58check("tz2DdCuWDUQffJxQLJQb6fk1MDAzKcP9LtMy").unwrap(),
            fee: 5871.into(),
            counter: 3.into(),
            gas_limit: 1728.into(),
            storage_limit: 1000000.into(),
            operation: OriginationContent {
                balance: 0.into(),
                delegate: None,
                script: Script {
                    /*
                    octez-client convert script "
                              parameter (list bytes);
                              storage bytes;
                              code { UNPAIR ; SWAP ; CONS ; CONCAT; NIL operation; PAIR}
                            " from Michelson to binary
                     */
                    code: hex::decode(
                        "020000001f0500055f0369050103690502020000000e037a034c031b031a053d036d0342",
                    )
                    .unwrap(),
                    // octez-client convert data "0xdead" from Michelson to binary
                    storage: hex::decode("0a00000002dead").unwrap(),
                },
            },
        });

        let delegation = OperationContent::Delegation(ManagerOperationContent {
            source: PublicKeyHash::from_b58check("tz2DdCuWDUQffJxQLJQb6fk1MDAzKcP9LtMy").unwrap(),
            fee: 572.into(),
            counter: 4.into(),
            gas_limit: 928.into(),
            storage_limit: 0.into(),
            operation: DelegationContent {
                delegate: Some(
                    PublicKeyHash::from_b58check("tz1QfAJGaWpTnrDg8KMGkAtUxAavYnJ7pn13").unwrap(),
                ),
            },
        });

        let content_list = OperationContentList {
            contents: vec![reveal, transaction, origination, delegation],
        };

        let encoded_content_list = content_list.to_bytes().unwrap();

        let bytes = hex::decode("6b013a3ea68f994f44c4790124ca15f6feda471f07c2d50301f910950201027844c87c7eda69da7248e3aa977247dbaff53c1fead305721cb0ba03001ae231006c013a3ea68f994f44c4790124ca15f6feda471f07c2dd0502b2190080ade204012c7806ce4f30169a32212537d5383b4810d0322c00ffff0a62616c616e63655f6f66000000290100000024747a336a38373378474b3231394472434b594c347573784d3845516748556d4464624a426d013a3ea68f994f44c4790124ca15f6feda471f07c2ef2d03c00dc0843d000000000024020000001f0500055f0369050103690502020000000e037a034c031b031a053d036d0342000000070a00000002dead6e013a3ea68f994f44c4790124ca15f6feda471f07c2bc0404a00700ff00370f64e2866fd59ff21271a806331465a54ccf79").unwrap();
        assert_eq!(bytes, encoded_content_list);

        let (bytes, decoded_content_list) =
            OperationContentList::nom_read(&encoded_content_list).unwrap();
        assert_eq!(content_list, decoded_content_list);
        assert!(bytes.is_empty());
    }

    /*
    jq -n '[range(1024) | {
        "kind": "reveal",
        "source": "tz1RwaPNHJVUTcY5cNPD6yK7PsyJ1KBQzAhF",
        "fee": "931",
        "counter": "236025",
        "gas_limit": "692",
        "storage_limit": "789",
        "public_key": "edpkvNBSpWssiLZYRdLwneLSLunkYjkWbdddeZjh4Y1ZZYJXbBphBX"
    }]' > /tmp/tezos-sdk-tests-many-operations.json
    octez-codec encode "023-PtSeouLo.operation.contents_list" from /tmp/tezos-sdk-tests-many-operations.json
    */
    #[test]
    fn many_operations_content_list_encoding() {
        const NB_REVEAL: usize = 1024;
        let reveal = OperationContent::Reveal(ManagerOperationContent {
            source: PublicKeyHash::from_b58check("tz1RwaPNHJVUTcY5cNPD6yK7PsyJ1KBQzAhF").unwrap(),
            fee: 931.into(),
            counter: 236025.into(),
            gas_limit: 692.into(),
            storage_limit: 789.into(),
            operation: RevealContent {
                pk: PublicKey::from_b58check(
                    "edpkvNBSpWssiLZYRdLwneLSLunkYjkWbdddeZjh4Y1ZZYJXbBphBX",
                )
                .unwrap(),
                proof: None,
            },
        });

        let content_list = OperationContentList {
            contents: vec![reveal; NB_REVEAL],
        };

        let encoded_content_list = content_list.to_bytes().unwrap();

        /*
        octez-codec encode "023-PtSeouLo.operation.contents" from '{
          "kind": "reveal",
          "source": "tz1RwaPNHJVUTcY5cNPD6yK7PsyJ1KBQzAhF",
          "fee": "931",
          "counter": "236025",
          "gas_limit": "692",
          "storage_limit": "789",
          "public_key": "edpkvNBSpWssiLZYRdLwneLSLunkYjkWbdddeZjh4Y1ZZYJXbBphBX"
        }'
        */
        let bytes = hex::decode("6b0045224867079dbc728ac2f7566bfd0adb31e2b266a307f9b30eb405950600e2f6f7b871734c41d121dc3351a6234433b8df8098447f8cf22c82e094a148c500".repeat(NB_REVEAL)).unwrap();

        assert_eq!(bytes, encoded_content_list);

        let (bytes, decoded_content_list) =
            OperationContentList::nom_read(&encoded_content_list).unwrap();
        assert_eq!(content_list, decoded_content_list);
        assert!(bytes.is_empty());
    }

    /*
    octez-codec encode "023-PtSeouLo.operation.unsigned" from '{
      "branch": "BLsyiLvAMFD7Yuk84hDuzLqgj7Sr8h4DTMWfxfE9TKtzff9LiFX",
      "contents": [
        {
          "kind": "reveal",
          "source": "tz4W54guExgiT71XKrNxkYQUdTKVQuJUq5mw",
          "fee": "711",
          "counter": "624692",
          "gas_limit": "4959",
          "storage_limit": "200",
          "public_key": "BLpk1wQsAuwyhy6z5rmRXiB3AQriT3Pes2gBQ9anuQwF99WypxHNyRjBnugF7sCVXCQgmPGdAHEr",
          "proof": "BLsigAUhX5J5G9uM94eTfL69paT7y6fiLQMLTxS9sWzHX8vX4317jZhxm2HCsJwdw61NAx3qH4WDzWdTMddkjZSUaRQtbU3L6nyhx2e94dKMCuiJ1Pg9RsM6ZTKjHYo6cNW7wK3k212SPN"
        },
        {
          "kind": "delegation",
          "source": "tz4W54guExgiT71XKrNxkYQUdTKVQuJUq5mw",
          "fee": "620",
          "counter": "624693",
          "gas_limit": "5032",
          "storage_limit": "138"
        },
        {
          "kind": "origination",
          "source": "tz4W54guExgiT71XKrNxkYQUdTKVQuJUq5mw",
          "fee": "285",
          "counter": "624694",
          "gas_limit": "7741",
          "storage_limit": "139",
          "balance": "50",
          "delegate": "tz2K1cCKb5HXoXX19yhmr8ChKbn4T8teV9Ei",
          "script": {
            "code": [
              { "prim": "parameter", "args": [ { "prim": "unit" } ] },
              { "prim": "storage", "args": [ { "prim": "mutez" } ] },
              { "prim": "code",
                "args": [
                  [
                    { "prim": "DROP" },
                    { "prim": "BALANCE" },
                    { "prim": "NIL", "args": [ { "prim": "operation" } ] },
                    { "prim": "PAIR" }
                  ]
                ]
              }
            ],
            "storage": { "int": "0" }
          }
        }
      ]
    }'
    */
    #[test]
    fn unsigned_encoding() {
        let reveal = OperationContent::Reveal(ManagerOperationContent {
            source: PublicKeyHash::from_b58check("tz4W54guExgiT71XKrNxkYQUdTKVQuJUq5mw").unwrap(),
            fee: 711.into(),
            counter: 624692.into(),
            gas_limit: 4959.into(),
            storage_limit: 200.into(),
            operation: RevealContent {
                pk: PublicKey::from_b58check(
                    "BLpk1wQsAuwyhy6z5rmRXiB3AQriT3Pes2gBQ9anuQwF99WypxHNyRjBnugF7sCVXCQgmPGdAHEr",
                )
                .unwrap(),
                proof: Some(BlsSignature::from_b58check("BLsigAUhX5J5G9uM94eTfL69paT7y6fiLQMLTxS9sWzHX8vX4317jZhxm2HCsJwdw61NAx3qH4WDzWdTMddkjZSUaRQtbU3L6nyhx2e94dKMCuiJ1Pg9RsM6ZTKjHYo6cNW7wK3k212SPN").unwrap()),
            },
        });

        let delegation = OperationContent::Delegation(ManagerOperationContent {
            source: PublicKeyHash::from_b58check("tz4W54guExgiT71XKrNxkYQUdTKVQuJUq5mw").unwrap(),
            fee: 620.into(),
            counter: 624693.into(),
            gas_limit: 5032.into(),
            storage_limit: 138.into(),
            operation: DelegationContent { delegate: None },
        });

        let origination = OperationContent::Origination(ManagerOperationContent {
            source: PublicKeyHash::from_b58check("tz4W54guExgiT71XKrNxkYQUdTKVQuJUq5mw").unwrap(),
            fee: 285.into(),
            counter: 624694.into(),
            gas_limit: 7741.into(),
            storage_limit: 139.into(),
            operation: OriginationContent {
                balance: 50.into(),
                delegate: Some(
                    PublicKeyHash::from_b58check("tz2K1cCKb5HXoXX19yhmr8ChKbn4T8teV9Ei").unwrap(),
                ),
                script: Script {
                    /*
                    octez-client convert script "
                              parameter unit;
                              storage mutez;
                              code {DROP; BALANCE; NIL operation; PAIR}
                            " from Michelson to binary
                     */
                    code: hex::decode(
                        "02000000190500036c0501036a0502020000000a03200315053d036d0342",
                    )
                    .unwrap(),
                    // octez-client convert data "0" from Michelson to binary
                    storage: hex::decode("0000").unwrap(),
                },
            },
        });

        let content_list = OperationContentList {
            contents: vec![reveal, delegation, origination],
        };

        let unsigned = UnsignedOperation {
            branch: BlockHash::from_b58check("BLsyiLvAMFD7Yuk84hDuzLqgj7Sr8h4DTMWfxfE9TKtzff9LiFX")
                .unwrap(),
            content_list,
        };

        let encoded_unsigned = unsigned.to_bytes().unwrap();

        let bytes = hex::decode("99b6629866d30896c198f26db69e0a937cbd0f2c3778cf13f6e977d94f46397b6b03e71335ab27e461cbada3a6d682580cc4e221f78ac705b49026df26c80103acdc542d7eadf36d6f3df4f1cdfa42a7238e89d8a8111f231a4234db9612066c7ae975b7185ab00da40faa07ad89783cff00000060990404e54f617bc3be20da2960744a67b874c446a2e673f028be7103878322710134714da0159196179e6738ea71ed100943b33c213672d40b1fe413b72d0f49805ecfe86939d06d9dcf3a5fcaff9f2f1d55c59a8c4ea2c944888a178b9673e26e03e71335ab27e461cbada3a6d682580cc4e221f78aec04b59026a8278a01006d03e71335ab27e461cbada3a6d682580cc4e221f78a9d02b69026bd3c8b0132ff017553df46bf9d1994760cfa8ca26e0e8a509d3f900000001e02000000190500036c0501036a0502020000000a03200315053d036d0342000000020000").unwrap();
        assert_eq!(bytes, encoded_unsigned);

        let (bytes, decoded_unsigned) = UnsignedOperation::nom_read(&encoded_unsigned).unwrap();
        assert_eq!(unsigned, decoded_unsigned);
        assert!(bytes.is_empty());
    }

    /// Test `smart_rollup_cement` encoding
    ///
    /// Generated with:
    /// ```sh
    /// octez-codec encode "023-PtSeouLo.operation.contents" from '{
    ///   "kind": "smart_rollup_cement",
    ///   "source": "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN",
    ///   "fee": "1234",
    ///   "counter": "789",
    ///   "gas_limit": "2000",
    ///   "storage_limit": "10",
    ///   "rollup": "sr1V6huFSUBUujzubUCg9nNXqpzfG9t4XD1h"
    /// }'
    /// ```
    #[test]
    fn smart_rollup_cement_encoding() {
        let operation = OperationContent::SmartRollupCement(ManagerOperationContent {
            source: PublicKeyHash::from_b58check("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN").unwrap(),
            fee: 1_234.into(),
            counter: 789.into(),
            gas_limit: 2_000.into(),
            storage_limit: 10.into(),
            operation: SmartRollupCementContent {
                address: SmartRollupHash::from_base58_check("sr1V6huFSUBUujzubUCg9nNXqpzfG9t4XD1h")
                    .unwrap(),
            },
        });

        let encoded_operation = operation.to_bytes().unwrap();

        let bytes = hex::decode("ca00e7670f32038107a59a2b9cfefae36ea21f5aa63cd2099506d00f0afceda8e679c698b26b5f6772411955354a81a05c").unwrap();
        assert_eq!(bytes, encoded_operation);

        let (bytes, decoded_operation) = OperationContent::nom_read(&encoded_operation).unwrap();
        assert_eq!(operation, decoded_operation);
        assert!(bytes.is_empty());
    }

    /// Test `smart_rollup_publish` encoding
    ///
    /// Generated with:
    ///
    /// ```sh
    /// octez-codec encode "023-PtSeouLo.operation.contents" from '{
    ///   "kind": "smart_rollup_publish",
    ///   "source": "tz4Quq6VcCeJVmCknjzTX5kcrhUzcMruoavF",
    ///   "fee": "4000",
    ///   "counter": "61",
    ///   "gas_limit": "4200",
    ///   "storage_limit": "0",
    ///   "rollup": "sr19fMYrr5C4qqvQqQrDSjtP31GcrWjodzvg",
    ///   "commitment": {
    ///     "compressed_state": "srs11ZWE34ur1d8j81Eqt68v2P5gFkP3hHms6kQ9Qo26j7ktDeu85y",
    ///     "inbox_level": 42,
    ///     "predecessor": "src12UJzB8mg7yU6nWPzicH7ofJbFjyJEbHvwtZdfRXi8DQHNp1LY8",
    ///     "number_of_ticks": "1234567890"
    ///   }
    /// }'
    /// ```
    #[test]
    fn smart_rollup_publish_encoding() {
        let operation = OperationContent::SmartRollupPublish(ManagerOperationContent {
            source: PublicKeyHash::from_b58check("tz4Quq6VcCeJVmCknjzTX5kcrhUzcMruoavF").unwrap(),
            fee: 4_000.into(),
            counter: 61.into(),
            gas_limit: 4_200.into(),
            storage_limit: 00.into(),
            operation: SmartRollupPublishContent {
                address: SmartRollupHash::from_base58_check("sr19fMYrr5C4qqvQqQrDSjtP31GcrWjodzvg")
                    .unwrap(),
                commitment: SmartRollupCommitment {
                    compressed_state: SmartRollupStateHash::from_base58_check(
                        "srs11ZWE34ur1d8j81Eqt68v2P5gFkP3hHms6kQ9Qo26j7ktDeu85y",
                    )
                    .unwrap(),
                    inbox_level: 42,
                    predecessor: SmartRollupCommitmentHash::from_base58_check(
                        "src12UJzB8mg7yU6nWPzicH7ofJbFjyJEbHvwtZdfRXi8DQHNp1LY8",
                    )
                    .unwrap(),
                    number_of_ticks: 1_234_567_890,
                },
            },
        });

        let encoded_operation = operation.to_bytes().unwrap();

        let bytes = hex::decode("cb03ae7b7d713977a27ec643969f0c2e665ba9ad9aa1a01f3de8200027b7e1d8fb4292cca7b57c065ac9f210fcf2250111111111111111111111111111111111111111111111111111111111111111110000002a000000000000000000000000000000000000000000000000000000000000000000000000499602d2").unwrap();
        assert_eq!(bytes, encoded_operation);

        let (bytes, decoded_operation) = OperationContent::nom_read(&encoded_operation).unwrap();
        assert_eq!(operation, decoded_operation);
        assert!(bytes.is_empty());
    }
}
