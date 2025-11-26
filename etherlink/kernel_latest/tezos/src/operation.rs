// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Tezos operations: this module defines the fragment of Tezos operations supported by Tezlink and how to serialize them.
/// The whole module is inspired of `src/proto_alpha/lib_protocol/operation_repr.ml` to represent the operation
use crate::enc_wrappers::{BlockHash, OperationHash};
use primitive_types::H256;
use rlp::Decodable;
use tezos_crypto_rs::blake2b::digest_256;
use tezos_crypto_rs::hash::{BlsSignature, SecretKeyEd25519, UnknownSignature};
use tezos_data_encoding::types::Narith;
use tezos_data_encoding::{
    enc::{BinError, BinWriter},
    nom::NomReader,
};
pub use tezos_protocol::operation::{
    Parameters,
    // Transfers are often called "transactions" in the protocol, we try
    // to consistently call them transfers to avoid confusion with the
    // Ethereum notion of transaction which is more generic as it also
    // encompasses the case of originations.
    TransactionContent as TransferContent,
};
use tezos_smart_rollup::types::{PublicKey, PublicKeyHash};
use thiserror::Error;

#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
pub struct RevealContent {
    pub pk: PublicKey,
    pub proof: Option<BlsSignature>,
}

// Original code is from sdk/rust/protocol/src/operation.rs
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

#[derive(Clone)]
pub enum OperationContent {
    Reveal(RevealContent),
    Transfer(TransferContent),
    Origination(OriginationContent),
}

// In Tezlink, we'll only support ManagerOperation so we don't
// have to worry about other operations
#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
pub struct ManagerOperation<C> {
    pub source: PublicKeyHash,
    pub fee: Narith,
    pub counter: Narith,
    pub gas_limit: Narith,
    pub storage_limit: Narith,
    pub operation: C,
}

#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
pub struct Operation {
    pub branch: BlockHash,
    pub content: Vec<ManagerOperationContent>,
    pub signature: UnknownSignature,
}

impl Operation {
    // The `rlp_append` function from the Encodable trait can't fail but `to_bytes`
    // return a result. To avoid unwrapping and risk a panic we're not implementing
    // the trait exactly, but we expose a serialization function.
    pub fn rlp_append(&self, s: &mut rlp::RlpStream) -> Result<(), BinError> {
        let bytes = self.to_bytes()?;
        s.append(&bytes);
        Ok(())
    }

    pub fn hash(&self) -> Result<OperationHash, BinError> {
        let serialized_op = self.to_bytes()?;
        let op_hash = digest_256(&serialized_op);
        Ok(OperationHash(H256::from_slice(&op_hash)))
    }
}

impl Decodable for Operation {
    fn decode(rlp: &rlp::Rlp) -> Result<Self, rlp::DecoderError> {
        let raw: Vec<u8> = rlp.as_val()?;
        Operation::nom_read_exact(&raw)
            .map_err(|_| rlp::DecoderError::Custom("Operation::try_from_bytes failed"))
    }
}

/**

There is a distance between the binary format of manager operations
and what we want to manipulate when applying them. The former is
imposed by the protocol and corresponds to this
ManagerOperationContent struct. The latter is
ManagerOperation<OperationContent> and is the input type of the
apply_operation function (in the lib.rs file of the tezos_execution
crate).

There are some fields common to all manager operations and some fields
specific to each kind of operation. A lot can be done about a manager
operation (in particular checking the signature and debiting the fees)
before we dispatch on the operation kind but the binary format starts
with the operation-kind tag, then the generic fields, and finally the
kind-specific fields.

*/
#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
#[encoding(tags = "u8")]
pub enum ManagerOperationContent {
    #[encoding(tag = 107)]
    Reveal(ManagerOperation<RevealContent>),
    #[encoding(tag = 108)]
    Transaction(ManagerOperation<TransferContent>),
    #[encoding(tag = 109)]
    Origination(ManagerOperation<OriginationContent>),
}

pub trait ManagerOperationField {
    fn gas_limit(&self) -> &Narith;
    fn source(&self) -> &PublicKeyHash;
}

impl ManagerOperationField for ManagerOperationContent {
    fn gas_limit(&self) -> &Narith {
        match self {
            ManagerOperationContent::Reveal(op) => &op.gas_limit,
            ManagerOperationContent::Transaction(op) => &op.gas_limit,
            ManagerOperationContent::Origination(op) => &op.gas_limit,
        }
    }

    fn source(&self) -> &PublicKeyHash {
        match self {
            ManagerOperationContent::Reveal(op) => &op.source,
            ManagerOperationContent::Transaction(op) => &op.source,
            ManagerOperationContent::Origination(op) => &op.source,
        }
    }
}

pub trait ManagerOperationContentConv: Sized {
    fn into_manager_operation_content(self) -> ManagerOperationContent;
    fn from_manager_operation_content(op: ManagerOperationContent) -> Self;
}

impl ManagerOperationContentConv for ManagerOperation<OperationContent> {
    fn into_manager_operation_content(self) -> ManagerOperationContent {
        let ManagerOperation {
            source,
            fee,
            counter,
            gas_limit,
            storage_limit,
            operation,
        } = self;
        match operation {
            OperationContent::Reveal(c) => {
                ManagerOperationContent::Reveal(ManagerOperation {
                    source,
                    fee,
                    counter,
                    gas_limit,
                    storage_limit,
                    operation: c,
                })
            }
            OperationContent::Transfer(c) => {
                ManagerOperationContent::Transaction(ManagerOperation {
                    source,
                    fee,
                    counter,
                    gas_limit,
                    storage_limit,
                    operation: c,
                })
            }
            OperationContent::Origination(c) => {
                ManagerOperationContent::Origination(ManagerOperation {
                    source,
                    fee,
                    counter,
                    gas_limit,
                    storage_limit,
                    operation: c,
                })
            }
        }
    }

    fn from_manager_operation_content(op: ManagerOperationContent) -> Self {
        match op {
            ManagerOperationContent::Reveal(ManagerOperation {
                source,
                fee,
                counter,
                gas_limit,
                storage_limit,
                operation: c,
            }) => ManagerOperation {
                source,
                fee,
                counter,
                gas_limit,
                storage_limit,
                operation: OperationContent::Reveal(c),
            },
            ManagerOperationContent::Transaction(ManagerOperation {
                source,
                fee,
                counter,
                gas_limit,
                storage_limit,
                operation: c,
            }) => ManagerOperation {
                source,
                fee,
                counter,
                gas_limit,
                storage_limit,
                operation: OperationContent::Transfer(c),
            },
            ManagerOperationContent::Origination(ManagerOperation {
                source,
                fee,
                counter,
                gas_limit,
                storage_limit,
                operation: c,
            }) => ManagerOperation {
                source,
                fee,
                counter,
                gas_limit,
                storage_limit,
                operation: OperationContent::Origination(c),
            },
        }
    }
}

pub fn serialize_unsigned_operation(
    branch: &BlockHash,
    content: &[ManagerOperationContent],
) -> Result<Vec<u8>, BinError> {
    // Watermark comes from `src/lib_crypto/signature_v2.ml`
    // The watermark for a ManagerOperation is always `Generic_operation`
    // encoded with `0x03`
    let watermark = 3_u8;

    let mut serialized_unsigned_operation = vec![watermark];

    branch.bin_write(&mut serialized_unsigned_operation)?;
    tezos_data_encoding::enc::list(ManagerOperationContent::bin_write)(
        content,
        &mut serialized_unsigned_operation,
    )?;

    Ok(serialized_unsigned_operation)
}

#[derive(Error, Debug)]
pub enum SignatureErrors {
    #[error("Signing failed with encoding error {0}")]
    BinError(#[from] BinError),
    #[error("Signing failed with cryptographic error {0}")]
    CryptoError(#[from] tezos_crypto_rs::CryptoError),
}

pub fn sign_operation(
    sk: &SecretKeyEd25519,
    branch: &BlockHash,
    content: &[ManagerOperationContent],
) -> Result<UnknownSignature, SignatureErrors> {
    let serialized_unsigned_operation = serialize_unsigned_operation(branch, content)?;

    let signature = sk.sign(serialized_unsigned_operation)?;

    Ok(signature.into())
}

#[cfg(test)]
fn make_dummy_operation(
    operation: OperationContent,
    signature: UnknownSignature,
) -> Operation {
    use crate::block::TezBlock;

    let branch = BlockHash::from(TezBlock::genesis_block_hash());

    // Public key hash in b58 for 0002298c03ed7d454a101eb7022bc95f7e5f41ac78
    let source = PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
        .expect("Public key hash conversion should succeeded");

    Operation {
        branch,
        content: vec![ManagerOperation {
            source,
            fee: 1_u64.into(),
            counter: 10_u64.into(),
            gas_limit: 68_u64.into(),
            storage_limit: 45_u64.into(),
            operation,
        }
        .into_manager_operation_content()],
        signature,
    }
}

#[cfg(test)]
pub fn make_dummy_reveal_operation() -> Operation {
    let pk = PublicKey::from_b58check(
        "edpkuT1qccDweCHnvgjLuNUHERpZmEaFZfbWvTzj2BxmTgQBZjaDFD",
    )
    .expect("Public key creation should have succeeded");

    let signature = UnknownSignature::from_base58_check("sigSPESPpW4p44JK181SmFCFgZLVvau7wsJVN85bv5ciigMu7WSRnxs9H2NydN5ecxKHJBQTudFPrUccktoi29zHYsuzpzBX").unwrap();

    make_dummy_operation(
        OperationContent::Reveal(RevealContent { pk, proof: None }),
        signature,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        encoding_test_data_helper::test_helpers::fetch_generated_data,
        operation::make_dummy_reveal_operation,
    };
    use mir::ast::michelson_address::Entrypoint;
    use pretty_assertions::assert_eq;
    use primitive_types::H256;
    use rlp::{Decodable, Rlp, RlpStream};
    use tezos_crypto_rs::{
        hash::{HashType, UnknownSignature},
        public_key::PublicKey,
    };
    use tezos_protocol::contract::Contract;
    use tezos_smart_rollup::types::PublicKeyHash;

    #[test]
    fn operation_rlp_roundtrip() {
        let operation = make_dummy_reveal_operation();
        let mut stream = RlpStream::new();
        operation
            .rlp_append(&mut stream)
            .expect("rlp_append should have succeeded");
        let bytes = stream.as_raw();
        let rlp = Rlp::new(bytes);
        let decoded_operation =
            Operation::decode(&rlp).expect("Decoding operation should have succeeded");
        assert_eq!(operation, decoded_operation);
    }

    #[test]
    fn test_reveal_operation_roundtrip_encoding() {
        let operation = make_dummy_reveal_operation();

        let bytes = operation
            .to_bytes()
            .expect("Encoding reveal operation should have succeeded");
        let operation_from_bytes = Operation::nom_read_exact(&bytes)
            .expect("Decoding reveal operation should have succeeded");

        assert_eq!(operation, operation_from_bytes);
    }

    #[test]
    fn tezos_compatibility_for_reveal() {
        // The goal of this test is to try to decode an encoding generated by 'octez-codec encode' command
        let branch_vec = HashType::b58check_to_hash(
            &HashType::BlockHash,
            "BKpbfCvh777DQHnXjU2sqHvVUNZ7dBAdqEfKkdw8EGSkD9LSYXb",
        )
        .unwrap();
        let branch = BlockHash::from(H256::from_slice(&branch_vec));
        let signature = UnknownSignature::from_base58_check("sigbQ5ZNvkjvGssJgoAnUAfY4Wvvg3QZqawBYB1j1VDBNTMBAALnCzRHWzer34bnfmzgHg3EvwdzQKdxgSghB897cono6gbQ").unwrap();
        let expected_operation = Operation {
            branch,
            content: vec![ManagerOperationContent::Reveal(ManagerOperation {
                source: PublicKeyHash::from_b58check(
                    "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
                )
                .unwrap(),
                fee: 33_u64.into(),
                counter: 732_u64.into(),
                operation: RevealContent {
                    pk: PublicKey::from_b58check(
                        "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
                    )
                    .unwrap(),
                    proof: None,
                },
                gas_limit: 9451117u64.into(),
                storage_limit: 57024931117u64.into(),
            })],
            signature,
        };

        let operation_bytes = fetch_generated_data("S023", "operation", "reveal");

        let operation = Operation::nom_read_exact(&operation_bytes)
            .expect("Decoding operation should have succeeded");

        assert_eq!(operation, expected_operation);
    }

    #[test]
    fn tezos_compatibility_for_transfer() {
        // The goal of this test is to try to decode an encoding generated by 'octez-codec encode' command
        let branch_vec = HashType::b58check_to_hash(
            &HashType::BlockHash,
            "BKpbfCvh777DQHnXjU2sqHvVUNZ7dBAdqEfKkdw8EGSkD9LSYXb",
        )
        .unwrap();
        let branch = BlockHash::from(H256::from_slice(&branch_vec));
        let signature = UnknownSignature::from_base58_check("sigbQ5ZNvkjvGssJgoAnUAfY4Wvvg3QZqawBYB1j1VDBNTMBAALnCzRHWzer34bnfmzgHg3EvwdzQKdxgSghB897cono6gbQ").unwrap();
        let expected_operation = Operation {
            branch,
            content: vec![ManagerOperationContent::Transaction(ManagerOperation {
                source: PublicKeyHash::from_b58check(
                    "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
                )
                .unwrap(),
                fee: 33u64.into(),
                counter: 732u64.into(),
                operation: TransferContent {
                    amount: 407u64.into(),
                    destination: Contract::from_b58check(
                        "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
                    )
                    .unwrap(),
                    parameters: Parameters::default(),
                },
                gas_limit: 9451117u64.into(),
                storage_limit: 57024931117u64.into(),
            })],
            signature,
        };

        let operation_bytes = fetch_generated_data(
            "S023",
            "operation",
            "operation-transaction-to-implicit",
        );

        let operation = Operation::nom_read_exact(&operation_bytes)
            .expect("Decoding operation should have succeeded");

        assert_eq!(operation, expected_operation);

        // Also test the encoding
        let kernel_bytes = expected_operation
            .to_bytes()
            .expect("Operation encoding should have succeed");

        assert_eq!(operation_bytes, kernel_bytes)
    }

    #[test]
    fn tezos_compatibility_for_smart_contract_address() {
        let address_bytes =
            fetch_generated_data("S023", "contract", "contract-originated");

        let expected_address =
            Contract::from_b58check("KT1DieU51jzXLerQx5AqMCiLC1SsCeM8yRat").unwrap();

        let (_, address) = Contract::nom_read(&address_bytes)
            .expect("Decoding operation should have succeeded");

        assert_eq!(address, expected_address);
    }

    // The operation below is the transfer to a smart contract using the mockup mode of octez-client as follows:
    //  $ alias mockup-client='./octez-client --mode mockup --base-dir /tmp/mockup --protocol PsRiotuma'
    //  $ rm -rf /tmp/mockup
    //  $ mockup-client create mockup
    //  $ mockup-client originate contract my-counter transferring 0 from 'tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx' running michelson_test_scripts/entrypoints/simple_entrypoints.tz --burn-cap 0.1 --force
    //  # Contract created on KT1EY9XA4Z5tybQN5zmVUL5cntku1zTCBLTv
    //  $ TRANSFER_HEX=$(mockup-client transfer 1 from 'tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx' to 'KT1EY9XA4Z5tybQN5zmVUL5cntku1zTCBLTv'
    // --entrypoint 'B' --arg '"Hello"' --burn-cap 0.1 --dry-run | grep Operation: | cut -d x -f 2)
    // Tezos address added: tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx
    //  $ octez-codec decode 022-PsRiotum.operation from "$TRANSFER_HEX"
    //  $ mockup-client convert data '"Hello"' from michelson to binary
    //  the result has been stored in json format in smart_contract_transfer.json
    #[test]
    fn tezos_compatibility_for_smart_contract_transfer() {
        let branch_vec = HashType::b58check_to_hash(
            &HashType::BlockHash,
            "BKpbfCvh777DQHnXjU2sqHvVUNZ7dBAdqEfKkdw8EGSkD9LSYXb",
        )
        .unwrap();

        let branch = BlockHash::from(H256::from_slice(&branch_vec));
        let signature = UnknownSignature::from_base58_check("sigbQ5ZNvkjvGssJgoAnUAfY4Wvvg3QZqawBYB1j1VDBNTMBAALnCzRHWzer34bnfmzgHg3EvwdzQKdxgSghB897cono6gbQ").unwrap();

        let expected_operation = Operation {
            branch,
            content: vec![ManagerOperationContent::Transaction(ManagerOperation {
                source: PublicKeyHash::from_b58check(
                    "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
                )
                .unwrap(),
                fee: 33_u64.into(),
                counter: 732_u64.into(),
                operation: TransferContent {
                    amount: 407u64.into(),
                    destination: Contract::from_b58check(
                        "KT1DieU51jzXLerQx5AqMCiLC1SsCeM8yRat",
                    )
                    .unwrap(),
                    parameters: Parameters {
                        entrypoint: Entrypoint::try_from("action").unwrap(),
                        value: vec![0x02, 0x00, 0x00, 0x00, 0x02, 0x03, 0x4f],
                    },
                },
                gas_limit: 9451117u64.into(),
                storage_limit: 57024931117u64.into(),
            })],
            signature,
        };

        let operation_bytes = fetch_generated_data(
            "S023",
            "operation",
            "operation-transaction-to-originated",
        );

        let operation = Operation::nom_read_exact(&operation_bytes)
            .expect("Decoding operation should have succeeded");

        assert_eq!(operation, expected_operation);

        // Also test the encoding
        let kernel_bytes = expected_operation
            .to_bytes()
            .expect("Operation encoding should have succeed");

        assert_eq!(operation_bytes, kernel_bytes)
    }

    // The operation below is the batch of a reveal and a transfer using the mockup mode of octez-client as follows:
    //  $ alias mockup-client='octez-client --mode mockup --base-dir /tmp/mockup --protocol
    //  PtSeouLo'
    //  $ mockup-client create mockup
    //  $ mockup-client import secret key alice unencrypted:edsk44ifgGvYJW7zEUasv156yPgVSUbNocwzXy4eMXjV2BSPBvQv3A
    //  $ mockup-client transfer 2 from bootstrap1 to alice --burn-cap 1
    //  $ BATCH_HEX=$(mockup-client transfer 1 from alice to bootstrap1 --burn-cap 1 --dry-run | grep Operation: | cut -d x -f 2)
    //  $ octez-codec decode 023-PtSeouLo.operation from "$BATCH_HEX"
    // the result has been stored in json format to be used as a regression test
    #[test]
    fn tezos_compatibility_for_reveal_transfer_batch() {
        // The goal of this test is to try to decode an encoding generated by 'octez-codec encode' command
        let branch_vec = HashType::b58check_to_hash(
            &HashType::BlockHash,
            "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU",
        )
        .unwrap();
        let branch = BlockHash::from(H256::from_slice(&branch_vec));
        let signature = UnknownSignature::from_base58_check("sigcBAU3AtdyxrMsZ4ScgFRWfTh66dD7mMNXQ2KmGP9y125hiJFgKtgcjmi3jVmUB5ytLjrU3xY6EVTveyfb4XcBvxNvCUDi").unwrap();
        let expected_operation = Operation {
            branch,
            content: vec![
                ManagerOperationContent::Reveal(ManagerOperation {
                    source: PublicKeyHash::from_b58check(
                        "tz1cckAZtxYwxAfwQuHnabTWfbp2ScWobxHH",
                    )
                    .unwrap(),
                    fee: 276_u64.into(),
                    counter: 2_u64.into(),
                    operation: RevealContent {
                        pk: PublicKey::from_b58check(
                            "edpkuqNrmPPcy2S3G1uKYnxmg7Gov3c8q7AABKRs9EtTVtfDg5Fu7R",
                        )
                        .unwrap(),
                        proof: None,
                    },
                    gas_limit: 171_u64.into(),
                    storage_limit: 0_u64.into(),
                }),
                ManagerOperationContent::Transaction(ManagerOperation {
                    source: PublicKeyHash::from_b58check(
                        "tz1cckAZtxYwxAfwQuHnabTWfbp2ScWobxHH",
                    )
                    .unwrap(),
                    fee: 365_u64.into(),
                    counter: 3_u64.into(),
                    operation: TransferContent {
                        amount: 1000000_u64.into(),
                        destination: Contract::from_b58check(
                            "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
                        )
                        .unwrap(),
                        parameters: Parameters::default(),
                    },
                    gas_limit: 2101_u64.into(),
                    storage_limit: 0_u64.into(),
                }),
            ],
            signature,
        };

        let operation_bytes = fetch_generated_data(
            "S023",
            "operation",
            "operation-batch-reveal-transaction",
        );

        let operation = Operation::nom_read_exact(&operation_bytes)
            .expect("Decoding operation should have succeeded");

        assert_eq!(operation, expected_operation);

        // Also test the encoding
        let kernel_bytes = expected_operation
            .to_bytes()
            .expect("Operation encoding should have succeed");

        assert_eq!(operation_bytes, kernel_bytes);
    }

    #[test]
    fn origination_encoding() {
        let branch_vec = HashType::b58check_to_hash(
            &HashType::BlockHash,
            "BKpbfCvh777DQHnXjU2sqHvVUNZ7dBAdqEfKkdw8EGSkD9LSYXb",
        )
        .unwrap();
        let branch = BlockHash::from(H256::from_slice(&branch_vec));
        let signature = UnknownSignature::from_base58_check("sigbQ5ZNvkjvGssJgoAnUAfY4Wvvg3QZqawBYB1j1VDBNTMBAALnCzRHWzer34bnfmzgHg3EvwdzQKdxgSghB897cono6gbQ").unwrap();
        let expected_operation = Operation {
            branch,
            content: vec![
ManagerOperationContent::Origination(ManagerOperation {
            source: PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
                .unwrap(),
            fee: 33.into(),
            counter: 732.into(),
            operation: OriginationContent {
                balance: 84143.into(),
                delegate: Some(PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap()),
                script: Script {
                    /*
                    octez-client convert script "
                              parameter string;
                              storage (option string);
                              code { CAR; SOME; NIL operation; PAIR }
                            " from Michelson to binary
                     */
                    code: hex::decode("020000001b050003680501056303680502020000000a03160346053d036d0342",
                    )
                    .unwrap(),
                    // octez-client convert data '{ "test" }' from Michelson to binary
                    storage: hex::decode("0200000009010000000474657374").unwrap(),
                },
            },
            gas_limit: 9451117.into(),
            storage_limit: 57024931117.into(),
})],
            signature,
        };

        let operation_bytes =
            fetch_generated_data("S023", "operation", "operation-origination");

        let (bytes, decoded_operation) = Operation::nom_read(&operation_bytes).unwrap();
        assert_eq!(expected_operation, decoded_operation);
        assert!(bytes.is_empty());
    }
}
