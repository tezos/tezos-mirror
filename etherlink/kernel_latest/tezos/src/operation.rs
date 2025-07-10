// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Tezos operations: this module defines the fragment of Tezos operations supported by Tezlink and how to serialize them.
/// The whole module is inspired of `src/proto_alpha/lib_protocol/operation_repr.ml` to represent the operation
use crate::enc_wrappers::{BlockHash, OperationHash};
use mir::ast::michelson_address::entrypoint;
use nom::error::{ErrorKind, ParseError};
use nom::Finish;
use primitive_types::H256;
use rlp::Decodable;
use tezos_crypto_rs::blake2b::digest_256;
use tezos_crypto_rs::hash::UnknownSignature;
use tezos_data_encoding::types::Narith;
use tezos_data_encoding::{
    enc::{BinError, BinWriter},
    nom::{error::DecodeError, NomError, NomReader},
};
use tezos_smart_rollup::types::{Contract, PublicKey, PublicKeyHash};

#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
pub struct Parameter {
    pub entrypoint: entrypoint::Entrypoint,
    #[encoding(dynamic, bytes)]
    pub value: Vec<u8>,
}

#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
pub struct RevealContent {
    pub pk: PublicKey,
}

/// Transfers are often called "transactions" in the protocol, we try
/// to consistently call them transfers to avoid confusion with the
/// Ethereum notion of transaction which is more generic as it also
/// encompasses the case of originations.
#[derive(PartialEq, Debug, Clone, NomReader, BinWriter)]
pub struct TransferContent {
    pub amount: Narith,
    pub destination: Contract,
    pub parameters: Option<Parameter>,
}

pub enum OperationContent {
    Reveal(RevealContent),
    Transfer(TransferContent),
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

#[derive(PartialEq, Debug, NomReader, BinWriter)]
pub struct Operation {
    pub branch: BlockHash,
    pub content: ManagerOperationContent,
    pub signature: UnknownSignature,
}

impl Operation {
    pub fn to_bytes(&self) -> Result<Vec<u8>, BinError> {
        let mut data = Vec::new();
        self.bin_write(&mut data)?;
        Ok(data)
    }

    pub fn try_from_bytes(data: &[u8]) -> Result<Self, DecodeError<&[u8]>> {
        let (remaining, operation) = Self::nom_read(data).finish()?;
        if !remaining.is_empty() {
            return Err(NomError::from_error_kind(remaining, ErrorKind::NonEmpty));
        }
        Ok(operation)
    }
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
        Operation::try_from_bytes(&raw)
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
    Transfer(ManagerOperation<TransferContent>),
}

impl From<ManagerOperation<OperationContent>> for ManagerOperationContent {
    fn from(op: ManagerOperation<OperationContent>) -> Self {
        let ManagerOperation {
            source,
            fee,
            counter,
            gas_limit,
            storage_limit,
            operation,
        } = op;
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
                ManagerOperationContent::Transfer(ManagerOperation {
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
}

impl From<ManagerOperationContent> for ManagerOperation<OperationContent> {
    fn from(op: ManagerOperationContent) -> Self {
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
            ManagerOperationContent::Transfer(ManagerOperation {
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
        }
    }
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
        content: ManagerOperation {
            source,
            fee: 1_u64.into(),
            counter: 10_u64.into(),
            gas_limit: 68_u64.into(),
            storage_limit: 45_u64.into(),
            operation,
        }
        .into(),
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

    make_dummy_operation(OperationContent::Reveal(RevealContent { pk }), signature)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::operation::make_dummy_reveal_operation;
    use primitive_types::H256;
    use rlp::{Decodable, Rlp, RlpStream};
    use tezos_crypto_rs::{
        hash::{HashType, UnknownSignature},
        public_key::PublicKey,
    };
    use tezos_smart_rollup::types::{Contract, PublicKeyHash};

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
        let operation_from_bytes = Operation::try_from_bytes(&bytes)
            .expect("Decoding reveal operation should have succeeded");

        assert_eq!(operation, operation_from_bytes);
    }

    // The operation below is the reveal produced by the following secret key: edsk44ifgGvYJW7zEUasv156yPgVSUbNocwzXy4eMXjV2BSPBvQv3A. We produced it using the mockup mode of octez-client as follows:
    //  $ alias mockup-client='octez-client --mode mockup --base-dir /tmp/mockup --protocol PsQuebec'
    //  $ mockup-client create mockup
    //  $ mockup-client import secret key alice unencrypted:edsk44ifgGvYJW7zEUasv156yPgVSUbNocwzXy4eMXjV2BSPBvQv3A
    //  $ mockup-client transfer 1 from bootstrap1 to alice --burn-cap 1
    //  $ REVEAL_HEX=$(mockup-client reveal key for alice --dry-run | grep Operation: | cut -d x -f 2)
    //  $ octez-codec decode 021-PsQuebec.operation from "$REVEAL_HEX"
    #[test]
    fn tezos_compatibility_for_reveal() {
        // The goal of this test is to try to decode an encoding generated by 'octez-codec encode' command
        let branch_vec = HashType::b58check_to_hash(
            &HashType::BlockHash,
            "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU",
        )
        .unwrap();
        let branch = BlockHash::from(H256::from_slice(&branch_vec));
        let signature = UnknownSignature::from_base58_check("sigRs6WkPHqKEuEhMQTmjhMZWn3b7TzYNXMozAaEHty7amNPa1Cw9QPQa84mN7kuBue3uwjxCUyHeaMeaY99Hq11GQ4jCx4x").unwrap();
        let expected_operation = Operation {
            branch,
            content: ManagerOperationContent::Reveal(ManagerOperation {
                source: PublicKeyHash::from_b58check(
                    "tz1cckAZtxYwxAfwQuHnabTWfbp2ScWobxHH",
                )
                .unwrap(),
                fee: 274_u64.into(),
                counter: 2_u64.into(),
                operation: RevealContent {
                    pk: PublicKey::from_b58check(
                        "edpkuqNrmPPcy2S3G1uKYnxmg7Gov3c8q7AABKRs9EtTVtfDg5Fu7R",
                    )
                    .unwrap(),
                },
                gas_limit: 169_u64.into(),
                storage_limit: 0_u64.into(),
            }),
            signature,
        };

        // This bytes sequence comes from the command just above the test
        let operation_bytes = hex::decode("8fcf233671b6a04fcf679d2a381c2544ea6c1ea29ba6157776ed842426e5cab86b00ba3bed311a5d7b06dc4daf3c94c5c406927e4bcf920202a90100009d05b06ea36a6ad464d94dc07a38b77b80b577c1ae51bbd8d20105cd5aed496c1da02ac8f6e1541c363874bcde7e90e1f959c8f28ab52ec3fdbbf7d54b1dad4004f2b70da27ce35de18d77ea9efee413b5fb2b2a858be4d95e45acfe47a73b0d").unwrap();

        let operation = Operation::try_from_bytes(&operation_bytes)
            .expect("Decoding operation should have succeeded");

        assert_eq!(operation, expected_operation);
    }

    // The operation below is the transfer using the mockup mode of octez-client as follows:
    //  $ alias mockup-client='octez-client --mode mockup --base-dir /tmp/mockup --protocol PsQuebec'
    //  $ mockup-client create mockup
    //  $ TRANSFER_HEX=$(mockup-client transfer 1 from bootstrap2 to bootstrap1 --burn-cap 1 --dry-run | grep Operation: | cut -d x -f 2)
    //  $ octez-codec decode 021-PsQuebec.operation from "$TRANSFER_HEX"
    #[test]
    fn tezos_compatibility_for_transfer() {
        // The goal of this test is to try to decode an encoding generated by 'octez-codec encode' command
        let branch_vec = HashType::b58check_to_hash(
            &HashType::BlockHash,
            "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU",
        )
        .unwrap();
        let branch = BlockHash::from(H256::from_slice(&branch_vec));
        let signature = UnknownSignature::from_base58_check("sigT4yGRRhiMZCjGigdhopaXkshKrwDbYrPw3jGFZGkjpvpT57a6KmLa4mFVKBTNHR8NrmyMEt9Pgusac5HLqUoJie2MB5Pd").unwrap();
        let expected_operation = Operation {
            branch,
            content: ManagerOperationContent::Transfer(ManagerOperation {
                source: PublicKeyHash::from_b58check(
                    "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN",
                )
                .unwrap(),
                fee: 267_u64.into(),
                counter: 1_u64.into(),
                operation: TransferContent {
                    amount: 1000000_u64.into(),
                    destination: Contract::from_b58check(
                        "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
                    )
                    .unwrap(),
                    parameters: None,
                },
                gas_limit: 169_u64.into(),
                storage_limit: 0_u64.into(),
            }),
            signature,
        };

        // This bytes sequence comes from the command just above the test
        let operation_bytes = hex::decode("8fcf233671b6a04fcf679d2a381c2544ea6c1ea29ba6157776ed842426e5cab86c00e7670f32038107a59a2b9cfefae36ea21f5aa63c8b0201a90100c0843d000002298c03ed7d454a101eb7022bc95f7e5f41ac780026d58f30f5f8caf70878ad4efc82d71cff01b76e584958411e5a89ea2a8908e37ffc28f0af92fa651c32f6cc7362d9c735344d590360864fbf0b156c3443b108").unwrap();

        let operation = Operation::try_from_bytes(&operation_bytes)
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
        // This test checks that deserialization of contract addresses is compatible with
        // Tezos. The tested address is "KT1Q36KWPSba7dHsH5E4ZsQHehrChc51e19d" and was
        // serialized using the following command:
        // $ octez-client normalize data '"KT1Q36KWPSba7dHsH5E4ZsQHehrChc51e19d"' of type address --unparsing-mode Optimized
        let address_bytes =
            hex::decode("01a9845b61ac052cdd0428af72a35bf75151dc754800").unwrap();
        let expected_address =
            Contract::from_b58check("KT1Q36KWPSba7dHsH5E4ZsQHehrChc51e19d").unwrap();

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
    #[test]
    fn tezos_compatibility_for_smart_contract_transfer() {
        let branch_vec = HashType::b58check_to_hash(
            &HashType::BlockHash,
            "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU",
        )
        .unwrap();

        let branch = BlockHash::from(H256::from_slice(&branch_vec));
        let signature = UnknownSignature::from_base58_check("sigWG2yRsgHD3gFcD4HiXuTFckRQsLxyAedcsDW3aryutXBhWZ3ek3AbyrRHh3Zt8ZZxkEAtVUKzCRqTjMfj6xvP9HtVskuk").unwrap();

        let expected_operation = Operation {
            branch,
            content: ManagerOperationContent::Transfer(ManagerOperation {
                source: PublicKeyHash::from_b58check(
                    "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
                )
                .unwrap(),
                fee: 405_u64.into(),
                counter: 2_u64.into(),
                operation: TransferContent {
                    amount: 1_000_000_u64.into(),
                    destination: Contract::from_b58check(
                        "KT1EY9XA4Z5tybQN5zmVUL5cntku1zTCBLTv",
                    )
                    .unwrap(),
                    parameters: Some(Parameter {
                        entrypoint: entrypoint::Entrypoint::try_from("B").unwrap(),
                        value: vec![
                            0x01, 0x00, 0x00, 0x00, 0x05, 0x48, 0x65, 0x6c, 0x6c, 0x6f,
                        ],
                    }),
                },
                gas_limit: 1380_u64.into(),
                storage_limit: 0_u64.into(),
            }),
            signature,
        };

        // This bytes sequence comes from the command just above the test
        let operation_bytes = hex::decode("8fcf233671b6a04fcf679d2a381c2544ea6c1ea29ba6157776ed842426e5cab86c0002298c03ed7d454a101eb7022bc95f7e5f41ac78950302e40a00c0843d014151d57ddff98da8cd49f0f2cbf89465bcf267a400ffff01420000000a010000000548656c6c6f3f391b739b8583427a69f0879cc5c5fd30bced4fcfc680fc37a39960e82dec2efbba8feae60b24f7fb4fdb4e553d9d9a34ac9c93b9e966da21b37ece0d63b00c").unwrap();

        let operation = Operation::try_from_bytes(&operation_bytes)
            .expect("Decoding operation should have succeeded");

        assert_eq!(operation, expected_operation);

        // Also test the encoding
        let kernel_bytes = expected_operation
            .to_bytes()
            .expect("Operation encoding should have succeed");

        assert_eq!(operation_bytes, kernel_bytes)
    }
}
