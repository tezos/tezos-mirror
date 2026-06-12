// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Tezos operations: this module defines the fragment of Tezos operations supported by Tezlink and how to serialize them.
/// The whole module is inspired of `src/proto_alpha/lib_protocol/operation_repr.ml` to represent the operation
use crate::operation_result::ValidityError;
use rlp::{Decodable, Encodable};
use tezos_crypto_rs::blake2b::digest_256;
use tezos_crypto_rs::hash::{
    BlockHash, HashType, OperationHash, SecretKeyEd25519, UnknownSignature,
};
use tezos_data_encoding::types::Narith;
use tezos_data_encoding::{
    enc::{BinError, BinResult, BinWriter},
    nom::{self as tezos_nom, NomReader, NomResult},
};
pub use tezos_protocol::operation::{
    ManagerOperationContent as ManagerOperation,
    OperationContent as ManagerOperationContent,
    OriginationContent,
    Parameters,
    RevealContent,
    Script,
    // Transfers are often called "transactions" in the protocol, we try
    // to consistently call them transfers to avoid confusion with the
    // Ethereum notion of transaction which is more generic as it also
    // encompasses the case of originations.
    TransactionContent as TransferContent,
};
#[cfg(test)]
use tezos_smart_rollup::types::PublicKey;
use tezos_smart_rollup::types::PublicKeyHash;
use thiserror::Error;

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
#[derive(Clone)]
pub enum OperationContent {
    Reveal(RevealContent),
    Transfer(TransferContent),
    Origination(OriginationContent),
}

// In Tezlink, we'll only support ManagerOperation so we don't
// have to worry about other operations
#[derive(PartialEq, Debug, Clone)]
pub struct Operation {
    pub branch: BlockHash,
    pub content: Vec<ManagerOperationContent>,
    pub signature: UnknownSignature,
}

// Manual `BinWriter` (the derived one would accept any `content`) so that
// writing an operation with an empty content list is rejected, keeping the
// writer symmetric with `nom_read` below which refuses to parse such
// operations: an empty-content operation can never be persisted and then
// fail to decode.
impl BinWriter for Operation {
    fn bin_write(&self, output: &mut Vec<u8>) -> BinResult {
        if self.content.is_empty() {
            return Err(BinError::custom("empty operation content list".to_string()));
        }
        self.branch.bin_write(output)?;
        for content in &self.content {
            content.bin_write(output)?;
        }
        self.signature.bin_write(output)
    }
}

// Byte length of the trailing `UnknownSignature`. Derived from the crypto
// type's own size so it cannot silently drift if the signature representation
// ever changes. It is 64 bytes for the non-BLS (Ed25519/Secp256k1/P256)
// signatures that `UnknownSignature` covers; BLS signatures are not used here.
const SIGNATURE_SIZE: usize = HashType::UnknownSignature.size();

// This reader locates the end of the variable-length `content` list purely by
// length: it parses content items until exactly `SIGNATURE_SIZE` (signature)
// bytes remain. It is therefore only correct when the operation occupies the
// entire input buffer, i.e. the 64-byte signature is the trailing suffix of
// `input`. Call it via `nom_read_exact` or on a length-delimited buffer;
// embedding `Operation` as a non-final field of a derived `NomReader` struct
// would make it over-consume following bytes as content.
impl NomReader<'_> for Operation {
    fn nom_read(input: &'_ [u8]) -> NomResult<'_, Self> {
        let (mut input, branch) = BlockHash::nom_read(input)?;
        let mut content = Vec::new();

        while input.len() > SIGNATURE_SIZE {
            let (remaining, operation_content) =
                ManagerOperationContent::nom_read(input)?;
            // Defensive guard against an infinite loop: every content item must
            // consume at least one byte (its tag). If a parser ever returned
            // `Ok` without making progress, bail out instead of spinning forever.
            if remaining.len() >= input.len() {
                return Err(nom::Err::Error(tezos_nom::error::DecodeError::invalid_tag(
                    input,
                    "operation content parser made no progress".into(),
                )));
            }
            content.push(operation_content);
            input = remaining;
        }

        if content.is_empty() {
            return Err(nom::Err::Error(tezos_nom::error::DecodeError::invalid_tag(
                input,
                "empty operation content list".into(),
            )));
        }

        let (input, signature) = UnknownSignature::nom_read(input)?;
        Ok((
            input,
            Self {
                branch,
                content,
                signature,
            },
        ))
    }
}

// `Encodable::rlp_append` can't fail but `to_bytes` can. We don't want the
// kernel to panic and we can't log without host access, so on failure encode
// an empty byte string (0x80): the RLP item count stays correct and decoding
// fails cleanly instead of corrupting the surrounding list. Use
// `bytes.rlp_append(stream)` (not `stream.append(&bytes)`) so the outer
// `stream.append(&operation)` doesn't double-count the item.
impl Encodable for Operation {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        self.to_bytes().unwrap_or_default().rlp_append(stream)
    }
}

impl Operation {
    pub fn hash(&self) -> Result<OperationHash, BinError> {
        let serialized_op = self.to_bytes()?;
        let op_hash = digest_256(&serialized_op);
        Ok(OperationHash::from(op_hash))
    }
}

impl Decodable for Operation {
    fn decode(rlp: &rlp::Rlp) -> Result<Self, rlp::DecoderError> {
        let raw: Vec<u8> = rlp.as_val()?;
        Operation::nom_read_exact(&raw)
            .map_err(|_| rlp::DecoderError::Custom("Operation::try_from_bytes failed"))
    }
}

pub trait ManagerOperationField {
    fn fee(&self) -> &Narith;
    fn gas_limit(&self) -> Result<&Narith, ValidityError>;
    fn source(&self) -> Result<&PublicKeyHash, ValidityError>;
}

impl ManagerOperationField for ManagerOperationContent {
    fn fee(&self) -> &Narith {
        match self {
            ManagerOperationContent::Reveal(op) => &op.fee,
            ManagerOperationContent::Transaction(op) => &op.fee,
            ManagerOperationContent::Origination(op) => &op.fee,
            ManagerOperationContent::Delegation(op) => &op.fee,
            ManagerOperationContent::SmartRollupCement(op) => &op.fee,
            ManagerOperationContent::SmartRollupPublish(op) => &op.fee,
        }
    }

    fn gas_limit(&self) -> Result<&Narith, ValidityError> {
        match self {
            ManagerOperationContent::Reveal(op) => Ok(&op.gas_limit),
            ManagerOperationContent::Transaction(op) => Ok(&op.gas_limit),
            ManagerOperationContent::Origination(op) => Ok(&op.gas_limit),
            _ => Err(ValidityError::UnsupportedOperation),
        }
    }

    fn source(&self) -> Result<&PublicKeyHash, ValidityError> {
        match self {
            ManagerOperationContent::Reveal(op) => Ok(&op.source),
            ManagerOperationContent::Transaction(op) => Ok(&op.source),
            ManagerOperationContent::Origination(op) => Ok(&op.source),
            _ => Err(ValidityError::UnsupportedOperation),
        }
    }
}

pub trait ManagerOperationContentConv: Sized {
    fn into_manager_operation_content(self) -> ManagerOperationContent;
    fn try_from_manager_operation_content(
        op: ManagerOperationContent,
    ) -> Result<Self, ValidityError>;
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

    fn try_from_manager_operation_content(
        op: ManagerOperationContent,
    ) -> Result<Self, ValidityError> {
        match op {
            ManagerOperationContent::Reveal(ManagerOperation {
                source,
                fee,
                counter,
                gas_limit,
                storage_limit,
                operation: c,
            }) => Ok(ManagerOperation {
                source,
                fee,
                counter,
                gas_limit,
                storage_limit,
                operation: OperationContent::Reveal(c),
            }),
            ManagerOperationContent::Transaction(ManagerOperation {
                source,
                fee,
                counter,
                gas_limit,
                storage_limit,
                operation: c,
            }) => Ok(ManagerOperation {
                source,
                fee,
                counter,
                gas_limit,
                storage_limit,
                operation: OperationContent::Transfer(c),
            }),
            ManagerOperationContent::Origination(ManagerOperation {
                source,
                fee,
                counter,
                gas_limit,
                storage_limit,
                operation: c,
            }) => Ok(ManagerOperation {
                source,
                fee,
                counter,
                gas_limit,
                storage_limit,
                operation: OperationContent::Origination(c),
            }),
            _ => Err(ValidityError::UnsupportedOperation),
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

    let branch = TezBlock::genesis_block_hash();

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
        operation::make_dummy_reveal_operation, protocol::TARGET_TEZOS_PROTOCOL,
    };
    use mir::ast::michelson_address::Entrypoint;
    use pretty_assertions::assert_eq;
    use rlp::{Decodable, Rlp};
    use tezos_crypto_rs::{hash::UnknownSignature, public_key::PublicKey};
    use tezos_protocol::contract::Contract;
    use tezos_smart_rollup::types::PublicKeyHash;

    #[test]
    fn operation_rlp_roundtrip() {
        let operation = make_dummy_reveal_operation();
        let encoded = rlp::encode(&operation);
        let rlp = Rlp::new(&encoded);
        let decoded_operation =
            Operation::decode(&rlp).expect("Decoding operation should have succeeded");
        assert_eq!(operation, decoded_operation);
    }

    // The reader rejects empty content lists; the writer must do the same so
    // an empty-content operation can never be persisted and then fail to
    // decode.
    #[test]
    fn empty_content_operation_is_rejected_by_writer() {
        let operation = Operation {
            content: vec![],
            ..make_dummy_reveal_operation()
        };
        assert!(
            operation.to_bytes().is_err(),
            "writing an operation with an empty content list should fail"
        );
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
    fn operation_decoder_does_not_parse_signature_as_content() {
        let operation_bytes = hex::decode(
            "ff1879f62b76d83d374e3e31238f0ac647898cc2e0d5ff8861f35f2f59ece67a6c0010d0238c078cb24b0dd11ba7e1f667b7390b8ad3a01ff30ab02200010000a51db41927a83e359322a34acc7644350b5581c200f80a705d5c26abed0d6548ba34785c80e10690338dbcfb14bd91a7d952c66c02d8ef89bec99bcc4eb8be6cf8bc0c88ee1acac83f0c467c5b49713019e3874b0e",
        )
        .expect("operation hex should decode");

        let operation = Operation::nom_read_exact(&operation_bytes).expect(
            "operation should decode without consuming signature bytes as content",
        );

        assert_eq!(operation.content.len(), 1);
        assert_eq!(
            operation
                .to_bytes()
                .expect("operation should encode after decoding"),
            operation_bytes
        );
    }

    #[test]
    fn operation_decoder_handles_multiple_contents() {
        // Reuse the single-content fixture to obtain a valid content item and
        // signature without hand-crafting hex, then build an operation whose
        // content list contains that item twice. This exercises the manual
        // reader's ability to separate a multi-item content list from the
        // trailing signature.
        let operation_bytes = hex::decode(
            "ff1879f62b76d83d374e3e31238f0ac647898cc2e0d5ff8861f35f2f59ece67a6c0010d0238c078cb24b0dd11ba7e1f667b7390b8ad3a01ff30ab02200010000a51db41927a83e359322a34acc7644350b5581c200f80a705d5c26abed0d6548ba34785c80e10690338dbcfb14bd91a7d952c66c02d8ef89bec99bcc4eb8be6cf8bc0c88ee1acac83f0c467c5b49713019e3874b0e",
        )
        .expect("operation hex should decode");

        let single = Operation::nom_read_exact(&operation_bytes)
            .expect("single-content operation should decode");
        assert_eq!(single.content.len(), 1);

        let content_item = single.content[0].clone();
        let operation = Operation {
            branch: single.branch.clone(),
            content: vec![content_item.clone(), content_item],
            signature: single.signature.clone(),
        };

        let bytes = operation
            .to_bytes()
            .expect("multi-content operation should encode");

        let decoded = Operation::nom_read_exact(&bytes).expect(
            "multi-content operation should decode without consuming signature bytes as content",
        );

        assert_eq!(decoded.content.len(), 2);
        assert_eq!(decoded, operation);
        assert_eq!(
            decoded
                .to_bytes()
                .expect("multi-content operation should re-encode"),
            bytes
        );
    }

    #[test]
    fn tezos_compatibility_for_reveal() {
        // The goal of this test is to try to decode an encoding generated by 'octez-codec encode' command
        let branch = BlockHash::from_base58_check(
            "BKpbfCvh777DQHnXjU2sqHvVUNZ7dBAdqEfKkdw8EGSkD9LSYXb",
        )
        .unwrap();
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

        let operation_bytes =
            fetch_generated_data(TARGET_TEZOS_PROTOCOL, "operation", "reveal");

        let operation = Operation::nom_read_exact(&operation_bytes)
            .expect("Decoding operation should have succeeded");

        assert_eq!(operation, expected_operation);
    }

    #[test]
    fn tezos_compatibility_for_transfer() {
        // The goal of this test is to try to decode an encoding generated by 'octez-codec encode' command
        let branch = BlockHash::from_base58_check(
            "BKpbfCvh777DQHnXjU2sqHvVUNZ7dBAdqEfKkdw8EGSkD9LSYXb",
        )
        .unwrap();
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
            TARGET_TEZOS_PROTOCOL,
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
        let address_bytes = fetch_generated_data(
            TARGET_TEZOS_PROTOCOL,
            "contract",
            "contract-originated",
        );

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
        let branch = BlockHash::from_base58_check(
            "BKpbfCvh777DQHnXjU2sqHvVUNZ7dBAdqEfKkdw8EGSkD9LSYXb",
        )
        .unwrap();

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
            TARGET_TEZOS_PROTOCOL,
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
        let branch = BlockHash::from_base58_check(
            "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU",
        )
        .unwrap();
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
            TARGET_TEZOS_PROTOCOL,
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
        let branch = BlockHash::from_base58_check(
            "BKpbfCvh777DQHnXjU2sqHvVUNZ7dBAdqEfKkdw8EGSkD9LSYXb",
        )
        .unwrap();
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

        let operation_bytes = fetch_generated_data(
            TARGET_TEZOS_PROTOCOL,
            "operation",
            "operation-origination",
        );

        let (bytes, decoded_operation) = Operation::nom_read(&operation_bytes).unwrap();
        assert_eq!(expected_operation, decoded_operation);
        assert!(bytes.is_empty());
    }
}
