// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Tezos operations: this module defines the fragment of Tezos operations supported by Tezlink and how to serialize them.

/// The whole module is inspired of `src/proto_alpha/lib_protocol/operation_repr.ml` to represent the operation
use nom::combinator::map;
use nom::error::{ErrorKind, ParseError};
use nom::{bytes::complete::take, Finish};
use primitive_types::H256;
use rlp::Decodable;
use tezos_crypto_rs::blake2b::digest_256;
use tezos_crypto_rs::hash::{HashType, UnknownSignature};
use tezos_data_encoding::types::Narith;
use tezos_data_encoding::{
    enc::{self as tezos_enc, BinError, BinResult, BinWriter},
    nom::{self as tezos_nom, error::DecodeError, NomError, NomReader, NomResult},
};
use tezos_smart_rollup::types::PublicKeyHash;
use tezos_smart_rollup::types::{Contract, PublicKey};

pub const REVEAL_TAG: u8 = 107_u8;
pub const TRANSFER_TAG: u8 = 108_u8;

#[derive(PartialEq, Debug)]
pub struct RevealContent {
    pub pk: PublicKey,
}

/// Transfers are often called "transactions" in the protocol, we try
/// to consistently call them transfers to avoid confusion with the
/// Ethereum notion of transaction which is more generic as it also
/// encompasses the case of originations.
#[derive(PartialEq, Debug)]
pub struct TransferContent {
    pub amount: Narith,
    pub destination: Contract,
    pub parameter: Option<()>,
}

#[derive(PartialEq, Debug)]
pub enum OperationContent {
    Reveal(RevealContent),
    Transfer(TransferContent),
}

impl OperationContent {
    pub fn tag(&self) -> u8 {
        match self {
            Self::Reveal(_) => REVEAL_TAG,
            Self::Transfer(_) => TRANSFER_TAG,
        }
    }

    pub fn from_bytes(tag: u8, bytes: &[u8]) -> NomResult<Self> {
        match tag {
            REVEAL_TAG => {
                let (array, pk) = PublicKey::nom_read(bytes)?;
                NomResult::Ok((array, Self::Reveal(RevealContent { pk })))
            }
            TRANSFER_TAG => {
                let (input, amount) = Narith::nom_read(bytes)?;
                let (input, destination) = Contract::nom_read(input)?;
                // TODO: parameter should be a Michelson expr, for now just use unit
                let (input, parameter) =
                    tezos_nom::optional_field(|input| Ok((input, ())))(input)?;
                NomResult::Ok((
                    input,
                    Self::Transfer(TransferContent {
                        amount,
                        destination,
                        parameter,
                    }),
                ))
            }
            _ => Err(nom::Err::Error(tezos_nom::NomError::invalid_tag(
                bytes,
                tag.to_string(),
            ))),
        }
    }
}

// TODO: !17672, derive all NomReader and BinWriter implementations in this module.
impl BinWriter for OperationContent {
    fn bin_write(&self, data: &mut Vec<u8>) -> BinResult {
        match self {
            Self::Reveal(RevealContent { pk }) => {
                pk.bin_write(data)?;
                Ok(())
            }
            Self::Transfer(TransferContent {
                amount,
                destination,
                parameter,
            }) => {
                amount.bin_write(data)?;
                destination.bin_write(data)?;
                // TODO: parameter should be a Michelson expr, for now just use unit
                let closure: for<'a> fn(&(), &'a mut Vec<u8>) -> BinResult =
                    |_, _| Ok(());
                tezos_enc::optional_field(closure)(parameter, data)?;
                Ok(())
            }
        }
    }
}

// In Tezlink, we'll only support ManagerOperation so we don't
// have to worry about other operations
#[derive(PartialEq, Debug)]
pub struct ManagerOperation<C> {
    pub source: PublicKeyHash,
    pub fee: Narith,
    pub counter: Narith,
    pub gas_limit: Narith,
    pub storage_limit: Narith,
    pub operation: C,
}

// TODO: !17672, derive all NomReader and BinWriter implementations in this module.
impl BinWriter for ManagerOperation<OperationContent> {
    fn bin_write(&self, data: &mut Vec<u8>) -> BinResult {
        let Self {
            source,
            fee,
            counter,
            operation,
            gas_limit,
            storage_limit,
        } = self;

        // Push the tag of the operation
        data.push(operation.tag());

        // Push data related to the operation
        source.bin_write(data)?;

        fee.bin_write(data)?;

        counter.bin_write(data)?;

        gas_limit.bin_write(data)?;

        storage_limit.bin_write(data)?;

        // Append the operation
        operation.bin_write(data)?;

        Ok(())
    }
}

// TODO: !17672, derive all NomReader and BinWriter implementations in this module.
impl NomReader<'_> for ManagerOperation<OperationContent> {
    fn nom_read(bytes: &[u8]) -> tezos_data_encoding::nom::NomResult<Self> {
        // Retrieve the tag of the operation, it will be used to decode the OperationContent
        let (bytes, tag) = nom::number::complete::u8(bytes)?;

        let (bytes, source) = PublicKeyHash::nom_read(bytes)?;

        let (bytes, fee) = Narith::nom_read(bytes)?;

        let (bytes, counter) = Narith::nom_read(bytes)?;

        let (bytes, gas_limit) = Narith::nom_read(bytes)?;

        let (bytes, storage_limit) = Narith::nom_read(bytes)?;

        let (bytes, operation) = OperationContent::from_bytes(tag, bytes)?;

        Ok((
            bytes,
            Self {
                source,
                fee,
                counter,
                operation,
                gas_limit,
                storage_limit,
            },
        ))
    }
}

#[derive(PartialEq, Debug)]
pub struct Operation {
    pub branch: BlockHash,
    pub content: ManagerOperation<OperationContent>,
    pub signature: UnknownSignature,
}

// TODO: !17672, derive all NomReader and BinWriter implementations in this module.
impl BinWriter for Operation {
    fn bin_write(&self, data: &mut Vec<u8>) -> BinResult {
        // Encode branch field
        self.branch.bin_write(data)?;

        self.content.bin_write(data)?;

        // In an operation, the signature is always encoded as an UnknownSignature
        // 'bin_write' function for signature adds 4 bytes for the size which
        // makes the decoding fail
        let b58_repr = self.signature.to_base58_check();

        let encoded_sig = HashType::UnknownSignature
            .b58check_to_hash(&b58_repr)
            .map_err(|b58_err| BinError::custom(format!("{:?}", b58_err)))?;

        data.extend_from_slice(&encoded_sig);
        Ok(())
    }
}

// TODO: !17672, derive all NomReader and BinWriter implementations in this module.
impl NomReader<'_> for Operation {
    fn nom_read(bytes: &[u8]) -> NomResult<Self> {
        let (bytes, branch) = BlockHash::nom_read(bytes)?;

        // We'll use the returned slice to decode the signature
        let (bytes, content) = ManagerOperation::nom_read(bytes)?;

        // Can't use Signature 'nom_read' function because it expect 4 bytes for the size
        // which is not the case in an operation (it's always an Unknown signature)
        let (bytes, signature) = UnknownSignature::nom_read(bytes)?;

        Ok((
            bytes,
            Operation {
                branch,
                content,
                signature,
            },
        ))
    }
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
    // return a result. To avoid unwraping and risk a panic we're not implementing
    // the trait exactly, but we expose a serialization function.
    pub fn rlp_append(&self, s: &mut rlp::RlpStream) -> Result<(), BinError> {
        let bytes = self.to_bytes()?;
        s.append(&bytes);
        Ok(())
    }

    pub fn hash(&self) -> Result<H256, BinError> {
        let serialized_op = self.to_bytes()?;
        let op_hash = digest_256(&serialized_op);
        Ok(H256::from_slice(&op_hash))
    }
}

impl Decodable for Operation {
    fn decode(rlp: &rlp::Rlp) -> Result<Self, rlp::DecoderError> {
        let raw: Vec<u8> = rlp.as_val()?;
        Operation::try_from_bytes(&raw)
            .map_err(|_| rlp::DecoderError::Custom("Operation::try_from_bytes failed"))
    }
}

#[derive(PartialEq, Debug)]
pub struct BlockHash(H256);

impl NomReader<'_> for BlockHash {
    fn nom_read(bytes: &[u8]) -> tezos_data_encoding::nom::NomResult<Self> {
        let (bytes, hash) =
            map(take::<usize, &[u8], NomError>(32_usize), H256::from_slice)(bytes)?;
        Ok((bytes, Self(hash)))
    }
}

impl BinWriter for BlockHash {
    fn bin_write(&self, data: &mut Vec<u8>) -> BinResult {
        let hash: [u8; 32] = self.0.to_fixed_bytes();
        data.extend_from_slice(&hash);
        Ok(())
    }
}

impl From<H256> for BlockHash {
    fn from(hash: H256) -> Self {
        Self(hash)
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
        },
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
            content: ManagerOperation {
                source: PublicKeyHash::from_b58check(
                    "tz1cckAZtxYwxAfwQuHnabTWfbp2ScWobxHH",
                )
                .unwrap(),
                fee: 274_u64.into(),
                counter: 2_u64.into(),
                operation: OperationContent::Reveal(RevealContent {
                    pk: PublicKey::from_b58check(
                        "edpkuqNrmPPcy2S3G1uKYnxmg7Gov3c8q7AABKRs9EtTVtfDg5Fu7R",
                    )
                    .unwrap(),
                }),
                gas_limit: 169_u64.into(),
                storage_limit: 0_u64.into(),
            },
            signature,
        };

        // This bytes sequence comes from the command just above the test
        let operation_bytes = hex::decode("8fcf233671b6a04fcf679d2a381c2544ea6c1ea29ba6157776ed842426e5cab86b00ba3bed311a5d7b06dc4daf3c94c5c406927e4bcf920202a90100009d05b06ea36a6ad464d94dc07a38b77b80b577c1ae51bbd8d20105cd5aed496c1da02ac8f6e1541c363874bcde7e90e1f959c8f28ab52ec3fdbbf7d54b1dad4004f2b70da27ce35de18d77ea9efee413b5fb2b2a858be4d95e45acfe47a73b0d").unwrap();

        let operation = Operation::try_from_bytes(&operation_bytes)
            .expect("Decoding operation should have succeded");

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
            content: ManagerOperation {
                source: PublicKeyHash::from_b58check(
                    "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN",
                )
                .unwrap(),
                fee: 267_u64.into(),
                counter: 1_u64.into(),
                operation: OperationContent::Transfer(TransferContent {
                    amount: 1000000_u64.into(),
                    destination: Contract::from_b58check(
                        "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
                    )
                    .unwrap(),
                    parameter: None,
                }),
                gas_limit: 169_u64.into(),
                storage_limit: 0_u64.into(),
            },
            signature,
        };

        // This bytes sequence comes from the command just above the test
        let operation_bytes = hex::decode("8fcf233671b6a04fcf679d2a381c2544ea6c1ea29ba6157776ed842426e5cab86c00e7670f32038107a59a2b9cfefae36ea21f5aa63c8b0201a90100c0843d000002298c03ed7d454a101eb7022bc95f7e5f41ac780026d58f30f5f8caf70878ad4efc82d71cff01b76e584958411e5a89ea2a8908e37ffc28f0af92fa651c32f6cc7362d9c735344d590360864fbf0b156c3443b108").unwrap();

        let operation = Operation::try_from_bytes(&operation_bytes)
            .expect("Decoding operation should have succeded");

        assert_eq!(operation, expected_operation);

        // Also test the encoding
        let kernel_bytes = expected_operation
            .to_bytes()
            .expect("Operation encoding should have succeed");

        assert_eq!(operation_bytes, kernel_bytes)
    }
}
