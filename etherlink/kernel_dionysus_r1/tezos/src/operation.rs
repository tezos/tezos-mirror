// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Tezos operations: this module defines the fragment of Tezos operations supported by Tezlink and how to serialize them.

/// The whole module is inspired of `src/proto_alpha/lib_protocol/operation_repr.ml` to represent the operation
use nom::combinator::map;
use nom::error::{ErrorKind, ParseError};
use nom::{bytes::complete::take, Finish};
use primitive_types::H256;
use tezos_crypto_rs::hash::{HashType, UnknownSignature};
use tezos_data_encoding::{
    enc::{self as tezos_enc, BinError, BinResult, BinWriter},
    nom::{self as tezos_nom, error::DecodeError, NomError, NomReader, NomResult},
};
use tezos_smart_rollup::types::PublicKey;
use tezos_smart_rollup::types::PublicKeyHash;

#[derive(PartialEq, Debug)]
pub enum OperationContent {
    Reveal { pk: PublicKey },
}

pub const REVEAL_TAG: u8 = 107_u8;

impl OperationContent {
    pub fn tag(&self) -> u8 {
        match self {
            Self::Reveal { pk: _ } => REVEAL_TAG,
        }
    }

    pub fn from_bytes(tag: u8, bytes: &[u8]) -> NomResult<Self> {
        match tag {
            REVEAL_TAG => {
                let (array, pk) = PublicKey::nom_read(bytes)?;
                NomResult::Ok((array, Self::Reveal { pk }))
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
            Self::Reveal { pk } => {
                pk.bin_write(data)?;
                Ok(())
            }
        }
    }
}

// In Tezlink, we'll only support ManagerOperation so we don't
// have to worry about other operations
#[derive(PartialEq, Debug)]
pub struct ManagerOperation {
    pub source: PublicKeyHash,
    pub fee: num_bigint::BigUint,
    pub counter: num_bigint::BigUint,
    pub gas_limit: num_bigint::BigUint,
    pub storage_limit: num_bigint::BigUint,
    pub operation: OperationContent,
}

// TODO: !17672, derive all NomReader and BinWriter implementations in this module.
impl BinWriter for ManagerOperation {
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

        tezos_enc::n_bignum(fee, data)?;

        tezos_enc::n_bignum(counter, data)?;

        tezos_enc::n_bignum(gas_limit, data)?;

        tezos_enc::n_bignum(storage_limit, data)?;

        // Append the operation
        operation.bin_write(data)?;

        Ok(())
    }
}

// TODO: !17672, derive all NomReader and BinWriter implementations in this module.
impl NomReader<'_> for ManagerOperation {
    fn nom_read(bytes: &[u8]) -> tezos_data_encoding::nom::NomResult<Self> {
        // Retrieve the tag of the operation, it will be used to decode the OperationContent
        let (bytes, tag) = nom::number::complete::u8(bytes)?;

        let (bytes, source) = PublicKeyHash::nom_read(bytes)?;

        let (bytes, fee) = tezos_nom::n_bignum(bytes)?;

        let (bytes, counter) = tezos_nom::n_bignum(bytes)?;

        let (bytes, gas_limit) = tezos_nom::n_bignum(bytes)?;

        let (bytes, storage_limit) = tezos_nom::n_bignum(bytes)?;

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
    pub branch: H256,
    pub content: ManagerOperation,
    pub signature: UnknownSignature,
}

// TODO: !17672, derive all NomReader and BinWriter implementations in this module.
impl BinWriter for Operation {
    fn bin_write(&self, data: &mut Vec<u8>) -> BinResult {
        // Encode branch field
        let branch: [u8; 32] = self.branch.to_fixed_bytes();

        data.extend_from_slice(&branch);
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
        let (bytes, branch) =
            map(take::<usize, &[u8], NomError>(32_usize), H256::from_slice)(bytes)?;

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
            return Err(tezos_nom::NomError::from_error_kind(
                remaining,
                ErrorKind::NonEmpty,
            ));
        }
        Ok(operation)
    }
}

#[cfg(test)]
mod tests {
    use super::{ManagerOperation, Operation, OperationContent};
    use crate::block::TezBlock;
    use primitive_types::H256;
    use tezos_crypto_rs::{
        hash::{HashType, UnknownSignature},
        public_key::PublicKey,
    };
    use tezos_smart_rollup::types::PublicKeyHash;

    fn make_dummy_operation(
        operation: OperationContent,
        signature: UnknownSignature,
    ) -> Operation {
        let branch = TezBlock::genesis_block_hash();

        // Public key hash in b58 for 0002298c03ed7d454a101eb7022bc95f7e5f41ac78
        let source = PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
            .expect("Public key hash conversion should succeed");

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

    fn make_dummy_reveal_operation() -> Operation {
        let pk = PublicKey::from_b58check(
            "edpkuT1qccDweCHnvgjLuNUHERpZmEaFZfbWvTzj2BxmTgQBZjaDFD",
        )
        .expect("Public key creation should have succeed");

        let signature = UnknownSignature::from_base58_check("sigSPESPpW4p44JK181SmFCFgZLVvau7wsJVN85bv5ciigMu7WSRnxs9H2NydN5ecxKHJBQTudFPrUccktoi29zHYsuzpzBX").unwrap();

        make_dummy_operation(OperationContent::Reveal { pk }, signature)
    }

    #[test]
    fn test_reveal_operation_roundtrip_encoding() {
        let operation = make_dummy_reveal_operation();

        let bytes = operation
            .to_bytes()
            .expect("Encoding reveal operation should have succeed");
        let operation_from_bytes = Operation::try_from_bytes(&bytes)
            .expect("Decoding reveal operation should have succeed");

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
        let branch = H256::from_slice(&branch_vec);
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
                operation: OperationContent::Reveal {
                    pk: PublicKey::from_b58check(
                        "edpkuqNrmPPcy2S3G1uKYnxmg7Gov3c8q7AABKRs9EtTVtfDg5Fu7R",
                    )
                    .unwrap(),
                },
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
}
