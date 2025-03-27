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
