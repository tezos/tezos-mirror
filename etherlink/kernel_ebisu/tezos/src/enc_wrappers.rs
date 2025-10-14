// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Encoding wrappers for Tezos data types used in Tezlink.

/// Wrapper types over used in Tezlink, implementing
/// `NomReader` and `BinWriter` to enable derivation for more complex encodings in a tezos compatible way.
use nom::{bytes::complete::take, combinator::map};
use primitive_types::{H256, U256};
use std::fmt::Display;
use tezos_data_encoding::enc as tezos_enc;
use tezos_data_encoding::{
    enc::{BinResult, BinWriter},
    nom::{NomError, NomReader, NomResult},
};
use thiserror::Error;

#[derive(PartialEq, Debug, NomReader, BinWriter, Clone, Copy)]
pub struct BlockNumber {
    pub block_number: u32,
}

impl Display for BlockNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        u32::fmt(&self.block_number, f)
    }
}

impl From<u32> for BlockNumber {
    fn from(block_number: u32) -> Self {
        Self { block_number }
    }
}

impl From<BlockNumber> for u32 {
    fn from(block_number: BlockNumber) -> Self {
        block_number.block_number
    }
}

impl From<BlockNumber> for U256 {
    fn from(block_number: BlockNumber) -> Self {
        block_number.block_number.into()
    }
}

#[derive(Error, Debug)]
pub struct BlockNumberOverflowError;

impl Display for BlockNumberOverflowError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Overflow during conversion of block number from U256 to u32"
        )
    }
}

impl TryFrom<U256> for BlockNumber {
    type Error = BlockNumberOverflowError;

    fn try_from(number: U256) -> Result<Self, Self::Error> {
        if number < U256::from(u32::MAX) {
            Ok(Self::from(number.as_u32()))
        } else {
            Err(BlockNumberOverflowError)
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct OperationHash(pub H256);

impl NomReader<'_> for OperationHash {
    fn nom_read(bytes: &[u8]) -> NomResult<'_, Self> {
        let (remaining, hash) =
            map(take::<usize, &[u8], NomError>(32_usize), H256::from_slice)(bytes)?;
        Ok((remaining, OperationHash(hash)))
    }
}

impl BinWriter for OperationHash {
    fn bin_write(&self, data: &mut Vec<u8>) -> BinResult {
        tezos_enc::put_bytes(&self.0.to_fixed_bytes(), data);
        Ok(())
    }
}

impl From<H256> for OperationHash {
    fn from(hash: H256) -> Self {
        Self(hash)
    }
}

impl From<OperationHash> for H256 {
    fn from(hash: OperationHash) -> Self {
        hash.0
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct BlockHash(pub H256);
impl NomReader<'_> for BlockHash {
    fn nom_read(bytes: &[u8]) -> NomResult<'_, Self> {
        let (remaining, hash) =
            map(take::<usize, &[u8], NomError>(32_usize), H256::from_slice)(bytes)?;
        Ok((remaining, BlockHash(hash)))
    }
}

impl BinWriter for BlockHash {
    fn bin_write(&self, data: &mut Vec<u8>) -> BinResult {
        tezos_enc::put_bytes(&self.0.to_fixed_bytes(), data);
        Ok(())
    }
}

impl From<H256> for BlockHash {
    fn from(hash: H256) -> Self {
        Self(hash)
    }
}
impl From<BlockHash> for H256 {
    fn from(hash: BlockHash) -> Self {
        hash.0
    }
}
