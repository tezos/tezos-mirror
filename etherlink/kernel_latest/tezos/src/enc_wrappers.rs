// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Encoding wrappers for Tezos data types used in Tezlink.

/// Wrapper types over used in Tezlink, implementing
/// `NomReader` and `BinWriter` to enable derivation for more complex encodings in a tezos compatible way.
use primitive_types::U256;
use rlp::Encodable;
use std::fmt::Display;
use tezos_data_encoding::{enc::BinWriter, nom::NomReader};
use thiserror::Error;

#[derive(PartialEq, Debug, NomReader, BinWriter, Clone, Copy)]
pub struct BlockNumber {
    pub block_number: u32,
}

impl Encodable for BlockNumber {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        s.append_internal(&self.block_number.to_le_bytes().to_vec());
    }
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
