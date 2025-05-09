// SPDX-FileCopyrightText: 2024-2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 draganrakita
//
// SPDX-License-Identifier: MIT

use core::cmp::min;

use alloc::vec::Vec;
use num_bigint::{BigInt, Sign};
use primitive_types::{H160, H256, U256};
use sha3::{Digest, Keccak256};

/// Get an array from the data, if data does not contain `start` to `len` bytes, add right padding with
/// zeroes
#[inline(always)]
pub fn get_right_padded<const S: usize>(data: &[u8], offset: usize) -> [u8; S] {
    let mut padded = [0; S];
    let start = min(offset, data.len());
    let end = min(start.saturating_add(S), data.len());
    padded[..end - start].copy_from_slice(&data[start..end]);
    padded
}

/// Get a vector of the data, if data does not contain the slice of `start` to `len`, right pad missing
/// part with zeroes
#[inline(always)]
pub fn get_right_padded_vec(data: &[u8], offset: usize, len: usize) -> Vec<u8> {
    let mut padded = vec![0; len];
    let start = min(offset, data.len());
    let end = min(start.saturating_add(len), data.len());
    padded[..end - start].copy_from_slice(&data[start..end]);
    padded
}

/// Left padding until `len`. If data is more then len, truncate the right most bytes.
#[inline(always)]
pub fn left_padding<const S: usize>(data: &[u8]) -> [u8; S] {
    let mut padded = [0; S];
    let end = min(S, data.len());
    padded[S - end..].copy_from_slice(&data[..end]);
    padded
}

/// Left padding until `len`. If data is more then len, truncate the right most bytes.
#[inline(always)]
pub fn left_padding_vec(data: &[u8], len: usize) -> Vec<u8> {
    let mut padded = vec![0; len];
    let end = min(len, data.len());
    padded[len - end..].copy_from_slice(&data[..end]);
    padded
}

pub fn create_address_legacy(caller: &H160, nonce: &u64) -> H160 {
    let mut stream = rlp::RlpStream::new_list(2);
    stream.append(caller);
    stream.append(nonce);
    H256::from_slice(Keccak256::digest(stream.out()).as_slice()).into()
}

/// Compute Keccak 256 for some bytes
pub fn keccak256_hash(bytes: &[u8]) -> H256 {
    H256(Keccak256::digest(bytes).into())
}

/// Try to cast BigInt to U256
pub fn bigint_to_u256(value: &BigInt) -> Result<U256, primitive_types::Error> {
    let (_, bytes) = value.to_bytes_le();
    if bytes.len() > 32 {
        return Err(primitive_types::Error::Overflow);
    }
    Ok(U256::from_little_endian(&bytes))
}

/// Converts a U256 to a BigInt
pub fn u256_to_bigint(value: U256) -> BigInt {
    let mut bytes = vec![0u8; 32];
    value.to_big_endian(&mut bytes);
    BigInt::from_bytes_be(Sign::Plus, &bytes)
}

pub fn u256_to_le_bytes(value: U256) -> Vec<u8> {
    let mut bytes = vec![0u8; 32];
    value.to_little_endian(&mut bytes);
    bytes
}

pub mod alloy {
    use super::*;
    use alloy_primitives::{self, Address, Uint};

    pub fn u256_to_alloy(value: &U256) -> Option<alloy_primitives::U256> {
        Some(alloy_primitives::U256::from_le_bytes::<32>(
            u256_to_le_bytes(*value).try_into().ok()?,
        ))
    }

    pub fn h160_to_alloy(value: &H160) -> alloy_primitives::Address {
        Address::from_slice(&value.to_fixed_bytes())
    }

    pub fn alloy_to_u256(value: &Uint<256, 4>) -> U256 {
        U256(value.into_limbs())
    }

    pub fn alloy_to_h160(value: &Address) -> Option<H160> {
        let value: [u8; 20] = value.as_slice().try_into().ok()?;
        Some(H160::from_slice(&value))
    }
}
