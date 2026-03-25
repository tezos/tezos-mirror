// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::H256;
use sha3::{Digest, Keccak256};
use tezos_data_encoding::nom::NomResult;

/// Compute the Keccak256 hash of `bytes`.
pub fn bytes_hash(bytes: &[u8]) -> H256 {
    H256(Keccak256::digest(bytes).into())
}

/// Verify if the result of a NomReader is incomplete
pub fn is_incomplete<T>(result: &NomResult<T>) -> bool {
    if let Err(nom_error) = result {
        nom_error.is_incomplete()
    } else {
        false
    }
}
