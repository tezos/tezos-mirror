// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::H256;
use sha3::{Digest, Keccak256};

/// Compute the Keccak256 hash of `bytes`.
pub fn bytes_hash(bytes: &[u8]) -> H256 {
    H256(Keccak256::digest(bytes).into())
}
