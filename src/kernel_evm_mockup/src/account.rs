// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::wei::Wei;

pub type OwnedHash = Vec<u8>;

pub type Hash<'a> = &'a Vec<u8>;

pub struct Account {
    pub hash: OwnedHash,
    pub nonce: u64, // initially 0, updated after each transaction
    pub balance: Wei,
    pub code_hash: OwnedHash, // 256 bits hash
}

impl Account {
    pub fn default_account(hash: OwnedHash, balance: Wei) -> Self {
        Self {
            hash,
            balance,
            nonce: 0,
            // TODO: https://gitlab.com/tezos/tezos/-/issues/4859
            // Use the hash corresponding to the empty code
            code_hash: vec![65; 32],
        }
    }
}
