// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::eth_gen::{OwnedHash, Quantity};
use crate::wei::Wei;

// Simple representation of an account, only contains fixed-sized values (no
// code nor storage).
pub struct Account {
    pub nonce: Quantity, // initially 0, updated after each transaction
    pub balance: Wei,
    pub code_hash: OwnedHash, // 256 bits hash
}

impl Account {
    pub fn default_account(balance: Wei) -> Self {
        Self {
            balance,
            nonce: 0,
            // TODO: https://gitlab.com/tezos/tezos/-/issues/4859
            // Use the hash corresponding to the empty code
            code_hash: vec![65; 32],
        }
    }
}
