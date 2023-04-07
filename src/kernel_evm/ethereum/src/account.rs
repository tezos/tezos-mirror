// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::eth_gen::OwnedHash;
use crate::wei::Wei;
use primitive_types::U256;

// Simple representation of an account, only contains fixed-sized values (no
// code nor storage).
pub struct Account {
    pub nonce: U256, // initially 0, updated after each transaction
    pub balance: Wei,
    pub code_hash: OwnedHash, // 256 bits hash
}

impl Account {
    pub fn blank() -> Self {
        Self::with_assets(Wei::zero())
    }

    pub fn with_assets(balance: Wei) -> Self {
        Self {
            balance,
            nonce: U256::zero(),
            // TODO: https://gitlab.com/tezos/tezos/-/issues/4859
            // Use the hash corresponding to the empty code
            code_hash: vec![65; 32],
        }
    }
}
