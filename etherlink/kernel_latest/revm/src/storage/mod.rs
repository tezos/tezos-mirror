// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use tezos_crypto_rs::hash::{ContractKt1Hash, HashTrait};
use tezos_evm_runtime::runtime::Runtime;

use crate::storage::world_state_handler::{KT1_B58_SIZE, NATIVE_TOKEN_TICKETER_PATH};

pub mod block;
pub mod code;
pub mod sequencer_key_change;
pub mod version;
pub mod world_state_handler;

pub fn read_ticketer(host: &impl Runtime) -> Option<ContractKt1Hash> {
    let ticketer = host
        .store_read(&NATIVE_TOKEN_TICKETER_PATH, 0, KT1_B58_SIZE)
        .ok()?;
    let kt1_b58 = String::from_utf8(ticketer.to_vec()).ok()?;
    ContractKt1Hash::from_b58check(&kt1_b58).ok()
}
