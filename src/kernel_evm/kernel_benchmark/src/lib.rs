// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2022-2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Contains the EVM kernel
//!
//! This kernel runs EVM contract code emulating Ethereum, but on a rollup.
//! It expects all messages to be RLP encoded signed EIP-155 ethereum
//! transactions. They can be transfers, creation of contracts, or calls.
//! It comes with a faucet and outputs debug messages with informations about
//! the execution, including its outcome (gas, result, new address, ...).
#![deny(missing_docs)]
#![forbid(unsafe_code)]

extern crate alloc;
extern crate tezos_smart_rollup_debug as debug;
extern crate tezos_smart_rollup_host as host;

mod error;
mod inbox;

use debug::debug_msg;
use host::runtime::Runtime;

use evm_execution::account_storage::{
    account_path, init_account_storage, EthereumAccountStorage,
};
use primitive_types::H160;

// faucet
// address: 0xd9e5c94a12f78a96640757ac97ba0c257e8aa262
// privateKey: 0xcb9db6b5878db2fa20586e23b7f7b51c22a7c6ed0530daafc2615b116f170cd3
// publicKey: 0xe457895cab717434de97fee000c75e4f814472d89e351c72f537e909d051593522989b7d734fa9479c54450ff3c34279e85c9c883b04d443e50873211ed43f88
const FAUCET_ADDR: &str = "d9e5c94a12f78a96640757ac97ba0c257e8aa262";

/// create or replenish the faucet if need be
fn setup_faucet<Host: Runtime>(
    host: &mut Host,
    evm_account_storage: &EthereumAccountStorage,
) {
    let faucet_addr: H160 = H160::from_slice(&hex::decode(FAUCET_ADDR).unwrap());
    let faucet_path = account_path(&faucet_addr).unwrap();
    let mut faucet = evm_account_storage
        .get_or_create(host, &faucet_path)
        .unwrap();
    if let Ok(b) = faucet.balance(host) {
        if b == primitive_types::U256::zero() {
            faucet
                .balance_add(host, primitive_types::U256::max_value())
                .expect("Should have filled up empty faucet");
        }
    }
}

/// Entrypoint of the *evm* kernel.
pub fn evm_kernel_run<Host: Runtime>(host: &mut Host) {
    let mut evm_account_storage = match init_account_storage() {
        Ok(v) => v,
        Err(err) => {
            panic!("Could not initialize EVM kernel account storage: {:?}", err);
        }
    };

    setup_faucet(host, &evm_account_storage);

    match host.read_input() {
        Ok(Some(message)) => {
            debug_msg!(
                host,
                "Processing MessageData {} at level {}",
                message.id,
                message.level
            );

            if let Err(err) = inbox::process_inbox_message(
                host,
                &mut evm_account_storage,
                message.level,
                message.as_ref(),
            ) {
                debug_msg!(host, "Error processing header payload {:?}", err);
            }

            if host.mark_for_reboot().is_err() {
                debug_msg!(host, "Could not mark host for reboot");
            }
        }
        Ok(None) => {}
        Err(e) => debug_msg!(host, "Failed to read inputs {:?}", e),
    }
}

/// Define the `kernel_run` for the transactions kernel.
#[cfg(feature = "evm_kernel_benchmark")]
pub mod evm_kernel_benchmark {
    use tezos_smart_rollup_entrypoint::kernel_entry;
    kernel_entry!(crate::evm_kernel_run);
}
