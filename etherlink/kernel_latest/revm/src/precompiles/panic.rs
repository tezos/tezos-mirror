// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! A precompile whose sole purpose is to deliberately trigger WASM traps,
//! used to exercise the kernel's behaviour in the face of unrecoverable
//! failures. It exposes two methods:
//!
//! - `panic()` aborts execution (via `panic!`), which compiles down to an
//!   `unreachable` instruction on `wasm32-unknown-unknown`.
//! - `oom()` exhausts the WASM linear memory, so the allocation error
//!   handler aborts, again reaching an `unreachable` trap.

use alloy_sol_types::{sol, SolInterface};
use evm_types::CustomPrecompileError;
use revm::interpreter::{CallInputs, Gas, InterpreterResult};

sol! {
    contract Panic {
        function panic() external;
        function oom() external;
    }
}

pub(crate) fn panic_precompile(
    calldata: &[u8],
    inputs: &CallInputs,
) -> Result<InterpreterResult, CustomPrecompileError> {
    let gas = Gas::new(inputs.gas_limit);

    let interface = Panic::PanicCalls::abi_decode(calldata)
        .map_err(|e| CustomPrecompileError::Revert(e.to_string(), gas))?;

    match interface {
        Panic::PanicCalls::panic(Panic::panicCall {}) => {
            // Aborts execution; compiles down to an `unreachable` WASM trap
            // on wasm32-unknown-unknown.
            panic!("panic precompile invoked")
        }
        Panic::PanicCalls::oom(Panic::oomCall {}) => {
            // Keep allocating and retaining ever-growing blocks until the
            // WASM linear memory is exhausted. The failed allocation triggers
            // the abort handler, reaching an `unreachable` WASM trap.
            let mut blocks: Vec<Vec<u8>> = Vec::new();
            loop {
                blocks.push(vec![0u8; 1 << 28]);
                std::hint::black_box(&blocks);
            }
        }
    }
}
