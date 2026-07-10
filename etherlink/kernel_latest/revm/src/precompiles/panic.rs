// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! A precompile whose sole purpose is to deliberately trigger WASM traps,
//! used to exercise the kernel's behaviour in the face of unrecoverable
//! failures. It exposes three methods:
//!
//! - `panic()` aborts execution (via `panic!`), which compiles down to an
//!   `unreachable` instruction on `wasm32-unknown-unknown`.
//! - `oom()` exhausts the WASM linear memory, so the allocation error
//!   handler aborts, again reaching an `unreachable` trap.
//! - `stack_overflow()` recurses until the call-depth guard injected by
//!   `smart-rollup-instrument` traps. Unlike the other two, it does not reach
//!   `unreachable` on its own: without the instrumentation it exhausts a real
//!   stack instead. See [`stack_overflow`] for why that distinction decides
//!   what the test proves.

use alloy_sol_types::{sol, SolInterface};
use evm_types::CustomPrecompileError;
use revm::interpreter::{CallInputs, Gas, InterpreterResult};

sol! {
    contract Panic {
        function panic() external;
        function oom() external;
        function stack_overflow() external;
    }
}

/// Recurse until something gives, keeping the per-frame footprint as small as
/// the compiler allows.
///
/// The frame size is what decides which stack runs out first, and therefore
/// what this test actually proves. Two stacks are in the race:
///
/// - the kernel's **shadow stack**, 1 MiB of linear memory (`__stack_pointer`
///   starts at 1048576 and grows down). Exhausting it drives the pointer
///   negative, which is read as a ~4 GiB address and traps out of bounds. The
///   PVM already absorbs that trap and recovers from it.
/// - the **host** stack of whichever engine runs the kernel. Exhausting that is
///   the failure this precompile exists to pin: it is not a WASM trap, so the
///   PVM has nothing to absorb and goes `Stuck`.
///
/// The body must therefore keep its frame small so the host stack is the one
/// that runs out first. A fat frame (e.g. a `[0u8; 256]` local) exhausts the
/// 1 MiB shadow stack within a few thousand calls and traps out of bounds *with
/// or without* the depth guard — pinning nothing. Spilling only a scalar keeps
/// the frame to ~16 bytes, so shadow-stack exhaustion is pushed far out and the
/// host stack dies first: without the instrumentation this reaches `Stuck` and
/// the test fails, with it the guard traps first at `DEFAULT_MAX_CALL_DEPTH`.
///
/// `black_box` is load-bearing twice over: it stops LLVM from folding the
/// recursion into a loop, and by consuming `deeper` afterwards it keeps the
/// self-call from becoming a tail call.
#[allow(unconditional_recursion)]
fn stack_overflow(depth: u64) -> u64 {
    let deeper = stack_overflow(std::hint::black_box(depth) + 1);
    std::hint::black_box(deeper).wrapping_add(1)
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
        Panic::PanicCalls::stack_overflow(Panic::stack_overflowCall {}) => {
            let _ = stack_overflow(0);
            Err(CustomPrecompileError::Revert(
                "stack_overflow precompile returned without trapping".to_string(),
                gas,
            ))
        }
    }
}
