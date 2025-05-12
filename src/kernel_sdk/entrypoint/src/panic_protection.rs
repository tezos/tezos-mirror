// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

#![cfg(pvm_kind = "riscv")]

// This module is enabled for RISC-V targets where `std` is available.
extern crate alloc;
extern crate std;

use alloc::format;
use core::{any::Any, panic::RefUnwindSafe};
use std::panic::{catch_unwind, resume_unwind};
use tezos_smart_rollup_host::runtime::{unwindable::UnwindableRuntime, Runtime};

/// Try to turn a panic payload into a string.
#[inline]
fn panic_payload_to_string(payload: &dyn Any) -> &str {
    if let Some(message) = payload.downcast_ref::<alloc::string::String>() {
        message.as_str()
    } else if let Some(message) = payload.downcast_ref::<&str>() {
        *message
    } else {
        "<unknown>"
    }
}

/// Wrap a kernel entrypoint function in order to catch panics it may raise.
#[inline(always)]
pub fn main_loop<R, F>(runtime: R, go: F) -> !
where
    R: Runtime,
    F: Fn(&mut UnwindableRuntime<R>) + RefUnwindSafe,
{
    let runtime = UnwindableRuntime::new(runtime);

    loop {
        let Err(error) = catch_unwind(runtime.wrap(&go)) else {
            continue;
        };

        let message = panic_payload_to_string(&error);
        let message = format!("Uncaught top-level panic: {message}");
        runtime.unreliably_write_debug(message.as_str());

        // The use of `is_poisoned` is safe here because there is no other thread.
        if runtime.is_poisoned() {
            // A poisoned lock prevents us from acquiring it again in the next run. We have no option but to continue
            // unwinding.
            resume_unwind(error)
        }
    }
}
