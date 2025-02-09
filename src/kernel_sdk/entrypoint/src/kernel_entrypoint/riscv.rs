// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::RollupHost;
use core::panic::RefUnwindSafe;
use tezos_smart_rollup_host::runtime::unwindable::UnwindableRuntime;

#[doc(hidden)]
pub fn kernel_entrypoint_fn<F: Fn(&mut UnwindableRuntime<RollupHost>) + RefUnwindSafe>(
    user_kernel_fn: F,
) -> ! {
    let host = unsafe { RollupHost::new() };
    crate::panic_protection::main_loop(host, user_kernel_fn)
}
