// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

// #![cfg(feature = "experimental-host-in-memory-store")]

use crate::RollupHost;

#[doc(hidden)]
pub fn kernel_entrypoint_fn(user_kernel_fn: fn(&mut RollupHost)) {
    crate::set_panic_hook();
    let mut host = unsafe { RollupHost::new() };
    user_kernel_fn(&mut host)
}
