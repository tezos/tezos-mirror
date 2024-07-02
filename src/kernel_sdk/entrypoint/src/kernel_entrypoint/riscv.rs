// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::RollupHost;

#[doc(hidden)]
pub fn kernel_entrypoint_fn<F: Fn(&mut RollupHost)>(user_kernel_fn: F) -> ! {
    crate::set_panic_hook();
    let mut host = unsafe { RollupHost::new() };
    loop {
        // TODO #6727: Capture and recover panics.
        user_kernel_fn(&mut host);
    }
}
