// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

#![deny(missing_docs)]
#![deny(rustdoc::broken_intra_doc_links)]

//! Hook to capture kernel panics, and write them to the debug log.
//!
//! The hook will abort execution once the panic has been printed to the
//! debug log.

use std::panic::PanicInfo;

/// Prints the panic info to the host's *debug log*, and then aborts.
///
/// When targeting WASM, this will be the *global* panic handler.
#[allow(unused)]
pub fn panic_handler(info: &PanicInfo) {
    #[cfg(feature = "debug")]
    {
        let message =
            if let Some(message) = info.payload().downcast_ref::<std::string::String>() {
                format!("Kernel panic {:?} at {:?}", message, info.location())
            } else {
                let message = info.payload().downcast_ref::<&str>();
                format!("Kernel panic {:?} at {:?}", message, info.location())
            };

        #[cfg(target_arch = "wasm32")]
        unsafe {
            tezos_smart_rollup_core::smart_rollup_core::write_debug(
                message.as_ptr(),
                message.len(),
            );
        }

        #[cfg(not(target_arch = "wasm32"))]
        eprintln!("{}", message);
    }

    // If we're testing, we want to be able to see the panic trace
    #[cfg(all(feature = "abort", target_arch = "wasm32"))]
    std::process::abort()
}
