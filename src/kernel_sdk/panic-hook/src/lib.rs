// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

#![deny(missing_docs)]
#![deny(rustdoc::broken_intra_doc_links)]

//! Hook to capture kernel panics, and write them to the debug log.
//!
//! The hook will abort execution once the panic has been printed to the
//! debug log.

// Don't depend on standard library by default.
#![no_std]

// If 'std' is on, pull in the standard library.
#[cfg(feature = "std")]
extern crate std;

extern crate alloc;

use core::panic::PanicInfo;

/// Prints the panic info to the host's *debug log*, and then aborts.
///
/// When targeting WASM, this will be the *global* panic handler.
#[allow(unused)]
pub fn panic_handler(info: &PanicInfo) {
    #[cfg(feature = "debug")]
    {
        let message = if let Some(message) =
            info.payload().downcast_ref::<alloc::string::String>()
        {
            alloc::format!("Kernel panic {:?} at {:?}", message, info.location())
        } else {
            let message = info.payload().downcast_ref::<&str>();
            alloc::format!("Kernel panic {:?} at {:?}", message, info.location())
        };

        #[cfg(any(target_arch = "wasm32", target_arch = "riscv64"))]
        unsafe {
            tezos_smart_rollup_core::smart_rollup_core::write_debug(
                message.as_ptr(),
                message.len(),
            );
        }

        #[cfg(all(feature = "std", not(target_arch = "wasm32")))]
        std::eprintln!("{}", message);
    }

    // If we're testing, we want to be able to see the panic trace
    #[cfg(all(feature = "abort", target_arch = "wasm32"))]
    std::process::abort()
}
