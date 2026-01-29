// SPDX-FileCopyrightText: 2022-2023,2026 TriliTech <contact@trili.tech>
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
#[cfg(any(feature = "std", pvm_kind = "riscv"))]
extern crate std;

extern crate alloc;

#[rustversion::before(1.81)]
mod panic_hook_arg {
    /// Panic info type
    pub type PanicHookArg<'a> = core::panic::PanicInfo<'a>;
}

#[rustversion::since(1.81)]
mod panic_hook_arg {
    /// Panic info type
    #[rustversion::attr(since(1.81), cfg(any(feature = "std", pvm_kind = "riscv")))]
    pub type PanicHookArg<'a> = std::panic::PanicHookInfo<'a>;
}

#[rustversion::attr(since(1.81), cfg(any(feature = "std", pvm_kind = "riscv")))]
pub use panic_hook_arg::PanicHookArg;

/// Prints the panic info to the host's *debug log*, and then aborts.
///
/// When targeting WASM, this will be the *global* panic handler.
#[rustversion::attr(since(1.81), cfg(any(feature = "std", pvm_kind = "riscv")))]
#[allow(unused)]
pub fn panic_handler(info: &PanicHookArg) {
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

        #[cfg(pvm_kind = "wasm")]
        unsafe {
            tezos_smart_rollup_core::target_impl::write_debug(
                message.as_ptr(),
                message.len(),
            );
        }

        #[cfg(any(feature = "std", pvm_kind = "riscv"))]
        std::eprintln!("{message}");
    }

    // We don't want to abort when testing because that prevents the panic trace
    // from being printed.
    #[cfg(all(feature = "abort", not(pvm_kind = "none")))]
    std::process::abort()
}
