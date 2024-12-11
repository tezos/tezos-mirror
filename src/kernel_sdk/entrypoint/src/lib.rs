// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

#![doc = include_str!("../README.md")]
#![deny(missing_docs)]
#![deny(rustdoc::broken_intra_doc_links)]
#![cfg_attr(not(feature = "std"), no_std)]

mod kernel_entrypoint;
mod panic_protection;

#[cfg(feature = "experimental-host-in-memory-store")]
pub(crate) mod host_in_memory_store;

#[cfg(all(feature = "dlmalloc", pvm_kind = "wasm"))]
mod allocator {
    use dlmalloc::GlobalDlmalloc;

    #[global_allocator]
    static ALLOCATOR: GlobalDlmalloc = GlobalDlmalloc;
}

/// Set panic hook
#[cfg(feature = "panic-hook")]
pub fn set_panic_hook() {
    std::panic::set_hook(Box::new(tezos_smart_rollup_panic_hook::panic_handler));
}

/// Dummy panic hook that does nothing.
#[cfg(not(feature = "panic-hook"))]
pub fn set_panic_hook() {}

#[cfg(feature = "alloc")]
extern crate alloc;

/// Derive `kernel_run` & `mock_kernel_run` entrypoints.
///
/// ```no_run
/// # extern crate alloc;
/// #[macro_use] extern crate tezos_smart_rollup_entrypoint;
/// #[macro_use] extern crate tezos_smart_rollup_debug;
///
/// use tezos_smart_rollup_host::runtime::Runtime;
///
/// fn run<Host: Runtime>(host: &mut Host) {
///   debug_msg!(host, "Hello: {}", "Kernel!");
/// }
///
/// # #[cfg(doc)]
/// kernel_entry!(run);
/// ```
#[deprecated]
#[macro_export]
macro_rules! kernel_entry {
    ($kernel_run: expr) => {
        #[tezos_smart_rollup::entrypoint::main]
        fn __tezos_wrapper_kernel_entrypoint(
            host: &mut impl tezos_smart_rollup::prelude::Runtime,
        ) {
            $kernel_run(host);
        }
    };
}

#[doc(hidden)]
#[cfg(not(feature = "experimental-host-in-memory-store"))]
pub use tezos_smart_rollup_core::rollup_host::RollupHost;

#[doc(hidden)]
#[cfg(feature = "experimental-host-in-memory-store")]
pub use host_in_memory_store::RollupHostWithInMemoryStorage as RollupHost;

#[doc(hidden)]
#[allow(unused_imports)]
pub use kernel_entrypoint::*;
