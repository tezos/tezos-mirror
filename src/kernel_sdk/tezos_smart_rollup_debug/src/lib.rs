// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Provides *debug log* which can be written to, but does not affect the host state.
//!
//! The result of writing to the debug log is *implementation specific* - it may, for
//! example, be written to a log file, or to `stdout` etc.
#![cfg_attr(target_arch = "wasm32", no_std)]
#![deny(missing_docs)]
#![deny(rustdoc::broken_intra_doc_links)]

/// Write a formatted message to host debug log. Formats follow [`core::fmt`].
///
/// You can use `debug_msg!` with any variable such that implements [`Runtime`].
/// This is supported by any type implementing [`SmartRollupCore`] - such as
/// [`RollupHost`].
///
/// Using `debug_msg!` requires an allocator - so either you will need to include
/// `extern crate alloc` when writing a kernel in a `no_std` context, or by
/// depending on `std`. If you want to write debug logs without pulling in the
/// allocator, use [debug_str] instead.
///
/// ```no_run
/// extern crate alloc;
/// use tezos_smart_rollup_debug::debug_msg;
/// use tezos_smart_rollup_host::runtime::Runtime;
///
/// fn log_runtime(host: &impl Runtime) {
///   debug_msg!(host, "Simple constant string");
///
///   debug_msg!(host, "A format {} with argument {}", "test", 5);
/// }
/// ```
///
/// [`Runtime`]: tezos_smart_rollup_host::runtime::Runtime
/// [`SmartRollupCore`]: tezos_smart_rollup_core::smart_rollup_core::SmartRollupCore
/// [`RollupHost`]: tezos_smart_rollup_core::rollup_host::RollupHost
#[cfg(feature = "alloc")]
#[macro_export]
macro_rules! debug_msg {
    ($host: expr, $($args: expr),*) => {
        {
            extern crate alloc;
            $crate::debug_str!($host, { &alloc::format!($($args), *) });
        }
    };
}

/// Write a string to the debug log.
///
/// While this is less powerful than [`debug_msg`] (which supports format strings),
/// it does _not_ require an allocator in order to be used.
///
/// Just like `debug_msg`, you can use it with any variable implementing the [`Runtime`]
/// trait.
///
/// ```no_run
/// use tezos_smart_rollup_debug::debug_str;
/// use tezos_smart_rollup_host::runtime::Runtime;
///
/// fn do_something(host: &impl Runtime) {
///   debug_str!(host, "Simple constant string");
/// }
/// ```
///
/// [`Runtime`]: tezos_smart_rollup_host::runtime::Runtime
#[macro_export]
macro_rules! debug_str {
    ($host: expr, $msg: expr) => {{
        use $crate::Runtime;
        $host.write_debug($msg);
    }};
}

#[doc(hidden)]
pub use tezos_smart_rollup_host::runtime::Runtime;
