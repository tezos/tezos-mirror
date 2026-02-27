// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Provides *debug log* which can be written to, but does not affect the host state.
//!
//! The result of writing to the debug log is *implementation specific* - it may, for
//! example, be written to a log file, or to `stderr` etc.
#![no_std]
#![deny(missing_docs)]
#![deny(rustdoc::broken_intra_doc_links)]

// Re-exported so that the wasm macro body can reference it via `$crate::__core`
// without requiring callers to depend on `tezos-smart-rollup-core` directly.
#[cfg(pvm_kind = "wasm")]
#[doc(hidden)]
pub use tezos_smart_rollup_core as __core;

/// Write a formatted message to host debug log. Formats follow [`core::fmt`].
///
/// On WASM the message is written via the host's `write_debug` import. On all
/// other targets it is written to `stderr` via `eprint!`.
///
/// The `host` argument is accepted for backward compatibility but is not used.
///
/// # Example
/// ```no_run
/// extern crate alloc;
/// use tezos_smart_rollup_debug::debug_msg;
///
/// fn log_something() {
///   debug_msg!((), "Simple constant string");
///   debug_msg!((), "A format {} with argument {}", "test", 5);
/// }
/// ```
#[cfg(feature = "alloc")]
#[macro_export]
macro_rules! debug_msg {
    ($host: expr, $($args: expr),*) => {{
        let _host = &$host;
        extern crate alloc;
        $crate::debug_str!($host, { &alloc::format!($($args),*) });
    }};
}

/// Write a string to the debug log.
///
/// While this is less powerful than [`debug_msg`] (which supports format strings),
/// it does _not_ require an allocator in order to be used.
///
/// On WASM the message is written via the host's `write_debug` import. On all
/// other targets it is written to `stderr` via `eprint!`.
///
/// The `host` argument is accepted for backward compatibility but is not used.
///
/// # Example
/// ```no_run
/// use tezos_smart_rollup_debug::debug_str;
///
/// fn do_something() {
///   debug_str!((), "Simple constant string");
/// }
/// ```
#[cfg(pvm_kind = "wasm")]
#[macro_export]
macro_rules! debug_str {
    ($host: expr, $msg: expr) => {{
        let _host = &$host;
        let msg: &str = $msg;
        // SAFETY: msg is a valid UTF-8 string so its pointer/length are valid
        unsafe { $crate::__core::target_impl::write_debug(msg.as_ptr(), msg.len()) };
    }};
}

/// Write a string to the debug log.
///
/// While this is less powerful than [`debug_msg`] (which supports format strings),
/// it does _not_ require an allocator in order to be used.
///
/// On WASM the message is written via the host's `write_debug` import. On all
/// other targets it is written to `stderr` via `eprint!`.
///
/// The `host` argument is accepted for backward compatibility but is not used.
///
/// # Example
/// ```no_run
/// use tezos_smart_rollup_debug::debug_str;
///
/// fn do_something() {
///   debug_str!((), "Simple constant string");
/// }
/// ```
#[cfg(not(pvm_kind = "wasm"))]
#[macro_export]
macro_rules! debug_str {
    ($host: expr, $msg: expr) => {{
        let _host = &$host;
        extern crate std;
        std::eprint!("{}", $msg);
    }};
}
