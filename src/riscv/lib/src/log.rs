// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

#![allow(unused_imports, unused_macros, reason = "Not all events may be used")]

#[cfg(feature = "log")]
mod implementation {
    // Usage of [`__tracing_do_not_use_directly`] is allowed here

    pub(crate) use __tracing_do_not_use_directly::debug;
    pub(crate) use __tracing_do_not_use_directly::error;
    pub(crate) use __tracing_do_not_use_directly::info;
    pub(crate) use __tracing_do_not_use_directly::trace;
    // Rename `warn` to avoid conflict with a Rust attribute of the same name
    pub(crate) use __tracing_do_not_use_directly::warn as warning;
}

#[cfg(not(feature = "log"))]
mod implementation {
    macro_rules! error {
        ($($ignore:tt)*) => {};
    }

    macro_rules! warning {
        ($($ignore:tt)*) => {};
    }

    macro_rules! info {
        ($($ignore:tt)*) => {};
    }

    macro_rules! debug {
        ($($ignore:tt)*) => {};
    }

    macro_rules! trace {
        ($($ignore:tt)*) => {};
    }

    // Export the macros from this module
    pub(crate) use debug;
    pub(crate) use error;
    pub(crate) use info;
    pub(crate) use trace;
    pub(crate) use warning;
}

#[cfg(feature = "log")]
#[doc(hidden)]
// Exporting the `tracing` crate for use in the API or Sandbox helps
//  - use the same `tracing` version across crates
//  - avoid a Cargo manifest mess where the same dependency has multiple names
pub use __tracing_do_not_use_directly as tracing_internal;
// Re-export the tracing macros for library-internal use
pub(crate) use implementation::*;
