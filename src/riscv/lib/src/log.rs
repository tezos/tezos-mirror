// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

#![allow(unused_imports, unused_macros, reason = "Not all events may be used")]

#[cfg(feature = "log")]
#[doc(hidden)]
pub(crate) mod implementation {
    // Usage of [`__tracing_do_not_use_directly`] is allowed here

    pub(crate) use __tracing_do_not_use_directly::debug;
    pub(crate) use __tracing_do_not_use_directly::error;
    pub(crate) use __tracing_do_not_use_directly::info;
    pub(crate) use __tracing_do_not_use_directly::trace;
    // Rename `warn` to avoid conflict with a Rust attribute of the same name
    pub(crate) use __tracing_do_not_use_directly::warn as warning;
}

#[cfg(not(feature = "log"))]
#[doc(hidden)]
pub(crate) mod implementation {
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

/// Helper macro for logging
///
/// It is mainly used to:
///     - Define the shape of the macro inputs for logging
///     - Eagerly evaluate expressions used when logging
///     - Forward the macro input to the right logging macro (this avoids duplicating macros)
macro_rules! __log_proxy {
    (
        [$macro:ident]
        $(
            $name:ident
            $(= $value:expr)?
            ,
        )*
        $msg:literal
        $(, $msg_param:expr)*
        $(,)?
    ) => {
        {
            // This is a workaround to avoid unused variable warnings
            if false {
                $(
                    $( let _ = $value; )?
                )*
                $(
                    let _ = $msg_param;
                )*
            }

            $crate::log::implementation::$macro!(
                $($name $(= $value)? ,)*
                $msg
                $(, $msg_param)*
            );
        }
    };
}

macro_rules! error {
    ($($body:tt)*) => {
        $crate::log::__log_proxy!([error] $($body)*);
    };
}

macro_rules! warning {
    ($($body:tt)*) => {
        $crate::log::__log_proxy!([warning] $($body)*);
    };
}

macro_rules! info {
    ($($body:tt)*) => {
        $crate::log::__log_proxy!([info] $($body)*);
    };
}

macro_rules! debug {
    ($($body:tt)*) => {
        $crate::log::__log_proxy!([debug] $($body)*);
    };
}

macro_rules! trace {
    ($($body:tt)*) => {
        $crate::log::__log_proxy!([trace] $($body)*);
    };
}

#[doc(hidden)]
pub(crate) use __log_proxy;
#[cfg(feature = "log")]
#[doc(hidden)]
// Exporting the `tracing` crate for use in the API or Sandbox helps
//  - use the same `tracing` version across crates
//  - avoid a Cargo manifest mess where the same dependency has multiple names
pub use __tracing_do_not_use_directly as tracing_internal;
pub(crate) use debug;
pub(crate) use error;
pub(crate) use info;
pub(crate) use trace;
pub(crate) use warning;
