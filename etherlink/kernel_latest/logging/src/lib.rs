// SPDX-FileCopyrightText: 2023-2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2026 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

cfg_if::cfg_if! {
    if #[cfg(not(pvm_kind = "none"))] {
        #[doc(hidden)]
        pub use tezos_smart_rollup_debug::debug_str;
    } else if #[cfg(feature = "capture-logs")] {
        use std::cell::RefCell;

        thread_local! {
            /// Capture of all calls to `debug_str` that occurred in the current thread.
            pub static DEBUG_LOG: RefCell<Vec<u8>> = RefCell::default();
        }

        #[macro_export]
        macro_rules! debug_str {
            ($msg: expr) => {{
                $crate::DEBUG_LOG.with_borrow_mut(|log| log.extend_from_slice($msg.as_bytes()));
            }};
        }
    } else {
        #[macro_export]
        macro_rules! debug_str {
            ($msg: expr) => {{
                eprint!("{}", $msg);
            }};
        }
    }
}

#[repr(u8)]
#[derive(PartialEq, Clone, Copy, PartialOrd, FromPrimitive)]
pub enum Level {
    Fatal = 0,
    Error,
    Info,
    Debug,
    Benchmarking,
    OTel,
}

impl TryFrom<u8> for Level {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, ()> {
        FromPrimitive::from_u8(value).ok_or(())
    }
}

impl Default for Level {
    fn default() -> Self {
        if cfg!(feature = "debug") {
            Self::Debug
        } else if cfg!(feature = "benchmark") {
            Self::Benchmarking
        } else {
            Self::Info
        }
    }
}

impl std::fmt::Display for Level {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Level::Info => write!(f, "Info"),
            Level::Error => write!(f, "Error"),
            Level::Fatal => write!(f, "Fatal"),
            Level::Debug => write!(f, "Debug"),
            Level::Benchmarking => write!(f, "Benchmarking"),
            Level::OTel => write!(f, "OTel"),
        }
    }
}

#[cfg(feature = "alloc")]
#[macro_export]
macro_rules! log {
    ($host: expr, $level: expr, $fmt: expr $(, $arg:expr)*)  => {{
        use $crate::Logging as _;
        if $host.verbosity() >= $level {
            let msg = format!("[{}] {}\n", $level, format_args!($fmt $(, $arg)*));
            $crate::debug_str!(&msg);
        }
    }};
}

#[cfg(all(feature = "tracing", feature = "alloc"))]
#[macro_export]
macro_rules! __trace_kernel {
    ($name:expr, $expr:expr) => {{
        let msg = format!("[{}] [start] {}", tezos_evm_logging::Level::OTel, $name);
        $crate::debug_str!(&msg);
        let __otel_result = { $expr };
        let msg = format!("[{}] [end] {}", tezos_evm_logging::Level::OTel, $name);
        $crate::debug_str!(&msg);
        __otel_result
    }};
}

pub enum OTelAttrValue {
    Bool(bool),
    Int(i32),
    // Floats are forbidden in the kernel, we use a string representation instead.
    // It's not statically typed so the values must be actual rust floats inside
    // the string otherwise they will be interpreted as strings and not floats.
    // Note that nothing will panic or crash, in the worst case the value will not
    // be interpreted with the proper type.
    Float(String),
    String(String),
}

#[cfg(all(feature = "tracing", feature = "alloc"))]
#[macro_export]
macro_rules! __trace_kernel_add_attrs {
    ($attrs:expr) => {{
        use $crate::OTelAttrValue;

        let attrs_str = $attrs
            .iter()
            .map(|(k, v)| {
                let value_str = match v {
                    OTelAttrValue::Bool(b) => format!("bool:{}", b),
                    OTelAttrValue::Int(i) => format!("int:{}", i),
                    OTelAttrValue::Float(f) => format!("float:{}", f),
                    OTelAttrValue::String(s) => format!("string:{}", s),
                };
                format!("{}®{}®", k, value_str)
            })
            .collect::<Vec<_>>()
            .join(" ");

        let msg = format!("[{}] [attrs] {}", tezos_evm_logging::Level::OTel, attrs_str);
        $crate::debug_str!(&msg);
    }};
}

// Must only be used by the procedural macro for kernel tracing.
#[cfg(feature = "tracing")]
pub fn internal_trace_kernel<F, R>(name: &str, f: F) -> R
where
    F: FnOnce() -> R,
{
    let msg = format!("[{}] [start] {}", crate::Level::OTel, name);
    debug_str!(&msg);
    let res = f();

    let msg = format!("[{}] [end] {}", crate::Level::OTel, name);
    debug_str!(&msg);

    res
}

#[cfg(not(feature = "tracing"))]
#[macro_export]
macro_rules! __trace_kernel_add_attrs {
    ($attrs:expr) => {{}};
}

#[cfg(not(feature = "tracing"))]
#[macro_export]
macro_rules! __trace_kernel {
    ($name:expr, $expr:expr) => {{
        $expr
    }};
}

// When the `tracing` feature is enabled, export tracing macros
#[cfg(feature = "tracing")]
pub mod tracing {
    pub use tracing::instrument;

    // Spans are not exported when the `tracing` feature is disabled, so their
    // use must be guarded by a feature check.
    pub use tracing::Span;

    // Spans are not exported when the `tracing` feature is disabled, so their
    // use must be guarded by a feature check.
    pub use tracing::info_span;

    /// Execute the given function in the context of a new [`Span`]
    #[macro_export]
    macro_rules! trace {
        ($name:expr, $f:expr) => {{
            tracing::info_span!($name).in_scope(|| $f)
        }};
    }
}

// When the `tracing` feature is disabled, export dummy tracing macros
#[cfg(not(feature = "tracing"))]
pub mod tracing {
    pub use nop_macros::nop as instrument;

    /// Execute the given function in the context of a new [`Span`]
    #[macro_export]
    macro_rules! trace {
        ($name:expr, $f:expr) => {{
            $f
        }};
    }
}

pub trait Logging {
    fn verbosity(&self) -> Level;
}
