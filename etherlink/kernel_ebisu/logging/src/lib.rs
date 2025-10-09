// SPDX-FileCopyrightText: 2023-2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

#[doc(hidden)]
pub use tezos_smart_rollup_debug::debug_str;

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

#[repr(u8)]
#[derive(PartialEq, Clone, Copy, PartialOrd, FromPrimitive)]
pub enum Level {
    Fatal = 0,
    Error,
    Info,
    Debug,
    Benchmarking,
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
        }
    }
}

pub trait Verbosity {
    fn verbosity(&self) -> Level;
}

#[cfg(feature = "alloc")]
#[macro_export]
macro_rules! log {
    ($host: expr, $level: expr, $fmt: expr $(, $arg:expr)*)  => {
        if $host.verbosity() >= $level {
            let msg = format!("[{}] {}\n", $level, format_args!($fmt $(, $arg)*));
            $crate::debug_str!($host, &msg);
        }
    };
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
