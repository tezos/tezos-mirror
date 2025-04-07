// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
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
