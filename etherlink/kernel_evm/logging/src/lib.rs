// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

#[doc(hidden)]
pub use tezos_smart_rollup_debug::debug_str;

#[derive(PartialEq)]
pub enum Level {
    Info,
    Error,
    Fatal,
    Debug,
    Benchmarking,
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

#[cfg(all(feature = "alloc", any(feature = "debug", feature = "benchmark")))]
#[macro_export]
macro_rules! log {
    ($host: expr, $level: expr, $fmt: expr $(, $arg:expr)*)  => {
        {
            let msg = format!("[{}] {}\n", $level, format_args!($fmt $(, $arg)*));
            $crate::debug_str!($host, &msg);
        }
    };
}

#[cfg(all(feature = "alloc", not(feature = "debug"), not(feature = "benchmark")))]
#[macro_export]
macro_rules! log {
    ($host: expr, Debug, $fmt: expr $(, $arg:expr)*)  => {
        let _ = $host;
        let _ = $fmt;
        $(let _ = $arg;)*
    };
    ($host: expr, Benchmarking, $fmt: expr $(, $arg:expr)*)  => {
        let _ = $host;
        let _ = $fmt;
        $(let _ = $arg;)*
    };
    ($host: expr, $level: expr, $fmt: expr $(, $arg:expr)*)  => {
        {
            let msg = format!("[{}] {}\n", $level, format_args!($fmt $(, $arg)*));
            $crate::debug_str!($host, &msg);
        }
    };
}
