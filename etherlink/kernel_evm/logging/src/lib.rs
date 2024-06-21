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

#[cfg(feature = "alloc")]
#[macro_export]
macro_rules! log {
    ($host: expr, $level: expr, $fmt: expr $(, $arg:expr)*)  => {
        match $level {
            $crate::Level::Debug => {
                #[cfg(not(feature = "debug"))]
                {}
                #[cfg(feature = "debug")]
                {
                    let msg = format!("[{}] {}\n", $level, format_args!($fmt $(, $arg)*));
                    $crate::debug_str!($host, &msg);
                }
            }
            $crate::Level::Benchmarking => {
                #[cfg(not(feature = "benchmark"))]
                {}
                #[cfg(feature = "benchmark")]
                {
                    let msg = format!("[{}] {}\n", $level, format_args!($fmt $(, $arg)*));
                    $crate::debug_str!($host, &msg);
                }
            }
            $crate::Level::Info | $crate::Level::Error | $crate::Level::Fatal => {
                let msg = format!("[{}] {}\n", $level, format_args!($fmt $(, $arg)*));
                $crate::debug_str!($host, &msg);
            }
        }
    };
}
