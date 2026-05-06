// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm_database_interface::DBErrorMarker;
use tezos_smart_rollup_host::{path::PathError, runtime::RuntimeError};
use thiserror::Error;

#[derive(Error, Debug, PartialEq, Eq, Clone)]
pub enum Error {
    #[error("Runtime error: {0}")]
    Runtime(#[from] RuntimeError),
    #[error("Execution error: {0}")]
    Custom(String),
    /// Converting non-execution fees to gas overflowed u64::max
    #[error("Gas for fees overflowed u64::max in conversion")]
    FeesToGasOverflow,
    /// Underflow of gas limit when subtracting gas for fees
    #[error("Insufficient gas to cover the non-execution fees")]
    GasToFeesUnderflow,
}

pub fn custom<E: std::fmt::Display>(e: E) -> Error {
    Error::Custom(e.to_string())
}

impl From<PathError> for Error {
    fn from(e: PathError) -> Self {
        Error::Custom(e.to_string())
    }
}

impl From<std::string::FromUtf8Error> for Error {
    fn from(e: std::string::FromUtf8Error) -> Self {
        Error::Custom(e.to_string())
    }
}

impl DBErrorMarker for Error {}
