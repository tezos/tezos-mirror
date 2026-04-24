// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm_interpreter::Gas;
use tezos_smart_rollup_host::runtime::RuntimeError;

use crate::{Error, PrecompileStateError};

/// Actual errors that propagate up past the precompile boundary
/// (never caught by the provider).
#[derive(Debug)]
pub enum CustomPrecompileAbort {
    Runtime(RuntimeError),
    Crac(String),
}

#[derive(Debug)]
pub enum CustomPrecompileError {
    Revert(String, Gas),
    OutOfGas,
    Abort(CustomPrecompileAbort),
}

impl From<CustomPrecompileAbort> for CustomPrecompileError {
    fn from(abort: CustomPrecompileAbort) -> Self {
        CustomPrecompileError::Abort(abort)
    }
}

impl From<CustomPrecompileAbort> for Error {
    fn from(abort: CustomPrecompileAbort) -> Self {
        match abort {
            CustomPrecompileAbort::Runtime(e) => Error::Runtime(e),
            CustomPrecompileAbort::Crac(msg) => {
                Error::Custom(format!("CRAC block abort: {msg}"))
            }
        }
    }
}

/// Classify a source error into [`CustomPrecompileError`],
/// attaching the remaining `gas` to revert outcomes.
pub trait IntoWithRemainder {
    fn into_with_remainder(self, gas: Gas) -> CustomPrecompileError;
}

impl IntoWithRemainder for RuntimeError {
    fn into_with_remainder(self, gas: Gas) -> CustomPrecompileError {
        match self {
            RuntimeError::PathNotFound => {
                CustomPrecompileError::Revert("Path not found".to_string(), gas)
            }
            other => CustomPrecompileError::Abort(CustomPrecompileAbort::Runtime(other)),
        }
    }
}

impl IntoWithRemainder for Error {
    fn into_with_remainder(self, gas: Gas) -> CustomPrecompileError {
        match self {
            Error::Runtime(e) => e.into_with_remainder(gas),
            Error::Custom(msg) => CustomPrecompileError::Revert(msg, gas),
            Error::FeesToGasOverflow => CustomPrecompileError::Revert(
                "Fees to gas conversion overflow".to_string(),
                gas,
            ),
            Error::GasToFeesUnderflow => CustomPrecompileError::Revert(
                "Gas to fees conversion underflow".to_string(),
                gas,
            ),
        }
    }
}

// Transitional: lets tezosx_journal::LayeredState and
// revm::Journal keep their pre-rework `CustomPrecompileError`
// signatures while the DB trait switches to `PrecompileStateError`.
// Removed in the next commit once LayeredState returns its own
// narrow error type.
impl From<PrecompileStateError> for CustomPrecompileError {
    fn from(e: PrecompileStateError) -> Self {
        e.into_with_remainder(Gas::new(0))
    }
}
