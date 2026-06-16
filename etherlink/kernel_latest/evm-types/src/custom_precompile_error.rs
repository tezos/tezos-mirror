// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm_interpreter::Gas;
use tezos_smart_rollup_host::runtime::RuntimeError;
use thiserror::Error;

/// Actual errors that propagate up past the precompile boundary,
/// never caught by the provider.
#[derive(Debug, Error)]
pub enum CustomPrecompileAbort {
    #[error("Runtime error: {0}")]
    Runtime(RuntimeError),
    #[error("CRAC block abort: {0}")]
    Crac(String),
}

/// Outcome of a custom precompile call
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
