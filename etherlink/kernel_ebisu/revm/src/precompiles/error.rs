// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup_host::runtime::RuntimeError;

use crate::Error;

#[derive(Debug)]
pub enum CustomPrecompileError {
    Abort(RuntimeError),
    Revert(String),
}

impl From<RuntimeError> for CustomPrecompileError {
    fn from(error: RuntimeError) -> Self {
        match error {
            RuntimeError::PathNotFound => {
                CustomPrecompileError::Revert("Path not found".to_string())
            }
            other => CustomPrecompileError::Abort(other),
        }
    }
}

impl From<Error> for CustomPrecompileError {
    fn from(error: Error) -> Self {
        match error {
            Error::Runtime(runtime) => CustomPrecompileError::from(runtime),
            Error::Custom(message) => CustomPrecompileError::Revert(message),
        }
    }
}
