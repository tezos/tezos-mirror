// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm::{
    interpreter::{Gas, InputsImpl, InstructionResult, InterpreterResult},
    primitives::{Address, Bytes},
};
use std::fmt::Display;

use crate::precompiles::error::CustomPrecompileError;

pub(crate) fn guard(
    current: Address,
    authorized: &[Address],
    transfer: &InputsImpl,
    is_static: bool,
) -> Result<(), CustomPrecompileError> {
    if transfer.target_address != current {
        return Err(CustomPrecompileError::Revert(
            "invalid transfer target address".to_string(),
        ));
    }
    if is_static {
        return Err(CustomPrecompileError::Revert(
            "invalid transfer target address".to_string(),
        ));
    }
    if !authorized.contains(&transfer.caller_address) {
        return Err(CustomPrecompileError::Revert(
            "unauthorized caller".to_string(),
        ));
    }
    Ok(())
}

pub(crate) fn revert<R>(reason: R, gas: Gas) -> InterpreterResult
where
    R: Display,
{
    InterpreterResult {
        result: InstructionResult::Revert,
        gas,
        output: Bytes::copy_from_slice(reason.to_string().as_bytes()),
    }
}

pub(crate) const OOG: InterpreterResult = InterpreterResult {
    result: InstructionResult::OutOfGas,
    gas: Gas::new(0),
    output: Bytes::new(),
};
