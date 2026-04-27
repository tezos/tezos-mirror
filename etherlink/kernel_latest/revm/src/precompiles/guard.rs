// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use evm_types::CustomPrecompileError;
use revm::{
    interpreter::{CallInputs, Gas},
    primitives::Address,
};

pub(crate) fn guard(
    current: Address,
    authorized: &[Address],
    inputs: &CallInputs,
    gas: Gas,
) -> Result<(), CustomPrecompileError> {
    if inputs.target_address != inputs.bytecode_address {
        return Err(CustomPrecompileError::Revert(
            "DELEGATECALLs and CALLCODEs are not allowed".to_string(),
            gas,
        ));
    }
    if inputs.target_address != current {
        return Err(CustomPrecompileError::Revert(
            "invalid transfer target address".to_string(),
            gas,
        ));
    }
    if inputs.is_static {
        return Err(CustomPrecompileError::Revert(
            "STATICCALLs are not allowed".to_string(),
            gas,
        ));
    }
    if !authorized.contains(&inputs.caller) {
        return Err(CustomPrecompileError::Revert(
            "unauthorized caller".to_string(),
            gas,
        ));
    }
    Ok(())
}

pub(crate) fn charge(gas: &mut Gas, cost: u64) -> Result<(), CustomPrecompileError> {
    if gas.record_cost(cost) {
        Ok(())
    } else {
        Err(CustomPrecompileError::OutOfGas)
    }
}
