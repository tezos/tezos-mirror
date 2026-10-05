// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use evm_types::CustomPrecompileError;
use revm::{
    interpreter::{CallInputs, Gas},
    primitives::Address,
};
use tezosx_interfaces::{EvmGas, Gas as TezosXGas, RuntimeId};

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

pub(crate) fn charge(
    gas: &mut Gas,
    cost: impl Into<EvmGas>,
) -> Result<(), CustomPrecompileError> {
    if gas.record_cost(u64::from(cost.into())) {
        Ok(())
    } else {
        Err(CustomPrecompileError::OutOfGas)
    }
}

/// Run `f` with a cross-runtime [`TezosXGas`] budget holding what is left of
/// `gas`, then charge `gas` for what `f` consumed from it.
///
/// Bridges REVM's [`Gas`] with the cross-runtime APIs, which meter a
/// runtime-tagged budget. The charge happens whatever `f` returns, so the
/// work done before a failure is still paid for: `f`'s result, error
/// included, is returned for the caller to handle.
pub(crate) fn with_revm_gas_budget<T>(
    gas: &mut Gas,
    f: impl FnOnce(&mut TezosXGas) -> T,
) -> Result<T, CustomPrecompileError> {
    let budget = TezosXGas::new(gas.remaining(), RuntimeId::Ethereum);
    let mut remaining = budget;
    let result = f(&mut remaining);
    charge(gas, budget - remaining)?;
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn with_revm_gas_budget_charges_what_was_consumed() {
        let mut gas = Gas::new(1_000);
        let result = with_revm_gas_budget(&mut gas, |budget| {
            budget.consume(TezosXGas::new(300, RuntimeId::Ethereum))
        })
        .unwrap();
        assert_eq!(result, Ok(()));
        assert_eq!(gas.remaining(), 700);
    }

    // The work done before a failure is charged, and the failure returned.
    #[test]
    fn with_revm_gas_budget_charges_even_when_f_fails() {
        let mut gas = Gas::new(1_000);
        let result = with_revm_gas_budget(&mut gas, |budget| {
            budget.consume(TezosXGas::new(300, RuntimeId::Ethereum))?;
            budget.consume(TezosXGas::new(1_000, RuntimeId::Ethereum))
        })
        .unwrap();
        assert_eq!(result, Err(tezosx_interfaces::TezosXRuntimeError::OutOfGas));
        assert_eq!(gas.remaining(), 700);
    }

    // A cost in milligas is charged rounded up to the next EVM gas.
    #[test]
    fn with_revm_gas_budget_rounds_a_finer_cost_up() {
        let mut gas = Gas::new(1_000);
        with_revm_gas_budget(&mut gas, |budget| {
            budget.consume(TezosXGas::new(1, RuntimeId::Tezos))
        })
        .unwrap()
        .unwrap();
        assert_eq!(gas.remaining(), 999);
    }
}
