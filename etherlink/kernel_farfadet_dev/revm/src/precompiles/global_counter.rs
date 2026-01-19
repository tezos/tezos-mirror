// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025-2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use alloy_sol_types::{sol, SolInterface, SolValue};
use revm::{
    context::ContextTr,
    interpreter::{CallInputs, Gas, InstructionResult, InterpreterResult},
    primitives::Bytes,
};

use crate::{
    database::DatabasePrecompileStateChanges,
    journal::Journal,
    precompiles::{
        constants::{
            FA_BRIDGE_SOL_ADDR, GLOBAL_COUNTER_BASE_COST,
            GLOBAL_COUNTER_PRECOMPILE_ADDRESS, XTZ_BRIDGE_SOL_ADDR,
        },
        error::CustomPrecompileError,
        guard::{guard, out_of_gas, revert},
    },
};

sol! {
    contract GlobalCounter {
        function get_and_increment() external;
    }
}

pub(crate) fn global_counter_precompile<CTX, DB>(
    calldata: &[u8],
    context: &mut CTX,
    inputs: &CallInputs,
) -> Result<InterpreterResult, CustomPrecompileError>
where
    DB: DatabasePrecompileStateChanges,
    CTX: ContextTr<Db = DB, Journal = Journal<DB>>,
{
    guard(
        GLOBAL_COUNTER_PRECOMPILE_ADDRESS,
        &[XTZ_BRIDGE_SOL_ADDR, FA_BRIDGE_SOL_ADDR],
        inputs,
    )?;

    let mut gas = Gas::new(inputs.gas_limit);
    if !gas.record_cost(GLOBAL_COUNTER_BASE_COST) {
        return Ok(out_of_gas(inputs.gas_limit));
    }

    let interface = match GlobalCounter::GlobalCounterCalls::abi_decode(calldata) {
        Ok(data) => data,
        Err(e) => return Ok(revert(e, gas)),
    };

    let counter = match interface {
        GlobalCounter::GlobalCounterCalls::get_and_increment(
            GlobalCounter::get_and_incrementCall,
        ) => context.journal_mut().get_and_increment_global_counter()?,
    };

    Ok(InterpreterResult {
        result: InstructionResult::Return,
        gas,
        output: Bytes::from(counter.abi_encode()),
    })
}
