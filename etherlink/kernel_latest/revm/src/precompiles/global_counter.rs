// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use alloy_sol_types::{sol, SolInterface, SolValue};
use revm::{
    context::ContextTr,
    interpreter::{Gas, InputsImpl, InstructionResult, InterpreterResult},
    primitives::Bytes,
};

use crate::{
    database::DatabasePrecompileStateChanges,
    journal::Journal,
    precompiles::{
        constants::{
            FA_BRIDGE_SOL_ADDR, GLOBAL_COUNTER_BASE_COST,
            GLOBAL_COUNTER_PRECOMPILE_ADDRESS, WITHDRAWAL_SOL_ADDR,
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
    input: &[u8],
    context: &mut CTX,
    is_static: bool,
    transfer: &InputsImpl,
    gas_limit: u64,
) -> Result<InterpreterResult, CustomPrecompileError>
where
    DB: DatabasePrecompileStateChanges,
    CTX: ContextTr<Db = DB, Journal = Journal<DB>>,
{
    guard(
        GLOBAL_COUNTER_PRECOMPILE_ADDRESS,
        &[WITHDRAWAL_SOL_ADDR, FA_BRIDGE_SOL_ADDR],
        transfer,
        is_static,
    )?;

    let mut gas = Gas::new(gas_limit);
    if !gas.record_cost(GLOBAL_COUNTER_BASE_COST) {
        return Ok(out_of_gas(gas_limit));
    }

    let interface = match GlobalCounter::GlobalCounterCalls::abi_decode(input) {
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
