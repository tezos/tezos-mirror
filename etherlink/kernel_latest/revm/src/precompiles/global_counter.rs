// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
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
            FA_WITHDRAWAL_SOL_ADDR, GLOBAL_COUNTER_BASE_COST,
            GLOBAL_COUNTER_PRECOMPILE_ADDRESS, WITHDRAWAL_SOL_ADDR,
        },
        provider::{revert, OOG},
    },
};

sol! {
    contract GlobalCounter {
        function get_and_increment() external;
    }
}

pub(crate) fn global_counte_precompile<CTX, DB>(
    input: &[u8],
    context: &mut CTX,
    is_static: bool,
    transfer: &InputsImpl,
    gas_limit: u64,
) -> InterpreterResult
where
    DB: DatabasePrecompileStateChanges,
    CTX: ContextTr<Db = DB, Journal = Journal<DB>>,
{
    if transfer.target_address != GLOBAL_COUNTER_PRECOMPILE_ADDRESS {
        return revert("invalid transfer target address");
    }

    if is_static {
        return revert("static calls are not allowed");
    }

    if !matches!(
        transfer.caller_address,
        WITHDRAWAL_SOL_ADDR | FA_WITHDRAWAL_SOL_ADDR
    ) {
        return revert("unauthorized caller");
    }

    let mut gas = Gas::new(gas_limit);

    if !gas.record_cost(GLOBAL_COUNTER_BASE_COST) {
        return OOG;
    }

    let interface = match GlobalCounter::GlobalCounterCalls::abi_decode(input) {
        Ok(data) => data,
        Err(e) => return revert(e),
    };

    let counter = match interface {
        GlobalCounter::GlobalCounterCalls::get_and_increment(
            GlobalCounter::get_and_incrementCall,
        ) => match context.journal_mut().get_and_increment_global_counter() {
            Ok(value) => value,
            Err(e) => return revert(e),
        },
    };

    InterpreterResult {
        result: InstructionResult::Return,
        gas,
        output: Bytes::from(counter.abi_encode()),
    }
}
