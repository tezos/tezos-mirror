// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025-2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use alloy_sol_types::{sol, SolInterface, SolValue};
use evm_types::CustomPrecompileError;
use revm::{
    context::ContextTr,
    interpreter::{CallInputs, Gas, InstructionResult, InterpreterResult},
    primitives::Bytes,
};

use tezos_smart_rollup_host::storage::StorageV1;
use tezosx_interfaces::Registry;

use crate::{
    database::EtherlinkVMDB,
    journal::Journal,
    precompiles::{
        constants::{
            FA_BRIDGE_SOL_ADDR, GLOBAL_COUNTER_BASE_COST,
            GLOBAL_COUNTER_PRECOMPILE_ADDRESS, XTZ_BRIDGE_SOL_ADDR,
        },
        guard::{charge, guard, revert},
    },
};

sol! {
    contract GlobalCounter {
        function get_and_increment() external;
    }
}

pub(crate) fn global_counter_precompile<'j, CTX, Host, R>(
    calldata: &[u8],
    context: &mut CTX,
    inputs: &CallInputs,
) -> Result<InterpreterResult, CustomPrecompileError>
where
    Host: StorageV1 + 'j,
    R: Registry + 'j,
    CTX: ContextTr<Db = EtherlinkVMDB<'j, Host, R>, Journal = Journal<'j, Host, R>>,
{
    guard(
        GLOBAL_COUNTER_PRECOMPILE_ADDRESS,
        &[XTZ_BRIDGE_SOL_ADDR, FA_BRIDGE_SOL_ADDR],
        inputs,
    )?;

    let mut gas = Gas::new(inputs.gas_limit);
    charge(&mut gas, GLOBAL_COUNTER_BASE_COST)?;

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
