// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025-2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use alloy_sol_types::{sol, SolInterface, SolValue};
use evm_types::{CustomPrecompileError, IntoWithRemainder};
use revm::{
    context::ContextTr,
    interpreter::{CallInputs, Gas, InstructionResult, InterpreterResult},
    primitives::Bytes,
};

use tezosx_interfaces::Registry;

use crate::{
    database::EtherlinkVMDB,
    journal::Journal,
    precompiles::{
        constants::{
            FA_BRIDGE_SOL_ADDR, GLOBAL_COUNTER_BASE_COST,
            GLOBAL_COUNTER_PRECOMPILE_ADDRESS, XTZ_BRIDGE_SOL_ADDR,
        },
        guard::{charge, guard},
    },
};
use tezos_evm_runtime::snapshot::{KeyspaceHost, SafeKeyspace};

sol! {
    contract GlobalCounter {
        function get_and_increment() external;
    }
}

pub(crate) fn global_counter_precompile<'j, CTX, Host, KS, R>(
    calldata: &[u8],
    context: &mut CTX,
    inputs: &CallInputs,
) -> Result<InterpreterResult, CustomPrecompileError>
where
    Host: KeyspaceHost<KS> + 'j,
    KS: SafeKeyspace + 'j,
    R: Registry<Journal = tezosx_journal::TezosXJournal> + 'j,
    CTX: ContextTr<
        Db = EtherlinkVMDB<'j, Host, KS, R>,
        Journal = Journal<'j, Host, KS, R>,
    >,
{
    let mut gas = Gas::new(inputs.gas_limit);
    guard(
        GLOBAL_COUNTER_PRECOMPILE_ADDRESS,
        &[XTZ_BRIDGE_SOL_ADDR, FA_BRIDGE_SOL_ADDR],
        inputs,
        gas,
    )?;

    charge(&mut gas, GLOBAL_COUNTER_BASE_COST)?;

    let interface = GlobalCounter::GlobalCounterCalls::abi_decode(calldata)
        .map_err(|e| CustomPrecompileError::Revert(e.to_string(), gas))?;

    let counter = match interface {
        GlobalCounter::GlobalCounterCalls::get_and_increment(
            GlobalCounter::get_and_incrementCall,
        ) => context
            .journal_mut()
            .get_and_increment_global_counter()
            .map_err(|e| e.into_with_remainder(gas))?,
    };

    Ok(InterpreterResult {
        result: InstructionResult::Return,
        gas,
        output: Bytes::from(counter.abi_encode()),
    })
}
