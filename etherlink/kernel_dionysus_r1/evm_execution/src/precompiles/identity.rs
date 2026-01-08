// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::precompiles::tick_model;
use crate::{handler::EvmHandler, precompiles::PrecompileOutcome, EthereumError};
use evm::{Context, Transfer};
use evm::{ExitReason, ExitSucceed};
use tezos_evm_logging::log;
use tezos_evm_logging::Level::Debug;
use tezos_evm_runtime::runtime::Runtime;

// Implementation of 0x02 precompiled (identity)
pub fn identity_precompile<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
    _context: &Context,
    _is_static: bool,
    _transfer: Option<Transfer>,
) -> Result<PrecompileOutcome, EthereumError> {
    log!(handler.borrow_host(), Debug, "Calling identity precompile");
    let estimated_ticks = tick_model::ticks_of_identity(input.len());

    let size = input.len() as u64;
    let data_word_size = size.div_ceil(32);
    let static_gas = 15;
    let dynamic_gas = 3 * data_word_size;
    let cost = static_gas + dynamic_gas;

    if let Err(err) = handler.record_cost(cost) {
        return Ok(PrecompileOutcome {
            exit_status: ExitReason::Error(err),
            output: vec![],
            withdrawals: vec![],
            estimated_ticks,
        });
    }

    Ok(PrecompileOutcome {
        exit_status: ExitReason::Succeed(ExitSucceed::Returned),
        output: input.to_vec(),
        withdrawals: vec![],
        estimated_ticks,
    })
}
