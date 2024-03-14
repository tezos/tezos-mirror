// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::fail_if_too_much;
use crate::precompiles::{tick_model, PrecompileOutcome};
use crate::{handler::EvmHandler, EthereumError};
use evm::{Context, ExitReason, ExitSucceed, Transfer};
use host::runtime::Runtime;
use ripemd::Ripemd160;
use sha2::{Digest, Sha256};
use tezos_evm_logging::log;
use tezos_evm_logging::Level::Debug;

// Implementation of 0x03 precompiled (sha256)
pub fn sha256_precompile<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
    _context: &Context,
    _is_static: bool,
    _transfer: Option<Transfer>,
) -> Result<PrecompileOutcome, EthereumError> {
    log!(handler.borrow_host(), Debug, "Calling sha2-256 precompile");
    let estimated_ticks =
        fail_if_too_much!(tick_model::ticks_of_sha256(input.len())?, handler);

    let size = input.len() as u64;
    let data_word_size = (31 + size) / 32;
    let cost = 60 + 12 * data_word_size;

    if let Err(err) = handler.record_cost(cost) {
        return Ok(PrecompileOutcome {
            exit_status: ExitReason::Error(err),
            output: vec![],
            withdrawals: vec![],
            estimated_ticks,
        });
    }

    let output = Sha256::digest(input);

    Ok(PrecompileOutcome {
        exit_status: ExitReason::Succeed(ExitSucceed::Returned),
        output: output.to_vec(),
        withdrawals: vec![],
        estimated_ticks,
    })
}

// Implementation of 0x04 precompiled (ripemd160)
pub fn ripemd160_precompile<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
    _context: &Context,
    _is_static: bool,
    _transfer: Option<Transfer>,
) -> Result<PrecompileOutcome, EthereumError> {
    log!(
        handler.borrow_host(),
        Debug,
        "Calling ripemd-160 precompile"
    );
    let estimated_ticks =
        fail_if_too_much!(tick_model::ticks_of_ripemd160(input.len())?, handler);

    let size = input.len() as u64;
    let data_word_size = (31 + size) / 32;
    let cost = 600 + 120 * data_word_size;

    if let Err(err) = handler.record_cost(cost) {
        return Ok(PrecompileOutcome {
            exit_status: ExitReason::Error(err),
            output: vec![],
            withdrawals: vec![],
            estimated_ticks,
        });
    }

    let hash = Ripemd160::digest(input);
    // The 20-byte hash is returned right aligned to 32 bytes
    let mut output = [0u8; 32];
    output[12..].clone_from_slice(&hash);

    Ok(PrecompileOutcome {
        exit_status: ExitReason::Succeed(ExitSucceed::Returned),
        output: output.to_vec(),
        withdrawals: vec![],
        estimated_ticks,
    })
}
