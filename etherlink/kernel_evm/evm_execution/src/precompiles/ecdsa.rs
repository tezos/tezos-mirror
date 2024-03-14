// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::fail_if_too_much;
use crate::precompiles::tick_model;
use crate::{handler::EvmHandler, precompiles::PrecompileOutcome, EthereumError};
use evm::{Context, Transfer};
use evm::{ExitReason, ExitSucceed};
use host::runtime::Runtime;
use libsecp256k1::{recover, Message, RecoveryId, Signature};
use sha2::Digest;
use sha3::Keccak256;
use std::cmp::min;
use tezos_evm_logging::log;
use tezos_evm_logging::Level::{Debug, Info};

macro_rules! unwrap_ecrecover {
    ($expr : expr) => {
        match $expr {
            Ok(x) => x,
            Err(_) => return Ok(erec_output_for_wrong_input()),
        }
    };
}

fn erec_output_for_wrong_input() -> PrecompileOutcome {
    PrecompileOutcome {
        exit_status: ExitReason::Succeed(ExitSucceed::Returned),
        output: vec![],
        withdrawals: vec![],
        estimated_ticks: 0,
    }
}

pub fn erec_parse_inputs(input: &[u8]) -> ([u8; 32], [u8; 32], [u8; 64]) {
    // input is padded with 0 on the right
    let mut clean_input: [u8; 128] = [0; 128];
    // and truncated if too large
    let input_size = min(128, input.len());
    clean_input[..input_size].copy_from_slice(&input[..input_size]);

    // extract values
    let mut hash = [0; 32];
    let mut v_array = [0; 32];
    let mut rs_array = [0; 64];
    hash.copy_from_slice(&clean_input[0..32]);
    v_array.copy_from_slice(&clean_input[32..64]);
    rs_array.copy_from_slice(&clean_input[64..128]);
    (hash, v_array, rs_array)
}

// Implementation of 0x01 ECDSA recover
pub fn ecrecover_precompile<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
    _context: &Context,
    _is_static: bool,
    _transfer: Option<Transfer>,
) -> Result<PrecompileOutcome, EthereumError> {
    log!(handler.borrow_host(), Debug, "Calling ecrecover precompile");

    // check that enough resources to execute (gas / ticks) are available
    let estimated_ticks = fail_if_too_much!(tick_model::ticks_of_ecrecover(), handler);
    let cost = 3000;
    if let Err(err) = handler.record_cost(cost) {
        log!(
            handler.borrow_host(),
            Info,
            "Couldn't record the cost of ecrecover {:?}",
            err
        );
        return Ok(PrecompileOutcome {
            exit_status: ExitReason::Error(err),
            output: vec![],
            withdrawals: vec![],
            estimated_ticks,
        });
    }

    log!(
        handler.borrow_host(),
        Debug,
        "Input is {:?}",
        hex::encode(input)
    );

    // parse inputs
    let (hash, v_array, rs_array) = erec_parse_inputs(input);
    let v_raw = v_array[31];

    if !(v_array[0..31] == [0u8; 31] && matches!(v_raw, 27 | 28)) {
        return Ok(erec_output_for_wrong_input());
    }

    // `parse_standard` will check for potential overflows
    let sig = unwrap_ecrecover!(Signature::parse_standard(&rs_array));
    let ri = unwrap_ecrecover!(RecoveryId::parse(v_raw - 27));

    // check signature
    let pubk = unwrap_ecrecover!(recover(&Message::parse(&hash), &sig, &ri));
    let mut hash = Keccak256::digest(&pubk.serialize()[1..]);
    hash[..12].fill(0);

    log!(
        handler.borrow_host(),
        Debug,
        "Output is {:?}",
        hex::encode(hash)
    );

    Ok(PrecompileOutcome {
        exit_status: ExitReason::Succeed(ExitSucceed::Returned),
        output: hash.to_vec(),
        withdrawals: vec![],
        estimated_ticks,
    })
}
