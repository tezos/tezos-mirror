// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::precompiles::{tick_model, PrecompileOutcome};
use crate::{handler::EvmHandler, EthereumError};
use evm::{Context, ExitReason, ExitSucceed, Transfer};
use ripemd::Ripemd160;
use sha2::{Digest, Sha256};
use tezos_evm_logging::log;
use tezos_evm_logging::Level::Debug;
use tezos_evm_runtime::runtime::Runtime;

// Implementation of 0x03 precompiled (sha256)
pub fn sha256_precompile<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
    _context: &Context,
    _is_static: bool,
    _transfer: Option<Transfer>,
) -> Result<PrecompileOutcome, EthereumError> {
    log!(handler.borrow_host(), Debug, "Calling sha2-256 precompile");
    let estimated_ticks = tick_model::ticks_of_sha256(input.len());

    let size = input.len() as u64;
    let data_word_size = size.div_ceil(32);
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
    let estimated_ticks = tick_model::ticks_of_ripemd160(input.len());

    let size = input.len() as u64;
    let data_word_size = size.div_ceil(32);
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

#[cfg(test)]
mod tests {
    use evm::ExitSucceed;
    use primitive_types::H160;

    use crate::{
        handler::{ExecutionOutcome, ExecutionResult},
        precompiles::test_helpers::execute_precompiled,
    };

    #[test]
    fn call_sha256() {
        // act
        let input: &[u8] = &[0xFF];
        let address = H160::from_low_u64_be(2u64);
        let result = execute_precompiled(address, input, None, Some(22000), true);

        // assert
        let expected_hash = hex::decode(
            "a8100ae6aa1940d0b663bb31cd466142ebbdbd5187131b92d93818987832eb89",
        )
        .expect("Result should be hex string");

        let expected_gas = 21000 // base cost
            + 72 // sha256 cost
            + 16; // transaction data cost

        let expected = ExecutionOutcome {
            gas_used: expected_gas,
            logs: vec![],
            result: ExecutionResult::CallSucceeded(ExitSucceed::Returned, expected_hash),
            withdrawals: vec![],
            estimated_ticks_used: 75_000,
        };

        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn call_ripemd() {
        // act
        let input: &[u8] = &[0xFF];
        let address = H160::from_low_u64_be(3u64);
        let result = execute_precompiled(address, input, None, Some(22000), true);

        // assert
        let expected_hash = hex::decode(
            "0000000000000000000000002c0c45d3ecab80fe060e5f1d7057cd2f8de5e557",
        )
        .expect("Result should be hex string");

        let expected_gas = 21000 // base cost
        + 600 + 120// ripeMD cost
        + 16; // transaction data cost

        let expected = ExecutionOutcome {
            gas_used: expected_gas,
            logs: vec![],
            result: ExecutionResult::CallSucceeded(ExitSucceed::Returned, expected_hash),
            withdrawals: vec![],
            estimated_ticks_used: 70_000,
        };

        assert_eq!(Ok(expected), result);
    }
}
