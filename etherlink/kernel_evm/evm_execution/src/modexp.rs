// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 draganrakita
//
// SPDX-License-Identifier: MIT

use crate::{
    handler::EvmHandler,
    precompiles::PrecompileOutcome,
    utilities::{get_right_padded, get_right_padded_vec, left_padding_vec},
    EthereumError,
};
use aurora_engine_modexp::modexp;
use evm::{
    executor::stack::PrecompileFailure, Context, ExitError, ExitReason, ExitSucceed,
    Transfer,
};
use host::runtime::Runtime;
use primitive_types::U256;
use tezos_evm_logging::log;
use tezos_evm_logging::Level::Info;

// The format of input is:
// <length_of_BASE> <length_of_EXPONENT> <length_of_MODULUS> <BASE> <EXPONENT> <MODULUS>
// Where every length is a 32-byte left-padded integer representing the number of bytes
// to be taken up by the next value
const HEADER_LENGTH: usize = 96;

// There is no gas check during the call to modexp precompile contract.
// It should be done externally.
pub fn modexp_precompile<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
    _context: &Context,
    _is_static: bool,
    _transfer: Option<Transfer>,
) -> Result<PrecompileOutcome, EthereumError> {
    log!(handler.borrow_host(), Info, "Calling modexp precompile");
    // TODO: Make the actual tick estimation (remove the stub value).
    let estimated_ticks = 1_000_000;

    // Extract the header.
    let base_len = U256::from_big_endian(&get_right_padded::<32>(input, 0));
    let exp_len = U256::from_big_endian(&get_right_padded::<32>(input, 32));
    let mod_len = U256::from_big_endian(&get_right_padded::<32>(input, 64));

    // cast base and modulus to usize, it does not make sense to handle larger values
    let Ok(base_len) = usize::try_from(base_len) else {
        return Err(EthereumError::PrecompileFailed(PrecompileFailure::Error { exit_status: ExitError::Other(std::borrow::Cow::Borrowed("modexp mod overflow")) }));
    };
    let Ok(mod_len) = usize::try_from(mod_len) else {
        return Err(EthereumError::PrecompileFailed(PrecompileFailure::Error { exit_status: ExitError::Other(std::borrow::Cow::Borrowed("modexp mod overflow")) }));
    };

    // Handle a special case when both the base and mod length is zero
    if base_len == 0 && mod_len == 0 {
        return Ok(PrecompileOutcome {
            exit_status: ExitReason::Succeed(ExitSucceed::Returned),
            output: vec![],
            withdrawals: vec![],
            estimated_ticks,
        });
    }

    // cast exponent length to usize, it does not make sense to handle larger values.
    let Ok(exp_len) = usize::try_from(exp_len) else {
        return Err(EthereumError::PrecompileFailed(PrecompileFailure::Error { exit_status: ExitError::Other(std::borrow::Cow::Borrowed("modexp mod overflow")) }));
    };

    // throw away the header data as we already extracted lengths.
    let input = if input.len() >= HEADER_LENGTH {
        &input[HEADER_LENGTH..]
    } else {
        // or set input to zero if there is no more data
        &[]
    };

    // Padding is needed if the input does not contain all 3 values.
    let base = get_right_padded_vec(input, 0, base_len);
    let exponent = get_right_padded_vec(input, base_len, exp_len);
    let modulus = get_right_padded_vec(input, base_len.saturating_add(exp_len), mod_len);

    // Call the modexp.
    let output = modexp(&base, &exponent, &modulus);

    Ok(PrecompileOutcome {
        exit_status: ExitReason::Succeed(ExitSucceed::Returned),
        // left pad the result to modulus length. bytes will always by less or equal to modulus length.
        output: left_padding_vec(&output, mod_len),
        withdrawals: vec![],
        estimated_ticks,
    })
}
