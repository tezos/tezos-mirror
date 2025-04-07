// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! ABI utility functions
//!
//! This implements the bare minimum for handling contract call
//! parameters encoded using Solidity ABI standard. See the documentation
//! at
//! [Contract ABI specification](https://docs.soliditylang.org/en/develop/abi-spec.html)

// TODO: https://gitlab.com/tezos/tezos/-/issues/7722
// This whole file is hack-ish.
// In the long term we should get rid of this file completely and rely on a proper
// implementation for abi parameters (for instance see cast's implementation to
// depend on the same crates).

use primitive_types::{H160, U256};

/// All arguments in ABI encoding are padded to 32 bytes
/// https://docs.soliditylang.org/en/develop/abi-spec.html#formal-specification-of-the-encoding
pub const ABI_H160_LEFT_PADDING: [u8; 12] = [0u8; 12];
pub const ABI_U32_LEFT_PADDING: [u8; 28] = [0u8; 28];
pub const ABI_B22_RIGHT_PADDING: [u8; 10] = [0u8; 10];

/// Get a single 32 bytes/256 bit parameter from a contract call, input data buffer
pub fn u256_parameter(input_data: &[u8], parameter_number: usize) -> Option<U256> {
    let location = parameter_number * 32;
    input_data
        .get(location..location + 32)
        .map(|bytes| bytes.into())
}

/// Get the bytes of a dynamic parameter from a contract call, input data buffer
pub fn bytes_parameter(input_data: &[u8], parameter_number: usize) -> Option<&[u8]> {
    let location: usize = u256_parameter(input_data, parameter_number)?
        .try_into()
        .ok()?;
    let length: usize = U256::from(input_data.get(location..location + 32)?)
        .try_into()
        .ok()?;
    input_data.get(location + 32..location + 32 + length)
}

/// Get a string parameter from a contract call, input data buffer
pub fn string_parameter(input_data: &[u8], parameter_number: usize) -> Option<&str> {
    core::str::from_utf8(bytes_parameter(input_data, parameter_number)?).ok()
}

/// Get an address parameter from the input data buffer
pub fn h160_parameter(input_data: &[u8], parameter_number: usize) -> Option<H160> {
    let location = parameter_number * 32;
    if location + 32 <= input_data.len() {
        // Check leading zeroes
        if input_data[location..location + 12]
            .iter()
            .all(|x| *x == 0u8)
        {
            let mut parameter = [0u8; 20];
            parameter.copy_from_slice(&input_data[location + 12..location + 32]);
            Some(H160(parameter))
        } else {
            None
        }
    } else {
        None
    }
}

/// Get a fixed N bytes parameter from the input data buffer, where 0 < N < 32
pub fn fixed_bytes_parameter<const N: usize>(
    input_data: &[u8],
    parameter_number: usize,
) -> Option<[u8; N]> {
    let location = parameter_number * 32;
    if location + 32 <= input_data.len() {
        // Check trailing zeroes
        if input_data[location + N..location + 32]
            .iter()
            .all(|x| *x == 0u8)
        {
            let mut parameter = [0u8; N];
            parameter.copy_from_slice(&input_data[location..location + N]);
            Some(parameter)
        } else {
            None
        }
    } else {
        None
    }
}
