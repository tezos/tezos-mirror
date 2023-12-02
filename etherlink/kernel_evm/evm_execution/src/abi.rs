// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! ABI utility functions
//!
//! This implements the bare minimum for handling contract call
//! parameters encoded using Solidity ABI standard. See the documentation
//! at
//! [Contract ABI specification](https://docs.soliditylang.org/en/develop/abi-spec.html)

// TODO investigate if we can use this Rust package instead:
// https://docs.rs/ethabi/latest/ethabi/

use primitive_types::U256;

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
