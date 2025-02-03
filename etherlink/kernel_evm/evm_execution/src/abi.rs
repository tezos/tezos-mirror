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

pub fn fast_withdrawal_parameters(input_data: &[u8]) -> Option<(&str, &str, Vec<u8>)> {
    // Disclaimer: The following implementation is REALLY hack-ish. But it's the most straighforward
    // decoding function until we find a better crate to handle these encodings.
    // We ignore the first three slices of 32 bytes, they are the position of the parameters.
    // In our case, since it's a hack-ish implementation, we can directly infer them.
    const U256_SIZE: usize = 32;

    let first_arg_size: usize = U256::from(input_data.get(3 * U256_SIZE..4 * U256_SIZE)?)
        .try_into()
        .ok()?;
    let number_of_slice = first_arg_size.div_ceil(U256_SIZE);
    let read_to = first_arg_size + 4 * U256_SIZE;
    let arg_1 = &input_data.get(4 * U256_SIZE..read_to)?;
    let arg_1 = core::str::from_utf8(arg_1).ok()?;

    let from = U256_SIZE * number_of_slice + 4 * U256_SIZE;
    let second_args_size: usize = U256::from(input_data.get(from..(from + U256_SIZE))?)
        .try_into()
        .ok()?;
    let number_of_slice = second_args_size.div_ceil(U256_SIZE);
    let read_to = (from + U256_SIZE) + second_args_size;
    let arg_2 = &input_data.get((from + U256_SIZE)..read_to)?;
    let arg_2 = core::str::from_utf8(arg_2).ok()?;

    let from = U256_SIZE * number_of_slice + (from + U256_SIZE);
    let third_args_size: usize = U256::from(input_data.get(from..(from + U256_SIZE))?)
        .try_into()
        .ok()?;
    let read_to = third_args_size + (from + U256_SIZE);
    let arg_3 = &input_data.get((from + U256_SIZE)..read_to)?;
    let arg_3 = arg_3.to_vec();

    Some((arg_1, arg_2, arg_3))
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

#[cfg(test)]
mod test {
    use super::fast_withdrawal_parameters;

    #[test]
    fn test_fast_withdrawal_parameters() {
        let test_case = hex::decode("000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000000000024747a316670356e63446d7159775943353638665245597a3969775154674751754b5a7158000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000024747a316670356e63446d7159775943353638665245597a3969775154674751754b5a7158000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000").unwrap();
        let (arg_1, arg_2, arg_3) = fast_withdrawal_parameters(&test_case).unwrap();
        assert_eq!("tz1fp5ncDmqYwYC568fREYz9iwQTgGQuKZqX", arg_1);
        assert_eq!("tz1fp5ncDmqYwYC568fREYz9iwQTgGQuKZqX", arg_2);
        assert_eq!(Vec::<u8>::new(), arg_3)
    }
}
