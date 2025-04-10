// SputnikVM - Apache 2.0 LICENSE - https://github.com/rust-ethereum/evm/blob/master/LICENSE
//
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::precompiles::call_precompile_with_gas_draining;
use crate::precompiles::tick_model;
use crate::{handler::EvmHandler, precompiles::PrecompileOutcome, EthereumError};
use alloc::borrow::Cow;
use evm::{executor::stack::PrecompileFailure, ExitError};
use evm::{Context, ExitReason, ExitSucceed, Transfer};
use tezos_evm_logging::log;
use tezos_evm_logging::Level::{Debug, Info};
use tezos_evm_runtime::runtime::Runtime;

/// The precomputed values for BLAKE2b [from the spec](https://tools.ietf.org/html/rfc7693#section-2.7)
/// There are 10 16-byte arrays - one for each round
/// the entries are calculated from the sigma constants.
const SIGMA: [[usize; 16]; 10] = [
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
    [14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3],
    [11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4],
    [7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8],
    [9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13],
    [2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9],
    [12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11],
    [13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10],
    [6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5],
    [10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0],
];

/// IV is the initialization vector for BLAKE2b. See https://tools.ietf.org/html/rfc7693#section-2.6
/// for details.
const IV: [u64; 8] = [
    0x6a09e667f3bcc908,
    0xbb67ae8584caa73b,
    0x3c6ef372fe94f82b,
    0xa54ff53a5f1d36f1,
    0x510e527fade682d1,
    0x9b05688c2b3e6c1f,
    0x1f83d9abfb41bd6b,
    0x5be0cd19137e2179,
];

#[inline(always)]
/// The G mixing function. See https://tools.ietf.org/html/rfc7693#section-3.1
fn g(v: &mut [u64], a: usize, b: usize, c: usize, d: usize, x: u64, y: u64) {
    v[a] = v[a].wrapping_add(v[b]).wrapping_add(x);
    v[d] = (v[d] ^ v[a]).rotate_right(32);
    v[c] = v[c].wrapping_add(v[d]);
    v[b] = (v[b] ^ v[c]).rotate_right(24);
    v[a] = v[a].wrapping_add(v[b]).wrapping_add(y);
    v[d] = (v[d] ^ v[a]).rotate_right(16);
    v[c] = v[c].wrapping_add(v[d]);
    v[b] = (v[b] ^ v[c]).rotate_right(63);
}

/// The Blake2 compression function F. See https://tools.ietf.org/html/rfc7693#section-3.2
/// Takes as an argument the state vector `h`, message block vector `m`, offset counter `t`, final
/// block indicator flag `f`, and number of rounds `rounds`. The state vector provided as the first
/// parameter is modified by the function.
pub fn compress(h: &mut [u64; 8], m: [u64; 16], t: [u64; 2], f: bool, rounds: usize) {
    let mut v = [0u64; 16];
    v[..h.len()].copy_from_slice(h); // First half from state.
    v[h.len()..].copy_from_slice(&IV); // Second half from IV.

    v[12] ^= t[0];
    v[13] ^= t[1];

    if f {
        v[14] = !v[14] // Invert all bits if the last-block-flag is set.
    }
    for i in 0..rounds {
        // Message word selection permutation for this round.
        let s = &SIGMA[i % 10];
        g(&mut v, 0, 4, 8, 12, m[s[0]], m[s[1]]);
        g(&mut v, 1, 5, 9, 13, m[s[2]], m[s[3]]);
        g(&mut v, 2, 6, 10, 14, m[s[4]], m[s[5]]);
        g(&mut v, 3, 7, 11, 15, m[s[6]], m[s[7]]);

        g(&mut v, 0, 5, 10, 15, m[s[8]], m[s[9]]);
        g(&mut v, 1, 6, 11, 12, m[s[10]], m[s[11]]);
        g(&mut v, 2, 7, 8, 13, m[s[12]], m[s[13]]);
        g(&mut v, 3, 4, 9, 14, m[s[14]], m[s[15]]);
    }

    for i in 0..8 {
        h[i] ^= v[i] ^ v[i + 8];
    }
}

trait Decodable {
    fn decode_from_le_slice(&mut self, source: &[u8]);
}

impl<const N: usize> Decodable for [u64; N] {
    fn decode_from_le_slice(&mut self, src: &[u8]) {
        let mut word_buf = [0_u8; 8];
        for (i, word) in self.iter_mut().enumerate() {
            word_buf.copy_from_slice(&src[i * 8..(i + 1) * 8]);
            *word = u64::from_le_bytes(word_buf);
        }
    }
}

fn blake2f_output_for_wrong_input() -> EthereumError {
    EthereumError::PrecompileFailed(PrecompileFailure::Error {
        exit_status: ExitError::Other(Cow::from("Wrong input for blake2f precompile")),
    })
}

fn blake2f_precompile_without_gas_draining<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
) -> Result<PrecompileOutcome, EthereumError> {
    log!(handler.borrow_host(), Debug, "Calling blake2f precompile");

    // The precompile requires 6 inputs tightly encoded, taking exactly 213 bytes
    if input.len() != 213 {
        return Err(blake2f_output_for_wrong_input());
    }

    // the number of rounds - 32-bit unsigned big-endian word
    let mut rounds_buf = [0_u8; 4];
    rounds_buf.copy_from_slice(&input[0..4]);
    let rounds: u32 = u32::from_be_bytes(rounds_buf);

    let estimated_ticks = tick_model::ticks_of_blake2f(rounds);
    let cost = rounds as u64; // static_gas + dynamic_gas
    if let Err(err) = handler.record_cost(cost) {
        log!(
            handler.borrow_host(),
            Info,
            "Couldn't record the cost of blake2f {:?}",
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
    // the state vector - 8 unsigned 64-bit little-endian words
    let mut h = [0_u64; 8];
    h.decode_from_le_slice(&input[4..68]);

    // the message block vector - 16 unsigned 64-bit little-endian words
    let mut m = [0_u64; 16];
    m.decode_from_le_slice(&input[68..196]);

    // offset counters - 2 unsigned 64-bit little-endian words
    let mut t = [0_u64; 2];
    t.decode_from_le_slice(&input[196..212]);

    // the final block indicator flag - 8-bit word (true if 1 or false if 0)
    let f = match input[212] {
        1 => true,
        0 => false,
        _ => return Err(blake2f_output_for_wrong_input()),
    };

    compress(&mut h, m, t, f, rounds as usize);

    let mut output = [0_u8; 64];
    for (i, state_word) in h.iter().enumerate() {
        output[i * 8..(i + 1) * 8].copy_from_slice(&state_word.to_le_bytes());
    }
    log!(
        handler.borrow_host(),
        Debug,
        "Output is {:?}",
        hex::encode(output)
    );
    Ok(PrecompileOutcome {
        exit_status: ExitReason::Succeed(ExitSucceed::Returned),
        output: output.to_vec(),
        withdrawals: vec![],
        estimated_ticks,
    })
}

pub fn blake2f_precompile<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
    _context: &Context,
    _is_static: bool,
    _transfer: Option<Transfer>,
) -> Result<PrecompileOutcome, EthereumError> {
    call_precompile_with_gas_draining(
        handler,
        input,
        blake2f_precompile_without_gas_draining,
    )
}

#[cfg(test)]
mod tests {
    use primitive_types::H160;

    use crate::precompiles::test_helpers::execute_precompiled;

    #[test]
    fn test_blake2f_invalid_empty() {
        let input = [0; 0];

        // act
        let result = execute_precompiled(
            H160::from_low_u64_be(9),
            &input,
            None,
            Some(25000),
            true,
        );

        // assert
        // expected outcome is Err

        assert!(result.is_err());
    }

    #[test]
    fn test_blake2f_invalid_flag() {
        let input = hex::decode(
            "0000000c48c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbab\
            d9831f79217e1319cde05b616263000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\
            0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\
            0000000000000000000000000000000000000000000000000300000000000000000000000000000002"
        ).unwrap();

        let result = execute_precompiled(
            H160::from_low_u64_be(9),
            &input,
            None,
            Some(25000),
            true,
        );

        assert!(result.is_err());
    }

    struct Blake2fTest {
        input: &'static str,
        expected: &'static str,
    }

    const BLAKE2F_TESTS: [Blake2fTest; 4] = [
        Blake2fTest {
            input: "\
            0000000048c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbab\
            d9831f79217e1319cde05b616263000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\
            0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\
            0000000000000000000000000000000000000000000000000300000000000000000000000000000001",
            expected: "\
            08c9bcf367e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d282e6ad7f520e511f6c3e2b8c68059b9442be0454267ce079\
            217e1319cde05b"
        },
        Blake2fTest {
            input: "\
            0000000c48c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbab\
            d9831f79217e1319cde05b616263000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\
            0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\
            0000000000000000000000000000000000000000000000000300000000000000000000000000000001",
            expected: "\
            ba80a53f981c4d0d6a2797b69f12f6e94c212f14685ac4b74b12bb6fdbffa2d17d87c5392aab792dc252d5de4533cc9518d38aa8dbf1925ab9\
            2386edd4009923"
        },
        Blake2fTest {
            input: "\
            0000000c48c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbab\
            d9831f79217e1319cde05b616263000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\
            0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\
            0000000000000000000000000000000000000000000000000300000000000000000000000000000000",
            expected: "\
            75ab69d3190a562c51aef8d88f1c2775876944407270c42c9844252c26d2875298743e7f6d5ea2f2d3e8d226039cd31b4e426ac4f2d3d666a6\
            10c2116fde4735"
        },
        Blake2fTest {
            input: "\
            0000000148c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbab\
            d9831f79217e1319cde05b616263000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\
            0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\
            0000000000000000000000000000000000000000000000000300000000000000000000000000000001",
            expected: "\
            b63a380cb2897d521994a85234ee2c181b5f844d2c624c002677e9703449d2fba551b3a8333bcdf5f2f7e08993d53923de3d64fcc68c034e71\
            7b9293fed7a421"
        }
    ];

    #[test]
    fn test_blake2f_input_spec() {
        for test in BLAKE2F_TESTS.iter() {
            let input = hex::decode(test.input).unwrap();
            let result = execute_precompiled(
                H160::from_low_u64_be(9),
                &input,
                None,
                Some(25000),
                true,
            );

            assert!(result.is_ok());
            let outcome = result.unwrap();
            println!("{}", outcome.gas_used);
            assert!(outcome.is_success());

            let expected = hex::decode(test.expected).unwrap();

            assert_eq!(Some(expected.as_slice()), outcome.output());
        }
    }
}
