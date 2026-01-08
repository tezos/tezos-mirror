// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::precompiles::tick_model;
use crate::{handler::EvmHandler, precompiles::PrecompileOutcome, EthereumError};
use evm::{Context, Transfer};
use evm::{ExitReason, ExitSucceed};
use libsecp256k1::{recover, Message, RecoveryId, Signature};
use sha2::Digest;
use sha3::Keccak256;
use std::cmp::min;
use tezos_evm_logging::log;
use tezos_evm_logging::Level::{Debug, Info};
use tezos_evm_runtime::runtime::Runtime;

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

fn erec_parse_inputs(input: &[u8]) -> ([u8; 32], [u8; 32], [u8; 64]) {
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

    let estimated_ticks = tick_model::ticks_of_ecrecover();
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

#[cfg(test)]
mod tests {
    use primitive_types::H160;

    use crate::precompiles::{
        ecdsa::erec_parse_inputs, test_helpers::execute_precompiled,
    };

    #[test]
    fn test_ercover_parse_input_padding() {
        let (h, v, rs) = erec_parse_inputs(&[1u8]);
        assert_eq!(1, h[0]);
        assert_eq!([0; 31], h[1..]);
        assert_eq!(0, v[31]);
        assert_eq!([0; 64], rs);
    }

    #[test]
    fn test_ercover_parse_input_order() {
        let input = [[1; 32], [2; 32], [3; 32], [3; 32]].join(&[0u8; 0][..]);
        let (h, v, rs) = erec_parse_inputs(&input);
        assert_eq!([1; 32], h);
        assert_eq!(2, v[31]);
        assert_eq!([3; 64], rs);
    }

    #[test]
    fn test_ercover_parse_input_ignore_right_padding() {
        let input = [[1; 32], [2; 32], [3; 32], [3; 32], [4; 32]].join(&[0u8; 0][..]);
        let (h, v, rs) = erec_parse_inputs(&input);
        assert_eq!([1; 32], h);
        assert_eq!(2, v[31]);
        assert_eq!([3; 64], rs);
    }

    #[test]
    fn test_ecrecover_invalid_empty() {
        // act
        let input: [u8; 0] = [0; 0];
        let result = execute_precompiled(
            H160::from_low_u64_be(1),
            &input,
            None,
            Some(25000),
            true,
        );

        // assert
        // expected outcome is OK and empty output

        assert!(result.is_ok());
        let outcome = result.unwrap();
        assert!(outcome.is_success());
        assert_eq!(Some(&[] as &[u8]), outcome.output());
    }

    #[test]
    fn test_ecrecover_invalid_zero() {
        // act
        let input: [u8; 128] = [0; 128];
        let result = execute_precompiled(
            H160::from_low_u64_be(1),
            &input,
            None,
            Some(25000),
            true,
        );

        // assert
        // expected outcome is OK but empty output

        assert!(result.is_ok());
        let outcome = result.unwrap();
        assert!(outcome.is_success());
        assert_eq!(Some(&[] as &[u8]), outcome.output());
    }

    #[test]
    fn test_ercover_parse_input_real() {
        let (hash, v, rs) = input_legacy();
        let input: [u8; 128] = assemble_input(hash, v, rs);
        let (ho, vo, rso) = erec_parse_inputs(&input);
        assert_eq!(hex::decode(hash).unwrap(), ho);
        assert_eq!(27, vo[31]);
        assert_eq!(hex::decode(rs).unwrap(), rso);
    }

    #[test]
    fn test_ercover_parse_input_spec() {
        let (hash, v, rs) = input_spec();
        let input: [u8; 128] = assemble_input(hash, v, rs);
        let (ho, vo, rso) = erec_parse_inputs(&input);
        assert_eq!(hex::decode(hash).unwrap(), ho);
        assert_eq!(28, vo[31]);
        assert_eq!(hex::decode(rs).unwrap(), rso);
    }

    fn assemble_input(h: &str, v: &str, rs: &str) -> [u8; 128] {
        let mut data_str = "".to_owned();
        data_str.push_str(h);
        data_str.push_str(v);
        data_str.push_str(rs);
        let data = hex::decode(data_str).unwrap();
        let mut input: [u8; 128] = [0; 128];
        input.copy_from_slice(&data);
        input
    }

    fn input_legacy() -> (&'static str, &'static str, &'static str) {
        // Obtain by signing a transaction tx_legacy.json (even though it doesn't need to be)
        // address: 0xf0affc80a5f69f4a9a3ee01a640873b6ba53e539
        // privateKey: 0x84e147b8bc36d99cc6b1676318a0635d8febc9f02897b0563ad27358589ee502
        // publicKey: 0x08a4681ba8c520aaab2308957d401ffded69155b358246596846f87c0728e76f618f9772f16687ed5a2854234b037b71e4c3bc92cad78e575fb12c8df8b8dae5
        // node etherlink/kernel_latest/benchmarks/scripts/sign_tx.js $(pwd)/src/kernel_latest/benchmarks/scripts/transactions_example/tx_legacy.json 0x84e147b8bc36d99cc6b1676318a0635d8febc9f02897b0563ad27358589ee502
        let hash = "3c74ed8cf6d9695ac4de8e5dda38ac3719b3f42e913e0109344a5fcbd1ff8562";
        let rs = "b17daf010e907d83f0235467faac96f346c4cc46600477d1b5f543ced8c986b770221fd3c40e0cbaef013e9bb62cf8adc70c77a5c313954c03897f3f08f90726";
        // v = 27 -> 1b, is encoded as 32 bytes
        let v = "000000000000000000000000000000000000000000000000000000000000001b";
        (hash, v, rs)
    }

    fn input_spec() -> (&'static str, &'static str, &'static str) {
        // taken from https://www.evm.codes/precompiled?fork=shanghai
        let hash = "456e9aea5e197a1f1af7a3e85a3212fa4049a3ba34c2289b4c860fc0b0c64ef3";
        let rs = "9242685bf161793cc25603c231bc2f568eb630ea16aa137d2664ac80388256084f8ae3bd7535248d0bd448298cc2e2071e56992d0774dc340c368ae950852ada";
        // v = 28 -> 1c, is encoded as 32 bytes
        let v = "000000000000000000000000000000000000000000000000000000000000001c";
        (hash, v, rs)
    }

    #[test]
    fn test_ecrecover_input_real() {
        // setup
        let (hash, v, rs) = input_legacy();
        let input: [u8; 128] = assemble_input(hash, v, rs);
        let mut expected_address: Vec<u8> =
            hex::decode("f0affc80a5f69f4a9a3ee01a640873b6ba53e539").unwrap();
        let mut expected_output = [0u8; 12].to_vec();
        expected_output.append(&mut expected_address);

        // act
        let result = execute_precompiled(
            H160::from_low_u64_be(1),
            &input,
            None,
            Some(35000),
            true,
        );

        // assert
        // expected outcome is OK and address over 32 bytes

        assert!(result.is_ok());
        let outcome = result.unwrap();
        assert!(outcome.is_success());
        assert_eq!(
            hex::encode(expected_output),
            hex::encode(outcome.output().unwrap())
        );
    }

    #[test]
    fn test_ecrecover_corrupted_data() {
        // Input was taken from the official ethereum test-suite at:
        // GeneralStateTests/stPrecompiledContracts2/CALLCODEEcrecoverV_prefixedf0.json
        let corrupted_input = hex::decode("18c547e4f7b0f325ad1e56f57e26c745b09a3e503d86e00e5255ff7f715d3d1c000000000000000000000000000000000000000000000000000000000000f01c73b1693892219d736caba55bdb67216e485557ea6b6af75f37096c9aa6a5a75feeb940b1d03b21e36b0e47e79769f095fe2ab855bd91e3a38756b7d75a9c4549").unwrap();

        let result = execute_precompiled(
            H160::from_low_u64_be(1),
            &corrupted_input,
            None,
            Some(35000),
            true,
        );

        assert!(result.is_ok());
        let outcome = result.unwrap();
        assert!(outcome.output().unwrap().is_empty());
    }

    #[test]
    fn test_ecrecover_signature_overflow() {
        // Input was taken from the official ethereum test-suite at:
        // GeneralStateTests/stPrecompiledContracts2/CallEcrecover_Overflow.json
        let input_overflow = hex::decode("18c547e4f7b0f325ad1e56f57e26c745b09a3e503d86e00e5255ff7f715d3d1c000000000000000000000000000000000000000000000000000000000000001c48b55bfa915ac795c431978d8a6a992b628d557da5ff759b307d495a36649353fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364142").unwrap();

        let result = execute_precompiled(
            H160::from_low_u64_be(1),
            &input_overflow,
            None,
            Some(35000),
            true,
        );

        assert!(result.is_ok());
        let outcome = result.unwrap();
        assert!(outcome.output().unwrap().is_empty());
    }

    #[test]
    fn test_ecrecover_input_spec() {
        let (hash, v, rs) = input_spec();
        let input: [u8; 128] = assemble_input(hash, v, rs);

        let mut expected_address: Vec<u8> =
            hex::decode("7156526fbd7a3c72969b54f64e42c10fbb768c8a").unwrap();
        let mut expected_output = [0u8; 12].to_vec();
        expected_output.append(&mut expected_address);

        // act
        let result = execute_precompiled(
            H160::from_low_u64_be(1),
            &input,
            None,
            Some(35000),
            true,
        );

        // assert
        // expected outcome is OK and address over 32 bytes

        assert!(result.is_ok());
        let outcome = result.unwrap();
        assert!(outcome.is_success());
        assert_eq!(
            hex::encode(expected_output),
            hex::encode(outcome.output().unwrap())
        );
    }
}
