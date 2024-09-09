// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 draganrakita
//
// SPDX-License-Identifier: MIT

use crate::precompiles::call_precompile_with_gas_draining;
use crate::{handler::EvmHandler, precompiles::PrecompileOutcome, EthereumError};
use alloc::vec::Vec;
use bn::{FieldError, GroupError};
use evm::{executor::stack::PrecompileFailure, ExitError, ExitReason, ExitSucceed};
use evm::{Context, Transfer};
use primitive_types::U256;
use tezos_evm_logging::log;
use tezos_evm_logging::Level::Debug;
use tezos_evm_runtime::runtime::Runtime;

/// Input length for the add operation.
const ADD_INPUT_LEN: usize = 128;

/// Input length for the multiplication operation.
const MUL_INPUT_LEN: usize = 128;

/// Pair element length.
const PAIR_ELEMENT_LEN: usize = 192;

fn bn128_field_point_not_a_member_error(_: FieldError) -> EthereumError {
    EthereumError::PrecompileFailed(PrecompileFailure::Error {
        exit_status: ExitError::Other(std::borrow::Cow::Borrowed(
            "Bn128FieldPointNotAMember",
        )),
    })
}

fn bn128_affine_g_failed_to_create_error(_: GroupError) -> EthereumError {
    EthereumError::PrecompileFailed(PrecompileFailure::Error {
        exit_status: ExitError::Other(std::borrow::Cow::Borrowed(
            "Bn128AffineGFailedToCreate",
        )),
    })
}

/// Reads the `x` and `y` points from an input at a given position.
fn read_point(input: &[u8], pos: usize) -> Result<bn::G1, EthereumError> {
    use bn::{AffineG1, Fq, Group, G1};

    let mut px_buf = [0u8; 32];
    px_buf.copy_from_slice(&input[pos..(pos + 32)]);
    let px = Fq::from_slice(&px_buf).map_err(bn128_field_point_not_a_member_error)?;

    let mut py_buf = [0u8; 32];
    py_buf.copy_from_slice(&input[(pos + 32)..(pos + 64)]);
    let py = Fq::from_slice(&py_buf).map_err(bn128_field_point_not_a_member_error)?;

    if px == Fq::zero() && py == bn::Fq::zero() {
        Ok(G1::zero())
    } else {
        AffineG1::new(px, py)
            .map(Into::into)
            .map_err(bn128_affine_g_failed_to_create_error)
    }
}

fn ecadd_precompile_without_gas_draining<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
) -> Result<PrecompileOutcome, EthereumError> {
    use bn::AffineG1;
    log!(handler.borrow_host(), Debug, "Calling ecAdd precompile");
    let estimated_ticks = 1_700_000;

    if let Err(record_err) = handler.record_cost(150) {
        return Ok(PrecompileOutcome {
            exit_status: ExitReason::Error(record_err),
            output: vec![],
            withdrawals: vec![],
            estimated_ticks,
        });
    }

    let mut input = input.to_vec();
    input.resize(ADD_INPUT_LEN, 0);

    let p1 = read_point(&input, 0)?;
    let p2 = read_point(&input, 64)?;

    let mut output = [0u8; 64];
    if let Some(sum) = AffineG1::from_jacobian(p1 + p2) {
        sum.x()
            .into_u256()
            .to_big_endian(&mut output[..32])
            .unwrap();
        sum.y()
            .into_u256()
            .to_big_endian(&mut output[32..])
            .unwrap();
    }

    Ok(PrecompileOutcome {
        exit_status: ExitReason::Succeed(ExitSucceed::Returned),
        output: output.to_vec(),
        withdrawals: vec![],
        estimated_ticks,
    })
}

pub fn ecadd_precompile<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
    _context: &Context,
    _is_static: bool,
    _transfer: Option<Transfer>,
) -> Result<PrecompileOutcome, EthereumError> {
    call_precompile_with_gas_draining(
        handler,
        input,
        ecadd_precompile_without_gas_draining,
    )
}

fn ecmul_precompile_without_gas_draining<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
) -> Result<PrecompileOutcome, EthereumError> {
    use bn::AffineG1;
    log!(handler.borrow_host(), Debug, "Calling ecMul precompile");
    let estimated_ticks = 100_000_000;

    if let Err(record_err) = handler.record_cost(6_000) {
        return Ok(PrecompileOutcome {
            exit_status: ExitReason::Error(record_err),
            output: vec![],
            withdrawals: vec![],
            estimated_ticks,
        });
    }

    let mut input = input.to_vec();
    input.resize(MUL_INPUT_LEN, 0);

    let p = read_point(&input, 0)?;

    let mut fr_buf = [0u8; 32];
    fr_buf.copy_from_slice(&input[64..96]);
    // Fr::from_slice can only fail on incorrect length, and this is not a case.
    let fr = bn::Fr::from_slice(&fr_buf[..]).unwrap();

    let mut output = [0u8; 64];
    if let Some(mul) = AffineG1::from_jacobian(p * fr) {
        mul.x().to_big_endian(&mut output[..32]).unwrap();
        mul.y().to_big_endian(&mut output[32..]).unwrap();
    }

    Ok(PrecompileOutcome {
        exit_status: ExitReason::Succeed(ExitSucceed::Returned),
        output: output.to_vec(),
        withdrawals: vec![],
        estimated_ticks,
    })
}

pub fn ecmul_precompile<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
    _context: &Context,
    _is_static: bool,
    _transfer: Option<Transfer>,
) -> Result<PrecompileOutcome, EthereumError> {
    call_precompile_with_gas_draining(
        handler,
        input,
        ecmul_precompile_without_gas_draining,
    )
}

fn ecpairing_precompile_without_gas_draining<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
) -> Result<PrecompileOutcome, EthereumError> {
    log!(handler.borrow_host(), Debug, "Calling ecPairing precompile");

    let mut estimated_ticks = 70_000;

    if input.len() % PAIR_ELEMENT_LEN == 0 {
        estimated_ticks += (input.len() / PAIR_ELEMENT_LEN) as u64 * 1_200_000_000
    }

    let gas_cost = (input.len() / PAIR_ELEMENT_LEN) as u64 * 34_000 /* ISTANBUL_PAIR_PER_POINT */ + 45_000 /* ISTANBUL_PAIR_BASE */;

    if let Err(record_err) = handler.record_cost(gas_cost) {
        return Ok(PrecompileOutcome {
            exit_status: ExitReason::Error(record_err),
            output: vec![],
            withdrawals: vec![],
            estimated_ticks,
        });
    }

    use bn::{AffineG1, AffineG2, Fq, Fq2, Group, Gt, G1, G2};

    if input.len() % PAIR_ELEMENT_LEN != 0 {
        return Err(EthereumError::PrecompileFailed(PrecompileFailure::Error {
            exit_status: ExitError::Other(std::borrow::Cow::Borrowed("Bn128PairLength")),
        }));
    }

    let output = if input.is_empty() {
        U256::one()
    } else {
        let elements = input.len() / PAIR_ELEMENT_LEN;
        let mut vals = Vec::with_capacity(elements);

        const PEL: usize = PAIR_ELEMENT_LEN;

        for idx in 0..elements {
            let mut buf = [0u8; 32];

            buf.copy_from_slice(&input[(idx * PEL)..(idx * PEL + 32)]);
            let ax =
                Fq::from_slice(&buf).map_err(bn128_field_point_not_a_member_error)?;
            buf.copy_from_slice(&input[(idx * PEL + 32)..(idx * PEL + 64)]);
            let ay =
                Fq::from_slice(&buf).map_err(bn128_field_point_not_a_member_error)?;
            buf.copy_from_slice(&input[(idx * PEL + 64)..(idx * PEL + 96)]);
            let bay =
                Fq::from_slice(&buf).map_err(bn128_field_point_not_a_member_error)?;
            buf.copy_from_slice(&input[(idx * PEL + 96)..(idx * PEL + 128)]);
            let bax =
                Fq::from_slice(&buf).map_err(bn128_field_point_not_a_member_error)?;
            buf.copy_from_slice(&input[(idx * PEL + 128)..(idx * PEL + 160)]);
            let bby =
                Fq::from_slice(&buf).map_err(bn128_field_point_not_a_member_error)?;
            buf.copy_from_slice(&input[(idx * PEL + 160)..(idx * PEL + 192)]);
            let bbx =
                Fq::from_slice(&buf).map_err(bn128_field_point_not_a_member_error)?;

            let a = {
                if ax.is_zero() && ay.is_zero() {
                    G1::zero()
                } else {
                    G1::from(
                        AffineG1::new(ax, ay)
                            .map_err(bn128_affine_g_failed_to_create_error)?,
                    )
                }
            };
            let b = {
                let ba = Fq2::new(bax, bay);
                let bb = Fq2::new(bbx, bby);

                if ba.is_zero() && bb.is_zero() {
                    G2::zero()
                } else {
                    G2::from(
                        AffineG2::new(ba, bb)
                            .map_err(bn128_affine_g_failed_to_create_error)?,
                    )
                }
            };
            vals.push((a, b))
        }

        let mul = vals
            .into_iter()
            .fold(Gt::one(), |s, (a, b)| s * bn::pairing(a, b));

        if mul == Gt::one() {
            U256::one()
        } else {
            U256::zero()
        }
    };

    let mut final_output: [u8; 32] = [0; 32];
    U256::to_big_endian(&output, &mut final_output);

    Ok(PrecompileOutcome {
        exit_status: ExitReason::Succeed(ExitSucceed::Returned),
        output: final_output.to_vec(),
        withdrawals: vec![],
        estimated_ticks,
    })
}

pub fn ecpairing_precompile<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
    _context: &Context,
    _is_static: bool,
    _transfer: Option<Transfer>,
) -> Result<PrecompileOutcome, EthereumError> {
    call_precompile_with_gas_draining(
        handler,
        input,
        ecpairing_precompile_without_gas_draining,
    )
}

#[cfg(test)]
mod tests {
    use crate::precompiles::test_helpers::execute_precompiled;
    use primitive_types::H160;

    #[test]
    fn test_ecadd_precompile() {
        let input = hex::decode(
            "\
             18b18acfb4c2c30276db5411368e7185b311dd124691610c5d3b74034e093dc9\
             063c909c4720840cb5134cb9f59fa749755796819658d32efc0d288198f37266\
             07c2b7f58a84bd6145f00c9c2bc0bb1a187f20ff2c92963a88019e7c6a014eed\
             06614e20c147e940f2d70da3f74c9a17df361706a4485c742bd6788478fa17d7",
        )
        .unwrap();
        let expected = hex::decode(
            "\
            2243525c5efd4b9c3d3c45ac0ca3fe4dd85e830a4ce6b65fa1eeaee202839703\
            301d1d33be6da8e509df21cc35964723180eed7532537db9ae5e7d48f195c915",
        )
        .unwrap();

        let result = execute_precompiled(
            H160::from_low_u64_be(6),
            &input,
            None,
            Some(50_000),
            false,
        );
        assert!(result.is_ok());
        let outcome = result.unwrap();
        assert!(outcome.is_success());
        assert_eq!(*outcome.output().unwrap(), expected);

        // zero sum test
        let input = hex::decode(
            "\
            0000000000000000000000000000000000000000000000000000000000000000\
            0000000000000000000000000000000000000000000000000000000000000000\
            0000000000000000000000000000000000000000000000000000000000000000\
            0000000000000000000000000000000000000000000000000000000000000000",
        )
        .unwrap();
        let expected = hex::decode(
            "\
            0000000000000000000000000000000000000000000000000000000000000000\
            0000000000000000000000000000000000000000000000000000000000000000",
        )
        .unwrap();

        let result = execute_precompiled(
            H160::from_low_u64_be(6),
            &input,
            None,
            Some(50_000),
            false,
        );
        assert!(result.is_ok());
        let outcome = result.unwrap();
        assert!(outcome.is_success());
        assert_eq!(*outcome.output().unwrap(), expected);

        // no input test
        let input = [0u8; 0];
        let expected = hex::decode(
            "\
            0000000000000000000000000000000000000000000000000000000000000000\
            0000000000000000000000000000000000000000000000000000000000000000",
        )
        .unwrap();

        let result = execute_precompiled(
            H160::from_low_u64_be(6),
            &input,
            None,
            Some(50_000),
            false,
        );
        assert!(result.is_ok());
        let outcome = result.unwrap();
        assert!(outcome.is_success());
        assert_eq!(*outcome.output().unwrap(), expected);

        // point not on curve fail
        let input = hex::decode(
            "\
            1111111111111111111111111111111111111111111111111111111111111111\
            1111111111111111111111111111111111111111111111111111111111111111\
            1111111111111111111111111111111111111111111111111111111111111111\
            1111111111111111111111111111111111111111111111111111111111111111",
        )
        .unwrap();

        let result = execute_precompiled(
            H160::from_low_u64_be(6),
            &input,
            None,
            Some(50_000),
            false,
        );
        // ERR_BN128_INVALID_POINT
        assert!(result.is_err());
    }

    #[test]
    fn test_ecmul_precompile() {
        let input = hex::decode(
            "\
            2bd3e6d0f3b142924f5ca7b49ce5b9d54c4703d7ae5648e61d02268b1a0a9fb7\
            21611ce0a6af85915e2f1d70300909ce2e49dfad4a4619c8390cae66cefdb204\
            00000000000000000000000000000000000000000000000011138ce750fa15c2",
        )
        .unwrap();
        let expected = hex::decode(
            "\
            070a8d6a982153cae4be29d434e8faef8a47b274a053f5a4ee2a6c9c13c31e5c\
            031b8ce914eba3a9ffb989f9cdd5b0f01943074bf4f0f315690ec3cec6981afc",
        )
        .unwrap();

        let result = execute_precompiled(
            H160::from_low_u64_be(7),
            &input,
            None,
            Some(40_000),
            false,
        );
        assert!(result.is_ok());
        let outcome = result.unwrap();
        assert!(outcome.is_success());
        assert_eq!(*outcome.output().unwrap(), expected);

        // zero multiplication test
        let input = hex::decode(
            "\
            0000000000000000000000000000000000000000000000000000000000000000\
            0000000000000000000000000000000000000000000000000000000000000000\
            0200000000000000000000000000000000000000000000000000000000000000",
        )
        .unwrap();
        let expected = hex::decode(
            "\
            0000000000000000000000000000000000000000000000000000000000000000\
            0000000000000000000000000000000000000000000000000000000000000000",
        )
        .unwrap();

        let result = execute_precompiled(
            H160::from_low_u64_be(7),
            &input,
            None,
            Some(40_000),
            false,
        );
        assert!(result.is_ok());
        let outcome = result.unwrap();
        assert!(outcome.is_success());
        assert_eq!(*outcome.output().unwrap(), expected);

        // no input test
        let input = [0u8; 0];
        let expected = hex::decode(
            "\
            0000000000000000000000000000000000000000000000000000000000000000\
            0000000000000000000000000000000000000000000000000000000000000000",
        )
        .unwrap();

        let result = execute_precompiled(
            H160::from_low_u64_be(7),
            &input,
            None,
            Some(40_000),
            false,
        );
        assert!(result.is_ok());
        let outcome = result.unwrap();
        assert!(outcome.is_success());
        assert_eq!(*outcome.output().unwrap(), expected);

        // point not on curve fail
        let input = hex::decode(
            "\
            1111111111111111111111111111111111111111111111111111111111111111\
            1111111111111111111111111111111111111111111111111111111111111111\
            0f00000000000000000000000000000000000000000000000000000000000000",
        )
        .unwrap();

        let result = execute_precompiled(
            H160::from_low_u64_be(7),
            &input,
            None,
            Some(40_000),
            false,
        );
        // ERR_BN128_INVALID_POINT
        assert!(result.is_err());
    }

    #[test]
    fn test_ecpairing_precompile() {
        let input = hex::decode(
            "\
            1c76476f4def4bb94541d57ebba1193381ffa7aa76ada664dd31c16024c43f59\
            3034dd2920f673e204fee2811c678745fc819b55d3e9d294e45c9b03a76aef41\
            209dd15ebff5d46c4bd888e51a93cf99a7329636c63514396b4a452003a35bf7\
            04bf11ca01483bfa8b34b43561848d28905960114c8ac04049af4b6315a41678\
            2bb8324af6cfc93537a2ad1a445cfd0ca2a71acd7ac41fadbf933c2a51be344d\
            120a2a4cf30c1bf9845f20c6fe39e07ea2cce61f0c9bb048165fe5e4de877550\
            111e129f1cf1097710d41c4ac70fcdfa5ba2023c6ff1cbeac322de49d1b6df7c\
            2032c61a830e3c17286de9462bf242fca2883585b93870a73853face6a6bf411\
            198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c2\
            1800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed\
            090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b\
            12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa",
        )
        .unwrap();
        let expected = hex::decode(
            "0000000000000000000000000000000000000000000000000000000000000001",
        )
        .unwrap();

        let result = execute_precompiled(
            H160::from_low_u64_be(8),
            &input,
            None,
            Some(260_000),
            false,
        );
        println!("result {:?}", result);
        assert!(result.is_ok());
        let outcome = result.unwrap();
        assert!(outcome.is_success());
        assert_eq!(*outcome.output().unwrap(), expected);

        // no input test
        let input = [0u8; 0];
        let expected = hex::decode(
            "0000000000000000000000000000000000000000000000000000000000000001",
        )
        .unwrap();

        let result = execute_precompiled(
            H160::from_low_u64_be(8),
            &input,
            None,
            Some(260_000),
            false,
        );
        assert!(result.is_ok());
        let outcome = result.unwrap();
        assert!(outcome.is_success());
        assert_eq!(*outcome.output().unwrap(), expected);

        // point not on curve fail
        let input = hex::decode(
            "\
            1111111111111111111111111111111111111111111111111111111111111111\
            1111111111111111111111111111111111111111111111111111111111111111\
            1111111111111111111111111111111111111111111111111111111111111111\
            1111111111111111111111111111111111111111111111111111111111111111\
            1111111111111111111111111111111111111111111111111111111111111111\
            1111111111111111111111111111111111111111111111111111111111111111",
        )
        .unwrap();

        let result = execute_precompiled(
            H160::from_low_u64_be(8),
            &input,
            None,
            Some(260_000),
            false,
        );
        // ERR_BN128_INVALID_A
        assert!(result.is_err());

        // invalid input length
        let input = hex::decode(
            "\
            1111111111111111111111111111111111111111111111111111111111111111\
            1111111111111111111111111111111111111111111111111111111111111111\
            111111111111111111111111111111\
        ",
        )
        .unwrap();

        let result = execute_precompiled(
            H160::from_low_u64_be(8),
            &input,
            None,
            Some(260_000),
            false,
        );
        // ERR_BN128_INVALID_LEN
        assert!(result.is_err());
    }
}
