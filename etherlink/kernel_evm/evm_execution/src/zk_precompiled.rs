// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 draganrakita
//
// SPDX-License-Identifier: MIT

use crate::{handler::EvmHandler, precompiles::PrecompileOutcome, EthereumError};
use alloc::vec::Vec;
use bn::{FieldError, GroupError};
use evm::{executor::stack::PrecompileFailure, ExitError, ExitReason, ExitSucceed};
use evm::{Context, Transfer};
use host::runtime::Runtime;
use primitive_types::U256;
use tezos_evm_logging::log;
use tezos_evm_logging::Level::Info;

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

pub fn ecadd_precompile<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
    _context: &Context,
    _is_static: bool,
    _transfer: Option<Transfer>,
) -> Result<PrecompileOutcome, EthereumError> {
    use bn::AffineG1;
    log!(handler.borrow_host(), Info, "Calling ecAdd precompile");
    // TODO: Make the actual tick estimation (remove the stub value).
    let estimated_ticks = 1_000_000;

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

pub fn ecmul_precompile<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
    _context: &Context,
    _is_static: bool,
    _transfer: Option<Transfer>,
) -> Result<PrecompileOutcome, EthereumError> {
    use bn::AffineG1;
    log!(handler.borrow_host(), Info, "Calling ecMul precompile");
    // TODO: Make the actual tick estimation (remove the stub value).
    let estimated_ticks = 1_000_000;

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

pub fn ecpairing_precompile<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
    _context: &Context,
    _is_static: bool,
    _transfer: Option<Transfer>,
) -> Result<PrecompileOutcome, EthereumError> {
    log!(handler.borrow_host(), Info, "Calling ecPairing precompile");
    // TODO: Make the actual tick estimation (remove the stub value).
    let estimated_ticks = 1_000_000;

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
