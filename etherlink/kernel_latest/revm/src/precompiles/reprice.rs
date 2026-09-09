// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! L2-1859 gas repricing of the `ecrecover` (0x01) and `ecPairing` (0x08)
//! builtins to reflect measured wasm wall-clock.
//!
//! This CHANGES CONSENSUS GAS and deliberately diverges from EVM equivalence
//! for these two precompiles (a documented, Frontier-style targeted divergence).
//!
//! It is confined to the gas constants: the repricing is expressed by overriding
//! the two entries of the standard revm [`Precompiles`] map, not by
//! reimplementing [`PrecompileProvider::run`], so all of upstream's `run`
//! behaviour is inherited unchanged — shared-memory calldata borrowing, and the
//! top-level (`depth == 1`) `set_precompile_error_context` that shapes the
//! consensus-visible response body on malformed input.
//!
//! It is applied unconditionally: this kernel's activation *is* the boundary at
//! which the new gas takes effect, so there is no in-kernel version gate.

use revm::precompile::{
    bn254, crypto, eth_precompile_fn, EthPrecompileOutput, EthPrecompileResult,
    Precompile, PrecompileHalt, PrecompileId, PrecompileSpecId, Precompiles,
};
use revm::primitives::{alloy_primitives::B512, Address, Bytes, OnceLock, B256};

/// ecrecover: `ECRECOVER_MULTIPLIER` × the upstream Istanbul base (3000).
const ECRECOVER_MULTIPLIER: u64 = 3;
const ECRECOVER_BASE_UPSTREAM: u64 = 3_000;
pub const ECRECOVER_GAS: u64 = ECRECOVER_MULTIPLIER * ECRECOVER_BASE_UPSTREAM;

/// ecPairing dynamic per-point coefficient: `PAIR_PER_POINT_MULTIPLIER` × the
/// upstream EIP-1108 coefficient (34000). Base is left at the Istanbul value.
const PAIR_PER_POINT_MULTIPLIER: u64 = 2;
pub const PAIR_PER_POINT: u64 =
    PAIR_PER_POINT_MULTIPLIER * bn254::pair::ISTANBUL_PAIR_PER_POINT;

pub const ECRECOVER_ADDRESS: Address = Address::with_last_byte(1);
pub const PAIRING_ADDRESS: Address = Address::with_last_byte(8);

/// Repriced `ecrecover`: identical to `revm::precompile::secp256k1::ec_recover_run`
/// except the flat gas is [`ECRECOVER_GAS`]. Returned as an [`EthPrecompileResult`]
/// and wrapped by upstream's `run`, so error/OOG handling is inherited. The
/// recovery still flows through `crypto()` (libsecp256k1 via `EtherlinkCrypto`).
fn ec_recover_run(input: &[u8], gas_limit: u64) -> EthPrecompileResult {
    if ECRECOVER_GAS > gas_limit {
        return Err(PrecompileHalt::OutOfGas);
    }
    let mut buf = [0u8; 128];
    let n = core::cmp::min(input.len(), 128);
    buf[..n].copy_from_slice(&input[..n]);

    if !(buf[32..63].iter().all(|&b| b == 0) && matches!(buf[63], 27 | 28)) {
        return Ok(EthPrecompileOutput::new(ECRECOVER_GAS, Bytes::new()));
    }
    let msg = <&B256>::try_from(&buf[0..32]).unwrap();
    let recid = buf[63] - 27;
    let sig = <&B512>::try_from(&buf[64..128]).unwrap();

    let out = crypto()
        .secp256k1_ecrecover(&sig.0, recid, &msg.0)
        .map(|o| o.to_vec().into())
        .unwrap_or_default();
    Ok(EthPrecompileOutput::new(ECRECOVER_GAS, out))
}

/// Repriced `ecPairing`: upstream `run_pair` with the dynamic per-point
/// coefficient raised to [`PAIR_PER_POINT`], base unchanged.
fn run_pair(input: &[u8], gas_limit: u64) -> EthPrecompileResult {
    bn254::run_pair(
        input,
        PAIR_PER_POINT,
        bn254::pair::ISTANBUL_PAIR_BASE,
        gas_limit,
    )
}

eth_precompile_fn!(ecrecover_precompile, ec_recover_run);
eth_precompile_fn!(pairing_precompile, run_pair);

fn build(spec: PrecompileSpecId) -> Precompiles {
    let mut precompiles = Precompiles::new(spec).clone();
    precompiles.extend([
        Precompile::new(PrecompileId::EcRec, ECRECOVER_ADDRESS, ecrecover_precompile),
        Precompile::new(
            PrecompileId::Bn254Pairing,
            PAIRING_ADDRESS,
            pairing_precompile,
        ),
    ]);
    precompiles
}

/// The standard precompiles for `spec` with `0x01`/`0x08` overridden to the
/// repriced gas, memoised as `&'static` (upstream `EthPrecompiles` holds a
/// `&'static Precompiles`). Repricing activates for a single spec, so a
/// first-spec-wins cache is sufficient; a later differing spec is a bug.
pub fn repriced_precompiles(spec: PrecompileSpecId) -> &'static Precompiles {
    static CACHE: OnceLock<(PrecompileSpecId, Precompiles)> = OnceLock::new();
    let (cached_spec, precompiles) = CACHE.get_or_init(|| (spec, build(spec)));
    debug_assert_eq!(
        *cached_spec, spec,
        "repriced_precompiles called with a second spec"
    );
    precompiles
}

#[cfg(test)]
mod tests {
    use super::*;

    // One fixed valid 192-byte k=1 pair (pairgen dump-hex 1).
    static PAIR1: [u8; 192] = [
        0x2c, 0x73, 0xfd, 0x31, 0x2a, 0x9c, 0x3b, 0x5c, 0x2a, 0xb5, 0x7c, 0x5f, 0xc1,
        0x2b, 0x4a, 0x1a, 0xd0, 0x8b, 0x24, 0x5a, 0x86, 0xec, 0xb1, 0x74, 0x4b, 0xb6,
        0x72, 0xda, 0x67, 0x6a, 0x9b, 0x23, 0x0a, 0x9f, 0x46, 0xd4, 0x38, 0x8a, 0xa8,
        0x9e, 0xc8, 0x1e, 0xf2, 0xbf, 0xc5, 0x38, 0x99, 0x6d, 0x9d, 0x2c, 0x0d, 0x85,
        0xd0, 0xed, 0x6a, 0x56, 0xe4, 0x65, 0x5b, 0x2b, 0xa0, 0x44, 0x3d, 0xe7, 0x23,
        0xbf, 0xf8, 0xed, 0xdb, 0x5b, 0x8d, 0x85, 0xf8, 0x1c, 0x27, 0xb1, 0x21, 0x1c,
        0x58, 0xbe, 0xa2, 0xfa, 0xc8, 0x06, 0x33, 0xef, 0xb6, 0xc1, 0x56, 0x5d, 0xf0,
        0x87, 0x61, 0x30, 0xc8, 0x10, 0x0e, 0x3f, 0xae, 0x2a, 0x1c, 0x5c, 0x50, 0x51,
        0x4e, 0x42, 0xdf, 0x8d, 0xc1, 0x36, 0x85, 0x59, 0x86, 0xd7, 0x9d, 0xed, 0x24,
        0x17, 0x3f, 0xe3, 0x2a, 0x4b, 0xac, 0x35, 0x44, 0x83, 0xbb, 0x3f, 0x2d, 0x35,
        0x51, 0x32, 0x00, 0xfb, 0x27, 0xca, 0x03, 0x1f, 0xd1, 0x8e, 0xbe, 0x5b, 0x0c,
        0xb0, 0xad, 0x55, 0x73, 0xb6, 0x96, 0x4a, 0xe9, 0x6a, 0x69, 0xe6, 0x6c, 0xaf,
        0x01, 0x27, 0x8e, 0x01, 0x01, 0xfa, 0x19, 0x90, 0x21, 0xdd, 0x91, 0x7c, 0x05,
        0x4c, 0xf5, 0x56, 0x89, 0x8d, 0xe2, 0xe8, 0x01, 0xc9, 0xe6, 0x6e, 0x31, 0x9f,
        0x89, 0xd3, 0xc3, 0xb0, 0x7a, 0x4a, 0x91, 0x98, 0x33, 0x6c,
    ];

    #[test]
    fn constants_are_multiples_of_upstream() {
        assert_eq!(ECRECOVER_GAS, 9_000, "ecrecover: 3x upstream 3000");
        assert_eq!(
            PAIR_PER_POINT,
            2 * bn254::pair::ISTANBUL_PAIR_PER_POINT,
            "ecPairing per-point: 2x upstream 34000"
        );
        assert_eq!(PAIR_PER_POINT, 68_000);
    }

    #[test]
    fn repriced_ecrecover_charges_9000() {
        // 128-byte input: msg | v(27) | r | s, a valid recovery.
        let mut input = [0u8; 128];
        input[0..32].copy_from_slice(&[0x42u8; 32]);
        input[63] = 27;
        input[64..128].copy_from_slice(&[
            0x24, 0x37, 0xab, 0xc6, 0x24, 0x0b, 0x3e, 0xa7, 0x9c, 0x93, 0x09, 0xcd, 0x70,
            0x61, 0x9f, 0xf0, 0x3b, 0x5c, 0xac, 0x49, 0x47, 0x57, 0x00, 0x1a, 0xb0, 0x06,
            0x41, 0x47, 0xce, 0x1a, 0x54, 0x36, 0x17, 0x69, 0x2c, 0x8b, 0x43, 0xb0, 0x78,
            0xe7, 0x25, 0xe7, 0xe1, 0xff, 0xb1, 0xa8, 0x3f, 0x46, 0x47, 0xff, 0xb1, 0x56,
            0x66, 0x97, 0xf8, 0x27, 0x60, 0xdc, 0x82, 0x70, 0x01, 0xac, 0x0b, 0xbe,
        ]);
        let out = ec_recover_run(&input, u64::MAX).expect("ok");
        assert_eq!(out.gas_used, ECRECOVER_GAS);
        assert_eq!(
            out.bytes.len(),
            32,
            "valid recovery returns an address word"
        );
    }

    #[test]
    fn repriced_pairing_charges_base_plus_new_per_point() {
        let out = run_pair(&PAIR1, u64::MAX).expect("ok");
        assert_eq!(
            out.gas_used,
            bn254::pair::ISTANBUL_PAIR_BASE + PAIR_PER_POINT,
            "k=1: 45000 + 68000"
        );
    }

    #[test]
    fn repriced_pairing_oog_below_cost() {
        let cost = bn254::pair::ISTANBUL_PAIR_BASE + PAIR_PER_POINT;
        assert!(matches!(
            run_pair(&PAIR1, cost - 1),
            Err(PrecompileHalt::OutOfGas)
        ));
    }
}
