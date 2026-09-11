// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Etherlink crypto provider installed into revm's global [`Crypto`] seam.
//!
//! This overrides only the *implementation* of crypto primitives, never their
//! gas. The precompile entry points (`ec_recover_run`, `run_pair`, …) keep
//! their upstream gas constants; they merely call `crypto().<primitive>()`,
//! which this provider services. Installing it therefore cannot change the
//! gas schedule — only how fast the same result is computed.
//!
//! `secp256k1_ecrecover` is routed to `libsecp256k1` instead of the default
//! `k256` path for two reasons (see L2-1859):
//!
//!   * `k256`/`ecdsa` recompute a full ECDSA verification *after* recovering
//!     the key (`ecdsa::recovery::recover_from_prehash` calls `verify_prehash`
//!     on the recovered key), which is mathematically redundant — the recovery
//!     equation already pins `(r, s)` to the recovered point. `libsecp256k1`'s
//!     `recover` performs a single `ecmult` with no re-verification.
//!   * `libsecp256k1` uses a 16 384-entry precomputed generator table and a
//!     field representation (10×26-bit limbs) tuned for 32-bit words, whereas
//!     `revm-precompile` pulls `k256` without `precomputed-tables` (it is in
//!     k256's defaults but not in the `ecdsa` feature it enables), so k256's
//!     `mul_by_generator` is a generic scalar multiplication.
//!
//! Equivalence with the k256 path is exact (same acceptance set, same output):
//!   * `r`/`s` ≥ n: rejected on both sides (`parse_standard` overflow check /
//!     k256 `from_slice`).
//!   * `r` = 0 or `s` = 0: rejected on both sides (`recover_raw` zero check /
//!     k256 `from_scalars`).
//!   * high `s` (s > n/2): accepted on both sides. k256 normalises to `n − s`
//!     and flips the recovery id; `libsecp256k1` recovers from the raw `s` and
//!     the original id. These yield the same key because the recovery point at
//!     the flipped parity is `−R` and `n − s ≡ −s (mod n)`, so
//!     `(n − s)(−R) = sR`.
//!   * `v` is restricted to 27/28 by `ec_recover_run` before this is reached,
//!     so `recid ∈ {0, 1}` and the `recid & 2` branch is never taken.
//!   * `R` off-curve or recovered point at infinity: rejected on both sides.

use revm::precompile::{Crypto, PrecompileHalt};
use revm::primitives::{keccak256, OnceLock};

/// Crypto provider backing Etherlink's precompiles. Every method not overridden
/// here inherits revm's default implementation, so BN254, modexp, blake2, etc.
/// are unchanged.
#[derive(Debug, Clone, Copy, Default)]
pub struct EtherlinkCrypto;

impl Crypto for EtherlinkCrypto {
    fn secp256k1_ecrecover(
        &self,
        sig: &[u8; 64],
        recid: u8,
        msg: &[u8; 32],
    ) -> Result<[u8; 32], PrecompileHalt> {
        let signature = libsecp256k1::Signature::parse_standard(sig)
            .map_err(|_| PrecompileHalt::Secp256k1RecoverFailed)?;
        let recovery_id = libsecp256k1::RecoveryId::parse(recid)
            .map_err(|_| PrecompileHalt::Secp256k1RecoverFailed)?;
        let message = libsecp256k1::Message::parse(msg);

        let public_key = libsecp256k1::recover(&message, &signature, &recovery_id)
            .map_err(|_| PrecompileHalt::Secp256k1RecoverFailed)?;

        // `serialize()` is the 65-byte uncompressed form `0x04 || X || Y`; the
        // Ethereum address is the low 20 bytes of keccak256 over `X || Y`.
        let serialized = public_key.serialize();
        let mut output = keccak256(&serialized[1..]).0;
        output[..12].fill(0); // 12 most significant bytes zeroized
        Ok(output)
    }
}

/// Ensure [`EtherlinkCrypto`] is installed as the process-wide revm crypto
/// provider, returning whether it is the active provider.
///
/// `revm::install_crypto` is a `OnceLock::set`: only the first `crypto()` user
/// in the process wins, so we cache our first attempt's outcome and later calls
/// (per tx / per reboot) are cheap no-ops. `kernel` calls this before any
/// precompile runs, so in production we always win; the returned bool lets it
/// warn in the shouldn't-happen case where another `crypto()` user had already
/// pinned the default backend (ecrecover then falls back to k256 — a slowdown,
/// not a wrong result, since every provider computes the same value).
pub fn ensure_etherlink_crypto_installed() -> bool {
    static INSTALLED: OnceLock<bool> = OnceLock::new();
    *INSTALLED.get_or_init(|| revm::install_crypto(EtherlinkCrypto))
}

#[cfg(test)]
mod tests {
    use super::*;
    use revm::precompile::Crypto;
    use revm::primitives::{alloy_primitives::B512, B256};

    // Deterministic valid signature over digest 0x42..42 (siggen), recid 0.
    const SIG64: [u8; 64] = [
        0x24, 0x37, 0xab, 0xc6, 0x24, 0x0b, 0x3e, 0xa7, 0x9c, 0x93, 0x09, 0xcd, 0x70,
        0x61, 0x9f, 0xf0, 0x3b, 0x5c, 0xac, 0x49, 0x47, 0x57, 0x00, 0x1a, 0xb0, 0x06,
        0x41, 0x47, 0xce, 0x1a, 0x54, 0x36, 0x17, 0x69, 0x2c, 0x8b, 0x43, 0xb0, 0x78,
        0xe7, 0x25, 0xe7, 0xe1, 0xff, 0xb1, 0xa8, 0x3f, 0x46, 0x47, 0xff, 0xb1, 0x56,
        0x66, 0x97, 0xf8, 0x27, 0x60, 0xdc, 0x82, 0x70, 0x01, 0xac, 0x0b, 0xbe,
    ];
    const MSG: [u8; 32] = [0x42u8; 32];
    // secp256k1 group order n.
    const N: [u8; 32] = [
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xfe, 0xba, 0xae, 0xdc, 0xe6, 0xaf, 0x48, 0xa0, 0x3b, 0xbf, 0xd2,
        0x5e, 0x8c, 0xd0, 0x36, 0x41, 0x41,
    ];

    // The upstream k256 path revm ships with, used as the oracle.
    fn k256_ref(sig: &[u8; 64], recid: u8, msg: &[u8; 32]) -> Option<[u8; 32]> {
        revm::precompile::secp256k1::k256::ecrecover(
            &B512::from_slice(sig),
            recid,
            &B256::from_slice(msg),
        )
        .ok()
        .map(|b| b.0)
    }

    fn etherlink(sig: &[u8; 64], recid: u8, msg: &[u8; 32]) -> Option<[u8; 32]> {
        EtherlinkCrypto.secp256k1_ecrecover(sig, recid, msg).ok()
    }

    // n - s (big-endian), to build the high-s twin of a low-s signature.
    fn neg_scalar(s: &[u8; 32]) -> [u8; 32] {
        let mut out = [0u8; 32];
        let mut borrow = 0i16;
        for i in (0..32).rev() {
            let d = N[i] as i16 - s[i] as i16 - borrow;
            out[i] = (d.rem_euclid(256)) as u8;
            borrow = i16::from(d < 0);
        }
        out
    }

    /// The gas-neutrality of W1+W2 rests on byte-identical output vs k256.
    /// This pins that invariant that otherwise lived only in a doc comment.
    #[test]
    fn ecrecover_matches_k256() {
        // valid low-s signature
        assert_eq!(etherlink(&SIG64, 0, &MSG), k256_ref(&SIG64, 0, &MSG));
        assert!(etherlink(&SIG64, 0, &MSG).is_some());

        // high-s twin: s' = n - s, recid flipped -> same recovered address
        let s: [u8; 32] = SIG64[32..64].try_into().unwrap();
        let mut hi = SIG64;
        hi[32..64].copy_from_slice(&neg_scalar(&s));
        assert_eq!(etherlink(&hi, 1, &MSG), k256_ref(&hi, 1, &MSG));
        assert_eq!(etherlink(&hi, 1, &MSG), etherlink(&SIG64, 0, &MSG));

        // degenerate: r = 0, s = 0, r = n (overflow) -> both reject
        let mut r0 = SIG64;
        r0[0..32].fill(0);
        assert_eq!(etherlink(&r0, 0, &MSG), k256_ref(&r0, 0, &MSG));
        assert!(etherlink(&r0, 0, &MSG).is_none());

        let mut s0 = SIG64;
        s0[32..64].fill(0);
        assert_eq!(etherlink(&s0, 0, &MSG), k256_ref(&s0, 0, &MSG));
        assert!(etherlink(&s0, 0, &MSG).is_none());

        let mut rn = SIG64;
        rn[0..32].copy_from_slice(&N);
        assert_eq!(etherlink(&rn, 0, &MSG), k256_ref(&rn, 0, &MSG));
        assert!(etherlink(&rn, 0, &MSG).is_none());
    }

    proptest::proptest! {
        /// Over arbitrary raw inputs (recovery id restricted to the 27/28 the
        /// precompile accepts), the libsecp256k1 and k256 paths must return the
        /// same result — `Some(addr)` or `None` — every time.
        #[test]
        fn ecrecover_agrees_with_k256_on_random_input(
            sig in proptest::prelude::any::<[u8; 64]>(),
            recid in 0u8..2,
            msg in proptest::prelude::any::<[u8; 32]>(),
        ) {
            proptest::prop_assert_eq!(
                etherlink(&sig, recid, &msg),
                k256_ref(&sig, recid, &msg)
            );
        }

        /// Over freshly signed messages, both implementations recover the same
        /// non-empty address.
        #[test]
        fn ecrecover_agrees_with_k256_on_valid_signatures(
            sk_bytes in proptest::prelude::any::<[u8; 32]>(),
            msg in proptest::prelude::any::<[u8; 32]>(),
        ) {
            proptest::prop_assume!(libsecp256k1::SecretKey::parse(&sk_bytes).is_ok());
            let sk = libsecp256k1::SecretKey::parse(&sk_bytes).unwrap();
            let message = libsecp256k1::Message::parse(&msg);
            let (sig, recid) = libsecp256k1::sign(&message, &sk);
            let sig64 = sig.serialize();
            let recid_b = recid.serialize();
            proptest::prop_assert_eq!(
                etherlink(&sig64, recid_b, &msg),
                k256_ref(&sig64, recid_b, &msg)
            );
            proptest::prop_assert!(etherlink(&sig64, recid_b, &msg).is_some());
        }
    }
}
