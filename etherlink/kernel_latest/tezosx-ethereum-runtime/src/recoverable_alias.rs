// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! EVM alias derivation of RFC-26051 (recoverable EVM aliases).
//!
//! The alias of a tz1/tz2/tz3 account is the address a rescue account for
//! that key would get on any EVM chain:
//!
//! ```text
//! initcode(impl, pkh) = PREFIX ‖ RUNTIME
//!   PREFIX  = 0x73 ‖ GUARD ‖ 0x3b15601e57600080fd5b604180602a6000396000f3
//!   RUNTIME = 0x363d3d373d3d3d363d73 ‖ impl ‖ 0x5af43d82803e903d91602b57fd5bf3 ‖ pkh
//! alias = CREATE2(SAFE_FACTORY, SALT, keccak256(initcode(IMPL_<type>, pkh)))
//! ```
//!
//! so funds sent to the alias on another EVM chain can be recovered with
//! the Tezos key. Any other address keeps the legacy derivation.

use alloy_primitives::{keccak256, Address, Keccak256};
use revm_etherlink::precompiles::constants::{
    RESCUE_GUARD_ADDRESS, RESCUE_IMPL_TZ1, RESCUE_IMPL_TZ2, RESCUE_IMPL_TZ3,
    RESCUE_SAFE_FACTORY, RESCUE_SALT,
};
use tezos_crypto_rs::public_key_hash::PublicKeyHash;

/// `PUSH20 GUARD; EXTCODESIZE; ISZERO; PUSH1 0x1e; JUMPI; ...`: reverts when
/// the guard has code, otherwise returns the runtime code that follows.
const PREFIX_HEAD: [u8; 1] = [0x73];
const PREFIX_TAIL: [u8; 21] = [
    0x3b, 0x15, 0x60, 0x1e, 0x57, 0x60, 0x00, 0x80, 0xfd, 0x5b, 0x60, 0x41, 0x80, 0x60,
    0x2a, 0x60, 0x00, 0x39, 0x60, 0x00, 0xf3,
];
/// EIP-1167 minimal proxy, around the implementation address.
const PROXY_HEAD: [u8; 10] = [0x36, 0x3d, 0x3d, 0x37, 0x3d, 0x3d, 0x3d, 0x36, 0x3d, 0x73];
const PROXY_TAIL: [u8; 15] = [
    0x5a, 0xf4, 0x3d, 0x82, 0x80, 0x3e, 0x90, 0x3d, 0x91, 0x60, 0x2b, 0x57, 0xfd, 0x5b,
    0xf3,
];

/// Hash of the rescue-account initcode of `pkh` behind `implementation`.
fn initcode_hash(implementation: &Address, pkh: &[u8; 20]) -> [u8; 32] {
    let mut hasher = Keccak256::new();
    hasher.update(PREFIX_HEAD);
    hasher.update(RESCUE_GUARD_ADDRESS);
    hasher.update(PREFIX_TAIL);
    hasher.update(PROXY_HEAD);
    hasher.update(implementation);
    hasher.update(PROXY_TAIL);
    hasher.update(pkh);
    hasher.finalize().0
}

/// The recoverable alias of `native_address`, the UTF-8 b58check of a
/// Tezos address, or `None` when no rescue account covers it (originated
/// contracts, tz4, or anything that is not an implicit account).
pub(crate) fn recoverable_alias(native_address: &[u8]) -> Option<Address> {
    let native_address = core::str::from_utf8(native_address).ok()?;
    let (implementation, pkh): (Address, [u8; 20]) =
        match PublicKeyHash::from_b58check(native_address).ok()? {
            PublicKeyHash::Ed25519(pkh) => (RESCUE_IMPL_TZ1, *pkh),
            PublicKeyHash::Secp256k1(pkh) => (RESCUE_IMPL_TZ2, *pkh),
            PublicKeyHash::P256(pkh) => (RESCUE_IMPL_TZ3, *pkh),
            // The Michelson runtime does not support tz4 accounts yet, and no
            // rescue account exists for them.
            PublicKeyHash::Bls(_) => return None,
        };
    Some(RESCUE_SAFE_FACTORY.create2(RESCUE_SALT, initcode_hash(&implementation, &pkh)))
}

/// Pre-RFC-26051 derivation: the first 20 bytes of the Keccak-256 of the
/// native address bytes.
pub(crate) fn legacy_alias(native_address: &[u8]) -> Address {
    Address::from_slice(&keccak256(native_address)[0..20])
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Alias vectors of `artifacts/frozen.json` in
    /// trilitech/tz-alias-recovery (commit b42336e).
    const VECTORS: [(&str, &str); 14] = [
        (
            "tz1aygVpocoeEMU89vXk92sMHqPYFNZVgvrh",
            "0x0ea7865238580e0d8f0d2ca196d3b672dea705ad",
        ),
        (
            "tz1WDRuEornRTSHFTgnwC5LPRQThxbZqofiF",
            "0x1975f732fea2a37de36e40017b907d6562ffce7d",
        ),
        (
            "tz1cgbBXC99Tkv26pxXDcFExd5agYtJKp6Ko",
            "0xcd9737deee8444f5c277bee986586de00b5b5d2b",
        ),
        (
            "tz1cEFKy9mT7yfTZX9yAGn4ZiFC77wxf5WC3",
            "0xf45fb97fc5d9785f1075bf86e050c608b601faaf",
        ),
        (
            "tz2BCeQSi5ETyKJsob61pWCoQvoGtsrJBEt2",
            "0x29dd12971b4b7f5741334ab0cd72fe9706e2e2cd",
        ),
        (
            "tz2DQNNCg1zbmNEMbYFK9sn1JakSkdkGy525",
            "0x3915cb0424fa0a7f08cf4f78aa9a6910177272b1",
        ),
        (
            "tz2RteZE6H6r4YSX7PwHDiEodx1j365BS6Zw",
            "0x78cd291942fdd47a55cea942adbdc35782f1a254",
        ),
        (
            "tz2Cv1duW3gb9j6yV4u3CzqkbrH2XYQuacu3",
            "0x3fdbb367414d2426d2549fa4f5429623d3c19c52",
        ),
        (
            "tz3bqAfFRnSA6dfPRG8XR6MBMmo6HZTTG44V",
            "0xc75658fcee30564ffda5e61c48cdebd47eb0633e",
        ),
        (
            "tz3UddKLx4bhUxev77uHWuy9UqDk1VHeDpfQ",
            "0x3ea387c764b0d48b8416e19a5d68d0219b9c13dd",
        ),
        (
            "tz3bxdqRaqmfQ8V8aNPtJC3sg29GKMtAxSo4",
            "0xbc46edf18e7c5d62ae88c8796a490c491cccdda2",
        ),
        (
            "tz3N5sNfWMwWLiUC65vH43rpHKDNYLGyrEnc",
            "0xee9440608815f399d8fd197cd5ead96dbcef288b",
        ),
        (
            "tz1aAdmy1MiGhnRkw8RWS6xbUpTuSRh3ZB4y",
            "0xcdb4d002582717fa10060665ec598ac68590072a",
        ),
        (
            "tz2K1exM7pLKyUtVTKqtDo6zFwv98trFxqBc",
            "0x40f2c21437e12669dbf98d5035b21c6141c7bb69",
        ),
    ];

    #[test]
    fn recoverable_alias_matches_frozen_vectors() {
        for (native, expected) in VECTORS {
            let alias = recoverable_alias(native.as_bytes())
                .unwrap_or_else(|| panic!("{native} has no recoverable alias"));
            assert_eq!(format!("{alias:#x}"), expected, "alias of {native}");
        }
    }

    #[test]
    fn uncovered_addresses_have_no_recoverable_alias() {
        for native in [
            "KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton",
            "tz4FENGt5zkiGaHPm1ya4MgLomgkL1k7Dy7q",
            "0x1234567890abcdef1234567890abcdef12345678",
            "tz1abc",
        ] {
            assert_eq!(recoverable_alias(native.as_bytes()), None, "{native}");
        }
    }
}
