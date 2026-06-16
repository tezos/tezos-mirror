// SPDX-CopyrightText: 2026 TriliTech <contact@trili.tech>
// SPDX-CopyrightText: 2026 Functori <contact@functori.com>
// SPDX-License-Identifier: MIT

use crate::enc::BinWriter;
use crate::nom::NomReader;
use crate::types::Zarith;

use num_bigint::BigInt;
use num_bigint::BigUint;
use num_bigint::Sign;
use proptest::prelude::prop_assert;
use proptest::prelude::prop_assert_eq;
use proptest::prelude::proptest;
use proptest::prelude::Strategy;
use proptest::prelude::TestCaseError;

/// Encode then decode: must be identity.
/// 2^55 fits in 7 big-endian bytes (d never reaches 7).
/// 2^58 needs 8 big-endian bytes, so d reaches 7 once.
/// 2^184 needs 24 big-endian bytes, so d reaches 7 three times.
/// Before the fix, the round-trip failed for 2^58 and 2^184
/// because the encoder silently dropped input bytes when d == 7.
#[test]
fn n_bignum_roundtrip_large() {
    let check_roundtrip = |n| {
        let mut encoded = Vec::new();
        crate::enc::n_bignum(n, &mut encoded).unwrap();

        let (rest, decoded) = crate::nom::n_bignum(&encoded).unwrap();

        assert!(rest.is_empty());
        assert_eq!(&decoded, n, "round-trip failed for {n}");
    };

    let values: Vec<BigUint> = vec![
        BigUint::from(1u8) << 5,   // 1 BE bytes
        BigUint::from(1u8) << 55,  // 7 BE bytes, d never reaches 7
        BigUint::from(1u8) << 58,  // 8 BE bytes, d=7 once
        BigUint::from(1u8) << 184, // 24 BE bytes, d=7 three times
    ];

    for n in &values {
        check_roundtrip(n)
    }
}

#[test]
fn n_bignum_roundtrip() {
    fn check_roundtrip(n: &BigUint) -> Result<(), TestCaseError> {
        let mut encoded = Vec::new();
        crate::enc::n_bignum(n, &mut encoded).unwrap();

        let (rest, decoded) = crate::nom::n_bignum(&encoded).unwrap();

        prop_assert!(rest.is_empty());
        prop_assert_eq!(&decoded, n, "round-trip failed for {}", n);
        Ok(())
    }

    proptest!(|(digits: Vec<u32>)| {
        let n = BigUint::new(digits);
        check_roundtrip(&n)?;
    });
}

/// Encode then decode: must be identity for signed Zarith as well.
#[test]
fn zarith_roundtrip_large() {
    let check_roundtrip = |zarith: Zarith| {
        let mut encoded = Vec::new();
        zarith.bin_write(&mut encoded).unwrap();

        let (rest, decoded) = Zarith::nom_read(&encoded).unwrap();
        assert!(rest.is_empty());
        assert_eq!(decoded, zarith, "round-trip failed for {}", zarith.0);
    };

    let values = [
        BigInt::from(1) << 5,
        BigInt::from(1) << 55,
        BigInt::from(1) << 58,
        BigInt::from(1) << 184,
        -(BigInt::from(1) << 55u8),
        -(BigInt::from(1) << 58u8),
        -(BigInt::from(1) << 184u8),
    ];

    for zarith in values.into_iter().map(Zarith) {
        check_roundtrip(zarith)
    }
}

#[test]
fn zarith_roundtrip() {
    fn check_roundtrip(zarith: &Zarith) -> Result<(), TestCaseError> {
        let mut encoded = Vec::new();
        zarith.bin_write(&mut encoded).unwrap();

        let (rest, decoded) = Zarith::nom_read(&encoded).unwrap();
        prop_assert!(rest.is_empty());
        prop_assert_eq!(&decoded, zarith, "round-trip failed for {}", &zarith.0);

        Ok(())
    }

    fn sign_strategy() -> impl Strategy<Value = Sign> {
        proptest::sample::select(vec![Sign::Minus, Sign::NoSign, Sign::Plus])
    }

    proptest!(|(sign in sign_strategy(), digits: Vec<u32>)| {
        let zarith = Zarith(BigInt::new(sign, digits));
        check_roundtrip(&zarith)?;
    });
}
