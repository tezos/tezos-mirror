// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! Definitions for [Fr], an element of the BLS12-381 scalar field F<sub>r</sub>

use std::mem::MaybeUninit;

use blst::*;
use num_bigint::{BigInt, Sign};
use num_traits::One;
use std::ops::{Add, Mul, Neg};

/// An element of the BLS12-381 scalar field F<sub>r</sub>
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Fr(blst_fr);

impl Fr {
    /// Size of serialized data in bytes.
    pub const BYTE_SIZE: usize = 32;

    /// Construct [Fr] from a scalar represented as raw bytes. If the scalar
    /// isn't part of the field, or if its length exceeds [Self::BYTE_SIZE], the
    /// result is [None].
    pub fn from_bytes(bs: &[u8]) -> Option<Self> {
        if bs.len() > Self::BYTE_SIZE {
            return None;
        }
        let mut buf: [u8; Self::BYTE_SIZE] = [0; Self::BYTE_SIZE];
        buf[0..bs.len()].copy_from_slice(bs);
        let mbfr = unsafe {
            let mut scalar = MaybeUninit::uninit();
            blst_scalar_from_lendian(scalar.as_mut_ptr(), buf.as_ptr());
            let scalar = scalar.assume_init();
            if blst_scalar_fr_check(&scalar) {
                let mut fr = MaybeUninit::uninit();
                blst_fr_from_scalar(fr.as_mut_ptr(), &scalar);
                Some(fr.assume_init())
            } else {
                None
            }
        };
        mbfr.map(Self)
    }

    /// Construct a scalar represented as raw bytes from [Fr].
    pub fn to_bytes(&self) -> [u8; Self::BYTE_SIZE] {
        let mut out = [0; Self::BYTE_SIZE];
        let mut scalar = MaybeUninit::uninit();
        unsafe {
            blst_scalar_from_fr(scalar.as_mut_ptr(), &self.0);
            blst_lendian_from_scalar(out.as_mut_ptr(), &scalar.assume_init());
        }
        out
    }

    fn order() -> &'static BigInt {
        // 52435875175126190479447740508185965837690552500527637822603658699938581184513,
        // the group order
        static MEM: std::sync::OnceLock<BigInt> = std::sync::OnceLock::new();
        MEM.get_or_init(|| {
            BigInt::from_slice(
                Sign::Plus,
                &[
                    1, 4294967295, 4294859774, 1404937218, 161601541, 859428872, 698187080,
                    1944954707,
                ],
            )
        })
    }

    /// Construct [Fr] from a [BigInt] scalar. The scalar is taken modulo group
    /// order.
    pub fn from_big_int(i: &BigInt) -> Self {
        // this would be likely slightly more efficient with rem_euclid, but
        // it's only added in num-bigint 0.4, and we're stuck with 0.3 for now
        // because of the Kernel SDK
        let norm = i.modpow(&BigInt::one(), Self::order());
        let mut buf = [0u8; Self::BYTE_SIZE];
        let bytes_le = norm.to_biguint().unwrap().to_bytes_le();
        buf[0..bytes_le.len()].copy_from_slice(&bytes_le);
        Self::from_bytes(&buf).unwrap()
    }

    /// Construct a [BigInt] scalar from [Fr].
    pub fn to_big_int(&self) -> BigInt {
        BigInt::from_bytes_le(Sign::Plus, &self.to_bytes())
    }

    /// Additive identity in the F<sub>r</sub> field.
    pub fn zero() -> Self {
        Self::from_bytes(&[]).unwrap()
    }

    /// Multiplicative identity in the F<sub>r</sub> field.
    pub fn one() -> Self {
        Self::from_bytes(&[1]).unwrap()
    }
}

impl Add for &Fr {
    type Output = Fr;
    fn add(self, rhs: Self) -> Self::Output {
        let res = unsafe {
            let mut res = MaybeUninit::uninit();
            blst_fr_add(res.as_mut_ptr(), &self.0, &rhs.0);
            res.assume_init()
        };
        Fr(res)
    }
}

super::instances::instances!(Add, add, Fr, Fr);

impl Mul for &Fr {
    type Output = Fr;
    fn mul(self, rhs: Self) -> Self::Output {
        let res = unsafe {
            let mut res = MaybeUninit::uninit();
            blst_fr_mul(res.as_mut_ptr(), &self.0, &rhs.0);
            res.assume_init()
        };
        Fr(res)
    }
}

super::instances::instances!(Mul, mul, Fr, Fr);

impl Neg for &Fr {
    type Output = Fr;
    fn neg(self) -> Self::Output {
        let mut res = blst_fr::default();
        unsafe { blst_fr_cneg(&mut res, &self.0, true) };
        Fr(res)
    }
}

impl Neg for Fr {
    type Output = Fr;
    fn neg(self) -> Self::Output {
        (&self).neg()
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;

    #[test]
    fn order() {
        assert_eq!(
            Fr::order(),
            &BigInt::from_str(
                "52435875175126190479447740508185965837690552500527637822603658699938581184513"
            )
            .unwrap()
        );
    }

    #[test]
    fn fr_from_to_bytes() {
        // roundtrip tests from-to bytes
        assert_eq!(&Fr::from_bytes(&[]).unwrap().to_bytes(), &[0; 32]);
        assert_eq!(
            &Fr::from_bytes(&[1]).unwrap().to_bytes(),
            [&[1u8] as &[u8], &[0; 31]].concat().as_slice()
        );
        let hex = &hex::decode("6582983f01c028b8959d31d2d5e537dd7ca38eaf8490c269727b2e5bd3ea961a")
            .unwrap();
        assert_eq!(&Fr::from_bytes(hex).unwrap().to_bytes(), hex.as_slice());
    }

    #[test]
    fn fr_from_big_int() {
        // roundtrip tests from int to bytes
        assert_eq!(&Fr::from_big_int(&0.into()).to_bytes(), &[0; 32]);
        assert_eq!(
            &Fr::from_big_int(&1.into()).to_bytes(),
            [&[1u8] as &[u8], &[0; 31]].concat().as_slice()
        );
        let int = BigInt::from_str(&"42".repeat(72)).unwrap();
        assert_eq!(
            &Fr::from_big_int(&int).to_bytes(),
            hex::decode("992529bdf61eb9e4a480fff193adb5e4a78ab7820d308f5582c015c2ded0103e")
                .unwrap()
                .as_slice()
        );
        let neg_int = -BigInt::from_str(&"42".repeat(72)).unwrap();
        assert_eq!(
            &Fr::from_big_int(&neg_int).to_bytes(),
            hex::decode("68dad64208e1461b5adbfe0d6ff6076f5d4dea86faa7aaddc5bc876774d6dc35")
                .unwrap()
                .as_slice()
        );
        assert_eq!(
            &Fr::from_big_int(&(-1).into()),
            &Fr::from_bytes(
                &hex::decode("00000000fffffffffe5bfeff02a4bd5305d8a10908d83933487d9d2953a7ed73")
                    .unwrap()
            )
            .unwrap(),
        )
    }

    #[test]
    fn fr_to_big_int() {
        // roundtrip tests from bytes to int
        assert_eq!(Fr::from_bytes(&[0; 32]).unwrap().to_big_int(), 0.into());
        assert_eq!(Fr::from_bytes(&[1]).unwrap().to_big_int(), 1.into());
        let int = BigInt::from_str(&"42".repeat(72))
            .unwrap()
            .modpow(&1.into(), Fr::order());
        assert_eq!(
            Fr::from_bytes(
                &hex::decode("992529bdf61eb9e4a480fff193adb5e4a78ab7820d308f5582c015c2ded0103e")
                    .unwrap()
            )
            .unwrap()
            .to_big_int(),
            int
        );
        let neg_int = -BigInt::from_str(&"42".repeat(72)).unwrap();
        let neg_int_mod = neg_int.modpow(&BigInt::one(), Fr::order());
        assert_eq!(
            Fr::from_bytes(
                &hex::decode("68dad64208e1461b5adbfe0d6ff6076f5d4dea86faa7aaddc5bc876774d6dc35")
                    .unwrap()
            )
            .unwrap()
            .to_big_int(),
            neg_int_mod,
        );
        assert_eq!(
            Fr::from_bytes(
                &hex::decode("00000000fffffffffe5bfeff02a4bd5305d8a10908d83933487d9d2953a7ed73")
                    .unwrap()
            )
            .unwrap()
            .to_big_int(),
            Fr::order() - BigInt::from(1),
        )
    }

    #[test]
    fn fr_add() {
        let zero = &Fr::from_big_int(&0.into());
        let one = &Fr::from_big_int(&1.into());
        let two = &Fr::from_big_int(&2.into());
        let big = &Fr::from_big_int(&BigInt::from_str(&"42".repeat(72)).unwrap());
        let big1 = &Fr::from_bytes(
            &hex::decode("9a2529bdf61eb9e4a480fff193adb5e4a78ab7820d308f5582c015c2ded0103e")
                .unwrap(),
        )
        .unwrap();
        let big2 = &Fr::from_bytes(
            &hex::decode("314b527aee3d72c94aa500e424b7ad754a3dcdfb1288e477bc038e5a6afa3308")
                .unwrap(),
        )
        .unwrap();

        assert_eq!(zero + one, *one);
        assert_eq!(one + zero, *one);
        assert_eq!(zero + zero, *zero);
        assert_eq!(one + one, *two);
        assert_eq!(big + zero, *big);
        assert_eq!(zero + big, *big);
        assert_eq!(big + one, *big1);
        assert_eq!(one + big, *big1);
        assert_eq!(big + big, *big2);
    }

    #[test]
    fn fr_mul() {
        let zero = &Fr::from_big_int(&0.into());
        let one = &Fr::from_big_int(&1.into());
        let big = &Fr::from_big_int(&BigInt::from_str(&"42".repeat(72)).unwrap());
        let big2 = &Fr::from_bytes(
            &hex::decode("cb6a743dd190f86475268c95979c93e8b6b84b3bf10754822c8eecc4d9ae281c")
                .unwrap(),
        )
        .unwrap();

        assert_eq!(zero * one, *zero);
        assert_eq!(one * zero, *zero);
        assert_eq!(zero * zero, *zero);
        assert_eq!(one * one, *one);
        assert_eq!(big * zero, *zero);
        assert_eq!(zero * big, *zero);
        assert_eq!(big * one, *big);
        assert_eq!(one * big, *big);
        assert_eq!(big * big, *big2);
    }

    #[test]
    fn fr_neg() {
        let zero = &Fr::from_big_int(&0.into());
        let one = &Fr::from_big_int(&1.into());
        let neg_one = &Fr::from_big_int(&(-1).into());
        let big = &Fr::from_big_int(&BigInt::from_str(&"42".repeat(72)).unwrap());
        let neg_big = &Fr::from_bytes(
            &hex::decode("68dad64208e1461b5adbfe0d6ff6076f5d4dea86faa7aaddc5bc876774d6dc35")
                .unwrap(),
        )
        .unwrap();

        assert_eq!(-zero, *zero);
        assert_eq!(-one, *neg_one);
        assert_eq!(-neg_one, *one);
        assert_eq!(-big, *neg_big);
        assert_eq!(-neg_big, *big);
    }
}
