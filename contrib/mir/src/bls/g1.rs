/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

//! Definitions for [G1], a point on the BLS12-381 curve G<sub>1</sub>.

use blst::*;
use std::{
    mem::MaybeUninit,
    ops::{Add, Mul, Neg},
};

use super::fr::Fr;

/// A point on the BLS12-381 curve G<sub>1</sub>.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct G1(blst_p1);

impl G1 {
    /// Byte size of serialized data.
    pub const BYTE_SIZE: usize = 96;

    /// Construct [G1] from raw bytes representing a point on G<sub>1</sub> in
    /// affine coordinates. If data size is not [Self::BYTE_SIZE] or the point
    /// is not on G<sub>1</sub>, the result is [None].
    pub fn from_bytes(bs: &[u8]) -> Option<Self> {
        if bs.len() != Self::BYTE_SIZE {
            None
        } else {
            // this could've been `MaybeUninit`, but `blst_p1_deserialize` may
            // not necessarily initialize it, and conversely, it may be
            // initialized even if `res` isn't `BLST_SUCCESS`. So we just
            // initialize it a priori.
            let mut affine = blst_p1_affine::default();
            let res = unsafe { blst_p1_deserialize(&mut affine, bs.as_ptr()) };
            if let BLST_ERROR::BLST_SUCCESS = res {
                Self::from_affine(affine)
            } else {
                None
            }
        }
    }

    fn from_affine(affine: blst_p1_affine) -> Option<Self> {
        let p1 = unsafe {
            let mut p1 = MaybeUninit::uninit();
            blst_p1_from_affine(p1.as_mut_ptr(), &affine);
            p1.assume_init()
        };
        if unsafe { blst_p1_in_g1(&p1) } {
            Some(G1(p1))
        } else {
            None
        }
    }

    pub(super) fn to_affine(&self) -> blst_p1_affine {
        let mut affine = MaybeUninit::uninit();
        unsafe {
            blst_p1_to_affine(affine.as_mut_ptr(), &self.0);
            affine.assume_init()
        }
    }

    /// Serialize [G1] to a byte array, representing a point on G<sub>1</sub> in
    /// affine coordinates.
    pub fn to_bytes(&self) -> [u8; Self::BYTE_SIZE] {
        let mut out = [0; Self::BYTE_SIZE];
        unsafe {
            blst_p1_serialize(out.as_mut_ptr(), &self.0);
        }
        out
    }

    /// Additive identity on G<sub>1</sub>.
    pub fn zero() -> Self {
        G1::from_bytes(&[
            0x40, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        ])
        .unwrap()
    }

    /// Multiplicative identity on G<sub>1</sub>.
    pub fn one() -> Self {
        Self::from_affine(unsafe { blst::BLS12_381_G1 }).unwrap()
    }

    /// Additive inverse of [`G1::one()`], such that
    ///
    /// ```
    /// # use mir::bls::g1::G1;
    /// # assert!(
    /// G1::one() + G1::neg_one() == G1::zero()
    /// # );
    /// ```
    pub fn neg_one() -> Self {
        Self::from_affine(unsafe { blst::BLS12_381_NEG_G1 }).unwrap()
    }
}

impl Add for &G1 {
    type Output = G1;
    fn add(self, rhs: Self) -> Self::Output {
        let res = unsafe {
            let mut res = MaybeUninit::uninit();
            blst_p1_add_or_double(res.as_mut_ptr(), &self.0, &rhs.0);
            res.assume_init()
        };
        G1(res)
    }
}

super::instances::instances!(Add, add, G1, G1);

impl Mul<&Fr> for &G1 {
    type Output = G1;
    fn mul(self, rhs: &Fr) -> Self::Output {
        let bytes = rhs.to_bytes();
        let res = unsafe {
            let mut res = MaybeUninit::uninit();
            blst_p1_mult(res.as_mut_ptr(), &self.0, bytes.as_ptr(), Fr::BYTE_SIZE * 8);
            res.assume_init()
        };
        G1(res)
    }
}

super::instances::instances!(Mul, mul, G1, Fr);

impl Neg for &G1 {
    type Output = G1;
    fn neg(self) -> Self::Output {
        self.clone().neg()
    }
}

impl Neg for G1 {
    type Output = G1;
    fn neg(mut self) -> Self::Output {
        unsafe { blst_p1_cneg(&mut self.0, true) };
        self
    }
}

#[cfg(test)]
pub(super) mod tests {
    use std::str::FromStr;

    use num_bigint::BigInt;

    use super::*;

    fn two() -> G1 {
        let s = "0572cbea904d67468808c8eb50a9450c9721db309128012543902d0ac358a62ae28f75bb8f1c7c42c39a8c5529bf0f4e166a9d8cabc673a322fda673779d8e3822ba3ecb8670e461f73bb9021d5fd76a4c56d9d4cd16bd1bba86881979749d28";
        G1::from_bytes(&hex::decode(s).unwrap()).unwrap()
    }

    pub fn some_val() -> G1 {
        let s = "026fcea34d1a4c5125142dfa3b616086309cab49e60e548d95de658af4d9329c269dc132bd5d884617e8767600daeee90c6f5d25f3d63540f3b799d291e5df4a90244346ed780d5c9d3afa8f3c9a196e089fa4edc4a9806592e8561d626579e3";
        G1::from_bytes(&hex::decode(s).unwrap()).unwrap()
    }

    fn some_val_plus_one() -> G1 {
        let s = "0c267c85b0895eab7a63d9d8c0aa9744f1d9eccdc95b8a38c32fe54b49ee71f901372d2e4f157a1298a7cfa907edbc3c1797fdc61d20c9428567c864bf1572d3b26cd98478111e8bd097ac929d9a80189b51e06d69c0f2185f1fed6bd9f18d84";
        G1::from_bytes(&hex::decode(s).unwrap()).unwrap()
    }

    pub fn neg_some_val() -> G1 {
        let s = "026fcea34d1a4c5125142dfa3b616086309cab49e60e548d95de658af4d9329c269dc132bd5d884617e8767600daeee90d91b4c445a9b15957640de3b165cd8cd453083e060d0562c9f5d811ba16dcb6160c5b10ecaa7f9a2716a9e29d9a30c8";
        G1::from_bytes(&hex::decode(s).unwrap()).unwrap()
    }

    #[test]
    fn from_to_bytes() {
        // roundtrip tests from-to bytes
        assert_eq!(G1::from_bytes(&G1::zero().to_bytes()).unwrap(), G1::zero());
        assert_eq!(G1::from_bytes(&G1::one().to_bytes()).unwrap(), G1::one());
        assert_eq!(
            G1::from_bytes(&G1::neg_one().to_bytes()).unwrap(),
            G1::neg_one()
        );
        assert_eq!(G1::from_bytes(&some_val().to_bytes()).unwrap(), some_val());
    }

    #[test]
    fn add() {
        assert_eq!(G1::zero() + G1::zero(), G1::zero());
        assert_eq!(G1::zero() + G1::one(), G1::one());
        assert_eq!(G1::one() + G1::zero(), G1::one());
        assert_eq!(G1::one() + G1::neg_one(), G1::zero());
        assert_eq!(G1::one() + G1::one(), two());
        assert_eq!(G1::zero() + some_val(), some_val());
        assert_eq!(some_val() + G1::zero(), some_val());
        assert_eq!(G1::one() + some_val(), some_val_plus_one());
        assert_eq!(some_val() + G1::one(), some_val_plus_one());
    }

    #[test]
    fn mul() {
        let fr_zero = &Fr::from_big_int(&0.into());
        let fr_one = &Fr::from_big_int(&1.into());
        let fr_big = &Fr::from_big_int(&BigInt::from_str(&"42".repeat(72)).unwrap());

        let g1_big = "15cd0704c8a681ffe8a47662dba6c04d1b0a560f133a60c6a81c0eb804d5453b14992bc3d1f01081d3ed3cf1362994b217e00dbcfdb7ec7cb455ed8381aa469f4d555b50fed7f5579fe4559cd5caf6fc9a07da522c7db5973168d1c74946dc51";
        let some_x_big = "0e76570914f9a1f625f3c97e389b5339f18b73fc22a291b5f88f2194fb665219ba96610fb573090527552a00ba78322a1393fdf1144cf64b35003b0ea4db6ce94b9bdac689634c94f29863836646a5f78285390c5e4101b83970546fd12d5d6e";

        assert_eq!(G1::zero() * fr_one, G1::zero());
        assert_eq!(G1::zero() * fr_zero, G1::zero());
        assert_eq!(G1::zero() * fr_big, G1::zero());
        assert_eq!(G1::one() * fr_zero, G1::zero());
        assert_eq!(G1::one() * fr_one, G1::one());
        assert_eq!(
            G1::one() * fr_big,
            G1::from_bytes(&hex::decode(g1_big).unwrap()).unwrap()
        );
        assert_eq!(some_val() * fr_zero, G1::zero());
        assert_eq!(some_val() * fr_one, some_val());
        assert_eq!(
            some_val() * fr_big,
            G1::from_bytes(&hex::decode(some_x_big).unwrap()).unwrap()
        );
    }

    #[test]
    fn neg() {
        assert_eq!(-G1::zero(), G1::zero());
        assert_eq!(-G1::one(), G1::neg_one());
        assert_eq!(-G1::neg_one(), G1::one());
        assert_eq!(-some_val(), neg_some_val());
        assert_eq!(-neg_some_val(), some_val());
    }
}
