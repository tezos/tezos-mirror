/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

//! Definitions for [G2], a point on the BLS12-381 curve G<sub>2</sub>.

use blst::*;
use std::{
    mem::MaybeUninit,
    ops::{Add, Mul, Neg},
};

use super::fr::Fr;

/// A point on the BLS12-381 curve G<sub>2</sub>.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct G2(blst_p2);

impl G2 {
    /// Byte size of serialized data.
    pub const BYTE_SIZE: usize = 192;

    /// Construct [G2] from raw bytes representing a point on G<sub>2</sub> in
    /// affine coordinates. If data size is not [Self::BYTE_SIZE] or the point
    /// is not on G<sub>2</sub>, the result is [None].
    pub fn from_bytes(bs: &[u8]) -> Option<Self> {
        if bs.len() != Self::BYTE_SIZE {
            None
        } else {
            // this could've been `MaybeUninit`, but `blst_p2_deserialize` may
            // not necessarily initialize it, and conversely, it may be
            // initialized even if `res` isn't `BLST_SUCCESS`. So we just
            // initialize it a priori.
            let mut affine = blst_p2_affine::default();
            let res = unsafe { blst_p2_deserialize(&mut affine, bs.as_ptr()) };
            if let BLST_ERROR::BLST_SUCCESS = res {
                Self::from_affine(affine)
            } else {
                None
            }
        }
    }

    fn from_affine(affine: blst_p2_affine) -> Option<Self> {
        let p2 = unsafe {
            let mut p2 = MaybeUninit::uninit();
            blst_p2_from_affine(p2.as_mut_ptr(), &affine);
            p2.assume_init()
        };
        if unsafe { blst_p2_in_g2(&p2) } {
            Some(G2(p2))
        } else {
            None
        }
    }

    pub(super) fn to_affine(&self) -> blst_p2_affine {
        let mut affine = MaybeUninit::uninit();
        unsafe {
            blst_p2_to_affine(affine.as_mut_ptr(), &self.0);
            affine.assume_init()
        }
    }

    /// Serialize [G2] to a byte array, representing a point on G<sub>2</sub> in
    /// affine coordinates.
    pub fn to_bytes(&self) -> [u8; Self::BYTE_SIZE] {
        let mut out = [0; Self::BYTE_SIZE];
        unsafe {
            blst_p2_serialize(out.as_mut_ptr(), &self.0);
        }
        out
    }

    /// Additive identity on G<sub>2</sub>.
    pub fn zero() -> Self {
        G2::from_bytes(&[
            0x40, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        ])
        .unwrap()
    }

    /// Multiplicative identity on G<sub>2</sub>.
    pub fn one() -> Self {
        Self::from_affine(unsafe { blst::BLS12_381_G2 }).unwrap()
    }

    /// Additive inverse of [`G2::one()`], such that
    ///
    /// ```
    /// # use mir::bls::g2::G2;
    /// # assert!(
    /// G2::one() + G2::neg_one() == G2::zero()
    /// # );
    /// ```
    pub fn neg_one() -> Self {
        Self::from_affine(unsafe { blst::BLS12_381_NEG_G2 }).unwrap()
    }
}

impl Add for &G2 {
    type Output = G2;
    fn add(self, rhs: Self) -> Self::Output {
        let res = unsafe {
            let mut res = MaybeUninit::uninit();
            blst_p2_add_or_double(res.as_mut_ptr(), &self.0, &rhs.0);
            res.assume_init()
        };
        G2(res)
    }
}

super::instances::instances!(Add, add, G2, G2);

impl Mul<&Fr> for &G2 {
    type Output = G2;
    fn mul(self, rhs: &Fr) -> Self::Output {
        let bytes = rhs.to_bytes();
        let res = unsafe {
            let mut res = MaybeUninit::uninit();
            blst_p2_mult(res.as_mut_ptr(), &self.0, bytes.as_ptr(), Fr::BYTE_SIZE * 8);
            res.assume_init()
        };
        G2(res)
    }
}

super::instances::instances!(Mul, mul, G2, Fr);

impl Neg for &G2 {
    type Output = G2;
    fn neg(self) -> Self::Output {
        self.clone().neg()
    }
}

impl Neg for G2 {
    type Output = G2;
    fn neg(mut self) -> Self::Output {
        unsafe { blst_p2_cneg(&mut self.0, true) };
        self
    }
}

#[cfg(test)]
pub(super) mod tests {
    use std::str::FromStr;

    use num_bigint::BigInt;

    use super::*;

    fn two() -> G2 {
        let s = "0a4edef9c1ed7f729f520e47730a124fd70662a904ba1074728114d1031e1572c6c886f6b57ec72a6178288c47c335771638533957d540a9d2370f17cc7ed5863bc0b995b8825e0ee1ea1e1e4d00dbae81f14b0bf3611b78c952aacab827a0530f6d4552fa65dd2638b361543f887136a43253d9c66c411697003f7a13c308f5422e1aa0a59c8967acdefd8b6e36ccf30468fb440d82b0630aeb8dca2b5256789a66da69bf91009cbfe6bd221e47aa8ae88dece9764bf3bd999d95d71e4c9899";
        G2::from_bytes(&hex::decode(s).unwrap()).unwrap()
    }

    pub fn some_val() -> G2 {
        let s = "14e9b22683a66543ec447b7aa76e4404424709728507581d0b3f60a8062c3f7c7d3365197c59f7c961fa9731084f5be60d0a936e93d556bdef2032cdcae2fa9902dcbe105e01d7ab7126d83486d882c4efd2fc1ac55044157333be19acf0cb7a10bc41c8081c9babd8d5b41b645badd4a679b3d4e1b3ea2c0e1f53b39c00b3889a40306c9b9ee2da5831e90148334d91016474d07e0f4e36d2d51b5ca11b633b9a940b9c126aebf4a2537c18fdc6967fb677824bfa902157e53cb499a021e57b";
        G2::from_bytes(&hex::decode(s).unwrap()).unwrap()
    }

    fn some_val_plus_one() -> G2 {
        let s = "013df716b3c482bddcc4bfe70b03d2b5e0d23d2334fbf4ed4e255e503ef5f33b957e32d14255aef3a52e123ec8e30c4f08b326c9c77bf265440900ee3d4564e445cdeeb2e264f0ef4f5d99fd6e2b0579089fd6dfad7c7c9ebb85080b5ef34b7f15e3dd2d528184f1e2eb1a00ec561db3d9493b75fe5c2016e012767758d8e0c06635282fbf89b7e8ae0348db49f9de4919782a0ccce2e05b3207c59a5e91fb813c1cf29cf6893ec2df1633ea1d9d8b956bbe43fb381c8cdd80368003750589e5";
        G2::from_bytes(&hex::decode(s).unwrap()).unwrap()
    }

    pub fn neg_some_val() -> G2 {
        let s = "14e9b22683a66543ec447b7aa76e4404424709728507581d0b3f60a8062c3f7c7d3365197c59f7c961fa9731084f5be60d0a936e93d556bdef2032cdcae2fa9902dcbe105e01d7ab7126d83486d882c4efd2fc1ac55044157333be19acf0cb7a0944d02231634aee7245f39adeefff02bdfd97b011d1289359117eed5ab0429b846bcf9215b51d2561cd16feb7cc5d1a189c9d19bb70986378468c59a230499bc9e33fe8e11a26cac4dd5687f8ea5fa468347db2b6c3dea7d4c24b665fddc530";
        G2::from_bytes(&hex::decode(s).unwrap()).unwrap()
    }

    #[test]
    fn from_to_bytes() {
        // roundtrip tests from-to bytes
        assert_eq!(G2::from_bytes(&G2::zero().to_bytes()).unwrap(), G2::zero());
        assert_eq!(G2::from_bytes(&G2::one().to_bytes()).unwrap(), G2::one());
        assert_eq!(
            G2::from_bytes(&G2::neg_one().to_bytes()).unwrap(),
            G2::neg_one()
        );
        assert_eq!(G2::from_bytes(&some_val().to_bytes()).unwrap(), some_val());
    }

    #[test]
    fn add() {
        assert_eq!(G2::zero() + G2::zero(), G2::zero());
        assert_eq!(G2::zero() + G2::one(), G2::one());
        assert_eq!(G2::one() + G2::zero(), G2::one());
        assert_eq!(G2::one() + G2::neg_one(), G2::zero());
        assert_eq!(G2::one() + G2::one(), two());
        assert_eq!(G2::zero() + some_val(), some_val());
        assert_eq!(some_val() + G2::zero(), some_val());
        assert_eq!(G2::one() + some_val(), some_val_plus_one());
        assert_eq!(some_val() + G2::one(), some_val_plus_one());
    }

    #[test]
    fn mul() {
        let fr_zero = &Fr::from_big_int(&0.into());
        let fr_one = &Fr::from_big_int(&1.into());
        let fr_big = &Fr::from_big_int(&BigInt::from_str(&"42".repeat(72)).unwrap());

        let g2_big = "0b5bbe13765b651ac1181215aa1a0969066aba3d39901ca10db654062b4076c356de42cefd2984da91682ef163bf5ef30b1654b0e20cbca44d770acd316e3f5acf3a72af4bad5c7f049f6b1941c5e8436f4262a533d3a093c87837b0ceef17ef114485907e511668365e43b4c109ab001405cc50db4b73c22fb348222a1b42c1bb448662500436ec1c9c479f2b68702507aa21abf497dd885b657f99b61a5ffb06326102cac8feb88d0c0ea512eb03ecdd4f53fb42d481c51564244e664904ad";
        let some_x_big = "045e5ecb13336a379270d71fad62ed5456a2a304217b8f0cbf1d7227d61171608164f5f417b80882188e7e76c71ff26e05d226933e9ecfa709234599fa68826f13ee15fa2df01d0829a3bceb4c69a66595bb100f87866296396e5db0a1b80d5b0ed456304f5737d6e58a23eca930d9b96584e6be8106892799a03c061fecfec49861556f95d7ef41545bef191acded56154967175cf4071d281991ce5af32e501c5a0e979dc7c0713be506dd0c3f45dba7668eb8dc42ad26a87deb6300dc0199";

        assert_eq!(G2::zero() * fr_one, G2::zero());
        assert_eq!(G2::zero() * fr_zero, G2::zero());
        assert_eq!(G2::zero() * fr_big, G2::zero());
        assert_eq!(G2::one() * fr_zero, G2::zero());
        assert_eq!(G2::one() * fr_one, G2::one());
        assert_eq!(
            G2::one() * fr_big,
            G2::from_bytes(&hex::decode(g2_big).unwrap()).unwrap()
        );
        assert_eq!(some_val() * fr_zero, G2::zero());
        assert_eq!(some_val() * fr_one, some_val());
        assert_eq!(
            some_val() * fr_big,
            G2::from_bytes(&hex::decode(some_x_big).unwrap()).unwrap()
        );
    }

    #[test]
    fn neg() {
        assert_eq!(-G2::zero(), G2::zero());
        assert_eq!(-G2::one(), G2::neg_one());
        assert_eq!(-G2::neg_one(), G2::one());
        assert_eq!(-some_val(), neg_some_val());
        assert_eq!(-neg_some_val(), some_val());
    }
}
