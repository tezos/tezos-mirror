/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use std::borrow::Borrow;

use blst::*;

use super::{G1, G2};

const BLST_FP12_ZERO: blst_fp12 = blst_fp12 {
    fp6: [blst_fp6 {
        fp2: [blst_fp2 {
            fp: [blst_fp { l: [0; 6] }; 2],
        }; 3],
    }; 2],
};

/// Verify that the product of pairings of the given list of points is equal to
/// 1 in the field Fq12. Returns `true` if the list is empty. The signature may
/// seem overly generic, but we want to accept both owned and borrowed
/// items, as there's no easy way to convert an owning iterator to iterator
/// over references.
pub fn pairing_check<TG1, TG2, Item>(pairs: impl IntoIterator<Item = Item>) -> bool
where
    TG1: Borrow<G1>,
    TG2: Borrow<G2>,
    Item: Borrow<(TG1, TG2)>,
{
    let mut x = blst_fp12::default(); // defaults to blst_fp12_one

    for i in pairs {
        let tmp_p1 = i.borrow().0.borrow().to_affine();
        let tmp_p2 = i.borrow().1.borrow().to_affine();
        let tmp = blst_fp12::miller_loop(&tmp_p2, &tmp_p1);
        unsafe {
            blst_fp12_mul(&mut x, &x, &tmp);
        }
    }

    if x == BLST_FP12_ZERO {
        false
    } else {
        let fin = x.final_exp();
        unsafe { blst_fp12_is_one(&fin) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_list() {
        assert!(pairing_check::<&_, &_, _>(&[]))
    }

    #[test]
    fn one_elt() {
        assert!(pairing_check([(G1::zero(), G2::zero())]));
        assert!(pairing_check([(G1::zero(), G2::one())]));
        assert!(pairing_check([(G1::one(), G2::zero())]));
        assert!(!pairing_check([(G1::one(), G2::one())]));
    }

    #[test]
    fn two_elts() {
        use super::super::{g1, g2};

        assert!(pairing_check([
            (G1::one(), G2::one()),
            (G1::one(), G2::neg_one())
        ]));
        assert!(pairing_check([
            (G1::neg_one(), G2::one()),
            (G1::one(), G2::one())
        ]));

        assert!(pairing_check([
            (g1::tests::some_val(), g2::tests::some_val()),
            (g1::tests::some_val(), g2::tests::neg_some_val()),
        ]));

        assert!(pairing_check([
            (g1::tests::some_val(), g2::tests::some_val()),
            (g1::tests::neg_some_val(), g2::tests::some_val()),
        ]));

        // failing checks
        assert!(!pairing_check([
            (G1::one(), G2::one()),
            (G1::neg_one(), G2::neg_one())
        ]));
        assert!(!pairing_check([
            (G1::neg_one(), G2::neg_one()),
            (G1::one(), G2::one())
        ]));
        assert!(!pairing_check([
            (G1::one(), G2::one()),
            (G1::one(), G2::one())
        ]));
        assert!(!pairing_check([
            (G1::neg_one(), G2::neg_one()),
            (G1::neg_one(), G2::neg_one())
        ]));

        assert!(!pairing_check([
            (g1::tests::some_val(), g2::tests::some_val()),
            (g1::tests::neg_some_val(), g2::tests::neg_some_val()),
        ]));

        assert!(!pairing_check([
            (g1::tests::some_val(), g2::tests::some_val()),
            (g1::tests::some_val(), g2::tests::some_val()),
        ]));
    }
}
