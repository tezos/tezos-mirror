/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

#[derive(Debug)]
pub struct Gas {
    milligas_amount: Option<u32>,
}

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
#[error("out of gas")]
pub struct OutOfGas;

// Default gas limit per transaction, according to
// https://opentezos.com/tezos-basics/economics-and-rewards/#transaction-cost
const DEFAULT_GAS_AMOUNT: u32 = 1_040_000;

impl Default for Gas {
    fn default() -> Self {
        Gas::new(DEFAULT_GAS_AMOUNT * 1000)
    }
}

impl Gas {
    pub fn new(milligas_amount: u32) -> Gas {
        Gas {
            milligas_amount: Some(milligas_amount),
        }
    }

    pub fn consume(&mut self, cost: u32) -> Result<(), OutOfGas> {
        self.milligas_amount = self.milligas().checked_sub(cost);
        if self.milligas_amount.is_none() {
            Err(OutOfGas)
        } else {
            Ok(())
        }
    }

    pub fn milligas(&self) -> u32 {
        self.milligas_amount
            .expect("Access to gas after exhaustion")
    }
}

trait AsGasCost {
    /// Try to convert a checked numeric type to gas cost; return `OutOfGas` on
    /// overflow.
    fn as_gas_cost(&self) -> Result<u32, OutOfGas>;
}

impl AsGasCost for checked::Checked<u32> {
    fn as_gas_cost(&self) -> Result<u32, OutOfGas> {
        self.ok_or(OutOfGas)
    }
}

impl AsGasCost for checked::Checked<usize> {
    fn as_gas_cost(&self) -> Result<u32, OutOfGas> {
        self.ok_or(OutOfGas)?.try_into().map_err(|_| OutOfGas)
    }
}

pub mod tc_cost {
    use checked::Checked;

    use super::{AsGasCost, OutOfGas};

    // Due to the quirk of the Tezos protocol implementation, step gas is
    // charged twice as often as in MIR.
    pub const INSTR_STEP: u32 = 220 * 2;

    pub const VALUE_STEP: u32 = 100;

    fn variadic(depth: u16) -> Result<u32, OutOfGas> {
        let depth = Checked::from(depth as u32);
        (depth * 50).as_gas_cost()
    }

    pub fn drop_n(depth: &Option<u16>) -> Result<u32, OutOfGas> {
        depth.map_or(Ok(0), variadic)
    }

    pub fn dip_n(depth: &Option<u16>) -> Result<u32, OutOfGas> {
        depth.map_or(Ok(0), variadic)
    }

    pub fn ty_eq(sz1: usize, sz2: usize) -> Result<u32, OutOfGas> {
        // complexity of comparing types T and U is O(min(|T|, |U|)), as
        // comparison short-circuits at the first mismatch
        let sz = Checked::from(std::cmp::min(sz1, sz2));
        (sz * 60).as_gas_cost()
    }
}

pub mod interpret_cost {
    use checked::Checked;

    use super::{AsGasCost, OutOfGas};
    use crate::ast::TypedValue;

    pub const DIP: u32 = 10;
    pub const DROP: u32 = 10;
    pub const DUP: u32 = 10;
    pub const GT: u32 = 10;
    pub const IF: u32 = 10;
    pub const IF_NONE: u32 = 10;
    pub const LOOP: u32 = 10;
    pub const SWAP: u32 = 10;
    pub const INT_NAT: u32 = 10;
    pub const PUSH: u32 = 10;
    pub const ADD_TEZ: u32 = 20;
    pub const UNIT: u32 = 10;
    pub const CAR: u32 = 10;
    pub const CDR: u32 = 10;
    pub const PAIR: u32 = 10;
    pub const SOME: u32 = 10;
    pub const AMOUNT: u32 = 10;
    pub const NIL: u32 = 10;

    pub const INTERPRET_RET: u32 = 15; // corresponds to KNil in the Tezos protocol
    pub const LOOP_ENTER: u32 = 10; // corresponds to KLoop_in in the Tezos protocol
    pub const LOOP_EXIT: u32 = 10;

    fn dropn(n: u16) -> Result<u32, OutOfGas> {
        // Approximates 30 + 2.713108*n, copied from the Tezos protocol
        let n = Checked::from(n as u32);
        (30 + n * 2 + (n >> 1) + (n >> 3)).as_gas_cost()
    }

    pub fn drop(mb_n: Option<u16>) -> Result<u32, OutOfGas> {
        mb_n.map_or(Ok(DROP), dropn)
    }

    fn dipn(n: u16) -> Result<u32, OutOfGas> {
        // Approximates 15 + 4.05787663635*n, copied from the Tezos protocol
        let n = Checked::from(n as u32);
        (15 + n * 4).as_gas_cost()
    }

    pub fn dip(mb_n: Option<u16>) -> Result<u32, OutOfGas> {
        mb_n.map_or(Ok(DIP), dipn)
    }

    pub fn undip(n: u16) -> Result<u32, OutOfGas> {
        // this is derived by observing gas costs as of Nairobi, as charged by
        // the Tezos protocol. It seems undip cost is charged as
        // cost_N_KUndip * n + cost_N_KCons,
        // where cost_N_KUndip = cost_N_KCons = 10
        let n = Checked::from(n as u32);
        ((n + 1) * 10).as_gas_cost()
    }

    fn dupn(n: u16) -> Result<u32, OutOfGas> {
        // Approximates 20 + 1.222263*n, copied from the Tezos protocol
        let n = Checked::from(n as u32);
        (20 + n + (n >> 2)).as_gas_cost()
    }

    pub fn dup(mb_n: Option<u16>) -> Result<u32, OutOfGas> {
        mb_n.map_or(Ok(DUP), dupn)
    }

    pub fn add_int<T>(i1: T, i2: T) -> Result<u32, OutOfGas> {
        // NB: eventually when using BigInts, use BigInt::bits() &c
        use std::mem::size_of_val;
        // max is copied from the Tezos protocol, ostensibly adding two big ints depends on
        // the larger of the two due to result allocation
        let sz = Checked::from(std::cmp::max(size_of_val(&i1), size_of_val(&i2)));
        (35 + (sz >> 1)).as_gas_cost()
    }

    pub fn compare(v1: &TypedValue, v2: &TypedValue) -> Result<u32, OutOfGas> {
        use TypedValue as V;
        let cmp_bytes = |s1: usize, s2: usize| {
            // Approximating 35 + 0.024413 x term
            let v = Checked::from(std::cmp::min(s1, s2));
            (35 + (v >> 6) + (v >> 7)).as_gas_cost()
        };
        let cmp_pair = |l1, r1, l2, r2| {
            let c = Checked::from(10u32);
            (c + compare(l1, r1)? + compare(l2, r2)?).as_gas_cost()
        };
        let cmp_option = Checked::from(10u32);
        Ok(match (v1, v2) {
            (V::Nat(l), V::Nat(r)) => {
                // NB: eventually when using BigInts, use BigInt::bits() &c
                cmp_bytes(std::mem::size_of_val(l), std::mem::size_of_val(r))?
            }
            (V::Int(l), V::Int(r)) => {
                // NB: eventually when using BigInts, use BigInt::bits() &c
                cmp_bytes(std::mem::size_of_val(l), std::mem::size_of_val(r))?
            }
            (V::Bool(_), V::Bool(_)) => cmp_bytes(1, 1)?,
            (V::Mutez(_), V::Mutez(_)) => cmp_bytes(8, 8)?,
            (V::String(l), V::String(r)) => cmp_bytes(l.len(), r.len())?,
            (V::Unit, V::Unit) => 10,
            (V::Pair(l1, r1), V::Pair(l2, r2)) => cmp_pair(l1, r1, l2, r2)?,
            (V::Option(l), V::Option(r)) => match (l, r) {
                (None, None) => cmp_option,
                (None, Some(_)) => cmp_option,
                (Some(_), None) => cmp_option,
                (Some(l), Some(r)) => cmp_option + compare(l, r)?,
            }
            .as_gas_cost()?,
            _ => unreachable!("Comparison of incomparable values"),
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn gas_consumption() {
        let mut gas = Gas::new(100);
        gas.consume(30).unwrap();
        assert_eq!(gas.milligas(), 70)
    }

    #[test]
    fn gas_exhaustion_error() {
        let mut gas = Gas::new(100);
        assert_eq!(gas.consume(1000), Err(OutOfGas));
    }

    #[test]
    #[should_panic(expected = "Access to gas after exhaustion")]
    fn gas_exhaustion_panic() {
        let mut gas = Gas::new(100);
        assert_eq!(gas.consume(1000), Err(OutOfGas));

        let _ = gas.consume(1000); // panics
    }

    #[test]
    fn overflow_to_out_of_gas() {
        for n in [usize::MAX, usize::MAX / 2, usize::MAX / 4] {
            assert_eq!(super::tc_cost::ty_eq(n, n), Err(OutOfGas));
        }
    }
}
