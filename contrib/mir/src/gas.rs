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

#[derive(Debug, PartialEq, Eq)]
pub struct OutOfGas;

/// Overflow errors can be treated as out-of-gas errors
impl From<std::num::TryFromIntError> for OutOfGas {
    fn from(_: std::num::TryFromIntError) -> Self {
        OutOfGas
    }
}

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

pub mod tc_cost {
    use super::OutOfGas;

    // Due to the quirk of the Tezos protocol implementation, step gas is
    // charged twice as often as in MIR.
    pub const INSTR_STEP: u32 = 220 * 2;

    pub const VALUE_STEP: u32 = 100;

    fn variadic(depth: u16) -> Result<u32, OutOfGas> {
        (depth as u32).checked_mul(50).ok_or(OutOfGas)
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
        Ok(std::cmp::min(sz1, sz2)
            .checked_mul(60) // according to Nairobi
            .ok_or(OutOfGas)?
            .try_into()?)
    }
}

pub mod interpret_cost {
    use super::OutOfGas;

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

    pub const INTERPRET_RET: u32 = 15; // corresponds to KNil in the Tezos protocol
    pub const LOOP_ENTER: u32 = 10; // corresponds to KLoop_in in the Tezos protocol
    pub const LOOP_EXIT: u32 = 10;

    fn dropn(n: u16) -> Result<u32, OutOfGas> {
        // Approximates 30 + 2.713108*n, copied from the Tezos protocol
        let go = |n: u32| {
            n.checked_mul(2)?
                .checked_add(30)?
                .checked_add(n >> 1)?
                .checked_add(n >> 3)
        };
        go(n as u32).ok_or(OutOfGas)
    }

    pub fn drop(mb_n: Option<u16>) -> Result<u32, OutOfGas> {
        mb_n.map_or(Ok(DROP), dropn)
    }

    fn dipn(n: u16) -> Result<u32, OutOfGas> {
        // Approximates 15 + 4.05787663635*n, copied from the Tezos protocol
        let go = |n: u32| n.checked_mul(4)?.checked_add(15);
        go(n as u32).ok_or(OutOfGas)
    }

    pub fn dip(mb_n: Option<u16>) -> Result<u32, OutOfGas> {
        mb_n.map_or(Ok(DIP), dipn)
    }

    pub fn undip(n: u16) -> Result<u32, OutOfGas> {
        // this is derived by observing gas costs as of Nairobi, as charged by
        // the Tezos protocol. It seems undip cost is charged as
        // cost_N_KUndip * n + cost_N_KCons,
        // where cost_N_KUndip = cost_N_KCons = 10
        let go = |n: u32| n.checked_add(1)?.checked_mul(10);
        go(n as u32).ok_or(OutOfGas)
    }

    fn dupn(n: u16) -> Result<u32, OutOfGas> {
        // Approximates 20 + 1.222263*n, copied from the Tezos protocol
        let go = |n: u32| n.checked_add(20)?.checked_add(n >> 2);
        go(n as u32).ok_or(OutOfGas)
    }

    pub fn dup(mb_n: Option<u16>) -> Result<u32, OutOfGas> {
        mb_n.map_or(Ok(DUP), dupn)
    }

    pub fn add_int<T>(i1: T, i2: T) -> Result<u32, OutOfGas> {
        // NB: eventually when using BigInts, use BigInt::bits() &c
        use std::mem::size_of_val;
        // max is copied from the Tezos protocol, ostensibly adding two big ints depends on
        // the larger of the two due to result allocation
        let sz = std::cmp::max(size_of_val(&i1), size_of_val(&i2));
        Ok((sz >> 1).checked_add(35).ok_or(OutOfGas)?.try_into()?)
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
