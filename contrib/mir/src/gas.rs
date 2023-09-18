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
    // Due to the quirk of the Tezos protocol implementation, step gas is
    // charged twice as often as in MIR.
    pub const INSTR_STEP: u32 = 220 * 2;

    pub const VALUE_STEP: u32 = 100;

    fn variadic(depth: usize) -> u32 {
        (depth * 50).try_into().unwrap()
    }

    pub fn drop_n(depth: &Option<usize>) -> u32 {
        depth.map(variadic).unwrap_or(0)
    }

    pub fn dip_n(depth: &Option<usize>) -> u32 {
        depth.map(variadic).unwrap_or(0)
    }

    pub fn ty_eq(sz1: usize, sz2: usize) -> u32 {
        // complexity of comparing types T and U is O(min(|T|, |U|)), as
        // comparison short-circuits at the first mismatch
        (std::cmp::min(sz1, sz2) * 60).try_into().unwrap() // according to Nairobi
    }
}

pub mod interpret_cost {
    pub const DIP: u32 = 10;
    pub const DROP: u32 = 10;
    pub const DUP: u32 = 10;
    pub const GT: u32 = 10;
    pub const IF: u32 = 10;
    pub const LOOP: u32 = 10;
    pub const SWAP: u32 = 10;
    pub const INT_NAT: u32 = 10;
    pub const PUSH: u32 = 10;

    pub const INTERPRET_RET: u32 = 15; // corresponds to KNil in the Tezos protocol
    pub const LOOP_ENTER: u32 = 10; // corresponds to KLoop_in in the Tezos protocol
    pub const LOOP_EXIT: u32 = 10;

    fn dropn(n: usize) -> u32 {
        // Approximates 30 + 2.713108*n, copied from the Tezos protocol
        (30 + (2 * n) + (n >> 1) + (n >> 3)).try_into().unwrap()
    }

    pub fn drop(mb_n: Option<usize>) -> u32 {
        mb_n.map(dropn).unwrap_or(DROP)
    }

    fn dipn(n: usize) -> u32 {
        // Approximates 15 + 4.05787663635*n, copied from the Tezos protocol
        (15 + (4 * n)).try_into().unwrap()
    }

    pub fn dip(mb_n: Option<usize>) -> u32 {
        mb_n.map(dipn).unwrap_or(DIP)
    }

    pub fn undip(n: usize) -> u32 {
        // this is derived by observing gas costs as of Nairobi, as charged by
        // the Tezos protocol. It seems undip cost is charged as
        // cost_N_KUndip * n + cost_N_KCons,
        // where cost_N_KUndip = cost_N_KCons = 10
        (10 * (n + 1)).try_into().unwrap()
    }

    fn dupn(n: usize) -> u32 {
        // Approximates 20 + 1.222263*n, copied from the Tezos protocol
        (20 + n + (n >> 2)).try_into().unwrap()
    }

    pub fn dup(mb_n: Option<usize>) -> u32 {
        mb_n.map(dupn).unwrap_or(DUP)
    }

    pub fn add_int(i1: i32, i2: i32) -> u32 {
        // NB: eventually when using BigInts, use BigInt::bits() &c
        use std::mem::size_of_val;
        // max is copied from the Tezos protocol, ostensibly adding two big ints depends on
        // the larger of the two due to result allocation
        let sz = std::cmp::max(size_of_val(&i1), size_of_val(&i2));
        (35 + (sz >> 1)).try_into().unwrap()
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
}
