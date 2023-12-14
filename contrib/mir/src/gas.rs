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

#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
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

/// A hack to get the integral logarithm base 2, rounded up.
/// Rounds up to the nearest power of 2 and counts trailing zeroes. Thus,
///
/// ```text
/// log2i(1) = log2i(0b1) = 0;
/// log2i(2) = log2i(0b10) = 1;
/// log2i(3) = log2i(4) = log2i(0b100) = 2;
/// ```
/// &c
///
/// `log2i(0)` is not well-defined, and likely is a logic error, hence the
/// function will panic on `0`.
fn log2i(x: usize) -> u32 {
    assert!(x != 0);
    x.next_power_of_two().trailing_zeros()
}

pub mod tc_cost {
    use checked::Checked;

    use super::{AsGasCost, OutOfGas};

    // Due to the quirk of the Tezos protocol implementation, step gas is
    // charged twice as often as in MIR.
    pub const INSTR_STEP: u32 = 220 * 2;

    pub const VALUE_STEP: u32 = 100;

    // Corresponds to cost_PARSE_TYPE1 in the Tezos protocol.
    pub const PARSE_TYPE_STEP: u32 = 60;
    // Taken to be the same as VERIFY_TYPE_STEP, but that's a guess
    pub const TYPE_PROP_STEP: u32 = 60;

    // corresponds to cost_B58CHECK_ENCODING_PUBLIC_KEY_HASH_bls in the
    // protocol. the protocol computes cost as
    // `max(bls,ed25519,p256,secp256k1)`, which happens to be `bls`
    pub const KEY_HASH_READABLE: u32 = 3200;

    // corresponds to cost_ENCODING_PUBLIC_KEY_HASH_bls in the
    // protocol. the protocol computes cost as
    // `max(bls,ed25519,p256,secp256k1)`, which happens to be `bls`
    pub const KEY_HASH_OPTIMIZED: u32 = 80;

    // corresponds to cost_B58CHECK_DECODING_PUBLIC_KEY_HASH_bls in the
    // protocol. the protocol computes cost as
    // `max(bls,ed25519,p256,secp256k1)`, which happens to be `bls`
    pub const KEY_READABLE: u32 = 3600;

    // corresponds to cost_DECODING_PUBLIC_KEY_HASH_bls in the
    // protocol. the protocol computes cost as
    // `max(bls,ed25519,p256,secp256k1)`, but they're all equal
    pub const KEY_OPTIMIZED: u32 = 60;

    // corresponds to cost_B58CHECK_DECODING_CHAIN_ID in the protocol
    pub const CHAIN_ID_READABLE: u32 = 1600;

    // corresponds to cost_DECODING_CHAIN_ID in the protocol
    pub const CHAIN_ID_OPTIMIZED: u32 = 50;

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

    pub fn construct_map(key_size: usize, sz: usize) -> Result<u32, OutOfGas> {
        // Tezos protocol constructs maps element by element, thus the cost ends
        // up Σ (80 + key_size*log2(i)) = 80 * n + key_size * Σ log2(i) = 80 * n
        // + key_size * log2(Π i) = 80 * n + key_size * log2(n!)
        // Using n * log2(n) as an approximation for log2(n!),
        // ≈ 80 * n + key_size * n * log2(n)
        // which seems like a reasonable first-order approximation.
        // to avoid log2(0) it's more practical to compute log2(n + 1)
        let n = Checked::from(sz);
        let key_size = Checked::from(key_size);
        let log2n = super::log2i((n + 1).ok_or(OutOfGas)?) as usize;
        (80 * n + key_size * n * log2n).as_gas_cost()
    }

    pub fn construct_set(val_size: usize, sz: usize) -> Result<u32, OutOfGas> {
        // Similar to `construct_map`, only the coefficient differs
        let n = Checked::from(sz);
        let key_size = Checked::from(val_size);
        let log2n = super::log2i((n + 1).ok_or(OutOfGas)?) as usize;
        (130 * n + key_size * n * log2n).as_gas_cost()
    }
}

pub mod interpret_cost {
    use checked::Checked;

    use super::{AsGasCost, OutOfGas};
    use crate::ast::{Key, KeyHash, Micheline, Or, TypedValue};

    pub const DIP: u32 = 10;
    pub const DROP: u32 = 10;
    pub const DUP: u32 = 10;
    pub const GT: u32 = 10;
    pub const EQ: u32 = 10;
    pub const LE: u32 = 10;
    pub const IF: u32 = 10;
    pub const IF_NONE: u32 = 10;
    pub const IF_CONS: u32 = 10;
    pub const IF_LEFT: u32 = 10;
    pub const LOOP: u32 = 10;
    pub const ITER: u32 = 20;
    pub const SWAP: u32 = 10;
    pub const INT_NAT: u32 = 10;
    pub const PUSH: u32 = 10;
    pub const ADD_TEZ: u32 = 20;
    pub const UNIT: u32 = 10;
    pub const CAR: u32 = 10;
    pub const CDR: u32 = 10;
    pub const PAIR: u32 = 10;
    pub const UNPAIR: u32 = 10;
    pub const SOME: u32 = 10;
    pub const NONE: u32 = 10;
    pub const AMOUNT: u32 = 10;
    pub const NIL: u32 = 10;
    pub const CONS: u32 = 15;
    pub const EMPTY_SET: u32 = 300;
    pub const CHAIN_ID: u32 = 15;
    pub const PACK: u32 = 0;
    pub const SELF: u32 = 10;
    pub const ADDRESS: u32 = 10;
    pub const LEFT: u32 = 10;
    pub const RIGHT: u32 = 10;

    // Gas costs obtained from https://gitlab.com/tezos/tezos/-/blob/9875fbebe032a8c5ce62b3b3cb1588ca9855a37e/src/proto_017_PtNairob/lib_protocol/michelson_v1_gas_costs_generated.ml
    pub const TRANSFER_TOKENS: u32 = 60;
    pub const SET_DELEGATE: u32 = 60;

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
        let cmp_pair = |l: &(_, _), r: &(_, _)| {
            let c = Checked::from(10u32);
            (c + compare(&l.0, &r.0)? + compare(&l.1, &r.1)?).as_gas_cost()
        };
        let cmp_option = Checked::from(10u32);
        const ADDRESS_SIZE: usize = 20 + 31; // hash size + max entrypoint size
        const CMP_CHAIN_ID: u32 = 30;
        const CMP_KEY: u32 = 92; // hard-coded in the protocol
        const CMP_SIGNATURE: u32 = 92; // hard-coded in the protocol
        let cmp_or = Checked::from(10u32);
        #[track_caller]
        fn incomparable() -> ! {
            unreachable!("Comparison of incomparable values")
        }
        Ok(match (v1, v2) {
            (V::Nat(l), V::Nat(r)) => {
                // NB: eventually when using BigInts, use BigInt::bits() &c
                cmp_bytes(std::mem::size_of_val(l), std::mem::size_of_val(r))?
            }
            (V::Nat(_), _) => incomparable(),

            (V::Int(l), V::Int(r)) => {
                // NB: eventually when using BigInts, use BigInt::bits() &c
                cmp_bytes(std::mem::size_of_val(l), std::mem::size_of_val(r))?
            }
            (V::Int(_), _) => incomparable(),

            (V::Bool(_), V::Bool(_)) => cmp_bytes(1, 1)?,
            (V::Bool(_), _) => incomparable(),

            (V::Mutez(_), V::Mutez(_)) => cmp_bytes(8, 8)?,
            (V::Mutez(_), _) => incomparable(),

            (V::String(l), V::String(r)) => cmp_bytes(l.len(), r.len())?,
            (V::String(_), _) => incomparable(),

            (V::Unit, V::Unit) => 10,
            (V::Unit, _) => incomparable(),

            (V::Pair(l), V::Pair(r)) => cmp_pair(l.as_ref(), r.as_ref())?,
            (V::Pair(_), _) => incomparable(),

            (V::Option(l), V::Option(r)) => match (l, r) {
                (None, None) => cmp_option,
                (None, Some(_)) => cmp_option,
                (Some(_), None) => cmp_option,
                (Some(l), Some(r)) => cmp_option + compare(l, r)?,
            }
            .as_gas_cost()?,
            (V::Option(_), _) => incomparable(),

            (V::Address(..), V::Address(..)) => cmp_bytes(ADDRESS_SIZE, ADDRESS_SIZE)?,
            (V::Address(_), _) => incomparable(),

            (V::ChainId(..), V::ChainId(..)) => CMP_CHAIN_ID,
            (V::ChainId(_), _) => incomparable(),

            (V::Bytes(l), V::Bytes(r)) => cmp_bytes(l.len(), r.len())?,
            (V::Bytes(_), _) => incomparable(),

            (V::Key(_), V::Key(_)) => CMP_KEY,
            (V::Key(_), _) => incomparable(),

            (V::Signature(_), V::Signature(_)) => CMP_SIGNATURE,
            (V::Signature(_), _) => incomparable(),

            (V::KeyHash(_), V::KeyHash(_)) => cmp_bytes(KeyHash::BYTE_SIZE, KeyHash::BYTE_SIZE)?,
            (V::KeyHash(_), _) => incomparable(),

            (V::Or(l), V::Or(r)) => match (l.as_ref(), r.as_ref()) {
                (Or::Left(x), Or::Left(y)) => cmp_or + compare(x, y)?,
                (Or::Right(x), Or::Right(y)) => cmp_or + compare(x, y)?,
                (Or::Left(_), Or::Right(_)) => cmp_or,
                (Or::Right(_), Or::Left(_)) => cmp_or,
            }
            .as_gas_cost()?,
            (V::Or(..), _) => incomparable(),

            (V::List(..) | V::Set(..) | V::Map(..) | V::Contract(_) | V::Operation(_), _) => {
                incomparable()
            }
        })
    }

    pub fn map_mem(k: &TypedValue, map_size: usize) -> Result<u32, OutOfGas> {
        map_get(k, map_size)
    }

    pub fn map_get(k: &TypedValue, map_size: usize) -> Result<u32, OutOfGas> {
        // NB: this doesn't copy the tezos model exactly; tezos model uses
        //
        // 80 + sizeof(key)*log2(map.size)
        //
        // this seems dubious, from first principles and dimensional analysis,
        // this seems more probable:
        //
        // 80 + cost_of_compare(key)*log2(map.size + 1)
        //
        // "+ 1" is from the observation that a lookup in a map of size 1 does
        // exactly one comparison.
        let compare_cost = compare(k, k)?;
        let size_log = super::log2i(map_size + 1);
        let lookup_cost = Checked::from(compare_cost) * size_log;
        (80 + lookup_cost).as_gas_cost()
    }

    pub fn set_mem(k: &TypedValue, map_size: usize) -> Result<u32, OutOfGas> {
        // NB: same considerations as for map_get
        let compare_cost = compare(k, k)?;
        let size_log = super::log2i(map_size + 1);
        let lookup_cost = Checked::from(compare_cost) * size_log;
        (115 + lookup_cost).as_gas_cost()
    }

    pub fn map_update(k: &TypedValue, map_size: usize) -> Result<u32, OutOfGas> {
        // NB: same considerations as for map_get
        let compare_cost = compare(k, k)?;
        let size_log = super::log2i(map_size + 1);
        let lookup_cost = Checked::from(compare_cost) * size_log;
        // NB: 2 factor copied from Tezos protocol, in principle it should
        // reflect update vs get overhead.
        (80 + 2 * lookup_cost).as_gas_cost()
    }

    pub fn set_update(k: &TypedValue, map_size: usize) -> Result<u32, OutOfGas> {
        // NB: same considerations as for map_update
        let compare_cost = compare(k, k)?;
        let size_log = super::log2i(map_size + 1);
        let lookup_cost = Checked::from(compare_cost) * size_log;
        // coefficient larger than in case of Map looks suspicious, something
        // to benchmark later
        (130 + 2 * lookup_cost).as_gas_cost()
    }

    /// Measures size of Michelson using several metrics.
    pub struct MichelineSize {
        /// Total number of nodes (including leaves).
        nodes_num: Checked<usize>,

        /// Total size of string and bytes literals.
        str_byte: Checked<usize>,

        /// Total size of zarith numbers, in bytes.
        zariths: Checked<usize>,
    }

    impl Default for MichelineSize {
        fn default() -> Self {
            MichelineSize {
                nodes_num: Checked::from(0),
                str_byte: Checked::from(0),
                zariths: Checked::from(0),
            }
        }
    }

    pub fn micheline_encoding<'a>(mich: &'a Micheline<'a>) -> Result<u32, OutOfGas> {
        let mut size = MichelineSize::default();
        collect_micheline_size(mich, &mut size);
        micheline_encoding_by_size(size)
    }

    fn micheline_encoding_by_size(size: MichelineSize) -> Result<u32, OutOfGas> {
        (size.nodes_num * 100 + size.zariths * 25 + size.str_byte * 10).as_gas_cost()
    }

    fn collect_micheline_size<'a>(mich: &'a Micheline<'a>, size: &mut MichelineSize) {
        size.nodes_num += 1;
        match mich {
            Micheline::String(s) => size.str_byte += s.len(),
            Micheline::Bytes(bs) => size.str_byte += bs.len(),
            Micheline::Int(i) => {
                // NB: eventually when using BigInts, use BigInt::bits() &c
                let bits = std::mem::size_of_val(i);
                let bytes = (bits + 7) / 8;
                size.zariths += bytes;
            }
            Micheline::Seq(ms) => {
                for m in *ms {
                    collect_micheline_size(m, size)
                }
            }
            Micheline::App(_prim, args, annots) => {
                for arg in *args {
                    collect_micheline_size(arg, size)
                }
                for annot in annots {
                    // Annotations are accounted as simple string literals
                    use crate::lexer::Annotation as Ann;
                    size.str_byte += match annot {
                        // Including annotation prefix into the size too
                        Ann::Field(a) => a.len() + 1,
                        Ann::Variable(a) => a.len() + 1,
                        Ann::Type(a) => a.len() + 1,
                        Ann::Special(a) => a.len(),
                    }
                }
            }
        }
    }

    pub fn check_signature(k: &Key, msg: &[u8]) -> Result<u32, OutOfGas> {
        let len = Checked::from(msg.len());
        match k {
            Key::Ed25519(..) => 65800 + ((len >> 3) + len),
            Key::Secp256k1(..) => 51600 + ((len >> 3) + len),
            Key::P256(..) => 341000 + ((len >> 3) + len),
            Key::Bls(..) => 1570000 + (len * 3),
        }
        .as_gas_cost()
    }

    pub fn slice(length: usize) -> Result<u32, OutOfGas> {
        // In the protocol, the gas costs for slicing strings and bytes are defined
        // separately (see `cost_N_ISlice_bytes` and `cost_N_ISlice_string`).
        //
        // In practice, they both have the same cost.
        ((Checked::from(length) >> 1) + 25).as_gas_cost()
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

    #[test]
    fn log2i_test() {
        assert_eq!(log2i(1), 0);
        assert_eq!(log2i(2), 1);
        assert_eq!(log2i(3), 2);
        assert_eq!(log2i(4), 2);
        assert_eq!(log2i(5), 3);
        assert_eq!(log2i(70_000), 17);
        assert_eq!(log2i(100_000), 17);
        assert_eq!(log2i(300_000), 19);
    }

    #[test]
    #[should_panic(expected = "assertion failed: x != 0")]
    fn log2i_test_panic() {
        log2i(0);
    }
}
