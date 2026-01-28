// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! Gas accounting and costs.

use num_bigint::{BigInt, BigUint};

/// Structure carrying the remaining gas amount.
#[derive(Debug)]
pub struct Gas {
    milligas_amount: Option<u32>,
}

/// Out of gas error.
#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
#[error("Gas_exhaustion")]
pub struct OutOfGas;

/// Default gas limit per transaction, according to
/// <https://opentezos.com/tezos-basics/economics-and-rewards/#transaction-cost>
pub const DEFAULT_GAS_AMOUNT: u32 = 1_040_000;

impl Default for Gas {
    /// Constructs [Gas] with [DEFAULT_GAS_AMOUNT] gas remaining.
    fn default() -> Self {
        Gas::new(DEFAULT_GAS_AMOUNT * 1000)
    }
}

impl Gas {
    /// Construct a new [Gas] with the specified `milligas_amount` milligas
    /// remaining.
    pub fn new(milligas_amount: u32) -> Gas {
        Gas {
            milligas_amount: Some(milligas_amount),
        }
    }

    /// Try to consume the specified milligas `cost`. If not enough gas left,
    /// return [OutOfGas], and mark gas as exhausted.
    ///
    /// # Panics
    ///
    /// If gas was previously exhausted.
    pub fn consume(&mut self, cost: u32) -> Result<(), OutOfGas> {
        self.milligas_amount = self.milligas().checked_sub(cost);
        if self.milligas_amount.is_none() {
            Err(OutOfGas)
        } else {
            Ok(())
        }
    }

    /// Get the remaining milligas amount.
    ///
    /// # Panics
    ///
    /// If gas was previously exhausted.
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

impl AsGasCost for checked::Checked<u64> {
    fn as_gas_cost(&self) -> Result<u32, OutOfGas> {
        self.ok_or(OutOfGas)?.try_into().map_err(|_| OutOfGas)
    }
}

trait Log2i {
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
    fn log2i(self) -> u32;
}

impl Log2i for usize {
    fn log2i(self) -> u32 {
        assert!(self != 0);
        self.next_power_of_two().trailing_zeros()
    }
}

impl Log2i for u64 {
    fn log2i(self) -> u32 {
        assert!(self != 0);
        self.next_power_of_two().trailing_zeros()
    }
}

/// Typechecking gas costs.
#[allow(missing_docs)]
pub mod tc_cost {
    use checked::Checked;

    use super::{AsGasCost, Log2i, OutOfGas};

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

    // corresponds to cost_DECODING_BLS_FR in the protocol.
    pub const BLS_FR: u32 = 120;

    // corresponds to cost_DECODING_BLS_G1 in the protocol.
    pub const BLS_G1: u32 = 54600;

    // corresponds to cost_DECODING_BLS_G2 in the protocol.
    pub const BLS_G2: u32 = 69000;

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

    pub fn timestamp_decoding(l: usize) -> Result<u32, OutOfGas> {
        use integer_sqrt::IntegerSquareRoot;
        let v0: Checked<usize> = Checked::from(l.integer_sqrt()) * l;
        (105 + ((v0 >> 5) + (v0 >> 6))).as_gas_cost()
    }

    fn variadic(depth: u16) -> Result<u32, OutOfGas> {
        let depth = Checked::from(depth as u32);
        (depth * 50).as_gas_cost()
    }

    pub fn dig_n(depth: usize) -> Result<u32, OutOfGas> {
        // corresponds to Cost_of.Typechecking.proof_argument in the protocol
        (Checked::from(depth) * 50).as_gas_cost()
    }

    pub fn dug_n(depth: usize) -> Result<u32, OutOfGas> {
        // corresponds to Cost_of.Typechecking.proof_argument in the protocol
        (Checked::from(depth) * 50).as_gas_cost()
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
        let log2n = (n + 1).ok_or(OutOfGas)?.log2i() as usize;
        (80 * n + key_size * n * log2n).as_gas_cost()
    }

    pub fn construct_set(val_size: usize, sz: usize) -> Result<u32, OutOfGas> {
        // Similar to `construct_map`, only the coefficient differs
        let n = Checked::from(sz);
        let key_size = Checked::from(val_size);
        let log2n = (n + 1).ok_or(OutOfGas)?.log2i() as usize;
        (130 * n + key_size * n * log2n).as_gas_cost()
    }

    pub fn pair_n(size: usize) -> Result<u32, OutOfGas> {
        // corresponds to Cost_of.Typechecking.proof_argument in the protocol
        (Checked::from(size) * 50).as_gas_cost()
    }

    pub fn unpair_n(size: usize) -> Result<u32, OutOfGas> {
        // corresponds to Cost_of.Typechecking.proof_argument in the protocol
        (Checked::from(size) * 50).as_gas_cost()
    }

    pub fn get_n(size: usize) -> Result<u32, OutOfGas> {
        // corresponds to Cost_of.Typechecking.proof_argument in the protocol
        (Checked::from(size) * 50).as_gas_cost()
    }

    pub fn update_n(size: usize) -> Result<u32, OutOfGas> {
        // corresponds to Cost_of.Typechecking.proof_argument in the protocol
        (Checked::from(size) * 50).as_gas_cost()
    }
}

/// Get byte size of [BigInt] or [BigUint].
pub trait BigIntByteSize {
    /// Minimal size in bytes a given bigint is representable in.
    fn byte_size(&self) -> u64;
}

/// Bit size divided by 8, rounded up
fn bits_to_bytes(bits: u64) -> u64 {
    (bits + 7) >> 3
}

impl BigIntByteSize for BigInt {
    fn byte_size(&self) -> u64 {
        bits_to_bytes(self.bits())
    }
}

impl BigIntByteSize for BigUint {
    fn byte_size(&self) -> u64 {
        bits_to_bytes(self.bits())
    }
}

/// Interpretation gas costs
#[allow(missing_docs)]
pub mod interpret_cost {
    use checked::Checked;
    use num_bigint::{BigInt, BigUint};
    use num_traits::Zero;
    use std::rc::Rc;
    use tezos_crypto_rs::public_key::PublicKey;
    use tezos_crypto_rs::CryptoError;
    use thiserror::Error;

    use super::{AsGasCost, BigIntByteSize, Log2i, OutOfGas};
    use crate::ast::{Micheline, Or, Ticket, TypedValue};

    pub const DIP: u32 = 10;
    pub const DROP: u32 = 10;
    pub const DUP: u32 = 10;
    pub const GT: u32 = 10;
    pub const GE: u32 = 10;
    pub const EQ: u32 = 10;
    pub const NEQ: u32 = 10;
    pub const LE: u32 = 10;
    pub const LT: u32 = 10;
    pub const IF: u32 = 10;
    pub const IF_NONE: u32 = 10;
    pub const IF_CONS: u32 = 10;
    pub const IF_LEFT: u32 = 10;
    pub const LOOP: u32 = 10;
    pub const ITER: u32 = 20;
    pub const SWAP: u32 = 10;
    pub const ABS: u32 = 10;
    pub const INT_NAT: u32 = 10;
    pub const ISNAT: u32 = 10;
    pub const INT_BLS_FR: u32 = 115;
    pub const PUSH: u32 = 10;
    pub const ADD_TEZ: u32 = 20;
    pub const ADD_BLS_FR: u32 = 30;
    pub const ADD_BLS_G1: u32 = 900;
    pub const ADD_BLS_G2: u32 = 2470;
    pub const MUL_BLS_G1: u32 = 103000;
    pub const MUL_BLS_G2: u32 = 220000;
    pub const MUL_BLS_FR: u32 = 45;
    pub const MUL_TEZ_NAT: u32 = 50;
    pub const MUL_NAT_TEZ: u32 = MUL_TEZ_NAT; // should be the same always
    pub const EDIV_TEZ_TEZ: u32 = 80;
    pub const EDIV_TEZ_NAT: u32 = 70;
    pub const NEG_FR: u32 = 30;
    pub const NEG_G1: u32 = 50;
    pub const NEG_G2: u32 = 70;
    pub const SUB_MUTEZ: u32 = 15;
    pub const UNIT: u32 = 10;
    pub const AND_BOOL: u32 = 10;
    pub const OR_BOOL: u32 = 10;
    pub const XOR_BOOL: u32 = 15;
    pub const NOT_BOOL: u32 = 10;
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
    pub const SIZE_STRING: u32 = 15;
    pub const SIZE_BYTES: u32 = 10;
    pub const SIZE_LIST: u32 = 10;
    pub const SIZE_SET: u32 = 10;
    pub const SIZE_MAP: u32 = 10;
    pub const EMPTY_MAP: u32 = 300;
    pub const EMPTY_BIG_MAP: u32 = 300;
    pub const CHAIN_ID: u32 = 15;
    pub const PACK: u32 = 0;
    pub const SELF: u32 = 10;
    pub const ADDRESS: u32 = 10;
    pub const LEFT: u32 = 10;
    pub const RIGHT: u32 = 10;

    // See `cost_N_IOpt_map` in the Tezos protocol
    pub const MAP_OPTION: u32 = 10;
    // See `cost_N_IList_map` in the Tezos protocol
    pub const MAP_LIST: u32 = 10;
    // In the Tezos protocol, the cost of `MAP` over a `map k v` is a function of its size (see `cost_N_IMap_map`)
    // because it needs to fold the map into a list of key-value pairs first.
    // We don't do that in this interpreter, therefore the cost here is a constant and not a function.
    pub const MAP_MAP: u32 = 20;

    // Gas costs obtained from https://gitlab.com/tezos/tezos/-/blob/9875fbebe032a8c5ce62b3b3cb1588ca9855a37e/src/proto_017_PtNairob/lib_protocol/michelson_v1_gas_costs_generated.ml
    pub const TRANSFER_TOKENS: u32 = 60;
    pub const SET_DELEGATE: u32 = 60;
    pub const LAMBDA: u32 = 10;
    pub const EXEC: u32 = 10;
    pub const HASH_KEY: u32 = 605;
    // slight deviation from the protocol: in the protocol, APPLY costs differer
    // depending on whether a lambda is recursive; here this distinction doesn't
    // make a lot of sense.
    pub const APPLY: u32 = 140;
    pub const TICKET: u32 = 10;
    pub const READ_TICKET: u32 = 10;
    pub const BALANCE: u32 = 10;
    pub const CONTRACT: u32 = 30;
    pub const LEVEL: u32 = 10;
    pub const MIN_BLOCK_TIME: u32 = 20;
    pub const SELF_ADDRESS: u32 = 10;
    pub const SENDER: u32 = 10;
    pub const SOURCE: u32 = 10;
    pub const NOW: u32 = 10;
    pub const IMPLICIT_ACCOUNT: u32 = 10;
    pub const IS_IMPLICIT_ACCOUNT: u32 = 10;
    pub const VOTING_POWER: u32 = 640;
    pub const TOTAL_VOTING_POWER: u32 = 450;
    pub const EMIT: u32 = 30;

    pub const INTERPRET_RET: u32 = 15; // corresponds to KNil in the Tezos protocol
    pub const LOOP_ENTER: u32 = 10; // corresponds to KLoop_in in the Tezos protocol
    pub const LOOP_LEFT_ENTER: u32 = 10; // corresponds to KLoop_in_left in the Tezos protocol
    pub const LOOP_EXIT: u32 = 10;
    pub const CREATE_CONTRACT: u32 = 60;
    pub const VIEW: u32 = 1460; // corresponds to cost_N_IView_synthesized in the Tezos protocol

    pub fn join_tickets(t1: &Ticket, t2: &Ticket) -> Result<u32, OutOfGas> {
        compare(&t1.content, &t2.content)?;
        add_num(&t1.amount, &t2.amount)
    }

    pub fn split_ticket(amount1: &BigUint, amount2: &BigUint) -> Result<u32, OutOfGas> {
        use std::mem::size_of_val;
        let sz = Checked::from(std::cmp::max(size_of_val(amount1), size_of_val(amount2)));
        (40 + (sz >> 1)).as_gas_cost()
    }

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

    pub fn dig(n: u16) -> Result<u32, OutOfGas> {
        let n = Checked::from(n as u32);
        (30 + 6 * n + (n >> 1) + (n >> 2)).as_gas_cost()
    }

    pub fn dug(n: u16) -> Result<u32, OutOfGas> {
        let n = Checked::from(n as u32);
        (35 + 6 * n + (n >> 1) + (n >> 2)).as_gas_cost()
    }

    pub fn dup(mb_n: Option<u16>) -> Result<u32, OutOfGas> {
        mb_n.map_or(Ok(DUP), dupn)
    }

    pub fn add_num(i1: &impl BigIntByteSize, i2: &impl BigIntByteSize) -> Result<u32, OutOfGas> {
        // max is copied from the Tezos protocol, ostensibly adding two big ints depends on
        // the larger of the two due to result allocation
        let sz = Checked::from(std::cmp::max(i1.byte_size(), i2.byte_size()));
        (35 + (sz >> 1)).as_gas_cost()
    }

    pub fn sub_num(i1: &impl BigIntByteSize, i2: &impl BigIntByteSize) -> Result<u32, OutOfGas> {
        let sz = Checked::from(std::cmp::max(i1.byte_size(), i2.byte_size()));
        (35 + (sz >> 1)).as_gas_cost()
    }

    /// Cost for `AND` on numbers and bytearrays
    pub fn and_num(i1: &impl BigIntByteSize, i2: &impl BigIntByteSize) -> Result<u32, OutOfGas> {
        let sz = Checked::from(Ord::min(i1.byte_size(), i2.byte_size()));
        (35 + (sz >> 1)).as_gas_cost()
    }

    pub fn and_bytes(b1: &[u8], b2: &[u8]) -> Result<u32, OutOfGas> {
        let sz = Checked::from(Ord::min(b1.len(), b2.len()));
        (35 + (sz >> 1)).as_gas_cost()
    }

    pub fn or_num(i1: &impl BigIntByteSize, i2: &impl BigIntByteSize) -> Result<u32, OutOfGas> {
        let sz = Checked::from(Ord::min(i1.byte_size(), i2.byte_size()));
        (35 + (sz >> 1)).as_gas_cost()
    }

    pub fn or_bytes(b1: &[u8], b2: &[u8]) -> Result<u32, OutOfGas> {
        // NB: Tezos takes maximum of the sizes, but in our implementation only
        // touches bytes in two vectors intersection. So taking the same formula
        // as in [and_bytes].
        let sz = Checked::from(Ord::min(b1.len(), b2.len()));
        (35 + (sz >> 1)).as_gas_cost()
    }

    pub fn xor_nat(i1: &BigUint, i2: &BigUint) -> Result<u32, OutOfGas> {
        let sz = Checked::from(Ord::min(i1.byte_size(), i2.byte_size()));
        (35 + (sz >> 1)).as_gas_cost()
    }

    pub fn xor_bytes(b1: &[u8], b2: &[u8]) -> Result<u32, OutOfGas> {
        let sz = Checked::from(Ord::min(b1.len(), b2.len()));
        (40 + (sz >> 1)).as_gas_cost()
    }

    pub fn not_num<T: BigIntByteSize>(n: &T) -> Result<u32, OutOfGas> {
        let sz = Checked::from(n.byte_size());
        (25 + (sz >> 1)).as_gas_cost()
    }

    pub fn not_bytes(b: &[u8]) -> Result<u32, OutOfGas> {
        let sz = Checked::from(b.len());
        (30 + (sz >> 1)).as_gas_cost()
    }

    pub fn lsl_nat(i1: &impl BigIntByteSize) -> Result<u32, OutOfGas> {
        let sz = i1.byte_size();
        let w1 = sz >> 1;
        Checked::from(w1 + 130).as_gas_cost()
    }

    pub fn lsl_bytes(i1: &[u8], i2: &usize) -> Result<u32, OutOfGas> {
        let size_1 = i1.len();
        let size_2 = *i2;
        let w1 = if size_2 > 0 {
            ((size_2 - 1) >> 4) + (size_1 >> 1)
        } else {
            size_1 >> 1
        };
        Checked::from(w1 + (size_1 >> 2) + 65).as_gas_cost()
    }

    pub fn lsr_nat(i1: &impl BigIntByteSize) -> Result<u32, OutOfGas> {
        let sz = i1.byte_size();
        Checked::from((sz >> 1) + 45).as_gas_cost()
    }

    pub fn lsr_bytes(i1: &[u8], i2: &usize) -> Result<u32, OutOfGas> {
        let size_1 = i1.len();
        let size_2 = *i2;
        let w1 = if size_1 >= (size_2 >> 3) {
            Checked::from(size_1 - (size_2 >> 3))
        } else {
            Checked::from(0usize)
        };
        ((w1 >> 1) + (w1 >> 2) + 55).as_gas_cost()
    }

    pub fn mul_int(i1: &impl BigIntByteSize, i2: &impl BigIntByteSize) -> Result<u32, OutOfGas> {
        let a = Checked::from(i1.byte_size()) + Checked::from(i2.byte_size());
        // log2 is ill-defined for zero, hence this check
        let v0 = if a.is_zero() {
            Checked::from(0)
        } else {
            a * (a.ok_or(OutOfGas)?.log2i() as u64)
        };
        (55 + (v0 >> 1) + (v0 >> 2) + (v0 >> 4)).as_gas_cost()
    }

    pub fn ediv_int(i1: &impl BigIntByteSize, i2: &impl BigIntByteSize) -> Result<u32, OutOfGas> {
        let size_1 = Checked::from(i1.byte_size());
        let size_2 = Checked::from(i2.byte_size());
        let w1 = if size_1 >= size_2 {
            size_1 - size_2
        } else {
            Checked::from(0u64)
        };
        ((w1 * 12) + (((w1 >> 10) + (w1 >> 13)) * size_2) + (size_1 >> 2) + size_1 + 150)
            .as_gas_cost()
    }

    pub fn ediv_nat(i1: &impl BigIntByteSize, i2: &impl BigIntByteSize) -> Result<u32, OutOfGas> {
        let size_1 = Checked::from(i1.byte_size());
        let size_2 = Checked::from(i2.byte_size());
        let w1 = if size_1 >= size_2 {
            size_1 - size_2
        } else {
            Checked::from(0u64)
        };
        ((w1 * 12) + (((w1 >> 10) + (w1 >> 13)) * size_2) + (size_1 >> 2) + size_1 + 150)
            .as_gas_cost()
    }

    pub fn compare(v1: &TypedValue, v2: &TypedValue) -> Result<u32, OutOfGas> {
        use TypedValue as V;
        let cmp_bytes = |s1: u64, s2: u64| {
            // Approximating 35 + 0.024413 x term
            let v = Checked::from(std::cmp::min(s1, s2));
            (35 + (v >> 6) + (v >> 7)).as_gas_cost()
        };
        let cmp_pair = |l1: &Rc<TypedValue>,
                        l2: &Rc<TypedValue>,
                        r1: &Rc<TypedValue>,
                        r2: &Rc<TypedValue>| {
            let c = Checked::from(10u32);
            (c + compare(l1.as_ref(), r1.as_ref())? + compare(l2.as_ref(), r2.as_ref())?)
                .as_gas_cost()
        };
        let cmp_option = Checked::from(10u32);
        const ADDRESS_SIZE: u64 = 20 + 31; // hash size + max entrypoint size
        const CMP_CHAIN_ID: u32 = 30;
        const CMP_KEY: u32 = 92; // hard-coded in the protocol
        const CMP_SIGNATURE: u32 = 92; // hard-coded in the protocol
        let cmp_or = Checked::from(10u32);
        #[track_caller]
        fn incomparable() -> ! {
            unreachable!("Comparison of incomparable values")
        }
        Ok(match (v1, v2) {
            (V::Nat(l), V::Nat(r)) => cmp_bytes(l.byte_size(), r.byte_size())?,
            (V::Nat(_), _) => incomparable(),

            (V::Int(l), V::Int(r)) => cmp_bytes(l.byte_size(), r.byte_size())?,
            (V::Int(_), _) => incomparable(),

            (V::Timestamp(l), V::Timestamp(r)) => cmp_bytes(l.byte_size(), r.byte_size())?,
            (V::Timestamp(_), _) => incomparable(),

            (V::Bool(_), V::Bool(_)) => cmp_bytes(1, 1)?,
            (V::Bool(_), _) => incomparable(),

            (V::Mutez(_), V::Mutez(_)) => cmp_bytes(8, 8)?,
            (V::Mutez(_), _) => incomparable(),

            (V::String(l), V::String(r)) => cmp_bytes(l.len() as u64, r.len() as u64)?,
            (V::String(_), _) => incomparable(),

            (V::Unit, V::Unit) => 10,
            (V::Unit, _) => incomparable(),

            (V::Pair(l1, l2), V::Pair(r1, r2)) => cmp_pair(l1, l2, r1, r2)?,
            (V::Pair(..), _) => incomparable(),

            (V::Option(l), V::Option(r)) => match (l, r) {
                (None, None) => cmp_option,
                (None, Some(_)) => cmp_option,
                (Some(_), None) => cmp_option,
                (Some(l), Some(r)) => cmp_option + compare(l.as_ref(), r.as_ref())?,
            }
            .as_gas_cost()?,
            (V::Option(_), _) => incomparable(),

            (V::Address(..), V::Address(..)) => cmp_bytes(ADDRESS_SIZE, ADDRESS_SIZE)?,
            (V::Address(_), _) => incomparable(),

            (V::ChainId(..), V::ChainId(..)) => CMP_CHAIN_ID,
            (V::ChainId(_), _) => incomparable(),

            (V::Bytes(l), V::Bytes(r)) => cmp_bytes(l.len() as u64, r.len() as u64)?,
            (V::Bytes(_), _) => incomparable(),

            (V::Key(_), V::Key(_)) => CMP_KEY,
            (V::Key(_), _) => incomparable(),

            (V::Signature(_), V::Signature(_)) => CMP_SIGNATURE,
            (V::Signature(_), _) => incomparable(),

            (V::KeyHash(_), V::KeyHash(_)) => cmp_bytes(20u64, 20u64)?,
            (V::KeyHash(_), _) => incomparable(),

            (V::Or(l), V::Or(r)) => match (l, r) {
                (Or::Left(x), Or::Left(y)) => cmp_or + compare(x.as_ref(), y.as_ref())?,
                (Or::Right(x), Or::Right(y)) => cmp_or + compare(x.as_ref(), y.as_ref())?,
                (Or::Left(_), Or::Right(_)) => cmp_or,
                (Or::Right(_), Or::Left(_)) => cmp_or,
            }
            .as_gas_cost()?,
            (V::Or(..), _) => incomparable(),

            #[cfg(feature = "bls")]
            (V::Bls12381Fr(_) | V::Bls12381G1(_) | V::Bls12381G2(_), _) => incomparable(),

            (
                V::List(..)
                | V::Set(..)
                | V::Map(..)
                | V::BigMap(..)
                | V::Contract(_)
                | V::Operation(_)
                | V::Ticket(_)
                | V::Lambda(_),
                _,
            ) => incomparable(),
        })
    }

    /// Cost charged for computing total entries size (needed for the subsequent
    /// gas calculation, so this is meta-gas).
    pub fn concat_list_precheck(list_size: usize) -> Result<u32, OutOfGas> {
        (10 * Checked::from(list_size)).as_gas_cost()
    }

    pub fn concat_string_list(total_len: Checked<usize>) -> Result<u32, OutOfGas> {
        // Copied from the Tezos protocol
        (total_len / 2 + 100).as_gas_cost()
    }

    pub fn concat_bytes_list(total_len: Checked<usize>) -> Result<u32, OutOfGas> {
        // Copied from the Tezos protocol
        (total_len / 2 + 100).as_gas_cost()
    }

    pub fn concat_string_pair(len1: usize, len2: usize) -> Result<u32, OutOfGas> {
        // Copied from the Tezos protocol
        ((Checked::from(len1) + Checked::from(len2)) / 2 + 45).as_gas_cost()
    }

    pub fn concat_bytes_pair(len1: usize, len2: usize) -> Result<u32, OutOfGas> {
        // Copied from the Tezos protocol
        ((Checked::from(len1) + Checked::from(len2)) / 2 + 45).as_gas_cost()
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
        let map_size = Checked::from(map_size);
        let compare_cost = compare(k, k)?;
        let size_log = (map_size + 1).ok_or(OutOfGas)?.log2i();
        let lookup_cost = Checked::from(compare_cost) * size_log;
        (80 + lookup_cost).as_gas_cost()
    }

    pub fn set_mem(k: &TypedValue, map_size: usize) -> Result<u32, OutOfGas> {
        // NB: same considerations as for map_get
        let compare_cost = compare(k, k)?;
        let size_log = (Checked::from(map_size) + 1).ok_or(OutOfGas)?.log2i();
        let lookup_cost = Checked::from(compare_cost) * size_log;
        (115 + lookup_cost).as_gas_cost()
    }

    pub fn map_update(k: &TypedValue, map_size: usize) -> Result<u32, OutOfGas> {
        // NB: same considerations as for map_get
        let map_size = Checked::from(map_size);
        let compare_cost = compare(k, k)?;
        let size_log = (map_size + 1).ok_or(OutOfGas)?.log2i();
        let lookup_cost = Checked::from(compare_cost) * size_log;
        // NB: 2 factor copied from Tezos protocol, in principle it should
        // reflect update vs get overhead.
        (80 + 2 * lookup_cost).as_gas_cost()
    }

    pub fn set_update(k: &TypedValue, map_size: usize) -> Result<u32, OutOfGas> {
        // NB: same considerations as for map_update
        let compare_cost = compare(k, k)?;
        let size_log = (Checked::from(map_size) + 1).ok_or(OutOfGas)?.log2i();
        let lookup_cost = Checked::from(compare_cost) * size_log;
        // coefficient larger than in case of Map looks suspicious, something
        // to benchmark later
        (130 + 2 * lookup_cost).as_gas_cost()
    }

    pub fn map_get_and_update(k: &TypedValue, map_size: usize) -> Result<u32, OutOfGas> {
        // NB: same considerations as for map_get
        let compare_cost = compare(k, k)?;
        let size_log = (Checked::from(map_size) + 1).ok_or(OutOfGas)?.log2i();
        let lookup_cost = Checked::from(compare_cost) * size_log;
        // NB: 3 factor copied from Tezos protocol, in principle it should
        // reflect update vs get overhead, but it seems like an overestimation,
        // get_and_update should cost almost exactly the same as update, any
        // observable difference would be in the constant term.
        //
        // However, note that this function is also reused for big_map version
        // of GET_AND_UPDATE, wherein it's more justified. That is to say, take
        // care when updating this.
        (80 + 3 * lookup_cost).as_gas_cost()
    }

    /// Measures size of Michelson using several metrics.
    pub struct MichelineSize {
        /// Total number of nodes (including leaves).
        nodes_num: Checked<u64>,

        /// Total size of string and bytes literals.
        str_byte: Checked<u64>,

        /// Total size of zarith numbers, in bytes.
        zariths: Checked<u64>,
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
            Micheline::String(s) => size.str_byte += s.len() as u64,
            Micheline::Bytes(bs) => size.str_byte += bs.len() as u64,
            Micheline::Int(i) => size.zariths += i.byte_size(),
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
                    use crate::ast::Annotation as Ann;
                    size.str_byte += match annot {
                        // Including annotation prefix into the size too
                        Ann::Field(a) => a.len() + 1,
                        Ann::Variable(a) => a.len() + 1,
                        Ann::Type(a) => a.len() + 1,
                        Ann::Special(a) => a.len(),
                    } as u64
                }
            }
        }
    }

    #[derive(Debug, Error)]
    pub enum SigCostError {
        #[error(transparent)]
        OutOfGas(#[from] OutOfGas),
        #[error(transparent)]
        Crypto(#[from] CryptoError),
    }

    pub fn check_signature(k: &PublicKey, msg: &[u8]) -> Result<u32, CryptoError> {
        let len = msg.len().min(u32::MAX as usize) as u32;
        let serialization_cost = len << 5;
        let checked_cost = match k {
            PublicKey::Ed25519(..) => 65_800 + ((len >> 3) + len),
            PublicKey::Secp256k1(..) => 51_600 + ((len >> 3) + len),
            PublicKey::P256(..) => 341_000 + ((len >> 3) + len),
            #[cfg(feature = "bls")]
            PublicKey::Bls(..) => 1_570_000 + (len * 3),
            #[cfg(not(feature = "bls"))]
            PublicKey::Bls(..) => {
                return Err(CryptoError::Unsupported(
                    "bls feature disabled, tz4 signature verification not supported",
                ))
            }
        };

        Ok(serialization_cost + checked_cost)
    }

    pub fn slice(length: usize) -> Result<u32, OutOfGas> {
        // In the protocol, the gas costs for slicing strings and bytes are defined
        // separately (see `cost_N_ISlice_bytes` and `cost_N_ISlice_string`).
        //
        // In practice, they both have the same cost.
        ((Checked::from(length) >> 1) + 25).as_gas_cost()
    }

    pub fn blake2b(msg: &[u8]) -> Result<u32, OutOfGas> {
        /* fun size -> (430. + (1.125 * size)) */
        let size = Checked::from(msg.len());
        (Checked::from(430) + ((size >> 3) + size)).as_gas_cost()
    }

    pub fn keccak(msg: &[u8]) -> Result<u32, OutOfGas> {
        /* fun size -> (1350. + (8.25 * size)) */
        let size = Checked::from(msg.len());
        (Checked::from(1350) + ((size >> 2) + (size * 8))).as_gas_cost()
    }

    pub fn sha256(msg: &[u8]) -> Result<u32, OutOfGas> {
        /* fun size -> (600. + (4.75 * size)) */
        let size = Checked::from(msg.len());
        (Checked::from(600) + ((size >> 2) + ((size >> 1) + (size * 4)))).as_gas_cost()
    }

    pub fn sha3(msg: &[u8]) -> Result<u32, OutOfGas> {
        /* fun size -> (1350. + (8.25 * size)) */
        let size = Checked::from(msg.len());
        (Checked::from(1350) + ((size >> 2) + (size * 8))).as_gas_cost()
    }

    pub fn sha512(msg: &[u8]) -> Result<u32, OutOfGas> {
        /* fun size -> (680. + (3. * size)) */
        let size = Checked::from(msg.len());
        (Checked::from(680) + (size * 3)).as_gas_cost()
    }

    pub fn pairing_check(size: usize) -> Result<u32, OutOfGas> {
        (450_000 + 342_500 * Checked::from(size)).as_gas_cost()
    }

    pub fn mul_bls_fr_big_int(int: &impl BigIntByteSize) -> Result<u32, OutOfGas> {
        // 265. + 1.0625 * size
        // NB: cost_N_IMul_bls12_381_fr_z and
        // cost_N_IMul_bls12_381_z_fr ar distinct in the protocol, but they're the
        // same exact operation, so we opted to use one function.
        let size = Checked::from(int.byte_size());
        (265 + ((size >> 4) + size)).as_gas_cost()
    }

    pub fn neg_int(int: &impl BigIntByteSize) -> Result<u32, OutOfGas> {
        // NB: taken from the protocol, this doesn't fit with MIR implementation.
        let size = Checked::from(int.byte_size());
        (25 + (size >> 1)).as_gas_cost()
    }

    pub fn abs(int: &impl BigIntByteSize) -> Result<u32, OutOfGas> {
        // NB: MIR implementation is constant-time and alloc free so could have
        // a constant gas cost but for consistency with the protocol we use the
        // same linear model.
        let size = Checked::from(int.byte_size());
        (20 + (size >> 1)).as_gas_cost()
    }

    pub fn int_bytes(size: usize) -> Result<u32, OutOfGas> {
        let size = Checked::from(size);
        (20 + ((size >> 1) + (size * 2))).as_gas_cost()
    }

    pub fn nat_bytes(size: usize) -> Result<u32, OutOfGas> {
        let size = Checked::from(size);
        (45 + ((size >> 1) + (size * 2))).as_gas_cost()
    }

    pub fn bytes_int(int: &BigInt) -> Result<u32, OutOfGas> {
        let size = Checked::from(int.byte_size());
        (90 + (size * 3)).as_gas_cost()
    }

    pub fn bytes_nat(int: &BigUint) -> Result<u32, OutOfGas> {
        let size = Checked::from(int.byte_size());
        (75 + (size * 3)).as_gas_cost()
    }

    pub fn unpack(bytes: &[u8]) -> Result<u32, OutOfGas> {
        let size = Checked::from(bytes.len());
        (260 + (size >> 1)).as_gas_cost()
    }

    pub fn pair_n(size: usize) -> Result<u32, OutOfGas> {
        let size = Checked::from(size);
        let v0 = size - 2;
        (40 + ((v0 >> 2) + (v0 * 3))).as_gas_cost()
    }

    pub fn unpair_n(size: usize) -> Result<u32, OutOfGas> {
        let size = Checked::from(size);
        let v0 = size - 2;
        (30 + (v0 * 4)).as_gas_cost()
    }

    pub fn get_n(size: usize) -> Result<u32, OutOfGas> {
        let size = Checked::from(size);
        (20 + ((size >> 1) + (size >> 4))).as_gas_cost()
    }

    pub fn update_n(size: usize) -> Result<u32, OutOfGas> {
        let size = Checked::from(size);
        (30 + ((size >> 5) + ((size >> 2) + size))).as_gas_cost()
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
        assert_eq!(1usize.log2i(), 0);
        assert_eq!(2usize.log2i(), 1);
        assert_eq!(3usize.log2i(), 2);
        assert_eq!(4usize.log2i(), 2);
        assert_eq!(5usize.log2i(), 3);
        assert_eq!(70_000usize.log2i(), 17);
        assert_eq!(100_000usize.log2i(), 17);
        assert_eq!(300_000usize.log2i(), 19);

        assert_eq!(1u64.log2i(), 0);
        assert_eq!(2u64.log2i(), 1);
        assert_eq!(3u64.log2i(), 2);
        assert_eq!(4u64.log2i(), 2);
        assert_eq!(5u64.log2i(), 3);
        assert_eq!(70_000u64.log2i(), 17);
        assert_eq!(100_000u64.log2i(), 17);
        assert_eq!(300_000u64.log2i(), 19);
    }

    #[test]
    #[should_panic(expected = "assertion failed: self != 0")]
    fn log2i_test_panic_usize() {
        0usize.log2i();
    }

    #[test]
    #[should_panic(expected = "assertion failed: self != 0")]
    fn log2i_test_panic_u64() {
        0u64.log2i();
    }
}
