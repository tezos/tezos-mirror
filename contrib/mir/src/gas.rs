// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! Gas accounting and costs.

use num_bigint::{BigInt, BigUint};
use tezos_data_encoding::enc::{BinError, BinWriter};

/// Structure carrying the remaining gas amount.
#[derive(Debug)]
pub struct Gas {
    milligas_amount: Option<u32>,
}

/// Gas-budget exhaustion. Produced **only** by [`Gas::consume`] when the
/// caller's remaining budget is insufficient. Arithmetic-overflow failures
/// during cost computation use [`CostOverflow`] instead — `OutOfGas` is not
/// an arithmetic error.
#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
#[error("Gas_exhaustion")]
pub struct OutOfGas;

impl BinWriter for OutOfGas {
    fn bin_write(&self, _: &mut Vec<u8>) -> Result<(), BinError> {
        Ok(())
    }
}

/// Arithmetic overflow during cost computation. Produced by
/// [`AsGasCost::as_gas_cost`] and [`Log2i::log2i`] when a cost-formula
/// intermediate exceeds `u32` or an input falls outside the helper's
/// domain (e.g. `log2i(0)`, `log2i(usize::MAX)`). Distinct from
/// [`OutOfGas`]: the caller's budget was not exhausted; the cost itself
/// is unrepresentable.
#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
#[error("arithmetic overflow in cost computation")]
pub struct CostOverflow;

/// Error when computing comparison cost.
#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum CompareError {
    /// Cost arithmetic overflowed while sizing the comparison. The
    /// cost-helper functions in [`tc_cost`] / [`interpret_cost`] do not
    /// charge gas (that happens at the call site via [`Gas::consume`]),
    /// they only do cost arithmetic, so [`CostOverflow`] is their only
    /// failure mode.
    #[error(transparent)]
    Cost(#[from] CostOverflow),
    /// Attempted to compare incomparable values.
    #[error("comparison of incomparable values")]
    Incomparable,
}

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

    /// [Gas] counter sized to `u32::MAX` milligas: bounded, but high
    /// enough that we don't expect to hit it in practice. Use as the
    /// `&mut Gas` argument in places where no metering should apply,
    /// typically tests, examples, and the TZT runner.
    ///
    /// For production paths that are not user-billed (kernel-synthesised
    /// receipts, RPC handlers, etc.), prefer [`Gas::default`] over
    /// [`Gas::unmetered`]: the standard L1 per-operation cap acts as a
    /// real OOG safety bound on absurdly large inputs, instead of just
    /// being a `u32::MAX` placeholder.
    pub fn unmetered() -> Gas {
        Gas::new(u32::MAX)
    }

    /// Try to consume the specified milligas `cost`. If not enough gas left,
    /// or gas was previously exhausted, return [OutOfGas] and mark gas as
    /// exhausted.
    pub fn consume(&mut self, cost: u32) -> Result<(), OutOfGas> {
        self.milligas_amount = self.milligas_amount.and_then(|m| m.checked_sub(cost));
        if self.milligas_amount.is_none() {
            Err(OutOfGas)
        } else {
            Ok(())
        }
    }

    /// Get the remaining milligas amount, or `None` if gas was exhausted.
    pub fn milligas(&self) -> Option<u32> {
        self.milligas_amount
    }
}

trait AsGasCost {
    /// Try to convert a checked numeric type to gas cost; return
    /// [`CostOverflow`] on overflow. This is pure cost arithmetic — it
    /// never returns `OutOfGas` (the budget is not consulted here).
    fn as_gas_cost(&self) -> Result<u32, CostOverflow>;
}

impl AsGasCost for checked::Checked<u32> {
    fn as_gas_cost(&self) -> Result<u32, CostOverflow> {
        self.ok_or(CostOverflow)
    }
}

impl AsGasCost for checked::Checked<usize> {
    fn as_gas_cost(&self) -> Result<u32, CostOverflow> {
        self.ok_or(CostOverflow)?
            .try_into()
            .map_err(|_| CostOverflow)
    }
}

impl AsGasCost for checked::Checked<u64> {
    fn as_gas_cost(&self) -> Result<u32, CostOverflow> {
        self.ok_or(CostOverflow)?
            .try_into()
            .map_err(|_| CostOverflow)
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
    /// `log2i(0)` is out of domain, as are inputs whose next power of two
    /// would overflow the integer type (e.g. `usize::MAX`). Both return
    /// [`CostOverflow`] rather than panic or wrap silently. This is a
    /// cost-arithmetic helper, so it never returns `OutOfGas`.
    fn log2i(self) -> Result<u32, CostOverflow>;
}

impl Log2i for usize {
    fn log2i(self) -> Result<u32, CostOverflow> {
        if self == 0 {
            Err(CostOverflow)
        } else {
            Ok(self
                .checked_next_power_of_two()
                .ok_or(CostOverflow)?
                .trailing_zeros())
        }
    }
}

impl Log2i for u64 {
    fn log2i(self) -> Result<u32, CostOverflow> {
        if self == 0 {
            Err(CostOverflow)
        } else {
            Ok(self
                .checked_next_power_of_two()
                .ok_or(CostOverflow)?
                .trailing_zeros())
        }
    }
}

/// Typechecking gas costs.
#[allow(missing_docs)]
pub mod tc_cost {
    use checked::Checked;

    use super::{AsGasCost, CostOverflow, Log2i};

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

    pub fn timestamp_decoding(l: usize) -> Result<u32, CostOverflow> {
        use integer_sqrt::IntegerSquareRoot;
        let v0: Checked<usize> = Checked::from(l.integer_sqrt()) * l;
        (105 + ((v0 >> 5) + (v0 >> 6))).as_gas_cost()
    }

    fn variadic(depth: u16) -> Result<u32, CostOverflow> {
        let depth = Checked::from(depth as u32);
        (depth * 50).as_gas_cost()
    }

    pub fn dig_n(depth: usize) -> Result<u32, CostOverflow> {
        // corresponds to Cost_of.Typechecking.proof_argument in the protocol
        (Checked::from(depth) * 50).as_gas_cost()
    }

    pub fn dug_n(depth: usize) -> Result<u32, CostOverflow> {
        // corresponds to Cost_of.Typechecking.proof_argument in the protocol
        (Checked::from(depth) * 50).as_gas_cost()
    }

    pub fn drop_n(depth: &Option<u16>) -> Result<u32, CostOverflow> {
        depth.map_or(Ok(0), variadic)
    }

    pub fn dip_n(depth: &Option<u16>) -> Result<u32, CostOverflow> {
        depth.map_or(Ok(0), variadic)
    }

    pub fn ty_eq(sz1: usize, sz2: usize) -> Result<u32, CostOverflow> {
        // complexity of comparing types T and U is O(min(|T|, |U|)), as
        // comparison short-circuits at the first mismatch
        let sz = Checked::from(std::cmp::min(sz1, sz2));
        (sz * 60).as_gas_cost()
    }

    pub fn construct_map(key_size: usize, sz: usize) -> Result<u32, CostOverflow> {
        // Tezos protocol constructs maps element by element, thus the cost ends
        // up Σ (80 + key_size*log2(i)) = 80 * n + key_size * Σ log2(i) = 80 * n
        // + key_size * log2(Π i) = 80 * n + key_size * log2(n!)
        // Using n * log2(n) as an approximation for log2(n!),
        // ≈ 80 * n + key_size * n * log2(n)
        // which seems like a reasonable first-order approximation.
        // to avoid log2(0) it's more practical to compute log2(n + 1)
        let n = Checked::from(sz);
        let key_size = Checked::from(key_size);
        let log2n = (n + 1).ok_or(CostOverflow)?.log2i()? as usize;
        (80 * n + key_size * n * log2n).as_gas_cost()
    }

    pub fn construct_set(val_size: usize, sz: usize) -> Result<u32, CostOverflow> {
        // Similar to `construct_map`, only the coefficient differs
        let n = Checked::from(sz);
        let key_size = Checked::from(val_size);
        let log2n = (n + 1).ok_or(CostOverflow)?.log2i()? as usize;
        (130 * n + key_size * n * log2n).as_gas_cost()
    }

    pub fn pair_n(size: usize) -> Result<u32, CostOverflow> {
        // corresponds to Cost_of.Typechecking.proof_argument in the protocol
        (Checked::from(size) * 50).as_gas_cost()
    }

    pub fn unpair_n(size: usize) -> Result<u32, CostOverflow> {
        // corresponds to Cost_of.Typechecking.proof_argument in the protocol
        (Checked::from(size) * 50).as_gas_cost()
    }

    pub fn get_n(size: usize) -> Result<u32, CostOverflow> {
        // corresponds to Cost_of.Typechecking.proof_argument in the protocol
        (Checked::from(size) * 50).as_gas_cost()
    }

    pub fn update_n(size: usize) -> Result<u32, CostOverflow> {
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
    use tezos_crypto_rs::public_key::PublicKey;
    use tezos_crypto_rs::CryptoError;
    use thiserror::Error;

    use super::{AsGasCost, BigIntByteSize, CompareError, CostOverflow, Log2i, OutOfGas};
    use crate::ast::{Micheline, Or, Ticket, TypedValue};

    pub const DIP: u32 = 10;
    // Re-benchmarked on the MIR interpreter (`cost_N_IDrop`).
    pub const DROP: u32 = 20;
    // Re-benchmarked on the MIR interpreter (`cost_N_IDup`).
    pub const DUP: u32 = 45;
    // correspond to cost_N_IGt / IGe / IEq / INeq / ILe / ILt in the Tezos
    // protocol.
    // TODO(L2-1554): these benchmarked constants (~1.1-1.2 us flat) look like
    // measurement artefacts: each instruction pops an int and tests its sign.
    // Same family as N_IDupN / N_ICons_list / N_IList_size; re-check the
    // micro-benchmarks.
    pub const GT: u32 = 1130;
    pub const GE: u32 = 1145;
    pub const EQ: u32 = 1115;
    pub const NEQ: u32 = 1185;
    pub const LE: u32 = 1210;
    pub const LT: u32 = 1150;
    pub const IF: u32 = 10;
    pub const IF_NONE: u32 = 10;
    pub const IF_CONS: u32 = 855;
    pub const IF_LEFT: u32 = 10;
    pub const LOOP: u32 = 10;
    // Iteration cost (`cost_N_IList_iter` / `cost_N_ISet_iter` /
    // `cost_N_IMap_iter`). L1's `set`/`map` variants are size-dependent
    // because L1 first materialises the collection into a list; MIR iterates
    // the entries directly (no such setup), so a flat cost matches MIR's work.
    pub const ITER: u32 = 20;
    pub const SWAP: u32 = 10;
    // corresponds to cost_N_IInt_nat in the Tezos protocol
    pub const INT_NAT: u32 = 3630;
    // corresponds to cost_N_IIs_nat in the Tezos protocol
    pub const ISNAT: u32 = 2465;
    pub const INT_BLS_FR: u32 = 115;
    // Re-benchmarked on the MIR interpreter (`cost_N_IPush`).
    pub const PUSH: u32 = 60;
    pub const ADD_TEZ: u32 = 45;
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
    pub const SUB_MUTEZ: u32 = 55;
    // Re-benchmarked on the MIR interpreter (`cost_N_IUnit`).
    pub const UNIT: u32 = 55;
    // corresponds to cost_N_IAnd in the Tezos protocol
    pub const AND_BOOL: u32 = 45;
    // corresponds to cost_N_IOr in the Tezos protocol
    pub const OR_BOOL: u32 = 45;
    // corresponds to cost_N_IXor in the Tezos protocol
    pub const XOR_BOOL: u32 = 45;
    // corresponds to cost_N_INot in the Tezos protocol
    pub const NOT_BOOL: u32 = 40;
    pub const CAR: u32 = 15;
    pub const CDR: u32 = 20;
    pub const PAIR: u32 = 10;
    pub const UNPAIR: u32 = 60;
    pub const SOME: u32 = 10;
    pub const NONE: u32 = 10;
    pub const AMOUNT: u32 = 65;
    pub const NIL: u32 = 60;
    pub const CONS: u32 = 15;
    pub const EMPTY_SET: u32 = 60;
    pub const SIZE_STRING: u32 = 15;
    pub const SIZE_BYTES: u32 = 10;
    // corresponds to cost_N_IList_size in the Tezos protocol
    // TODO(L2-1547): the benchmarked constant (~920 ns) looks like a
    // measurement artefact: SIZE on a list is Vec::len(), O(1). Same family
    // as the ~840 ns constants (N_IDupN, N_ICons_list, N_IIf_cons); re-check
    // the micro-benchmarks.
    pub const SIZE_LIST: u32 = 920;
    // corresponds to cost_N_ISet_size in the Tezos protocol
    // TODO(L2-1547): ~110 ns for BTreeSet::len() also looks too high;
    // re-check the micro-benchmarks.
    pub const SIZE_SET: u32 = 110;
    // corresponds to cost_N_IMap_size in the Tezos protocol
    pub const SIZE_MAP: u32 = 45;
    pub const EMPTY_MAP: u32 = 60;
    pub const EMPTY_BIG_MAP: u32 = 60;
    pub const CHAIN_ID: u32 = 65;
    pub const PACK: u32 = 0;
    pub const SELF: u32 = 10;
    pub const ADDRESS: u32 = 60;
    pub const LEFT: u32 = 20;
    pub const RIGHT: u32 = 20;

    // corresponds to cost_N_IOpt_map_some in the Tezos protocol
    pub const MAP_OPTION_SOME: u32 = 10;
    // corresponds to cost_N_IOpt_map_none in the Tezos protocol
    pub const MAP_OPTION_NONE: u32 = 35;
    // See `cost_N_IList_map` in the Tezos protocol
    pub const MAP_LIST: u32 = 20;
    // L1's `cost_N_IMap_map` is size-dependent because it folds the `map` into
    // a list of key-value pairs first. MIR iterates the map's entries directly
    // (no such setup), so a flat cost matches MIR's actual work.
    pub const MAP_MAP: u32 = 20;

    // Gas costs obtained from https://gitlab.com/tezos/tezos/-/blob/9875fbebe032a8c5ce62b3b3cb1588ca9855a37e/src/proto_017_PtNairob/lib_protocol/michelson_v1_gas_costs_generated.ml
    pub const TRANSFER_TOKENS: u32 = 120;
    pub const SET_DELEGATE: u32 = 75;
    pub const LAMBDA: u32 = 10;
    pub const EXEC: u32 = 10;
    pub const HASH_KEY: u32 = 605;
    // slight deviation from the protocol: in the protocol, APPLY costs differer
    // depending on whether a lambda is recursive; here this distinction doesn't
    // make a lot of sense.
    pub const APPLY: u32 = 140;
    pub const TICKET: u32 = 10;
    pub const READ_TICKET: u32 = 10;
    pub const BALANCE: u32 = 60;
    pub const CONTRACT: u32 = 115;
    pub const LEVEL: u32 = 85;
    pub const MIN_BLOCK_TIME: u32 = 60;
    pub const SELF_ADDRESS: u32 = 70;
    pub const SENDER: u32 = 75;
    pub const SOURCE: u32 = 70;
    pub const NOW: u32 = 75;
    pub const IMPLICIT_ACCOUNT: u32 = 65;
    pub const IS_IMPLICIT_ACCOUNT: u32 = 65;
    pub const VOTING_POWER: u32 = 45;
    pub const TOTAL_VOTING_POWER: u32 = 75;
    pub const EMIT: u32 = 75;

    pub const INTERPRET_RET: u32 = 130; // corresponds to cost_N_IHalt in the Tezos protocol: charged at the end of every code block, like L1's terminal IHalt

    /// Per-frame charge deducted at every worklist-growth push onto
    /// `frames: Vec<InterpFrame>`. Layered *on top of* L1's existing
    /// per-continuation costs (`cost_N_KCons = cost_N_KLoop_in =
    /// cost_N_KIter = cost_N_KReturn = 10` milligas in
    /// `src/proto_alpha/lib_protocol/michelson_v1_gas_costs_generated.ml`,
    /// consumed once per continuation step in L1's `next`), which MIR
    /// already mirrors as [`INTERPRET_RET`] (`KNil`),
    /// [`LOOP_ENTER`] (`KLoop_in`), [`LOOP_LEFT_ENTER`]
    /// (`KLoop_in_left`), etc. The motivation for the surcharge is not
    /// continuation accounting — L1 has that — it is *host memory*:
    /// L1's recursive OCaml interpreter holds the continuation chain
    /// on the runtime stack under automatic tail-call elimination, so
    /// each pending frame costs ≈ 0 bytes; MIR's iterative driver
    /// allocates each frame on the heap (an `InterpFrame` enum
    /// variant plus `Vec` growth), so each pending frame costs O(frame
    /// size) of host RAM. The surcharge converts host-memory
    /// consumption into a gas-priced resource so a divergent
    /// `LAMBDA_REC int int { EXEC }` — the only Michelson shape that
    /// grows the worklist without bound at constant per-iteration gas
    /// cost — hits a clean `OutOfGas` before the worklist exhausts
    /// host memory and the kernel WASM module aborts.
    ///
    /// `IF`/`DIP` are bounded by source size; `LOOP`/`LOOP_LEFT`/
    /// `ITER`/`MAP` unwind their per-iteration bodies before the next
    /// iteration starts (so don't accumulate worklist depth);
    /// `VIEW` is single-shot. The charge fires at every
    /// `handle_step::Open*` arm uniformly even so, which is what
    /// makes the per-call gas profile diverge from L1's.
    ///
    /// Per-instruction net delta vs. L1 (i.e. ignoring per-iteration
    /// continuation pops both interpreters already charge for):
    ///   - `OpenBlock` (`IF*`): + `FRAME_PUSH` (no L1 equivalent for
    ///     the branch-arm push since L1 uses direct dispatch);
    ///   - `OpenDip`, `OpenMapList`, `OpenMapOption`, `OpenMapMap`:
    ///     + 2 × `FRAME_PUSH`;
    ///   - `OpenLoop*`, `OpenIter*`: + `FRAME_PUSH` − (existing
    ///     `LOOP_ENTER` / `ITER` charge MIR already paid);
    ///   - `OpenExec`, `OpenView`: + `STACK_PUSH + 2 × FRAME_PUSH` =
    ///     700 milligas, against L1's `cost_N_KReturn + cost_N_KCons`
    ///     ≈ 20 milligas equivalent — net per-iteration surcharge
    ///     of 680 milligas for a divergent recursive lambda.
    ///
    /// Observable on receipts and `eth_estimateGas` for code with
    /// deeply-nested control flow or large `ITER`/`MAP` collections.
    /// Tracked separately for tuning; see
    /// `contrib/mir/docs/frame_gas_calibration.md` (when present).
    pub const FRAME_PUSH: u32 = 100;

    /// Sibling charge for pushes onto the per-EXEC/per-VIEW substack
    /// (`stacks: Vec<IStack>`). Same layered-on-top-of-L1 framing as
    /// [`FRAME_PUSH`]: L1 has no analogue because its recursive OCaml
    /// interpreter does not materialize a fresh sub-stack per `Apply`
    /// invocation — the OCaml runtime stack absorbs the inner frame
    /// under TCE. MIR allocates a `Vec<Rc<TypedValue>>` per push
    /// (`Vec` header + initial allocation + initial entries: lambda
    /// + arg for `EXEC`, view arg for `VIEW`), so the host-memory
    /// footprint per push is ~5 × that of a bare [`FRAME_PUSH`];
    /// `STACK_PUSH` is sized accordingly. Pushed only by `OpenExec`
    /// / `OpenView` in `handle_step`, so on a divergent recursive
    /// lambda this fires once per iteration and is the single
    /// biggest source of bounded host-memory cost.
    pub const STACK_PUSH: u32 = 500;
    pub const LOOP_ENTER: u32 = 10; // corresponds to KLoop_in in the Tezos protocol
    pub const LOOP_LEFT_ENTER: u32 = 10; // corresponds to KLoop_in_left in the Tezos protocol
    pub const LOOP_EXIT: u32 = 15;
    pub const CREATE_CONTRACT: u32 = 60;
    pub const VIEW: u32 = 65; // corresponds to cost_N_IView_synthesized in the Tezos protocol

    pub fn join_tickets(t1: &Ticket, t2: &Ticket) -> Result<u32, CompareError> {
        compare(&t1.content, &t2.content)?;
        Ok(add_num(&t1.amount, &t2.amount)?)
    }

    pub fn split_ticket(amount1: &BigUint, amount2: &BigUint) -> Result<u32, CostOverflow> {
        use std::mem::size_of_val;
        let sz = Checked::from(std::cmp::max(size_of_val(amount1), size_of_val(amount2)));
        (40 + (sz >> 1)).as_gas_cost()
    }

    fn dropn(n: u16) -> Result<u32, CostOverflow> {
        // `cost_N_IDropN`, re-benchmarked on the MIR interpreter. Piecewise
        // model with segments split at depths 300 and 400. The model's `S.sub`
        // is saturating, hence `saturating_sub` (a plain `Checked` subtraction
        // would underflow to an error below the segment thresholds).
        let n = n as u32;
        let w3 = std::cmp::min(300, n);
        let w1 = std::cmp::min(400, n).saturating_sub(300);
        let w2 = n.saturating_sub(400);
        let w1 = Checked::from(w1);
        let w2 = Checked::from(w2);
        let w3 = Checked::from(w3);
        ((w1 >> 1) + (w1 >> 2) + (w1 >> 4) + (w1 >> 5)
            + (w2 >> 1) + (w2 >> 2) + (w2 >> 3) + (w2 >> 6)
            + (w3 >> 1) + (w3 >> 2) + (w3 >> 4) + (w3 >> 6)
            + 10)
        .as_gas_cost()
    }

    pub fn drop(mb_n: Option<u16>) -> Result<u32, CostOverflow> {
        mb_n.map_or(Ok(DROP), dropn)
    }

    fn dipn(n: u16) -> Result<u32, CostOverflow> {
        // `cost_N_IDipN`, re-benchmarked on the MIR interpreter: max(10, ~0.305*n).
        let n = Checked::from(n as u32);
        let cost = ((n >> 2) + (n >> 5) + (n >> 6) + (n >> 7)).as_gas_cost()?;
        Ok(cost.max(10))
    }

    pub fn dip(mb_n: Option<u16>) -> Result<u32, CostOverflow> {
        mb_n.map_or(Ok(DIP), dipn)
    }

    pub fn undip(n: u16) -> Result<u32, CostOverflow> {
        // this is derived by observing gas costs as of Nairobi, as charged by
        // the Tezos protocol. It seems undip cost is charged as
        // cost_N_KUndip * n + cost_N_KCons,
        // where cost_N_KUndip = cost_N_KCons = 10
        let n = Checked::from(n as u32);
        ((n + 1) * 10).as_gas_cost()
    }

    fn dupn(_n: u16) -> Result<u32, CostOverflow> {
        // `cost_N_IDupN`, re-benchmarked on the MIR interpreter: the measured
        // slope is ~0, so the cost is size-independent and charged as a flat
        // constant.
        //
        // TODO(L2-1553): check the micro-benchmark results for `N_IDupN`.
        // This 840 looks wrong. The fitted model is a flat constant (~835.8 ns,
        // slope ~0), which is physically implausible: `DUP n` does the same work
        // as `DUP` (clone) plus an O(n) stack walk, so it should start close to
        // `DUP` (= 45) and grow slightly with n, not jump to 840 for every n.
        // The flat ~840 ns const smells of `Timer_latency` contamination /
        // scatter in the raw bench (cf. report_mir_interpreter PDF). This creates
        // a discontinuity: `DUP` = 45 but `DUP 1` = `DUP 2` = ... = 840, even
        // though `DUP 1` is semantically identical to `DUP` (both duplicate the
        // top). MIR routes `DUP 1` -> DupN exactly like L1 (script_ir_translator
        // `I_DUP [n]` -> `IDup_n` even for n = 1), so this is not a MIR bug but a
        // benchmark/model problem to fix upstream (re-fit `N_IDupN` so it is
        // continuous with `N_IDup`), then regenerate and re-port here.
        Ok(840)
    }

    pub fn dig(n: u16) -> Result<u32, CostOverflow> {
        // `cost_N_IDig`, re-benchmarked on the MIR interpreter: 10 + ~0.227*n.
        let n = Checked::from(n as u32);
        ((n >> 3) + (n >> 4) + (n >> 5) + (n >> 7) + 10).as_gas_cost()
    }

    pub fn dug(n: u16) -> Result<u32, CostOverflow> {
        // `cost_N_IDug`, re-benchmarked on the MIR interpreter: 10 + ~0.207*n.
        let n = Checked::from(n as u32);
        ((n >> 3) + (n >> 4) + (n >> 6) + (n >> 8) + 10).as_gas_cost()
    }

    pub fn dup(mb_n: Option<u16>) -> Result<u32, CostOverflow> {
        mb_n.map_or(Ok(DUP), dupn)
    }

    pub fn add_num(
        i1: &impl BigIntByteSize,
        i2: &impl BigIntByteSize,
    ) -> Result<u32, CostOverflow> {
        // max is copied from the Tezos protocol, ostensibly adding two big ints depends on
        // the larger of the two due to result allocation
        let sz = Checked::from(std::cmp::max(i1.byte_size(), i2.byte_size()));
        (65 + (sz >> 3) + (sz >> 7)).as_gas_cost()
    }

    pub fn sub_num(
        i1: &impl BigIntByteSize,
        i2: &impl BigIntByteSize,
    ) -> Result<u32, CostOverflow> {
        let sz = Checked::from(std::cmp::max(i1.byte_size(), i2.byte_size()));
        (65 + (sz >> 3) + (sz >> 7)).as_gas_cost()
    }

    // corresponds to cost_N_IAnd_int_nat in the Tezos protocol
    pub fn and_int_nat(
        i1: &impl BigIntByteSize,
        i2: &impl BigIntByteSize,
    ) -> Result<u32, CostOverflow> {
        let w = Checked::from(Ord::min(i1.byte_size(), i2.byte_size()));
        ((w >> 2) + (w >> 6) + 65).as_gas_cost()
    }

    // corresponds to cost_N_IAnd_nat in the Tezos protocol
    pub fn and_nat(
        i1: &impl BigIntByteSize,
        i2: &impl BigIntByteSize,
    ) -> Result<u32, CostOverflow> {
        let w = Checked::from(Ord::min(i1.byte_size(), i2.byte_size()));
        ((w >> 3) + (w >> 5) + (w >> 7) + 65).as_gas_cost()
    }

    // corresponds to cost_N_IAnd_bytes in the Tezos protocol
    pub fn and_bytes(b1: &[u8], b2: &[u8]) -> Result<u32, CostOverflow> {
        let w = Checked::from(Ord::min(b1.len(), b2.len()));
        (w + (w >> 2) + (w >> 3) + 70).as_gas_cost()
    }

    // corresponds to cost_N_IOr_nat in the Tezos protocol
    pub fn or_num(i1: &impl BigIntByteSize, i2: &impl BigIntByteSize) -> Result<u32, CostOverflow> {
        let w = Checked::from(Ord::max(i1.byte_size(), i2.byte_size()));
        ((w >> 4) + (w >> 5) + (w >> 6) + (w >> 7) + (w >> 8) + (w >> 9) + 65).as_gas_cost()
    }

    // corresponds to cost_N_IOr_bytes in the Tezos protocol
    pub fn or_bytes(b1: &[u8], b2: &[u8]) -> Result<u32, CostOverflow> {
        let w = Checked::from(Ord::max(b1.len(), b2.len()));
        (w + (w >> 2) + (w >> 4) + (w >> 5) + 65).as_gas_cost()
    }

    // corresponds to cost_N_IXor_nat in the Tezos protocol
    pub fn xor_nat(i1: &BigUint, i2: &BigUint) -> Result<u32, CostOverflow> {
        let w = Checked::from(Ord::max(i1.byte_size(), i2.byte_size()));
        ((w >> 4) + (w >> 5) + (w >> 6) + (w >> 7) + (w >> 8) + (w >> 9) + 70).as_gas_cost()
    }

    // corresponds to cost_N_IXor_bytes in the Tezos protocol
    pub fn xor_bytes(b1: &[u8], b2: &[u8]) -> Result<u32, CostOverflow> {
        let w = Checked::from(Ord::max(b1.len(), b2.len()));
        (w + (w >> 1) + (w >> 3) + 60).as_gas_cost()
    }

    // corresponds to cost_N_INot_int in the Tezos protocol
    pub fn not_num<T: BigIntByteSize>(n: &T) -> Result<u32, CostOverflow> {
        let sz = Checked::from(n.byte_size());
        ((sz >> 4) + (sz >> 6) + (sz >> 7) + (sz >> 8) + 90).as_gas_cost()
    }

    // corresponds to cost_N_INot_bytes in the Tezos protocol
    pub fn not_bytes(b: &[u8]) -> Result<u32, CostOverflow> {
        let sz = Checked::from(b.len());
        ((sz >> 4) + (sz >> 5) + (sz >> 7) + (sz >> 9) + 40).as_gas_cost()
    }

    // corresponds to cost_N_ILsl_nat in the Tezos protocol
    pub fn lsl_nat(i1: &impl BigIntByteSize) -> Result<u32, CostOverflow> {
        let sz = Checked::from(i1.byte_size());
        ((sz >> 3) + (sz >> 6) + (sz >> 8) + 85).as_gas_cost()
    }

    // corresponds to cost_N_ILsl_bytes in the Tezos protocol
    pub fn lsl_bytes(i1: &[u8], i2: &usize) -> Result<u32, CostOverflow> {
        let s1 = Checked::from(i1.len());
        let w1 = Checked::from(i2.saturating_sub(1));
        ((w1 >> 8) + (w1 >> 10) + (w1 >> 12) + (w1 >> 13)
            + (s1 >> 4) + (s1 >> 5) + s1
            + 120)
        .as_gas_cost()
    }

    // corresponds to cost_N_ILsr_nat in the Tezos protocol
    pub fn lsr_nat(i1: &impl BigIntByteSize) -> Result<u32, CostOverflow> {
        let sz = Checked::from(i1.byte_size());
        ((sz >> 3) + (sz >> 7) + 85).as_gas_cost()
    }

    // corresponds to cost_N_ILsr_bytes in the Tezos protocol
    pub fn lsr_bytes(i1: &[u8], i2: &usize) -> Result<u32, CostOverflow> {
        let w1 = Checked::from(i1.len().saturating_sub(*i2 >> 3));
        (w1 + (w1 >> 4) + 90).as_gas_cost()
    }

    pub fn mul_int(
        i1: &impl BigIntByteSize,
        i2: &impl BigIntByteSize,
    ) -> Result<u32, CostOverflow> {
        let a = Checked::from(i1.byte_size()) + Checked::from(i2.byte_size());
        // log2 is ill-defined for zero, hence this check
        let v0 = if a.is_zero() {
            Checked::from(0)
        } else {
            a * (a.ok_or(CostOverflow)?.log2i()? as u64)
        };
        (55 + (v0 >> 1) + (v0 >> 2) + (v0 >> 4)).as_gas_cost()
    }

    pub fn ediv_int(
        i1: &impl BigIntByteSize,
        i2: &impl BigIntByteSize,
    ) -> Result<u32, CostOverflow> {
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

    pub fn ediv_nat(
        i1: &impl BigIntByteSize,
        i2: &impl BigIntByteSize,
    ) -> Result<u32, CostOverflow> {
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

    pub fn compare(v1: &TypedValue, v2: &TypedValue) -> Result<u32, CompareError> {
        use TypedValue as V;
        let cmp_bytes = |s1: u64, s2: u64| -> Result<u32, CompareError> {
            // corresponds to cost_N_ICompare in the Tezos protocol
            let w = Checked::from(std::cmp::min(s1, s2).saturating_sub(1));
            Ok(((w >> 5) + (w >> 6) + (w >> 8) + 85).as_gas_cost()?)
        };
        const ADDRESS_SIZE: u64 = 20 + 31; // hash size + max entrypoint size
        const CMP_CHAIN_ID: u32 = 30;
        const CMP_KEY: u32 = 92; // hard-coded in the protocol
        const CMP_SIGNATURE: u32 = 92; // hard-coded in the protocol
        const CMP_NODE: u32 = 10; // per Pair / Option / Or node

        // The COMPARE cost is an order-independent sum of per-node costs, so an
        // explicit-stack walk with a single accumulator is bit-identical to the
        // previous recursive sum while no longer recursing on deep
        // `pair`/`option`/`or` values -- which would overflow the kernel's ~1 MiB
        // Rust stack here, *before* any gas is charged, where gas can never gate
        // it. See L2-1449.
        let mut total = Checked::from(0u32);
        let mut stack: Vec<(&TypedValue, &TypedValue)> = vec![(v1, v2)];
        while let Some((v1, v2)) = stack.pop() {
            let node: u32 = match (v1, v2) {
                (V::Nat(l), V::Nat(r)) => cmp_bytes(l.byte_size(), r.byte_size())?,
                (V::Nat(_), _) => return Err(CompareError::Incomparable),

                (V::Int(l), V::Int(r)) => cmp_bytes(l.byte_size(), r.byte_size())?,
                (V::Int(_), _) => return Err(CompareError::Incomparable),

                (V::Timestamp(l), V::Timestamp(r)) => cmp_bytes(l.byte_size(), r.byte_size())?,
                (V::Timestamp(_), _) => return Err(CompareError::Incomparable),

                (V::Bool(_), V::Bool(_)) => cmp_bytes(1, 1)?,
                (V::Bool(_), _) => return Err(CompareError::Incomparable),

                (V::Mutez(_), V::Mutez(_)) => cmp_bytes(8, 8)?,
                (V::Mutez(_), _) => return Err(CompareError::Incomparable),

                (V::String(l), V::String(r)) => cmp_bytes(l.len() as u64, r.len() as u64)?,
                (V::String(_), _) => return Err(CompareError::Incomparable),

                (V::Unit, V::Unit) => CMP_NODE,
                (V::Unit, _) => return Err(CompareError::Incomparable),

                (V::Pair(l1, l2), V::Pair(r1, r2)) => {
                    stack.push((l1.as_ref(), r1.as_ref()));
                    stack.push((l2.as_ref(), r2.as_ref()));
                    CMP_NODE
                }
                (V::Pair(..), _) => return Err(CompareError::Incomparable),

                (V::Option(l), V::Option(r)) => {
                    if let (Some(l), Some(r)) = (l, r) {
                        stack.push((l.as_ref(), r.as_ref()));
                    }
                    CMP_NODE
                }
                (V::Option(_), _) => return Err(CompareError::Incomparable),

                (V::Address(..), V::Address(..)) => cmp_bytes(ADDRESS_SIZE, ADDRESS_SIZE)?,
                (V::Address(_), _) => return Err(CompareError::Incomparable),

                (V::ChainId(..), V::ChainId(..)) => CMP_CHAIN_ID,
                (V::ChainId(_), _) => return Err(CompareError::Incomparable),

                (V::Bytes(l), V::Bytes(r)) => cmp_bytes(l.len() as u64, r.len() as u64)?,
                (V::Bytes(_), _) => return Err(CompareError::Incomparable),

                (V::Key(_), V::Key(_)) => CMP_KEY,
                (V::Key(_), _) => return Err(CompareError::Incomparable),

                (V::Signature(_), V::Signature(_)) => CMP_SIGNATURE,
                (V::Signature(_), _) => return Err(CompareError::Incomparable),

                (V::KeyHash(_), V::KeyHash(_)) => cmp_bytes(20u64, 20u64)?,
                (V::KeyHash(_), _) => return Err(CompareError::Incomparable),

                (V::Or(l), V::Or(r)) => {
                    match (l, r) {
                        (Or::Left(x), Or::Left(y)) | (Or::Right(x), Or::Right(y)) => {
                            stack.push((x.as_ref(), y.as_ref()))
                        }
                        (Or::Left(_), Or::Right(_)) | (Or::Right(_), Or::Left(_)) => {}
                    }
                    CMP_NODE
                }
                (V::Or(..), _) => return Err(CompareError::Incomparable),

                #[cfg(feature = "bls")]
                (V::Bls12381Fr(_) | V::Bls12381G1(_) | V::Bls12381G2(_), _) => {
                    return Err(CompareError::Incomparable)
                }

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
                ) => return Err(CompareError::Incomparable),
            };
            total = total + node;
        }
        Ok(total.as_gas_cost()?)
    }

    /// Cost charged for computing total entries size (needed for the subsequent
    /// gas calculation, so this is meta-gas).
    pub fn concat_list_precheck(list_size: usize) -> Result<u32, CostOverflow> {
        (10 * Checked::from(list_size)).as_gas_cost()
    }

    pub fn concat_string_list(total_len: Checked<usize>) -> Result<u32, CostOverflow> {
        // Copied from the Tezos protocol
        (total_len / 2 + 100).as_gas_cost()
    }

    pub fn concat_bytes_list(total_len: Checked<usize>) -> Result<u32, CostOverflow> {
        // Copied from the Tezos protocol
        (total_len / 2 + 100).as_gas_cost()
    }

    pub fn concat_string_pair(len1: usize, len2: usize) -> Result<u32, CostOverflow> {
        // Copied from the Tezos protocol
        ((Checked::from(len1) + Checked::from(len2)) / 2 + 45).as_gas_cost()
    }

    pub fn concat_bytes_pair(len1: usize, len2: usize) -> Result<u32, CostOverflow> {
        // Copied from the Tezos protocol
        ((Checked::from(len1) + Checked::from(len2)) / 2 + 45).as_gas_cost()
    }

    pub fn map_mem(k: &TypedValue, map_size: usize) -> Result<u32, CompareError> {
        map_get(k, map_size)
    }

    pub fn map_get(k: &TypedValue, map_size: usize) -> Result<u32, CompareError> {
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
        let size_log = (map_size + 1).ok_or(CostOverflow)?.log2i()?;
        let lookup_cost = Checked::from(compare_cost) * size_log;
        Ok((80 + lookup_cost).as_gas_cost()?)
    }

    pub fn set_mem(k: &TypedValue, map_size: usize) -> Result<u32, CompareError> {
        // NB: same considerations as for map_get
        let compare_cost = compare(k, k)?;
        let size_log = (Checked::from(map_size) + 1).ok_or(CostOverflow)?.log2i()?;
        let lookup_cost = Checked::from(compare_cost) * size_log;
        Ok((115 + lookup_cost).as_gas_cost()?)
    }

    pub fn map_update(k: &TypedValue, map_size: usize) -> Result<u32, CompareError> {
        // NB: same considerations as for map_get
        let map_size = Checked::from(map_size);
        let compare_cost = compare(k, k)?;
        let size_log = (map_size + 1).ok_or(CostOverflow)?.log2i()?;
        let lookup_cost = Checked::from(compare_cost) * size_log;
        // NB: 2 factor copied from Tezos protocol, in principle it should
        // reflect update vs get overhead.
        Ok((80 + 2 * lookup_cost).as_gas_cost()?)
    }

    pub fn set_update(k: &TypedValue, map_size: usize) -> Result<u32, CompareError> {
        // NB: same considerations as for map_update
        let compare_cost = compare(k, k)?;
        let size_log = (Checked::from(map_size) + 1).ok_or(CostOverflow)?.log2i()?;
        let lookup_cost = Checked::from(compare_cost) * size_log;
        // coefficient larger than in case of Map looks suspicious, something
        // to benchmark later
        Ok((130 + 2 * lookup_cost).as_gas_cost()?)
    }

    pub fn map_get_and_update(k: &TypedValue, map_size: usize) -> Result<u32, CompareError> {
        // NB: same considerations as for map_get
        let compare_cost = compare(k, k)?;
        let size_log = (Checked::from(map_size) + 1).ok_or(CostOverflow)?.log2i()?;
        let lookup_cost = Checked::from(compare_cost) * size_log;
        // NB: 3 factor copied from Tezos protocol, in principle it should
        // reflect update vs get overhead, but it seems like an overestimation,
        // get_and_update should cost almost exactly the same as update, any
        // observable difference would be in the constant term.
        //
        // However, note that this function is also reused for big_map version
        // of GET_AND_UPDATE, wherein it's more justified. That is to say, take
        // care when updating this.
        Ok((80 + 3 * lookup_cost).as_gas_cost()?)
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

    pub fn micheline_encoding<'a>(mich: &'a Micheline<'a>) -> Result<u32, CostOverflow> {
        let mut size = MichelineSize::default();
        collect_micheline_size(mich, &mut size);
        micheline_encoding_by_size(size)
    }

    /// Mirrors L1's `cost_DECODING_MICHELINE_bytes`
    /// (`src/proto_alpha/lib_protocol/script_repr_costs_generated.ml:23`):
    /// 20 milligas/byte, with a 10-milligas floor.
    pub fn micheline_decoding_bytes(bytes_len: usize) -> Result<u32, CostOverflow> {
        let scaled = (Checked::from(bytes_len) * 20).as_gas_cost()?;
        Ok(scaled.max(10))
    }

    fn micheline_encoding_by_size(size: MichelineSize) -> Result<u32, CostOverflow> {
        (size.nodes_num * 100 + size.zariths * 25 + size.str_byte * 10).as_gas_cost()
    }

    /// Iterative tree fold over `Micheline` so the size accounting (and
    /// the resulting `PACK` gas charge) does not blow the WASM call stack
    /// on deeply nested input. Order of visit does not matter: the counter
    /// is a commutative sum, so the result is bit identical to the
    /// previous recursive form.
    fn collect_micheline_size<'a>(mich: &'a Micheline<'a>, size: &mut MichelineSize) {
        use crate::ast::Annotation as Ann;
        let mut stack: Vec<&'a Micheline<'a>> = vec![mich];
        while let Some(m) = stack.pop() {
            size.nodes_num += 1;
            match m {
                Micheline::String(s) => size.str_byte += s.len() as u64,
                Micheline::Bytes(bs) => size.str_byte += bs.len() as u64,
                Micheline::Int(i) => size.zariths += i.byte_size(),
                Micheline::Seq(ms) => stack.extend(ms.iter()),
                Micheline::App(_prim, args, annots) => {
                    stack.extend(args.iter());
                    for annot in annots {
                        // Annotations are accounted as simple string literals
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

    pub fn slice(length: usize) -> Result<u32, CostOverflow> {
        // In the protocol, the gas costs for slicing strings and bytes are defined
        // separately (see `cost_N_ISlice_bytes` and `cost_N_ISlice_string`).
        //
        // In practice, they both have the same cost.
        ((Checked::from(length) >> 1) + 25).as_gas_cost()
    }

    pub fn blake2b(msg: &[u8]) -> Result<u32, CostOverflow> {
        /* fun size -> (430. + (1.125 * size)) */
        let size = Checked::from(msg.len());
        (Checked::from(430) + ((size >> 3) + size)).as_gas_cost()
    }

    pub fn keccak(msg: &[u8]) -> Result<u32, CostOverflow> {
        /* fun size -> (1350. + (8.25 * size)) */
        let size = Checked::from(msg.len());
        (Checked::from(1350) + ((size >> 2) + (size * 8))).as_gas_cost()
    }

    pub fn sha256(msg: &[u8]) -> Result<u32, CostOverflow> {
        /* fun size -> (600. + (4.75 * size)) */
        let size = Checked::from(msg.len());
        (Checked::from(600) + ((size >> 2) + ((size >> 1) + (size * 4)))).as_gas_cost()
    }

    pub fn sha3(msg: &[u8]) -> Result<u32, CostOverflow> {
        /* fun size -> (1350. + (8.25 * size)) */
        let size = Checked::from(msg.len());
        (Checked::from(1350) + ((size >> 2) + (size * 8))).as_gas_cost()
    }

    pub fn sha512(msg: &[u8]) -> Result<u32, CostOverflow> {
        /* fun size -> (680. + (3. * size)) */
        let size = Checked::from(msg.len());
        (Checked::from(680) + (size * 3)).as_gas_cost()
    }

    pub fn pairing_check(size: usize) -> Result<u32, CostOverflow> {
        (450_000 + 342_500 * Checked::from(size)).as_gas_cost()
    }

    pub fn mul_bls_fr_big_int(int: &impl BigIntByteSize) -> Result<u32, CostOverflow> {
        // 265. + 1.0625 * size
        // NB: cost_N_IMul_bls12_381_fr_z and
        // cost_N_IMul_bls12_381_z_fr ar distinct in the protocol, but they're the
        // same exact operation, so we opted to use one function.
        let size = Checked::from(int.byte_size());
        (265 + ((size >> 4) + size)).as_gas_cost()
    }

    // corresponds to cost_N_INeg in the Tezos protocol
    pub fn neg_int(int: &impl BigIntByteSize) -> Result<u32, CostOverflow> {
        let size = Checked::from(int.byte_size());
        ((size >> 4) + (size >> 6) + (size >> 7) + (size >> 8) + 55).as_gas_cost()
    }

    pub fn abs(int: &impl BigIntByteSize) -> Result<u32, CostOverflow> {
        let size = Checked::from(int.byte_size());
        (50 + (size >> 4) + (size >> 6) + (size >> 8) + (size >> 9)).as_gas_cost()
    }

    // corresponds to cost_N_IInt_bytes in the Tezos protocol
    pub fn int_bytes(size: usize) -> Result<u32, CostOverflow> {
        let size = Checked::from(size);
        ((size >> 1) + (size >> 2) + (size >> 5) + (size >> 6) + 50).as_gas_cost()
    }

    // corresponds to cost_N_INat_bytes in the Tezos protocol
    pub fn nat_bytes(size: usize) -> Result<u32, CostOverflow> {
        let size = Checked::from(size);
        ((size >> 1) + (size >> 6) + 55).as_gas_cost()
    }

    // corresponds to cost_N_IBytes_int in the Tezos protocol
    pub fn bytes_int(int: &BigInt) -> Result<u32, CostOverflow> {
        let size = Checked::from(int.byte_size());
        ((size >> 1) + (size >> 3) + (size >> 4) + size + 100).as_gas_cost()
    }

    // corresponds to cost_N_IBytes_nat in the Tezos protocol
    pub fn bytes_nat(int: &BigUint) -> Result<u32, CostOverflow> {
        let size = Checked::from(int.byte_size());
        ((size >> 2) + (size >> 3) + (size >> 4) + size + 90).as_gas_cost()
    }

    pub fn unpack(bytes: &[u8]) -> Result<u32, CostOverflow> {
        let size = Checked::from(bytes.len());
        (260 + (size >> 1)).as_gas_cost()
    }

    pub fn pair_n(size: usize) -> Result<u32, CostOverflow> {
        // corresponds to cost_N_IComb in the Tezos protocol
        let size = Checked::from(size);
        let v0 = size - 2;
        (50 + (v0 * 15)).as_gas_cost()
    }

    pub fn unpair_n(size: usize) -> Result<u32, CostOverflow> {
        // corresponds to cost_N_IUncomb in the Tezos protocol
        let size = Checked::from(size);
        let v0 = size - 2;
        (50 + ((v0 * 7) + (v0 >> 2))).as_gas_cost()
    }

    pub fn get_n(size: usize) -> Result<u32, CostOverflow> {
        // corresponds to cost_N_IComb_get in the Tezos protocol
        let size = Checked::from(size);
        (60 + ((size >> 1) + (size * 18))).as_gas_cost()
    }

    pub fn update_n(size: usize) -> Result<u32, CostOverflow> {
        // corresponds to cost_N_IComb_set in the Tezos protocol
        let size = Checked::from(size);
        (50 + ((size >> 2) + (size * 13))).as_gas_cost()
    }
}

/// Costs related to conversion from AST to Micheline. They are meant
/// to cover the cost of allocating the Micheline tree (the memory but
/// also the time spent to allocate).
pub mod unparsing_cost {
    use super::{AsGasCost, BigInt, BigIntByteSize, CostOverflow};
    use crate::ast::annotations::Annotation;
    use checked::Checked;

    /// Cost for allocating a Micheline node: 100mg.
    pub const NODE: u32 = 100;

    /// Cost for allocating a Micheline Int node: 100 mg + 25 mg/byte.
    pub fn int(i: &BigInt) -> Result<u32, CostOverflow> {
        let size = Checked::from(i.byte_size());
        (100 + size * 25).as_gas_cost()
    }

    /// Cost for allocating a Micheline String node: 100 mg + 10 mg/byte
    pub fn string(string: &str) -> Result<u32, CostOverflow> {
        let size = Checked::from(string.len());
        (100 + size * 10).as_gas_cost()
    }

    /// Cost for allocating a Micheline Bytes node: 100mg + 10 mg/byte
    pub fn bytes(bytes: &[u8]) -> Result<u32, CostOverflow> {
        let size = Checked::from(bytes.len());
        (100 + size * 10).as_gas_cost()
    }

    /// Cost for allocating a Micheline annotation: 10 mg/byte. No
    /// cost for allocating a Micheline node in this case because
    /// annotations are not Micheline nodes.
    pub fn annotation(annot: &Annotation) -> Result<u32, CostOverflow> {
        let size = Checked::from(annot.len());
        (size * 10).as_gas_cost()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn gas_consumption() {
        let mut gas = Gas::new(100);
        gas.consume(30).unwrap();
        assert_eq!(gas.milligas(), Some(70))
    }

    #[test]
    fn gas_exhaustion_error() {
        let mut gas = Gas::new(100);
        assert_eq!(gas.consume(1000), Err(OutOfGas));
    }

    #[test]
    fn gas_exhaustion_no_panic() {
        let mut gas = Gas::new(100);
        assert_eq!(gas.consume(1000), Err(OutOfGas));
        assert_eq!(gas.milligas(), None);

        // Consuming after exhaustion returns OutOfGas without panicking.
        assert_eq!(gas.consume(1000), Err(OutOfGas));
    }

    #[test]
    fn overflow_to_cost_overflow() {
        for n in [usize::MAX, usize::MAX / 2, usize::MAX / 4] {
            assert_eq!(super::tc_cost::ty_eq(n, n), Err(CostOverflow));
        }
    }

    /// Regression for L2-1449: `interpret_cost::compare` recursed on `pair`
    /// (before any gas was charged) and overflowed the kernel's ~1 MiB Rust
    /// stack on a deep comparable value, where gas could never gate it. The
    /// cost is an order-independent sum, so the iterative walk yields the same
    /// total without recursing.
    #[test]
    fn compare_cost_deep_pair_does_not_overflow() {
        use crate::ast::TypedValue as V;
        const DEPTH: usize = 100_000;
        std::thread::Builder::new()
            .stack_size(1024 * 1024)
            .spawn(|| {
                let deep = || {
                    let mut v = V::int(0);
                    for _ in 0..DEPTH {
                        v = V::new_pair(V::int(0), v);
                    }
                    v
                };
                let a = deep();
                let b = deep();
                let cost = interpret_cost::compare(&a, &b).expect("comparable");
                // 10 per pair node (DEPTH nodes) plus the per-leaf costs; mainly
                // we assert the walk completed without overflowing.
                assert!(cost >= 10 * DEPTH as u32);
                // `TypedValue` has no iterative Drop yet (L2-1446).
                std::mem::forget(a);
                std::mem::forget(b);
            })
            .unwrap()
            .join()
            .expect("worker thread completes");
    }

    #[test]
    fn log2i_test() {
        assert_eq!(1usize.log2i(), Ok(0));
        assert_eq!(2usize.log2i(), Ok(1));
        assert_eq!(3usize.log2i(), Ok(2));
        assert_eq!(4usize.log2i(), Ok(2));
        assert_eq!(5usize.log2i(), Ok(3));
        assert_eq!(70_000usize.log2i(), Ok(17));
        assert_eq!(100_000usize.log2i(), Ok(17));
        assert_eq!(300_000usize.log2i(), Ok(19));

        assert_eq!(1u64.log2i(), Ok(0));
        assert_eq!(2u64.log2i(), Ok(1));
        assert_eq!(3u64.log2i(), Ok(2));
        assert_eq!(4u64.log2i(), Ok(2));
        assert_eq!(5u64.log2i(), Ok(3));
        assert_eq!(70_000u64.log2i(), Ok(17));
        assert_eq!(100_000u64.log2i(), Ok(17));
        assert_eq!(300_000u64.log2i(), Ok(19));
    }

    #[test]
    fn log2i_zero_usize() {
        assert_eq!(0usize.log2i(), Err(CostOverflow));
    }

    #[test]
    fn log2i_zero_u64() {
        assert_eq!(0u64.log2i(), Err(CostOverflow));
    }

    // Inputs whose next power of two would overflow must also return
    // CostOverflow, not panic in debug and wrap in release.
    #[test]
    fn log2i_overflow_usize() {
        assert_eq!(usize::MAX.log2i(), Err(CostOverflow));
        // largest input whose next power of two still fits: 2^(BITS-1).
        let high = 1usize << (usize::BITS - 1);
        assert_eq!(high.log2i(), Ok(usize::BITS - 1));
        assert_eq!((high + 1).log2i(), Err(CostOverflow));
    }

    #[test]
    fn log2i_overflow_u64() {
        assert_eq!(u64::MAX.log2i(), Err(CostOverflow));
        let high = 1u64 << (u64::BITS - 1);
        assert_eq!(high.log2i(), Ok(u64::BITS - 1));
        assert_eq!((high + 1).log2i(), Err(CostOverflow));
    }
}
