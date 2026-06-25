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
    /// Cost arithmetic overflowed while sizing the comparison. Most
    /// cost-helper functions in [`tc_cost`] / [`interpret_cost`] do not
    /// charge gas (that happens at the call site via [`Gas::consume`]),
    /// they only do cost arithmetic. [`interpret_cost::compare`] is the
    /// exception: it meters incrementally (charging each node as it
    /// walks), so it can also fail with [`CompareError::OutOfGas`].
    #[error(transparent)]
    Cost(#[from] CostOverflow),
    /// Attempted to compare incomparable values.
    #[error("comparison of incomparable values")]
    Incomparable,
    /// Gas was exhausted while metering the comparison incrementally.
    #[error(transparent)]
    OutOfGas(#[from] OutOfGas),
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

    // `key` (public key) literal decode: the decode plus the per-curve point
    // validation (`PublicKey::check_validity`) charged by the typechecker's
    // `T::Key` arms. Max over the supported curves (P256 dominates), benchmarked
    // on the MIR interpreter and scaled into the MIR gas schedule via its
    // `check_signature` gas anchors.
    // Re-derive if the `bls` feature is enabled: BLS (tz4) keys would then be
    // validated, and their check (~2x P256) would dominate.
    pub const KEY_READABLE: u32 = 36500;
    pub const KEY_OPTIMIZED: u32 = 34000;

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

    pub fn check_printable(len: usize) -> Result<u32, CostOverflow> {
        // corresponds to cost_CHECK_PRINTABLE in the protocol: 10 * len + 15
        (Checked::from(len) * 10 + 15).as_gas_cost()
    }

    // Flat cost of resolving an entrypoint. L1 walks the parameter's Or-tree
    // charging cost_FIND_ENTRYPOINT (= cost_N_ICompare 31 31 = 35) per node;
    // MIR resolves against a flat entrypoint HashMap in O(1), so a single
    // constant charge matches MIR's actual complexity.
    pub const FIND_ENTRYPOINT: u32 = 35;
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
// TODO(L2-1315): several constants in this module come from suspicious
// benchmark fits (flat costs for O(1) work, under-priced crypto). The
// live list of doubtful models is tracked in the issue.
#[allow(missing_docs)]
pub mod interpret_cost {
    use checked::Checked;
    use num_bigint::{BigInt, BigUint};
    use tezos_crypto_rs::public_key::PublicKey;
    use tezos_crypto_rs::CryptoError;
    use thiserror::Error;

    use super::{AsGasCost, BigIntByteSize, CompareError, CostOverflow, Gas, OutOfGas};
    use crate::ast::{Micheline, Or, Ticket, TypedValue};

    pub const DIP: u32 = 10;
    // Re-benchmarked on the MIR interpreter (`cost_N_IDrop`).
    pub const DROP: u32 = 20;
    // Re-benchmarked on the MIR interpreter (`cost_N_IDup`).
    pub const DUP: u32 = 45;
    // correspond to cost_N_IGt / IGe / IEq / INeq / ILe / ILt in the Tezos
    // protocol.
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
    // corresponds to cost_N_ILoop in the Tezos protocol
    pub const LOOP: u32 = 15;
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
    // corresponds to cost_N_IMul_teznat in the Tezos protocol
    pub const MUL_TEZ_NAT: u32 = 60;
    // corresponds to cost_N_IMul_nattez in the Tezos protocol
    pub const MUL_NAT_TEZ: u32 = 65;
    // corresponds to cost_N_IEdiv_tez in the Tezos protocol
    pub const EDIV_TEZ_TEZ: u32 = 120;
    // corresponds to cost_N_IEdiv_teznat in the Tezos protocol
    pub const EDIV_TEZ_NAT: u32 = 125;
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
    // corresponds to cost_N_ICons_pair in the Tezos protocol
    pub const PAIR: u32 = 20;
    pub const UNPAIR: u32 = 60;
    // corresponds to cost_N_ICons_some in the Tezos protocol
    pub const SOME: u32 = 20;
    // corresponds to cost_N_ICons_none in the Tezos protocol
    pub const NONE: u32 = 65;
    pub const AMOUNT: u32 = 65;
    pub const NIL: u32 = 60;
    // corresponds to cost_N_ICons_list in the Tezos protocol
    pub const CONS: u32 = 845;
    pub const EMPTY_SET: u32 = 60;
    // correspond to cost_N_IString_size / cost_N_IBytes_size in the Tezos
    // protocol.
    pub const SIZE_STRING: u32 = 1570;
    pub const SIZE_BYTES: u32 = 1595;
    // corresponds to cost_N_IList_size in the Tezos protocol
    pub const SIZE_LIST: u32 = 920;
    // corresponds to cost_N_ISet_size in the Tezos protocol
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
    // corresponds to cost_N_IList_map in the Tezos protocol
    pub const MAP_LIST: u32 = 70;
    // corresponds to cost_N_IMap_map in the Tezos protocol. The MIR
    // micro-benchmarks confirm the flat cost (fitted size coefficient is 0).
    pub const MAP_MAP: u32 = 45;

    // Gas costs obtained from https://gitlab.com/tezos/tezos/-/blob/9875fbebe032a8c5ce62b3b3cb1588ca9855a37e/src/proto_017_PtNairob/lib_protocol/michelson_v1_gas_costs_generated.ml
    pub const TRANSFER_TOKENS: u32 = 120;
    pub const SET_DELEGATE: u32 = 75;
    // Lambda creation cost (`cost_N_ILambda_lam` / `cost_N_ILambda_lamrec`).
    // A single constant for both forms, the worst case of the two models
    // (lam 80, lamrec 75), as the protocol charges.
    pub const LAMBDA: u32 = 80;
    pub const EXEC: u32 = 10;
    // corresponds to cost_N_IHash_key in the Tezos protocol
    pub const HASH_KEY: u32 = 215;
    // corresponds to cost_N_IApply in the Tezos protocol (the generated
    // model merges the recursive and non-recursive cases)
    pub const APPLY: u32 = 85;
    // correspond to cost_N_ITicket / cost_N_IRead_ticket in the Tezos
    // protocol.
    pub const TICKET: u32 = 3580;
    pub const READ_TICKET: u32 = 3935;
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
    /// Flat cost of `INDEX_ADDRESS`, matching L1's `cost_N_IIndex_address`
    /// (charged whether or not the address is newly registered).
    /// https://gitlab.com/tezos/tezos/-/blob/5781c10e4e7d/src/proto_024_PtTALLiN/lib_protocol/michelson_v1_gas_costs_generated.ml#L1697
    pub const INDEX_ADDRESS: u32 = 3480;
    /// Cost of `GET_ADDRESS_INDEX`, matching L1's `cost_N_IGet_address_index`.
    /// https://gitlab.com/tezos/tezos/-/blob/5781c10e4e7d/src/proto_024_PtTALLiN/lib_protocol/michelson_v1_gas_costs_generated.ml#L1589
    pub const GET_ADDRESS_INDEX: u32 = 530;
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
    /// `etherlink/kernel_latest/mir/docs/frame_gas_calibration.md` (when present).
    pub const FRAME_PUSH: u32 = 100;

    /// Sibling charge for pushes onto the per-EXEC/per-VIEW substack
    /// (`stacks: Vec<IStack>`). Same layered-on-top-of-L1 framing as
    /// [`FRAME_PUSH`]: L1 has no analogue because its recursive OCaml
    /// interpreter does not materialize a fresh sub-stack per `Apply`
    /// invocation — the OCaml runtime stack absorbs the inner frame
    /// under TCE. MIR allocates a `Vec<Rc<TypedValue>>` per push
    /// (`Vec` header + initial allocation + initial entries: lambda +
    /// arg for `EXEC`, view arg for `VIEW`), so the host-memory
    /// footprint per push is ~5 × that of a bare [`FRAME_PUSH`];
    /// `STACK_PUSH` is sized accordingly. Pushed only by `OpenExec`
    /// / `OpenView` in `handle_step`, so on a divergent recursive
    /// lambda this fires once per iteration and is the single
    /// biggest source of bounded host-memory cost.
    pub const STACK_PUSH: u32 = 500;
    pub const LOOP_ENTER: u32 = 10; // corresponds to KLoop_in in the Tezos protocol
    pub const LOOP_LEFT_ENTER: u32 = 10; // corresponds to KLoop_in_left in the Tezos protocol
    pub const LOOP_EXIT: u32 = 15;
    // corresponds to cost_N_ICreate_contract in the Tezos protocol
    pub const CREATE_CONTRACT: u32 = 340;
    pub const VIEW: u32 = 65; // corresponds to cost_N_IView_synthesized in the Tezos protocol

    // corresponds to cost_N_IJoin_tickets in the Tezos protocol
    pub fn join_tickets(t1: &Ticket, t2: &Ticket) -> Result<u32, CompareError> {
        let w1 =
            Checked::from(std::cmp::max(t1.amount.byte_size(), t2.amount.byte_size()));
        let w2 = Checked::from(std::cmp::min(
            comparable_size(&t1.content)?,
            comparable_size(&t2.content)?,
        ));
        Ok(((w1 >> 3) + (w1 >> 5) + (w2 >> 3) + (w2 >> 7) + 245).as_gas_cost()?)
    }

    // corresponds to cost_N_ISplit_ticket in the Tezos protocol
    pub fn split_ticket(
        amount1: &BigUint,
        amount2: &BigUint,
    ) -> Result<u32, CostOverflow> {
        let w = Checked::from(std::cmp::max(amount1.byte_size(), amount2.byte_size()));
        ((w >> 2) + (w >> 7) + 475).as_gas_cost()
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
        ((w1 >> 1)
            + (w1 >> 2)
            + (w1 >> 4)
            + (w1 >> 5)
            + (w2 >> 1)
            + (w2 >> 2)
            + (w2 >> 3)
            + (w2 >> 6)
            + (w3 >> 1)
            + (w3 >> 2)
            + (w3 >> 4)
            + (w3 >> 6)
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
    pub fn or_num(
        i1: &impl BigIntByteSize,
        i2: &impl BigIntByteSize,
    ) -> Result<u32, CostOverflow> {
        let w = Checked::from(Ord::max(i1.byte_size(), i2.byte_size()));
        ((w >> 4) + (w >> 5) + (w >> 6) + (w >> 7) + (w >> 8) + (w >> 9) + 65)
            .as_gas_cost()
    }

    // corresponds to cost_N_IOr_bytes in the Tezos protocol
    pub fn or_bytes(b1: &[u8], b2: &[u8]) -> Result<u32, CostOverflow> {
        let w = Checked::from(Ord::max(b1.len(), b2.len()));
        (w + (w >> 2) + (w >> 4) + (w >> 5) + 65).as_gas_cost()
    }

    // corresponds to cost_N_IXor_nat in the Tezos protocol
    pub fn xor_nat(i1: &BigUint, i2: &BigUint) -> Result<u32, CostOverflow> {
        let w = Checked::from(Ord::max(i1.byte_size(), i2.byte_size()));
        ((w >> 4) + (w >> 5) + (w >> 6) + (w >> 7) + (w >> 8) + (w >> 9) + 70)
            .as_gas_cost()
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
        ((w1 >> 8)
            + (w1 >> 10)
            + (w1 >> 12)
            + (w1 >> 13)
            + (s1 >> 4)
            + (s1 >> 5)
            + s1
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

    // L2-1691: num-bigint has no FFT (schoolbook / Karatsuba / Toom-3), so the
    // real MUL work is super-(n log n); L1's FFT-shaped `a*log(a)` cost
    // (`cost_N_IMul_*`, calibrated for GMP and re-fitted to the same shape by
    // the MIR snoop) under-prices large multiplications. We price MUL on the
    // product of operand sizes instead -- the schoolbook bound num-bigint never
    // exceeds (its sub-quadratic paths only do less). The per-product
    // coefficient is taken from `ediv_*`'s product term, which the
    // sequencer-machine snoop calibrated for the equivalent O(size1*size2)
    // num-bigint schoolbook work (`cost_N_IEdiv_int ~ 0.0203*q*size2`).
    // TODO(https://linear.app/tezos/issue/L2-1691): confirm/refine with a
    // dedicated MUL micro-benchmark (product-of-sizes model) on the sequencer
    // machine.
    fn mul_cost(s1: u64, s2: u64) -> Result<u32, CostOverflow> {
        let p = Checked::from(s1) * Checked::from(s2);
        ((p >> 6) + (p >> 8) + (p >> 10) + 85).as_gas_cost()
    }

    pub fn mul_int(
        i1: &impl BigIntByteSize,
        i2: &impl BigIntByteSize,
    ) -> Result<u32, CostOverflow> {
        mul_cost(i1.byte_size(), i2.byte_size())
    }

    pub fn mul_nat(
        i1: &impl BigIntByteSize,
        i2: &impl BigIntByteSize,
    ) -> Result<u32, CostOverflow> {
        mul_cost(i1.byte_size(), i2.byte_size())
    }

    // corresponds to cost_N_IEdiv_int in the Tezos protocol
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
        ((w1 * 3)
            + (((w1 >> 6) + (w1 >> 8) + (w1 >> 10)) * size_2)
            + (size_1 >> 2)
            + (size_1 >> 3)
            + (size_1 >> 7)
            + 70)
            .as_gas_cost()
    }

    // corresponds to cost_N_IEdiv_nat in the Tezos protocol
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
        ((w1 * 3)
            + (((w1 >> 6) + (w1 >> 8) + (w1 >> 10)) * size_2)
            + (w1 >> 3)
            + (size_1 >> 3)
            + (size_1 >> 4)
            + (size_1 >> 5)
            + (size_1 >> 7)
            + 70)
            .as_gas_cost()
    }

    pub fn compare(
        gas: &mut Gas,
        v1: &TypedValue,
        v2: &TypedValue,
    ) -> Result<(), CompareError> {
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

        // The COMPARE cost is an order-independent sum of per-node costs.
        // Consume each node's cost as the explicit-stack walk visits it, so a
        // value whose in-memory DAG expands to a 2^n-node tree (shared
        // sub-terms built via `DUP; PAIR`) runs out of gas mid-walk instead of
        // walking the whole expansion before any gas is charged. The total
        // charged is identical to summing then consuming once. The walk is also
        // explicit-stack (not recursive) to avoid overflowing the kernel's
        // ~1 MiB Rust stack on deep `pair`/`option`/`or` values. See L2-1449,
        // L2-1654.
        let mut stack: Vec<(&TypedValue, &TypedValue)> = vec![(v1, v2)];
        while let Some((v1, v2)) = stack.pop() {
            let node: u32 = match (v1, v2) {
                (V::Nat(l), V::Nat(r)) => cmp_bytes(l.byte_size(), r.byte_size())?,
                (V::Nat(_), _) => return Err(CompareError::Incomparable),

                (V::Int(l), V::Int(r)) => cmp_bytes(l.byte_size(), r.byte_size())?,
                (V::Int(_), _) => return Err(CompareError::Incomparable),

                (V::Timestamp(l), V::Timestamp(r)) => {
                    cmp_bytes(l.byte_size(), r.byte_size())?
                }
                (V::Timestamp(_), _) => return Err(CompareError::Incomparable),

                (V::Bool(_), V::Bool(_)) => cmp_bytes(1, 1)?,
                (V::Bool(_), _) => return Err(CompareError::Incomparable),

                (V::Mutez(_), V::Mutez(_)) => cmp_bytes(8, 8)?,
                (V::Mutez(_), _) => return Err(CompareError::Incomparable),

                (V::String(l), V::String(r)) => {
                    cmp_bytes(l.len() as u64, r.len() as u64)?
                }
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

                (V::Address(..), V::Address(..)) => {
                    cmp_bytes(ADDRESS_SIZE, ADDRESS_SIZE)?
                }
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
            gas.consume(node)?;
        }
        Ok(())
    }

    /// Cost charged for computing total entries size (needed for the subsequent
    /// gas calculation, so this is meta-gas).
    pub fn concat_list_precheck(list_size: usize) -> Result<u32, CostOverflow> {
        (10 * Checked::from(list_size)).as_gas_cost()
    }

    /// Per-byte allocation gas, charged as `max(time, alloc)` whenever an op
    /// materialises a fresh contiguous `Bytes`/`String` buffer. The coefficient
    /// ties gas to allocated memory so the *aggregate* an operation can fund
    /// stays within the WASM heap:
    ///   `MICHELSON_MAX_MILLIGAS_PER_OPERATION` (660_000_000 mg)
    ///   / WASM linear-memory cap (4 GiB = 4_294_967_296 B)  ≈ 0.1537 mg/byte,
    /// rounded up to `(n>>3)+(n>>5)` = 0.15625, so an operation runs out of gas
    /// before the buffers it materialises can exhaust the 4 GiB heap.
    ///
    /// This bounds the *total* an operation allocates, not the length of any
    /// single value: a `Bytes`/`String` is a `Vec<u8>`, itself capped at
    /// `isize::MAX` (~2 GiB on wasm32); exceeding that is a separate concern.
    fn alloc_cost(bytes: Checked<usize>) -> Result<u32, CostOverflow> {
        ((bytes >> 3) + (bytes >> 5)).as_gas_cost()
    }

    // corresponds to cost_N_IConcat_string in the Tezos protocol (the model's
    // list-length coefficient is 0; the per-element precheck above stays as a
    // MIR-specific protection)
    pub fn concat_string_list(total_len: Checked<usize>) -> Result<u32, CostOverflow> {
        let time = ((total_len >> 4) + (total_len >> 6) + 55).as_gas_cost()?;
        Ok(time.max(alloc_cost(total_len)?))
    }

    // corresponds to cost_N_IConcat_bytes in the Tezos protocol
    pub fn concat_bytes_list(total_len: Checked<usize>) -> Result<u32, CostOverflow> {
        let time = ((total_len >> 4) + (total_len >> 6) + 55).as_gas_cost()?;
        Ok(time.max(alloc_cost(total_len)?))
    }

    // corresponds to cost_N_IConcat_string_pair in the Tezos protocol
    pub fn concat_string_pair(len1: usize, len2: usize) -> Result<u32, CostOverflow> {
        let w = Checked::from(len1) + Checked::from(len2);
        let time = ((w >> 4) + (w >> 5) + (w >> 6) + (w >> 8) + 55).as_gas_cost()?;
        Ok(time.max(alloc_cost(w)?))
    }

    // corresponds to cost_N_IConcat_bytes_pair in the Tezos protocol
    pub fn concat_bytes_pair(len1: usize, len2: usize) -> Result<u32, CostOverflow> {
        let w = Checked::from(len1) + Checked::from(len2);
        let time =
            ((w >> 4) + (w >> 5) + (w >> 6) + (w >> 7) + (w >> 8) + 65).as_gas_cost()?;
        Ok(time.max(alloc_cost(w)?))
    }

    /// Comparable byte size of a key, the size axis of the benchmarked
    /// collection lookup models. Mirrors the protocol's
    /// `Gas_comparable_input_size.size_of_comparable_value`, walked
    /// iteratively.
    fn comparable_size(k: &TypedValue) -> Result<u64, CostOverflow> {
        use TypedValue as V;
        let mut total = Checked::from(0u64);
        let mut stack: Vec<&TypedValue> = vec![k];
        while let Some(v) = stack.pop() {
            let sz: u64 = match v {
                // integer leaves floor, as the protocol's
                // Gas_comparable_input_size.integer (Z.numbits / 8)
                V::Nat(n) => n.bits() / 8,
                V::Int(n) => n.bits() / 8,
                V::Timestamp(t) => t.bits() / 8,
                V::Unit => 1,
                V::Bool(_) => 1,
                V::Mutez(_) => 8,
                V::String(s) => s.len() as u64,
                V::Bytes(b) => b.len() as u64,
                V::Pair(l, r) => {
                    stack.push(l.as_ref());
                    stack.push(r.as_ref());
                    1
                }
                V::Option(o) => {
                    if let Some(x) = o {
                        stack.push(x.as_ref());
                    }
                    1
                }
                V::Or(or) => {
                    match or {
                        Or::Left(x) | Or::Right(x) => stack.push(x.as_ref()),
                    }
                    1
                }
                // public key hash size (tag byte + 20-byte hash) plus the
                // entrypoint length, as in the protocol's `address`
                V::Address(a) => 21 + a.entrypoint.as_bytes().len() as u64,
                V::ChainId(..) => 4,
                V::Key(k) => match k {
                    PublicKey::Ed25519(..) => 33,
                    PublicKey::Secp256k1(..) | PublicKey::P256(..) => 34,
                    PublicKey::Bls(..) => 49,
                },
                V::Signature(s) => match s {
                    crate::ast::Signature::Bls(..) => 96,
                    _ => 64,
                },
                V::KeyHash(_) => 21,
                // non-comparable values cannot be collection keys
                _ => 0,
            };
            total += sz;
        }
        total.ok_or(CostOverflow)
    }

    /// `comparable_size(key) * log2(size + 1)`, the variable of every
    /// benchmarked collection lookup model.
    fn lookup_var(k: &TypedValue, size: usize) -> Result<Checked<u64>, CompareError> {
        let s1 = comparable_size(k)?;
        // the protocol's log2 is 1 + numbits, i.e. ilog2 + 2
        let log = (Checked::from(size as u64) + 1)
            .ok_or(CostOverflow)?
            .ilog2()
            + 2;
        Ok(Checked::from(s1) * (log as u64))
    }

    // corresponds to cost_N_IMap_mem in the Tezos protocol
    pub fn map_mem(k: &TypedValue, map_size: usize) -> Result<u32, CompareError> {
        let w = lookup_var(k, map_size)?;
        Ok(((w >> 4) + (w >> 5) + (w >> 8) + (w >> 9) + 40).as_gas_cost()?)
    }

    // Single model for GET and MEM, as the protocol's map_get = map_mem
    // alias; the dedicated cost_N_IMap_get fit is the same minus its
    // w >> 9 term.
    pub fn map_get(k: &TypedValue, map_size: usize) -> Result<u32, CompareError> {
        map_mem(k, map_size)
    }

    // corresponds to cost_N_ISet_mem in the Tezos protocol
    pub fn set_mem(k: &TypedValue, map_size: usize) -> Result<u32, CompareError> {
        let w = lookup_var(k, map_size)?;
        Ok(((w >> 3) + (w >> 4) + (w >> 5) + (w >> 6) + 40).as_gas_cost()?)
    }

    // corresponds to cost_N_IMap_update in the Tezos protocol
    pub fn map_update(k: &TypedValue, map_size: usize) -> Result<u32, CompareError> {
        let w = lookup_var(k, map_size)?;
        Ok(((w * 4) + (w >> 2) + 55).as_gas_cost()?)
    }

    // corresponds to cost_N_ISet_update in the Tezos protocol
    pub fn set_update(k: &TypedValue, map_size: usize) -> Result<u32, CompareError> {
        let w = lookup_var(k, map_size)?;
        Ok(((w * 3) + (w >> 1) + (w >> 2) + 60).as_gas_cost()?)
    }

    // corresponds to cost_N_IMap_get_and_update in the Tezos protocol
    pub fn map_get_and_update(
        k: &TypedValue,
        map_size: usize,
    ) -> Result<u32, CompareError> {
        let w = lookup_var(k, map_size)?;
        Ok(((w * 4) + (w >> 2) + (w >> 3) + 80).as_gas_cost()?)
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
        // The per-node cost corresponds to the benchmarked
        // N_IPack_micheline_nodes parameter; the per-byte components are not
        // in the re-benchmarked set.
        (size.nodes_num * 125 + size.zariths * 25 + size.str_byte * 10).as_gas_cost()
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

    // correspond to cost_N_ICheck_signature_{ed25519,secp256k1,p256} in the
    // Tezos protocol. BLS is not part of the re-benchmarked set; its arm
    // keeps the previous total (including the former 32 * len serialization
    // surcharge, now folded in).
    // Saturating arithmetic, as the protocol's `Saturation_repr`: a
    // saturated cost exceeds any operation budget and exhausts gas instead
    // of wrapping in release builds.
    pub fn check_signature(k: &PublicKey, msg: &[u8]) -> Result<u32, CryptoError> {
        let len = msg.len().min(u32::MAX as usize) as u32;
        let cost = match k {
            PublicKey::Ed25519(..) => 48_395_u32
                .saturating_add(len >> 4)
                .saturating_add(len >> 5)
                .saturating_add(len),
            PublicKey::Secp256k1(..) => {
                183_265_u32.saturating_add(len >> 3).saturating_add(len)
            }
            PublicKey::P256(..) => {
                487_855_u32.saturating_add(len >> 3).saturating_add(len)
            }
            #[cfg(feature = "bls")]
            PublicKey::Bls(..) => 1_570_000_u32.saturating_add(len.saturating_mul(35)),
            #[cfg(not(feature = "bls"))]
            PublicKey::Bls(..) => {
                return Err(CryptoError::Unsupported(
                    "bls feature disabled, tz4 signature verification not supported",
                ))
            }
        };

        Ok(cost)
    }

    // corresponds to cost_N_ISlice_string in the Tezos protocol
    pub fn slice_string(length: usize) -> Result<u32, CostOverflow> {
        let s = Checked::from(length);
        ((s >> 4) + (s >> 5) + (s >> 6) + 75).as_gas_cost()
    }

    // corresponds to cost_N_ISlice_bytes in the Tezos protocol
    pub fn slice_bytes(length: usize) -> Result<u32, CostOverflow> {
        let s = Checked::from(length);
        ((s >> 4) + (s >> 5) + (s >> 6) + (s >> 9) + 80).as_gas_cost()
    }

    // corresponds to cost_N_IBlake2b in the Tezos protocol
    pub fn blake2b(msg: &[u8]) -> Result<u32, CostOverflow> {
        let size = Checked::from(msg.len());
        ((size >> 4) + (size >> 5) + size + 245).as_gas_cost()
    }

    // corresponds to cost_N_IKeccak in the Tezos protocol
    pub fn keccak(msg: &[u8]) -> Result<u32, CostOverflow> {
        let size = Checked::from(msg.len());
        ((size >> 1) + (size >> 2) + (size >> 3) + (size >> 4) + (size * 2) + 550)
            .as_gas_cost()
    }

    // corresponds to cost_N_ISha256 in the Tezos protocol
    pub fn sha256(msg: &[u8]) -> Result<u32, CostOverflow> {
        let size = Checked::from(msg.len());
        ((size >> 1) + (size >> 2) + (size >> 4) + (size * 3) + 350).as_gas_cost()
    }

    // corresponds to cost_N_ISha3 in the Tezos protocol
    pub fn sha3(msg: &[u8]) -> Result<u32, CostOverflow> {
        let size = Checked::from(msg.len());
        ((size >> 1) + (size >> 2) + (size >> 3) + (size >> 4) + (size * 2) + 545)
            .as_gas_cost()
    }

    // corresponds to cost_N_ISha512 in the Tezos protocol
    pub fn sha512(msg: &[u8]) -> Result<u32, CostOverflow> {
        let size = Checked::from(msg.len());
        ((size >> 2) + (size >> 3) + (size * 2) + 455).as_gas_cost()
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

    // `ISNAT` deep-clones a DUP-shared operand's magnitude in `pop!` -- the same
    // O(width) copy `abs` performs -- so its cost is the flat coercion base
    // (`ISNAT`) plus `abs`'s size term. The size term bounds per-gas work:
    // without it a `DUP; ISNAT` loop would do O(k*width) work for O(k) gas
    // (L2-1794). Diverges from L1's flat cost, as MIR's `MUL` pricing does.
    pub fn isnat(int: &impl BigIntByteSize) -> Result<u32, CostOverflow> {
        let size = Checked::from(int.byte_size());
        (Checked::from(u64::from(ISNAT))
            + (size >> 4)
            + (size >> 6)
            + (size >> 8)
            + (size >> 9))
            .as_gas_cost()
    }

    // `INT(nat)` clones a DUP-shared magnitude in `pop!` like `ISNAT`, so its
    // cost is the flat coercion base (`INT_NAT`) plus `abs`'s size term.
    pub fn int_nat(int: &impl BigIntByteSize) -> Result<u32, CostOverflow> {
        let size = Checked::from(int.byte_size());
        (Checked::from(u64::from(INT_NAT))
            + (size >> 4)
            + (size >> 6)
            + (size >> 8)
            + (size >> 9))
            .as_gas_cost()
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

    // corresponds to cost_N_IUnpack in the Tezos protocol. The model is
    // flat: the per-byte work is carried by the carbonated Micheline decode
    // and typecheck that UNPACK also charges.
    pub fn unpack(_bytes: &[u8]) -> Result<u32, CostOverflow> {
        Ok(140)
    }

    // corresponds to Interp_costs.unpack_failed in the Tezos protocol: the
    // size-dependent penalty charged when a packed (0x05) payload fails to
    // decode or typecheck. `len` excludes the 0x05 tag. alloc_mbytes_cost(1) =
    // 30000, alloc_cost(3) + step_cost(1) = 9000 (milligas).
    pub fn unpack_failed(len: usize) -> Result<u32, CostOverflow> {
        let n = Checked::from(len);
        // d = Z.numbits len (significant bits; 0 for len = 0)
        let d = Checked::from((usize::BITS - len.leading_zeros()) as usize);
        (n * 30000 + n * d * 9000).as_gas_cost()
    }

    pub fn pair_n(size: usize) -> Result<u32, CostOverflow> {
        // corresponds to cost_N_IComb in the Tezos protocol; the protocol's
        // S.sub floors at 0, hence saturating_sub
        let v0 = Checked::from(size.saturating_sub(2));
        (50 + (v0 * 15)).as_gas_cost()
    }

    pub fn unpair_n(size: usize) -> Result<u32, CostOverflow> {
        // corresponds to cost_N_IUncomb in the Tezos protocol; the protocol's
        // S.sub floors at 0, hence saturating_sub
        let v0 = Checked::from(size.saturating_sub(2));
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
    use super::{AsGasCost, BigInt, BigIntByteSize, BigUint, CostOverflow};
    use crate::ast::annotations::Annotation;
    use checked::Checked;

    /// Cost for allocating a Micheline node: 125 mg (the benchmarked
    /// N_IPack_micheline_nodes parameter).
    pub const NODE: u32 = 125;

    /// Cost for allocating a Micheline Int node: 125 mg + 25 mg/byte.
    pub fn int(i: &BigInt) -> Result<u32, CostOverflow> {
        int_by_size(i.byte_size())
    }

    fn int_by_size(size: u64) -> Result<u32, CostOverflow> {
        let size = Checked::from(size);
        (125 + size * 25).as_gas_cost()
    }

    /// Cost for allocating a non-negative Micheline Int node without first
    /// converting the source [`BigUint`] into an owned [`BigInt`].
    pub fn nat(i: &BigUint) -> Result<u32, CostOverflow> {
        int_by_size(i.byte_size())
    }

    /// Cost for allocating a Micheline String node: 125 mg + 10 mg/byte
    pub fn string(string: &str) -> Result<u32, CostOverflow> {
        let size = Checked::from(string.len());
        (125 + size * 10).as_gas_cost()
    }

    /// Cost for allocating a Micheline Bytes node: 125 mg + 10 mg/byte
    pub fn bytes(bytes: &[u8]) -> Result<u32, CostOverflow> {
        let size = Checked::from(bytes.len());
        (125 + size * 10).as_gas_cost()
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
                let mut cost_gas = Gas::default();
                interpret_cost::compare(&mut cost_gas, &a, &b).expect("comparable");
                let cost =
                    Gas::default().milligas().unwrap() - cost_gas.milligas().unwrap();
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
    fn compare_shared_dag_is_gas_bounded() {
        // L2-1654: a value whose in-memory DAG expands to a 2^n-node tree
        // (shared sub-terms, as built by `DUP; PAIR`) must run out of gas
        // mid-walk instead of walking the whole 2^n expansion before any gas is
        // charged. 40 shared pair levels = 40 nodes in memory but 2^40 expanded
        // leaves; with a small budget the metered COMPARE OOGs after ~budget
        // nodes (so this test terminates) instead of hanging on 2^40 nodes.
        use crate::ast::TypedValue as V;
        use std::rc::Rc;
        let mut shared = Rc::new(V::int(0));
        for _ in 0..40 {
            shared = Rc::new(V::new_pair_rc(shared.clone(), shared.clone()));
        }
        let mut gas = Gas::new(10_000);
        assert_eq!(
            interpret_cost::compare(&mut gas, shared.as_ref(), shared.as_ref()),
            Err(CompareError::OutOfGas(OutOfGas))
        );
    }

    #[test]
    fn mul_gas_grows_quadratically_l2_1691() {
        // L2-1691: MUL gas must grow ~quadratically in operand size (product
        // of sizes), not ~linearly (the old a*log(a) FFT formula), so it
        // upper-bounds num-bigint's no-FFT (schoolbook/Karatsuba/Toom-3) work
        // and the DUP;MUL squaring witness OOGs while real CPU is still bounded.
        use num_bigint::BigUint;
        let big = |bytes: usize| BigUint::from_bytes_le(&vec![0xffu8; bytes]);
        let g = |s| interpret_cost::mul_nat(&big(s), &big(s)).unwrap();
        let g1 = g(4096);
        let g2 = g(8192);
        // Doubling each operand quadruples the cost (~4x); the old a*log(a)
        // formula only ~doubled it. >= 3.5x pins the quadratic shape.
        assert!(
            g2 >= g1 * 7 / 2,
            "doubling operand size should ~4x MUL gas: g(4096)={g1} g(8192)={g2}"
        );
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
