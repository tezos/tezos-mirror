// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::{cell::Cell, rc::Rc};

use tezos_crypto_rs::hash::OperationHash;
use tezos_evm_runtime::runtime_keyspaces::RuntimeKeyspaces;
use tezos_evm_runtime::snapshot::SnapshotError;
use tezos_smart_rollup_keyspace::KeySpaceLoader;
use tezos_tezlink::{
    block::AppliedOperation,
    operation_result::{
        InternalOperationSum, OperationDataAndMetadata, OperationResultSum,
    },
};

/// Walk an `AppliedOperation` and transform every `Applied` result —
/// top-level and every internal op — to `BackTracked`.  Used by
/// `MichelsonJournal::revert_frame` when migrating receipts from the
/// pending list to the backtracked list.
///
/// `Failed` and `Skipped` results are left untouched (the dedicated
/// `transform_result_backtrack` helpers no-op on non-Applied
/// variants).  Origination/Reveal top-levels are not produced by the
/// CRAC receipt builder but are handled defensively in case the
/// shape evolves.
fn backtrack_receipt(receipt: &mut AppliedOperation) {
    let OperationDataAndMetadata::OperationWithMetadata(ref mut batch) =
        receipt.op_and_receipt;
    for op in batch.operations.iter_mut() {
        op.receipt.transform_result_backtrack();
        if let OperationResultSum::Transfer(ref mut result) = op.receipt {
            for iop in result.internal_operation_results.iter_mut() {
                iop.transform_result_backtrack();
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct ExternalCheckpoint {
    /// `savepoints` at the time the checkpoint was created.
    savepoint_watermark: usize,
    /// Length of `pending_crac_receipts` at the time the checkpoint was
    /// created.  On revert, receipts are truncated back to this count.
    receipt_count: usize,
    /// `origination_index` at the time the checkpoint was created.
    /// On revert, the index is rolled back to this value so that
    /// `CREATE_CONTRACT` slots consumed inside the reverted EVM frame
    /// are freed for reuse by subsequent inbound CRACs.  On commit
    /// the saved value is dropped (the index already reflects the
    /// post-frame state).
    origination_index: u32,
    /// Length of `pending_alias_origination_internals` at the time the
    /// checkpoint was created. On revert the list is truncated back to
    /// this count; on commit the saved value is dropped.
    alias_internals_count: usize,
}

/// Error returned by [`MichelsonJournal::set_dispatch_result`].
#[derive(Debug, PartialEq, Eq)]
pub enum DispatchSlotError {
    /// No dispatch slot is open — there is nothing to write to.
    NoSlot,
    /// The current dispatch slot already holds a result
    /// (once-per-dispatch invariant).
    AlreadySet,
}

impl core::fmt::Display for DispatchSlotError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::NoSlot => f.write_str("no active dispatch slot"),
            Self::AlreadySet => f.write_str("dispatch result already set"),
        }
    }
}

/// A single dispatch slot: the `%collect_result` payload, plus the
/// address of the contract that owns the slot.
///
/// The owner is the serve target — the contract whose code the current
/// `serve` invocation is running. Only that contract's own
/// `%collect_result` may write the slot; `owner: None` means the slot
/// has not been assigned yet (e.g. the view/GET path, which never
/// assigns an owner because views cannot emit `%collect_result`).
///
/// Stored as the canonical `AddressHash::to_bytes` encoding rather
/// than the typed Michelson address: the journal only ever stores the
/// owner and compares it for equality, so it has no need to depend on
/// the Michelson interpreter crate to interpret it.
#[derive(Debug, PartialEq, Eq, Default)]
struct DispatchSlot {
    owner: Option<Vec<u8>>,
    payload: Option<Vec<u8>>,
}

impl std::error::Error for DispatchSlotError {}

/// Michelson side of the cross-runtime journal of a transaction.
///
/// # Snapshot, savepoint, checkpoint
///
/// The journal undoes the Michelson account writes of a reverted call. Three
/// names cover it, from storage up to the journal API:
///
/// - **Snapshot**: the [`Snapshot`](tezos_evm_runtime::snapshot::Snapshot)
///   held by a [`RuntimeKeyspaces`] for its `/tez/tez_accounts`. It is a
///   stack of backups of that keyspace, not a backup itself. There is one
///   per `RuntimeKeyspaces`: the transaction's and, during a Tezos operation
///   batch, the one of the batch's `SafeStorage` mirror, nested in the
///   transaction's.
/// - **Savepoint**: one backup on a snapshot's stack, a copy of the accounts
///   taken at some point (`/__savepoint/<index><root>`). Released, it is
///   deleted and the writes made since are kept. Rolled back, it is moved
///   back over the accounts. `savepoints` counts those open across every
///   snapshot the journal drives, oldest first.
/// - **Checkpoint**: the journal-level operation that opens a savepoint
///   and returns its index ([`Self::checkpoint`]), to commit or revert it
///   later ([`Self::checkpoint_commit`], [`Self::checkpoint_revert`]). The
///   names mirror the EVM journal's checkpoints, and an *external*
///   checkpoint is an EVM call frame: it records the savepoint count as
///   its watermark, with the receipt and counter state that
///   [`Self::commit_frame`] / [`Self::revert_frame`] keep or roll back.
#[derive(Debug, PartialEq, Eq)]
pub struct MichelsonJournal {
    /// Number of savepoints opened by the checkpoint functions and not yet
    /// released or rolled back, across the accounts snapshots of the scopes
    /// ended by [`Self::release_all_savepoints`]: the transaction's and, inside
    /// it, a Tezos operation batch's. Callers must route every savepoint of a
    /// snapshot through these functions to keep the two in sync.
    savepoints: usize,
    external_checkpoints: Vec<ExternalCheckpoint>,
    /// Per-Michelson-dispatch slot stack for the `%collect_result`
    /// payload.  Pushed and popped exclusively by `TezosRuntime::serve`,
    /// independently of REVM's `external_checkpoints` stack: the slot's
    /// lifecycle is Michelson-dispatch-shaped (at most one deposit per
    /// `serve` invocation) whereas `external_checkpoints` is shaped by
    /// EVM call boundaries.
    dispatch_result_slots: Vec<DispatchSlot>,
    /// Currently-applied CRAC receipts (EVM → Michelson).  Subject to
    /// revert: when the calling EVM frame reverts, `revert_frame`
    /// drains the entries pushed during the frame, transforms every
    /// status to `BackTracked`, and migrates them to
    /// `backtracked_crac_receipts` so the receipt survives the merge
    /// — matching L1 manager-op semantics where applied internals
    /// reverted by a later failure appear as `backtracked` rather
    /// than vanishing.
    /// Each entry is `(seq, receipt)` where `seq` is a monotonic
    /// counter shared with the failed and backtracked lists, used to
    /// recover cross-list execution order at drain time.
    pub pending_crac_receipts: Vec<(u64, AppliedOperation)>,
    /// Failed CRAC receipts.  NOT subject to revert: a failed CRAC is
    /// recorded even when the EVM transaction reverts entirely, so that
    /// the Michelson block shows the attempt with `status: failed`.
    pub failed_crac_receipts: Vec<(u64, AppliedOperation)>,
    /// Reverted-but-recorded CRAC receipts.  Receipts of CRACs that
    /// were Applied at execution time but whose enclosing EVM frame
    /// later reverted: state effects are still rolled back via the
    /// snapshot mechanism, but the receipt is kept (every result
    /// transformed to `BackTracked`) so the merged synthetic
    /// Michelson manager-op records what was attempted.  Like
    /// `failed_crac_receipts`, NOT subject to further revert: once
    /// migrated here, a subsequent inner revert does not re-delete.
    pub backtracked_crac_receipts: Vec<(u64, AppliedOperation)>,
    /// Monotonic counter assigned to each CRAC receipt at push time,
    /// shared across pending, failed, and backtracked lists so that
    /// merging them recovers execution order.
    next_receipt_seq: u64,
    /// Internal `Origination` ops produced by alias-forwarder
    /// materialization (`ensure_alias` branch 3) that have not yet
    /// been folded into a CRAC receipt. The Tezos runtime's
    /// `execute_entrypoint_call` drains this list at the start of
    /// each cross-runtime call it serves, so each CRAC receipt only
    /// picks up the aliases the gateway precompile materialized for
    /// IT (the aliases of its sender and source). Nested CRACs
    /// work naturally: each inner call drains the entries pushed
    /// for its own precompile invocation, leaving any outer-CRAC
    /// entries that were drained at the outer's entry point alone.
    /// Nonces here are 0 placeholders; `renumber_nonces` rewrites
    /// them block-wide from the flat list order at block
    /// finalization.
    pending_alias_origination_internals: Vec<InternalOperationSum>,
    /// Operation hash of this Michelson manager operation — the seed
    /// half of the operation's single origination nonce.  Set once at
    /// construction and never mutated thereafter; read through
    /// [`Self::operation_hash`].  A child KT1 originated by a
    /// `CREATE_CONTRACT` is `digest_160(operation_hash[32] ||
    /// index[4])`, so this value MUST be deterministic AND unique: two
    /// child KT1s may collide only when their indices collide, never
    /// across two distinct operations.  For a Tezos-entered operation
    /// this is the real `Operation::hash()`, matching L1; for an
    /// EVM-entered synthetic operation — which has no real hash — see
    /// [`crate::TezosXJournal::synthetic_operation_hash`].
    operation_hash: OperationHash,
    /// Index half of the operation's single origination nonce:
    /// monotonic, incremented once per `CREATE_CONTRACT`, whether the
    /// origination is native Michelson or reached through an inbound
    /// CRAC.  This journal is the *sole* owner — indices are claimed
    /// through [`Self::next_origination_index`], never copied into a
    /// local and written back — so nested and sequential frames alike
    /// (`EVM → Michelson → EVM → Michelson`, or a native origination
    /// after a CRAC-reached one) draw from one sequence and cannot
    /// collide on a child KT1.  Rolled back by `revert_frame` for
    /// EVM-driven frames, and by
    /// [`Self::restore_origination_index`] on the native path, which
    /// opens no journal frame.
    origination_index: u32,
    /// Monotonic MIR internal-operation counter — the replay identity
    /// L1 enforces for internal operations (`Internal_operation_replay`).
    /// Persisted across reentrant inbound Michelson frames within the
    /// same synthetic manager operation so that, in an
    /// `EVM → Michelson → EVM → Michelson` chain, each Michelson frame
    /// resumes from the previous frame's value instead of restarting
    /// from 0.  Without this, two internal operations in distinct
    /// reentrant frames would share the same MIR counter, breaking L1's
    /// single internal-nonce namespace for the manager operation's
    /// internal-op tree (L2-1676).  Threaded exactly like
    /// `origination_index`: read with
    /// [`internal_operation_counter`](Self::internal_operation_counter),
    /// written back with
    /// [`set_internal_operation_counter`](Self::set_internal_operation_counter),
    /// kept on both `revert_frame` and `commit_frame`.
    ///
    /// It is also the position checked against the per-block
    /// internal-operation cap: it counts the synthetic entries of CRAC
    /// receipts too, and it is never rolled back because receipts of
    /// reverted frames stay in the block.
    internal_operation_counter: u128,
    /// Temporary big map IDs of this manager operation, shared by every
    /// one of its Michelson frames.
    ///
    /// Deliberately *not* captured in [`ExternalCheckpoint`]: the
    /// counter only ever moves forward, even when a frame reverts.
    temporary_big_map_id_allocator: TemporaryBigMapIdAllocator,
}

/// Counter value while a manager operation has been handed out no
/// temporary big map ID. Temporary IDs are negative and progress away
/// from 0: the opposite of permanent IDs.
const NO_TEMPORARY_BIG_MAP_ID: i64 = 0;

/// Handle on a manager operation's temporary big map ID counter, cloned
/// into every context that allocates temporary big maps for it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TemporaryBigMapIdAllocator(Rc<Cell<i64>>);

impl TemporaryBigMapIdAllocator {
    /// A fresh counter, no ID handed out yet.
    pub fn new() -> Self {
        Self(Rc::new(Cell::new(NO_TEMPORARY_BIG_MAP_ID)))
    }

    /// Hand out the next temporary big map ID.
    pub fn allocate(&self) -> i64 {
        let id = self.0.get() - 1;
        self.0.set(id);
        id
    }

    /// The last ID handed out, [`NO_TEMPORARY_BIG_MAP_ID`] — which is
    /// not a temporary ID — while there is none.
    pub fn last_allocated(&self) -> i64 {
        self.0.get()
    }
}

impl Default for TemporaryBigMapIdAllocator {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for MichelsonJournal {
    /// `Default` constructs the journal with `OperationHash::default()`
    /// (32 zero bytes) as the operation hash.  This is **not safe for
    /// production**: every inbound-CRAC `CREATE_CONTRACT` will collide
    /// on the same KT1.  Use [`MichelsonJournal::new`] in production
    /// with a deterministically-derived seed; the `Default` impl
    /// exists purely to keep tests that don't exercise origination
    /// paths terse.
    fn default() -> Self {
        Self::new(OperationHash::default())
    }
}

impl MichelsonJournal {
    /// Construct a fresh Michelson journal for one Michelson manager
    /// operation.  [`operation_hash`] seeds the operation's single
    /// origination nonce, shared by native and CRAC-reached
    /// originations alike; it MUST be deterministic and unique per
    /// operation.  Pass the real `Operation::hash()` when the operation
    /// is Tezos-entered; for an EVM-entered synthetic operation, which
    /// has no real hash, derive it with
    /// [`crate::TezosXJournal::synthetic_operation_hash`].
    pub fn new(operation_hash: OperationHash) -> Self {
        Self {
            savepoints: 0,
            external_checkpoints: Vec::new(),
            dispatch_result_slots: Vec::new(),
            pending_crac_receipts: Vec::new(),
            failed_crac_receipts: Vec::new(),
            backtracked_crac_receipts: Vec::new(),
            next_receipt_seq: 0,
            pending_alias_origination_internals: Vec::new(),
            operation_hash,
            origination_index: 0,
            internal_operation_counter: 0,
            temporary_big_map_id_allocator: TemporaryBigMapIdAllocator::new(),
        }
    }

    /// Claim the next execution-order sequence number.  `u64` overflow
    /// would require 2^64 CRAC receipts in a single block and is not
    /// representable in practice — use `checked_add` so the invariant
    /// is explicit rather than silently saturating (which would tie
    /// sequence numbers together and break the drain-time sort).
    fn claim_receipt_seq(&mut self) -> u64 {
        let seq = self.next_receipt_seq;
        self.next_receipt_seq = self
            .next_receipt_seq
            .checked_add(1)
            .expect("cross-runtime call receipt sequence counter overflowed u64");
        seq
    }

    /// Number of EVM frames still open. Non-zero at the end of an operation
    /// means a frame leaked its checkpoint, which mis-points the CRAC receipt
    /// watermarks. Reported by `close_tezosx_journal`.
    pub fn open_frame_count(&self) -> usize {
        self.external_checkpoints.len()
    }

    /// Push a successful CRAC receipt, tagging it with the next
    /// execution-order sequence number.  The invariant that the
    /// `pending` list holds only Applied top-level receipts (and
    /// `failed` only Failed ones) is load-bearing for the post-merge
    /// top-level reconciliation in `drain_pending_crac_receipts`.
    pub fn push_pending_crac_receipt(&mut self, receipt: AppliedOperation) {
        let seq = self.claim_receipt_seq();
        self.pending_crac_receipts.push((seq, receipt));
    }

    /// Push a failed CRAC receipt, tagging it with the next
    /// execution-order sequence number.  See `push_pending_crac_receipt`
    /// for the pending-vs-failed invariant.
    pub fn push_failed_crac_receipt(&mut self, receipt: AppliedOperation) {
        let seq = self.claim_receipt_seq();
        self.failed_crac_receipts.push((seq, receipt));
    }

    /// Record an internal `Origination` op produced by alias-forwarder
    /// materialization. The Tezos runtime drains this list at the
    /// start of each cross-runtime call and folds the entries into
    /// the CRAC receipt that call eventually emits.
    pub fn push_pending_alias_origination_internal(
        &mut self,
        internal: InternalOperationSum,
    ) {
        self.pending_alias_origination_internals.push(internal);
    }

    /// Drain the pending alias-forwarder origination internal ops in
    /// FIFO order. Called by the Tezos runtime at the entry of every
    /// cross-runtime call it serves, so each CRAC receipt only
    /// inherits the aliases its gateway precompile materialized for
    /// it. Drain-on-entry is what scopes outer-CRAC aliases out of
    /// any inner CRAC the outer call may spawn.
    pub fn take_pending_alias_origination_internals(
        &mut self,
    ) -> Vec<InternalOperationSum> {
        std::mem::take(&mut self.pending_alias_origination_internals)
    }

    /// Number of alias originations pending, to be passed to
    /// [`Self::take_pending_alias_origination_internals_from`].
    pub fn pending_alias_origination_count(&self) -> usize {
        self.pending_alias_origination_internals.len()
    }

    /// Take the alias originations pushed since `watermark` (a
    /// [`Self::pending_alias_origination_count`]), leaving the earlier ones
    /// to whoever took that watermark first.
    pub fn take_pending_alias_origination_internals_from(
        &mut self,
        watermark: usize,
    ) -> Vec<InternalOperationSum> {
        let start = watermark.min(self.pending_alias_origination_internals.len());
        self.pending_alias_origination_internals
            .drain(start..)
            .collect()
    }

    /// Operation hash of this synthetic Michelson manager operation.
    /// Set once at construction and stable for the journal's life.
    pub fn operation_hash(&self) -> &OperationHash {
        &self.operation_hash
    }

    /// Current origination-nonce index — the count of
    /// `CREATE_CONTRACT`s already performed in this Michelson manager
    /// operation, native and CRAC-reached alike.  Combined with
    /// [`Self::operation_hash`] to derive the next child KT1 as
    /// `digest_160(operation_hash || index)`.
    ///
    /// Read this only to snapshot the value; claim indices with
    /// [`next_origination_index`](Self::next_origination_index) so no
    /// caller ever holds a stale local copy.
    pub fn origination_index(&self) -> u32 {
        self.origination_index
    }

    /// Restore the origination-nonce index to a previously snapshotted
    /// value.  Needed on the native Michelson path, which reverts
    /// through `SafeStorage` without opening a journal frame, so the
    /// checkpoint machinery that rolls this back for EVM-driven frames
    /// does not fire: a backtracked batch must not consume indices, as
    /// on L1 the nonce dies with the reverted context.
    pub fn restore_origination_index(&mut self, index: u32) {
        self.origination_index = index;
    }

    /// Claim the next origination-nonce index, use-then-increment like
    /// L1 (`raw_context.ml increment_origination_nonce`): the first
    /// `CREATE_CONTRACT` of the operation gets index 0.
    ///
    /// This is the single source of truth for the whole manager
    /// operation.  Native originations and those reached through an
    /// inbound CRAC claim from the same sequence, so an
    /// `EVM → Michelson → EVM → Michelson` chain — or a native
    /// origination following a CRAC-reached one — can never collide on
    /// a child KT1.  Claiming (rather than a read/write-back pair) is
    /// what makes that safe under nesting: an outer frame holding a
    /// local copy while an inner frame bumps the journal is exactly the
    /// collision this avoids.
    ///
    /// `u32` overflow would need 2^32 originations in a single manager
    /// operation, which no gas limit permits — use `checked_add` so the
    /// invariant is explicit rather than saturating, which would tie two
    /// originations to the same index and collide their KT1s.
    pub fn next_origination_index(&mut self) -> u32 {
        let current = self.origination_index;
        self.origination_index = self
            .origination_index
            .checked_add(1)
            .expect("origination nonce index overflowed u32");
        current
    }

    /// Current MIR internal-operation counter — the highest replay
    /// identity assigned to an internal operation so far in this
    /// synthetic manager operation.  An inbound CRAC handler resumes
    /// from this value so that internal operations keep a monotonic,
    /// collision-free identity across reentrant Michelson frames,
    /// matching L1's single internal-nonce namespace (L2-1676).  Pair
    /// every read with
    /// [`set_internal_operation_counter`](Self::set_internal_operation_counter)
    /// to write the post-execution value back.
    pub fn internal_operation_counter(&self) -> u128 {
        self.internal_operation_counter
    }

    /// Persist `counter` as the new MIR internal-operation counter,
    /// called by the inbound-CRAC handler at the end of an
    /// `execute_entrypoint_call` to record the highest counter the
    /// frame's Michelson execution reached.  The next reentrant
    /// Michelson frame resumes from this value rather than restarting
    /// from 0 (L2-1676).
    pub fn set_internal_operation_counter(&mut self, counter: u128) {
        self.internal_operation_counter = counter;
    }

    /// Handle on the operation-wide temporary big map ID counter, to be
    /// cloned into every `TcCtx` built for this manager operation, so
    /// all its frames — native and inbound CRAC alike — allocate from
    /// one sequence.
    pub fn temporary_big_map_id_allocator(&self) -> TemporaryBigMapIdAllocator {
        self.temporary_big_map_id_allocator.clone()
    }
}

impl MichelsonJournal {
    // Called by an external journal on checkpoint creation.
    // Records the current savepoint count, receipt count and
    // origination-nonce index as the lower boundary for this call
    // frame.
    pub fn push_external_checkpoint(&mut self) {
        self.external_checkpoints.push(ExternalCheckpoint {
            savepoint_watermark: self.savepoints,
            receipt_count: self.pending_crac_receipts.len(),
            origination_index: self.origination_index,
            alias_internals_count: self.pending_alias_origination_internals.len(),
        });
    }

    /// Pop the checkpoint of the open EVM frame, or an all-zero one if
    /// none is open.
    fn pop_external_checkpoint(&mut self) -> ExternalCheckpoint {
        self.external_checkpoints
            .pop()
            .unwrap_or(ExternalCheckpoint {
                savepoint_watermark: 0,
                receipt_count: 0,
                origination_index: 0,
                alias_internals_count: 0,
            })
    }

    /// Undo the journal bookkeeping of a reverted EVM frame — everything
    /// but the durable state, which the caller restores from a savepoint.
    fn backtrack_frame(&mut self, checkpoint: &ExternalCheckpoint) {
        // Roll the origination-nonce index back to its value at
        // checkpoint entry: any `CREATE_CONTRACT` performed inside the
        // reverted EVM frame is undone (the child KT1 was never
        // durably written, since Michelson is all-or-nothing inside a
        // CRAC and the savepoint rollback also reverts the durable
        // state), and the freed indices are reusable by the next CRAC.
        // Done unconditionally so a partial inbound CRAC followed by a
        // caller-side EVM revert does not leak burned indices.
        self.origination_index = checkpoint.origination_index;
        // The MIR internal-operation counter is NOT rolled back: the
        // receipts of the reverted frame stay in the block (as
        // backtracked or failed), so their entries must keep counting
        // towards the per-block internal-operation cap.
        self.pending_alias_origination_internals
            .truncate(checkpoint.alias_internals_count);
        // Drain (not truncate) the CRAC receipts pushed during this
        // frame, transform their statuses to `BackTracked`, and
        // migrate them to the backtracked list.  Like
        // `failed_crac_receipts`, the backtracked list is NOT subject
        // to further revert: once migrated, a subsequent inner revert
        // does not re-delete the entry.
        // Clamped: `receipt_count` can outlive the list.
        let drain_from = checkpoint
            .receipt_count
            .min(self.pending_crac_receipts.len());
        let drained: Vec<(u64, AppliedOperation)> =
            self.pending_crac_receipts.drain(drain_from..).collect();
        for (seq, mut receipt) in drained {
            backtrack_receipt(&mut receipt);
            self.backtracked_crac_receipts.push((seq, receipt));
        }
    }

    /// Open a fresh dispatch slot for the current `serve` invocation.
    /// Paired with [`take_dispatch_result`]; not touched by REVM
    /// checkpoints. The slot has no owner until
    /// [`set_current_dispatch_owner`] assigns one.
    pub fn push_dispatch_slot(&mut self) {
        self.dispatch_result_slots.push(DispatchSlot::default());
    }

    /// Assign the owner of the topmost dispatch slot: the contract
    /// whose code is about to run for this `serve` invocation. Only
    /// this address's own `%collect_result` may write the slot.
    ///
    /// A no-op if no slot is open — a POST request always has one
    /// open at this point, so an empty stack here is a kernel bug,
    /// not a state this method needs to surface as an error.
    pub fn set_current_dispatch_owner(&mut self, owner: Vec<u8>) {
        if let Some(top) = self.dispatch_result_slots.last_mut() {
            top.owner = Some(owner);
        }
    }

    /// The owner of the topmost dispatch slot, if any. `None` if no
    /// slot is open, or if the slot's owner hasn't been assigned yet.
    pub fn dispatch_slot_owner(&self) -> Option<&[u8]> {
        self.dispatch_result_slots.last()?.owner.as_deref()
    }

    /// Whether a dispatch slot is currently open. Lets a caller
    /// distinguish "no slot at all" (an unbalanced call outside
    /// `serve` — a kernel bug) from "a slot is open but its owner
    /// doesn't match" (an unauthorized `%collect_result`, expected to
    /// happen under normal operation and handled by dropping the
    /// write rather than erroring).
    pub fn has_dispatch_slot(&self) -> bool {
        !self.dispatch_result_slots.is_empty()
    }

    /// Deposit the `%collect_result` payload on the topmost dispatch
    /// slot.
    ///
    /// Fails with [`DispatchSlotError::NoSlot`] if no dispatch slot
    /// is open (i.e. called outside `serve`), or
    /// [`DispatchSlotError::AlreadySet`] if the current slot already
    /// holds a payload (once-per-dispatch invariant).
    ///
    /// Identity-agnostic: this method does not check the caller
    /// against the slot's owner. The owner check is the
    /// `%collect_result` handler's responsibility (via
    /// [`dispatch_slot_owner`]); the view (GET) path calls this
    /// method directly with no sender to authenticate.
    pub fn set_dispatch_result(
        &mut self,
        bytes: Vec<u8>,
    ) -> Result<(), DispatchSlotError> {
        let top = self
            .dispatch_result_slots
            .last_mut()
            .ok_or(DispatchSlotError::NoSlot)?;
        if top.payload.is_some() {
            return Err(DispatchSlotError::AlreadySet);
        }
        top.payload = Some(bytes);
        Ok(())
    }

    /// Pop the topmost dispatch slot and return whatever was deposited.
    ///
    /// `Ok(Some(_))` — the slot held a payload.
    /// `Ok(None)` — the slot was open but never written to.
    /// `Err(NoSlot)` — no slot was open: an unbalanced call, paired
    /// neither with a prior `push_dispatch_slot` nor following an
    /// already-consumed slot. A kernel bug; surfaced as `Err` rather
    /// than collapsed into `None` so the caller can distinguish
    /// "no `%collect_result`" from "no slot at all".
    pub fn take_dispatch_result(&mut self) -> Result<Option<Vec<u8>>, DispatchSlotError> {
        self.dispatch_result_slots
            .pop()
            .ok_or(DispatchSlotError::NoSlot)
            .map(|slot| slot.payload)
    }

    /// Release every savepoint still open on the snapshot of `rk`,
    /// the one the checkpoint functions below open theirs on: keep
    /// the writes made since, delete the backups, and count the open
    /// savepoints back to those of the enclosing snapshots. Savepoint
    /// indices and frame watermarks keep counting from the savepoints
    /// still open on an enclosing snapshot, e.g. the transaction's
    /// while a Tezos operation batch runs on its mirror.
    ///
    /// Whoever runs a scope on `rk` (a transaction, a Tezos operation batch
    /// on its mirror) calls it at the end, on every path, error paths
    /// included. Otherwise the journal would keep counting these savepoints,
    /// and every later index, watermark and release would be off by that
    /// many.
    ///
    /// Fails `UntrackedSavepoints` if the snapshot of `rk` holds more
    /// savepoints than the journal counts. Its savepoints are released
    /// anyway, and the count is left as is.
    pub fn release_all_savepoints<Host: KeySpaceLoader>(
        &mut self,
        rk: &mut RuntimeKeyspaces<'_, Host>,
    ) -> Result<(), SnapshotError> {
        let enclosing = self.enclosing_savepoints(rk);
        rk.tez_accounts_release_all();
        self.savepoints = enclosing?;
        Ok(())
    }

    // Called by EVM journal on checkpoint commit.
    //
    // Pops the current frame's watermark. If a parent EVM frame exists,
    // leaves the savepoint at the watermark for the parent's potential
    // revert. If this is the outermost frame, releases it too.
    // Receipts are kept (commit preserves them).
    pub fn commit_frame<Host: KeySpaceLoader>(
        &mut self,
        rk: &mut RuntimeKeyspaces<'_, Host>,
    ) -> Result<(), SnapshotError> {
        let checkpoint = self.pop_external_checkpoint();
        // The saved origination index is dropped on commit: any
        // increments performed during this frame are kept, so a later
        // CRAC in the same synthetic op continues from the post-frame
        // value rather than re-using already-burned indices.
        let _ = checkpoint.origination_index;
        let _ = checkpoint.alias_internals_count;
        // The savepoint at the watermark is left for the parent frame, if any.
        let height = if self.external_checkpoints.is_empty() {
            checkpoint.savepoint_watermark
        } else {
            checkpoint.savepoint_watermark + 1
        };
        self.release_to(rk, height)
    }

    // Called by EVM journal on checkpoint revert.
    //
    // Pops the current frame's watermark, restores the accounts of `rk`
    // to their state at the start of this call frame, and migrates
    // applied CRAC receipts pushed during this frame to the
    // backtracked list with every result transformed to `BackTracked`.
    // The state effects of those CRACs are rolled back via the frame's
    // savepoint — the receipt is kept purely as an indexer record of
    // what was attempted (matching L1 manager-op semantics where
    // applied internals reverted by a later failure appear as
    // `backtracked` rather than vanishing).
    pub fn revert_frame<Host: KeySpaceLoader>(
        &mut self,
        rk: &mut RuntimeKeyspaces<'_, Host>,
    ) -> Result<(), SnapshotError> {
        let checkpoint = self.pop_external_checkpoint();
        self.backtrack_frame(&checkpoint);
        if checkpoint.savepoint_watermark >= self.savepoints {
            return Ok(());
        }
        self.release_to(rk, checkpoint.savepoint_watermark + 1)?;
        self.rollback(rk)
    }

    // Called by Michelson CRAC logic.
    //
    // Backs up the accounts of `rk` in a new savepoint before
    // execution and returns an index used to commit or revert.
    pub fn checkpoint<Host: KeySpaceLoader>(
        &mut self,
        rk: &mut RuntimeKeyspaces<'_, Host>,
    ) -> Result<usize, SnapshotError> {
        let new_index = self.savepoints;
        rk.tez_accounts_savepoint()?;
        self.savepoints += 1;
        Ok(new_index)
    }

    // Called by Michelson CRAC logic.
    //
    // Releases the savepoints opened after `checkpoint_index`, keeping the
    // one at `checkpoint_index` so a parent caller can still revert through
    // it. Fails `NoSavepoint`, leaving everything untouched, if the accounts
    // snapshot of `rk` does not hold that savepoint, as
    // `checkpoint_revert` does.
    pub fn checkpoint_commit<Host: KeySpaceLoader>(
        &mut self,
        rk: &mut RuntimeKeyspaces<'_, Host>,
        checkpoint_index: usize,
    ) -> Result<(), SnapshotError> {
        self.check_open(rk, checkpoint_index)?;
        self.release_to(rk, checkpoint_index + 1)
    }

    // Called by Michelson CRAC logic.
    //
    // Releases the savepoints opened after `checkpoint_index`, then rolls
    // the accounts of `rk` back to the savepoint at `checkpoint_index`.
    // Fails `NoSavepoint`, leaving everything untouched, if the accounts
    // snapshot of `rk` does not hold that savepoint: it was released
    // already, or it belongs to an enclosing snapshot.
    pub fn checkpoint_revert<Host: KeySpaceLoader>(
        &mut self,
        rk: &mut RuntimeKeyspaces<'_, Host>,
        checkpoint_index: usize,
    ) -> Result<(), SnapshotError> {
        self.check_open(rk, checkpoint_index)?;
        self.release_to(rk, checkpoint_index + 1)?;
        self.rollback(rk)
    }

    /// Fails `NoSavepoint` unless the accounts snapshot of `rk` holds the
    /// savepoint at `checkpoint_index`: it is neither released already nor
    /// held by an enclosing snapshot.
    fn check_open<Host: KeySpaceLoader>(
        &self,
        rk: &RuntimeKeyspaces<'_, Host>,
        checkpoint_index: usize,
    ) -> Result<(), SnapshotError> {
        if checkpoint_index < self.enclosing_savepoints(rk)?
            || checkpoint_index >= self.savepoints
        {
            return Err(SnapshotError::NoSavepoint);
        }
        Ok(())
    }

    /// Number of savepoints currently open, to be passed to
    /// [`Self::release_savepoints_from`].
    pub fn savepoint_count(&self) -> usize {
        self.savepoints
    }

    /// Release the savepoints opened since `watermark` (a
    /// [`Self::savepoint_count`]) by work that succeeded, e.g. an alias
    /// materialized from a Michelson operation, whose savepoint no EVM frame
    /// would otherwise release.
    ///
    /// The revert point of an open EVM frame is kept. It is one of these
    /// savepoints only if the work opened the first savepoint of the frame:
    /// on the paths that reach this, a cross-runtime transfer opened one
    /// before, so all of them are released.
    pub fn release_savepoints_from<Host: KeySpaceLoader>(
        &mut self,
        rk: &mut RuntimeKeyspaces<'_, Host>,
        watermark: usize,
    ) -> Result<(), SnapshotError> {
        let height = if self.is_frame_revert_point(watermark) {
            watermark.saturating_add(1)
        } else {
            watermark
        };
        self.release_to(rk, height)
    }

    /// Undo work that failed after opening savepoints since `watermark` (a
    /// [`Self::savepoint_count`]): restore the accounts of `rk` from the
    /// first of them, and release them. Nothing is restored if the work
    /// opened none.
    ///
    /// As in [`Self::release_savepoints_from`], the revert point of an open
    /// EVM frame is restored from but kept.
    pub fn revert_savepoints_from<Host: KeySpaceLoader>(
        &mut self,
        rk: &mut RuntimeKeyspaces<'_, Host>,
        watermark: usize,
    ) -> Result<(), SnapshotError> {
        if self.savepoints <= watermark {
            return Ok(());
        }
        self.release_to(rk, watermark.saturating_add(1))?;
        if self.is_frame_revert_point(watermark) {
            rk.tez_accounts_restore_top()
        } else {
            self.rollback(rk)
        }
    }

    /// Whether the savepoint at `index` is the revert point of the open EVM
    /// frame, which [`Self::revert_frame`] restores: it must outlive the work
    /// that opened it.
    fn is_frame_revert_point(&self, index: usize) -> bool {
        self.external_checkpoints
            .last()
            .is_some_and(|frame| frame.savepoint_watermark == index)
    }

    /// Number of savepoints open on the snapshots enclosing the accounts
    /// snapshot of `rk`, e.g. the transaction's while `rk` is the mirror of a
    /// Tezos operation batch.
    ///
    /// `self.savepoints` counts the savepoints open across every snapshot the
    /// journal drives, oldest snapshot first, while the snapshot of `rk` holds
    /// only its own, the top ones. The difference is what the enclosing
    /// snapshots hold: 0 for the outermost one.
    ///
    /// Fails `UntrackedSavepoints` if the snapshot of `rk` holds more
    /// savepoints than the journal counts: the count is out of sync, and
    /// every index, watermark and release derived from it would be wrong.
    fn enclosing_savepoints<Host: KeySpaceLoader>(
        &self,
        rk: &RuntimeKeyspaces<'_, Host>,
    ) -> Result<usize, SnapshotError> {
        let held = rk.tez_accounts_snapshot().len();
        self.savepoints
            .checked_sub(held)
            .ok_or(SnapshotError::UntrackedSavepoints {
                held,
                tracked: self.savepoints,
            })
    }

    /// Release the savepoints above the `height` oldest, keeping their
    /// writes.
    ///
    /// `height` may be one above the savepoints held: a frame that opened
    /// no savepoint keeps the one above the top for its parent, and there
    /// is nothing to release. Fails `ReleaseAboveHeight` if it is higher:
    /// the caller asks to keep savepoints that no longer exist. Fails
    /// `NoSavepoint` if it is below the savepoints of the enclosing
    /// snapshots: those cannot be released through `rk`.
    fn release_to<Host: KeySpaceLoader>(
        &mut self,
        rk: &mut RuntimeKeyspaces<'_, Host>,
        height: usize,
    ) -> Result<(), SnapshotError> {
        // Nothing above `height` to release, e.g. for a frame that opened no
        // savepoint: the snapshot is left untouched.
        if height >= self.savepoints {
            return if height > self.savepoints + 1 {
                Err(SnapshotError::ReleaseAboveHeight {
                    height,
                    len: self.savepoints,
                })
            } else {
                Ok(())
            };
        }
        let enclosing = self.enclosing_savepoints(rk)?;
        if height < enclosing {
            return Err(SnapshotError::NoSavepoint);
        }
        rk.tez_accounts_release_to(height - enclosing)?;
        self.savepoints = enclosing + rk.tez_accounts_snapshot().len();
        Ok(())
    }

    /// Restore the accounts of `rk` from the top savepoint, and release it.
    fn rollback<Host: KeySpaceLoader>(
        &mut self,
        rk: &mut RuntimeKeyspaces<'_, Host>,
    ) -> Result<(), SnapshotError> {
        let enclosing = self.enclosing_savepoints(rk)?;
        rk.tez_accounts_rollback()?;
        self.savepoints = enclosing + rk.tez_accounts_snapshot().len();
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_evm_runtime::runtime_keyspaces::MockRuntimeKeyspaces;
    use tezos_evm_runtime::safe_storage::SafeStorage;
    use tezos_smart_rollup_host::{path::OwnedPath, storage::StorageV1};
    use tezos_smart_rollup_keyspace::{Key, KeySpace};

    /// The single key the tests write in the accounts.
    const DATA: Key = Key::from_static(b"/data");

    /// Load the keyspaces, whose accounts snapshot the savepoints go on.
    fn start_world(host: &mut MockKernelHost) -> MockRuntimeKeyspaces<'_> {
        RuntimeKeyspaces::init(host).unwrap()
    }

    fn write_data<Host: KeySpaceLoader>(
        rk: &mut RuntimeKeyspaces<'_, Host>,
        data: &[u8],
    ) {
        rk.tez_accounts_mut().set(&DATA, data).unwrap();
    }

    fn read_data<Host: KeySpaceLoader>(rk: &RuntimeKeyspaces<'_, Host>) -> Vec<u8> {
        rk.tez_accounts().get(&DATA).unwrap()
    }

    /// Whether the backup of savepoint `i` is held.
    fn has_snap(rk: &MockRuntimeKeyspaces<'_>, i: usize) -> bool {
        let path = OwnedPath::try_from(format!("/__savepoint/{i}/tez/tez_accounts/data"))
            .unwrap();
        rk.host().store_has(&path).unwrap().is_some()
    }

    // US-1: EVM(A), Mich(B), FAILWITH
    // B reverts its own changes, A catches and sees the pre-B state
    #[test]
    fn test_us1_evm_calls_mich_failwith() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");

        // A enters: watermark=0, no snapshots
        journal.push_external_checkpoint();

        // B starts: snap 0 captures v0, B writes vb
        let idx_b = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vb");
        // B FAILWITH: snap 0 consumed, world restored to v0
        journal.checkpoint_revert(&mut rk, idx_b).unwrap();

        assert_eq!(read_data(&rk), b"v0");
        assert!(!has_snap(&rk, 0));

        // A catches: no snapshots in A's frame, commit is noop
        journal.commit_frame(&mut rk).unwrap();

        assert_eq!(read_data(&rk), b"v0");
    }

    // US-2: EVM(A), Mich(B), Mich(C), Mich(D), FAILWITH
    // D fails, C and B propagate: native Michelson all-or-nothing, A catches
    #[test]
    fn test_us2_evm_calls_deep_mich_chain_all_revert() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");

        // A enters: watermark=0
        journal.push_external_checkpoint();

        // B starts: snap 0 captures v0, B writes vb
        let idx_b = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vb");
        // C starts: snap 1 captures vb, C writes vc
        let idx_c = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vc");
        // D starts: snap 2 captures vc, D writes vd
        let idx_d = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vd");

        // D FAILWITH: snap 2 consumed, world restored to vc
        journal.checkpoint_revert(&mut rk, idx_d).unwrap();
        assert_eq!(read_data(&rk), b"vc");

        // C propagates: snap 1 consumed, world restored to vb
        journal.checkpoint_revert(&mut rk, idx_c).unwrap();
        assert_eq!(read_data(&rk), b"vb");

        // B propagates: snap 0 consumed, world restored to v0
        journal.checkpoint_revert(&mut rk, idx_b).unwrap();
        assert_eq!(read_data(&rk), b"v0");

        // A catches: no snapshots remain, commit is noop
        journal.commit_frame(&mut rk).unwrap();

        assert!(!has_snap(&rk, 0));
        assert!(!has_snap(&rk, 1));
        assert!(!has_snap(&rk, 2));
    }

    // US-3: EVM(A), Mich(B), EVM(C, try-catch), Mich(D), FAILWITH
    // D fails, C catches and commits, B is unaffected, A commits
    #[test]
    fn test_us3_mich_nested_in_inner_evm_failwith_caught() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");

        // A enters: watermark=0
        journal.push_external_checkpoint();

        // B starts: snap 0 captures v0, B writes vb
        let idx_b = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vb");

        // C enters: watermark=1 (snap 0 exists)
        journal.push_external_checkpoint();

        // D starts: snap 1 captures vb, D writes vd
        let idx_d = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vd");
        // D FAILWITH: snap 1 consumed, world restored to vb
        journal.checkpoint_revert(&mut rk, idx_d).unwrap();
        assert_eq!(read_data(&rk), b"vb");

        // C catches: sub-call (A still on stack), drains snaps[2..], nothing to drain
        journal.commit_frame(&mut rk).unwrap();

        // snap 0 from B is intact
        assert!(has_snap(&rk, 0));

        // B commits: snaps above idx_b drained (none), snap 0 remains
        journal.checkpoint_commit(&mut rk, idx_b).unwrap();

        // A commits: outermost, snap 0 deleted
        journal.commit_frame(&mut rk).unwrap();

        assert!(!has_snap(&rk, 0));
        assert_eq!(read_data(&rk), b"vb");
    }

    // US-4: Mich(A), EVM(B, no catch), Mich(C), FAILWITH
    // C reverts, B's EVM revert is noop on the journal, A reverts all
    #[test]
    fn test_us4_mich_outer_evm_no_catch_mich_failwith_all_revert() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");

        // A starts: snap 0 captures v0, A writes va
        let idx_a = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"va");

        // B enters: watermark=1 (snap 0 exists)
        journal.push_external_checkpoint();

        // C starts: snap 1 captures va, C writes vc
        let idx_c = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vc");
        // C FAILWITH: snap 1 consumed, world restored to va
        journal.checkpoint_revert(&mut rk, idx_c).unwrap();
        assert_eq!(read_data(&rk), b"va");

        // B has no catch: revert_frame is noop (no snaps at watermark or above remain)
        journal.revert_frame(&mut rk).unwrap();
        assert_eq!(read_data(&rk), b"va");
        assert!(has_snap(&rk, 0));

        // A propagates failure: snap 0 consumed, world restored to v0
        journal.checkpoint_revert(&mut rk, idx_a).unwrap();

        assert_eq!(read_data(&rk), b"v0");
        assert!(!has_snap(&rk, 0));
    }

    // US-5: EVM(A, try-catch): Mich(B) succeeds, then EVM op fails
    // try block reverts: B's pending snapshot is consumed, catch runs on clean state
    #[test]
    fn test_us5_evm_try_mich_succeeds_then_evm_op_fails() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");

        // A enters try block: watermark=0
        journal.push_external_checkpoint();

        // B starts: snap 0 captures v0, B writes vb, B commits (snap 0 remains)
        let idx_b = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vb");
        journal.checkpoint_commit(&mut rk, idx_b).unwrap();
        assert!(has_snap(&rk, 0));

        // EVM op fails: try block reverts, snap 0 consumed, world restored to v0
        journal.revert_frame(&mut rk).unwrap();

        assert_eq!(read_data(&rk), b"v0");
        assert!(!has_snap(&rk, 0));
    }

    // US-6: EVM(A), Mich(B), EVM(C), Mich(D), EVM(E), Mich(F), FAILWITH
    // only F reverts, E's revert is noop, C is sub-call (snap 1 deferred to A's commit)
    #[test]
    fn test_us6_deep_alternating_stack_innermost_failwith() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");

        // A enters: watermark=0
        journal.push_external_checkpoint();

        // B starts: snap 0 captures v0, B writes vb, B commits
        let idx_b = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vb");
        journal.checkpoint_commit(&mut rk, idx_b).unwrap();

        // C enters: watermark=1
        journal.push_external_checkpoint();

        // D starts: snap 1 captures vb, D writes vd, D commits
        let idx_d = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vd");
        journal.checkpoint_commit(&mut rk, idx_d).unwrap();

        // E enters: watermark=2
        journal.push_external_checkpoint();

        // F starts: snap 2 captures vd, F writes vf
        let idx_f = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vf");
        // F FAILWITH: snap 2 consumed, world restored to vd
        journal.checkpoint_revert(&mut rk, idx_f).unwrap();
        assert_eq!(read_data(&rk), b"vd");

        // E reverts: noop (no snaps at watermark=2 or above remain)
        journal.revert_frame(&mut rk).unwrap();
        assert_eq!(read_data(&rk), b"vd");

        // snaps 0 and 1 are intact: only F's snap was consumed
        assert!(has_snap(&rk, 0));
        assert!(has_snap(&rk, 1));

        // C commits: sub-call (A still on stack), snap 1 deferred to A's commit
        journal.commit_frame(&mut rk).unwrap();
        assert!(has_snap(&rk, 0));

        // A commits: outermost, snaps 0 and 1 deleted
        journal.commit_frame(&mut rk).unwrap();
        assert!(!has_snap(&rk, 0));
        assert!(!has_snap(&rk, 1));

        assert_eq!(read_data(&rk), b"vd");
    }

    // US-7: EVM(A), Mich(B), EVM(C): B and C succeed (pending), then A reverts
    // B's committed snapshot is consumed by A's revert, neither B nor C takes effect
    #[test]
    fn test_us7_mich_evm_succeed_then_outer_evm_reverts() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");

        // A enters: watermark=0
        journal.push_external_checkpoint();

        // B starts: snap 0 captures v0, B writes vb, B commits
        let idx_b = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vb");
        journal.checkpoint_commit(&mut rk, idx_b).unwrap();

        // C enters: watermark=1, no Michelson CRACs
        journal.push_external_checkpoint();
        // C commits: sub-call (A still on stack), drains snaps[2..], nothing to drain
        journal.commit_frame(&mut rk).unwrap();

        // snap 0 from B is still present
        assert!(has_snap(&rk, 0));

        // A reverts: outermost, snap 0 consumed, world restored to v0
        journal.revert_frame(&mut rk).unwrap();

        assert_eq!(read_data(&rk), b"v0");
        assert!(!has_snap(&rk, 0));
    }

    // US-8: Mich(A), EVM(B), Mich(C) succeed, then Mich(A), EVM(D) REVERT
    // all-or-nothing: A reverts everything including B and C's committed changes
    #[test]
    fn test_us8_mich_outer_second_evm_call_reverts_all() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");

        // A starts: snap 0 captures v0, A writes va
        let idx_a = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"va");

        // B enters: watermark=1
        journal.push_external_checkpoint();

        // C starts: snap 1 captures va, C writes vc, C commits
        let idx_c = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vc");
        journal.checkpoint_commit(&mut rk, idx_c).unwrap();

        // B commits: outermost EVM frame, snap 1 deleted
        journal.commit_frame(&mut rk).unwrap();
        assert!(!has_snap(&rk, 1));
        assert!(has_snap(&rk, 0));
        assert_eq!(read_data(&rk), b"vc");

        // A makes a second EVM call: D enters, no Michelson CRACs
        journal.push_external_checkpoint();
        // D reverts: outermost, noop (no snaps at watermark=1 or above)
        journal.revert_frame(&mut rk).unwrap();
        assert_eq!(read_data(&rk), b"vc");
        assert!(has_snap(&rk, 0));

        // A reverts all-or-nothing: snap 0 consumed, world restored to v0
        journal.checkpoint_revert(&mut rk, idx_a).unwrap();

        assert_eq!(read_data(&rk), b"v0");
        assert!(!has_snap(&rk, 0));
    }

    // US-9: EVM(top) -> EVM(1) ~CRAC~> Mich -> REVERT
    // EVM(1) commits with a Michelson CRAC inside; EVM(top) then reverts.
    // The CRAC snapshot must survive EVM(1)'s sub-call commit so EVM(top) can restore.
    #[test]
    fn test_us9_evm_subcall_crac_commits_then_outer_reverts() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");

        // EVM(top) enters
        journal.push_external_checkpoint();

        // EVM(1) enters
        journal.push_external_checkpoint();

        // Michelson CRAC inside EVM(1): snap 0 captures v0, writes vb, commits
        let idx = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vb");
        journal.checkpoint_commit(&mut rk, idx).unwrap();
        assert!(has_snap(&rk, 0));

        // EVM(1) commits: sub-call (EVM(top) is still on the stack)
        // snap 0 must survive for EVM(top)'s potential revert
        journal.commit_frame(&mut rk).unwrap();
        assert!(has_snap(&rk, 0));

        // EVM(top) reverts: outermost, snap 0 consumed, world restored to v0
        journal.revert_frame(&mut rk).unwrap();

        assert_eq!(read_data(&rk), b"v0");
        assert!(!has_snap(&rk, 0));
    }

    // edge case: revert with no snapshots is a noop
    #[test]
    fn test_edge_revert_on_empty_journal() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");

        journal.push_external_checkpoint();
        journal.revert_frame(&mut rk).unwrap();

        assert_eq!(read_data(&rk), b"v0");
    }

    // edge case: external frame with no Michelson CRACs, revert is noop
    #[test]
    fn test_edge_external_frame_no_cracs_revert_noop() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");

        // a CRAC committed outside the frame: snap 0 remains
        let idx = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"v1");
        journal.checkpoint_commit(&mut rk, idx).unwrap();

        // frame with no CRACs: watermark=1, revert is noop
        journal.push_external_checkpoint();
        journal.revert_frame(&mut rk).unwrap();

        assert_eq!(read_data(&rk), b"v1");
        assert!(has_snap(&rk, 0));
    }

    // edge case: external frame with no Michelson CRACs, commit is noop
    #[test]
    fn test_edge_external_frame_no_cracs_commit_noop() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");

        // a CRAC committed outside the frame: snap 0 remains
        let idx = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"v1");
        journal.checkpoint_commit(&mut rk, idx).unwrap();

        // frame with no CRACs: watermark=1, commit is noop
        journal.push_external_checkpoint();
        journal.commit_frame(&mut rk).unwrap();

        assert_eq!(read_data(&rk), b"v1");
        assert!(has_snap(&rk, 0));
    }

    // edge case: commit and revert scoped to watermark do not touch earlier snapshots
    #[test]
    fn test_edge_watermark_isolates_earlier_snapshots() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");

        // two CRACs committed before the frame: snaps 0 and 1 belong to an outer context
        let idx0 = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"v1");
        journal.checkpoint_commit(&mut rk, idx0).unwrap();

        let idx1 = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"v2");
        journal.checkpoint_commit(&mut rk, idx1).unwrap();

        // inner frame adds one more CRAC: snap 2 belongs to this frame
        journal.push_external_checkpoint();
        let idx2 = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"v3");
        journal.checkpoint_commit(&mut rk, idx2).unwrap();

        // commit: outermost, only snap 2 deleted, snaps 0 and 1 untouched
        journal.commit_frame(&mut rk).unwrap();

        assert!(!has_snap(&rk, 2));
        assert!(has_snap(&rk, 0));
        assert!(has_snap(&rk, 1));
        assert_eq!(read_data(&rk), b"v3");

        // inner frame adds another CRAC: snap 2 again (index reused after deletion)
        journal.push_external_checkpoint();
        let idx2b = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"v4");
        journal.checkpoint_commit(&mut rk, idx2b).unwrap();

        // revert: outermost, only snap 2 consumed, snaps 0 and 1 untouched, world restored to v3
        journal.revert_frame(&mut rk).unwrap();

        assert!(!has_snap(&rk, 2));
        assert!(has_snap(&rk, 0));
        assert!(has_snap(&rk, 1));
        assert_eq!(read_data(&rk), b"v3");
    }

    // US-9b: EVM(top) -> EVM(1) ~CRAC~> Mich -> COMMIT (complement of US-9)
    // EVM(1) commits with a Michelson CRAC inside; EVM(top) then commits.
    // Changes must persist; the deferred snapshot is deleted at outermost commit.
    #[test]
    fn test_us9b_evm_subcall_crac_commits_then_outer_commits() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");

        journal.push_external_checkpoint();
        journal.push_external_checkpoint();

        let idx = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vb");
        journal.checkpoint_commit(&mut rk, idx).unwrap();
        assert!(has_snap(&rk, 0));

        // EVM(1) commits: sub-call, snap 0 deferred
        journal.commit_frame(&mut rk).unwrap();
        assert!(has_snap(&rk, 0));

        // EVM(top) commits: outermost, snap 0 deleted, vb persists
        journal.commit_frame(&mut rk).unwrap();
        assert!(!has_snap(&rk, 0));
        assert_eq!(read_data(&rk), b"vb");
    }

    // EVM(top) -> EVM(B1) ~CRAC~> [commits] -> EVM(B2) ~CRAC~> [commits] -> REVERT
    // Two sequential sub-calls each leave a snapshot; outer revert must use the
    // oldest (snap 0, v0) and delete the later one (snap 1).
    #[test]
    fn test_sequential_subcalls_with_cracs_outer_reverts() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");

        journal.push_external_checkpoint();

        // B1: CRAC v0->vb, commits as sub-call
        journal.push_external_checkpoint();
        let idx_b1 = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vb");
        journal.checkpoint_commit(&mut rk, idx_b1).unwrap();
        journal.commit_frame(&mut rk).unwrap();
        assert!(has_snap(&rk, 0));

        // B2: CRAC vb->vc, commits as sub-call
        journal.push_external_checkpoint();
        let idx_b2 = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vc");
        journal.checkpoint_commit(&mut rk, idx_b2).unwrap();
        journal.commit_frame(&mut rk).unwrap();
        assert!(has_snap(&rk, 0));
        assert!(has_snap(&rk, 1));

        // EVM(top) reverts: snap 1 deleted, snap 0 restores v0
        journal.revert_frame(&mut rk).unwrap();
        assert_eq!(read_data(&rk), b"v0");
        assert!(!has_snap(&rk, 0));
        assert!(!has_snap(&rk, 1));
    }

    // Same as above but outer commits: both sub-calls' changes persist.
    #[test]
    fn test_sequential_subcalls_with_cracs_outer_commits() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");

        journal.push_external_checkpoint();

        journal.push_external_checkpoint();
        let idx_b1 = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vb");
        journal.checkpoint_commit(&mut rk, idx_b1).unwrap();
        journal.commit_frame(&mut rk).unwrap();

        journal.push_external_checkpoint();
        let idx_b2 = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vc");
        journal.checkpoint_commit(&mut rk, idx_b2).unwrap();
        journal.commit_frame(&mut rk).unwrap();

        // EVM(top) commits: all snaps deleted, vc persists
        journal.commit_frame(&mut rk).unwrap();
        assert!(!has_snap(&rk, 0));
        assert!(!has_snap(&rk, 1));
        assert_eq!(read_data(&rk), b"vc");
    }

    // EVM(top) -> EVM(B) with two CRACs inside -> B commits -> REVERT
    // B's sub-call commit must prune snap 1 (second CRAC) but keep snap 0
    // (first CRAC, at watermark) for top's revert.
    #[test]
    fn test_subcall_two_cracs_outer_reverts() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");

        journal.push_external_checkpoint();
        journal.push_external_checkpoint();

        let idx1 = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vb");
        journal.checkpoint_commit(&mut rk, idx1).unwrap();

        let idx2 = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vc");
        journal.checkpoint_commit(&mut rk, idx2).unwrap();

        // B commits: sub-call, snap 1 pruned, snap 0 deferred
        journal.commit_frame(&mut rk).unwrap();
        assert!(has_snap(&rk, 0));
        assert!(!has_snap(&rk, 1));

        // EVM(top) reverts: snap 0 restores v0
        journal.revert_frame(&mut rk).unwrap();
        assert_eq!(read_data(&rk), b"v0");
        assert!(!has_snap(&rk, 0));
    }

    // Same as above but outer commits: both CRACs' net effect (vc) persists.
    #[test]
    fn test_subcall_two_cracs_outer_commits() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");

        journal.push_external_checkpoint();
        journal.push_external_checkpoint();

        let idx1 = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vb");
        journal.checkpoint_commit(&mut rk, idx1).unwrap();

        let idx2 = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vc");
        journal.checkpoint_commit(&mut rk, idx2).unwrap();

        journal.commit_frame(&mut rk).unwrap();
        assert!(has_snap(&rk, 0));

        journal.commit_frame(&mut rk).unwrap();
        assert!(!has_snap(&rk, 0));
        assert_eq!(read_data(&rk), b"vc");
    }

    // EVM(A) -> EVM(B) -> EVM(C) ~CRAC~> [commits] -> C commits -> B commits -> A reverts
    // Three-level nesting: snap 0 propagates up through two sub-call commits.
    #[test]
    fn test_three_nested_evm_frames_outer_reverts() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");

        journal.push_external_checkpoint();
        journal.push_external_checkpoint();
        journal.push_external_checkpoint();

        let idx = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vc");
        journal.checkpoint_commit(&mut rk, idx).unwrap();

        // C commits: sub-call (B, A still on stack)
        journal.commit_frame(&mut rk).unwrap();
        assert!(has_snap(&rk, 0));

        // B commits: sub-call (A still on stack)
        journal.commit_frame(&mut rk).unwrap();
        assert!(has_snap(&rk, 0));

        // A reverts: outermost, snap 0 restores v0
        journal.revert_frame(&mut rk).unwrap();
        assert_eq!(read_data(&rk), b"v0");
        assert!(!has_snap(&rk, 0));
    }

    // EVM(top) -> EVM(B) ~CRAC~> [B reverts via revert_frame] -> EVM(top) reverts
    // B's revert consumes the snapshot; top's revert finds nothing and is a noop.
    #[test]
    fn test_subcall_reverts_then_outer_reverts() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");

        journal.push_external_checkpoint();
        journal.push_external_checkpoint();

        let idx = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vb");
        journal.checkpoint_commit(&mut rk, idx).unwrap();

        // B reverts: restores v0, snap 0 consumed
        journal.revert_frame(&mut rk).unwrap();
        assert_eq!(read_data(&rk), b"v0");
        assert!(!has_snap(&rk, 0));

        // EVM(top) reverts: no snaps, noop
        journal.revert_frame(&mut rk).unwrap();
        assert_eq!(read_data(&rk), b"v0");
    }

    // EVM(top) -> EVM(B) ~CRAC~> [B reverts] -> EVM(top) commits
    // B reverted its CRAC; top commits with no net Michelson change.
    #[test]
    fn test_subcall_reverts_then_outer_commits() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");

        journal.push_external_checkpoint();
        journal.push_external_checkpoint();

        let idx = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vb");
        journal.checkpoint_commit(&mut rk, idx).unwrap();

        // B reverts: snap 0 consumed, world back to v0
        journal.revert_frame(&mut rk).unwrap();
        assert_eq!(read_data(&rk), b"v0");

        // EVM(top) commits: no snaps to clean up, v0 persists
        journal.commit_frame(&mut rk).unwrap();
        assert!(!has_snap(&rk, 0));
        assert_eq!(read_data(&rk), b"v0");
    }

    /// Build a distinguishable dummy receipt whose top-level destination
    /// amount encodes `id` so we can tell receipts apart.
    fn dummy_receipt(id: u64) -> AppliedOperation {
        use tezos_crypto_rs::hash::{
            BlockHash, HashTrait, OperationHash, UnknownSignature,
        };
        use tezos_data_encoding::types::Narith;
        use tezos_tezlink::operation::{
            ManagerOperation, ManagerOperationContent, TransferContent,
        };
        use tezos_tezlink::operation_result::{
            ContentResult, OperationBatchWithMetadata, OperationDataAndMetadata,
            OperationResult, OperationResultSum, OperationWithMetadata, TransferSuccess,
            TransferTarget,
        };
        let signature = UnknownSignature::try_from([0u8; 64].as_slice()).unwrap();
        let top = OperationResult {
            balance_updates: vec![],
            result: ContentResult::Applied(TransferTarget::from(
                TransferSuccess::default(),
            )),
            internal_operation_results: vec![],
        };
        let source = tezos_smart_rollup::types::PublicKeyHash::from_b58check(
            "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU",
        )
        .unwrap();
        AppliedOperation {
            hash: OperationHash::default(),
            branch: BlockHash::default(),
            op_and_receipt: OperationDataAndMetadata::OperationWithMetadata(
                OperationBatchWithMetadata {
                    operations: vec![OperationWithMetadata {
                        content: ManagerOperationContent::Transaction(ManagerOperation {
                            source,
                            fee: Narith(0u64.into()),
                            counter: Narith(0u64.into()),
                            gas_limit: Narith(0u64.into()),
                            storage_limit: Narith(0u64.into()),
                            operation: TransferContent {
                                amount: Narith(id.into()),
                                destination: tezos_protocol::contract::Contract::Originated(
                                    tezos_crypto_rs::hash::ContractKt1Hash::from_b58check(
                                        "KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT",
                                    )
                                    .unwrap(),
                                ),
                                parameters: tezos_tezlink::operation::Parameters {
                                    entrypoint: mir::ast::Entrypoint::default(),
                                    value: vec![],
                                },
                            },
                        }),
                        receipt: OperationResultSum::Transfer(top),
                    }],
                    signature,
                },
            ),
        }
    }

    /// Extract the amount from a dummy receipt (used as an identifier).
    fn receipt_id(receipt: &AppliedOperation) -> u64 {
        use tezos_tezlink::operation::ManagerOperationContent;
        use tezos_tezlink::operation_result::OperationDataAndMetadata;
        let OperationDataAndMetadata::OperationWithMetadata(ref batch) =
            receipt.op_and_receipt;
        let ManagerOperationContent::Transaction(ref mgr) = batch.operations[0].content
        else {
            panic!("expected Transaction content");
        };
        mgr.operation.amount.0.clone().try_into().unwrap()
    }

    // Revert migrates CRAC receipts pushed during the reverted frame
    // from `pending_crac_receipts` to `backtracked_crac_receipts`,
    // preserving them as a record of what was attempted.
    //
    //   EVM(A) checkpoint → push receipt 0
    //     EVM(B) checkpoint → push receipt 1
    //     B reverts → receipt 1 migrated (pending=[0], backtracked=[1])
    //   push receipt 2
    //   A commits → receipts = [0, 2]; backtracked = [1]
    #[test]
    fn test_revert_frame_migrates_receipts_to_backtracked() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());
        write_data(&mut rk, b"v0");

        // A checkpoint
        journal.push_external_checkpoint();
        let _idx_a = journal.checkpoint(&mut rk).unwrap();
        journal.push_pending_crac_receipt(dummy_receipt(0));

        // B checkpoint
        journal.push_external_checkpoint();
        journal.push_pending_crac_receipt(dummy_receipt(1));
        assert_eq!(journal.pending_crac_receipts.len(), 2);
        assert_eq!(journal.backtracked_crac_receipts.len(), 0);

        // B reverts — receipt 1 migrated to backtracked, receipt 0 kept.
        journal.revert_frame(&mut rk).unwrap();
        assert_eq!(journal.pending_crac_receipts.len(), 1);
        assert_eq!(receipt_id(&journal.pending_crac_receipts[0].1), 0);
        assert_eq!(journal.backtracked_crac_receipts.len(), 1);
        assert_eq!(receipt_id(&journal.backtracked_crac_receipts[0].1), 1);

        // Push another receipt after revert
        journal.push_pending_crac_receipt(dummy_receipt(2));
        assert_eq!(journal.pending_crac_receipts.len(), 2);
        assert_eq!(receipt_id(&journal.pending_crac_receipts[0].1), 0);
        assert_eq!(receipt_id(&journal.pending_crac_receipts[1].1), 2);

        // A commits — pending receipts preserved; backtracked list is
        // NOT subject to commit/revert so receipt 1 remains.
        journal.checkpoint_commit(&mut rk, 0).unwrap();
        journal.commit_frame(&mut rk).unwrap();
        assert_eq!(journal.pending_crac_receipts.len(), 2);
        assert_eq!(receipt_id(&journal.pending_crac_receipts[0].1), 0);
        assert_eq!(receipt_id(&journal.pending_crac_receipts[1].1), 2);
        assert_eq!(journal.backtracked_crac_receipts.len(), 1);
        assert_eq!(receipt_id(&journal.backtracked_crac_receipts[0].1), 1);
    }

    // A second inner revert after a frame already migrated its
    // receipts must not re-touch the backtracked list (the invariant
    // documented on `backtracked_crac_receipts`: "NOT subject to
    // further revert").
    #[test]
    fn test_revert_frame_does_not_re_revert_backtracked() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());
        write_data(&mut rk, b"v0");

        // A checkpoint
        journal.push_external_checkpoint();
        let _idx_a = journal.checkpoint(&mut rk).unwrap();
        // B checkpoint — push and revert: B's receipt migrates.
        journal.push_external_checkpoint();
        journal.push_pending_crac_receipt(dummy_receipt(7));
        journal.revert_frame(&mut rk).unwrap();
        assert_eq!(journal.backtracked_crac_receipts.len(), 1);

        // A reverts as well: must not affect the already-migrated entry.
        journal.revert_frame(&mut rk).unwrap();
        assert_eq!(journal.backtracked_crac_receipts.len(), 1);
        assert_eq!(receipt_id(&journal.backtracked_crac_receipts[0].1), 7);
    }

    // --- dispatch result slot ---

    // With no open slot, `set_dispatch_result` and
    // `take_dispatch_result` both signal `NoSlot`.
    #[test]
    fn test_dispatch_result_no_slot() {
        let mut journal = MichelsonJournal::new(OperationHash::default());
        assert_eq!(
            journal.set_dispatch_result(vec![1, 2, 3]),
            Err(DispatchSlotError::NoSlot)
        );
        assert_eq!(
            journal.take_dispatch_result(),
            Err(DispatchSlotError::NoSlot)
        );
    }

    // A payload deposited on the top slot is taken out by the next
    // `take_dispatch_result`; a follow-up take with no slot open
    // signals `NoSlot`.
    #[test]
    fn test_dispatch_result_set_then_take() {
        let mut journal = MichelsonJournal::new(OperationHash::default());
        journal.push_dispatch_slot();
        journal.set_dispatch_result(vec![0xCA, 0xFE]).unwrap();
        assert_eq!(journal.take_dispatch_result(), Ok(Some(vec![0xCA, 0xFE])));
        // Slot is gone; a follow-up take is now an unbalanced call.
        assert_eq!(
            journal.take_dispatch_result(),
            Err(DispatchSlotError::NoSlot)
        );
    }

    // An open slot with no deposit yields `Ok(None)` on take —
    // distinct from `Err(NoSlot)` which means no slot at all.
    #[test]
    fn test_dispatch_result_take_empty_slot() {
        let mut journal = MichelsonJournal::new(OperationHash::default());
        journal.push_dispatch_slot();
        assert_eq!(journal.take_dispatch_result(), Ok(None));
    }

    // Second call to `set_dispatch_result` on the same slot fails; the
    // first value survives and is what `take_dispatch_result` yields.
    #[test]
    fn test_dispatch_result_double_set_fails() {
        let mut journal = MichelsonJournal::new(OperationHash::default());
        journal.push_dispatch_slot();
        journal.set_dispatch_result(vec![0x01]).unwrap();
        assert_eq!(
            journal.set_dispatch_result(vec![0x02]),
            Err(DispatchSlotError::AlreadySet)
        );
        assert_eq!(journal.take_dispatch_result(), Ok(Some(vec![0x01])));
    }

    // Nested slots are independent: the inner deposit is taken at the
    // inner level, and the outer slot survives untouched.
    #[test]
    fn test_dispatch_result_nested_slots_independent() {
        let mut journal = MichelsonJournal::new(OperationHash::default());

        // Outer slot deposits its payload.
        journal.push_dispatch_slot();
        journal.set_dispatch_result(vec![0xAA]).unwrap();

        // Inner slot starts empty and holds its own payload.
        journal.push_dispatch_slot();
        journal.set_dispatch_result(vec![0xBB]).unwrap();
        assert_eq!(journal.take_dispatch_result(), Ok(Some(vec![0xBB])));

        // Outer slot is intact and still refuses a second set.
        assert_eq!(
            journal.set_dispatch_result(vec![0xCC]),
            Err(DispatchSlotError::AlreadySet)
        );
        assert_eq!(journal.take_dispatch_result(), Ok(Some(vec![0xAA])));
    }

    // Dispatch slots are disjoint from REVM's external_checkpoints
    // stack: pushing an external checkpoint must not satisfy the
    // dispatch-slot invariant.
    #[test]
    fn test_dispatch_result_disjoint_from_external_checkpoint() {
        let mut journal = MichelsonJournal::new(OperationHash::default());
        journal.push_external_checkpoint();
        assert_eq!(
            journal.set_dispatch_result(vec![0x01]),
            Err(DispatchSlotError::NoSlot)
        );
    }

    // --- dispatch slot owner ---

    fn owner_a() -> Vec<u8> {
        vec![1, 2, 3]
    }

    fn owner_b() -> Vec<u8> {
        vec![4, 5, 6]
    }

    // A freshly-pushed slot has no owner until explicitly assigned.
    #[test]
    fn test_dispatch_slot_owner_unset_by_default() {
        let mut journal = MichelsonJournal::new(OperationHash::default());
        journal.push_dispatch_slot();
        assert_eq!(journal.dispatch_slot_owner(), None);
    }

    // `set_current_dispatch_owner` assigns the topmost slot's owner,
    // observable via `dispatch_slot_owner`.
    #[test]
    fn test_dispatch_slot_owner_set_and_get() {
        let mut journal = MichelsonJournal::new(OperationHash::default());
        journal.push_dispatch_slot();
        journal.set_current_dispatch_owner(owner_a());
        assert_eq!(journal.dispatch_slot_owner(), Some(owner_a().as_slice()));
    }

    // `set_current_dispatch_owner` with no open slot is a no-op — it
    // must not panic and must not create a slot.
    #[test]
    fn test_dispatch_slot_owner_set_without_slot_is_noop() {
        let mut journal = MichelsonJournal::new(OperationHash::default());
        journal.set_current_dispatch_owner(owner_a());
        assert_eq!(journal.dispatch_slot_owner(), None);
        assert_eq!(
            journal.take_dispatch_result(),
            Err(DispatchSlotError::NoSlot)
        );
    }

    // `set_dispatch_result` remains identity-agnostic: it writes
    // regardless of the slot's owner. The sender check is the
    // caller's (the `%collect_result` handler's) responsibility, not
    // the journal's — this is what lets the view path keep depositing
    // via `set_dispatch_result` with no owner assigned at all.
    #[test]
    fn test_dispatch_result_set_is_owner_agnostic() {
        let mut journal = MichelsonJournal::new(OperationHash::default());
        journal.push_dispatch_slot();
        journal.set_current_dispatch_owner(owner_a());
        // No sender check here: the write succeeds regardless of who
        // "would" be calling — the journal only enforces the
        // once-per-dispatch invariant.
        journal.set_dispatch_result(vec![0x01]).unwrap();
        assert_eq!(journal.take_dispatch_result(), Ok(Some(vec![0x01])));
    }

    // Nested slots each carry their own independent owner.
    #[test]
    fn test_dispatch_slot_owner_nested_independent() {
        let mut journal = MichelsonJournal::new(OperationHash::default());
        journal.push_dispatch_slot();
        journal.set_current_dispatch_owner(owner_a());

        journal.push_dispatch_slot();
        journal.set_current_dispatch_owner(owner_b());
        assert_eq!(journal.dispatch_slot_owner(), Some(owner_b().as_slice()));

        journal.take_dispatch_result().unwrap();
        assert_eq!(journal.dispatch_slot_owner(), Some(owner_a().as_slice()));
    }

    // --- origination nonce state ---

    fn dummy_hash(b: u8) -> OperationHash {
        OperationHash::from([b; 32])
    }

    // `operation_hash` is set by the constructor and stable for the
    // life of the journal; subsequent inbound CRACs all see the same
    // seed.
    #[test]
    fn test_operation_hash_set_by_constructor() {
        let journal = MichelsonJournal::new(dummy_hash(0xAA));
        assert_eq!(*journal.operation_hash(), dummy_hash(0xAA));
    }

    // Indices are claimed, use-then-increment: the first
    // `CREATE_CONTRACT` of the operation gets 0, and every subsequent
    // claim — from a later CRAC, a nested frame, or the native path —
    // draws the next one from the same sequence.
    #[test]
    fn test_origination_nonce_index_is_claimed_in_sequence() {
        let mut journal = MichelsonJournal::new(dummy_hash(0x01));
        assert_eq!(journal.origination_index(), 0);
        // CRAC A originates three children.
        assert_eq!(journal.next_origination_index(), 0);
        assert_eq!(journal.next_origination_index(), 1);
        assert_eq!(journal.next_origination_index(), 2);
        assert_eq!(journal.origination_index(), 3);
        // CRAC B continues from there rather than restarting at 0.
        assert_eq!(journal.next_origination_index(), 3);
        assert_eq!(journal.next_origination_index(), 4);
        assert_eq!(journal.origination_index(), 5);
    }

    // Reverting an EVM frame rolls the origination index back to the
    // value at frame entry, so child-KT1 slots burned inside the
    // reverted frame are released for reuse.
    #[test]
    fn test_origination_index_rolls_back_on_revert_frame() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(dummy_hash(0x42));
        // Outer EVM frame enters at index 0.
        journal.push_external_checkpoint();
        // Inbound CRAC inside that frame originated two children.
        journal.next_origination_index();
        journal.next_origination_index();
        assert_eq!(journal.origination_index(), 2);
        // Outer frame reverts: index must drop back to 0.
        journal.revert_frame(&mut rk).unwrap();
        assert_eq!(journal.origination_index(), 0);
    }

    // Committing an EVM frame keeps the increments performed inside
    // it.  The next inbound CRAC continues from the post-frame
    // index, never re-using already-burned KT1 slots.
    #[test]
    fn test_origination_index_persists_on_commit_frame() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(dummy_hash(0x42));
        journal.push_external_checkpoint();
        journal.next_origination_index();
        journal.next_origination_index();
        journal.commit_frame(&mut rk).unwrap();
        assert_eq!(journal.origination_index(), 2);
    }

    // Nested frames: an inner revert only rolls back the inner
    // frame's increments; the outer's prior increments survive.
    #[test]
    fn test_origination_index_nested_revert_only_inner() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(dummy_hash(0x42));
        // Outer frame: outer CRAC originates 2.
        journal.push_external_checkpoint();
        journal.next_origination_index();
        journal.next_origination_index();
        // Inner frame: inner CRAC originates 3 more.
        journal.push_external_checkpoint();
        journal.next_origination_index();
        journal.next_origination_index();
        journal.next_origination_index();
        assert_eq!(journal.origination_index(), 5);
        // Inner reverts: index back to 2 (outer's tail), NOT to 0.
        journal.revert_frame(&mut rk).unwrap();
        assert_eq!(journal.origination_index(), 2);
        // Outer commits: index stays at 2.
        journal.commit_frame(&mut rk).unwrap();
        assert_eq!(journal.origination_index(), 2);
    }

    // The native Michelson path opens no journal frame (it reverts
    // through `SafeStorage`), so it hands indices back explicitly. A
    // backtracked batch must not consume any.
    #[test]
    fn test_restore_origination_index_releases_claimed_indices() {
        let mut journal = MichelsonJournal::new(dummy_hash(0x42));
        journal.next_origination_index();
        let before = journal.origination_index();
        journal.next_origination_index();
        journal.next_origination_index();
        assert_eq!(journal.origination_index(), 3);
        journal.restore_origination_index(before);
        assert_eq!(journal.origination_index(), 1);
        // The released slot is handed out again.
        assert_eq!(journal.next_origination_index(), 1);
    }

    // L2-1676: the MIR internal-operation counter is the replay
    // identity L1 enforces. The value returned by
    // `internal_operation_counter()` is the value last written via
    // `set_internal_operation_counter`; reentrant inbound Michelson
    // frames in the same synthetic op resume from it instead of
    // restarting at 0, so internal-operation identities stay monotonic
    // across the whole NAC transaction.
    #[test]
    fn test_internal_operation_counter_persists_across_calls() {
        let mut journal = MichelsonJournal::new(dummy_hash(0x01));
        assert_eq!(journal.internal_operation_counter(), 0);
        journal.set_internal_operation_counter(2); // frame A emitted 2 internal ops
        assert_eq!(journal.internal_operation_counter(), 2);
        journal.set_internal_operation_counter(3); // frame B emitted 1 more
        assert_eq!(journal.internal_operation_counter(), 3);
    }

    // Reverting an EVM frame keeps the MIR counter: the
    // receipts of the reverted frame stay in the block as backtracked or
    // failed entries, so they must keep counting towards the per-block
    // internal-operation cap.
    #[test]
    fn test_internal_operation_counter_persists_on_revert_frame() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(dummy_hash(0x42));
        // Outer EVM frame enters at counter 0.
        journal.push_external_checkpoint();
        // Inbound CRAC inside that frame emitted two internal ops.
        journal.set_internal_operation_counter(2);
        assert_eq!(journal.internal_operation_counter(), 2);
        // Outer frame reverts: counter stays at 2.
        journal.revert_frame(&mut rk).unwrap();
        assert_eq!(journal.internal_operation_counter(), 2);
    }

    // Committing an EVM frame keeps the increments performed inside it.
    // The next reentrant frame continues from the post-frame counter,
    // never re-using an already-assigned replay identity.
    #[test]
    fn test_internal_operation_counter_persists_on_commit_frame() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(dummy_hash(0x42));
        journal.push_external_checkpoint();
        journal.set_internal_operation_counter(2);
        journal.commit_frame(&mut rk).unwrap();
        assert_eq!(journal.internal_operation_counter(), 2);
    }

    fn dummy_alias_internal() -> InternalOperationSum {
        use tezos_crypto_rs::hash::ContractKt1Hash;
        use tezos_data_encoding::types::Narith;
        use tezos_protocol::contract::Contract;
        use tezos_tezlink::operation::{Parameters, TransferContent};
        use tezos_tezlink::operation_result::{
            ContentResult, InternalContentWithMetadata,
        };
        let kt1 = Contract::Originated(
            ContractKt1Hash::from_base58_check("KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton")
                .unwrap(),
        );
        InternalOperationSum::Transfer(InternalContentWithMetadata {
            sender: kt1.clone(),
            nonce: 0,
            content: TransferContent {
                amount: Narith(0u64.into()),
                destination: kt1,
                parameters: Parameters::default(),
            },
            result: ContentResult::Skipped,
        })
    }

    // Outside an EVM frame, nothing else would delete the snapshots: they are
    // all released, and the world state is left as written.
    #[test]
    fn release_savepoints_from_deletes_them_outside_an_evm_frame() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());
        write_data(&mut rk, b"v0");

        let watermark = journal.savepoint_count();
        journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"v1");
        journal.release_savepoints_from(&mut rk, watermark).unwrap();

        assert_eq!(journal.savepoint_count(), 0);
        assert!(!has_snap(&rk, 0));
        assert_eq!(read_data(&rk), b"v1");
    }

    // Inside an EVM frame whose first snapshot the work took, that snapshot
    // is kept so the frame can still revert through it, and later ones are
    // released.
    #[test]
    fn release_savepoints_from_keeps_the_frame_revert_point() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());
        write_data(&mut rk, b"v0");
        journal.push_external_checkpoint();

        let watermark = journal.savepoint_count();
        journal.checkpoint(&mut rk).unwrap();
        journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"v1");
        journal.release_savepoints_from(&mut rk, watermark).unwrap();

        assert_eq!(journal.savepoint_count(), 1);
        assert!(has_snap(&rk, 0));
        assert!(!has_snap(&rk, 1));
        journal.revert_frame(&mut rk).unwrap();
        assert_eq!(read_data(&rk), b"v0");
    }

    // The usual case inside an EVM frame: a cross-runtime transfer took the
    // frame's revert point first, so all the work's snapshots are released.
    #[test]
    fn release_savepoints_from_releases_all_after_the_frame_revert_point() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());
        write_data(&mut rk, b"v0");
        journal.push_external_checkpoint();
        journal.checkpoint(&mut rk).unwrap();

        let watermark = journal.savepoint_count();
        journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"v1");
        journal.release_savepoints_from(&mut rk, watermark).unwrap();

        assert_eq!(journal.savepoint_count(), 1);
        assert!(!has_snap(&rk, 1));
        journal.revert_frame(&mut rk).unwrap();
        assert_eq!(read_data(&rk), b"v0");
    }

    // A failed work is undone without relying on an enclosing rollback.
    #[test]
    fn revert_savepoints_from_restores_the_state_before_the_work() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());
        write_data(&mut rk, b"v0");

        let watermark = journal.savepoint_count();
        journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"v1");
        journal.revert_savepoints_from(&mut rk, watermark).unwrap();

        assert_eq!(read_data(&rk), b"v0");
        assert_eq!(journal.savepoint_count(), 0);
        assert!(!has_snap(&rk, 0));
    }

    // A work that failed before taking any snapshot must not restore one taken
    // by its caller.
    #[test]
    fn revert_savepoints_from_without_savepoint_leaves_the_caller_one() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());
        write_data(&mut rk, b"v0");
        journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"v1");

        let watermark = journal.savepoint_count();
        journal.revert_savepoints_from(&mut rk, watermark).unwrap();

        assert_eq!(read_data(&rk), b"v1");
        assert_eq!(journal.savepoint_count(), 1);
    }

    // Undoing a work that took the frame's first snapshot restores from it
    // but keeps it, so the frame can still revert through it.
    #[test]
    fn revert_savepoints_from_keeps_the_frame_revert_point() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());
        write_data(&mut rk, b"v0");
        journal.push_external_checkpoint();

        let watermark = journal.savepoint_count();
        journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"v1");
        journal.revert_savepoints_from(&mut rk, watermark).unwrap();

        assert_eq!(read_data(&rk), b"v0");
        assert_eq!(journal.savepoint_count(), 1);
        assert!(has_snap(&rk, 0));
    }

    // Only the originations pushed since the watermark are taken: the earlier
    // ones belong to whoever took that watermark first.
    #[test]
    fn take_pending_alias_origination_internals_from_leaves_earlier_ones() {
        let mut journal = MichelsonJournal::new(OperationHash::default());
        journal.push_pending_alias_origination_internal(dummy_alias_internal());
        let watermark = journal.pending_alias_origination_count();
        journal.push_pending_alias_origination_internal(dummy_alias_internal());

        assert_eq!(
            journal.take_pending_alias_origination_internals_from(watermark),
            vec![dummy_alias_internal()]
        );
        assert_eq!(journal.pending_alias_origination_count(), 1);
        // A watermark past the end takes nothing.
        assert!(journal
            .take_pending_alias_origination_internals_from(5)
            .is_empty());
    }

    #[test]
    fn test_pending_alias_originations_roll_back_on_revert_frame() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(dummy_hash(0x42));
        journal.push_external_checkpoint();
        journal.push_pending_alias_origination_internal(dummy_alias_internal());
        journal.revert_frame(&mut rk).unwrap();
        assert!(journal
            .take_pending_alias_origination_internals()
            .is_empty());
    }

    #[test]
    fn test_pending_alias_originations_persist_on_commit_frame() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(dummy_hash(0x42));
        journal.push_external_checkpoint();
        journal.push_pending_alias_origination_internal(dummy_alias_internal());
        journal.commit_frame(&mut rk).unwrap();
        assert_eq!(journal.take_pending_alias_origination_internals().len(), 1);
    }

    #[test]
    fn test_pending_alias_originations_nested_revert_only_inner() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(dummy_hash(0x42));
        journal.push_external_checkpoint();
        journal.push_pending_alias_origination_internal(dummy_alias_internal());
        journal.push_external_checkpoint();
        journal.push_pending_alias_origination_internal(dummy_alias_internal());
        journal.push_pending_alias_origination_internal(dummy_alias_internal());
        journal.revert_frame(&mut rk).unwrap();
        journal.commit_frame(&mut rk).unwrap();
        assert_eq!(journal.take_pending_alias_origination_internals().len(), 1);
    }

    /// Regression: a checkpoint whose `receipt_count` outlives the list it
    /// indexes must not trap the kernel.
    ///
    /// `push_external_checkpoint` records the pending list's length, and the
    /// list is shared with the Tezos runtime, whose
    /// `drain_reentrant_crac_ops` takes receipts back out of it. Should the
    /// frame stack ever be unbalanced, `receipt_count` outlives what it
    /// indexes, and `Vec::drain(start..)` panics when `start > len` — in WASM
    /// an uncatchable trap, re-fired on every replay of the same immutable
    /// inbox level, i.e. a permanent wedge.
    ///
    /// Twin of the kernel-side
    /// `drain_reentrant_crac_ops_survives_a_watermark_past_the_end`: that one
    /// guards the derived drain, this one the drain at the source.
    #[test]
    fn revert_frame_with_a_receipt_count_past_the_end_does_not_trap() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        journal.push_pending_crac_receipt(dummy_receipt(0));
        journal.push_pending_crac_receipt(dummy_receipt(1));
        // The checkpoint records `receipt_count = 2`.
        journal.push_external_checkpoint();
        // The re-entrant drain empties the list before the frame closes, so
        // the recorded count now points past the end.
        journal.pending_crac_receipts.clear();

        journal.revert_frame(&mut rk).unwrap();

        assert!(
            journal.pending_crac_receipts.is_empty(),
            "there was nothing left to drain"
        );
        assert!(
            journal.backtracked_crac_receipts.is_empty(),
            "a stale receipt_count must migrate nothing, not trap"
        );
    }

    /// `commit_frame`'s outermost-frame branch must not trap on a watermark
    /// past the open savepoints.
    ///
    /// Sequence: a savepoint is opened, a frame opens over it (watermark = 1),
    /// the savepoint is consumed by a revert (count back to 0), then the frame
    /// commits with an empty checkpoint stack — over no savepoints.
    #[test]
    fn commit_frame_with_a_savepoint_watermark_past_the_end_does_not_trap() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");
        let idx = journal.checkpoint(&mut rk).unwrap();
        journal.push_external_checkpoint(); // savepoint_watermark = 1
        journal.checkpoint_revert(&mut rk, idx).unwrap(); // savepoints back to 0

        journal.commit_frame(&mut rk).unwrap();

        assert_eq!(
            journal.open_frame_count(),
            0,
            "the frame was popped; reaching this line at all is the point"
        );
    }

    // A savepoint released already (here by its parent's commit) cannot be
    // reverted to: the revert fails instead of restoring the savepoint left
    // on top, and changes nothing.
    #[test]
    fn test_checkpoint_revert_released_index_fails() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");
        let idx_a = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"va");
        let idx_b = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vb");
        journal.checkpoint_commit(&mut rk, idx_a).unwrap();

        assert_eq!(
            journal.checkpoint_revert(&mut rk, idx_b),
            Err(SnapshotError::NoSavepoint)
        );
        assert_eq!(read_data(&rk), b"vb");
        assert_eq!(journal.savepoint_count(), 1);
        assert!(has_snap(&rk, 0));
    }

    // A savepoint held by an enclosing snapshot cannot be reverted to
    // through an inner one: the revert fails and leaves the inner
    // savepoints in place.
    #[test]
    fn test_checkpoint_revert_enclosing_index_fails() {
        let mut host = MockKernelHost::default();
        let mut outer = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut outer, b"v0");
        let idx_outer = journal.checkpoint(&mut outer).unwrap();

        // The accounts of a mirror, as for a Tezos operation batch.
        let mut mirror = SafeStorage::new(outer.host_mut(), vec![]);
        let mut inner = RuntimeKeyspaces::init(&mut mirror).unwrap();
        journal.checkpoint(&mut inner).unwrap();
        write_data(&mut inner, b"inner");

        assert_eq!(
            journal.checkpoint_revert(&mut inner, idx_outer),
            Err(SnapshotError::NoSavepoint)
        );
        assert_eq!(inner.tez_accounts_snapshot().len(), 1);
        assert_eq!(journal.savepoint_count(), 2);
        assert_eq!(read_data(&inner), b"inner");

        journal.release_all_savepoints(&mut inner).unwrap();
        assert_eq!(journal.savepoint_count(), 1);
    }

    // While the transaction holds a savepoint, a frame reverted on a batch
    // mirror restores the accounts as they were when the frame started.
    #[test]
    fn test_revert_frame_inside_an_enclosing_snapshot() {
        let mut host = MockKernelHost::default();
        let mut outer = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut outer, b"v0");
        journal.checkpoint(&mut outer).unwrap();

        // The accounts of a mirror, as for a Tezos operation batch.
        let mut mirror = SafeStorage::new(outer.host_mut(), vec![]);
        let mut inner = RuntimeKeyspaces::init(&mut mirror).unwrap();
        write_data(&mut inner, b"i0");

        // Watermark 1: savepoint 0 is on the enclosing snapshot.
        journal.push_external_checkpoint();
        journal.checkpoint(&mut inner).unwrap();
        write_data(&mut inner, b"ia");
        journal.checkpoint(&mut inner).unwrap();
        write_data(&mut inner, b"ib");

        journal.revert_frame(&mut inner).unwrap();
        assert_eq!(read_data(&inner), b"i0");
        assert_eq!(journal.savepoint_count(), 1);
        assert!(inner.tez_accounts_snapshot().is_empty());
    }

    // A savepoint released already (here by a commit of an older one)
    // cannot be committed: the commit fails instead of doing nothing, and
    // changes nothing.
    #[test]
    fn test_checkpoint_commit_released_index_fails() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");
        let idx_a = journal.checkpoint(&mut rk).unwrap();
        let idx_b = journal.checkpoint(&mut rk).unwrap();
        let idx_c = journal.checkpoint(&mut rk).unwrap();
        write_data(&mut rk, b"vc");
        journal.checkpoint_commit(&mut rk, idx_a).unwrap();

        assert_eq!(
            journal.checkpoint_commit(&mut rk, idx_b),
            Err(SnapshotError::NoSavepoint)
        );
        assert_eq!(
            journal.checkpoint_commit(&mut rk, idx_c),
            Err(SnapshotError::NoSavepoint)
        );
        assert_eq!(read_data(&rk), b"vc");
        assert_eq!(journal.savepoint_count(), 1);
        assert!(has_snap(&rk, 0));
    }

    // A savepoint held by an enclosing snapshot cannot be committed
    // through an inner one: the commit fails and leaves the inner
    // savepoints in place.
    #[test]
    fn test_checkpoint_commit_enclosing_index_fails() {
        let mut host = MockKernelHost::default();
        let mut outer = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        let idx_outer = journal.checkpoint(&mut outer).unwrap();

        // The accounts of a mirror, as for a Tezos operation batch.
        let mut mirror = SafeStorage::new(outer.host_mut(), vec![]);
        let mut inner = RuntimeKeyspaces::init(&mut mirror).unwrap();
        journal.checkpoint(&mut inner).unwrap();

        assert_eq!(
            journal.checkpoint_commit(&mut inner, idx_outer),
            Err(SnapshotError::NoSavepoint)
        );
        assert_eq!(inner.tez_accounts_snapshot().len(), 1);
        assert_eq!(journal.savepoint_count(), 2);

        journal.release_all_savepoints(&mut inner).unwrap();
        assert_eq!(journal.savepoint_count(), 1);
    }

    // A frame whose watermark is below the savepoints of the enclosing
    // snapshot cannot release them through an inner one: the commit fails
    // instead of releasing only the inner savepoints.
    #[test]
    fn test_commit_frame_below_the_enclosing_snapshot_fails() {
        let mut host = MockKernelHost::default();
        let mut outer = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        // Watermark 0, below the savepoint the enclosing snapshot opens next.
        journal.push_external_checkpoint();
        journal.checkpoint(&mut outer).unwrap();

        // The accounts of a mirror, as for a Tezos operation batch.
        let mut mirror = SafeStorage::new(outer.host_mut(), vec![]);
        let mut inner = RuntimeKeyspaces::init(&mut mirror).unwrap();
        journal.checkpoint(&mut inner).unwrap();

        assert_eq!(
            journal.commit_frame(&mut inner),
            Err(SnapshotError::NoSavepoint)
        );
        assert_eq!(inner.tez_accounts_snapshot().len(), 1);
        assert_eq!(journal.savepoint_count(), 2);

        journal.release_all_savepoints(&mut inner).unwrap();
        assert_eq!(journal.savepoint_count(), 1);
    }

    // With a parent frame open, a child frame whose watermark is past the
    // open savepoints fails to commit instead of clamping the release.
    //
    // Sequence: the parent frame opens (watermark 0), a savepoint is opened,
    // the child frame opens over it (watermark 1), the savepoint is consumed
    // by a revert (count back to 0), then the child frame commits: it would
    // keep savepoint 1 for its parent, past the end.
    #[test]
    fn test_nested_commit_frame_with_a_watermark_past_the_end_fails() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        journal.push_external_checkpoint(); // parent, savepoint_watermark = 0
        let idx = journal.checkpoint(&mut rk).unwrap();
        journal.push_external_checkpoint(); // child, savepoint_watermark = 1
        journal.checkpoint_revert(&mut rk, idx).unwrap(); // savepoints back to 0

        assert_eq!(
            journal.commit_frame(&mut rk),
            Err(SnapshotError::ReleaseAboveHeight { height: 2, len: 0 })
        );
        assert_eq!(journal.open_frame_count(), 1);
        assert_eq!(journal.savepoint_count(), 0);
    }

    // A savepoint opened on the accounts snapshot behind the journal's back
    // puts the count out of sync: the functions that derive the enclosing
    // savepoints from it fail instead of counting from a clamped value, and
    // the final release still deletes the backups.
    #[test]
    fn test_untracked_savepoint_fails() {
        let mut host = MockKernelHost::default();
        let mut rk = start_world(&mut host);
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut rk, b"v0");
        let idx = journal.checkpoint(&mut rk).unwrap();
        rk.tez_accounts_savepoint().unwrap();
        write_data(&mut rk, b"va");
        let untracked = || {
            Err(SnapshotError::UntrackedSavepoints {
                held: 2,
                tracked: 1,
            })
        };

        assert_eq!(journal.checkpoint_revert(&mut rk, idx), untracked());
        assert_eq!(journal.checkpoint_commit(&mut rk, idx), untracked());
        assert_eq!(read_data(&rk), b"va");
        assert_eq!(rk.tez_accounts_snapshot().len(), 2);
        assert_eq!(journal.savepoint_count(), 1);

        assert_eq!(journal.release_all_savepoints(&mut rk), untracked());
        assert_eq!(read_data(&rk), b"va");
        assert!(rk.tez_accounts_snapshot().is_empty());
        assert!(!has_snap(&rk, 0));
        assert!(!has_snap(&rk, 1));
    }
}
