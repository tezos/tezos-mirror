// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_crypto_rs::hash::OperationHash;
use tezos_smart_rollup_host::{
    path::{concat, OwnedPath, Path, RefPath},
    runtime::RuntimeError,
    storage::StorageV1,
};
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
    /// Index into `snapshots` at the time the checkpoint was created.
    snapshot_watermark: usize,
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
    /// `internal_operation_counter` at the time the checkpoint was
    /// created.  Threaded identically to `origination_index`: on
    /// revert it is rolled back so MIR counters consumed inside the
    /// reverted EVM frame are released, and on commit the saved value
    /// is dropped (the counter already reflects the post-frame state).
    /// See the field of the same name on [`MichelsonJournal`] (L2-1676).
    internal_operation_counter: u128,
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

/// A pending revert target: the indexed location where the world-state
/// subtree was copied at checkpoint time, paired with the original path
/// it was copied from. On revert we [`store_move`] the snapshot back to
/// `from_path`, so each snapshot must remember its own origin — different
/// CRACs may snapshot different subtrees (e.g. `/tez/tez_accounts`
/// for the Michelson context), and the EVM frame revert path must not
/// hard-code one of them.
#[derive(Debug, PartialEq, Eq)]
struct Snapshot {
    snapshot_path: OwnedPath,
    from_path: OwnedPath,
}

#[derive(Debug, PartialEq, Eq)]
pub struct MichelsonJournal {
    snapshots: Vec<Snapshot>,
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
    /// Operation hash of this synthetic Michelson manager operation.
    /// Set once at construction and never mutated thereafter; read
    /// through [`Self::operation_hash`].  A child KT1 originated
    /// through an inbound `CREATE_CONTRACT` is
    /// `digest_160(operation_hash[32] || index[4])`, so this value
    /// MUST be deterministic AND unique: two child KT1s may collide
    /// only when their indices collide, never across two distinct
    /// synthetic operations.  See [`crate::TezosXJournal::new`] for
    /// the exact seed computation.
    operation_hash: OperationHash,
    /// Monotonic origination-nonce index, incremented once per
    /// `CREATE_CONTRACT` executed through an inbound CRAC.  Persisted
    /// across multiple inbound CRAC handler invocations within the
    /// same synthetic Michelson manager operation so two sequential
    /// CRACs (e.g. an EVM transaction calling two distinct Michelson
    /// callees in turn) get consecutive indices — and therefore
    /// distinct child KT1s — rather than restarting from 0 each time.
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
    /// rolled back on `revert_frame`, kept on `commit_frame`.
    internal_operation_counter: u128,
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
    /// Construct a fresh Michelson journal for one synthetic Michelson
    /// manager operation.  [`operation_hash`] seeds the inbound-CRAC
    /// origination nonce; it MUST be derived deterministically from
    /// the originating transaction's context (typically
    /// `blake2b(crac:<block_number>:<crac_id>)`) so two distinct
    /// synthetic ops in the same block see different seeds.
    pub fn new(operation_hash: OperationHash) -> Self {
        Self {
            snapshots: Vec::new(),
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

    /// Operation hash of this synthetic Michelson manager operation.
    /// Set once at construction and stable for the journal's life.
    pub fn operation_hash(&self) -> &OperationHash {
        &self.operation_hash
    }

    /// Current inbound-CRAC origination-nonce index — the count of
    /// `CREATE_CONTRACT`s already performed in this synthetic
    /// Michelson manager operation.  Combined with
    /// [`Self::operation_hash`] to derive the next child KT1 as
    /// `digest_160(operation_hash || index+1)`.
    ///
    /// Persisted across multiple inbound CRAC handler invocations
    /// within the same synthetic op so two sequential CRACs see
    /// consecutive indices — and therefore distinct child KT1s —
    /// rather than restarting from 0 each time.  Pair every read
    /// with [`set_origination_index`](Self::set_origination_index)
    /// to write the post-execution value back.
    pub fn origination_index(&self) -> u32 {
        self.origination_index
    }

    /// Persist `index` as the new origination-nonce index, typically
    /// called by the inbound-CRAC handler at the end of an
    /// `execute_entrypoint_call` to record the count of
    /// `CREATE_CONTRACT`s performed in that call.  The next inbound
    /// CRAC will resume from this value.
    pub fn set_origination_index(&mut self, index: u32) {
        self.origination_index = index;
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
}

pub fn indexed_path<T: Path>(depth: usize, path: &T) -> Result<OwnedPath, RuntimeError> {
    let path_bytes = format!("/{depth}");
    let indexed_path = RefPath::assert_from(path_bytes.as_bytes());
    concat(&indexed_path, path).map_err(|_| RuntimeError::PathNotFound)
}

impl MichelsonJournal {
    // Called by an external journal on checkpoint creation.
    // Records the current snapshot count, receipt count and
    // origination-nonce index as the lower boundary for this call
    // frame.
    pub fn push_external_checkpoint(&mut self) {
        self.external_checkpoints.push(ExternalCheckpoint {
            snapshot_watermark: self.snapshots.len(),
            receipt_count: self.pending_crac_receipts.len(),
            origination_index: self.origination_index,
            internal_operation_counter: self.internal_operation_counter,
            alias_internals_count: self.pending_alias_origination_internals.len(),
        });
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

    // Called by EVM journal on checkpoint commit.
    //
    // Pops the current frame's watermark. If a parent EVM frame exists,
    // leaves the snapshot at watermark for the parent's potential revert.
    // If this is the outermost frame, deletes it too.
    // Receipts are kept (commit preserves them).
    pub fn commit_frame<Host>(&mut self, host: &mut Host) -> Result<(), RuntimeError>
    where
        Host: StorageV1,
    {
        let checkpoint = self
            .external_checkpoints
            .pop()
            .unwrap_or(ExternalCheckpoint {
                snapshot_watermark: 0,
                receipt_count: 0,
                origination_index: 0,
                internal_operation_counter: 0,
                alias_internals_count: 0,
            });
        // The saved origination index is dropped on commit: any
        // increments performed during this frame are kept, so a later
        // CRAC in the same synthetic op continues from the post-frame
        // value rather than re-using already-burned indices.
        let _ = checkpoint.origination_index;
        // Likewise the saved MIR internal-operation counter is dropped
        // on commit: counters consumed in this frame stay consumed, so
        // a later reentrant frame keeps a monotonic, collision-free
        // replay identity (L2-1676).
        let _ = checkpoint.internal_operation_counter;
        let _ = checkpoint.alias_internals_count;
        let drain_from = if self.external_checkpoints.is_empty() {
            checkpoint.snapshot_watermark
        } else {
            (checkpoint.snapshot_watermark + 1).min(self.snapshots.len())
        };
        for snapshot in self.snapshots.drain(drain_from..) {
            host.store_delete(&snapshot.snapshot_path)?;
        }
        Ok(())
    }

    // Called by EVM journal on checkpoint revert.
    //
    // Pops the current frame's watermark, reverts durable storage to
    // the state captured at the start of this call frame, and migrates
    // applied CRAC receipts pushed during this frame to the
    // backtracked list with every result transformed to `BackTracked`.
    // The state effects of those CRACs are rolled back via the
    // snapshot mechanism — the receipt is kept purely as an indexer
    // record of what was attempted (matching L1 manager-op semantics
    // where applied internals reverted by a later failure appear as
    // `backtracked` rather than vanishing). The revert target is
    // recovered from the snapshot itself — see [`Snapshot`] for why
    // this must not be a caller-supplied parameter.
    pub fn revert_frame<Host>(&mut self, host: &mut Host) -> Result<(), RuntimeError>
    where
        Host: StorageV1,
    {
        let checkpoint = self
            .external_checkpoints
            .pop()
            .unwrap_or(ExternalCheckpoint {
                snapshot_watermark: 0,
                receipt_count: 0,
                origination_index: 0,
                internal_operation_counter: 0,
                alias_internals_count: 0,
            });
        // Roll the origination-nonce index back to its value at
        // checkpoint entry: any `CREATE_CONTRACT` performed inside the
        // reverted EVM frame is undone (the child KT1 was never
        // durably written, since Michelson is all-or-nothing inside a
        // CRAC and the snapshot mechanism above also reverts the
        // durable state), and the freed indices are reusable by the
        // next CRAC.  Done unconditionally so a partial inbound CRAC
        // followed by a caller-side EVM revert does not leak burned
        // indices.
        self.origination_index = checkpoint.origination_index;
        // Roll the MIR internal-operation counter back the same way:
        // the internal operations applied inside the reverted frame are
        // backtracked, so their replay identities are released and the
        // next reentrant frame resumes from the pre-frame counter
        // (L2-1676).
        self.internal_operation_counter = checkpoint.internal_operation_counter;
        self.pending_alias_origination_internals
            .truncate(checkpoint.alias_internals_count);
        // Drain (not truncate) the CRAC receipts pushed during this
        // frame, transform their statuses to `BackTracked`, and
        // migrate them to the backtracked list.  Like
        // `failed_crac_receipts`, the backtracked list is NOT subject
        // to further revert: once migrated, a subsequent inner revert
        // does not re-delete the entry.
        let drained: Vec<(u64, AppliedOperation)> = self
            .pending_crac_receipts
            .drain(checkpoint.receipt_count..)
            .collect();
        for (seq, mut receipt) in drained {
            backtrack_receipt(&mut receipt);
            self.backtracked_crac_receipts.push((seq, receipt));
        }
        if checkpoint.snapshot_watermark >= self.snapshots.len() {
            return Ok(());
        }
        for snapshot in self.snapshots.drain(checkpoint.snapshot_watermark + 1..) {
            host.store_delete(&snapshot.snapshot_path)?;
        }
        if let Some(snapshot) = self.snapshots.pop() {
            host.store_move(&snapshot.snapshot_path, &snapshot.from_path)?;
        }
        Ok(())
    }

    // Called by Michelson CRAC logic.
    //
    // Creates a temporary copy of the provided path before execution
    // and returns an index used to commit or revert.
    pub fn checkpoint<Host>(
        &mut self,
        host: &mut Host,
        from_path: &OwnedPath,
    ) -> Result<usize, RuntimeError>
    where
        Host: StorageV1,
    {
        let new_index = self.snapshots.len();
        let snapshot_path = indexed_path(new_index, from_path)?;
        host.store_copy(from_path, &snapshot_path)?;
        self.snapshots.push(Snapshot {
            snapshot_path,
            from_path: from_path.clone(),
        });
        Ok(new_index)
    }

    // Called by Michelson CRAC logic.
    //
    // Removes snapshots taken after checkpoint_index, keeping the snapshot at
    // checkpoint_index so a parent caller can still revert through it.
    pub fn checkpoint_commit<Host>(
        &mut self,
        host: &mut Host,
        checkpoint_index: usize,
    ) -> Result<(), RuntimeError>
    where
        Host: StorageV1,
    {
        let start = (checkpoint_index + 1).min(self.snapshots.len());
        for snapshot in self.snapshots.drain(start..) {
            host.store_delete(&snapshot.snapshot_path)?;
        }
        Ok(())
    }

    // Called by Michelson CRAC logic.
    //
    // Removes snapshots taken after checkpoint_index, then restores durable
    // storage from the snapshot at checkpoint_index. The revert target is
    // recovered from the snapshot itself.
    pub fn checkpoint_revert<Host>(
        &mut self,
        host: &mut Host,
        checkpoint_index: usize,
    ) -> Result<(), RuntimeError>
    where
        Host: StorageV1,
    {
        let start = (checkpoint_index + 1).min(self.snapshots.len());
        for snapshot in self.snapshots.drain(start..) {
            host.store_delete(&snapshot.snapshot_path)?;
        }
        if let Some(snapshot) = self.snapshots.pop() {
            host.store_move(&snapshot.snapshot_path, &snapshot.from_path)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_smart_rollup_mock::MockHost;

    fn world_path() -> OwnedPath {
        OwnedPath::try_from("/world".to_string()).unwrap()
    }

    fn write_data(host: &mut MockHost, path: &OwnedPath, data: &[u8]) {
        host.store_write(path, data, 0).unwrap();
    }

    fn read_data(host: &MockHost, path: &OwnedPath) -> Vec<u8> {
        host.store_read(path, 0, 1024).unwrap()
    }

    fn has_snap(host: &MockHost, i: usize, world: &OwnedPath) -> bool {
        let path = indexed_path(i, world).unwrap();
        host.store_has(&path).map(|v| v.is_some()).unwrap_or(false)
    }

    // US-1: EVM(A), Mich(B), FAILWITH
    // B reverts its own changes, A catches and sees the pre-B state
    #[test]
    fn test_us1_evm_calls_mich_failwith() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut host, &world, b"v0");

        // A enters: watermark=0, no snapshots
        journal.push_external_checkpoint();

        // B starts: snap 0 captures v0, B writes vb
        let idx_b = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vb");
        // B FAILWITH: snap 0 consumed, world restored to v0
        journal.checkpoint_revert(&mut host, idx_b).unwrap();

        assert_eq!(read_data(&host, &world), b"v0");
        assert!(!has_snap(&host, 0, &world));

        // A catches: no snapshots in A's frame, commit is noop
        journal.commit_frame(&mut host).unwrap();

        assert_eq!(read_data(&host, &world), b"v0");
    }

    // US-2: EVM(A), Mich(B), Mich(C), Mich(D), FAILWITH
    // D fails, C and B propagate: native Michelson all-or-nothing, A catches
    #[test]
    fn test_us2_evm_calls_deep_mich_chain_all_revert() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut host, &world, b"v0");

        // A enters: watermark=0
        journal.push_external_checkpoint();

        // B starts: snap 0 captures v0, B writes vb
        let idx_b = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vb");
        // C starts: snap 1 captures vb, C writes vc
        let idx_c = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vc");
        // D starts: snap 2 captures vc, D writes vd
        let idx_d = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vd");

        // D FAILWITH: snap 2 consumed, world restored to vc
        journal.checkpoint_revert(&mut host, idx_d).unwrap();
        assert_eq!(read_data(&host, &world), b"vc");

        // C propagates: snap 1 consumed, world restored to vb
        journal.checkpoint_revert(&mut host, idx_c).unwrap();
        assert_eq!(read_data(&host, &world), b"vb");

        // B propagates: snap 0 consumed, world restored to v0
        journal.checkpoint_revert(&mut host, idx_b).unwrap();
        assert_eq!(read_data(&host, &world), b"v0");

        // A catches: no snapshots remain, commit is noop
        journal.commit_frame(&mut host).unwrap();

        assert!(!has_snap(&host, 0, &world));
        assert!(!has_snap(&host, 1, &world));
        assert!(!has_snap(&host, 2, &world));
    }

    // US-3: EVM(A), Mich(B), EVM(C, try-catch), Mich(D), FAILWITH
    // D fails, C catches and commits, B is unaffected, A commits
    #[test]
    fn test_us3_mich_nested_in_inner_evm_failwith_caught() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut host, &world, b"v0");

        // A enters: watermark=0
        journal.push_external_checkpoint();

        // B starts: snap 0 captures v0, B writes vb
        let idx_b = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vb");

        // C enters: watermark=1 (snap 0 exists)
        journal.push_external_checkpoint();

        // D starts: snap 1 captures vb, D writes vd
        let idx_d = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vd");
        // D FAILWITH: snap 1 consumed, world restored to vb
        journal.checkpoint_revert(&mut host, idx_d).unwrap();
        assert_eq!(read_data(&host, &world), b"vb");

        // C catches: sub-call (A still on stack), drains snaps[2..], nothing to drain
        journal.commit_frame(&mut host).unwrap();

        // snap 0 from B is intact
        assert!(has_snap(&host, 0, &world));

        // B commits: snaps above idx_b drained (none), snap 0 remains
        journal.checkpoint_commit(&mut host, idx_b).unwrap();

        // A commits: outermost, snap 0 deleted
        journal.commit_frame(&mut host).unwrap();

        assert!(!has_snap(&host, 0, &world));
        assert_eq!(read_data(&host, &world), b"vb");
    }

    // US-4: Mich(A), EVM(B, no catch), Mich(C), FAILWITH
    // C reverts, B's EVM revert is noop on the journal, A reverts all
    #[test]
    fn test_us4_mich_outer_evm_no_catch_mich_failwith_all_revert() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut host, &world, b"v0");

        // A starts: snap 0 captures v0, A writes va
        let idx_a = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"va");

        // B enters: watermark=1 (snap 0 exists)
        journal.push_external_checkpoint();

        // C starts: snap 1 captures va, C writes vc
        let idx_c = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vc");
        // C FAILWITH: snap 1 consumed, world restored to va
        journal.checkpoint_revert(&mut host, idx_c).unwrap();
        assert_eq!(read_data(&host, &world), b"va");

        // B has no catch: revert_frame is noop (no snaps at watermark or above remain)
        journal.revert_frame(&mut host).unwrap();
        assert_eq!(read_data(&host, &world), b"va");
        assert!(has_snap(&host, 0, &world));

        // A propagates failure: snap 0 consumed, world restored to v0
        journal.checkpoint_revert(&mut host, idx_a).unwrap();

        assert_eq!(read_data(&host, &world), b"v0");
        assert!(!has_snap(&host, 0, &world));
    }

    // US-5: EVM(A, try-catch): Mich(B) succeeds, then EVM op fails
    // try block reverts: B's pending snapshot is consumed, catch runs on clean state
    #[test]
    fn test_us5_evm_try_mich_succeeds_then_evm_op_fails() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut host, &world, b"v0");

        // A enters try block: watermark=0
        journal.push_external_checkpoint();

        // B starts: snap 0 captures v0, B writes vb, B commits (snap 0 remains)
        let idx_b = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vb");
        journal.checkpoint_commit(&mut host, idx_b).unwrap();
        assert!(has_snap(&host, 0, &world));

        // EVM op fails: try block reverts, snap 0 consumed, world restored to v0
        journal.revert_frame(&mut host).unwrap();

        assert_eq!(read_data(&host, &world), b"v0");
        assert!(!has_snap(&host, 0, &world));
    }

    // US-6: EVM(A), Mich(B), EVM(C), Mich(D), EVM(E), Mich(F), FAILWITH
    // only F reverts, E's revert is noop, C is sub-call (snap 1 deferred to A's commit)
    #[test]
    fn test_us6_deep_alternating_stack_innermost_failwith() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut host, &world, b"v0");

        // A enters: watermark=0
        journal.push_external_checkpoint();

        // B starts: snap 0 captures v0, B writes vb, B commits
        let idx_b = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vb");
        journal.checkpoint_commit(&mut host, idx_b).unwrap();

        // C enters: watermark=1
        journal.push_external_checkpoint();

        // D starts: snap 1 captures vb, D writes vd, D commits
        let idx_d = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vd");
        journal.checkpoint_commit(&mut host, idx_d).unwrap();

        // E enters: watermark=2
        journal.push_external_checkpoint();

        // F starts: snap 2 captures vd, F writes vf
        let idx_f = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vf");
        // F FAILWITH: snap 2 consumed, world restored to vd
        journal.checkpoint_revert(&mut host, idx_f).unwrap();
        assert_eq!(read_data(&host, &world), b"vd");

        // E reverts: noop (no snaps at watermark=2 or above remain)
        journal.revert_frame(&mut host).unwrap();
        assert_eq!(read_data(&host, &world), b"vd");

        // snaps 0 and 1 are intact: only F's snap was consumed
        assert!(has_snap(&host, 0, &world));
        assert!(has_snap(&host, 1, &world));

        // C commits: sub-call (A still on stack), snap 1 deferred to A's commit
        journal.commit_frame(&mut host).unwrap();
        assert!(has_snap(&host, 0, &world));

        // A commits: outermost, snaps 0 and 1 deleted
        journal.commit_frame(&mut host).unwrap();
        assert!(!has_snap(&host, 0, &world));
        assert!(!has_snap(&host, 1, &world));

        assert_eq!(read_data(&host, &world), b"vd");
    }

    // US-7: EVM(A), Mich(B), EVM(C): B and C succeed (pending), then A reverts
    // B's committed snapshot is consumed by A's revert, neither B nor C takes effect
    #[test]
    fn test_us7_mich_evm_succeed_then_outer_evm_reverts() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut host, &world, b"v0");

        // A enters: watermark=0
        journal.push_external_checkpoint();

        // B starts: snap 0 captures v0, B writes vb, B commits
        let idx_b = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vb");
        journal.checkpoint_commit(&mut host, idx_b).unwrap();

        // C enters: watermark=1, no Michelson CRACs
        journal.push_external_checkpoint();
        // C commits: sub-call (A still on stack), drains snaps[2..], nothing to drain
        journal.commit_frame(&mut host).unwrap();

        // snap 0 from B is still present
        assert!(has_snap(&host, 0, &world));

        // A reverts: outermost, snap 0 consumed, world restored to v0
        journal.revert_frame(&mut host).unwrap();

        assert_eq!(read_data(&host, &world), b"v0");
        assert!(!has_snap(&host, 0, &world));
    }

    // US-8: Mich(A), EVM(B), Mich(C) succeed, then Mich(A), EVM(D) REVERT
    // all-or-nothing: A reverts everything including B and C's committed changes
    #[test]
    fn test_us8_mich_outer_second_evm_call_reverts_all() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut host, &world, b"v0");

        // A starts: snap 0 captures v0, A writes va
        let idx_a = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"va");

        // B enters: watermark=1
        journal.push_external_checkpoint();

        // C starts: snap 1 captures va, C writes vc, C commits
        let idx_c = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vc");
        journal.checkpoint_commit(&mut host, idx_c).unwrap();

        // B commits: outermost EVM frame, snap 1 deleted
        journal.commit_frame(&mut host).unwrap();
        assert!(!has_snap(&host, 1, &world));
        assert!(has_snap(&host, 0, &world));
        assert_eq!(read_data(&host, &world), b"vc");

        // A makes a second EVM call: D enters, no Michelson CRACs
        journal.push_external_checkpoint();
        // D reverts: outermost, noop (no snaps at watermark=1 or above)
        journal.revert_frame(&mut host).unwrap();
        assert_eq!(read_data(&host, &world), b"vc");
        assert!(has_snap(&host, 0, &world));

        // A reverts all-or-nothing: snap 0 consumed, world restored to v0
        journal.checkpoint_revert(&mut host, idx_a).unwrap();

        assert_eq!(read_data(&host, &world), b"v0");
        assert!(!has_snap(&host, 0, &world));
    }

    // US-9: EVM(top) -> EVM(1) ~CRAC~> Mich -> REVERT
    // EVM(1) commits with a Michelson CRAC inside; EVM(top) then reverts.
    // The CRAC snapshot must survive EVM(1)'s sub-call commit so EVM(top) can restore.
    #[test]
    fn test_us9_evm_subcall_crac_commits_then_outer_reverts() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut host, &world, b"v0");

        // EVM(top) enters
        journal.push_external_checkpoint();

        // EVM(1) enters
        journal.push_external_checkpoint();

        // Michelson CRAC inside EVM(1): snap 0 captures v0, writes vb, commits
        let idx = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vb");
        journal.checkpoint_commit(&mut host, idx).unwrap();
        assert!(has_snap(&host, 0, &world));

        // EVM(1) commits: sub-call (EVM(top) is still on the stack)
        // snap 0 must survive for EVM(top)'s potential revert
        journal.commit_frame(&mut host).unwrap();
        assert!(has_snap(&host, 0, &world));

        // EVM(top) reverts: outermost, snap 0 consumed, world restored to v0
        journal.revert_frame(&mut host).unwrap();

        assert_eq!(read_data(&host, &world), b"v0");
        assert!(!has_snap(&host, 0, &world));
    }

    // edge case: revert with no snapshots is a noop
    #[test]
    fn test_edge_revert_on_empty_journal() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut host, &world, b"v0");

        journal.push_external_checkpoint();
        journal.revert_frame(&mut host).unwrap();

        assert_eq!(read_data(&host, &world), b"v0");
    }

    // edge case: external frame with no Michelson CRACs, revert is noop
    #[test]
    fn test_edge_external_frame_no_cracs_revert_noop() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut host, &world, b"v0");

        // a CRAC committed outside the frame: snap 0 remains
        let idx = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"v1");
        journal.checkpoint_commit(&mut host, idx).unwrap();

        // frame with no CRACs: watermark=1, revert is noop
        journal.push_external_checkpoint();
        journal.revert_frame(&mut host).unwrap();

        assert_eq!(read_data(&host, &world), b"v1");
        assert!(has_snap(&host, 0, &world));
    }

    // edge case: external frame with no Michelson CRACs, commit is noop
    #[test]
    fn test_edge_external_frame_no_cracs_commit_noop() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut host, &world, b"v0");

        // a CRAC committed outside the frame: snap 0 remains
        let idx = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"v1");
        journal.checkpoint_commit(&mut host, idx).unwrap();

        // frame with no CRACs: watermark=1, commit is noop
        journal.push_external_checkpoint();
        journal.commit_frame(&mut host).unwrap();

        assert_eq!(read_data(&host, &world), b"v1");
        assert!(has_snap(&host, 0, &world));
    }

    // edge case: commit and revert scoped to watermark do not touch earlier snapshots
    #[test]
    fn test_edge_watermark_isolates_earlier_snapshots() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut host, &world, b"v0");

        // two CRACs committed before the frame: snaps 0 and 1 belong to an outer context
        let idx0 = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"v1");
        journal.checkpoint_commit(&mut host, idx0).unwrap();

        let idx1 = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"v2");
        journal.checkpoint_commit(&mut host, idx1).unwrap();

        // inner frame adds one more CRAC: snap 2 belongs to this frame
        journal.push_external_checkpoint();
        let idx2 = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"v3");
        journal.checkpoint_commit(&mut host, idx2).unwrap();

        // commit: outermost, only snap 2 deleted, snaps 0 and 1 untouched
        journal.commit_frame(&mut host).unwrap();

        assert!(!has_snap(&host, 2, &world));
        assert!(has_snap(&host, 0, &world));
        assert!(has_snap(&host, 1, &world));
        assert_eq!(read_data(&host, &world), b"v3");

        // inner frame adds another CRAC: snap 2 again (index reused after deletion)
        journal.push_external_checkpoint();
        let idx2b = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"v4");
        journal.checkpoint_commit(&mut host, idx2b).unwrap();

        // revert: outermost, only snap 2 consumed, snaps 0 and 1 untouched, world restored to v3
        journal.revert_frame(&mut host).unwrap();

        assert!(!has_snap(&host, 2, &world));
        assert!(has_snap(&host, 0, &world));
        assert!(has_snap(&host, 1, &world));
        assert_eq!(read_data(&host, &world), b"v3");
    }

    // US-9b: EVM(top) -> EVM(1) ~CRAC~> Mich -> COMMIT (complement of US-9)
    // EVM(1) commits with a Michelson CRAC inside; EVM(top) then commits.
    // Changes must persist; the deferred snapshot is deleted at outermost commit.
    #[test]
    fn test_us9b_evm_subcall_crac_commits_then_outer_commits() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut host, &world, b"v0");

        journal.push_external_checkpoint();
        journal.push_external_checkpoint();

        let idx = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vb");
        journal.checkpoint_commit(&mut host, idx).unwrap();
        assert!(has_snap(&host, 0, &world));

        // EVM(1) commits: sub-call, snap 0 deferred
        journal.commit_frame(&mut host).unwrap();
        assert!(has_snap(&host, 0, &world));

        // EVM(top) commits: outermost, snap 0 deleted, vb persists
        journal.commit_frame(&mut host).unwrap();
        assert!(!has_snap(&host, 0, &world));
        assert_eq!(read_data(&host, &world), b"vb");
    }

    // EVM(top) -> EVM(B1) ~CRAC~> [commits] -> EVM(B2) ~CRAC~> [commits] -> REVERT
    // Two sequential sub-calls each leave a snapshot; outer revert must use the
    // oldest (snap 0, v0) and delete the later one (snap 1).
    #[test]
    fn test_sequential_subcalls_with_cracs_outer_reverts() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut host, &world, b"v0");

        journal.push_external_checkpoint();

        // B1: CRAC v0->vb, commits as sub-call
        journal.push_external_checkpoint();
        let idx_b1 = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vb");
        journal.checkpoint_commit(&mut host, idx_b1).unwrap();
        journal.commit_frame(&mut host).unwrap();
        assert!(has_snap(&host, 0, &world));

        // B2: CRAC vb->vc, commits as sub-call
        journal.push_external_checkpoint();
        let idx_b2 = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vc");
        journal.checkpoint_commit(&mut host, idx_b2).unwrap();
        journal.commit_frame(&mut host).unwrap();
        assert!(has_snap(&host, 0, &world));
        assert!(has_snap(&host, 1, &world));

        // EVM(top) reverts: snap 1 deleted, snap 0 restores v0
        journal.revert_frame(&mut host).unwrap();
        assert_eq!(read_data(&host, &world), b"v0");
        assert!(!has_snap(&host, 0, &world));
        assert!(!has_snap(&host, 1, &world));
    }

    // Same as above but outer commits: both sub-calls' changes persist.
    #[test]
    fn test_sequential_subcalls_with_cracs_outer_commits() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut host, &world, b"v0");

        journal.push_external_checkpoint();

        journal.push_external_checkpoint();
        let idx_b1 = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vb");
        journal.checkpoint_commit(&mut host, idx_b1).unwrap();
        journal.commit_frame(&mut host).unwrap();

        journal.push_external_checkpoint();
        let idx_b2 = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vc");
        journal.checkpoint_commit(&mut host, idx_b2).unwrap();
        journal.commit_frame(&mut host).unwrap();

        // EVM(top) commits: all snaps deleted, vc persists
        journal.commit_frame(&mut host).unwrap();
        assert!(!has_snap(&host, 0, &world));
        assert!(!has_snap(&host, 1, &world));
        assert_eq!(read_data(&host, &world), b"vc");
    }

    // EVM(top) -> EVM(B) with two CRACs inside -> B commits -> REVERT
    // B's sub-call commit must prune snap 1 (second CRAC) but keep snap 0
    // (first CRAC, at watermark) for top's revert.
    #[test]
    fn test_subcall_two_cracs_outer_reverts() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut host, &world, b"v0");

        journal.push_external_checkpoint();
        journal.push_external_checkpoint();

        let idx1 = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vb");
        journal.checkpoint_commit(&mut host, idx1).unwrap();

        let idx2 = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vc");
        journal.checkpoint_commit(&mut host, idx2).unwrap();

        // B commits: sub-call, snap 1 pruned, snap 0 deferred
        journal.commit_frame(&mut host).unwrap();
        assert!(has_snap(&host, 0, &world));
        assert!(!has_snap(&host, 1, &world));

        // EVM(top) reverts: snap 0 restores v0
        journal.revert_frame(&mut host).unwrap();
        assert_eq!(read_data(&host, &world), b"v0");
        assert!(!has_snap(&host, 0, &world));
    }

    // Same as above but outer commits: both CRACs' net effect (vc) persists.
    #[test]
    fn test_subcall_two_cracs_outer_commits() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut host, &world, b"v0");

        journal.push_external_checkpoint();
        journal.push_external_checkpoint();

        let idx1 = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vb");
        journal.checkpoint_commit(&mut host, idx1).unwrap();

        let idx2 = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vc");
        journal.checkpoint_commit(&mut host, idx2).unwrap();

        journal.commit_frame(&mut host).unwrap();
        assert!(has_snap(&host, 0, &world));

        journal.commit_frame(&mut host).unwrap();
        assert!(!has_snap(&host, 0, &world));
        assert_eq!(read_data(&host, &world), b"vc");
    }

    // EVM(A) -> EVM(B) -> EVM(C) ~CRAC~> [commits] -> C commits -> B commits -> A reverts
    // Three-level nesting: snap 0 propagates up through two sub-call commits.
    #[test]
    fn test_three_nested_evm_frames_outer_reverts() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut host, &world, b"v0");

        journal.push_external_checkpoint();
        journal.push_external_checkpoint();
        journal.push_external_checkpoint();

        let idx = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vc");
        journal.checkpoint_commit(&mut host, idx).unwrap();

        // C commits: sub-call (B, A still on stack)
        journal.commit_frame(&mut host).unwrap();
        assert!(has_snap(&host, 0, &world));

        // B commits: sub-call (A still on stack)
        journal.commit_frame(&mut host).unwrap();
        assert!(has_snap(&host, 0, &world));

        // A reverts: outermost, snap 0 restores v0
        journal.revert_frame(&mut host).unwrap();
        assert_eq!(read_data(&host, &world), b"v0");
        assert!(!has_snap(&host, 0, &world));
    }

    // EVM(top) -> EVM(B) ~CRAC~> [B reverts via revert_frame] -> EVM(top) reverts
    // B's revert consumes the snapshot; top's revert finds nothing and is a noop.
    #[test]
    fn test_subcall_reverts_then_outer_reverts() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut host, &world, b"v0");

        journal.push_external_checkpoint();
        journal.push_external_checkpoint();

        let idx = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vb");
        journal.checkpoint_commit(&mut host, idx).unwrap();

        // B reverts: restores v0, snap 0 consumed
        journal.revert_frame(&mut host).unwrap();
        assert_eq!(read_data(&host, &world), b"v0");
        assert!(!has_snap(&host, 0, &world));

        // EVM(top) reverts: no snaps, noop
        journal.revert_frame(&mut host).unwrap();
        assert_eq!(read_data(&host, &world), b"v0");
    }

    // EVM(top) -> EVM(B) ~CRAC~> [B reverts] -> EVM(top) commits
    // B reverted its CRAC; top commits with no net Michelson change.
    #[test]
    fn test_subcall_reverts_then_outer_commits() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new(OperationHash::default());

        write_data(&mut host, &world, b"v0");

        journal.push_external_checkpoint();
        journal.push_external_checkpoint();

        let idx = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vb");
        journal.checkpoint_commit(&mut host, idx).unwrap();

        // B reverts: snap 0 consumed, world back to v0
        journal.revert_frame(&mut host).unwrap();
        assert_eq!(read_data(&host, &world), b"v0");

        // EVM(top) commits: no snaps to clean up, v0 persists
        journal.commit_frame(&mut host).unwrap();
        assert!(!has_snap(&host, 0, &world));
        assert_eq!(read_data(&host, &world), b"v0");
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
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new(OperationHash::default());
        write_data(&mut host, &world, b"v0");

        // A checkpoint
        journal.push_external_checkpoint();
        let _idx_a = journal.checkpoint(&mut host, &world).unwrap();
        journal.push_pending_crac_receipt(dummy_receipt(0));

        // B checkpoint
        journal.push_external_checkpoint();
        journal.push_pending_crac_receipt(dummy_receipt(1));
        assert_eq!(journal.pending_crac_receipts.len(), 2);
        assert_eq!(journal.backtracked_crac_receipts.len(), 0);

        // B reverts — receipt 1 migrated to backtracked, receipt 0 kept.
        journal.revert_frame(&mut host).unwrap();
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
        journal.checkpoint_commit(&mut host, 0).unwrap();
        journal.commit_frame(&mut host).unwrap();
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
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new(OperationHash::default());
        write_data(&mut host, &world, b"v0");

        // A checkpoint
        journal.push_external_checkpoint();
        let _idx_a = journal.checkpoint(&mut host, &world).unwrap();
        // B checkpoint — push and revert: B's receipt migrates.
        journal.push_external_checkpoint();
        journal.push_pending_crac_receipt(dummy_receipt(7));
        journal.revert_frame(&mut host).unwrap();
        assert_eq!(journal.backtracked_crac_receipts.len(), 1);

        // A reverts as well: must not affect the already-migrated entry.
        journal.revert_frame(&mut host).unwrap();
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

    // The index returned by `origination_index()` is the value last
    // written via `set_origination_index`; consecutive inbound CRACs
    // in the same synthetic op observe consecutive indices.
    #[test]
    fn test_origination_nonce_index_persists_across_calls() {
        let mut journal = MichelsonJournal::new(dummy_hash(0x01));
        assert_eq!(journal.origination_index(), 0);
        journal.set_origination_index(3); // CRAC A originated 3 children
        assert_eq!(journal.origination_index(), 3);
        journal.set_origination_index(5); // CRAC B originated 2 more
        assert_eq!(journal.origination_index(), 5);
    }

    // Reverting an EVM frame rolls the origination index back to the
    // value at frame entry, so child-KT1 slots burned inside the
    // reverted frame are released for reuse.
    #[test]
    fn test_origination_index_rolls_back_on_revert_frame() {
        let mut host = MockHost::default();
        let mut journal = MichelsonJournal::new(dummy_hash(0x42));
        // Outer EVM frame enters at index 0.
        journal.push_external_checkpoint();
        // Inbound CRAC inside that frame originated two children.
        journal.set_origination_index(2);
        assert_eq!(journal.origination_index(), 2);
        // Outer frame reverts: index must drop back to 0.
        journal.revert_frame(&mut host).unwrap();
        assert_eq!(journal.origination_index(), 0);
    }

    // Committing an EVM frame keeps the increments performed inside
    // it.  The next inbound CRAC continues from the post-frame
    // index, never re-using already-burned KT1 slots.
    #[test]
    fn test_origination_index_persists_on_commit_frame() {
        let mut host = MockHost::default();
        let mut journal = MichelsonJournal::new(dummy_hash(0x42));
        journal.push_external_checkpoint();
        journal.set_origination_index(2);
        journal.commit_frame(&mut host).unwrap();
        assert_eq!(journal.origination_index(), 2);
    }

    // Nested frames: an inner revert only rolls back the inner
    // frame's increments; the outer's prior increments survive.
    #[test]
    fn test_origination_index_nested_revert_only_inner() {
        let mut host = MockHost::default();
        let mut journal = MichelsonJournal::new(dummy_hash(0x42));
        // Outer frame
        journal.push_external_checkpoint();
        journal.set_origination_index(2); // outer CRAC originated 2
                                          // Inner frame
        journal.push_external_checkpoint();
        journal.set_origination_index(5); // inner CRAC originated 3 more
                                          // Inner reverts: index back to 2 (outer's tail), NOT to 0.
        journal.revert_frame(&mut host).unwrap();
        assert_eq!(journal.origination_index(), 2);
        // Outer commits: index stays at 2.
        journal.commit_frame(&mut host).unwrap();
        assert_eq!(journal.origination_index(), 2);
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

    // Reverting an EVM frame rolls the MIR counter back to the value at
    // frame entry: the internal operations applied inside the reverted
    // frame are backtracked, so their replay identities are released and
    // the next reentrant frame resumes from the pre-frame counter.
    #[test]
    fn test_internal_operation_counter_rolls_back_on_revert_frame() {
        let mut host = MockHost::default();
        let mut journal = MichelsonJournal::new(dummy_hash(0x42));
        // Outer EVM frame enters at counter 0.
        journal.push_external_checkpoint();
        // Inbound CRAC inside that frame emitted two internal ops.
        journal.set_internal_operation_counter(2);
        assert_eq!(journal.internal_operation_counter(), 2);
        // Outer frame reverts: counter must drop back to 0.
        journal.revert_frame(&mut host).unwrap();
        assert_eq!(journal.internal_operation_counter(), 0);
    }

    // Committing an EVM frame keeps the increments performed inside it.
    // The next reentrant frame continues from the post-frame counter,
    // never re-using an already-assigned replay identity.
    #[test]
    fn test_internal_operation_counter_persists_on_commit_frame() {
        let mut host = MockHost::default();
        let mut journal = MichelsonJournal::new(dummy_hash(0x42));
        journal.push_external_checkpoint();
        journal.set_internal_operation_counter(2);
        journal.commit_frame(&mut host).unwrap();
        assert_eq!(journal.internal_operation_counter(), 2);
    }

    // Nested frames: an inner revert only rolls back the inner frame's
    // increments; the outer frame's prior increments survive.
    #[test]
    fn test_internal_operation_counter_nested_revert_only_inner() {
        let mut host = MockHost::default();
        let mut journal = MichelsonJournal::new(dummy_hash(0x42));
        // Outer frame
        journal.push_external_checkpoint();
        journal.set_internal_operation_counter(2); // outer frame emitted 2
                                                   // Inner frame
        journal.push_external_checkpoint();
        journal.set_internal_operation_counter(5); // inner frame emitted 3 more
                                                   // Inner reverts: counter back to 2 (outer's tail), NOT to 0.
        journal.revert_frame(&mut host).unwrap();
        assert_eq!(journal.internal_operation_counter(), 2);
        // Outer commits: counter stays at 2.
        journal.commit_frame(&mut host).unwrap();
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

    #[test]
    fn test_pending_alias_originations_roll_back_on_revert_frame() {
        let mut host = MockHost::default();
        let mut journal = MichelsonJournal::new(dummy_hash(0x42));
        journal.push_external_checkpoint();
        journal.push_pending_alias_origination_internal(dummy_alias_internal());
        journal.revert_frame(&mut host).unwrap();
        assert!(journal
            .take_pending_alias_origination_internals()
            .is_empty());
    }

    #[test]
    fn test_pending_alias_originations_persist_on_commit_frame() {
        let mut host = MockHost::default();
        let mut journal = MichelsonJournal::new(dummy_hash(0x42));
        journal.push_external_checkpoint();
        journal.push_pending_alias_origination_internal(dummy_alias_internal());
        journal.commit_frame(&mut host).unwrap();
        assert_eq!(journal.take_pending_alias_origination_internals().len(), 1);
    }

    #[test]
    fn test_pending_alias_originations_nested_revert_only_inner() {
        let mut host = MockHost::default();
        let mut journal = MichelsonJournal::new(dummy_hash(0x42));
        journal.push_external_checkpoint();
        journal.push_pending_alias_origination_internal(dummy_alias_internal());
        journal.push_external_checkpoint();
        journal.push_pending_alias_origination_internal(dummy_alias_internal());
        journal.push_pending_alias_origination_internal(dummy_alias_internal());
        journal.revert_frame(&mut host).unwrap();
        journal.commit_frame(&mut host).unwrap();
        assert_eq!(journal.take_pending_alias_origination_internals().len(), 1);
    }
}
