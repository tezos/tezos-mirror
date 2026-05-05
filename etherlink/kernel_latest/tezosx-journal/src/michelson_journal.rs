// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup_host::{
    path::{concat, OwnedPath, Path, RefPath},
    runtime::RuntimeError,
    storage::StorageV1,
};
use tezos_tezlink::{
    block::AppliedOperation,
    operation_result::{OperationDataAndMetadata, OperationResultSum},
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
    /// Payload deposited by Michelson via `%collect_result` during this
    /// frame.  Populated at most once per frame; dropped (alongside the
    /// checkpoint) on `commit_frame`/`revert_frame`.
    frame_result: Option<Vec<u8>>,
}

/// Error returned by [`MichelsonJournal::set_frame_result`].
#[derive(Debug, PartialEq, Eq)]
pub enum SetFrameResultError {
    /// No external checkpoint is active — there is no frame to write to.
    NoFrame,
    /// The current frame already holds a result (once-per-frame invariant).
    AlreadySet,
}

impl core::fmt::Display for SetFrameResultError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::NoFrame => f.write_str("no active external checkpoint"),
            Self::AlreadySet => f.write_str("frame result already set"),
        }
    }
}

impl std::error::Error for SetFrameResultError {}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct MichelsonJournal {
    snapshots: Vec<OwnedPath>,
    external_checkpoints: Vec<ExternalCheckpoint>,
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
}

impl MichelsonJournal {
    pub fn new() -> Self {
        Self::default()
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
            .expect("CRAC receipt sequence counter overflowed u64");
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
}

pub fn indexed_path<T: Path>(depth: usize, path: &T) -> Result<OwnedPath, RuntimeError> {
    let path_bytes = format!("/{depth}");
    let indexed_path = RefPath::assert_from(path_bytes.as_bytes());
    concat(&indexed_path, path).map_err(|_| RuntimeError::PathNotFound)
}

impl MichelsonJournal {
    // Called by an external journal on checkpoint creation.
    // Records the current snapshot count and receipt count as the
    // lower boundary for this call frame.
    pub fn push_external_checkpoint(&mut self) {
        self.external_checkpoints.push(ExternalCheckpoint {
            snapshot_watermark: self.snapshots.len(),
            receipt_count: self.pending_crac_receipts.len(),
            frame_result: None,
        });
    }

    /// Returns the current frame's `%collect_result` payload, if one has
    /// been set.  Non-destructive: repeated calls observe the same value
    /// until the frame is committed or reverted.
    pub fn frame_result(&self) -> Option<&[u8]> {
        self.external_checkpoints.last()?.frame_result.as_deref()
    }

    /// Deposit the `%collect_result` payload on the topmost frame.
    ///
    /// Fails with [`SetFrameResultError::NoFrame`] if called outside an
    /// external checkpoint, or [`SetFrameResultError::AlreadySet`] if the
    /// current frame already holds a result (once-per-frame invariant).
    pub fn set_frame_result(
        &mut self,
        bytes: Vec<u8>,
    ) -> Result<(), SetFrameResultError> {
        let top = self
            .external_checkpoints
            .last_mut()
            .ok_or(SetFrameResultError::NoFrame)?;
        if top.frame_result.is_some() {
            return Err(SetFrameResultError::AlreadySet);
        }
        top.frame_result = Some(bytes);
        Ok(())
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
                frame_result: None,
            });
        let drain_from = if self.external_checkpoints.is_empty() {
            checkpoint.snapshot_watermark
        } else {
            (checkpoint.snapshot_watermark + 1).min(self.snapshots.len())
        };
        for snapshot in self.snapshots.drain(drain_from..) {
            host.store_delete(&snapshot)?;
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
    // `backtracked` rather than vanishing).
    pub fn revert_frame<Host>(
        &mut self,
        host: &mut Host,
        to_path: &OwnedPath,
    ) -> Result<(), RuntimeError>
    where
        Host: StorageV1,
    {
        let checkpoint = self
            .external_checkpoints
            .pop()
            .unwrap_or(ExternalCheckpoint {
                snapshot_watermark: 0,
                receipt_count: 0,
                frame_result: None,
            });
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
            host.store_delete(&snapshot)?;
        }
        if let Some(snapshot) = self.snapshots.pop() {
            host.store_move(&snapshot, to_path)?;
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
        let indexed_path = indexed_path(new_index, from_path)?;
        host.store_copy(from_path, &indexed_path)?;
        self.snapshots.push(indexed_path);
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
            host.store_delete(&snapshot)?;
        }
        Ok(())
    }

    // Called by Michelson CRAC logic.
    //
    // Removes snapshots taken after checkpoint_index, then restores durable
    // storage from the snapshot at checkpoint_index.
    pub fn checkpoint_revert<Host>(
        &mut self,
        host: &mut Host,
        to_path: &OwnedPath,
        checkpoint_index: usize,
    ) -> Result<(), RuntimeError>
    where
        Host: StorageV1,
    {
        let start = (checkpoint_index + 1).min(self.snapshots.len());
        for snapshot in self.snapshots.drain(start..) {
            host.store_delete(&snapshot)?;
        }
        if let Some(snapshot) = self.snapshots.pop() {
            host.store_move(&snapshot, to_path)?;
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
        let mut journal = MichelsonJournal::new();

        write_data(&mut host, &world, b"v0");

        // A enters: watermark=0, no snapshots
        journal.push_external_checkpoint();

        // B starts: snap 0 captures v0, B writes vb
        let idx_b = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vb");
        // B FAILWITH: snap 0 consumed, world restored to v0
        journal.checkpoint_revert(&mut host, &world, idx_b).unwrap();

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
        let mut journal = MichelsonJournal::new();

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
        journal.checkpoint_revert(&mut host, &world, idx_d).unwrap();
        assert_eq!(read_data(&host, &world), b"vc");

        // C propagates: snap 1 consumed, world restored to vb
        journal.checkpoint_revert(&mut host, &world, idx_c).unwrap();
        assert_eq!(read_data(&host, &world), b"vb");

        // B propagates: snap 0 consumed, world restored to v0
        journal.checkpoint_revert(&mut host, &world, idx_b).unwrap();
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
        let mut journal = MichelsonJournal::new();

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
        journal.checkpoint_revert(&mut host, &world, idx_d).unwrap();
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
        let mut journal = MichelsonJournal::new();

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
        journal.checkpoint_revert(&mut host, &world, idx_c).unwrap();
        assert_eq!(read_data(&host, &world), b"va");

        // B has no catch: revert_frame is noop (no snaps at watermark or above remain)
        journal.revert_frame(&mut host, &world).unwrap();
        assert_eq!(read_data(&host, &world), b"va");
        assert!(has_snap(&host, 0, &world));

        // A propagates failure: snap 0 consumed, world restored to v0
        journal.checkpoint_revert(&mut host, &world, idx_a).unwrap();

        assert_eq!(read_data(&host, &world), b"v0");
        assert!(!has_snap(&host, 0, &world));
    }

    // US-5: EVM(A, try-catch): Mich(B) succeeds, then EVM op fails
    // try block reverts: B's pending snapshot is consumed, catch runs on clean state
    #[test]
    fn test_us5_evm_try_mich_succeeds_then_evm_op_fails() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new();

        write_data(&mut host, &world, b"v0");

        // A enters try block: watermark=0
        journal.push_external_checkpoint();

        // B starts: snap 0 captures v0, B writes vb, B commits (snap 0 remains)
        let idx_b = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vb");
        journal.checkpoint_commit(&mut host, idx_b).unwrap();
        assert!(has_snap(&host, 0, &world));

        // EVM op fails: try block reverts, snap 0 consumed, world restored to v0
        journal.revert_frame(&mut host, &world).unwrap();

        assert_eq!(read_data(&host, &world), b"v0");
        assert!(!has_snap(&host, 0, &world));
    }

    // US-6: EVM(A), Mich(B), EVM(C), Mich(D), EVM(E), Mich(F), FAILWITH
    // only F reverts, E's revert is noop, C is sub-call (snap 1 deferred to A's commit)
    #[test]
    fn test_us6_deep_alternating_stack_innermost_failwith() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new();

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
        journal.checkpoint_revert(&mut host, &world, idx_f).unwrap();
        assert_eq!(read_data(&host, &world), b"vd");

        // E reverts: noop (no snaps at watermark=2 or above remain)
        journal.revert_frame(&mut host, &world).unwrap();
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
        let mut journal = MichelsonJournal::new();

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
        journal.revert_frame(&mut host, &world).unwrap();

        assert_eq!(read_data(&host, &world), b"v0");
        assert!(!has_snap(&host, 0, &world));
    }

    // US-8: Mich(A), EVM(B), Mich(C) succeed, then Mich(A), EVM(D) REVERT
    // all-or-nothing: A reverts everything including B and C's committed changes
    #[test]
    fn test_us8_mich_outer_second_evm_call_reverts_all() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new();

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
        journal.revert_frame(&mut host, &world).unwrap();
        assert_eq!(read_data(&host, &world), b"vc");
        assert!(has_snap(&host, 0, &world));

        // A reverts all-or-nothing: snap 0 consumed, world restored to v0
        journal.checkpoint_revert(&mut host, &world, idx_a).unwrap();

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
        let mut journal = MichelsonJournal::new();

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
        journal.revert_frame(&mut host, &world).unwrap();

        assert_eq!(read_data(&host, &world), b"v0");
        assert!(!has_snap(&host, 0, &world));
    }

    // edge case: revert with no snapshots is a noop
    #[test]
    fn test_edge_revert_on_empty_journal() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new();

        write_data(&mut host, &world, b"v0");

        journal.push_external_checkpoint();
        journal.revert_frame(&mut host, &world).unwrap();

        assert_eq!(read_data(&host, &world), b"v0");
    }

    // edge case: external frame with no Michelson CRACs, revert is noop
    #[test]
    fn test_edge_external_frame_no_cracs_revert_noop() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new();

        write_data(&mut host, &world, b"v0");

        // a CRAC committed outside the frame: snap 0 remains
        let idx = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"v1");
        journal.checkpoint_commit(&mut host, idx).unwrap();

        // frame with no CRACs: watermark=1, revert is noop
        journal.push_external_checkpoint();
        journal.revert_frame(&mut host, &world).unwrap();

        assert_eq!(read_data(&host, &world), b"v1");
        assert!(has_snap(&host, 0, &world));
    }

    // edge case: external frame with no Michelson CRACs, commit is noop
    #[test]
    fn test_edge_external_frame_no_cracs_commit_noop() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new();

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
        let mut journal = MichelsonJournal::new();

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
        journal.revert_frame(&mut host, &world).unwrap();

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
        let mut journal = MichelsonJournal::new();

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
        let mut journal = MichelsonJournal::new();

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
        journal.revert_frame(&mut host, &world).unwrap();
        assert_eq!(read_data(&host, &world), b"v0");
        assert!(!has_snap(&host, 0, &world));
        assert!(!has_snap(&host, 1, &world));
    }

    // Same as above but outer commits: both sub-calls' changes persist.
    #[test]
    fn test_sequential_subcalls_with_cracs_outer_commits() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new();

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
        let mut journal = MichelsonJournal::new();

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
        journal.revert_frame(&mut host, &world).unwrap();
        assert_eq!(read_data(&host, &world), b"v0");
        assert!(!has_snap(&host, 0, &world));
    }

    // Same as above but outer commits: both CRACs' net effect (vc) persists.
    #[test]
    fn test_subcall_two_cracs_outer_commits() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new();

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
        let mut journal = MichelsonJournal::new();

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
        journal.revert_frame(&mut host, &world).unwrap();
        assert_eq!(read_data(&host, &world), b"v0");
        assert!(!has_snap(&host, 0, &world));
    }

    // EVM(top) -> EVM(B) ~CRAC~> [B reverts via revert_frame] -> EVM(top) reverts
    // B's revert consumes the snapshot; top's revert finds nothing and is a noop.
    #[test]
    fn test_subcall_reverts_then_outer_reverts() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new();

        write_data(&mut host, &world, b"v0");

        journal.push_external_checkpoint();
        journal.push_external_checkpoint();

        let idx = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vb");
        journal.checkpoint_commit(&mut host, idx).unwrap();

        // B reverts: restores v0, snap 0 consumed
        journal.revert_frame(&mut host, &world).unwrap();
        assert_eq!(read_data(&host, &world), b"v0");
        assert!(!has_snap(&host, 0, &world));

        // EVM(top) reverts: no snaps, noop
        journal.revert_frame(&mut host, &world).unwrap();
        assert_eq!(read_data(&host, &world), b"v0");
    }

    // EVM(top) -> EVM(B) ~CRAC~> [B reverts] -> EVM(top) commits
    // B reverted its CRAC; top commits with no net Michelson change.
    #[test]
    fn test_subcall_reverts_then_outer_commits() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new();

        write_data(&mut host, &world, b"v0");

        journal.push_external_checkpoint();
        journal.push_external_checkpoint();

        let idx = journal.checkpoint(&mut host, &world).unwrap();
        write_data(&mut host, &world, b"vb");
        journal.checkpoint_commit(&mut host, idx).unwrap();

        // B reverts: snap 0 consumed, world back to v0
        journal.revert_frame(&mut host, &world).unwrap();
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
        let mut journal = MichelsonJournal::new();
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
        journal.revert_frame(&mut host, &world).unwrap();
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
        let mut journal = MichelsonJournal::new();
        write_data(&mut host, &world, b"v0");

        // A checkpoint
        journal.push_external_checkpoint();
        let _idx_a = journal.checkpoint(&mut host, &world).unwrap();
        // B checkpoint — push and revert: B's receipt migrates.
        journal.push_external_checkpoint();
        journal.push_pending_crac_receipt(dummy_receipt(7));
        journal.revert_frame(&mut host, &world).unwrap();
        assert_eq!(journal.backtracked_crac_receipts.len(), 1);

        // A reverts as well: must not affect the already-migrated entry.
        journal.revert_frame(&mut host, &world).unwrap();
        assert_eq!(journal.backtracked_crac_receipts.len(), 1);
        assert_eq!(receipt_id(&journal.backtracked_crac_receipts[0].1), 7);
    }

    // --- frame result slot ---

    // With no active frame, `frame_result` observes nothing and
    // `set_frame_result` refuses to write.
    #[test]
    fn test_frame_result_no_frame() {
        let mut journal = MichelsonJournal::new();
        assert_eq!(journal.frame_result(), None);
        assert_eq!(
            journal.set_frame_result(vec![1, 2, 3]),
            Err(SetFrameResultError::NoFrame)
        );
    }

    // A payload deposited on the top frame is visible to repeated reads
    // until the frame is dropped.
    #[test]
    fn test_frame_result_set_is_observable() {
        let mut journal = MichelsonJournal::new();
        journal.push_external_checkpoint();
        assert_eq!(journal.frame_result(), None);

        journal.set_frame_result(vec![0xCA, 0xFE]).unwrap();
        assert_eq!(journal.frame_result(), Some(&[0xCA, 0xFE][..]));
        // Non-destructive: still there on re-read.
        assert_eq!(journal.frame_result(), Some(&[0xCA, 0xFE][..]));
    }

    // `commit_frame` drops the slot: after commit, the parent frame's
    // (absent) payload is what `frame_result` observes.
    #[test]
    fn test_frame_result_commit_drops_slot() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new();
        write_data(&mut host, &world, b"v0");

        journal.push_external_checkpoint();
        journal.push_external_checkpoint();
        journal.set_frame_result(vec![0xAA]).unwrap();
        assert_eq!(journal.frame_result(), Some(&[0xAA][..]));

        journal.commit_frame(&mut host).unwrap();
        // Outer frame has no result of its own.
        assert_eq!(journal.frame_result(), None);
    }

    // `revert_frame` drops the slot just like `commit_frame`.  A fresh
    // frame pushed afterwards starts empty.
    #[test]
    fn test_frame_result_revert_drops_slot() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new();
        write_data(&mut host, &world, b"v0");

        journal.push_external_checkpoint();
        journal.set_frame_result(vec![0xBB]).unwrap();
        journal.revert_frame(&mut host, &world).unwrap();

        journal.push_external_checkpoint();
        assert_eq!(journal.frame_result(), None);
    }

    // Second call to `set_frame_result` on the same frame fails; the
    // first value is kept intact.
    #[test]
    fn test_frame_result_double_set_fails() {
        let mut journal = MichelsonJournal::new();
        journal.push_external_checkpoint();
        journal.set_frame_result(vec![0x01]).unwrap();
        assert_eq!(
            journal.set_frame_result(vec![0x02]),
            Err(SetFrameResultError::AlreadySet)
        );
        assert_eq!(journal.frame_result(), Some(&[0x01][..]));
    }

    // Nested frames get independent slots: the inner payload is never
    // observable from the outer frame, and vice versa.
    #[test]
    fn test_frame_result_nested_frames_independent() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new();
        write_data(&mut host, &world, b"v0");

        // Outer frame sets its payload.
        journal.push_external_checkpoint();
        journal.set_frame_result(vec![0xAA]).unwrap();

        // Inner frame starts empty and can hold its own payload.
        journal.push_external_checkpoint();
        assert_eq!(journal.frame_result(), None);
        journal.set_frame_result(vec![0xBB]).unwrap();
        assert_eq!(journal.frame_result(), Some(&[0xBB][..]));

        // Committing the inner frame uncovers the outer's payload.
        journal.commit_frame(&mut host).unwrap();
        assert_eq!(journal.frame_result(), Some(&[0xAA][..]));

        // Outer frame still refuses a second set.
        assert_eq!(
            journal.set_frame_result(vec![0xCC]),
            Err(SetFrameResultError::AlreadySet)
        );
    }
}
