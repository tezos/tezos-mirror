// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup_host::{
    path::{concat, OwnedPath, Path, RefPath},
    runtime::RuntimeError,
    storage::StorageV1,
};
use tezos_tezlink::block::AppliedOperation;

#[derive(Debug, PartialEq, Eq)]
struct ExternalCheckpoint {
    /// Index into `snapshots` at the time the checkpoint was created.
    snapshot_watermark: usize,
    /// Length of `pending_crac_receipts` at the time the checkpoint was
    /// created.  On revert, receipts are truncated back to this count.
    receipt_count: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub struct MichelsonJournal {
    snapshots: Vec<OwnedPath>,
    external_checkpoints: Vec<ExternalCheckpoint>,
    /// Successful CRAC receipts (EVM → Michelson).  Subject to revert:
    /// truncated by `revert_frame` when the calling EVM frame reverts.
    pub pending_crac_receipts: Vec<AppliedOperation>,
    /// Failed CRAC receipts.  NOT subject to revert: a failed CRAC is
    /// recorded even when the EVM transaction reverts entirely, so that
    /// the Michelson block shows the attempt with `status: failed`.
    pub failed_crac_receipts: Vec<AppliedOperation>,
    /// Whether the next incoming CRAC should emit a CRAC-ID event.
    /// Set to `false` when Michelson originates a CRAC or emits a
    /// CRAC-ID event.  Stays `false` for the lifetime of the journal
    /// (one transaction); a fresh journal is created per transaction.
    should_emit_crac_id: bool,
}

impl Default for MichelsonJournal {
    fn default() -> Self {
        Self {
            snapshots: Vec::new(),
            external_checkpoints: Vec::new(),
            pending_crac_receipts: Vec::new(),
            failed_crac_receipts: Vec::new(),
            should_emit_crac_id: true,
        }
    }
}

impl MichelsonJournal {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn should_emit_crac_id(&self) -> bool {
        self.should_emit_crac_id
    }

    /// Suppress CRAC-ID event emission (Michelson is originating a
    /// CRAC or has already emitted the event for this chain).
    pub fn suppress_crac_id(&mut self) {
        self.should_emit_crac_id = false;
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
        });
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
    // Pops the current frame's watermark, reverts durable storage to the
    // state captured at the start of this call frame, and drops any
    // CRAC receipts pushed since the checkpoint.
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
            });
        // Drop CRAC receipts pushed during this frame.
        self.pending_crac_receipts
            .truncate(checkpoint.receipt_count);
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

    // Revert drops CRAC receipts pushed during the reverted frame.
    //
    //   EVM(A) checkpoint → push receipt 0
    //     EVM(B) checkpoint → push receipt 1
    //     B reverts → receipt 1 dropped
    //   push receipt 2
    //   A commits → receipts = [0, 2]
    #[test]
    fn test_revert_frame_drops_receipts() {
        let mut host = MockHost::default();
        let world = world_path();
        let mut journal = MichelsonJournal::new();
        write_data(&mut host, &world, b"v0");

        // A checkpoint
        journal.push_external_checkpoint();
        let _idx_a = journal.checkpoint(&mut host, &world).unwrap();
        journal.pending_crac_receipts.push(dummy_receipt(0));

        // B checkpoint
        journal.push_external_checkpoint();
        journal.pending_crac_receipts.push(dummy_receipt(1));
        assert_eq!(journal.pending_crac_receipts.len(), 2);

        // B reverts — receipt 1 should be dropped, receipt 0 kept
        journal.revert_frame(&mut host, &world).unwrap();
        assert_eq!(journal.pending_crac_receipts.len(), 1);
        assert_eq!(receipt_id(&journal.pending_crac_receipts[0]), 0);

        // Push another receipt after revert
        journal.pending_crac_receipts.push(dummy_receipt(2));
        assert_eq!(journal.pending_crac_receipts.len(), 2);
        assert_eq!(receipt_id(&journal.pending_crac_receipts[0]), 0);
        assert_eq!(receipt_id(&journal.pending_crac_receipts[1]), 2);

        // A commits — both receipts preserved
        journal.checkpoint_commit(&mut host, 0).unwrap();
        journal.commit_frame(&mut host).unwrap();
        assert_eq!(journal.pending_crac_receipts.len(), 2);
        assert_eq!(receipt_id(&journal.pending_crac_receipts[0]), 0);
        assert_eq!(receipt_id(&journal.pending_crac_receipts[1]), 2);
    }
}
