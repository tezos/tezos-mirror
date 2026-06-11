// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Storage-backed, revertible transaction over a single keyspace `root`.
//!
//! `start` copies the live `root` into a depth-0 backup keyspace and sets an
//! in-progress marker; block code then mutates the live `root` in place.
//! `commit` discards the backups and clears the marker; `revert` moves the
//! depth-0 backup back over the live `root`. `savepoint`/`release`/`rollback`
//! apply the same copy-then-restore at deeper frames for nested
//! sub-transactions. Backups and marker live in storage, so a reboot
//! mid-block is recovered by `resume`.

use tezos_smart_rollup_keyspace::{
    Key, KeySpace, KeySpaceLoader, KeySpaceLoaderError, Name, NameError,
};

#[derive(Debug)]
pub enum SnapshotError {
    /// Underlying loader failure.
    Loader(KeySpaceLoaderError),
    /// `resume` found no in-progress marker for this `root`.
    NothingToResume,
    /// The live `root` handle was still held when the transaction boundary was
    /// crossed; the caller must drop it first.
    RootHeldAcrossBoundary(Name),
    /// A derived backup or meta keyspace name was not a valid `Name` (e.g.
    /// savepoints nested deep enough to exceed the name length limit).
    InvalidName(NameError),
}

impl From<KeySpaceLoaderError> for SnapshotError {
    fn from(e: KeySpaceLoaderError) -> Self {
        Self::Loader(e)
    }
}

const FRAME_PREFIX: &str = "/__snapshot/frame";

/// Backup keyspace name for `root` at frame `depth`.
/// (`start` uses 0 for the block; `savepoint` uses ≥1).
fn frame_name(root: &Name, depth: usize) -> Result<Name, SnapshotError> {
    format!("{FRAME_PREFIX}/{depth}{root}")
        .parse()
        .map_err(SnapshotError::InvalidName)
}

/// Whether `root`'s backup name still parses as a valid keyspace name at
/// `max_depth`. Consumers call this to bound how deep savepoints may nest.
pub fn backup_name_fits(root: &str, max_depth: usize) -> bool {
    format!("{FRAME_PREFIX}/{max_depth}{root}")
        .parse::<Name>()
        .is_ok()
}

const META_PREFIX: &str = "/__snapshot/meta";

/// Per-root keyspace holding this root's in-progress marker.
fn meta_name(root: &Name) -> Result<Name, SnapshotError> {
    format!("{META_PREFIX}{root}")
        .parse()
        .map_err(SnapshotError::InvalidName)
}

const IN_PROGRESS_KEY: &Key = &Key::from_static(b"/in_progress");

const IN_PROGRESS_MARKER: &[u8] = &[1];

/// Load the live `name`, mapping `AlreadyLoaded` to `RootHeldAcrossBoundary`
/// (the caller did not drop its handle before this boundary).
fn load_live<L: KeySpaceLoader>(
    loader: &mut L,
    name: &Name,
) -> Result<L::KeySpace, SnapshotError> {
    loader.load_or_create(name.clone()).map_err(|e| match e {
        KeySpaceLoaderError::AlreadyLoaded => {
            SnapshotError::RootHeldAcrossBoundary(name.clone())
        }
        other => SnapshotError::Loader(other),
    })
}

/// Copy the live `root` into a fresh `backup_name` keyspace and return that
/// backup handle. Overwrites any stale backup; drops the live handle.
fn snapshot_root<L: KeySpaceLoader>(
    loader: &mut L,
    root: &Name,
    backup_name: Name,
) -> Result<L::KeySpace, SnapshotError> {
    let live = load_live(loader, root)?;
    let mut backup = loader.load_or_create(backup_name)?;
    backup.copy_from(&live);
    drop(live);
    Ok(backup)
}

/// Move `backup` back over the live `root`, replacing its contents and
/// emptying `backup`.
fn restore_root<L: KeySpaceLoader>(
    loader: &mut L,
    root: &Name,
    backup: &mut L::KeySpace,
) -> Result<(), SnapshotError> {
    let mut live = load_live(loader, root)?;
    live.move_from(backup);
    Ok(())
}

/// An open transaction on a single `root`.
pub struct Snapshot<KS: KeySpace> {
    /// The keyspace under transaction.
    root: Name,
    /// Backup stack: `frames[0]` is the block, `frames[1..]` are savepoints.
    frames: Vec<KS>,
    /// Keyspace holding the in-progress marker.
    meta: KS,
}

impl<KS: KeySpace> Snapshot<KS> {
    /// Begin a transaction: back up the live `root` at depth 0 and set the
    /// in-progress marker.
    pub fn start(
        loader: &mut impl KeySpaceLoader<KeySpace = KS>,
        root: &Name,
    ) -> Result<Self, SnapshotError> {
        let backup = snapshot_root(loader, root, frame_name(root, 0)?)?;
        let mut meta = loader.load_or_create(meta_name(root)?)?;
        meta.set(IN_PROGRESS_KEY, IN_PROGRESS_MARKER)
            .expect("the in-progress marker is a tiny value");
        Ok(Self {
            root: root.clone(),
            frames: vec![backup],
            meta,
        })
    }

    /// Re-attach to a transaction interrupted by a reboot: the depth-0 backup
    /// still holds the pre-block state and the live `root` keeps its partial
    /// work. Fails `NothingToResume` if `root` has no in-progress marker.
    pub fn resume(
        loader: &mut impl KeySpaceLoader<KeySpace = KS>,
        root: &Name,
    ) -> Result<Self, SnapshotError> {
        let meta = loader.load_or_create(meta_name(root)?)?;
        if !meta.contains(IN_PROGRESS_KEY) {
            return Err(SnapshotError::NothingToResume);
        }
        let backup = loader.load_or_create(frame_name(root, 0)?)?;
        Ok(Self {
            root: root.clone(),
            frames: vec![backup],
            meta,
        })
    }

    /// Open a nested sub-transaction: back up the current live `root` at the
    /// next frame depth.
    pub fn savepoint(
        &mut self,
        loader: &mut impl KeySpaceLoader<KeySpace = KS>,
    ) -> Result<(), SnapshotError> {
        let depth = self.frames.len();
        let backup = snapshot_root(loader, &self.root, frame_name(&self.root, depth)?)?;
        self.frames.push(backup);
        Ok(())
    }

    /// Pop the top savepoint backup, or `None` if only the block frame remains
    /// (the block frame is never popped).
    fn pop_savepoint(&mut self) -> Option<KS> {
        if self.frames.len() > 1 {
            self.frames.pop()
        } else {
            None
        }
    }

    /// Accept the top savepoint: discard its backup, keeping the live writes.
    /// No-op with no open savepoint.
    pub fn release(&mut self) {
        if let Some(mut backup) = self.pop_savepoint() {
            backup.clear();
        }
    }

    /// Undo the top savepoint: restore the live `root` from its backup. No-op
    /// with no open savepoint.
    pub fn rollback(
        &mut self,
        loader: &mut impl KeySpaceLoader<KeySpace = KS>,
    ) -> Result<(), SnapshotError> {
        match self.pop_savepoint() {
            Some(mut backup) => restore_root(loader, &self.root, &mut backup),
            None => Ok(()),
        }
    }

    /// Accept the whole transaction: discard all backups and clear the marker.
    /// The live `root` keeps every write.
    pub fn commit(mut self) {
        for backup in &mut self.frames {
            backup.clear();
        }
        self.meta.clear();
    }

    /// Undo the whole transaction: discard savepoint backups, restore the live
    /// `root` from the depth-0 block backup, and clear the marker.
    pub fn revert(
        mut self,
        loader: &mut impl KeySpaceLoader<KeySpace = KS>,
    ) -> Result<(), SnapshotError> {
        for backup in self.frames.iter_mut().skip(1) {
            backup.clear();
        }
        restore_root(loader, &self.root, &mut self.frames[0])?;
        self.meta.clear();
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::MockKernelHost;

    /// A safe root, addressed by keyspace name.
    const ROOT_A: &str = "/evm/world_state";
    const ROOT_B: &str = "/tez/tez_accounts";

    fn root_a() -> Name {
        ROOT_A.parse().unwrap()
    }

    fn root_b() -> Name {
        ROOT_B.parse().unwrap()
    }

    fn key(k: &[u8]) -> Key {
        Key::from_bytes(k).unwrap()
    }

    /// Write `value` at `k` inside the keyspace `name`, through the KeySpace
    /// API. The handle is loaded and dropped within the call, mirroring how a
    /// consumer touches a root transiently between transaction boundaries.
    fn write(host: &mut MockKernelHost, name: &str, k: &[u8], value: &[u8]) {
        let mut ks = host.load_or_create(name.parse().unwrap()).unwrap();
        ks.set(&key(k), value).unwrap();
    }

    /// Read the value at `k` inside the keyspace `name`, through the KeySpace
    /// API. Requires `&mut` because loading a keyspace takes a (transient)
    /// owner handle.
    fn read(host: &mut MockKernelHost, name: &str, k: &[u8]) -> Option<Vec<u8>> {
        let ks = host.load_or_create(name.parse().unwrap()).unwrap();
        ks.get(&key(k))
    }

    /// The block frame (depth 0) backup of ROOT_A.
    const BLOCK_A: &str = "/__snapshot/frame/0/evm/world_state";

    // ----- Frame naming / budget -----

    #[test]
    fn frame_name_keys_by_depth() {
        let root: Name = "/tez/tez_accounts".parse().unwrap();
        // Depth 0 is the block; deeper frames are savepoints.
        assert_eq!(
            frame_name(&root, 0).unwrap().as_ref(),
            "/__snapshot/frame/0/tez/tez_accounts"
        );
        assert_eq!(
            frame_name(&root, 1).unwrap().as_ref(),
            "/__snapshot/frame/1/tez/tez_accounts"
        );
    }

    #[test]
    fn backup_name_fits_for_representative_roots() {
        // The budget guard the consumer drives with its authoritative root set
        // (see the kernel-side test next to the safe-root `RefPath`s). Far beyond
        // any realistic savepoint nesting depth — the two-phase op pattern only
        // nests one or two deep.
        const MAX_DEPTH: usize = 9999;
        for root in [ROOT_A, ROOT_B] {
            assert!(backup_name_fits(root, MAX_DEPTH));
        }
    }

    // ----- Block level -----

    #[test]
    fn commit_keeps_live_writes() {
        let mut host = MockKernelHost::default();
        write(&mut host, ROOT_A, b"/balance", b"initial");

        let snapshot = Snapshot::start(&mut host, &root_a()).unwrap();

        // Mutate the live root directly, as block execution would.
        write(&mut host, ROOT_A, b"/balance", b"updated");
        write(&mut host, ROOT_A, b"/new", b"value");

        snapshot.commit();

        assert_eq!(
            read(&mut host, ROOT_A, b"/balance"),
            Some(b"updated".to_vec())
        );
        assert_eq!(read(&mut host, ROOT_A, b"/new"), Some(b"value".to_vec()));
        // Backup discarded.
        assert_eq!(read(&mut host, BLOCK_A, b"/balance"), None);
    }

    #[test]
    fn revert_restores_pre_block_state() {
        let mut host = MockKernelHost::default();
        write(&mut host, ROOT_A, b"/balance", b"initial");

        let snapshot = Snapshot::start(&mut host, &root_a()).unwrap();

        // Mutate the live root.
        write(&mut host, ROOT_A, b"/balance", b"updated");
        write(&mut host, ROOT_A, b"/new", b"value");

        snapshot.revert(&mut host).unwrap();

        // Everything restored to the pre-block snapshot.
        assert_eq!(
            read(&mut host, ROOT_A, b"/balance"),
            Some(b"initial".to_vec())
        );
        assert_eq!(read(&mut host, ROOT_A, b"/new"), None);
    }

    #[test]
    fn revert_clears_root_that_started_empty() {
        let mut host = MockKernelHost::default();
        // ROOT_A starts empty (no writes before the snapshot).
        let snapshot = Snapshot::start(&mut host, &root_a()).unwrap();

        write(&mut host, ROOT_A, b"/created", b"during-block");

        snapshot.revert(&mut host).unwrap();

        // A root that did not exist before the block must be empty again.
        assert_eq!(read(&mut host, ROOT_A, b"/created"), None);
    }

    #[test]
    fn resume_then_revert_uses_existing_backup() {
        let mut host = MockKernelHost::default();
        write(&mut host, ROOT_A, b"/balance", b"initial");

        // First reboot: take a snapshot and accumulate partial work, then
        // drop the snapshot without committing (simulating a reboot).
        {
            let snapshot = Snapshot::start(&mut host, &root_a()).unwrap();
            write(&mut host, ROOT_A, b"/balance", b"partial");
            drop(snapshot);
        }

        // The backup from the first reboot must still hold the original value.
        assert_eq!(
            read(&mut host, BLOCK_A, b"/balance"),
            Some(b"initial".to_vec())
        );

        // Second reboot: resume (no fresh snapshot), do more work, then fail.
        {
            let snapshot = Snapshot::resume(&mut host, &root_a()).unwrap();
            write(&mut host, ROOT_A, b"/balance", b"more-partial");
            snapshot.revert(&mut host).unwrap();
        }

        // Revert must restore the original pre-block state, not the partial
        // work from either reboot.
        assert_eq!(
            read(&mut host, ROOT_A, b"/balance"),
            Some(b"initial".to_vec())
        );
    }

    #[test]
    fn resume_then_commit_keeps_accumulated_work() {
        let mut host = MockKernelHost::default();
        write(&mut host, ROOT_A, b"/balance", b"initial");

        {
            let snapshot = Snapshot::start(&mut host, &root_a()).unwrap();
            write(&mut host, ROOT_A, b"/balance", b"reboot1");
            drop(snapshot);
        }
        {
            let snapshot = Snapshot::resume(&mut host, &root_a()).unwrap();
            write(&mut host, ROOT_A, b"/balance", b"reboot2");
            snapshot.commit();
        }

        assert_eq!(
            read(&mut host, ROOT_A, b"/balance"),
            Some(b"reboot2".to_vec())
        );
    }

    #[test]
    fn start_overwrites_stale_backup() {
        let mut host = MockKernelHost::default();
        // Pre-seed a stale backup from a hypothetical crashed run. The backup
        // keyspace is observed indirectly via revert: we can't `read` it
        // directly here because the live `Snapshot` owns that handle.
        write(&mut host, BLOCK_A, b"/stale", b"garbage");
        write(&mut host, ROOT_A, b"/balance", b"initial");

        // `start` must *replace* the stale backup with a clean copy of the live
        // root, not merge into it.
        let snapshot = Snapshot::start(&mut host, &root_a()).unwrap();

        write(&mut host, ROOT_A, b"/balance", b"updated");
        snapshot.revert(&mut host).unwrap();

        // Revert restores the live root from the backup. If `start` had merged
        // (kept `/stale`) instead of overwriting, the stale key would leak into
        // the live root; it must not, and the pre-block balance is restored.
        assert_eq!(
            read(&mut host, ROOT_A, b"/balance"),
            Some(b"initial".to_vec())
        );
        assert_eq!(read(&mut host, ROOT_A, b"/stale"), None);
    }

    // ----- Resume marker (guards against silent live-root wipe) -----

    #[test]
    fn resume_without_marker_refuses() {
        let mut host = MockKernelHost::default();
        write(&mut host, ROOT_A, b"/balance", b"live-data");

        // No prior `start`, so no in-progress marker: resume must refuse rather
        // than re-attach to absent backups.
        let err = Snapshot::resume(&mut host, &root_a());
        assert!(matches!(err, Err(SnapshotError::NothingToResume)));

        // The live root must be untouched (no accidental wipe).
        assert_eq!(
            read(&mut host, ROOT_A, b"/balance"),
            Some(b"live-data".to_vec())
        );
    }

    #[test]
    fn commit_clears_marker_so_later_resume_refuses() {
        let mut host = MockKernelHost::default();
        write(&mut host, ROOT_A, b"/balance", b"initial");

        let snapshot = Snapshot::start(&mut host, &root_a()).unwrap();
        write(&mut host, ROOT_A, b"/balance", b"committed");
        snapshot.commit();

        // After commit the backups are discarded; a stale resume (e.g. a BIP
        // left behind) must refuse rather than restore from the empty backups.
        let err = Snapshot::resume(&mut host, &root_a());
        assert!(matches!(err, Err(SnapshotError::NothingToResume)));

        // Committed state is intact.
        assert_eq!(
            read(&mut host, ROOT_A, b"/balance"),
            Some(b"committed".to_vec())
        );
    }

    #[test]
    fn revert_clears_marker_so_later_resume_refuses() {
        let mut host = MockKernelHost::default();
        write(&mut host, ROOT_A, b"/balance", b"initial");

        let snapshot = Snapshot::start(&mut host, &root_a()).unwrap();
        write(&mut host, ROOT_A, b"/balance", b"aborted");
        snapshot.revert(&mut host).unwrap();

        let err = Snapshot::resume(&mut host, &root_a());
        assert!(matches!(err, Err(SnapshotError::NothingToResume)));
    }

    #[test]
    fn resume_is_independent_per_root() {
        let mut host = MockKernelHost::default();
        write(&mut host, ROOT_A, b"/balance", b"initial");

        // Start over ROOT_A only, accumulate partial work, then drop (reboot).
        {
            let snapshot = Snapshot::start(&mut host, &root_a()).unwrap();
            write(&mut host, ROOT_A, b"/balance", b"partial");
            drop(snapshot);
        }

        // ROOT_B was never started: its marker is absent, so a resume over it
        // refuses rather than attaching to a non-existent backup.
        assert!(matches!(
            Snapshot::resume(&mut host, &root_b()),
            Err(SnapshotError::NothingToResume)
        ));

        // ROOT_A resumes and reverts on its own.
        let snapshot = Snapshot::resume(&mut host, &root_a()).unwrap();
        snapshot.revert(&mut host).unwrap();
        assert_eq!(
            read(&mut host, ROOT_A, b"/balance"),
            Some(b"initial".to_vec())
        );
    }

    // ----- Transient-handle precondition (descriptive error + helper) -----

    #[test]
    fn start_reports_root_held_across_boundary() {
        let mut host = MockKernelHost::default();

        // A consumer holds a live handle to a safe root across `start`.
        let _held = host
            .load_or_create("/evm/world_state".parse().unwrap())
            .unwrap();

        match Snapshot::start(&mut host, &root_a()) {
            Err(SnapshotError::RootHeldAcrossBoundary(name)) => {
                assert_eq!(name.as_ref(), "/evm/world_state");
            }
            Err(other) => panic!("expected RootHeldAcrossBoundary, got {other:?}"),
            Ok(_) => panic!("expected RootHeldAcrossBoundary, got Ok"),
        }
    }

    // ----- Savepoints (nested per-operation transactions) -----

    /// The first savepoint (depth 1, above the block at depth 0) backup of ROOT_B.
    const SP1_B: &str = "/__snapshot/frame/1/tez/tez_accounts";

    #[test]
    fn release_keeps_savepoint_writes() {
        let mut host = MockKernelHost::default();
        write(&mut host, ROOT_B, b"/balance", b"initial");

        let mut snapshot = Snapshot::start(&mut host, &root_b()).unwrap();
        snapshot.savepoint(&mut host).unwrap();
        write(&mut host, ROOT_B, b"/balance", b"updated");
        snapshot.release();

        assert_eq!(
            read(&mut host, ROOT_B, b"/balance"),
            Some(b"updated".to_vec())
        );
        // Savepoint backup discarded.
        assert_eq!(read(&mut host, SP1_B, b"/balance"), None);
        snapshot.commit();
    }

    #[test]
    fn rollback_restores_pre_savepoint_state() {
        let mut host = MockKernelHost::default();
        write(&mut host, ROOT_B, b"/balance", b"initial");

        let mut snapshot = Snapshot::start(&mut host, &root_b()).unwrap();
        snapshot.savepoint(&mut host).unwrap();
        write(&mut host, ROOT_B, b"/balance", b"updated");
        write(&mut host, ROOT_B, b"/new", b"value");
        snapshot.rollback(&mut host).unwrap();

        assert_eq!(
            read(&mut host, ROOT_B, b"/balance"),
            Some(b"initial".to_vec())
        );
        assert_eq!(read(&mut host, ROOT_B, b"/new"), None);
        assert_eq!(read(&mut host, SP1_B, b"/balance"), None);
        snapshot.commit();
    }

    #[test]
    fn rollback_clears_root_empty_at_savepoint() {
        let mut host = MockKernelHost::default();
        // ROOT_B is empty when the savepoint is taken.
        let mut snapshot = Snapshot::start(&mut host, &root_b()).unwrap();
        snapshot.savepoint(&mut host).unwrap();
        write(&mut host, ROOT_B, b"/created", b"during-op");
        snapshot.rollback(&mut host).unwrap();

        assert_eq!(read(&mut host, ROOT_B, b"/created"), None);
        snapshot.commit();
    }

    #[test]
    fn two_phase_keeps_validation_drops_application() {
        // Mirrors validate_and_apply_operation: phase 1 (validation) is
        // released, phase 2 (application) is rolled back; the validation-phase
        // write survives.
        let mut host = MockKernelHost::default();
        write(&mut host, ROOT_B, b"/balance", b"initial");

        let mut snapshot = Snapshot::start(&mut host, &root_b()).unwrap();

        // Phase 1: validation succeeds and is released (kept).
        snapshot.savepoint(&mut host).unwrap();
        write(&mut host, ROOT_B, b"/balance", b"fee-debited");
        snapshot.release();

        // Phase 2: application fails and is rolled back.
        snapshot.savepoint(&mut host).unwrap();
        write(&mut host, ROOT_B, b"/balance", b"applied");
        write(&mut host, ROOT_B, b"/storage", b"junk");
        snapshot.rollback(&mut host).unwrap();

        // The fee debit from validation survives; the application is gone.
        assert_eq!(
            read(&mut host, ROOT_B, b"/balance"),
            Some(b"fee-debited".to_vec())
        );
        assert_eq!(read(&mut host, ROOT_B, b"/storage"), None);
        snapshot.commit();
    }

    #[test]
    fn nested_savepoints_roll_back_independently() {
        let mut host = MockKernelHost::default();
        write(&mut host, ROOT_B, b"/v", b"0");

        let mut snapshot = Snapshot::start(&mut host, &root_b()).unwrap();

        snapshot.savepoint(&mut host).unwrap(); // depth 1
        write(&mut host, ROOT_B, b"/v", b"1");

        snapshot.savepoint(&mut host).unwrap(); // depth 2
        write(&mut host, ROOT_B, b"/v", b"2");

        // Roll back the inner savepoint: back to "1".
        snapshot.rollback(&mut host).unwrap();
        assert_eq!(read(&mut host, ROOT_B, b"/v"), Some(b"1".to_vec()));

        // Roll back the outer savepoint: back to "0".
        snapshot.rollback(&mut host).unwrap();
        assert_eq!(read(&mut host, ROOT_B, b"/v"), Some(b"0".to_vec()));

        snapshot.commit();
    }

    #[test]
    fn savepoint_disjoint_from_block_backup() {
        // A nested savepoint rollback restores mid-block state, while the block
        // backup still allows a full revert to pre-block.
        let mut host = MockKernelHost::default();
        write(&mut host, ROOT_B, b"/account", b"pre-block");

        let mut snapshot = Snapshot::start(&mut host, &root_b()).unwrap();
        write(&mut host, ROOT_B, b"/account", b"mid-block");

        // Nested savepoint + rollback: restores mid-block, not pre-block.
        snapshot.savepoint(&mut host).unwrap();
        write(&mut host, ROOT_B, b"/account", b"in-op");
        snapshot.rollback(&mut host).unwrap();
        assert_eq!(
            read(&mut host, ROOT_B, b"/account"),
            Some(b"mid-block".to_vec())
        );

        // The block backup is still intact: a block revert restores pre-block.
        snapshot.revert(&mut host).unwrap();
        assert_eq!(
            read(&mut host, ROOT_B, b"/account"),
            Some(b"pre-block".to_vec())
        );
    }

    #[test]
    fn release_with_no_savepoint_is_noop() {
        // With only the block frame open, release/rollback must not pop it.
        let mut host = MockKernelHost::default();
        write(&mut host, ROOT_A, b"/balance", b"initial");

        let mut snapshot = Snapshot::start(&mut host, &root_a()).unwrap();
        snapshot.release();
        snapshot.rollback(&mut host).unwrap();

        // The block frame is intact: a revert still restores pre-block state.
        write(&mut host, ROOT_A, b"/balance", b"updated");
        snapshot.revert(&mut host).unwrap();
        assert_eq!(
            read(&mut host, ROOT_A, b"/balance"),
            Some(b"initial".to_vec())
        );
    }
}
