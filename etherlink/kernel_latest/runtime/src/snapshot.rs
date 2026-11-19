// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Stack of savepoints over a single keyspace `root`.
//!
//! Terms used throughout:
//! - `root`: the name of the keyspace being protected, e.g.
//!   `/tez/tez_accounts`.
//! - live: the handle on `root` where reads and writes happen. The snapshot
//!   borrows it for each call.
//! - backup: a copy of the live `root` taken at some point, stored in its
//!   own keyspace `/__savepoint/<index><root>`.
//! - savepoint: taking such a backup. Backups form a stack, oldest at index
//!   0. The height is the number of backups held, and the top is the most
//!   recent one, at index height - 1.
//!
//! `savepoint` copies the live `root` into a new backup on top of the
//! stack; code then mutates the live `root` in place. `release_to(height)`
//! drops the backups above the `height` oldest, keeping the live writes;
//! `rollback` moves the top backup back over the live `root` and pops it;
//! `restore_top` copies it back and keeps it for a later restore. `commit`
//! drops every backup, keeping the live writes.
//!
//! The snapshot never loads the live `root`: its owner keeps the handle and
//! lends it to the operations that read or restore it. Only the backups are
//! owned by the [`Snapshot`]. The operations are crate-private:
//! [`crate::runtime_keyspaces::RuntimeKeyspaces`] is the owner, and lends
//! its own handle and the loader it came from.
//!
//! Backups live in durable storage, but the stack bookkeeping (which backups
//! are held) is kept in memory only, so a snapshot cannot outlive a reboot:
//! take its savepoints and commit them within one kernel run, e.g.
//! around one transaction.
//!
//! There is no reboot recovery, by design: no caller keeps a snapshot open
//! across a reboot, so nothing records the open savepoints nor restores
//! them on the next run. Backups a reboot leaves behind are not cleaned up
//! either; the next `savepoint` at the same index overwrites them.

use tezos_smart_rollup_keyspace::{KeySpace, KeySpaceLoader, KeySpaceLoaderError, Name};

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum SnapshotError {
    /// Underlying loader failure, including a backup keyspace name that is
    /// not a valid `Name` (e.g. one that exceeds the name size limit).
    #[error("snapshot keyspace loader error: {0}")]
    Loader(#[from] KeySpaceLoaderError),
    /// The savepoint asked for is not on the stack, e.g. a rollback or a
    /// restore on an empty one.
    #[error("no such snapshot savepoint")]
    NoSavepoint,
    /// A release asked to keep more backups than the stack holds.
    #[error("cannot release snapshot to height {height}: {len} backups held")]
    ReleaseAboveHeight { height: usize, len: usize },
    /// The snapshot holds more savepoints than its caller counts across
    /// the snapshots it drives: the count and the snapshot are out of sync.
    #[error("{held} snapshot savepoints held, only {tracked} tracked")]
    UntrackedSavepoints { held: usize, tracked: usize },
}

const SAVEPOINT_PREFIX: &str = "/__savepoint";

/// Backup keyspace name for `root` at stack `index`.
///
/// Fails with [`KeySpaceLoaderError::InvalidName`], as a loader does when the
/// name it builds is invalid, so that both reach the caller in one shape.
fn frame_name(root: &Name, index: usize) -> Result<Name, KeySpaceLoaderError> {
    Ok(format!("{SAVEPOINT_PREFIX}/{index}{root}").parse()?)
}

/// A stack of backups of a single `root`.
///
/// At most one snapshot of a given `root` may be alive at a time: backups are
/// named after `root` and their index in the stack only, so two snapshots of
/// the same `root` would claim the same backup names. A loader that tracks
/// the keyspaces it lent, as the Irmin durable storage one does, fails the
/// second one's `savepoint` with [`KeySpaceLoaderError::AlreadyLoaded`]. The
/// NDS loader tracks nothing: both snapshots then share the same backups,
/// and each one overwrites the other's. Nested snapshots must be of distinct
/// roots, e.g. a root and its mirror.
///
/// Commit the snapshot before reverting or promoting the mirror its backups
/// were loaded through: an empty backup cannot be told apart from a missing
/// one in storage (both are an absent path), so restoring a backup dropped
/// by [`crate::safe_storage::SafeStorage::revert`] clears the live `root`.
pub struct Snapshot<KS: KeySpace> {
    /// Backup stack, oldest first: `backups[i]` is named after index `i`.
    backups: Vec<KS>,
}

impl<KS: KeySpace> Snapshot<KS> {
    /// A snapshot holding no backup.
    pub(crate) fn new() -> Self {
        Self {
            backups: Vec::new(),
        }
    }

    /// Number of backups held.
    pub fn len(&self) -> usize {
        self.backups.len()
    }

    /// Whether no backup is held.
    pub fn is_empty(&self) -> bool {
        self.backups.is_empty()
    }

    /// Copy the borrowed `live` root into a new backup on top of the stack.
    /// Overwrites any stale backup of the same name.
    ///
    /// `loader` must be the loader `live` was loaded from. The backup is
    /// named after the full name of `live`, mirror prefixes included, and
    /// loaded through `loader`: a mirroring loader keeps it inside its
    /// mirror, while another loader would put it elsewhere, e.g. outside the
    /// `/tmp` that [`crate::safe_storage::SafeStorage::revert`] drops.
    /// Nothing checks it: both loaders yield the same keyspace type, hence
    /// the operations are crate-private.
    ///
    /// Fails `Loader(AlreadyLoaded)` if another snapshot of `root` holds
    /// the backup keyspace and `loader` tracks the keyspaces it lent (the
    /// NDS loader does not), and `Loader(InvalidName)` if the backup name,
    /// or the name `loader` derives from it, exceeds the name size limit.
    pub(crate) fn savepoint(
        &mut self,
        loader: &mut impl KeySpaceLoader<KeySpace = KS>,
        live: &KS,
    ) -> Result<(), SnapshotError> {
        // TODO: SDK-148
        // Under NDS, each backup name is mapped to its own database for
        // good: clearing a backup keeps its database allocated, so the
        // databases of the highest stack ever reached are never reclaimed.
        let mut backup =
            loader.load_or_create(frame_name(live.name(), self.backups.len())?)?;
        backup.copy_from(live);
        self.backups.push(backup);
        Ok(())
    }

    /// Drop the backups above the `height` oldest, keeping the live writes.
    /// No-op if exactly `height` backups are held.
    ///
    /// Fails `ReleaseAboveHeight` if fewer than `height` backups are held:
    /// the caller's savepoints and releases are unbalanced.
    pub(crate) fn release_to(&mut self, height: usize) -> Result<(), SnapshotError> {
        let len = self.backups.len();
        if height > len {
            return Err(SnapshotError::ReleaseAboveHeight { height, len });
        }
        self.drop_above(height);
        Ok(())
    }

    /// Clear and drop the backups above the `height` oldest.
    /// Requires `height <= self.backups.len()`.
    fn drop_above(&mut self, height: usize) {
        // Keep `backups[..height]`, the oldest ones; take the rest.
        let dropped = self.backups.split_off(height);
        // Wipe each dropped backup from durable storage. The live `root` is
        // never touched, so its writes are kept.
        for mut backup in dropped {
            backup.clear();
        }
    }

    /// Move the top backup back over the borrowed `live` root and pop it.
    /// Fails `NoSavepoint` on an empty stack.
    pub(crate) fn rollback(&mut self, live: &mut KS) -> Result<(), SnapshotError> {
        let mut backup = self.backups.pop().ok_or(SnapshotError::NoSavepoint)?;
        live.move_from(&mut backup);
        Ok(())
    }

    /// Copy the top backup back over the borrowed `live` root, keeping it on
    /// the stack. Fails `NoSavepoint` on an empty stack.
    pub(crate) fn restore_top(&self, live: &mut KS) -> Result<(), SnapshotError> {
        let backup = self.backups.last().ok_or(SnapshotError::NoSavepoint)?;
        live.copy_from(backup);
        Ok(())
    }

    /// Drop every backup still held. The live `root` keeps every write.
    ///
    /// Nothing does it implicitly: a snapshot dropped without `commit` (e.g.
    /// when a caller leaves early on an error) leaves its backups in storage.
    /// They are harmless, as the next `savepoint` at the same index
    /// overwrites them, but they occupy durable storage until then.
    pub(crate) fn commit(&mut self) {
        self.drop_above(0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::MockKernelHost;
    use crate::safe_storage::SafeStorage;
    use tezos_smart_rollup_keyspace::Key;

    type MockKeySpace = <MockKernelHost as KeySpaceLoader>::KeySpace;

    const ROOT_A: &str = "/tez/tez_accounts";

    fn key(k: &[u8]) -> Key {
        Key::from_bytes(k).unwrap()
    }

    /// Load the live keyspace `name`. The test holds this handle throughout
    /// and lends it to the snapshot, as `RuntimeKeyspaces` does.
    fn live(host: &mut MockKernelHost, name: &str) -> MockKeySpace {
        host.load_or_create(name.parse().unwrap()).unwrap()
    }

    fn set(ks: &mut MockKeySpace, k: &[u8], value: &[u8]) {
        ks.set(&key(k), value).unwrap();
    }

    fn get(ks: &MockKeySpace, k: &[u8]) -> Option<Vec<u8>> {
        ks.get(&key(k))
    }

    /// Read `k` inside the keyspace `name` (e.g. a backup the snapshot no
    /// longer holds). The handle is dropped within the call.
    fn read(host: &mut MockKernelHost, name: &str, k: &[u8]) -> Option<Vec<u8>> {
        let ks = host.load_or_create(name.parse().unwrap()).unwrap();
        ks.get(&key(k))
    }

    const BACKUP_0: &str = "/__savepoint/0/tez/tez_accounts";
    const BACKUP_1: &str = "/__savepoint/1/tez/tez_accounts";

    #[test]
    fn frame_name_keys_by_index() {
        let root: Name = ROOT_A.parse().unwrap();
        assert_eq!(frame_name(&root, 0).unwrap().as_ref(), BACKUP_0);
        assert_eq!(frame_name(&root, 1).unwrap().as_ref(), BACKUP_1);
    }

    #[test]
    fn frame_name_fits_under_nested_mirrors() {
        // The batch snapshot's root is the accounts of a mirror of the block
        // mirror, and its backups are loaded through that same mirror.
        let mut host = MockKernelHost::default();
        let mut block = SafeStorage::new(&mut host, vec![]);
        let mut batch = SafeStorage::new(&mut block, vec![]);
        let root = batch.load_or_create(ROOT_A.parse().unwrap()).unwrap();
        assert_eq!(root.name().as_ref(), "/tmp/tmp/tez/tez_accounts");
        let mut snapshot = Snapshot::new();
        // Far beyond any realistic nesting of cross-runtime calls.
        for _ in 0..1000 {
            snapshot.savepoint(&mut batch, &root).unwrap();
        }
        assert_eq!(snapshot.len(), 1000);
        snapshot.commit();
    }

    #[test]
    fn savepoint_copies_live_root() {
        let mut host = MockKernelHost::default();
        let mut root = live(&mut host, ROOT_A);
        set(&mut root, b"/balance", b"initial");

        let mut snapshot = Snapshot::new();
        snapshot.savepoint(&mut host, &root).unwrap();
        set(&mut root, b"/balance", b"updated");

        assert_eq!(snapshot.len(), 1);
        assert_eq!(get(&root, b"/balance"), Some(b"updated".to_vec()));
        assert_eq!(
            get(&snapshot.backups[0], b"/balance"),
            Some(b"initial".to_vec())
        );
    }

    #[test]
    fn release_to_keeps_live_writes_and_drops_backups() {
        let mut host = MockKernelHost::default();
        let mut root = live(&mut host, ROOT_A);
        set(&mut root, b"/balance", b"v0");

        let mut snapshot = Snapshot::new();
        snapshot.savepoint(&mut host, &root).unwrap();
        set(&mut root, b"/balance", b"v1");
        snapshot.savepoint(&mut host, &root).unwrap();
        set(&mut root, b"/balance", b"v2");

        snapshot.release_to(1).unwrap();
        assert_eq!(snapshot.len(), 1);
        assert_eq!(get(&root, b"/balance"), Some(b"v2".to_vec()));
        assert_eq!(get(&snapshot.backups[0], b"/balance"), Some(b"v0".to_vec()));
        drop(snapshot);
        assert_eq!(read(&mut host, BACKUP_1, b"/balance"), None);
    }

    #[test]
    fn release_to_above_len_fails() {
        let mut host = MockKernelHost::default();
        let root = live(&mut host, ROOT_A);
        let mut snapshot = Snapshot::new();
        assert_eq!(
            snapshot.release_to(3),
            Err(SnapshotError::ReleaseAboveHeight { height: 3, len: 0 })
        );
        assert!(snapshot.is_empty());
        snapshot.savepoint(&mut host, &root).unwrap();
        snapshot.release_to(1).unwrap();
        assert_eq!(
            snapshot.release_to(2),
            Err(SnapshotError::ReleaseAboveHeight { height: 2, len: 1 })
        );
        assert_eq!(snapshot.len(), 1);
    }

    #[test]
    fn rollback_restores_top_and_pops_it() {
        let mut host = MockKernelHost::default();
        let mut root = live(&mut host, ROOT_A);
        set(&mut root, b"/balance", b"v0");

        let mut snapshot = Snapshot::new();
        snapshot.savepoint(&mut host, &root).unwrap();
        set(&mut root, b"/balance", b"v1");
        snapshot.savepoint(&mut host, &root).unwrap();
        set(&mut root, b"/balance", b"v2");
        set(&mut root, b"/new", b"value");

        snapshot.rollback(&mut root).unwrap();
        assert_eq!(snapshot.len(), 1);
        assert_eq!(get(&root, b"/balance"), Some(b"v1".to_vec()));
        assert_eq!(get(&root, b"/new"), None);

        snapshot.rollback(&mut root).unwrap();
        assert!(snapshot.is_empty());
        assert_eq!(get(&root, b"/balance"), Some(b"v0".to_vec()));
        assert_eq!(read(&mut host, BACKUP_0, b"/balance"), None);
        assert_eq!(read(&mut host, BACKUP_1, b"/balance"), None);
    }

    #[test]
    fn rollback_clears_root_empty_at_savepoint() {
        let mut host = MockKernelHost::default();
        let mut root = live(&mut host, ROOT_A);

        let mut snapshot = Snapshot::new();
        snapshot.savepoint(&mut host, &root).unwrap();
        set(&mut root, b"/balance", b"value");
        snapshot.rollback(&mut root).unwrap();

        assert_eq!(get(&root, b"/balance"), None);
    }

    #[test]
    fn rollback_and_restore_on_empty_stack_fail() {
        let mut host = MockKernelHost::default();
        let mut root = live(&mut host, ROOT_A);
        set(&mut root, b"/balance", b"value");

        let mut snapshot = Snapshot::new();
        assert_eq!(
            snapshot.rollback(&mut root),
            Err(SnapshotError::NoSavepoint)
        );
        assert_eq!(
            snapshot.restore_top(&mut root),
            Err(SnapshotError::NoSavepoint)
        );
        assert_eq!(get(&root, b"/balance"), Some(b"value".to_vec()));
    }

    #[test]
    fn restore_top_keeps_backup_for_a_later_restore() {
        let mut host = MockKernelHost::default();
        let mut root = live(&mut host, ROOT_A);
        set(&mut root, b"/balance", b"v0");

        let mut snapshot = Snapshot::new();
        snapshot.savepoint(&mut host, &root).unwrap();
        set(&mut root, b"/balance", b"v1");
        snapshot.restore_top(&mut root).unwrap();
        assert_eq!(snapshot.len(), 1);
        assert_eq!(get(&root, b"/balance"), Some(b"v0".to_vec()));

        set(&mut root, b"/balance", b"v2");
        snapshot.rollback(&mut root).unwrap();
        assert_eq!(get(&root, b"/balance"), Some(b"v0".to_vec()));
    }

    #[test]
    fn restore_top_restores_the_top_backup_only() {
        let mut host = MockKernelHost::default();
        let mut root = live(&mut host, ROOT_A);
        set(&mut root, b"/balance", b"v0");

        let mut snapshot = Snapshot::new();
        snapshot.savepoint(&mut host, &root).unwrap();
        set(&mut root, b"/balance", b"v1");
        snapshot.savepoint(&mut host, &root).unwrap();
        set(&mut root, b"/balance", b"v2");

        snapshot.restore_top(&mut root).unwrap();
        assert_eq!(snapshot.len(), 2);
        assert_eq!(get(&root, b"/balance"), Some(b"v1".to_vec()));
        assert_eq!(get(&snapshot.backups[0], b"/balance"), Some(b"v0".to_vec()));
        assert_eq!(get(&snapshot.backups[1], b"/balance"), Some(b"v1".to_vec()));

        // Both backups are still held: rolling back twice walks down the
        // stack to the oldest one.
        set(&mut root, b"/balance", b"v3");
        snapshot.rollback(&mut root).unwrap();
        assert_eq!(get(&root, b"/balance"), Some(b"v1".to_vec()));
        snapshot.rollback(&mut root).unwrap();
        assert_eq!(get(&root, b"/balance"), Some(b"v0".to_vec()));
        assert!(snapshot.is_empty());
    }

    #[test]
    fn savepoint_overwrites_stale_backup() {
        let mut host = MockKernelHost::default();
        let mut root = live(&mut host, ROOT_A);
        set(&mut root, b"/balance", b"v0");
        set(&mut root, b"/stale", b"value");

        // Dropped without `commit`: its backup stays in storage.
        let mut stale = Snapshot::new();
        stale.savepoint(&mut host, &root).unwrap();
        drop(stale);
        assert_eq!(
            read(&mut host, BACKUP_0, b"/stale"),
            Some(b"value".to_vec())
        );

        assert!(root.delete(&key(b"/stale")));
        set(&mut root, b"/balance", b"v1");
        let mut snapshot = Snapshot::new();
        snapshot.savepoint(&mut host, &root).unwrap();
        set(&mut root, b"/balance", b"v2");
        snapshot.rollback(&mut root).unwrap();

        assert_eq!(get(&root, b"/balance"), Some(b"v1".to_vec()));
        assert_eq!(get(&root, b"/stale"), None);
    }

    #[test]
    fn savepoint_of_empty_root_clears_stale_backup() {
        let mut host = MockKernelHost::default();
        let mut root = live(&mut host, ROOT_A);
        set(&mut root, b"/balance", b"v0");

        // Dropped without `commit`: its backup stays in storage.
        let mut stale = Snapshot::new();
        stale.savepoint(&mut host, &root).unwrap();
        drop(stale);

        root.clear();
        let mut snapshot = Snapshot::new();
        snapshot.savepoint(&mut host, &root).unwrap();
        set(&mut root, b"/balance", b"v1");
        snapshot.rollback(&mut root).unwrap();

        assert_eq!(get(&root, b"/balance"), None);
    }

    #[test]
    fn commit_keeps_live_writes_and_drops_backups() {
        let mut host = MockKernelHost::default();
        let mut root = live(&mut host, ROOT_A);
        set(&mut root, b"/balance", b"v0");

        let mut snapshot = Snapshot::new();
        snapshot.savepoint(&mut host, &root).unwrap();
        snapshot.savepoint(&mut host, &root).unwrap();
        set(&mut root, b"/balance", b"v1");
        snapshot.commit();

        assert_eq!(get(&root, b"/balance"), Some(b"v1".to_vec()));
        assert_eq!(read(&mut host, BACKUP_0, b"/balance"), None);
        assert_eq!(read(&mut host, BACKUP_1, b"/balance"), None);
    }

    #[test]
    fn second_snapshot_of_a_root_is_rejected() {
        let mut host = MockKernelHost::default();
        let root = live(&mut host, ROOT_A);

        let mut first = Snapshot::new();
        first.savepoint(&mut host, &root).unwrap();
        let mut second = Snapshot::new();
        assert_eq!(
            second.savepoint(&mut host, &root),
            Err(SnapshotError::Loader(KeySpaceLoaderError::AlreadyLoaded))
        );
        assert!(second.is_empty());
        assert_eq!(first.len(), 1);
    }
}
