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
