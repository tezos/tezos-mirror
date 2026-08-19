// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Storage-backed, revertible transactions over a keyspace.
//!
//! [`SnapshottedKeySpace`] reads and writes the live keyspace, and keeps one
//! backup per open frame. [`SafeKeyspace::commit_inner`] keeps the writes of
//! the innermost frame, [`SafeKeyspace::revert_inner`] undoes them.
//!
//! A reboot loses the stack, but not the backups: they are in storage.
//! [`SnapshottedKeySpace::create_reboot_marker`] writes down how many frames
//! were open, and [`SnapshottedKeySpace::start`] reads that back. Without a
//! marker, `start` begins a fresh stack.
//!
//! A run that cannot close its frames has [`SafeKeyspace::revert_all`], which
//! puts the live state back to the bedrock.
//!
//! A run the PVM cuts short leaves no marker and runs no [`Drop`], so its
//! bedrock survives. [`PreviousRun`] tells `start`, which puts the live state
//! back to it instead of adopting it.
//!
//! Every backup is named after the keyspace it copies, under the `/__snapshot`
//! prefix it keeps for itself.

use thiserror::Error;

use tezos_smart_rollup_host::runtime::RuntimeError;
use tezos_smart_rollup_keyspace::{
    Key, KeySpace, KeySpaceLoader, KeySpaceLoaderError, KeySpaceWriteError, Name,
    NameError,
};

#[derive(Debug, PartialEq, Eq, Error)]
pub enum SnapshotError {
    #[error(transparent)]
    Loader(#[from] KeySpaceLoaderError),
    #[error(transparent)]
    MarkerWrite(#[from] KeySpaceWriteError),
    /// A `commit_inner` or `revert_inner` with no frame open.
    #[error("no open frame to close")]
    NoOpenFrame,
    /// A `commit_all` with a frame still open: the live state it would promote
    /// is covered by a frame no close has accepted yet.
    #[error("a frame is still open")]
    FrameStillOpen,
    /// More frames open at once than a frame number counts.
    #[error("too many open frames")]
    TooManyFrames,
    #[error(transparent)]
    InvalidName(#[from] NameError),
    /// Reading whether the previous run was cut short failed.
    #[error("cannot read whether the previous run was cut short: {0:?}")]
    PreviousRun(RuntimeError),
}

/// What the run before this one left behind.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PreviousRun {
    /// It reached its end, or there was no previous run.
    Complete,
    /// The PVM cut it short, so its writes sit in the live keyspace with no
    /// close behind them. Read from `WasmHost::last_run_aborted`.
    Aborted,
}

const FRAME_PREFIX: &str = "/__snapshot/frame";
const BEDROCK_PREFIX: &str = "/__snapshot/bedrock";
const META_PREFIX: &str = "/__snapshot/meta";

/// The one key a marked run leaves behind, holding how many frames it left
/// open, little-endian. Absent, no run marked itself; `0` is the bedrock alone.
const FRAME_MARKER: Key = Key::from_static(b"/reboot/frames");

/// The number a frame is named by. [`FRAME_NUMBER_HEX_DIGITS`] follows it.
type FrameNumber = u32;

/// Width a frame number is spelled on in a name: a [`FrameNumber`] in
/// hexadecimal.
const FRAME_NUMBER_HEX_DIGITS: usize = FRAME_NUMBER_BYTES * 2;

/// Width a frame number takes in storage: a [`FrameNumber`] little-endian.
const FRAME_NUMBER_BYTES: usize = std::mem::size_of::<FrameNumber>();

/// Returns the [`FrameNumber`] of a frame at the given `depth`.
///
/// Fails with [`SnapshotError::TooManyFrames`] if `depth` is larger than the maximum supported frame number.
fn frame_number(depth: usize) -> Result<FrameNumber, SnapshotError> {
    FrameNumber::try_from(depth).map_err(|_| SnapshotError::TooManyFrames)
}

/// Name of `root`'s backup at the frame numbered `depth`.
///
/// The `depth` is always formatted to be [`FRAME_NUMBER_HEX_DIGITS`] long. Frame names for
/// a given `root` thus always have the same length.
fn frame_name(root: &Name, depth: FrameNumber) -> Result<Name, NameError> {
    Name::try_from(format!(
        "{FRAME_PREFIX}/{depth:0FRAME_NUMBER_HEX_DIGITS$x}{root}"
    ))
}

/// Name of `root`'s bedrock, the copy sitting below the frames. Its own prefix,
/// so no frame depth can ever name it.
fn bedrock_name(root: &Name) -> Result<Name, NameError> {
    Name::try_from(format!("{BEDROCK_PREFIX}{root}"))
}

/// Name of `root`'s meta keyspace, holding its reboot marker. Its own prefix,
/// so no frame depth can ever name it.
fn meta_name(root: &Name) -> Result<Name, NameError> {
    Name::try_from(format!("{META_PREFIX}{root}"))
}

/// A keyspace with a stack of revertible frames over it.
///
/// Implements [`KeySpace`] by reading and writing the live keyspace.
///
/// `KS: KeySpace` is bound on the struct because [`Drop`] needs it, and a `Drop`
/// impl must repeat the bounds of its struct.
pub struct SnapshottedKeySpace<KS: KeySpace> {
    /// The live keyspace. Every [`KeySpace`] call goes to it.
    live: KS,
    /// A copy of the live state taken at [`Self::start`]. Not a frame: it is not
    /// counted by [`SafeKeyspace::depth`] and no close consumes it.
    bedrock: KS,
    /// One backup per open frame, outermost first. `frames[d]` is the live state
    /// as it was when depth `d` opened.
    frames: Vec<KS>,
    /// The keyspace holding this root's marker. Kept for the whole run because
    /// [`Drop`] has to read it and has no loader.
    meta: KS,
}

impl<KS: KeySpace> SnapshottedKeySpace<KS> {
    /// Start over `live`.
    ///
    /// If a previous run left a marker, its backups are taken back and the
    /// caller resumes at the depth it was cut at. Otherwise the stack starts
    /// empty.
    ///
    /// The marker is cleared here, so a run that wants to survive the next
    /// reboot has to mark itself again.
    ///
    /// A marker wins over `previous`: its backups are taken back either way.
    /// With no marker, `previous` decides which way the copy goes.
    pub fn start(
        loader: &mut impl KeySpaceLoader<KeySpace = KS>,
        live: KS,
        previous: PreviousRun,
    ) -> Result<Self, SnapshotError> {
        // Frame names are the longest generated names for this root. Since the
        // fixed-width depth makes every frame name equally long, validating depth 0
        // ensures that frame names at every valid depth can be constructed.
        frame_name(live.name(), 0)?;
        let meta = loader.load_or_create(meta_name(live.name())?)?;
        let bedrock = loader.load_or_create(bedrock_name(live.name())?)?;
        let mut this = Self {
            live,
            bedrock,
            frames: Vec::new(),
            meta,
        };
        match this.reboot_frames() {
            Some(open) => {
                // Take the backups as they stand, never copied over: that content
                // is what the previous run has to close against.
                for depth in 0..open {
                    this.frames
                        .push(loader.load_or_create(this.backup_name(depth)?)?);
                }
                this.meta.clear();
            }
            None => match previous {
                // Nothing to take back: the live state becomes the bedrock.
                PreviousRun::Complete => this.bedrock.copy_from(&this.live),
                // Nothing to keep either: the bedrock comes back over it.
                PreviousRun::Aborted => this.live.copy_from(&this.bedrock),
            },
        }
        Ok(this)
    }

    /// How many frames the run that marked itself for a reboot left open,
    /// `None` if no run did.
    fn reboot_frames(&self) -> Option<usize> {
        self.meta
            .get_prefix_exact::<FRAME_NUMBER_BYTES>(&FRAME_MARKER)
            .map(|open| FrameNumber::from_le_bytes(open) as usize)
    }

    /// Name of this run's backup at `depth`.
    fn backup_name(&self, depth: usize) -> Result<Name, SnapshotError> {
        Ok(frame_name(self.live.name(), frame_number(depth)?)?)
    }

    /// The bedrock, for tests. They cannot load it themselves: the run holds the
    /// only handle on that name.
    #[cfg(test)]
    fn bedrock(&self) -> &KS {
        &self.bedrock
    }

    /// Back up the live state at the next depth, replacing anything already
    /// stored there.
    fn open_frame(
        &mut self,
        loader: &mut impl KeySpaceLoader<KeySpace = KS>,
    ) -> Result<(), SnapshotError> {
        let mut backup = loader.load_or_create(self.backup_name(self.frames.len())?)?;
        backup.copy_from(&self.live);
        self.frames.push(backup);
        Ok(())
    }

    /// Close the innermost frame. With `restore`, the live state comes back from
    /// its backup first. The bedrock is not a frame, so an empty stack closes
    /// nothing.
    ///
    /// The marker is written on the way out of a run and `start` clears it on
    /// the way in, so it is always absent here: there is no count to keep in
    /// step.
    fn close_frame(&mut self, restore: bool) -> Result<(), SnapshotError> {
        let mut backup = self.frames.pop().ok_or(SnapshotError::NoOpenFrame)?;
        if restore {
            self.live.move_from(&mut backup);
        } else {
            backup.clear();
        }
        Ok(())
    }
}

/// A keyspace that can open and close revertible frames over itself.
///
/// Implemented by [`SnapshottedKeySpace`] and by mutable borrows of one.
pub trait SafeKeyspace: KeySpace {
    /// The live keyspace the frames back up.
    type Live: KeySpace;

    /// Open a frame: back up the live state so a later close can bring it back.
    fn checkpoint(
        &mut self,
        loader: &mut impl KeySpaceLoader<KeySpace = Self::Live>,
    ) -> Result<(), SnapshotError>;

    /// How many frames are open. A scope reads it on entry and closes back down
    /// to it, which also closes any frame a callee forgot.
    fn depth(&self) -> usize;

    /// Keep the writes of the innermost frame and drop its backup. Errors with
    /// [`SnapshotError::NoOpenFrame`] if no frame is open.
    fn commit_inner(&mut self) -> Result<(), SnapshotError>;

    /// Undo the writes of the innermost frame, from its backup, then drop it.
    /// Same errors as [`Self::commit_inner`].
    fn revert_inner(&mut self) -> Result<(), SnapshotError>;

    /// Give up the run: revert the live state to the bedrock, dropping every
    /// write since, and close every frame that was open. The backups are
    /// emptied so they do not sit in storage, and the marker is cleared: a
    /// given-up run is never resumed.
    fn revert_all(&mut self);

    /// Replace the bedrock with the live state. The next [`Self::revert_all`]
    /// will restore the live state to this point.
    ///
    /// Errors with [`SnapshotError::FrameStillOpen`] if a frame is open: the
    /// live state would carry writes no close has accepted.
    fn commit_all(&mut self) -> Result<(), SnapshotError>;

    /// Write down how many frames are open, so the next
    /// [`SnapshottedKeySpace::start`] takes their backups back. Call it before
    /// yielding to a reboot.
    fn create_reboot_marker(&mut self) -> Result<(), SnapshotError>;
}

/// An unmarked run's backups are never read again: `start` only takes them back
/// on a marker, and this is the branch with none. Leaving them would be just as
/// correct, they are emptied so they do not sit in storage. The marker needs
/// nothing: this is the branch where it is already absent.
///
/// The live keyspace is left alone either way: a drop is not a revert.
impl<KS: KeySpace> Drop for SnapshottedKeySpace<KS> {
    fn drop(&mut self) {
        if self.reboot_frames().is_some() {
            return;
        }
        self.frames.iter_mut().for_each(KS::clear);
        self.bedrock.clear();
    }
}

impl<KS: KeySpace> SafeKeyspace for SnapshottedKeySpace<KS> {
    type Live = KS;

    fn checkpoint(
        &mut self,
        loader: &mut impl KeySpaceLoader<KeySpace = KS>,
    ) -> Result<(), SnapshotError> {
        self.open_frame(loader)
    }

    fn depth(&self) -> usize {
        self.frames.len()
    }

    fn commit_inner(&mut self) -> Result<(), SnapshotError> {
        self.close_frame(false)
    }

    fn revert_inner(&mut self) -> Result<(), SnapshotError> {
        self.close_frame(true)
    }

    fn revert_all(&mut self) {
        // Every backup above the bedrock is dead: the live state is going back
        // below all of them. Empty them before dropping the handles, or they
        // would sit in storage with nothing left to clear them.
        self.frames.iter_mut().for_each(KS::clear);
        self.frames.clear();
        // A given-up run is never resumed: the marker goes with it.
        self.meta.clear();
        self.live.copy_from(&self.bedrock);
    }

    fn create_reboot_marker(&mut self) -> Result<(), SnapshotError> {
        // One write, so a marking is never cut half way through.
        let open = frame_number(self.frames.len())?;
        self.meta.set(&FRAME_MARKER, open.to_le_bytes())?;
        Ok(())
    }

    fn commit_all(&mut self) -> Result<(), SnapshotError> {
        if !self.frames.is_empty() {
            return Err(SnapshotError::FrameStillOpen);
        }
        self.bedrock.copy_from(&self.live);
        Ok(())
    }
}

/// A mutable borrow of a [`SafeKeyspace`] is one too: same transaction.
impl<S: SafeKeyspace> SafeKeyspace for &mut S {
    type Live = S::Live;

    fn checkpoint(
        &mut self,
        loader: &mut impl KeySpaceLoader<KeySpace = Self::Live>,
    ) -> Result<(), SnapshotError> {
        (**self).checkpoint(loader)
    }

    fn depth(&self) -> usize {
        (**self).depth()
    }

    fn commit_inner(&mut self) -> Result<(), SnapshotError> {
        (**self).commit_inner()
    }

    fn revert_inner(&mut self) -> Result<(), SnapshotError> {
        (**self).revert_inner()
    }

    fn revert_all(&mut self) {
        (**self).revert_all()
    }

    fn create_reboot_marker(&mut self) -> Result<(), SnapshotError> {
        (**self).create_reboot_marker()
    }

    fn commit_all(&mut self) -> Result<(), SnapshotError> {
        (**self).commit_all()
    }
}

/// Delegates every operation to the live keyspace.
impl<KS: KeySpace> KeySpace for SnapshottedKeySpace<KS> {
    fn name(&self) -> &Name {
        self.live.name()
    }

    fn get(&self, key: &Key) -> Option<Vec<u8>> {
        self.live.get(key)
    }

    fn read(&self, key: &Key, offset: usize, buffer: &mut [u8]) -> Option<usize> {
        self.live.read(key, offset, buffer)
    }

    fn set(
        &mut self,
        key: &Key,
        value: impl AsRef<[u8]>,
    ) -> Result<(), KeySpaceWriteError> {
        self.live.set(key, value)
    }

    fn write(
        &mut self,
        key: &Key,
        offset: usize,
        data: impl AsRef<[u8]>,
    ) -> Result<usize, KeySpaceWriteError> {
        self.live.write(key, offset, data)
    }

    fn value_length(&self, key: &Key) -> Option<usize> {
        self.live.value_length(key)
    }

    fn contains(&self, key: &Key) -> bool {
        self.live.contains(key)
    }

    fn delete(&mut self, key: &Key) -> bool {
        self.live.delete(key)
    }

    fn clear(&mut self) {
        self.live.clear()
    }

    /// Replaces the live associations only: `self`'s stack covers this write
    /// like any other.
    fn copy_from(&mut self, other: &Self) {
        self.live.copy_from(&other.live)
    }

    /// Like [`Self::copy_from`], and empties `other`'s live root. Each stack
    /// covers its own live root and no other, so a move between two roots is
    /// atomic only if both hold an open frame.
    fn move_from(&mut self, other: &mut Self) {
        self.live.move_from(&mut other.live)
    }

    fn hash(&self) -> Vec<u8> {
        self.live.hash()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::MockKernelHost;
    use tezos_smart_rollup_keyspace::MAX_KEYSPACE_NAME_SIZE;

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

    /// Read `k` in the backup `name`, once its handle has been released.
    fn read_backup(host: &mut MockKernelHost, name: Name, k: &[u8]) -> Option<Vec<u8>> {
        let ks = host.load_or_create(name).unwrap();
        ks.get(&key(k))
    }

    /// ROOT_A's backup below the frames. The probes derive their names;
    /// `frame_name_keys_by_depth` is where the spelling is pinned.
    fn bedrock_a() -> Name {
        bedrock_name(&root_a()).unwrap()
    }

    /// ROOT_A's backup at `depth`.
    fn frame_a(depth: FrameNumber) -> Name {
        frame_name(&root_a(), depth).unwrap()
    }

    /// ROOT_B's backup at `depth`.
    fn frame_b(depth: FrameNumber) -> Name {
        frame_name(&root_b(), depth).unwrap()
    }

    /// Start over `live` and open the block frame.
    fn open_over<KS: KeySpace>(
        loader: &mut impl KeySpaceLoader<KeySpace = KS>,
        live: KS,
    ) -> SnapshottedKeySpace<KS> {
        let mut snapshot =
            SnapshottedKeySpace::start(loader, live, PreviousRun::Complete).unwrap();
        snapshot.checkpoint(loader).unwrap();
        snapshot
    }

    /// Close every frame above `scope`, as a consumer scope does on exit.
    fn close_down_to(
        snapshot: &mut impl SafeKeyspace,
        scope: usize,
        restore: bool,
    ) -> Result<(), SnapshotError> {
        while snapshot.depth() > scope {
            if restore {
                snapshot.revert_inner()?;
            } else {
                snapshot.commit_inner()?;
            }
        }
        Ok(())
    }

    // ----- Naming -----

    #[test]
    fn frame_name_keys_by_depth() {
        let root: Name = "/tez/tez_accounts".parse().unwrap();
        // Each backup is named by its depth, spelled on a fixed width.
        assert_eq!(
            frame_name(&root, 0).unwrap().as_ref(),
            "/__snapshot/frame/00000000/tez/tez_accounts"
        );
        assert_eq!(
            frame_name(&root, 1).unwrap().as_ref(),
            "/__snapshot/frame/00000001/tez/tez_accounts"
        );
        assert_eq!(
            frame_name(&root, 10).unwrap().as_ref(),
            "/__snapshot/frame/0000000a/tez/tez_accounts"
        );
    }

    #[test]
    fn a_frame_number_never_eats_into_its_root() {
        let root = root_b();
        let at_zero = frame_name(&root, 0).unwrap();
        // Whatever a number takes to write, it takes the same room in the name.
        for depth in [1, 9, 10, 0xff, FrameNumber::MAX] {
            let name = frame_name(&root, depth).unwrap();
            assert_ne!(name, at_zero);
            assert_eq!(name.as_ref().len(), at_zero.as_ref().len());
        }
    }

    /// A `usize` wider than a frame number only exists off the kernel target.
    #[test]
    #[cfg(target_pointer_width = "64")]
    fn a_depth_past_a_frame_number_has_no_name() {
        assert_eq!(
            frame_number(FrameNumber::MAX as usize + 1),
            Err(SnapshotError::TooManyFrames)
        );
    }

    /// A root of `N` bytes: a slash and then filler.
    fn root_of<const N: usize>() -> Name {
        let mut root = [b'a'; N];
        root[0] = b'/';
        std::str::from_utf8(&root).unwrap().parse().unwrap()
    }

    /// What a frame name spends before its root: the prefix, the separator and
    /// the number.
    const FRAME_NAME_OVERHEAD: usize =
        FRAME_PREFIX.len() + "/".len() + FRAME_NUMBER_HEX_DIGITS;

    #[test]
    fn a_root_whose_backups_have_no_name_never_starts() {
        let mut host = MockKernelHost::default();
        // The longest root the bedrock still names, so the frames are the only
        // backup it is too long for.
        let root = root_of::<{ MAX_KEYSPACE_NAME_SIZE - BEDROCK_PREFIX.len() }>();
        assert!(frame_name(&root, 0).is_err());
        assert!(bedrock_name(&root).is_ok());
        let live = host.load_or_create(root).unwrap();

        // Refused at the top of the run, not at the first checkpoint that would
        // have tried to back it up.
        assert!(matches!(
            SnapshottedKeySpace::start(&mut host, live, PreviousRun::Complete),
            Err(SnapshotError::InvalidName(NameError::NameTooLong))
        ));
    }

    #[test]
    fn a_root_that_starts_backs_up_at_every_depth() {
        let mut host = MockKernelHost::default();
        // The longest root whose backups still have names.
        let root = root_of::<{ MAX_KEYSPACE_NAME_SIZE - FRAME_NAME_OVERHEAD }>();
        let live = host.load_or_create(root).unwrap();
        let mut snapshot =
            SnapshottedKeySpace::start(&mut host, live, PreviousRun::Complete).unwrap();

        // Deeper than one digit, on a root with no byte to spare.
        for _ in 0..12 {
            snapshot.checkpoint(&mut host).unwrap();
        }
        assert_eq!(snapshot.depth(), 12);
    }

    #[test]
    fn the_bedrock_is_named_out_of_the_frames() {
        let root: Name = "/tez/tez_accounts".parse().unwrap();
        // Its own prefix, so no frame depth can ever collide with it.
        assert_eq!(
            bedrock_name(&root).unwrap().as_ref(),
            "/__snapshot/bedrock/tez/tez_accounts"
        );
    }

    // ----- Transaction level -----

    #[test]
    fn commit_inner_keeps_live_writes() {
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_a()).unwrap();
        live.set(&key(b"/balance"), b"initial").unwrap();

        let mut snapshot = open_over(&mut host, live);

        // Write through the snapshot, as block execution does.
        snapshot.set(&key(b"/balance"), b"updated").unwrap();
        snapshot.set(&key(b"/new"), b"value").unwrap();

        snapshot.commit_inner().unwrap();

        assert_eq!(snapshot.get(&key(b"/balance")), Some(b"updated".to_vec()));
        assert_eq!(snapshot.get(&key(b"/new")), Some(b"value".to_vec()));
        // The backup is emptied.
        assert_eq!(read_backup(&mut host, frame_a(0), b"/balance"), None);
    }

    #[test]
    fn revert_inner_restores_pre_block_state() {
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_a()).unwrap();
        live.set(&key(b"/balance"), b"initial").unwrap();

        let mut snapshot = open_over(&mut host, live);

        snapshot.set(&key(b"/balance"), b"updated").unwrap();
        snapshot.set(&key(b"/new"), b"value").unwrap();

        snapshot.revert_inner().unwrap();

        // Back to the state the frame opened at.
        assert_eq!(snapshot.get(&key(b"/balance")), Some(b"initial".to_vec()));
        assert_eq!(snapshot.get(&key(b"/new")), None);
    }

    #[test]
    fn revert_inner_clears_root_that_started_empty() {
        let mut host = MockKernelHost::default();
        let live = host.load_or_create(root_a()).unwrap();
        // ROOT_A is empty when the frame opens.
        let mut snapshot = open_over(&mut host, live);

        snapshot.set(&key(b"/created"), b"during-block").unwrap();

        snapshot.revert_inner().unwrap();

        // A key created inside the frame is gone.
        assert_eq!(snapshot.get(&key(b"/created")), None);
    }

    #[test]
    fn starting_unmarked_overwrites_a_stale_bedrock() {
        let mut host = MockKernelHost::default();
        // Leave garbage at depth -1, with no marker, and release the handle.
        {
            let mut stale = host.load_or_create(bedrock_a()).unwrap();
            stale.set(&key(b"/stale"), b"garbage").unwrap();
        }

        let mut live = host.load_or_create(root_a()).unwrap();
        live.set(&key(b"/balance"), b"initial").unwrap();

        // `start` must replace that content, not merge into it.
        let mut snapshot = open_over(&mut host, live);

        snapshot.set(&key(b"/balance"), b"updated").unwrap();
        snapshot.revert_inner().unwrap();

        // Merged instead, `/stale` would leak into the live root on revert.
        assert_eq!(snapshot.get(&key(b"/balance")), Some(b"initial".to_vec()));
        assert_eq!(snapshot.get(&key(b"/stale")), None);
        assert_eq!(snapshot.bedrock().get(&key(b"/stale")), None);
    }

    #[test]
    fn starting_after_a_trap_gives_up_the_interrupted_block() {
        let mut host = MockKernelHost::default();
        // A run the PVM cut short runs no `Drop`, so its bedrock survives.
        {
            let mut bedrock = host.load_or_create(bedrock_a()).unwrap();
            bedrock.set(&key(b"/balance"), b"promoted").unwrap();
        }
        // Its own writes stay in the live root.
        let mut live = host.load_or_create(root_a()).unwrap();
        live.set(&key(b"/balance"), b"half-written").unwrap();
        live.set(&key(b"/leftover"), b"garbage").unwrap();

        let snapshot =
            SnapshottedKeySpace::start(&mut host, live, PreviousRun::Aborted).unwrap();

        // The interrupted block is gone, as deleting `/tmp` did before.
        assert_eq!(snapshot.get(&key(b"/balance")), Some(b"promoted".to_vec()));
        assert_eq!(snapshot.get(&key(b"/leftover")), None);
        assert_eq!(snapshot.depth(), 0);
    }

    #[test]
    fn a_trap_does_not_undo_a_run_that_reached_its_end() {
        let mut host = MockKernelHost::default();
        {
            let mut bedrock = host.load_or_create(bedrock_a()).unwrap();
            bedrock.set(&key(b"/balance"), b"stale").unwrap();
        }
        let mut live = host.load_or_create(root_a()).unwrap();
        live.set(&key(b"/balance"), b"promoted").unwrap();

        // Same storage, but a complete previous run: the stale bedrock is
        // replaced, not applied.
        let snapshot =
            SnapshottedKeySpace::start(&mut host, live, PreviousRun::Complete).unwrap();

        assert_eq!(snapshot.get(&key(b"/balance")), Some(b"promoted".to_vec()));
        assert_eq!(
            snapshot.bedrock().get(&key(b"/balance")),
            Some(b"promoted".to_vec())
        );
    }

    // ----- Marker -----

    #[test]
    fn start_without_a_marker_opens_no_frame() {
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_a()).unwrap();
        live.set(&key(b"/balance"), b"live-data").unwrap();

        // No marker, so nothing is taken back.
        let snapshot =
            SnapshottedKeySpace::start(&mut host, live, PreviousRun::Complete).unwrap();
        assert_eq!(snapshot.depth(), 0);

        // The live root is untouched.
        assert_eq!(snapshot.get(&key(b"/balance")), Some(b"live-data".to_vec()));
    }

    #[test]
    fn a_committed_block_left_unmarked_starts_over() {
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_a()).unwrap();
        live.set(&key(b"/balance"), b"initial").unwrap();

        let mut snapshot = open_over(&mut host, live);
        snapshot.set(&key(b"/balance"), b"committed").unwrap();
        snapshot.commit_inner().unwrap();
        drop(snapshot);

        // No marker, so the next run starts over.
        let live = host.load_or_create(root_a()).unwrap();
        let snapshot =
            SnapshottedKeySpace::start(&mut host, live, PreviousRun::Complete).unwrap();
        assert_eq!(snapshot.depth(), 0);
        assert_eq!(snapshot.get(&key(b"/balance")), Some(b"committed".to_vec()));
    }

    #[test]
    fn a_committed_block_leaves_the_next_run_a_fresh_bedrock() {
        // The bedrock is where `revert_all` lands, so it has to hold the state
        // this run started from, not the one an earlier run did.
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_a()).unwrap();
        live.set(&key(b"/balance"), b"initial").unwrap();

        let mut snapshot = open_over(&mut host, live);
        snapshot.set(&key(b"/balance"), b"committed").unwrap();
        snapshot.commit_inner().unwrap();
        drop(snapshot);

        let live = host.load_or_create(root_a()).unwrap();
        let mut snapshot =
            SnapshottedKeySpace::start(&mut host, live, PreviousRun::Complete).unwrap();
        snapshot.set(&key(b"/balance"), b"in flight").unwrap();
        snapshot.revert_all();

        assert_eq!(snapshot.get(&key(b"/balance")), Some(b"committed".to_vec()));
    }

    #[test]
    fn a_reverted_block_left_unmarked_starts_over() {
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_a()).unwrap();
        live.set(&key(b"/balance"), b"initial").unwrap();

        let mut snapshot = open_over(&mut host, live);
        snapshot.set(&key(b"/balance"), b"aborted").unwrap();
        snapshot.revert_inner().unwrap();
        drop(snapshot);

        let live = host.load_or_create(root_a()).unwrap();
        let snapshot =
            SnapshottedKeySpace::start(&mut host, live, PreviousRun::Complete).unwrap();
        assert_eq!(snapshot.depth(), 0);
        assert_eq!(snapshot.get(&key(b"/balance")), Some(b"initial".to_vec()));
    }

    #[test]
    fn start_is_independent_per_root() {
        let mut host = MockKernelHost::default();
        let mut live_a = host.load_or_create(root_a()).unwrap();
        live_a.set(&key(b"/balance"), b"initial").unwrap();

        // Work on ROOT_A, mark, then drop: a reboot mid-block.
        {
            let mut snapshot = open_over(&mut host, live_a);
            snapshot.set(&key(b"/balance"), b"partial").unwrap();
            snapshot.create_reboot_marker().unwrap();
            drop(snapshot);
        }

        // The marker is per root, so ROOT_A's says nothing about ROOT_B.
        let live_b = host.load_or_create(root_b()).unwrap();
        let snapshot_b =
            SnapshottedKeySpace::start(&mut host, live_b, PreviousRun::Complete).unwrap();
        assert_eq!(snapshot_b.depth(), 0);
        drop(snapshot_b);

        // ROOT_A takes its own frame back and reverts it.
        let live_a = host.load_or_create(root_a()).unwrap();
        let mut snapshot_a =
            SnapshottedKeySpace::start(&mut host, live_a, PreviousRun::Complete).unwrap();
        assert_eq!(snapshot_a.depth(), 1);
        snapshot_a.revert_inner().unwrap();
        assert_eq!(snapshot_a.get(&key(b"/balance")), Some(b"initial".to_vec()));
    }

    // ----- Nested frames -----

    #[test]
    fn commit_inner_keeps_checkpoint_writes() {
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_b()).unwrap();
        live.set(&key(b"/balance"), b"initial").unwrap();

        let mut snapshot = open_over(&mut host, live);
        snapshot.checkpoint(&mut host).unwrap();
        snapshot.set(&key(b"/balance"), b"updated").unwrap();
        snapshot.commit_inner().unwrap();

        assert_eq!(snapshot.get(&key(b"/balance")), Some(b"updated".to_vec()));
        // The nested backup is emptied.
        assert_eq!(read_backup(&mut host, frame_b(1), b"/balance"), None);
        snapshot.commit_inner().unwrap();
    }

    #[test]
    fn revert_inner_restores_pre_checkpoint_state() {
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_b()).unwrap();
        live.set(&key(b"/balance"), b"initial").unwrap();

        let mut snapshot = open_over(&mut host, live);
        snapshot.checkpoint(&mut host).unwrap();
        snapshot.set(&key(b"/balance"), b"updated").unwrap();
        snapshot.set(&key(b"/new"), b"value").unwrap();
        snapshot.revert_inner().unwrap();

        assert_eq!(snapshot.get(&key(b"/balance")), Some(b"initial".to_vec()));
        assert_eq!(snapshot.get(&key(b"/new")), None);
        assert_eq!(read_backup(&mut host, frame_b(1), b"/balance"), None);
        snapshot.commit_inner().unwrap();
    }

    #[test]
    fn revert_inner_clears_root_empty_at_the_checkpoint() {
        let mut host = MockKernelHost::default();
        let live = host.load_or_create(root_b()).unwrap();
        // ROOT_B is empty when the nested frame opens.
        let mut snapshot = open_over(&mut host, live);
        snapshot.checkpoint(&mut host).unwrap();
        snapshot.set(&key(b"/created"), b"during-op").unwrap();
        snapshot.revert_inner().unwrap();

        assert_eq!(snapshot.get(&key(b"/created")), None);
        snapshot.commit_inner().unwrap();
    }

    #[test]
    fn two_phase_keeps_validation_drops_application() {
        // As validate_and_apply_operation does: commit the validation, revert
        // the application, and the fee debit survives.
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_b()).unwrap();
        live.set(&key(b"/balance"), b"initial").unwrap();

        let mut snapshot = open_over(&mut host, live);

        // Phase 1: validation succeeds and is kept.
        snapshot.checkpoint(&mut host).unwrap();
        snapshot.set(&key(b"/balance"), b"fee-debited").unwrap();
        snapshot.commit_inner().unwrap();

        // Phase 2: application fails and is undone.
        snapshot.checkpoint(&mut host).unwrap();
        snapshot.set(&key(b"/balance"), b"applied").unwrap();
        snapshot.set(&key(b"/storage"), b"junk").unwrap();
        snapshot.revert_inner().unwrap();

        // The fee debit from validation survives; the application is gone.
        assert_eq!(
            snapshot.get(&key(b"/balance")),
            Some(b"fee-debited".to_vec())
        );
        assert_eq!(snapshot.get(&key(b"/storage")), None);
        snapshot.commit_inner().unwrap();
    }

    #[test]
    fn closing_a_scope_absorbs_a_checkpoint_left_open() {
        // Closing down to a recorded depth also closes what a callee left open.
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_b()).unwrap();
        live.set(&key(b"/balance"), b"initial").unwrap();

        let mut snapshot = open_over(&mut host, live);

        let scope = snapshot.depth();
        snapshot.checkpoint(&mut host).unwrap();
        snapshot.set(&key(b"/balance"), b"applied").unwrap();
        // The callee's frame, opened after the scope's write and never closed.
        snapshot.checkpoint(&mut host).unwrap();
        snapshot.set(&key(b"/storage"), b"forwarder").unwrap();

        close_down_to(&mut snapshot, scope, true).unwrap();

        assert_eq!(snapshot.get(&key(b"/balance")), Some(b"initial".to_vec()));
        assert_eq!(snapshot.get(&key(b"/storage")), None);
        assert_eq!(snapshot.depth(), scope);
        snapshot.commit_inner().unwrap();
    }

    #[test]
    fn accepting_a_scope_absorbs_a_checkpoint_left_open() {
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_b()).unwrap();
        live.set(&key(b"/balance"), b"initial").unwrap();

        let mut snapshot = open_over(&mut host, live);

        let scope = snapshot.depth();
        snapshot.checkpoint(&mut host).unwrap();
        snapshot.set(&key(b"/balance"), b"applied").unwrap();
        snapshot.checkpoint(&mut host).unwrap();
        snapshot.set(&key(b"/storage"), b"forwarder").unwrap();

        close_down_to(&mut snapshot, scope, false).unwrap();

        assert_eq!(snapshot.get(&key(b"/balance")), Some(b"applied".to_vec()));
        assert_eq!(snapshot.get(&key(b"/storage")), Some(b"forwarder".to_vec()));
        assert_eq!(snapshot.depth(), scope);
        snapshot.commit_inner().unwrap();
    }

    #[test]
    fn a_scope_already_at_its_depth_closes_nothing() {
        // Closing at its own depth does nothing, so a scope that closes twice
        // does not eat the frame below it.
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_a()).unwrap();
        live.set(&key(b"/balance"), b"initial").unwrap();

        let mut snapshot = open_over(&mut host, live);
        let scope = snapshot.depth();
        close_down_to(&mut snapshot, scope, false).unwrap();
        close_down_to(&mut snapshot, scope, true).unwrap();
        assert_eq!(snapshot.depth(), scope);

        // The frame below is intact: a revert still undoes the block.
        snapshot.set(&key(b"/balance"), b"updated").unwrap();
        snapshot.revert_inner().unwrap();
        assert_eq!(snapshot.get(&key(b"/balance")), Some(b"initial".to_vec()));
    }

    #[test]
    fn nested_checkpoints_roll_back_independently() {
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_b()).unwrap();
        live.set(&key(b"/v"), b"0").unwrap();

        let mut snapshot = open_over(&mut host, live);

        snapshot.checkpoint(&mut host).unwrap(); // depth 1
        snapshot.set(&key(b"/v"), b"1").unwrap();

        snapshot.checkpoint(&mut host).unwrap(); // depth 2
        snapshot.set(&key(b"/v"), b"2").unwrap();

        // Undo the inner frame: back to "1".
        snapshot.revert_inner().unwrap();
        assert_eq!(snapshot.get(&key(b"/v")), Some(b"1".to_vec()));

        // Undo the outer frame: back to "0".
        snapshot.revert_inner().unwrap();
        assert_eq!(snapshot.get(&key(b"/v")), Some(b"0".to_vec()));

        snapshot.commit_inner().unwrap();
    }

    #[test]
    fn a_nested_frame_is_disjoint_from_depth_zero() {
        // Each frame closes against its own state, not the one below it.
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_b()).unwrap();
        live.set(&key(b"/account"), b"pre-block").unwrap();

        let mut snapshot = open_over(&mut host, live);
        snapshot.set(&key(b"/account"), b"mid-block").unwrap();

        // The nested revert goes back to mid-block, not pre-block.
        snapshot.checkpoint(&mut host).unwrap();
        snapshot.set(&key(b"/account"), b"in-op").unwrap();
        snapshot.revert_inner().unwrap();
        assert_eq!(snapshot.get(&key(b"/account")), Some(b"mid-block".to_vec()));

        // The frame below still holds pre-block.
        snapshot.revert_inner().unwrap();
        assert_eq!(snapshot.get(&key(b"/account")), Some(b"pre-block".to_vec()));
    }

    // ----- KeySpace delegation -----

    /// An accessor bounded by [`KeySpace`] alone, which knows nothing of frames.
    fn accessor_roundtrip(ks: &mut impl KeySpace) -> Option<Vec<u8>> {
        ks.set(&key(b"/written"), b"by-accessor").unwrap();
        assert!(ks.contains(&key(b"/written")));
        assert_eq!(ks.value_length(&key(b"/written")), Some(11));
        let read_back = ks.get(&key(b"/written"));
        assert!(ks.delete(&key(b"/written")));
        ks.set(&key(b"/written"), b"by-accessor").unwrap();
        read_back
    }

    #[test]
    fn accessors_behave_the_same_on_a_transactional_keyspace() {
        let mut host = MockKernelHost::default();

        // The same accessor, on a plain root then on a snapshotted one.
        let mut plain = host.load_or_create(root_a()).unwrap();
        let from_plain = accessor_roundtrip(&mut plain);

        let live = host.load_or_create(root_b()).unwrap();
        let mut snapshot = open_over(&mut host, live);
        let from_snapshotted = accessor_roundtrip(&mut snapshot);

        assert_eq!(from_plain, from_snapshotted);
        // `name` gives the live root, never a backup.
        assert_eq!(snapshot.name(), &root_b());

        // Its writes are still in a frame, so a revert undoes them.
        snapshot.checkpoint(&mut host).unwrap();
        snapshot.set(&key(b"/written"), b"clobbered").unwrap();
        snapshot.revert_inner().unwrap();
        assert_eq!(
            snapshot.get(&key(b"/written")),
            Some(b"by-accessor".to_vec())
        );

        // `hash` is not asserted: the mock hashes a call counter, not the data.
        snapshot.commit_inner().unwrap();
    }

    #[test]
    fn delegated_writes_land_on_the_live_root() {
        let mut host = MockKernelHost::default();
        let live = host.load_or_create(root_b()).unwrap();

        let mut snapshot = open_over(&mut host, live);
        snapshot.set(&key(b"/balance"), b"written-through").unwrap();
        snapshot.commit_inner().unwrap();

        // The write is on the live root, so reloading it after the drop sees it.
        drop(snapshot);
        let live = host.load_or_create(root_b()).unwrap();
        assert_eq!(
            live.get(&key(b"/balance")),
            Some(b"written-through".to_vec())
        );
        assert_eq!(live.name(), &root_b());
    }

    // ----- In-place lifecycle -----

    /// Leave a marked, half-finished block in storage, as a run does when it
    /// yields to a reboot.
    fn interrupted_transaction(host: &mut MockKernelHost) {
        let mut live = host.load_or_create(root_a()).unwrap();
        live.set(&key(b"/balance"), b"pre-block").unwrap();
        let mut snapshot = open_over(host, live);
        snapshot.set(&key(b"/balance"), b"mid-block").unwrap();
        snapshot.create_reboot_marker().unwrap();
        drop(snapshot);
    }

    #[test]
    fn start_reuses_the_backup_a_reboot_left() {
        // A reboot loses the stack, not the backups, so taking them back must
        // not copy over them.
        let mut host = MockKernelHost::default();
        interrupted_transaction(&mut host);

        assert_eq!(
            read_backup(&mut host, frame_a(0), b"/balance"),
            Some(b"pre-block".to_vec())
        );

        let live = host.load_or_create(root_a()).unwrap();
        let snapshot =
            SnapshottedKeySpace::start(&mut host, live, PreviousRun::Complete).unwrap();
        assert_eq!(snapshot.depth(), 1);
        // The interrupted run's writes are still live, and the backup below
        // the frames was taken back as it stood.
        assert_eq!(snapshot.get(&key(b"/balance")), Some(b"mid-block".to_vec()));
        assert_eq!(
            snapshot.bedrock().get(&key(b"/balance")),
            Some(b"pre-block".to_vec())
        );
    }

    #[test]
    fn start_resumes_then_revert_undoes_the_whole_block() {
        let mut host = MockKernelHost::default();
        interrupted_transaction(&mut host);

        let live = host.load_or_create(root_a()).unwrap();
        let mut snapshot =
            SnapshottedKeySpace::start(&mut host, live, PreviousRun::Complete).unwrap();
        snapshot.set(&key(b"/balance"), b"more-partial").unwrap();
        snapshot.revert_inner().unwrap();

        // Back to pre-block, dropping both runs' writes.
        assert_eq!(snapshot.get(&key(b"/balance")), Some(b"pre-block".to_vec()));
    }

    #[test]
    fn start_resumes_then_commit_keeps_accumulated_work() {
        let mut host = MockKernelHost::default();
        interrupted_transaction(&mut host);

        let live = host.load_or_create(root_a()).unwrap();
        let mut snapshot =
            SnapshottedKeySpace::start(&mut host, live, PreviousRun::Complete).unwrap();
        snapshot.set(&key(b"/balance"), b"reboot2").unwrap();
        snapshot.commit_inner().unwrap();

        assert_eq!(snapshot.get(&key(b"/balance")), Some(b"reboot2".to_vec()));
    }

    #[test]
    fn a_close_leaves_no_backup_behind() {
        // Commit and revert both empty the backup they used.
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_a()).unwrap();
        live.set(&key(b"/balance"), b"initial").unwrap();
        let mut snapshot =
            SnapshottedKeySpace::start(&mut host, live, PreviousRun::Complete).unwrap();

        snapshot.checkpoint(&mut host).unwrap();
        snapshot.set(&key(b"/balance"), b"committed").unwrap();
        snapshot.commit_inner().unwrap();
        assert_eq!(read_backup(&mut host, frame_a(0), b"/balance"), None);

        snapshot.checkpoint(&mut host).unwrap();
        snapshot.set(&key(b"/balance"), b"aborted").unwrap();
        snapshot.revert_inner().unwrap();
        assert_eq!(read_backup(&mut host, frame_a(0), b"/balance"), None);
        assert_eq!(snapshot.get(&key(b"/balance")), Some(b"committed".to_vec()));
    }

    #[test]
    fn a_run_that_closed_its_frame_starts_over() {
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_a()).unwrap();
        live.set(&key(b"/v"), b"before").unwrap();
        let mut snapshot =
            SnapshottedKeySpace::start(&mut host, live, PreviousRun::Complete).unwrap();

        snapshot.checkpoint(&mut host).unwrap();
        snapshot.set(&key(b"/v"), b"during").unwrap();
        snapshot.revert_inner().unwrap();

        assert_eq!(snapshot.get(&key(b"/v")), Some(b"before".to_vec()));
        drop(snapshot);

        // No marker, so there is nothing to take back.
        let live = host.load_or_create(root_a()).unwrap();
        let snapshot =
            SnapshottedKeySpace::start(&mut host, live, PreviousRun::Complete).unwrap();
        assert_eq!(snapshot.depth(), 0);
    }

    #[test]
    fn a_lone_frame_survives_a_reboot() {
        // A single open frame is counted like any other.
        let mut host = MockKernelHost::default();
        {
            let mut live = host.load_or_create(root_a()).unwrap();
            live.set(&key(b"/v"), b"before").unwrap();
            let mut snapshot =
                SnapshottedKeySpace::start(&mut host, live, PreviousRun::Complete)
                    .unwrap();
            snapshot.checkpoint(&mut host).unwrap();
            snapshot.set(&key(b"/v"), b"during").unwrap();
            snapshot.create_reboot_marker().unwrap();
            drop(snapshot);
        }

        let live = host.load_or_create(root_a()).unwrap();
        let mut snapshot =
            SnapshottedKeySpace::start(&mut host, live, PreviousRun::Complete).unwrap();
        assert_eq!(snapshot.depth(), 1);
        snapshot.revert_inner().unwrap();

        assert_eq!(snapshot.get(&key(b"/v")), Some(b"before".to_vec()));
    }

    #[test]
    fn a_nested_stack_survives_a_reboot() {
        // The count covers the whole stack, so it all comes back and each
        // frame still closes against its own state.
        let mut host = MockKernelHost::default();
        {
            let mut live = host.load_or_create(root_a()).unwrap();
            live.set(&key(b"/v"), b"before").unwrap();
            let mut snapshot =
                SnapshottedKeySpace::start(&mut host, live, PreviousRun::Complete)
                    .unwrap();
            snapshot.checkpoint(&mut host).unwrap();
            snapshot.set(&key(b"/v"), b"block").unwrap();
            snapshot.checkpoint(&mut host).unwrap();
            snapshot.set(&key(b"/v"), b"operation").unwrap();
            snapshot.create_reboot_marker().unwrap();
            drop(snapshot);
        }

        let live = host.load_or_create(root_a()).unwrap();
        let mut snapshot =
            SnapshottedKeySpace::start(&mut host, live, PreviousRun::Complete).unwrap();
        assert_eq!(snapshot.depth(), 2);

        // The inner frame undoes the operation, the outer one the block.
        snapshot.revert_inner().unwrap();
        assert_eq!(snapshot.get(&key(b"/v")), Some(b"block".to_vec()));
        snapshot.revert_inner().unwrap();
        assert_eq!(snapshot.get(&key(b"/v")), Some(b"before".to_vec()));
        assert!(matches!(
            snapshot.revert_inner(),
            Err(SnapshotError::NoOpenFrame)
        ));
    }

    #[test]
    fn the_backup_below_the_frames_is_never_closed() {
        // It sits at a depth no frame can claim, and closing the last frame
        // leaves it alone.
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_a()).unwrap();
        live.set(&key(b"/v"), b"before").unwrap();

        let mut snapshot = open_over(&mut host, live);
        snapshot.set(&key(b"/v"), b"during").unwrap();
        snapshot.commit_inner().unwrap();

        assert_eq!(snapshot.depth(), 0);
        // Depth 0 named the frame just closed, not this one.
        assert_eq!(read_backup(&mut host, frame_a(0), b"/v"), None);
        assert_eq!(
            snapshot.bedrock().get(&key(b"/v")),
            Some(b"before".to_vec())
        );

        // Marked, so the drop keeps it and its name can be read.
        snapshot.create_reboot_marker().unwrap();
        drop(snapshot);
        assert_eq!(
            read_backup(&mut host, bedrock_a(), b"/v"),
            Some(b"before".to_vec())
        );
    }

    #[test]
    fn an_unmarked_drop_empties_the_backups_but_not_the_live_root() {
        // Nothing will read these backups again, so they are emptied.
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_a()).unwrap();
        live.set(&key(b"/v"), b"before").unwrap();

        let mut snapshot = open_over(&mut host, live);
        snapshot.set(&key(b"/v"), b"during").unwrap();
        // Two frames open and no marker: a run cut short.
        snapshot.checkpoint(&mut host).unwrap();
        drop(snapshot);

        assert_eq!(read_backup(&mut host, bedrock_a(), b"/v"), None);
        assert_eq!(read_backup(&mut host, frame_a(0), b"/v"), None);
        assert_eq!(read_backup(&mut host, frame_a(1), b"/v"), None);

        // The live root keeps the writes: a drop is not a revert.
        let live = host.load_or_create(root_a()).unwrap();
        assert_eq!(live.get(&key(b"/v")), Some(b"during".to_vec()));
    }

    #[test]
    fn a_marked_drop_leaves_every_backup_standing() {
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_a()).unwrap();
        live.set(&key(b"/v"), b"before").unwrap();

        let mut snapshot = open_over(&mut host, live);
        snapshot.set(&key(b"/v"), b"during").unwrap();
        snapshot.checkpoint(&mut host).unwrap();
        snapshot.create_reboot_marker().unwrap();
        drop(snapshot);

        assert_eq!(
            read_backup(&mut host, bedrock_a(), b"/v"),
            Some(b"before".to_vec())
        );
        assert_eq!(
            read_backup(&mut host, frame_a(0), b"/v"),
            Some(b"before".to_vec())
        );
        assert_eq!(
            read_backup(&mut host, frame_a(1), b"/v"),
            Some(b"during".to_vec())
        );
    }

    #[test]
    fn a_marked_run_that_starts_again_leaves_the_marker_consumed() {
        // The marker covers one reboot only: taken back once, then gone.
        let mut host = MockKernelHost::default();
        interrupted_transaction(&mut host);

        let live = host.load_or_create(root_a()).unwrap();
        let snapshot =
            SnapshottedKeySpace::start(&mut host, live, PreviousRun::Complete).unwrap();
        assert_eq!(snapshot.depth(), 1);
        drop(snapshot);

        let live = host.load_or_create(root_a()).unwrap();
        let snapshot =
            SnapshottedKeySpace::start(&mut host, live, PreviousRun::Complete).unwrap();
        assert_eq!(snapshot.depth(), 0);
    }

    #[test]
    fn closing_when_idle_is_refused() {
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_a()).unwrap();
        live.set(&key(b"/balance"), b"untouched").unwrap();
        let mut snapshot =
            SnapshottedKeySpace::start(&mut host, live, PreviousRun::Complete).unwrap();

        // Closing with no frame open is an error, not a no-op.
        assert!(matches!(
            snapshot.commit_inner(),
            Err(SnapshotError::NoOpenFrame)
        ));
        assert!(matches!(
            snapshot.revert_inner(),
            Err(SnapshotError::NoOpenFrame)
        ));
        // Either way the live root is untouched.
        assert_eq!(snapshot.get(&key(b"/balance")), Some(b"untouched".to_vec()));
    }

    #[test]
    fn a_scope_whose_frame_a_callee_took_closes_nothing() {
        // A callee that closed too many frames leaves the caller nothing to
        // close, over writes already committed. Only a bare close reports it.
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_b()).unwrap();
        live.set(&key(b"/balance"), b"initial").unwrap();

        let mut snapshot = open_over(&mut host, live);
        let scope = snapshot.depth();
        snapshot.checkpoint(&mut host).unwrap();
        snapshot.set(&key(b"/balance"), b"applied").unwrap();

        // The callee closes its own frame, then the caller's.
        snapshot.commit_inner().unwrap();
        snapshot.commit_inner().unwrap();
        assert_eq!(snapshot.depth(), 0);

        // The caller's close finds an empty stack and does nothing.
        close_down_to(&mut snapshot, scope, true).unwrap();
        assert_eq!(snapshot.get(&key(b"/balance")), Some(b"applied".to_vec()));

        assert!(matches!(
            snapshot.revert_inner(),
            Err(SnapshotError::NoOpenFrame)
        ));
    }

    #[test]
    fn wrapper_reopens_across_blocks() {
        // One snapshot for the whole run, one frame per block.
        let mut host = MockKernelHost::default();
        let live = host.load_or_create(root_a()).unwrap();
        let mut snapshot =
            SnapshottedKeySpace::start(&mut host, live, PreviousRun::Complete).unwrap();

        // With no frame open, writes go straight to the live root.
        snapshot.set(&key(b"/v"), b"genesis").unwrap();

        // Block 1 commits.
        snapshot.checkpoint(&mut host).unwrap();
        snapshot.set(&key(b"/v"), b"block-1").unwrap();
        snapshot.commit_inner().unwrap();

        // Block 2 reverts: back to block 1's state, not genesis.
        snapshot.checkpoint(&mut host).unwrap();
        snapshot.set(&key(b"/v"), b"block-2").unwrap();
        snapshot.revert_inner().unwrap();

        assert_eq!(snapshot.get(&key(b"/v")), Some(b"block-1".to_vec()));
    }

    // ----- Bedrock -----

    #[test]
    fn commit_all_moves_the_floor_the_next_revert_all_lands_on() {
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_a()).unwrap();
        live.set(&key(b"/balance"), b"pre-run").unwrap();

        let mut snapshot =
            SnapshottedKeySpace::start(&mut host, live, PreviousRun::Complete).unwrap();
        snapshot.set(&key(b"/balance"), b"settled").unwrap();
        snapshot.commit_all().unwrap();

        snapshot.set(&key(b"/balance"), b"later").unwrap();
        snapshot.set(&key(b"/scratch"), b"junk").unwrap();
        snapshot.revert_all();

        // The floor moved to the `commit_all` point, so the run start is out
        // of reach and only the writes after it are dropped.
        assert_eq!(snapshot.get(&key(b"/balance")), Some(b"settled".to_vec()));
        assert_eq!(snapshot.get(&key(b"/scratch")), None);
    }

    #[test]
    fn commit_all_with_a_frame_open_is_refused() {
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_a()).unwrap();
        live.set(&key(b"/balance"), b"pre-run").unwrap();

        let mut snapshot = open_over(&mut host, live);
        snapshot.set(&key(b"/balance"), b"in-block").unwrap();

        // The live state is covered by a frame no close has accepted, so it is
        // not a floor anyone can fall back to.
        assert!(matches!(
            snapshot.commit_all(),
            Err(SnapshotError::FrameStillOpen)
        ));

        // Refused, so the floor is still where the run started.
        assert_eq!(snapshot.depth(), 1);
        snapshot.revert_all();
        assert_eq!(snapshot.get(&key(b"/balance")), Some(b"pre-run".to_vec()));
    }

    #[test]
    fn revert_all_restores_the_state_the_run_started_from() {
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_a()).unwrap();
        live.set(&key(b"/balance"), b"pre-run").unwrap();

        let mut snapshot = open_over(&mut host, live);
        snapshot.set(&key(b"/balance"), b"block").unwrap();
        snapshot.checkpoint(&mut host).unwrap();
        snapshot.set(&key(b"/created"), b"in-op").unwrap();

        // Two frames left open, as a run that closed neither.
        snapshot.revert_all();

        assert_eq!(snapshot.get(&key(b"/balance")), Some(b"pre-run".to_vec()));
        assert_eq!(snapshot.get(&key(b"/created")), None);
        // Giving up closes the whole stack: nothing is left to close against.
        assert_eq!(snapshot.depth(), 0);
    }

    #[test]
    fn a_bedrock_reverted_run_empties_its_backups() {
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_a()).unwrap();
        live.set(&key(b"/v"), b"pre-run").unwrap();

        let mut snapshot = open_over(&mut host, live);
        snapshot.set(&key(b"/v"), b"during").unwrap();
        snapshot.checkpoint(&mut host).unwrap();
        snapshot.revert_all();
        // The revert emptied the frames, so the drop only has the bedrock
        // left to clear.
        drop(snapshot);

        assert_eq!(read_backup(&mut host, bedrock_a(), b"/v"), None);
        assert_eq!(read_backup(&mut host, frame_a(0), b"/v"), None);
        assert_eq!(read_backup(&mut host, frame_a(1), b"/v"), None);

        // The next run starts over from the reverted live state.
        let live = host.load_or_create(root_a()).unwrap();
        let snapshot =
            SnapshottedKeySpace::start(&mut host, live, PreviousRun::Complete).unwrap();
        assert_eq!(snapshot.depth(), 0);
        assert_eq!(snapshot.get(&key(b"/v")), Some(b"pre-run".to_vec()));
    }

    #[test]
    fn a_reverted_run_drops_its_marker() {
        let mut host = MockKernelHost::default();
        let mut live = host.load_or_create(root_a()).unwrap();
        live.set(&key(b"/v"), b"pre-run").unwrap();

        let mut snapshot = open_over(&mut host, live);
        snapshot.set(&key(b"/v"), b"during").unwrap();
        snapshot.create_reboot_marker().unwrap();
        snapshot.revert_all();
        drop(snapshot);

        // The marker went with the revert, so the next start begins fresh
        // rather than resuming the given-up run.
        let live = host.load_or_create(root_a()).unwrap();
        let snapshot =
            SnapshottedKeySpace::start(&mut host, live, PreviousRun::Complete).unwrap();
        assert_eq!(snapshot.depth(), 0);
        assert_eq!(snapshot.get(&key(b"/v")), Some(b"pre-run".to_vec()));
    }
}
