// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! [`RuntimeKeyspaces`]: the storage handle threaded through kernel execution.
//!
//! It borrows the host and holds the keyspaces the execution reads and
//! writes, and lends them out one at a time. [`RuntimeKeyspaces::init`]
//! builds it over the live host.
//!
//! The keyspaces are `/evm/eth_accounts` and `/tez/tez_accounts`, the account
//! state of the two runtimes, and `/evm/world_state` and `/tez/world_state`,
//! their chain-level state. `/common_world_state` holds the chain-level state
//! that the runtimes share.
//!
//! `trace` and `__http_trace` are reserved children of `/common_world_state`:
//! they hold the node-local output of `debug_trace*` and `http_trace*`, and
//! only exist on the throwaway states of a replay or a simulation.
//!
//! [`RuntimeKeyspaces::start_safe`] gives a safe copy of a handle: a handle
//! of its own, over copies of the keyspaces under `/tmp` and the prefix of
//! the handle, whose raw accesses go under that same prefix (see
//! [`RawHost`]). It is closed by [`RuntimeKeyspaces::into_keyspaces`] and
//! [`RuntimeKeyspaces::promote_from`] on the handle it copies, which keep
//! its writes, or by [`RuntimeKeyspaces::revert_safe`], which drops them.
//! Safe copies nest: a copy of a copy is under `/tmp/tmp`.
//!
//! `/tmp` is reserved under every handle: a raw access to a path under `/tmp`
//! lands under the prefix of the next safe copy, which deletes it when it
//! starts or closes. `/__savepoint` is reserved too: it holds the backups of
//! the savepoints of the handle.
//!
//! [`BlueprintKeyspaces`] is the handle of the stages before block production:
//! it holds `/base`, `/tez/world_state`, `/evm/world_state`,
//! `/common_world_state` and `/kernel`, loaded on the live host, and
//! [`BlueprintKeyspaces::into_base`] keeps only `/base` once the execution
//! loads a [`RuntimeKeyspaces`].

use tezos_smart_rollup_host::path::{concat, OwnedPath, RefPath};
use tezos_smart_rollup_host::runtime::RuntimeError;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_keyspace::{KeySpace, KeySpaceLoader, KeySpaceLoaderError, Name};

use crate::raw_host::{prefixed_name, RawHost};
use crate::runtime::MockKernelHost;
use crate::snapshot::{Snapshot, SnapshotError};

/// The prefix a safe copy adds to the prefix of the handle it copies.
pub(crate) const TMP: &str = "/tmp";

/// [`TMP`] as a path.
pub const TMP_PATH: RefPath = RefPath::assert_from(TMP.as_bytes());

/// Name of the `/base` keyspace, holding kernel configuration and
/// node-interaction values that do not belong to any world state.
pub const BASE_KEYSPACE_NAME: Name = Name::from_static("/base");

/// Name of the `/kernel` keyspace, which holds the kernel that the rollup
/// boots. The reboot flag that the host writes at `/kernel/env/reboot` lies
/// under it too.
const KERNEL_KEYSPACE_NAME: Name = Name::from_static("/kernel");

/// Durable root of the `/evm/eth_accounts` keyspace.
const ETH_ACCOUNTS_ROOT: &str = "/evm/eth_accounts";

/// Name of the `/evm/eth_accounts` keyspace, holding the EVM runtime's
/// account state.
pub const ETH_ACCOUNTS_KEYSPACE_NAME: Name = Name::from_static(ETH_ACCOUNTS_ROOT);

/// Durable root of the `/tez/tez_accounts` keyspace.
const TEZ_ACCOUNTS_ROOT: &str = "/tez/tez_accounts";

/// Name of the `/tez/tez_accounts` keyspace, which holds the account state of
/// the Michelson runtime.
pub const TEZ_ACCOUNTS_KEYSPACE_NAME: Name = Name::from_static(TEZ_ACCOUNTS_ROOT);

/// The durable path of [`TEZ_ACCOUNTS_KEYSPACE_NAME`], for the raw accesses
/// to the big maps, which have no keyspace yet.
pub const TEZ_ACCOUNTS_ROOT_PATH: RefPath =
    RefPath::assert_from(TEZ_ACCOUNTS_ROOT.as_bytes());

/// Durable root of the `/evm/world_state` keyspace.
const EVM_WORLD_STATE_ROOT: &str = "/evm/world_state";

/// Name of the `/evm/world_state` keyspace, which holds the chain-level state
/// of the EVM runtime.
pub const EVM_WORLD_STATE_KEYSPACE_NAME: Name = Name::from_static(EVM_WORLD_STATE_ROOT);

/// The durable path of [`EVM_WORLD_STATE_KEYSPACE_NAME`], for the raw
/// accesses of the storage migrations.
pub const EVM_WORLD_STATE_ROOT_PATH: RefPath =
    RefPath::assert_from(EVM_WORLD_STATE_ROOT.as_bytes());

/// Durable root of the `/tez/world_state` keyspace.
const TEZ_WORLD_STATE_ROOT: &str = "/tez/world_state";

/// Name of the `/tez/world_state` keyspace, which holds the chain-level state
/// of the Michelson runtime.
pub const TEZ_WORLD_STATE_KEYSPACE_NAME: Name = Name::from_static(TEZ_WORLD_STATE_ROOT);

/// Durable root of the `/common_world_state` keyspace.
const COMMON_WORLD_STATE_ROOT: &str = "/common_world_state";

/// Name of the `/common_world_state` keyspace, which holds the chain-level
/// state that the runtimes share.
pub const COMMON_WORLD_STATE_KEYSPACE_NAME: Name =
    Name::from_static(COMMON_WORLD_STATE_ROOT);

/// Storage handle threaded through kernel execution.
///
/// The host is borrowed for `'host`: its owner keeps it, and gets it back
/// once the handle is gone. A handle built by [`Self::init`] has no prefix;
/// a safe copy has `/tmp` and the prefix of the handle it copies.
pub struct RuntimeKeyspaces<'host, Host: KeySpaceLoader> {
    host: RawHost<&'host mut Host>,
    keyspaces: Keyspaces<Host::KeySpace>,
    /// The snapshot of `/tez/tez_accounts`, on which the Michelson journal
    /// opens, releases and rolls back the savepoints of its checkpoints.
    tez_accounts_snapshot: Snapshot<Host::KeySpace>,
}

impl<'host, Host: KeySpaceLoader> RuntimeKeyspaces<'host, Host> {
    /// The `/evm/eth_accounts` keyspace.
    pub fn eth_accounts(&self) -> &Host::KeySpace {
        &self.keyspaces.eth_accounts
    }

    /// The `/evm/eth_accounts` keyspace, for the writers.
    pub fn eth_accounts_mut(&mut self) -> &mut Host::KeySpace {
        &mut self.keyspaces.eth_accounts
    }

    /// Returns the `/tez/tez_accounts` keyspace.
    pub fn tez_accounts(&self) -> &Host::KeySpace {
        &self.keyspaces.tez_accounts
    }

    /// Returns the `/tez/tez_accounts` keyspace for writing.
    pub fn tez_accounts_mut(&mut self) -> &mut Host::KeySpace {
        &mut self.keyspaces.tez_accounts
    }

    /// Releases every savepoint of `/tez/tez_accounts`: deletes the backups
    /// its snapshot still holds, keeping the live writes. The next savepoint
    /// starts the stack again at index 0.
    ///
    /// Reserved for the Michelson journal, as [`Self::tez_accounts_savepoint`].
    pub fn tez_accounts_release_all(&mut self) {
        self.tez_accounts_snapshot.commit()
    }

    /// The snapshot of `/tez/tez_accounts`.
    pub fn tez_accounts_snapshot(&self) -> &Snapshot<Host::KeySpace> {
        &self.tez_accounts_snapshot
    }

    /// Opens a savepoint of `/tez/tez_accounts` on its snapshot, loading its
    /// backup under the prefix of the handle: the backup of a safe copy is
    /// deleted with the copy.
    ///
    /// Fails with [`SnapshotError::Loader`], and opens no savepoint, when the
    /// host cannot load the backup keyspace, e.g. when its name exceeds the
    /// keyspace name size limit.
    ///
    /// Reserved for the Michelson journal, like the other functions that open,
    /// release or roll back the savepoints of this snapshot: the journal
    /// counts the savepoints open across the snapshots it drives, and derives
    /// its checkpoint indices and frame watermarks from that count. A
    /// savepoint opened, released or rolled back here by anything else
    /// leaves the count wrong.
    // TODO: L2-2253
    // Keep the count with the snapshot, so that nothing has to be in sync.
    pub fn tez_accounts_savepoint(&mut self) -> Result<(), SnapshotError> {
        self.tez_accounts_snapshot
            .savepoint(&mut self.host, &self.keyspaces.tez_accounts)
    }

    /// Releases the savepoints of `/tez/tez_accounts` above the `height`
    /// oldest of its snapshot, keeping the live writes.
    ///
    /// Fails with [`SnapshotError::ReleaseAboveHeight`], and releases
    /// nothing, when fewer than `height` savepoints are open.
    ///
    /// Reserved for the Michelson journal, as [`Self::tez_accounts_savepoint`].
    pub fn tez_accounts_release_to(
        &mut self,
        height: usize,
    ) -> Result<(), SnapshotError> {
        self.tez_accounts_snapshot.release_to(height)
    }

    /// Restores `/tez/tez_accounts` from the top savepoint of its snapshot,
    /// and releases it.
    ///
    /// Fails with [`SnapshotError::NoSavepoint`], and leaves
    /// `/tez/tez_accounts` unchanged, when no savepoint is open.
    ///
    /// Reserved for the Michelson journal, as [`Self::tez_accounts_savepoint`].
    pub fn tez_accounts_rollback(&mut self) -> Result<(), SnapshotError> {
        self.tez_accounts_snapshot
            .rollback(&mut self.keyspaces.tez_accounts)
    }

    /// Restores `/tez/tez_accounts` from the top savepoint of its snapshot,
    /// keeping it for a later restore.
    ///
    /// Fails with [`SnapshotError::NoSavepoint`], and leaves
    /// `/tez/tez_accounts` unchanged, when no savepoint is open.
    ///
    /// Reserved for the Michelson journal, as [`Self::tez_accounts_savepoint`].
    pub fn tez_accounts_restore_top(&mut self) -> Result<(), SnapshotError> {
        self.tez_accounts_snapshot
            .restore_top(&mut self.keyspaces.tez_accounts)
    }

    /// Returns the `/evm/world_state` keyspace.
    pub fn evm_world_state(&self) -> &Host::KeySpace {
        &self.keyspaces.evm_world_state
    }

    /// Returns the `/evm/world_state` keyspace for writing.
    pub fn evm_world_state_mut(&mut self) -> &mut Host::KeySpace {
        &mut self.keyspaces.evm_world_state
    }

    /// Returns the `/tez/world_state` keyspace.
    pub fn tez_world_state(&self) -> &Host::KeySpace {
        &self.keyspaces.tez_world_state
    }

    /// Returns the `/tez/world_state` keyspace for writing.
    pub fn tez_world_state_mut(&mut self) -> &mut Host::KeySpace {
        &mut self.keyspaces.tez_world_state
    }

    /// Returns the `/common_world_state` keyspace.
    pub fn common_world_state(&self) -> &Host::KeySpace {
        &self.keyspaces.common_world_state
    }

    /// Returns the `/common_world_state` keyspace for writing.
    pub fn common_world_state_mut(&mut self) -> &mut Host::KeySpace {
        &mut self.keyspaces.common_world_state
    }

    /// The host, for the durable accesses that have no keyspace yet.
    pub fn host(&self) -> &RawHost<&'host mut Host> {
        &self.host
    }

    /// The host, for the durable accesses that have no keyspace yet.
    pub fn host_mut(&mut self) -> &mut RawHost<&'host mut Host> {
        &mut self.host
    }

    /// Drops the handle and gives the host back.
    pub fn into_host(self) -> &'host mut Host {
        self.host.host
    }

    /// Loads the keyspaces from `host`.
    ///
    /// Errors with the loader's error: a keyspace already held elsewhere, or
    /// a name the loader cannot accept.
    pub fn init(host: &'host mut Host) -> Result<Self, KeySpaceLoaderError> {
        let keyspaces = Keyspaces::load(|name| host.load_or_create(name))?;
        Ok(Self {
            host: RawHost { host, prefix: None },
            keyspaces,
            tez_accounts_snapshot: Snapshot::new(),
        })
    }

    /// The prefix of a safe copy of `self`: `/tmp` and `self`'s prefix.
    ///
    /// Errors with [`RuntimeError::PathNotFound`] if it exceeds the path size
    /// limit.
    fn copy_prefix(&self) -> Result<OwnedPath, RuntimeError> {
        copy_prefix_of(self.host.prefix.as_ref())
    }

    /// Loads the keyspaces of a safe copy of `self`, named after
    /// [`Self::copy_prefix`], with that prefix.
    fn load_copy(
        &mut self,
    ) -> Result<(Keyspaces<Host::KeySpace>, OwnedPath), SafeCopyError> {
        let prefix = self.copy_prefix()?;
        let host = &mut *self.host.host;
        let keyspaces =
            Keyspaces::load(|name| host.load_or_create(prefixed_name(&prefix, &name)?))?;
        Ok((keyspaces, prefix))
    }

    /// The safe copy of `self` over `keyspaces`, under `prefix`.
    fn copy_over(
        &mut self,
        keyspaces: Keyspaces<Host::KeySpace>,
        prefix: OwnedPath,
    ) -> RuntimeKeyspaces<'_, Host> {
        RuntimeKeyspaces {
            host: RawHost {
                host: &mut *self.host.host,
                prefix: Some(prefix),
            },
            keyspaces,
            tez_accounts_snapshot: Snapshot::new(),
        }
    }

    /// Errors with [`SafeCopyError::SavepointsOpen`] if a savepoint of
    /// `/tez/tez_accounts` is open.
    fn check_no_savepoint(&self) -> Result<(), SafeCopyError> {
        match self.tez_accounts_snapshot.len() {
            0 => Ok(()),
            open => Err(SafeCopyError::SavepointsOpen(open)),
        }
    }

    /// The keyspaces of a safe copy, ending its borrow of the handle it
    /// copies, for [`Self::promote_from`].
    ///
    /// Errors with [`SafeCopyError::SavepointsOpen`] if a savepoint of
    /// `/tez/tez_accounts` is open: the copy is dropped with its savepoints.
    pub fn into_keyspaces(self) -> Result<Keyspaces<Host::KeySpace>, SafeCopyError> {
        self.check_no_savepoint()?;
        Ok(self.keyspaces)
    }
}

impl<Host: KeySpaceLoader + StorageV1> RuntimeKeyspaces<'_, Host> {
    /// A safe copy of `self`, with no side effect on it: loads the keyspaces
    /// under `/tmp` and `self`'s prefix, deletes what an earlier copy left
    /// there, then copies `self`'s keyspaces into them. The copy's snapshot
    /// of `/tez/tez_accounts` holds no savepoint, and `self`'s cannot be
    /// reached until the copy is gone.
    ///
    /// The copy must be closed by [`Self::into_keyspaces`] and
    /// [`Self::promote_from`], once its savepoints are released through the
    /// journal, or by [`Self::revert_safe`].
    ///
    /// Errors with [`SafeCopyError::Loader`], writing nothing, or with
    /// [`SafeCopyError::Storage`] if the deletion fails.
    pub fn start_safe(&mut self) -> Result<RuntimeKeyspaces<'_, Host>, SafeCopyError> {
        let (mut keyspaces, prefix) = self.load_copy()?;
        delete_if_present(self.host.host, &prefix)?;
        keyspaces.zip_mut(&mut self.keyspaces, |copy, keyspace| {
            copy.copy_from(keyspace)
        });
        Ok(self.copy_over(keyspaces, prefix))
    }

    /// The safe copy of `self` that a dropped handle left, e.g. on a kernel
    /// reboot, without deleting or copying anything. Closed as the copy of
    /// [`Self::start_safe`] is.
    ///
    /// Errors with [`SafeCopyError::NoCopy`] if nothing is under the prefix
    /// of a safe copy of `self`, e.g. no copy was started or it was closed:
    /// promoting empty keyspaces would clear `self`'s. A copy that holds no
    /// key cannot be told apart from no copy: the caller must have written
    /// to the copy it resumes, e.g. the block in progress. Errors with
    /// [`SafeCopyError::Loader`] or [`SafeCopyError::Storage`].
    pub fn resume_safe(&mut self) -> Result<RuntimeKeyspaces<'_, Host>, SafeCopyError> {
        if self.host.host.store_has(&self.copy_prefix()?)?.is_none() {
            return Err(SafeCopyError::NoCopy);
        }
        let (keyspaces, prefix) = self.load_copy()?;
        Ok(self.copy_over(keyspaces, prefix))
    }

    /// Moves `keyspaces` over `self`'s, then deletes what is left under the
    /// prefix of a safe copy of `self`: what the copy wrote outside its
    /// keyspaces.
    ///
    /// `keyspaces` may come from any safe copy, not strictly only from
    /// a direct copy of `self`.
    ///
    /// Errors with [`SafeCopyError::Storage`] if the deletion fails, the
    /// keyspaces having been moved.
    pub fn promote_from(
        &mut self,
        mut keyspaces: Keyspaces<Host::KeySpace>,
    ) -> Result<(), SafeCopyError> {
        let prefix = self.copy_prefix()?;
        self.keyspaces
            .zip_mut(&mut keyspaces, |keyspace, copy| keyspace.move_from(copy));
        drop(keyspaces);
        delete_if_present(self.host.host, &prefix)?;
        Ok(())
    }

    /// Releases the savepoints of `/tez/tez_accounts` still open, clears the
    /// keyspaces of a safe copy and deletes what is left under its prefix.
    ///
    /// The release bypasses the Michelson journal: the caller must drop or
    /// reset the journal that counts these savepoints.
    ///
    /// Errors with [`SafeCopyError::NotACopy`] if `self` has no prefix: the
    /// handle is dropped and nothing is released, cleared or deleted. Errors
    /// with [`SafeCopyError::Storage`] if the deletion fails.
    pub fn revert_safe(mut self) -> Result<(), SafeCopyError> {
        let Some(prefix) = self.host.prefix.take() else {
            return Err(SafeCopyError::NotACopy);
        };
        self.tez_accounts_snapshot.commit();
        self.keyspaces.for_each_mut(|copy| copy.clear());
        drop(self.keyspaces);
        delete_if_present(self.host.host, &prefix)?;
        Ok(())
    }
}

/// Why a safe copy cannot be started, resumed, promoted or reverted.
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum SafeCopyError {
    #[error("safe copy keyspace loader error: {0}")]
    Loader(#[from] KeySpaceLoaderError),
    #[error("safe copy storage error: {0}")]
    Storage(#[from] RuntimeError),
    #[error("safe copy promoted with {0} savepoints open")]
    SavepointsOpen(usize),
    #[error("reverted handle is not a safe copy")]
    NotACopy,
    #[error("no safe copy to resume")]
    NoCopy,
}

/// Deletes `path` from `host`, if present.
fn delete_if_present(
    host: &mut impl StorageV1,
    path: &OwnedPath,
) -> Result<(), RuntimeError> {
    if host.store_has(path)?.is_none() {
        return Ok(());
    }
    host.store_delete(path)
}

/// The prefix of a safe copy of a handle with `prefix`: `/tmp` and `prefix`.
///
/// Errors with [`RuntimeError::PathNotFound`] if it exceeds the path size
/// limit.
fn copy_prefix_of(prefix: Option<&OwnedPath>) -> Result<OwnedPath, RuntimeError> {
    match prefix {
        None => Ok(OwnedPath::from(TMP_PATH)),
        Some(prefix) => concat(&TMP_PATH, prefix).map_err(|_| RuntimeError::PathNotFound),
    }
}

/// Loads the copy of the keyspace `name` that a safe copy of a handle built
/// by [`RuntimeKeyspaces::init`] on `host` holds, e.g. to read what a dropped
/// copy left on a kernel reboot. The keyspace is empty when no copy holds it.
///
/// Errors with [`SafeCopyError::Loader`] with the loader's error: a keyspace
/// already held elsewhere, or a name the loader cannot accept.
pub fn load_in_safe_copy<Host: KeySpaceLoader>(
    host: &mut Host,
    name: Name,
) -> Result<Host::KeySpace, SafeCopyError> {
    let prefix = copy_prefix_of(None)?;
    Ok(host.load_or_create(prefixed_name(&prefix, &name)?)?)
}

/// The runtime keyspaces.
pub struct Keyspaces<KS> {
    eth_accounts: KS,
    tez_accounts: KS,
    evm_world_state: KS,
    tez_world_state: KS,
    common_world_state: KS,
}

impl<KS> Keyspaces<KS> {
    /// Loads each keyspace with `load`, by its name.
    fn load<E>(mut load: impl FnMut(Name) -> Result<KS, E>) -> Result<Self, E> {
        Ok(Self {
            eth_accounts: load(ETH_ACCOUNTS_KEYSPACE_NAME)?,
            tez_accounts: load(TEZ_ACCOUNTS_KEYSPACE_NAME)?,
            evm_world_state: load(EVM_WORLD_STATE_KEYSPACE_NAME)?,
            tez_world_state: load(TEZ_WORLD_STATE_KEYSPACE_NAME)?,
            common_world_state: load(COMMON_WORLD_STATE_KEYSPACE_NAME)?,
        })
    }

    /// Gives each keyspace to `f`.
    fn for_each_mut(&mut self, mut f: impl FnMut(&mut KS)) {
        f(&mut self.eth_accounts);
        f(&mut self.tez_accounts);
        f(&mut self.evm_world_state);
        f(&mut self.tez_world_state);
        f(&mut self.common_world_state);
    }

    /// Gives each keyspace to `f` with the same keyspace of `other`.
    fn zip_mut(&mut self, other: &mut Self, mut f: impl FnMut(&mut KS, &mut KS)) {
        f(&mut self.eth_accounts, &mut other.eth_accounts);
        f(&mut self.tez_accounts, &mut other.tez_accounts);
        f(&mut self.evm_world_state, &mut other.evm_world_state);
        f(&mut self.tez_world_state, &mut other.tez_world_state);
        f(&mut self.common_world_state, &mut other.common_world_state);
    }
}

/// The handle over a borrowed [`MockKernelHost`], for the tests.
pub type MockRuntimeKeyspaces<'host> = RuntimeKeyspaces<'host, MockKernelHost>;

/// The keyspaces the kernel holds from the inbox to the blueprint, before
/// block production: `/base`, `/tez/world_state`, `/evm/world_state`,
/// `/common_world_state` and `/kernel`.
///
/// The handles address the durable roots, not the safe copy of a block.
/// The kernel does not write through the world states while a safe copy is
/// open, and calls [`Self::into_base`] before it loads a
/// [`RuntimeKeyspaces`] on the same host, which holds the world states again.
pub struct BlueprintKeyspaces<KS> {
    base: KS,
    tez_world_state: KS,
    evm_world_state: KS,
    common_world_state: KS,
    kernel: KS,
}

impl<KS: KeySpace> BlueprintKeyspaces<KS> {
    /// Loads `/base`, `/tez/world_state`, `/evm/world_state`,
    /// `/common_world_state` then `/kernel` from `host`.
    ///
    /// Errors with the loader's error: a keyspace already held elsewhere, or
    /// a name the loader cannot accept.
    pub fn load<Host: KeySpaceLoader<KeySpace = KS>>(
        host: &mut Host,
    ) -> Result<Self, KeySpaceLoaderError> {
        let base = host.load_or_create(BASE_KEYSPACE_NAME)?;
        let tez_world_state = host.load_or_create(TEZ_WORLD_STATE_KEYSPACE_NAME)?;
        let evm_world_state = host.load_or_create(EVM_WORLD_STATE_KEYSPACE_NAME)?;
        let common_world_state = host.load_or_create(COMMON_WORLD_STATE_KEYSPACE_NAME)?;
        let kernel = host.load_or_create(KERNEL_KEYSPACE_NAME)?;
        Ok(Self {
            base,
            tez_world_state,
            evm_world_state,
            common_world_state,
            kernel,
        })
    }

    /// The `/base` keyspace.
    pub fn base(&self) -> &KS {
        &self.base
    }

    /// The `/base` keyspace, for the writers.
    pub fn base_mut(&mut self) -> &mut KS {
        &mut self.base
    }

    /// The `/tez/world_state` keyspace.
    pub fn tez_world_state(&self) -> &KS {
        &self.tez_world_state
    }

    /// The `/tez/world_state` keyspace, for the writers.
    pub fn tez_world_state_mut(&mut self) -> &mut KS {
        &mut self.tez_world_state
    }

    /// The `/evm/world_state` keyspace.
    pub fn evm_world_state(&self) -> &KS {
        &self.evm_world_state
    }

    /// The `/evm/world_state` keyspace, for the writers.
    pub fn evm_world_state_mut(&mut self) -> &mut KS {
        &mut self.evm_world_state
    }

    /// The `/common_world_state` keyspace.
    pub fn common_world_state(&self) -> &KS {
        &self.common_world_state
    }

    /// The `/base` and `/kernel` keyspaces, for a writer that needs both at
    /// once.
    pub fn base_and_kernel_mut(&mut self) -> (&mut KS, &mut KS) {
        (&mut self.base, &mut self.kernel)
    }

    /// Keeps `/base` and drops the other keyspaces, so that a
    /// [`RuntimeKeyspaces::init`] on the same host can load the world states
    /// again.
    pub fn into_base(self) -> KS {
        self.base
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_smart_rollup_keyspace::Key;

    const PROBE: Key = Key::from_static(b"/probe");

    #[test]
    fn load_holds_the_live_roots_until_into_base() {
        let mut host = MockKernelHost::default();
        let mut bk = BlueprintKeyspaces::load(&mut host).unwrap();
        assert_eq!(bk.base().name().to_string(), "/base");
        assert_eq!(bk.tez_world_state().name().to_string(), "/tez/world_state");
        assert_eq!(bk.evm_world_state().name().to_string(), "/evm/world_state");
        assert_eq!(
            bk.common_world_state().name().to_string(),
            "/common_world_state"
        );
        assert_eq!(bk.base_and_kernel_mut().1.name().to_string(), "/kernel");
        bk.tez_world_state_mut().set(&PROBE, b"live").unwrap();
        assert_eq!(
            host.store_read_all(&RefPath::assert_from(b"/tez/world_state/probe"))
                .unwrap(),
            b"live"
        );
        bk.evm_world_state_mut().set(&PROBE, b"live").unwrap();
        assert_eq!(
            host.store_read_all(&RefPath::assert_from(b"/evm/world_state/probe"))
                .unwrap(),
            b"live"
        );
        // The world states and `/kernel` are still held: they cannot be
        // loaded again.
        for name in [
            TEZ_WORLD_STATE_KEYSPACE_NAME,
            EVM_WORLD_STATE_KEYSPACE_NAME,
            COMMON_WORLD_STATE_KEYSPACE_NAME,
            KERNEL_KEYSPACE_NAME,
        ] {
            assert!(matches!(
                host.load_or_create(name),
                Err(KeySpaceLoaderError::AlreadyLoaded)
            ));
        }
        let base = bk.into_base();
        let rk = RuntimeKeyspaces::init(&mut host).unwrap();
        assert_eq!(
            rk.tez_world_state().get(&PROBE).as_deref(),
            Some(&b"live"[..])
        );
        assert_eq!(
            rk.evm_world_state().get(&PROBE).as_deref(),
            Some(&b"live"[..])
        );
        assert_eq!(base.name().to_string(), "/base");
    }

    #[test]
    fn tez_accounts_snapshot_starts_empty() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        assert_eq!(rk.tez_accounts_snapshot().len(), 0);
        assert_eq!(rk.tez_accounts_release_to(0), Ok(()));
        assert_eq!(
            rk.tez_accounts_release_to(1),
            Err(SnapshotError::ReleaseAboveHeight { height: 1, len: 0 })
        );
        assert_eq!(rk.tez_accounts_rollback(), Err(SnapshotError::NoSavepoint));
        assert_eq!(
            rk.tez_accounts_restore_top(),
            Err(SnapshotError::NoSavepoint)
        );
    }

    #[test]
    fn tez_accounts_snapshot_restores_and_commit_keeps_the_writes() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        rk.tez_accounts_mut().set(&PROBE, b"v0").unwrap();
        rk.tez_accounts_savepoint().unwrap();
        rk.tez_accounts_mut().set(&PROBE, b"v1").unwrap();
        rk.tez_accounts_restore_top().unwrap();
        assert_eq!(rk.tez_accounts().get(&PROBE).as_deref(), Some(&b"v0"[..]));

        rk.tez_accounts_mut().set(&PROBE, b"v2").unwrap();
        rk.tez_accounts_savepoint().unwrap();
        rk.tez_accounts_mut().set(&PROBE, b"v3").unwrap();
        rk.tez_accounts_rollback().unwrap();
        assert_eq!(rk.tez_accounts().get(&PROBE).as_deref(), Some(&b"v2"[..]));
        assert_eq!(rk.tez_accounts_snapshot().len(), 1);

        rk.tez_accounts_mut().set(&PROBE, b"v4").unwrap();
        rk.tez_accounts_release_all();
        assert_eq!(rk.tez_accounts().get(&PROBE).as_deref(), Some(&b"v4"[..]));
        assert_eq!(rk.tez_accounts_snapshot().len(), 0);
        // Releasing every savepoint dropped the backup still held.
        let backup = RefPath::assert_from(b"/__savepoint/0/tez/tez_accounts/probe");
        assert!(rk.host().store_has(&backup).unwrap().is_none());
        // And the stack starts again at index 0.
        rk.tez_accounts_savepoint().unwrap();
        assert_eq!(rk.host().store_read_all(&backup).unwrap(), b"v4");
    }

    // The safe-copy tests read keyspaces only through keyspace handles: under
    // the NDS backend, a keyspace is not at the durable path of its name.

    /// The number of runtime keyspaces.
    const KEYSPACES: usize = 5;

    const LIVE_NAMES: [&str; KEYSPACES] = [
        "/evm/eth_accounts",
        "/tez/tez_accounts",
        "/evm/world_state",
        "/tez/world_state",
        "/common_world_state",
    ];

    const TMP_NAMES: [&str; KEYSPACES] = [
        "/tmp/evm/eth_accounts",
        "/tmp/tez/tez_accounts",
        "/tmp/evm/world_state",
        "/tmp/tez/world_state",
        "/tmp/common_world_state",
    ];

    const TMP_TMP_NAMES: [&str; KEYSPACES] = [
        "/tmp/tmp/evm/eth_accounts",
        "/tmp/tmp/tez/tez_accounts",
        "/tmp/tmp/evm/world_state",
        "/tmp/tmp/tez/world_state",
        "/tmp/tmp/common_world_state",
    ];

    const NONE: [Option<Vec<u8>>; KEYSPACES] = [const { None }; KEYSPACES];

    /// The value at the probe of each keyspace of `names`, loaded on `loader`.
    fn probes_in(
        loader: &mut impl KeySpaceLoader,
        names: [&str; KEYSPACES],
    ) -> [Option<Vec<u8>>; KEYSPACES] {
        names.map(|name| {
            let keyspace = loader
                .load_or_create(Name::try_from(name.to_string()).unwrap())
                .unwrap();
            keyspace.get(&PROBE).map(|value| value.to_vec())
        })
    }

    /// A host whose keyspaces hold `before` at the probe.
    fn seeded_host() -> MockKernelHost {
        let mut host = MockKernelHost::default();
        set_probes(&mut RuntimeKeyspaces::init(&mut host).unwrap(), b"before");
        host
    }

    /// The name of each keyspace of `rk`.
    fn names<Host: KeySpaceLoader>(
        rk: &RuntimeKeyspaces<'_, Host>,
    ) -> [String; KEYSPACES] {
        [
            rk.eth_accounts().name().to_string(),
            rk.tez_accounts().name().to_string(),
            rk.evm_world_state().name().to_string(),
            rk.tez_world_state().name().to_string(),
            rk.common_world_state().name().to_string(),
        ]
    }

    /// Writes `value` at the probe of each keyspace of `rk`.
    fn set_probes<Host: KeySpaceLoader>(
        rk: &mut RuntimeKeyspaces<'_, Host>,
        value: &[u8],
    ) {
        rk.eth_accounts_mut().set(&PROBE, value).unwrap();
        rk.tez_accounts_mut().set(&PROBE, value).unwrap();
        rk.evm_world_state_mut().set(&PROBE, value).unwrap();
        rk.tez_world_state_mut().set(&PROBE, value).unwrap();
        rk.common_world_state_mut().set(&PROBE, value).unwrap();
    }

    /// The value at the probe of each keyspace of `rk`.
    fn get_probes<Host: KeySpaceLoader>(
        rk: &RuntimeKeyspaces<'_, Host>,
    ) -> [Option<Vec<u8>>; KEYSPACES] {
        [
            rk.eth_accounts().get(&PROBE),
            rk.tez_accounts().get(&PROBE),
            rk.evm_world_state().get(&PROBE),
            rk.tez_world_state().get(&PROBE),
            rk.common_world_state().get(&PROBE),
        ]
        .map(|value| value.map(|value| value.to_vec()))
    }

    fn all(value: &[u8]) -> [Option<Vec<u8>>; KEYSPACES] {
        [(); KEYSPACES].map(|()| Some(value.to_vec()))
    }

    #[test]
    fn start_safe_copies_every_keyspace_under_tmp() {
        let mut host = seeded_host();
        {
            let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
            let mut copy = rk.start_safe().unwrap();
            assert_eq!(names(&copy), TMP_NAMES);
            assert_eq!(get_probes(&copy), all(b"before"));
            set_probes(&mut copy, b"after");
            let inner = copy.start_safe().unwrap();
            assert_eq!(names(&inner), TMP_TMP_NAMES);
            assert_eq!(get_probes(&inner), all(b"after"));
        }
        // Dropped unclosed: the writes stay in the copy.
        assert_eq!(probes_in(&mut host, LIVE_NAMES), all(b"before"));
        assert_eq!(probes_in(&mut host, TMP_NAMES), all(b"after"));
        assert_eq!(probes_in(&mut host, TMP_TMP_NAMES), all(b"after"));
    }

    #[test]
    fn promote_from_moves_the_writes_to_the_parent() {
        let mut host = seeded_host();
        {
            let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
            let mut copy = rk.start_safe().unwrap();
            set_probes(&mut copy, b"after");
            let keyspaces = copy.into_keyspaces().unwrap();
            rk.promote_from(keyspaces).unwrap();
            assert_eq!(get_probes(&rk), all(b"after"));
        }
        assert_eq!(probes_in(&mut host, LIVE_NAMES), all(b"after"));
        assert_eq!(probes_in(&mut host, TMP_NAMES), NONE);
        assert_eq!(host.store_has(&TMP_PATH).unwrap(), None);
    }

    #[test]
    fn revert_safe_leaves_the_parent_untouched() {
        let mut host = seeded_host();
        {
            let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
            let mut copy = rk.start_safe().unwrap();
            set_probes(&mut copy, b"after");
            copy.revert_safe().unwrap();
            assert_eq!(get_probes(&rk), all(b"before"));
        }
        assert_eq!(probes_in(&mut host, LIVE_NAMES), all(b"before"));
        assert_eq!(probes_in(&mut host, TMP_NAMES), NONE);
        assert_eq!(host.store_has(&TMP_PATH).unwrap(), None);
    }

    #[test]
    fn revert_safe_refuses_a_handle_that_is_not_a_copy() {
        let mut host = seeded_host();
        let rk = RuntimeKeyspaces::init(&mut host).unwrap();
        assert_eq!(rk.revert_safe(), Err(SafeCopyError::NotACopy));
        assert_eq!(probes_in(&mut host, LIVE_NAMES), all(b"before"));
    }

    #[test]
    fn a_reverted_inner_copy_leaves_the_outer_writes() {
        let mut host = seeded_host();
        {
            let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
            let mut outer = rk.start_safe().unwrap();
            set_probes(&mut outer, b"outer");
            let mut inner = outer.start_safe().unwrap();
            assert_eq!(get_probes(&inner), all(b"outer"));
            set_probes(&mut inner, b"inner");
            inner.revert_safe().unwrap();
            assert_eq!(get_probes(&outer), all(b"outer"));
            let keyspaces = outer.into_keyspaces().unwrap();
            rk.promote_from(keyspaces).unwrap();
        }
        assert_eq!(probes_in(&mut host, LIVE_NAMES), all(b"outer"));
        assert_eq!(probes_in(&mut host, TMP_TMP_NAMES), NONE);
        assert_eq!(host.store_has(&TMP_PATH).unwrap(), None);
    }

    #[test]
    fn a_promoted_inner_copy_reaches_live_only_with_the_outer_one() {
        let mut host = seeded_host();
        {
            let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
            let mut outer = rk.start_safe().unwrap();
            let mut inner = outer.start_safe().unwrap();
            set_probes(&mut inner, b"after");
            let keyspaces = inner.into_keyspaces().unwrap();
            outer.promote_from(keyspaces).unwrap();
            assert_eq!(names(&outer), TMP_NAMES);
            assert_eq!(get_probes(&outer), all(b"after"));
        }
        // The outer copy, dropped unclosed, holds the writes; live does not.
        assert_eq!(probes_in(&mut host, LIVE_NAMES), all(b"before"));
        assert_eq!(probes_in(&mut host, TMP_TMP_NAMES), NONE);
        {
            let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
            let outer = rk.resume_safe().unwrap();
            let keyspaces = outer.into_keyspaces().unwrap();
            rk.promote_from(keyspaces).unwrap();
        }
        assert_eq!(probes_in(&mut host, LIVE_NAMES), all(b"after"));
        assert_eq!(host.store_has(&TMP_PATH).unwrap(), None);
    }

    #[test]
    fn the_keyspaces_of_a_grandchild_promote_into_the_root() {
        let mut host = seeded_host();
        {
            let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
            let mut outer = rk.start_safe().unwrap();
            set_probes(&mut outer, b"outer");
            let mut inner = outer.start_safe().unwrap();
            set_probes(&mut inner, b"inner");
            let keyspaces = inner.into_keyspaces().unwrap();
            drop(outer);
            rk.promote_from(keyspaces).unwrap();
            assert_eq!(get_probes(&rk), all(b"inner"));
        }
        assert_eq!(probes_in(&mut host, LIVE_NAMES), all(b"inner"));
        assert_eq!(probes_in(&mut host, TMP_TMP_NAMES), NONE);
        assert_eq!(host.store_has(&TMP_PATH).unwrap(), None);
    }

    #[test]
    fn resume_safe_fails_without_a_copy() {
        let mut host = seeded_host();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        assert_eq!(rk.resume_safe().err(), Some(SafeCopyError::NoCopy));

        let mut copy = rk.start_safe().unwrap();
        set_probes(&mut copy, b"after");
        let keyspaces = copy.into_keyspaces().unwrap();
        rk.promote_from(keyspaces).unwrap();
        assert_eq!(rk.resume_safe().err(), Some(SafeCopyError::NoCopy));

        rk.start_safe().unwrap().revert_safe().unwrap();
        assert_eq!(rk.resume_safe().err(), Some(SafeCopyError::NoCopy));
        assert_eq!(get_probes(&rk), all(b"after"));
    }

    #[test]
    fn resume_safe_reopens_the_copy_of_a_dropped_handle() {
        let mut host = seeded_host();
        {
            let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
            let mut copy = rk.start_safe().unwrap();
            set_probes(&mut copy, b"after");
        }
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        assert_eq!(get_probes(&rk), all(b"before"));
        let copy = rk.resume_safe().unwrap();
        assert_eq!(names(&copy), TMP_NAMES);
        assert_eq!(get_probes(&copy), all(b"after"));
        let keyspaces = copy.into_keyspaces().unwrap();
        rk.promote_from(keyspaces).unwrap();
        assert_eq!(get_probes(&rk), all(b"after"));
        assert_eq!(rk.host().store_has(&TMP_PATH).unwrap(), None);
    }

    /// A raw path outside the keyspaces.
    const RAW: RefPath = RefPath::assert_from(b"/raw");

    /// A key that only a copy holds, e.g. a trace of an interrupted block.
    const STALE: Key = Key::from_static(b"/trace/call_trace/0");

    #[test]
    fn start_safe_deletes_what_a_dropped_copy_left_and_resume_safe_keeps_it() {
        let tmp_raw = RefPath::assert_from(b"/tmp/raw");
        let mut host = seeded_host();
        {
            let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
            let mut copy = rk.start_safe().unwrap();
            copy.host_mut().store_write_all(&RAW, b"left").unwrap();
            set_probes(&mut copy, b"left");
            copy.common_world_state_mut().set(&STALE, b"left").unwrap();
        }
        {
            let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
            let copy = rk.resume_safe().unwrap();
            assert_eq!(copy.host().store_read_all(&RAW).unwrap(), b"left");
            assert_eq!(get_probes(&copy), all(b"left"));
        }
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let copy = rk.start_safe().unwrap();
        assert_eq!(copy.host().host.store_has(&tmp_raw).unwrap(), None);
        assert_eq!(get_probes(&copy), all(b"before"));
        assert_eq!(copy.common_world_state().get(&STALE), None);
    }

    #[test]
    fn start_safe_failing_partway_writes_nothing() {
        let leftover = RefPath::assert_from(b"/tmp/raw");
        let mut host = seeded_host();
        host.store_write_all(&leftover, b"left").unwrap();
        // Held, so loading the copy of `/tez/world_state` fails.
        let held = host
            .load_or_create(Name::from_static("/tmp/tez/world_state"))
            .unwrap();
        {
            let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
            assert_eq!(
                rk.start_safe().err(),
                Some(SafeCopyError::Loader(KeySpaceLoaderError::AlreadyLoaded))
            );
            assert_eq!(rk.host().store_read_all(&leftover).unwrap(), b"left");
        }
        assert_eq!(held.get(&PROBE), None);
        drop(held);
        assert_eq!(probes_in(&mut host, TMP_NAMES), NONE);
    }

    /// The raw accesses of a copy go under its prefix, and are dropped when it
    /// closes: only its keyspaces are promoted.
    #[test]
    fn raw_accesses_follow_the_copy_and_are_not_promoted() {
        let tmp_raw = RefPath::assert_from(b"/tmp/raw");
        let tmp_tmp_raw = RefPath::assert_from(b"/tmp/tmp/raw");
        let mut host = seeded_host();
        host.store_write_all(&RAW, b"live").unwrap();
        {
            let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
            let mut outer = rk.start_safe().unwrap();
            // Only the keyspaces are copied.
            assert_eq!(outer.host().store_has(&RAW).unwrap(), None);
            outer.host_mut().store_write_all(&RAW, b"outer").unwrap();
            set_probes(&mut outer, b"outer");
            assert_eq!(
                outer.host().host.store_read_all(&tmp_raw).unwrap(),
                b"outer"
            );
            assert_eq!(outer.host().host.store_read_all(&RAW).unwrap(), b"live");
            let mut inner = outer.start_safe().unwrap();
            assert_eq!(inner.host().store_has(&RAW).unwrap(), None);
            inner.host_mut().store_write_all(&RAW, b"inner").unwrap();
            assert_eq!(
                inner.host().host.store_read_all(&tmp_tmp_raw).unwrap(),
                b"inner"
            );
            inner.revert_safe().unwrap();
            assert_eq!(outer.host().store_read_all(&RAW).unwrap(), b"outer");
            let keyspaces = outer.into_keyspaces().unwrap();
            rk.promote_from(keyspaces).unwrap();
            assert_eq!(get_probes(&rk), all(b"outer"));
        }
        assert_eq!(host.store_read_all(&RAW).unwrap(), b"live");
        assert_eq!(host.store_has(&TMP_PATH).unwrap(), None);
    }

    #[test]
    fn revert_safe_deletes_the_raw_writes_outside_the_keyspaces() {
        let mut host = seeded_host();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let mut copy = rk.start_safe().unwrap();
        copy.host_mut().store_write_all(&RAW, b"inside").unwrap();
        copy.revert_safe().unwrap();
        assert_eq!(rk.host().store_has(&RAW).unwrap(), None);
        assert_eq!(rk.host().store_has(&TMP_PATH).unwrap(), None);
    }

    #[test]
    fn a_savepoint_of_the_parent_is_usable_again_after_promote() {
        let mut host = seeded_host();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        rk.tez_accounts_savepoint().unwrap();
        rk.tez_accounts_mut().set(&PROBE, b"outer").unwrap();
        let mut copy = rk.start_safe().unwrap();
        assert_eq!(copy.tez_accounts_snapshot().len(), 0);
        assert_eq!(
            copy.tez_accounts_rollback(),
            Err(SnapshotError::NoSavepoint)
        );
        assert_eq!(
            copy.tez_accounts_release_to(1),
            Err(SnapshotError::ReleaseAboveHeight { height: 1, len: 0 })
        );
        copy.tez_accounts_mut().set(&PROBE, b"inner").unwrap();
        let keyspaces = copy.into_keyspaces().unwrap();
        rk.promote_from(keyspaces).unwrap();

        assert_eq!(rk.tez_accounts_snapshot().len(), 1);
        rk.tez_accounts_rollback().unwrap();
        assert_eq!(
            rk.tez_accounts().get(&PROBE).as_deref(),
            Some(&b"before"[..])
        );
    }

    #[test]
    fn the_savepoints_of_a_copy_are_under_tmp() {
        let backup =
            RefPath::assert_from(b"/tmp/__savepoint/0/tmp/tez/tez_accounts/probe");
        let mut host = seeded_host();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let mut copy = rk.start_safe().unwrap();
        copy.tez_accounts_savepoint().unwrap();
        assert_eq!(copy.host().host.store_read_all(&backup).unwrap(), b"before");
        assert_eq!(
            copy.host()
                .host
                .store_has(&RefPath::assert_from(b"/__savepoint"))
                .unwrap(),
            None
        );
        copy.tez_accounts_mut().set(&PROBE, b"after").unwrap();
        copy.tez_accounts_release_to(0).unwrap();
        let keyspaces = copy.into_keyspaces().unwrap();
        rk.promote_from(keyspaces).unwrap();
        assert_eq!(rk.host().store_has(&backup).unwrap(), None);
        assert_eq!(
            rk.tez_accounts().get(&PROBE).as_deref(),
            Some(&b"after"[..])
        );
    }

    #[test]
    fn the_savepoints_of_a_copy_of_a_copy_are_under_its_prefix() {
        let backup = RefPath::assert_from(
            b"/tmp/tmp/__savepoint/0/tmp/tmp/tez/tez_accounts/probe",
        );
        let mut host = seeded_host();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let mut copy = rk.start_safe().unwrap();
        let mut nested = copy.start_safe().unwrap();
        nested.tez_accounts_savepoint().unwrap();
        assert_eq!(
            nested.host().host.store_read_all(&backup).unwrap(),
            b"before"
        );
        nested.tez_accounts_release_to(0).unwrap();
        assert_eq!(nested.host().host.store_has(&backup).unwrap(), None);
    }

    #[test]
    fn load_in_safe_copy_reads_what_a_dropped_copy_left() {
        let mut host = seeded_host();
        {
            let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
            let mut copy = rk.start_safe().unwrap();
            copy.common_world_state_mut().set(&PROBE, b"left").unwrap();
        }
        let left = load_in_safe_copy(&mut host, COMMON_WORLD_STATE_KEYSPACE_NAME)
            .unwrap()
            .get(&PROBE);
        assert_eq!(left.as_deref(), Some(&b"left"[..]));
    }

    #[test]
    fn promoting_a_copy_with_a_savepoint_open_fails() {
        let mut host = seeded_host();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let mut copy = rk.start_safe().unwrap();
        copy.tez_accounts_savepoint().unwrap();
        assert_eq!(
            copy.into_keyspaces().err(),
            Some(SafeCopyError::SavepointsOpen(1))
        );
    }

    #[test]
    fn revert_safe_releases_the_savepoints_left_open() {
        let backups = RefPath::assert_from(b"/tmp/__savepoint");
        let mut host = seeded_host();
        {
            let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
            let mut copy = rk.start_safe().unwrap();
            copy.tez_accounts_savepoint().unwrap();
            copy.tez_accounts_mut().set(&PROBE, b"inside").unwrap();
            copy.tez_accounts_savepoint().unwrap();
            assert_eq!(copy.tez_accounts_snapshot().len(), 2);
            assert!(copy.host().host.store_has(&backups).unwrap().is_some());
            copy.revert_safe().unwrap();
            assert_eq!(
                rk.tez_accounts().get(&PROBE).as_deref(),
                Some(&b"before"[..])
            );
        }
        assert_eq!(host.store_has(&TMP_PATH).unwrap(), None);
    }

    /// Far beyond any realistic nesting of cross-runtime calls: the names of
    /// the backups still fit the keyspace name size limit.
    #[test]
    fn a_copy_of_a_copy_fits_a_thousand_savepoints() {
        let mut host = seeded_host();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let mut copy = rk.start_safe().unwrap();
        let mut nested = copy.start_safe().unwrap();
        for _ in 0..1000 {
            nested.tez_accounts_savepoint().unwrap();
        }
        assert_eq!(nested.tez_accounts_snapshot().len(), 1000);
        nested.tez_accounts_release_all();
        nested.revert_safe().unwrap();
    }
}
