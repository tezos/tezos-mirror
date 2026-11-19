// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! [`RuntimeKeyspaces`]: the storage handle threaded through kernel execution.
//!
//! It borrows the host and holds the keyspaces the execution reads and
//! writes, and lends them out one at a time. [`RuntimeKeyspaces::init`]
//! builds it over any [`KeySpaceLoader`]: the live host, or a
//! [`SafeStorage`](crate::safe_storage::SafeStorage) whose `/tmp` mirror then
//! covers the keyspaces.
//!
//! The keyspaces are `/evm/eth_accounts` and `/tez/tez_accounts`, the account
//! state of the two runtimes, and `/evm/world_state` and `/tez/world_state`,
//! their chain-level state. `/common_world_state` holds the chain-level state
//! that the runtimes share.
//!
//! `trace` and `__http_trace` are reserved children of `/evm/world_state`:
//! they hold the node-local output of `debug_trace*` and `http_trace*`, and
//! only exist on the throwaway states of a replay or a simulation.
//!
//! [`BlueprintKeyspaces`] is the handle of the stages before block production:
//! it holds `/base`, `/tez/world_state` and `/evm/world_state`, loaded on the
//! live host, and [`BlueprintKeyspaces::into_base`] keeps only `/base` once
//! the execution loads a [`RuntimeKeyspaces`].

use tezos_smart_rollup_host::path::RefPath;
use tezos_smart_rollup_keyspace::{KeySpace, KeySpaceLoader, KeySpaceLoaderError, Name};

use crate::raw_host::RawHost;
use crate::runtime::MockKernelHost;
use crate::snapshot::{Snapshot, SnapshotError};

/// Name of the `/base` keyspace, holding kernel configuration and
/// node-interaction values that do not belong to any world state.
pub const BASE_KEYSPACE_NAME: Name = Name::from_static("/base");

/// Durable root of the `/evm/eth_accounts` keyspace.
const ETH_ACCOUNTS_ROOT: &str = "/evm/eth_accounts";

/// Name of the `/evm/eth_accounts` keyspace, holding the EVM runtime's
/// account state.
pub const ETH_ACCOUNTS_KEYSPACE_NAME: Name = Name::from_static(ETH_ACCOUNTS_ROOT);

/// [`ETH_ACCOUNTS_KEYSPACE_NAME`] as a path, for the raw copies and moves of
/// the `/tmp` mirror.
pub const ETH_ACCOUNTS_ROOT_PATH: RefPath =
    RefPath::assert_from(ETH_ACCOUNTS_ROOT.as_bytes());

/// Durable root of the `/tez/tez_accounts` keyspace.
const TEZ_ACCOUNTS_ROOT: &str = "/tez/tez_accounts";

/// Name of the `/tez/tez_accounts` keyspace, which holds the account state of
/// the Michelson runtime.
pub const TEZ_ACCOUNTS_KEYSPACE_NAME: Name = Name::from_static(TEZ_ACCOUNTS_ROOT);

/// The durable path of [`TEZ_ACCOUNTS_KEYSPACE_NAME`].
pub const TEZ_ACCOUNTS_ROOT_PATH: RefPath =
    RefPath::assert_from(TEZ_ACCOUNTS_ROOT.as_bytes());

/// Durable root of the `/evm/world_state` keyspace.
const EVM_WORLD_STATE_ROOT: &str = "/evm/world_state";

/// Name of the `/evm/world_state` keyspace, which holds the chain-level state
/// of the EVM runtime.
pub const EVM_WORLD_STATE_KEYSPACE_NAME: Name = Name::from_static(EVM_WORLD_STATE_ROOT);

/// The durable path of the `/evm/world_state` root.
pub const EVM_WORLD_STATE_ROOT_PATH: RefPath =
    RefPath::assert_from(EVM_WORLD_STATE_ROOT.as_bytes());

/// Durable root of the `/tez/world_state` keyspace.
const TEZ_WORLD_STATE_ROOT: &str = "/tez/world_state";

/// Name of the `/tez/world_state` keyspace, which holds the chain-level state
/// of the Michelson runtime.
pub const TEZ_WORLD_STATE_KEYSPACE_NAME: Name = Name::from_static(TEZ_WORLD_STATE_ROOT);

/// The durable path of the `/tez/world_state` root.
pub const TEZ_WORLD_STATE_ROOT_PATH: RefPath =
    RefPath::assert_from(TEZ_WORLD_STATE_ROOT.as_bytes());

/// Durable root of the `/common_world_state` keyspace.
const COMMON_WORLD_STATE_ROOT: &str = "/common_world_state";

/// Name of the `/common_world_state` keyspace, which holds the chain-level
/// state that the runtimes share.
pub const COMMON_WORLD_STATE_KEYSPACE_NAME: Name =
    Name::from_static(COMMON_WORLD_STATE_ROOT);

/// The durable path of the `/common_world_state` root.
pub const COMMON_WORLD_STATE_ROOT_PATH: RefPath =
    RefPath::assert_from(COMMON_WORLD_STATE_ROOT.as_bytes());

/// Storage handle threaded through kernel execution.
///
/// The host is borrowed for `'host`: its owner keeps it, and gets it back
/// once the handle is gone.
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
    /// backup through the host the keyspace was loaded from.
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
            .savepoint(self.host.host, &self.keyspaces.tez_accounts)
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
        let eth_accounts = host.load_or_create(ETH_ACCOUNTS_KEYSPACE_NAME)?;
        let tez_accounts = host.load_or_create(TEZ_ACCOUNTS_KEYSPACE_NAME)?;
        let evm_world_state = host.load_or_create(EVM_WORLD_STATE_KEYSPACE_NAME)?;
        let tez_world_state = host.load_or_create(TEZ_WORLD_STATE_KEYSPACE_NAME)?;
        let common_world_state = host.load_or_create(COMMON_WORLD_STATE_KEYSPACE_NAME)?;
        Ok(Self {
            host: RawHost { host, prefix: None },
            keyspaces: Keyspaces {
                eth_accounts,
                tez_accounts,
                evm_world_state,
                tez_world_state,
                common_world_state,
            },
            tez_accounts_snapshot: Snapshot::new(),
        })
    }
}

/// The keyspaces the handle lends out.
struct Keyspaces<KS> {
    eth_accounts: KS,
    tez_accounts: KS,
    evm_world_state: KS,
    tez_world_state: KS,
    common_world_state: KS,
}

/// The handle over a borrowed [`MockKernelHost`], for the tests.
pub type MockRuntimeKeyspaces<'host> = RuntimeKeyspaces<'host, MockKernelHost>;

/// The keyspaces the kernel holds from the inbox to the blueprint, before
/// block production: `/base`, `/tez/world_state` and `/evm/world_state`.
///
/// The handles address the durable roots, not the `/tmp` mirror of a block.
/// The kernel does not write through the world states while a mirror is
/// active, and calls [`Self::into_base`] before it loads a
/// [`RuntimeKeyspaces`] on the same host, which holds the world states again.
pub struct BlueprintKeyspaces<KS> {
    base: KS,
    tez_world_state: KS,
    evm_world_state: KS,
}

impl<KS: KeySpace> BlueprintKeyspaces<KS> {
    /// Loads `/base`, `/tez/world_state` then `/evm/world_state` from `host`.
    ///
    /// Errors with the loader's error: a keyspace already held elsewhere, or
    /// a name the loader cannot accept.
    pub fn load<Host: KeySpaceLoader<KeySpace = KS>>(
        host: &mut Host,
    ) -> Result<Self, KeySpaceLoaderError> {
        let base = host.load_or_create(BASE_KEYSPACE_NAME)?;
        let tez_world_state = host.load_or_create(TEZ_WORLD_STATE_KEYSPACE_NAME)?;
        let evm_world_state = host.load_or_create(EVM_WORLD_STATE_KEYSPACE_NAME)?;
        Ok(Self {
            base,
            tez_world_state,
            evm_world_state,
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

    /// Keeps `/base` and drops the world states, so that a
    /// [`RuntimeKeyspaces::init`] on the same host can load them again.
    pub fn into_base(self) -> KS {
        self.base
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::safe_storage::{safe_path, SafeStorage, TMP_PATH};
    use tezos_smart_rollup_host::path::{OwnedPath, Path};
    use tezos_smart_rollup_host::storage::StorageV1;
    use tezos_smart_rollup_keyspace::{Key, KeySpace};

    const PROBE: Key = Key::from_static(b"/probe");

    #[test]
    fn init_loads_the_keyspaces_in_the_tmp_copy() {
        let mut host = MockKernelHost::default();
        let roots = [
            ETH_ACCOUNTS_ROOT_PATH,
            TEZ_ACCOUNTS_ROOT_PATH,
            EVM_WORLD_STATE_ROOT_PATH,
            TEZ_WORLD_STATE_ROOT_PATH,
            COMMON_WORLD_STATE_ROOT_PATH,
        ];
        // `start` copies the roots, so they have to exist.
        for root in &roots {
            host.store_write_all(root, b"root").unwrap();
        }
        let mut safe = SafeStorage::new(&mut host, roots.map(OwnedPath::from).to_vec());
        safe.start().unwrap();

        {
            let mut rk = RuntimeKeyspaces::init(&mut safe).unwrap();
            assert_eq!(
                rk.eth_accounts().name().to_string(),
                "/tmp/evm/eth_accounts"
            );
            assert_eq!(
                rk.tez_accounts().name().to_string(),
                "/tmp/tez/tez_accounts"
            );
            assert_eq!(
                rk.evm_world_state().name().to_string(),
                "/tmp/evm/world_state"
            );
            assert_eq!(
                rk.tez_world_state().name().to_string(),
                "/tmp/tez/world_state"
            );
            assert_eq!(
                rk.common_world_state().name().to_string(),
                "/tmp/common_world_state"
            );
            rk.eth_accounts_mut().set(&PROBE, b"inside").unwrap();
            rk.tez_accounts_mut().set(&PROBE, b"inside").unwrap();
            rk.evm_world_state_mut().set(&PROBE, b"inside").unwrap();
            rk.tez_world_state_mut().set(&PROBE, b"inside").unwrap();
            rk.common_world_state_mut().set(&PROBE, b"inside").unwrap();
        }
        let probe_paths = roots.map(|root| {
            OwnedPath::try_from([root.as_bytes(), PROBE.as_bytes()].concat()).unwrap()
        });
        for probe_path in &probe_paths {
            assert!(safe.host().store_has(probe_path).unwrap().is_none());
            assert_eq!(
                safe.host()
                    .store_read_all(&safe_path(probe_path).unwrap())
                    .unwrap(),
                b"inside"
            );
        }

        safe.promote().unwrap();
        for probe_path in &probe_paths {
            assert_eq!(safe.host().store_read_all(probe_path).unwrap(), b"inside");
        }
    }

    #[test]
    fn load_holds_the_live_roots_until_into_base() {
        let mut host = MockKernelHost::default();
        let mut bk = BlueprintKeyspaces::load(&mut host).unwrap();
        assert_eq!(bk.base().name().to_string(), "/base");
        assert_eq!(bk.tez_world_state().name().to_string(), "/tez/world_state");
        assert_eq!(bk.evm_world_state().name().to_string(), "/evm/world_state");
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
        // The world states are still held: they cannot be loaded again.
        for name in [TEZ_WORLD_STATE_KEYSPACE_NAME, EVM_WORLD_STATE_KEYSPACE_NAME] {
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

    #[test]
    fn tez_accounts_savepoint_in_a_mirror_is_dropped_by_revert() {
        let mut host = MockKernelHost::default();
        // `start` copies the roots, so they have to exist.
        host.store_write_all(&TEZ_ACCOUNTS_ROOT_PATH, b"root")
            .unwrap();
        let mut safe =
            SafeStorage::new(&mut host, vec![OwnedPath::from(TEZ_ACCOUNTS_ROOT_PATH)]);
        safe.start().unwrap();

        let backup =
            RefPath::assert_from(b"/tmp/__savepoint/0/tmp/tez/tez_accounts/probe");
        {
            let mut rk = RuntimeKeyspaces::init(&mut safe).unwrap();
            rk.tez_accounts_mut().set(&PROBE, b"v0").unwrap();
            rk.tez_accounts_savepoint().unwrap();
            // The backup is named after the mirrored root, and loaded through
            // the mirror: it lands under `/tmp`, and nowhere else.
            assert_eq!(
                rk.host().host.host().store_read_all(&backup).unwrap(),
                b"v0"
            );
            assert!(rk
                .host()
                .host
                .host()
                .store_has(&RefPath::assert_from(b"/__savepoint"))
                .unwrap()
                .is_none());
        }

        safe.revert().unwrap();
        assert!(safe.host().store_has(&backup).unwrap().is_none());
        assert!(safe.host().store_has(&TMP_PATH).unwrap().is_none());
    }
}
