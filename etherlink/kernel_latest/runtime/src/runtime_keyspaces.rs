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
//! state of the two runtimes, and `/tez/world_state`, the chain-level state of
//! the Michelson runtime.
//!
//! [`BlueprintKeyspaces`] is the handle of the stages before block production:
//! it holds `/base` and `/tez/world_state`, loaded on the live host, and
//! [`BlueprintKeyspaces::into_base`] keeps only `/base` once the execution
//! loads a [`RuntimeKeyspaces`].

use tezos_smart_rollup_host::path::RefPath;
use tezos_smart_rollup_keyspace::{KeySpace, KeySpaceLoader, KeySpaceLoaderError, Name};

use crate::runtime::MockKernelHost;

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

/// Storage handle threaded through kernel execution.
///
/// The host is borrowed for `'host`: its owner keeps it, and gets it back
/// once the handle is gone.
pub struct RuntimeKeyspaces<'host, Host: KeySpaceLoader> {
    host: &'host mut Host,
    keyspaces: Keyspaces<Host::KeySpace>,
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

    /// Returns the `/tez/world_state` keyspace.
    pub fn tez_world_state(&self) -> &Host::KeySpace {
        &self.keyspaces.tez_world_state
    }

    /// Returns the `/tez/world_state` keyspace for writing.
    pub fn tez_world_state_mut(&mut self) -> &mut Host::KeySpace {
        &mut self.keyspaces.tez_world_state
    }

    /// The host, for the durable accesses that have no keyspace yet.
    pub fn host(&self) -> &Host {
        &*self.host
    }

    /// The host, for the durable accesses that have no keyspace yet.
    pub fn host_mut(&mut self) -> &mut Host {
        &mut *self.host
    }

    /// Loads the keyspaces from `host`.
    ///
    /// Errors with the loader's error: a keyspace already held elsewhere, or
    /// a name the loader cannot accept.
    pub fn init(host: &'host mut Host) -> Result<Self, KeySpaceLoaderError> {
        let eth_accounts = host.load_or_create(ETH_ACCOUNTS_KEYSPACE_NAME)?;
        let tez_accounts = host.load_or_create(TEZ_ACCOUNTS_KEYSPACE_NAME)?;
        let tez_world_state = host.load_or_create(TEZ_WORLD_STATE_KEYSPACE_NAME)?;
        Ok(Self {
            host,
            keyspaces: Keyspaces {
                eth_accounts,
                tez_accounts,
                tez_world_state,
            },
        })
    }
}

/// The keyspaces the handle lends out.
struct Keyspaces<KS> {
    eth_accounts: KS,
    tez_accounts: KS,
    tez_world_state: KS,
}

/// The handle over a borrowed [`MockKernelHost`], for the tests.
pub type MockRuntimeKeyspaces<'host> = RuntimeKeyspaces<'host, MockKernelHost>;

/// The keyspaces the kernel holds from the inbox to the blueprint, before
/// block production: `/base` and `/tez/world_state`.
///
/// The handles address the durable roots, not the `/tmp` mirror of a block.
/// The kernel does not write through the world state while a mirror is
/// active, and calls [`Self::into_base`] before it loads a
/// [`RuntimeKeyspaces`] on the same host, which holds the world state again.
pub struct BlueprintKeyspaces<KS> {
    base: KS,
    tez_world_state: KS,
}

impl<KS: KeySpace> BlueprintKeyspaces<KS> {
    /// Loads `/base` then `/tez/world_state` from `host`.
    ///
    /// Errors with the loader's error: a keyspace already held elsewhere, or
    /// a name the loader cannot accept.
    pub fn load<Host: KeySpaceLoader<KeySpace = KS>>(
        host: &mut Host,
    ) -> Result<Self, KeySpaceLoaderError> {
        let base = host.load_or_create(BASE_KEYSPACE_NAME)?;
        let tez_world_state = host.load_or_create(TEZ_WORLD_STATE_KEYSPACE_NAME)?;
        Ok(Self {
            base,
            tez_world_state,
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

    /// Keeps `/base` and drops the world state, so that a
    /// [`RuntimeKeyspaces::init`] on the same host can load it again.
    pub fn into_base(self) -> KS {
        self.base
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::safe_storage::{safe_path, SafeStorage};
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
            TEZ_WORLD_STATE_ROOT_PATH,
        ];
        // `start` copies the roots, so they have to exist.
        for root in &roots {
            host.store_write_all(root, b"root").unwrap();
        }
        let mut safe = SafeStorage {
            host: &mut host,
            world_states: roots.map(OwnedPath::from).to_vec(),
        };
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
                rk.tez_world_state().name().to_string(),
                "/tmp/tez/world_state"
            );
            rk.eth_accounts_mut().set(&PROBE, b"inside").unwrap();
            rk.tez_accounts_mut().set(&PROBE, b"inside").unwrap();
            rk.tez_world_state_mut().set(&PROBE, b"inside").unwrap();
        }
        let probe_paths = roots.map(|root| {
            OwnedPath::try_from([root.as_bytes(), PROBE.as_bytes()].concat()).unwrap()
        });
        for probe_path in &probe_paths {
            assert!(safe.host.store_has(probe_path).unwrap().is_none());
            assert_eq!(
                safe.host
                    .store_read_all(&safe_path(probe_path).unwrap())
                    .unwrap(),
                b"inside"
            );
        }

        safe.promote().unwrap();
        for probe_path in &probe_paths {
            assert_eq!(safe.host.store_read_all(probe_path).unwrap(), b"inside");
        }
    }

    #[test]
    fn load_holds_the_live_roots_until_into_base() {
        let mut host = MockKernelHost::default();
        let mut bk = BlueprintKeyspaces::load(&mut host).unwrap();
        assert_eq!(bk.base().name().to_string(), "/base");
        assert_eq!(bk.tez_world_state().name().to_string(), "/tez/world_state");
        bk.tez_world_state_mut().set(&PROBE, b"live").unwrap();
        assert_eq!(
            host.store_read_all(&RefPath::assert_from(b"/tez/world_state/probe"))
                .unwrap(),
            b"live"
        );
        // `/tez/world_state` is still held: `init` cannot load it again.
        assert!(matches!(
            RuntimeKeyspaces::init(&mut host),
            Err(KeySpaceLoaderError::AlreadyLoaded)
        ));
        let base = bk.into_base();
        let rk = RuntimeKeyspaces::init(&mut host).unwrap();
        assert_eq!(
            rk.tez_world_state().get(&PROBE).as_deref(),
            Some(&b"live"[..])
        );
        assert_eq!(base.name().to_string(), "/base");
    }
}
