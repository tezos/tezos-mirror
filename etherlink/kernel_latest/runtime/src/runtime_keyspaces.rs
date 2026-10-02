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
//! state of the two runtimes.

use tezos_smart_rollup_host::path::RefPath;
use tezos_smart_rollup_keyspace::{KeySpaceLoader, KeySpaceLoaderError, Name};

use crate::runtime::MockKernelHost;

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

/// Storage handle threaded through kernel execution.
///
/// The host is borrowed for `'host`: its owner keeps it, and gets it back
/// once the handle is gone.
pub struct RuntimeKeyspaces<'host, Host, KeySpace> {
    host: &'host mut Host,
    keyspaces: Keyspaces<KeySpace>,
}

impl<'host, Host, KeySpace> RuntimeKeyspaces<'host, Host, KeySpace> {
    /// The `/evm/eth_accounts` keyspace.
    pub fn eth_accounts(&self) -> &KeySpace {
        &self.keyspaces.eth_accounts
    }

    /// The `/evm/eth_accounts` keyspace, for the writers.
    pub fn eth_accounts_mut(&mut self) -> &mut KeySpace {
        &mut self.keyspaces.eth_accounts
    }

    /// Returns the `/tez/tez_accounts` keyspace.
    pub fn tez_accounts(&self) -> &KeySpace {
        &self.keyspaces.tez_accounts
    }

    /// Returns the `/tez/tez_accounts` keyspace for writing.
    pub fn tez_accounts_mut(&mut self) -> &mut KeySpace {
        &mut self.keyspaces.tez_accounts
    }

    /// The host, for the durable accesses that have no keyspace yet.
    pub fn host(&self) -> &Host {
        &*self.host
    }

    /// The host, for the durable accesses that have no keyspace yet.
    pub fn host_mut(&mut self) -> &mut Host {
        &mut *self.host
    }
}

impl<'host, Host: KeySpaceLoader> RuntimeKeyspaces<'host, Host, Host::KeySpace> {
    /// Loads the keyspaces from `host`.
    ///
    /// Errors with the loader's error: a keyspace already held elsewhere, or
    /// a name the loader cannot accept.
    pub fn init(host: &'host mut Host) -> Result<Self, KeySpaceLoaderError> {
        let eth_accounts = host.load_or_create(ETH_ACCOUNTS_KEYSPACE_NAME)?;
        let tez_accounts = host.load_or_create(TEZ_ACCOUNTS_KEYSPACE_NAME)?;
        Ok(Self {
            host,
            keyspaces: Keyspaces {
                eth_accounts,
                tez_accounts,
            },
        })
    }
}

/// The keyspaces the handle lends out.
struct Keyspaces<KS> {
    eth_accounts: KS,
    tez_accounts: KS,
}

/// A keyspace a [`MockKernelHost`] mints, for the tests.
pub type MockKeySpace = <MockKernelHost as KeySpaceLoader>::KeySpace;

/// The handle over a borrowed [`MockKernelHost`], for the tests.
pub type MockRuntimeKeyspaces<'host> =
    RuntimeKeyspaces<'host, MockKernelHost, MockKeySpace>;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::safe_storage::{safe_path, SafeStorage};
    use tezos_smart_rollup_host::path::{OwnedPath, Path};
    use tezos_smart_rollup_host::storage::StorageV1;
    use tezos_smart_rollup_keyspace::{Key, KeySpace};

    const PROBE: Key = Key::from_static(b"/probe");

    #[test]
    fn init_loads_the_accounts_keyspaces_in_the_tmp_copy() {
        let mut host = MockKernelHost::default();
        // `start` copies the roots, so they have to exist.
        host.store_write_all(&ETH_ACCOUNTS_ROOT_PATH, b"root")
            .unwrap();
        host.store_write_all(&TEZ_ACCOUNTS_ROOT_PATH, b"root")
            .unwrap();
        let mut safe = SafeStorage {
            host: &mut host,
            world_states: vec![
                OwnedPath::from(ETH_ACCOUNTS_ROOT_PATH),
                OwnedPath::from(TEZ_ACCOUNTS_ROOT_PATH),
            ],
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
            rk.tez_accounts_mut().set(&PROBE, b"inside").unwrap();
        }
        let probe_path = OwnedPath::try_from(
            [TEZ_ACCOUNTS_ROOT_PATH.as_bytes(), PROBE.as_bytes()].concat(),
        )
        .unwrap();
        assert!(safe.host.store_has(&probe_path).unwrap().is_none());
        assert_eq!(
            safe.host
                .store_read_all(&safe_path(&probe_path).unwrap())
                .unwrap(),
            b"inside"
        );

        safe.promote().unwrap();
        assert_eq!(safe.host.store_read_all(&probe_path).unwrap(), b"inside");
    }
}
