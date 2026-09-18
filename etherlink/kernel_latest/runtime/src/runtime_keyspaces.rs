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
        Ok(Self {
            host,
            keyspaces: Keyspaces { eth_accounts },
        })
    }
}

/// The keyspaces the handle lends out.
struct Keyspaces<KS> {
    eth_accounts: KS,
}

/// A keyspace a [`MockKernelHost`] mints, for the tests.
pub type MockKeySpace = <MockKernelHost as KeySpaceLoader>::KeySpace;

/// The handle over a borrowed [`MockKernelHost`], for the tests.
pub type MockRuntimeKeyspaces<'host> =
    RuntimeKeyspaces<'host, MockKernelHost, MockKeySpace>;
