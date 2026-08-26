// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! [`RuntimeKeyspaces`]: the storage handle threaded through kernel execution.
//!
//! It holds the host and the keyspaces the execution reads and writes, and
//! lends them out one at a time. [`RuntimeKeyspaces::init`] builds it, and is
//! the only keyspace load of a kernel invocation.

use std::borrow::{Borrow, BorrowMut};

use tezos_evm_logging::{log, set_global_verbosity, Level};
use thiserror::Error;

use tezos_smart_rollup_host::path::OwnedPath;
use tezos_smart_rollup_host::runtime::RuntimeError;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_host::wasm::WasmHost;
use tezos_smart_rollup_keyspace::{KeySpaceLoader, Name};
use tezos_smart_rollup_mock::MockHost;

use crate::runtime::{read_logs_verbosity, KernelHost, MockKernelHost};
use crate::safe_storage::SafeStorage;
use crate::snapshot::{PreviousRun, SafeKeyspace, SnapshotError, SnapshottedKeySpace};

/// Name of the `/base` keyspace, holding kernel configuration and
/// node-interaction values that do not belong to any world state.
pub const BASE_KEYSPACE_NAME: Name = Name::from_static("/base");

/// Name of the `/evm/eth_accounts` keyspace, holding the EVM runtime's
/// account state.
pub const ETH_ACCOUNTS_KEYSPACE_NAME: Name = Name::from_static("/evm/eth_accounts");

/// Storage handle threaded through kernel execution.
pub struct RuntimeKeyspaces<Host, KS> {
    host: Host,
    base: KS,
    keyspaces: Keyspaces<KS>,
}

impl<Host, KS> RuntimeKeyspaces<Host, KS> {
    /// The `/base` keyspace.
    pub fn base(&self) -> &KS {
        &self.base
    }

    /// The `/base` keyspace, for the writers.
    pub fn base_mut(&mut self) -> &mut KS {
        &mut self.base
    }

    /// The `/evm/eth_accounts` keyspace.
    pub fn eth_accounts(&self) -> &KS {
        &self.keyspaces.eth_accounts
    }

    /// The `/evm/eth_accounts` keyspace, for the writers.
    pub fn eth_accounts_mut(&mut self) -> &mut KS {
        &mut self.keyspaces.eth_accounts
    }

    /// The host, for the durable accesses that have no keyspace yet.
    pub fn host(&self) -> &Host {
        &self.host
    }

    /// The host, for the durable accesses that have no keyspace yet.
    pub fn host_mut(&mut self) -> &mut Host {
        &mut self.host
    }

    /// Open a frame on every keyspace.
    ///
    /// On `Err`, some keyspaces may be framed and some not: the caller must
    /// abort the run.
    pub fn checkpoint(&mut self) -> Result<(), SnapshotError>
    where
        KS: SafeKeyspace,
        Host: KeySpaceLoader<KeySpace = KS::Live>,
    {
        for keyspace in self.keyspaces.iter_mut() {
            keyspace.checkpoint(&mut self.host)?;
        }
        Ok(())
    }

    /// Commit the innermost frame of every keyspace.
    pub fn commit_inner(&mut self) -> Result<(), SnapshotError>
    where
        KS: SafeKeyspace,
    {
        for keyspace in self.keyspaces.iter_mut() {
            keyspace.commit_inner()?;
        }
        Ok(())
    }

    /// Revert the innermost frame of every keyspace.
    pub fn revert_inner(&mut self) -> Result<(), SnapshotError>
    where
        KS: SafeKeyspace,
    {
        for keyspace in self.keyspaces.iter_mut() {
            keyspace.revert_inner()?;
        }
        Ok(())
    }

    /// End the kernel run. A keyspace left at a non-zero depth is one whose
    /// writes no close covered: it is reverted to its bedrock rather than
    /// kept half-written.
    pub fn end_kernel_run(&mut self)
    where
        KS: SafeKeyspace,
    {
        for keyspace in self.keyspaces.iter_mut().filter(|ks| ks.depth() > 0) {
            log!(
                Level::Error,
                "kernel run ended with {} open frame(s) on {}, reverting to its bedrock",
                keyspace.depth(),
                keyspace.name()
            );
            keyspace.revert_all();
        }
    }

    /// Mark every transactional keyspace's open frames so the next run takes
    /// them back. Call it before yielding to a reboot, in place of
    /// [`Self::end_kernel_run`].
    ///
    /// An `Err` may leave the roots marked by halves, and a reboot on top of
    /// that resumes one root while starting the other over: the caller must
    /// abort the run rather than yield.
    pub fn create_reboot_marker(&mut self) -> Result<(), SnapshotError>
    where
        KS: SafeKeyspace,
    {
        for keyspace in self.keyspaces.iter_mut().filter(|ks| ks.depth() > 0) {
            keyspace.create_reboot_marker()?;
        }
        Ok(())
    }

    /// Replace the bedrocks with the live state. The next
    /// [`Self::end_kernel_run`] will restore the live state to this point.
    pub fn commit_all(&mut self) -> Result<(), SnapshotError>
    where
        KS: SafeKeyspace,
    {
        for keyspace in self.keyspaces.iter_mut() {
            keyspace.commit_all()?;
        }
        Ok(())
    }

    /// Wrap the host in the failsafe mirror
    pub fn to_safe_host(
        &mut self,
        world_states: Vec<OwnedPath>,
    ) -> RuntimeKeyspaces<SafeStorage<&mut Host>, &mut KS> {
        RuntimeKeyspaces {
            host: SafeStorage {
                host: &mut self.host,
                world_states,
            },
            base: &mut self.base,
            keyspaces: Keyspaces {
                eth_accounts: &mut self.keyspaces.eth_accounts,
            },
        }
    }
}

/// A root [`RuntimeKeyspaces::revert_both`] could not revert.
#[derive(Debug, Error)]
pub enum RevertError {
    #[error("cannot revert the /tmp copy: {0:?}")]
    TmpCopy(RuntimeError),
    #[error("cannot revert the keyspace frames: {0}")]
    Frames(#[from] SnapshotError),
}

impl<Host, KS> RuntimeKeyspaces<SafeStorage<&mut Host>, &mut KS>
where
    Host: StorageV1,
    KS: SafeKeyspace,
{
    /// Revert both roots this scope covers, the `/tmp` copy and the keyspace
    /// frames. Both are attempted before either is reported.
    pub fn revert_both(&mut self) -> Result<(), RevertError> {
        let tmp_copy = self.host_mut().revert();
        let frames = self.revert_inner();
        tmp_copy.map_err(RevertError::TmpCopy)?;
        frames?;
        Ok(())
    }
}

impl<R, H>
    RuntimeKeyspaces<
        KernelHost<R, H>,
        SnapshottedKeySpace<<KernelHost<R, H> as KeySpaceLoader>::KeySpace>,
    >
where
    KernelHost<R, H>: KeySpaceLoader,
    H: BorrowMut<R> + Borrow<R>,
    R: WasmHost,
{
    /// Build the kernel host over `host`, load the keyspaces, and apply the
    /// log verbosity recorded under `/base`.
    ///
    /// Each keyspace is started here, at the top of the run, so a transaction
    /// a reboot interrupted is re-attached before any caller looks: `start`
    /// reads the in-progress marker and takes depth 0 back when it is set.
    ///
    /// Whether the previous run was cut short is read before any `start`, since
    /// `start` is what would otherwise adopt an interrupted run's writes.
    pub fn init(host: H) -> Result<Self, SnapshotError> {
        let mut host = KernelHost::init(host);
        let previous = if host
            .last_run_aborted()
            .map_err(SnapshotError::PreviousRun)?
        {
            log!(Level::Error, "The previous kernel run was cut short");
            PreviousRun::Aborted
        } else {
            PreviousRun::Complete
        };
        let base = host.load_or_create(BASE_KEYSPACE_NAME)?;
        set_global_verbosity(read_logs_verbosity(&base));
        // `/base` belongs to no block: reverting it would drop blueprints from
        // an inbox level that cannot be read twice.
        let base = SnapshottedKeySpace::start(&mut host, base, PreviousRun::Complete)?;
        let eth_accounts = host.load_or_create(ETH_ACCOUNTS_KEYSPACE_NAME)?;
        let eth_accounts = SnapshottedKeySpace::start(&mut host, eth_accounts, previous)?;
        Ok(Self {
            host,
            base,
            keyspaces: Keyspaces { eth_accounts },
        })
    }
}

/// The keyspaces under transactional control. `/base` is not one of them.
///
/// A field of its own so that `self.host` and `self.keyspaces` can be
/// borrowed at the same time.
struct Keyspaces<KS> {
    eth_accounts: KS,
}

impl<KS> Keyspaces<KS> {
    /// Lends each keyspace in turn. Putting a keyspace under transactional
    /// control means adding an entry here.
    fn iter_mut(&mut self) -> impl Iterator<Item = &mut KS> {
        [&mut self.eth_accounts].into_iter()
    }
}

/// A keyspace a [`MockKernelHost`] mints, for the tests.
pub type MockKeySpace = SnapshottedKeySpace<<MockKernelHost as KeySpaceLoader>::KeySpace>;

/// The handle a [`MockKernelHost`] mints, for the tests.
pub type MockRuntimeKeyspaces = RuntimeKeyspaces<MockKernelHost, MockKeySpace>;

/// Handle over a fresh mock host, for the tests.
impl Default for MockRuntimeKeyspaces {
    fn default() -> Self {
        Self::init(MockHost::default()).expect("failed to init the runtime keyspaces")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_smart_rollup_host::path::RefPath;
    use tezos_smart_rollup_host::storage::StorageV1;

    const PROBE: RefPath = RefPath::assert_from(b"/evm/eth_accounts/probe");

    #[test]
    fn frames_cover_the_eth_accounts_keyspace() {
        let mut rk = MockRuntimeKeyspaces::default();
        rk.host_mut().store_write_all(&PROBE, b"before").unwrap();

        rk.checkpoint().unwrap();
        rk.host_mut().store_write_all(&PROBE, b"inside").unwrap();
        rk.revert_inner().unwrap();

        assert_eq!(rk.host_mut().store_read_all(&PROBE).unwrap(), b"before");
    }

    #[test]
    fn end_kernel_run_restores_the_eth_accounts_bedrock() {
        let mut rk = MockRuntimeKeyspaces::default();
        rk.host_mut().store_write_all(&PROBE, b"before").unwrap();

        rk.checkpoint().unwrap();
        rk.host_mut().store_write_all(&PROBE, b"inside").unwrap();
        rk.end_kernel_run();

        // The bedrock was taken at `init`, before any write: the whole run's
        // writes are gone.
        assert!(rk.host_mut().store_read_all(&PROBE).is_err());
    }
}
