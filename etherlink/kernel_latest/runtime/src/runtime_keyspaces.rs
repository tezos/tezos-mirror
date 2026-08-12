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
use tezos_smart_rollup_host::path::OwnedPath;
use tezos_smart_rollup_keyspace::{KeySpaceLoader, Name};
use tezos_smart_rollup_mock::MockHost;

use crate::runtime::{read_logs_verbosity, KernelHost, MockKernelHost};
use crate::safe_storage::SafeStorage;
use crate::snapshot::{SafeKeyspace, SnapshotError, SnapshottedKeySpace};

/// Name of the `/base` keyspace, holding kernel configuration and
/// node-interaction values that do not belong to any world state.
pub const BASE_KEYSPACE_NAME: Name = Name::from_static("/base");

/// Storage handle threaded through kernel execution.
pub struct RuntimeKeyspaces<Host, KS> {
    host: Host,
    base: KS,
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

    /// The host, for the durable accesses that have no keyspace yet.
    pub fn host(&self) -> &Host {
        &self.host
    }

    /// The host, for the durable accesses that have no keyspace yet.
    pub fn host_mut(&mut self) -> &mut Host {
        &mut self.host
    }

    /// The keyspaces under transactional control. `/base` is not one of them:
    /// nothing checkpoints it.
    fn snapshotted_keyspaces(&mut self) -> impl Iterator<Item = &mut KS> {
        std::iter::empty()
    }

    /// End the kernel run. A keyspace left at a non-zero depth is one whose
    /// writes no close covered: it is reverted to its bedrock rather than
    /// kept half-written.
    pub fn end_kernel_run(&mut self)
    where
        KS: SafeKeyspace,
    {
        for keyspace in self.snapshotted_keyspaces().filter(|ks| ks.depth() > 0) {
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
        for keyspace in self.snapshotted_keyspaces() {
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
        for keyspace in self.snapshotted_keyspaces() {
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
        }
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
{
    /// Build the kernel host over `host`, load the keyspaces, and apply the
    /// log verbosity recorded under `/base`.
    ///
    /// Each keyspace is started here, at the top of the run, so a transaction
    /// a reboot interrupted is re-attached before any caller looks: `start`
    /// reads the in-progress marker and takes depth 0 back when it is set.
    ///
    /// `Err` is unreachable on the kernel host: it starts from an empty
    /// registry, and the names loaded here do not overlap.
    pub fn init(host: H) -> Result<Self, SnapshotError> {
        let mut host = KernelHost::init(host);
        let base = host.load_or_create(BASE_KEYSPACE_NAME)?;
        set_global_verbosity(read_logs_verbosity(&base));
        let base = SnapshottedKeySpace::start(&mut host, base)?;
        Ok(Self { host, base })
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
