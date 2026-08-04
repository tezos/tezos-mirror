// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! [`RuntimeKeyspaces`]: the storage handle threaded through kernel execution.
//!
//! It holds the host and the keyspaces the execution reads and writes, and
//! lends them out one at a time. [`RuntimeKeyspaces::init`] builds it, and is
//! the only keyspace load of a kernel invocation.

use std::borrow::{Borrow, BorrowMut};

use tezos_evm_logging::set_global_verbosity;
use tezos_smart_rollup_host::path::OwnedPath;
use tezos_smart_rollup_keyspace::{KeySpaceLoader, KeySpaceLoaderError, Name};

use crate::runtime::{read_logs_verbosity, KernelHost};
use crate::safe_storage::SafeStorage;

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

    /// Wrap the host in the failsafe mirror shadowing `world_states`, and
    /// lend the same `/base` to the handle it returns.
    ///
    /// `world_states` must not overlap: no path may be a prefix of another.
    /// On a handle whose host is already a mirror, this stacks a second one.
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
    RuntimeKeyspaces<KernelHost<R, H>, <KernelHost<R, H> as KeySpaceLoader>::KeySpace>
where
    KernelHost<R, H>: KeySpaceLoader,
    H: BorrowMut<R> + Borrow<R>,
{
    /// Build the kernel host over `host`, load the `/base` keyspace, and
    /// apply the log verbosity it records.
    ///
    /// The `Err` cannot fire: the `KernelHost` it builds starts from an empty
    /// registry, so this first load has no name to collide with.
    pub fn init(host: H) -> Result<Self, KeySpaceLoaderError> {
        let mut host = KernelHost::init(host);
        let base = host.load_or_create(BASE_KEYSPACE_NAME)?;
        set_global_verbosity(read_logs_verbosity(&base));
        Ok(Self { host, base })
    }
}
