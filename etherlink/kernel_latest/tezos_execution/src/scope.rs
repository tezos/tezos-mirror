// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! The Michelson scopes.
//!
//! A scope is the region between a snapshot being taken and its commit or
//! revert. [`CracOperationScope`] borrows a keyspace handle through
//! [`ScopeRuntimeKeyspace`].

use crate::mir_ctx::TcCtx;
use tezos_evm_runtime::runtime_keyspaces::RuntimeKeyspaces;
use tezos_smart_rollup_host::path::OwnedPath;
use tezos_smart_rollup_host::runtime::RuntimeError;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_keyspace::KeySpaceLoader;
use tezosx_journal::TezosXJournal;

/// Reaches the keyspace handle under one of the kernel's context values.
pub trait ScopeRuntimeKeyspace<'h> {
    /// The durable host under the handle.
    type Host: StorageV1 + KeySpaceLoader + 'h;

    /// Returns the keyspace handle.
    fn rk_mut(&mut self) -> &mut RuntimeKeyspaces<'h, Self::Host>;
}

impl<'h, Host: StorageV1 + KeySpaceLoader> ScopeRuntimeKeyspace<'h>
    for RuntimeKeyspaces<'h, Host>
{
    type Host = Host;

    fn rk_mut(&mut self) -> &mut RuntimeKeyspaces<'h, Host> {
        self
    }
}

impl<'h, Host: KeySpaceLoader + StorageV1> ScopeRuntimeKeyspace<'h>
    for TcCtx<'_, 'h, Host>
{
    type Host = Host;

    fn rk_mut(&mut self) -> &mut RuntimeKeyspaces<'h, Host> {
        self.rk
    }
}

/// The scope of the Michelson side of a cross-runtime call.
///
/// Open snapshots a world-state subtree through the journal; close keeps the
/// writes made under it or restores the subtree.
#[must_use = "a cross-runtime scope must be committed, reverted or deferred to the enclosing EVM frame"]
pub struct CracOperationScope<'s, C> {
    ctx: &'s mut C,
    journal: &'s mut TezosXJournal,
    index: usize,
}

impl<'s, 'h, C: ScopeRuntimeKeyspace<'h>> CracOperationScope<'s, C> {
    /// Open the scope: snapshot the subtree at `from_path`.
    pub fn open(
        ctx: &'s mut C,
        journal: &'s mut TezosXJournal,
        from_path: &OwnedPath,
    ) -> Result<Self, RuntimeError> {
        let index = journal
            .michelson
            .checkpoint(ctx.rk_mut().host_mut(), from_path)?;
        Ok(Self {
            ctx,
            journal,
            index,
        })
    }

    /// Close the scope on success: `checkpoint_commit`, which drops the
    /// snapshots taken under this scope and keeps its own for the enclosing
    /// EVM frame to revert through.
    pub fn commit(self) -> Result<(), RuntimeError> {
        let Self {
            ctx,
            journal,
            index,
        } = self;
        journal
            .michelson
            .checkpoint_commit(ctx.rk_mut().host_mut(), index)
    }

    /// Close the scope on failure: `checkpoint_revert`, restoring the subtree
    /// as it was at open.
    pub fn revert(self) -> Result<(), RuntimeError> {
        let Self {
            ctx,
            journal,
            index,
        } = self;
        journal
            .michelson
            .checkpoint_revert(ctx.rk_mut().host_mut(), index)
    }

    /// Leave the snapshot to the enclosing EVM frame: its `commit_frame` or
    /// `revert_frame` disposes of it.
    pub fn defer_to_parent(self) {}

    /// The context and the journal, for callees that need both at once.
    pub fn parts_mut(&mut self) -> (&mut C, &mut TezosXJournal) {
        (&mut *self.ctx, &mut *self.journal)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_evm_runtime::runtime_keyspaces::{
        MockRuntimeKeyspaces, TEZ_ACCOUNTS_ROOT_PATH,
    };
    use tezos_smart_rollup_host::path::{concat, RefPath};
    use tezos_smart_rollup_host::storage::StorageV1;
    use tezosx_interfaces::RuntimeId;

    /// A key under the snapshotted subtree.
    fn probe() -> OwnedPath {
        concat(&TEZ_ACCOUNTS_ROOT_PATH, &RefPath::assert_from(b"/probe"))
            .expect("the probe path is built")
    }

    fn world_state() -> OwnedPath {
        OwnedPath::from(TEZ_ACCOUNTS_ROOT_PATH)
    }

    #[test]
    fn commit_keeps_the_writes() {
        let mut host = MockKernelHost::default();
        let mut rk = MockRuntimeKeyspaces::init(&mut host).expect("the handle is built");
        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        rk.host_mut()
            .store_write_all(&probe(), b"before")
            .expect("the probe is written");

        let mut scope = CracOperationScope::open(&mut rk, &mut journal, &world_state())
            .expect("the scope is opened");
        let (ctx, _journal) = scope.parts_mut();
        ctx.host_mut()
            .store_write_all(&probe(), b"inside")
            .expect("the write goes through");
        scope.commit().expect("the scope is committed");

        assert_eq!(
            rk.host_mut()
                .store_read_all(&probe())
                .expect("the probe is readable"),
            b"inside"
        );
    }

    #[test]
    fn revert_restores_the_subtree() {
        let mut host = MockKernelHost::default();
        let mut rk = MockRuntimeKeyspaces::init(&mut host).expect("the handle is built");
        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        rk.host_mut()
            .store_write_all(&probe(), b"before")
            .expect("the probe is written");

        let mut scope = CracOperationScope::open(&mut rk, &mut journal, &world_state())
            .expect("the scope is opened");
        let (ctx, _journal) = scope.parts_mut();
        ctx.host_mut()
            .store_write_all(&probe(), b"inside")
            .expect("the write goes through");
        scope.revert().expect("the scope is reverted");

        assert_eq!(
            rk.host_mut()
                .store_read_all(&probe())
                .expect("the probe survives"),
            b"before"
        );
    }

    #[test]
    fn a_deferred_scope_is_reverted_by_the_enclosing_frame() {
        let mut host = MockKernelHost::default();
        let mut rk = MockRuntimeKeyspaces::init(&mut host).expect("the handle is built");
        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        rk.host_mut()
            .store_write_all(&probe(), b"before")
            .expect("the probe is written");

        journal.michelson.push_external_checkpoint();
        let mut scope = CracOperationScope::open(&mut rk, &mut journal, &world_state())
            .expect("the scope is opened");
        let (ctx, _journal) = scope.parts_mut();
        ctx.host_mut()
            .store_write_all(&probe(), b"inside")
            .expect("the write goes through");
        scope.defer_to_parent();
        journal
            .michelson
            .revert_frame(rk.host_mut())
            .expect("the frame is reverted");

        assert_eq!(
            rk.host_mut()
                .store_read_all(&probe())
                .expect("the probe survives"),
            b"before"
        );
    }
}
