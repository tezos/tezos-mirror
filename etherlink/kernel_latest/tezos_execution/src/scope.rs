// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! The Michelson scopes.
//!
//! A scope is the region between a snapshot being taken and its commit or
//! revert. The mirror is the `/tmp` copy of the safe roots that
//! [`SafeStorage`] writes to until `promote`; a keyspace loaded through it
//! writes into the mirror too.
//!
//! [`CracOperationScope`] borrows a keyspace handle through
//! [`ScopeRuntimeKeyspace`] and opens a savepoint on the snapshot of its
//! Michelson accounts, while [`OperationScope`] owns the operation mirror and
//! builds the keyspace handle over it.

use crate::mir_ctx::TcCtx;
use tezos_evm_runtime::runtime_keyspaces::RuntimeKeyspaces;
use tezos_evm_runtime::safe_storage::SafeStorage;
use tezos_evm_runtime::snapshot::SnapshotError;
use tezos_smart_rollup_host::path::OwnedPath;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_keyspace::{KeySpaceLoader, KeySpaceLoaderError};
use tezos_tezlink::operation_result::OperationError;
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

/// How a scope's body asks it to close. Both variants carry the body's
/// value out to the caller of [`OperationScope::run`] or
/// [`CracOperationScope::run`].
pub enum Close<T> {
    /// Commit the scope. See each scope's `run` for what is kept.
    Commit(T),
    /// Revert the scope. See each scope's `run` for what is restored.
    Revert(T),
}

/// The scope of the Michelson side of a cross-runtime call.
///
/// Open checkpoints the Michelson accounts of the keyspace handle through the
/// journal, opening a savepoint on their snapshot; close keeps the writes made
/// under it or restores the accounts. Only [`Self::run`] opens and closes it,
/// around a body; [`Self::checkpoint_for_parent`] opens one whose close is
/// left to the enclosing code.
pub struct CracOperationScope<'s, C> {
    ctx: &'s mut C,
    journal: &'s mut TezosXJournal,
    index: usize,
}

impl<'s, 'h, C: ScopeRuntimeKeyspace<'h>> CracOperationScope<'s, C> {
    /// Open the scope: checkpoint the Michelson accounts.
    fn open(
        ctx: &'s mut C,
        journal: &'s mut TezosXJournal,
    ) -> Result<Self, SnapshotError> {
        let index = journal.michelson.checkpoint(ctx.rk_mut())?;
        Ok(Self {
            ctx,
            journal,
            index,
        })
    }

    /// Close the scope on success: `checkpoint_commit`, which releases the
    /// savepoints opened under this scope and keeps its own for the enclosing
    /// EVM frame to revert through.
    fn commit(self) -> Result<(), SnapshotError> {
        let Self {
            ctx,
            journal,
            index,
        } = self;
        journal.michelson.checkpoint_commit(ctx.rk_mut(), index)
    }

    /// Close the scope on failure: `checkpoint_revert`, restoring the accounts
    /// as they were at open.
    fn revert(self) -> Result<(), SnapshotError> {
        let Self {
            ctx,
            journal,
            index,
        } = self;
        journal.michelson.checkpoint_revert(ctx.rk_mut(), index)
    }

    /// The context and the journal, for callees that need both at once.
    pub fn parts_mut(&mut self) -> (&mut C, &mut TezosXJournal) {
        (&mut *self.ctx, &mut *self.journal)
    }

    /// Runs `body` under a journal checkpoint of the Michelson accounts, then
    /// keeps the writes on [`Close::Commit`] or restores the accounts on
    /// [`Close::Revert`] or a body error.
    ///
    /// A commit leaves the checkpoint to the enclosing EVM frame, which can
    /// still revert it. A close error takes precedence over the body's result.
    pub fn run<T, E: From<SnapshotError>>(
        ctx: &'s mut C,
        journal: &'s mut TezosXJournal,
        body: impl FnOnce(&mut Self) -> Result<Close<T>, E>,
    ) -> Result<T, E> {
        let mut scope = Self::open(ctx, journal)?;
        match body(&mut scope) {
            Ok(Close::Commit(t)) => scope.commit().map(|()| t).map_err(E::from),
            Ok(Close::Revert(t)) => scope.revert().map(|()| t).map_err(E::from),
            Err(e) => {
                scope.revert()?;
                Err(e)
            }
        }
    }

    /// Checkpoints the Michelson accounts and leaves the checkpoint to the
    /// enclosing code, which closes it through the journal: an EVM frame's
    /// `commit_frame` or `revert_frame`, or `release_savepoints_from` or
    /// `revert_savepoints_from`.
    ///
    /// Fails if the savepoint cannot be opened.
    pub fn checkpoint_for_parent(
        ctx: &'s mut C,
        journal: &'s mut TezosXJournal,
    ) -> Result<(), SnapshotError> {
        let _scope = Self::open(ctx, journal)?;
        Ok(())
    }
}

/// The phase of a Michelson operation a scope covers.
///
/// An operation runs in two phases, each under its own scope, and the
/// scopes close independently: the validation scope is committed before
/// the application scope opens, so reverting the application leaves the
/// validation writes in place.
#[derive(Clone, Copy, Debug)]
pub enum Phase {
    /// The operation is checked, the fee is debited. A revert only drops the
    /// mirror.
    Validation,
    /// The operation is applied. A revert also clears the EVM journal and the
    /// cross-runtime originator, and restores the origination index.
    Application,
}

/// What a phase keeps from its open for its close.
#[derive(Clone, Copy, Debug)]
enum PhaseState {
    Validation,
    Application {
        /// The origination index at open.
        origination_index_before: u32,
    },
}

/// The scope of one phase of a Michelson operation.
///
/// Open starts the operation mirror over the host; close promotes it or
/// rolls it back. Only [`Self::run`] opens and closes it, around a body. The
/// keyspace handle is built over the mirror on demand, so the keyspace writes
/// follow the close as well.
pub struct OperationScope<'s, Host> {
    safe_host: SafeStorage<&'s mut Host>,
    journal: &'s mut TezosXJournal,
    phase: PhaseState,
}

impl<'s, Host: StorageV1> OperationScope<'s, Host> {
    /// Close the scope on success: `promote`.
    fn commit(self) -> Result<(), OperationError> {
        let Self { mut safe_host, .. } = self;
        safe_host.promote()?;
        Ok(())
    }

    /// Close the scope on failure, in this order: `revert` → under
    /// [`Phase::Application`] only: `evm.clear()`, `reset_original_source()`,
    /// `restore_origination_index(index at open)`.
    ///
    /// The mirror rolls back durable storage only, so the in-memory journal
    /// state is restored by hand: the EVM journal, which would otherwise
    /// persist the writes of a backtracked operation; the cross-runtime
    /// originator, which must not reach the next operation; the origination
    /// index, which a backtracked batch must not consume.
    fn revert(self) -> Result<(), OperationError> {
        let Self {
            mut safe_host,
            journal,
            phase,
            ..
        } = self;
        safe_host.revert()?;
        if let PhaseState::Application {
            origination_index_before,
        } = phase
        {
            journal.evm.clear();
            journal.reset_original_source();
            journal
                .michelson
                .restore_origination_index(origination_index_before);
        }
        Ok(())
    }
}

impl<'s, Host: StorageV1 + KeySpaceLoader> OperationScope<'s, Host> {
    /// Open the scope: `start` the mirror of `world_states` over the host
    /// under `ctx`, and under [`Phase::Application`] record the origination
    /// index.
    ///
    /// `phase` is consumed by [`Self::revert`].
    /// No EVM frame covers this path, so nothing rolls the origination index
    /// back: it is captured here for [`Self::revert`] to restore.
    fn open<'h, C>(
        ctx: &'s mut C,
        world_states: &[OwnedPath],
        journal: &'s mut TezosXJournal,
        phase: Phase,
    ) -> Result<Self, OperationError>
    where
        'h: 's,
        Host: 'h,
        C: ScopeRuntimeKeyspace<'h, Host = Host>,
    {
        let host = ctx.rk_mut().host_mut().unprefixed_mut().ok_or_else(|| {
            OperationError::BlockAbort(
                "the operation scope needs an unprefixed host".into(),
            )
        })?;
        let mut safe_host = SafeStorage::new(host, world_states.to_vec());
        safe_host.start()?;
        let phase = match phase {
            Phase::Validation => PhaseState::Validation,
            Phase::Application => PhaseState::Application {
                origination_index_before: journal.michelson.origination_index(),
            },
        };
        Ok(Self {
            safe_host,
            journal,
            phase,
        })
    }

    /// Runs `body` on a `/tmp` mirror of `world_states`, then promotes the
    /// mirror on [`Close::Commit`] or drops it on [`Close::Revert`] or a body
    /// error.
    ///
    /// A commit keeps only `world_states`; see [`Phase`] for what a revert
    /// also resets. `world_states` must not overlap. A close error takes
    /// precedence over the body's result.
    pub fn run<'h, C, T, E>(
        ctx: &'s mut C,
        world_states: &[OwnedPath],
        journal: &'s mut TezosXJournal,
        phase: Phase,
        body: impl FnOnce(&mut Self) -> Result<Close<T>, E>,
    ) -> Result<T, E>
    where
        'h: 's,
        Host: 'h,
        C: ScopeRuntimeKeyspace<'h, Host = Host>,
        E: From<OperationError>,
    {
        let mut scope = Self::open(ctx, world_states, journal, phase)?;
        match body(&mut scope) {
            Ok(Close::Commit(t)) => scope.commit().map(|()| t).map_err(E::from),
            Ok(Close::Revert(t)) => scope.revert().map(|()| t).map_err(E::from),
            Err(e) => {
                scope.revert()?;
                Err(e)
            }
        }
    }

    /// The keyspace handle, loaded over the mirror, and the journal, for
    /// callees that need both at once.
    ///
    /// The handle borrows the scope, so it is gone before the scope closes.
    /// Errors with the loader's error: a mirrored name over the size limit,
    /// or a handle on the same name still held elsewhere.
    #[allow(clippy::type_complexity)]
    pub fn parts_mut(
        &mut self,
    ) -> Result<
        (
            RuntimeKeyspaces<
                '_,
                impl StorageV1 + KeySpaceLoader<KeySpace = Host::KeySpace> + use<'s, Host>,
            >,
            &mut TezosXJournal,
        ),
        KeySpaceLoaderError,
    > {
        let rk = RuntimeKeyspaces::init(&mut self.safe_host)?;
        Ok((rk, &mut *self.journal))
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

        CracOperationScope::run(
            &mut rk,
            &mut journal,
            |scope| -> Result<Close<()>, SnapshotError> {
                let (ctx, _journal) = scope.parts_mut();
                ctx.host_mut()
                    .store_write_all(&probe(), b"inside")
                    .expect("the write goes through");
                Ok(Close::Commit(()))
            },
        )
        .expect("the scope is committed");

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

        CracOperationScope::run(
            &mut rk,
            &mut journal,
            |scope| -> Result<Close<()>, SnapshotError> {
                let (ctx, _journal) = scope.parts_mut();
                ctx.host_mut()
                    .store_write_all(&probe(), b"inside")
                    .expect("the write goes through");
                Ok(Close::Revert(()))
            },
        )
        .expect("the scope is reverted");

        assert_eq!(
            rk.host_mut()
                .store_read_all(&probe())
                .expect("the probe survives"),
            b"before"
        );
    }

    #[test]
    fn run_reverts_when_the_body_errs() {
        let mut host = MockKernelHost::default();
        let mut rk = MockRuntimeKeyspaces::init(&mut host).expect("the handle is built");
        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        rk.host_mut()
            .store_write_all(&probe(), b"before")
            .expect("the probe is written");

        let result = CracOperationScope::run(
            &mut rk,
            &mut journal,
            |scope| -> Result<Close<()>, SnapshotError> {
                let (ctx, _journal) = scope.parts_mut();
                ctx.host_mut()
                    .store_write_all(&probe(), b"inside")
                    .expect("the write goes through");
                Err(SnapshotError::NoSavepoint)
            },
        );

        assert_eq!(result, Err(SnapshotError::NoSavepoint));
        assert_eq!(
            rk.host_mut()
                .store_read_all(&probe())
                .expect("the probe survives"),
            b"before"
        );
    }

    #[test]
    fn run_returns_the_bodys_value_after_a_revert() {
        let mut host = MockKernelHost::default();
        let mut rk = MockRuntimeKeyspaces::init(&mut host).expect("the handle is built");
        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        rk.host_mut()
            .store_write_all(&probe(), b"before")
            .expect("the probe is written");

        let value = CracOperationScope::run(
            &mut rk,
            &mut journal,
            |scope| -> Result<Close<u8>, SnapshotError> {
                let (ctx, _journal) = scope.parts_mut();
                ctx.host_mut()
                    .store_write_all(&probe(), b"inside")
                    .expect("the write goes through");
                Ok(Close::Revert(42))
            },
        )
        .expect("the scope is reverted");

        assert_eq!(value, 42);
        assert_eq!(
            rk.host_mut()
                .store_read_all(&probe())
                .expect("the probe survives"),
            b"before"
        );
    }

    #[test]
    fn a_checkpoint_for_parent_is_reverted_by_the_enclosing_frame() {
        let mut host = MockKernelHost::default();
        let mut rk = MockRuntimeKeyspaces::init(&mut host).expect("the handle is built");
        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        rk.host_mut()
            .store_write_all(&probe(), b"before")
            .expect("the probe is written");

        journal.michelson.push_external_checkpoint();
        CracOperationScope::checkpoint_for_parent(&mut rk, &mut journal)
            .expect("the checkpoint is taken");
        rk.host_mut()
            .store_write_all(&probe(), b"inside")
            .expect("the write goes through");
        journal
            .michelson
            .revert_frame(&mut rk)
            .expect("the frame is reverted");

        assert_eq!(
            rk.host_mut()
                .store_read_all(&probe())
                .expect("the probe survives"),
            b"before"
        );
    }

    #[test]
    fn a_committed_operation_scope_keeps_its_writes() {
        use tezos_evm_runtime::safe_storage::TMP_PATH;

        let mut host = MockKernelHost::default();
        let mut rk = MockRuntimeKeyspaces::init(&mut host).expect("the handle is built");
        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        // The mirror copies the safe roots: the subtree must exist.
        rk.host_mut()
            .store_write_all(&probe(), b"before")
            .expect("the probe is written");

        OperationScope::run(
            &mut rk,
            &[world_state()],
            &mut journal,
            Phase::Application,
            |scope| -> Result<Close<()>, OperationError> {
                let (mut scoped_rk, _) = scope.parts_mut()?;
                scoped_rk
                    .host_mut()
                    .store_write_all(&probe(), b"inside")
                    .expect("the write goes through");
                Ok(Close::Commit(()))
            },
        )
        .expect("the scope is committed");

        assert_eq!(
            rk.host_mut()
                .store_read_all(&probe())
                .expect("the probe is readable"),
            b"inside"
        );
        assert!(
            rk.host_mut()
                .store_has(&TMP_PATH)
                .expect("/tmp is readable")
                .is_none(),
            "a committed scope leaves no `/tmp` behind"
        );
    }

    #[test]
    fn a_reverted_application_scope_gives_the_origination_indices_back() {
        let mut host = MockKernelHost::default();
        let mut rk = MockRuntimeKeyspaces::init(&mut host).expect("the handle is built");
        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        rk.host_mut()
            .store_write_all(&probe(), b"before")
            .expect("the probe is written");
        let index_before = journal.michelson.origination_index();

        OperationScope::run(
            &mut rk,
            &[world_state()],
            &mut journal,
            Phase::Application,
            |scope| -> Result<Close<()>, OperationError> {
                let (mut scoped_rk, scoped_journal) = scope.parts_mut()?;
                scoped_rk
                    .host_mut()
                    .store_write_all(&probe(), b"inside")
                    .expect("the write goes through");
                scoped_journal.michelson.next_origination_index();
                Ok(Close::Revert(()))
            },
        )
        .expect("the scope is reverted");

        assert_eq!(
            rk.host_mut()
                .store_read_all(&probe())
                .expect("the probe survives"),
            b"before"
        );
        assert_eq!(journal.michelson.origination_index(), index_before);
    }

    #[test]
    fn a_reverted_application_scope_resets_the_journal() {
        use tezosx_journal::OriginalSource;

        let mut host = MockKernelHost::default();
        let mut rk = MockRuntimeKeyspaces::init(&mut host).expect("the handle is built");
        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        rk.host_mut()
            .store_write_all(&probe(), b"before")
            .expect("the probe is written");

        OperationScope::run(
            &mut rk,
            &[world_state()],
            &mut journal,
            Phase::Application,
            |scope| -> Result<Close<()>, OperationError> {
                let (_scoped_rk, scoped_journal) = scope.parts_mut()?;
                scoped_journal.evm.set_crac_chain_depth(1);
                scoped_journal.set_original_source(OriginalSource::new(
                    RuntimeId::Tezos,
                    "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx".to_string(),
                ));
                Ok(Close::Revert(()))
            },
        )
        .expect("the scope is reverted");

        assert_eq!(journal.evm.crac_chain_depth(), 0);
        assert_eq!(journal.original_source(), None);
    }

    /// Guards the shared-journal case: an operation dispatched from the EVM
    /// runs on that execution's journal.
    #[test]
    fn a_reverted_validation_scope_leaves_the_origination_index_alone() {
        let mut host = MockKernelHost::default();
        let mut rk = MockRuntimeKeyspaces::init(&mut host).expect("the handle is built");
        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        rk.host_mut()
            .store_write_all(&probe(), b"before")
            .expect("the probe is written");
        let index_before = journal.michelson.origination_index();

        OperationScope::run(
            &mut rk,
            &[world_state()],
            &mut journal,
            Phase::Validation,
            |scope| -> Result<Close<()>, OperationError> {
                let (_scoped_rk, scoped_journal) = scope.parts_mut()?;
                scoped_journal.michelson.next_origination_index();
                Ok(Close::Revert(()))
            },
        )
        .expect("the scope is reverted");

        assert_eq!(journal.michelson.origination_index(), index_before + 1);
    }

    /// Runs one application scope that writes to the `/probe` key of the
    /// `eth_accounts` keyspace it lends, commits or reverts it, and returns
    /// what the live keyspace holds afterwards.
    fn eth_accounts_probe_after(commit: bool) -> Option<Vec<u8>> {
        use tezos_evm_runtime::runtime_keyspaces::ETH_ACCOUNTS_ROOT_PATH;
        use tezos_smart_rollup_keyspace::{Key, KeySpace};

        let key = Key::from_static(b"/probe");
        let mut host = MockKernelHost::default();
        // The mirror copies the safe roots: the root must exist.
        host.store_write_all(&ETH_ACCOUNTS_ROOT_PATH, b"seed")
            .expect("the root is written");
        let mut rk = MockRuntimeKeyspaces::init(&mut host).expect("the handle is built");
        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);

        OperationScope::run(
            &mut rk,
            &[OwnedPath::from(ETH_ACCOUNTS_ROOT_PATH)],
            &mut journal,
            Phase::Application,
            |scope| -> Result<Close<()>, OperationError> {
                let (mut scoped_rk, _journal) = scope.parts_mut()?;
                scoped_rk
                    .eth_accounts_mut()
                    .set(&key, b"inside")
                    .expect("the write goes through");
                Ok(if commit {
                    Close::Commit(())
                } else {
                    Close::Revert(())
                })
            },
        )
        .expect("the scope is closed");

        rk.eth_accounts().get(&key)
    }

    #[test]
    fn a_committed_operation_scope_keeps_the_keyspace_writes() {
        assert_eq!(eth_accounts_probe_after(true), Some(b"inside".to_vec()));
    }

    #[test]
    fn a_reverted_operation_scope_drops_the_keyspace_writes() {
        assert_eq!(eth_accounts_probe_after(false), None);
    }

    #[test]
    fn run_reverts_the_operation_scope_when_the_body_errs() {
        use tezos_evm_runtime::safe_storage::TMP_PATH;

        let mut host = MockKernelHost::default();
        let mut rk = MockRuntimeKeyspaces::init(&mut host).expect("the handle is built");
        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        rk.host_mut()
            .store_write_all(&probe(), b"before")
            .expect("the probe is written");
        let index_before = journal.michelson.origination_index();

        let result = OperationScope::run(
            &mut rk,
            &[world_state()],
            &mut journal,
            Phase::Application,
            |scope| -> Result<Close<()>, OperationError> {
                let (mut scoped_rk, scoped_journal) = scope.parts_mut()?;
                scoped_rk
                    .host_mut()
                    .store_write_all(&probe(), b"inside")
                    .expect("the write goes through");
                scoped_journal.michelson.next_origination_index();
                Err(OperationError::BlockAbort("the body fails".into()))
            },
        );

        assert!(matches!(result, Err(OperationError::BlockAbort(_))));
        assert_eq!(
            rk.host_mut()
                .store_read_all(&probe())
                .expect("the probe survives"),
            b"before"
        );
        assert_eq!(journal.michelson.origination_index(), index_before);
        assert!(
            rk.host_mut()
                .store_has(&TMP_PATH)
                .expect("/tmp is readable")
                .is_none(),
            "a reverted scope leaves no `/tmp` behind"
        );
    }
}
