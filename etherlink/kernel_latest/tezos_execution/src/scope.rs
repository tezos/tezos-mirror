// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! The Michelson scopes.
//!
//! A scope is the region between a snapshot being taken and its commit or
//! revert.
//!
//! [`CracOperationScope`] borrows a keyspace handle through
//! [`ScopeRuntimeKeyspace`] and opens a savepoint on the snapshot of its
//! Michelson accounts, while [`OperationScope`] holds a safe copy of the
//! handle (see [`RuntimeKeyspaces::start_safe`]).

use crate::mir_ctx::TcCtx;
use tezos_evm_runtime::runtime_keyspaces::{RuntimeKeyspaces, SafeCopyError};
use tezos_evm_runtime::snapshot::SnapshotError;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_keyspace::KeySpaceLoader;
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
    /// safe copy.
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
/// It holds a safe copy of the caller's keyspace handle: only
/// [`Self::run`] opens and closes it, around a body, which promotes the copy
/// into the handle or reverts it.
pub struct OperationScope<'s, 'j, Host: KeySpaceLoader> {
    rk: RuntimeKeyspaces<'s, Host>,
    journal: &'j mut TezosXJournal,
}

/// The error of a safe copy that cannot be opened or closed.
fn safe_copy_error(err: SafeCopyError) -> OperationError {
    match err {
        SafeCopyError::Loader(err) => OperationError::KeySpace(err),
        SafeCopyError::Storage(err) => OperationError::RuntimeError(err),
        SafeCopyError::SavepointsOpen(_)
        | SafeCopyError::NotACopy
        | SafeCopyError::NoCopy => OperationError::BlockAbort(err.to_string()),
    }
}

impl PhaseState {
    /// The state of `phase` at open. No EVM frame covers this path, so
    /// nothing rolls the origination index back: it is captured here for the
    /// revert to restore.
    fn open(phase: Phase, journal: &TezosXJournal) -> Self {
        match phase {
            Phase::Validation => PhaseState::Validation,
            Phase::Application => PhaseState::Application {
                origination_index_before: journal.michelson.origination_index(),
            },
        }
    }

    /// Under [`Phase::Application`] only: `evm.clear()`,
    /// `reset_original_source()`, `restore_origination_index(index at open)`.
    ///
    /// The safe copy rolls back durable storage only, so the in-memory journal
    /// state is restored by hand: the EVM journal, which would otherwise
    /// persist the writes of a backtracked operation; the cross-runtime
    /// originator, which must not reach the next operation; the origination
    /// index, which a backtracked batch must not consume.
    fn reset(self, journal: &mut TezosXJournal) {
        if let PhaseState::Application {
            origination_index_before,
        } = self
        {
            journal.evm.clear();
            journal.reset_original_source();
            journal
                .michelson
                .restore_origination_index(origination_index_before);
        }
    }
}

impl<Host: StorageV1 + KeySpaceLoader> OperationScope<'_, '_, Host> {
    /// Runs `body` in a safe copy of the handle under `ctx`. On every path,
    /// releases the savepoints left open in the copy through the journal,
    /// which brings its count back to the enclosing snapshots'. Then promotes
    /// the copy into the handle on [`Close::Commit`], or reverts it on
    /// [`Close::Revert`] or a body error.
    ///
    /// See [`Phase`] for what a revert also resets, which a failed commit
    /// resets too. A close error takes precedence over the body's result.
    pub fn run<'h, C, T, E>(
        ctx: &mut C,
        journal: &mut TezosXJournal,
        phase: Phase,
        body: impl FnOnce(&mut OperationScope<'_, '_, Host>) -> Result<Close<T>, E>,
    ) -> Result<T, E>
    where
        Host: 'h,
        C: ScopeRuntimeKeyspace<'h, Host = Host>,
        E: From<OperationError>,
    {
        let phase = PhaseState::open(phase, journal);
        let parent = ctx.rk_mut();
        let copy = parent.start_safe().map_err(safe_copy_error)?;
        let mut scope = OperationScope {
            rk: copy,
            journal: &mut *journal,
        };
        let result = body(&mut scope);
        let OperationScope { rk: mut copy, .. } = scope;
        let released = journal
            .michelson
            .release_all_savepoints(&mut copy)
            .map_err(|e| {
                OperationError::BlockAbort(format!("Batch savepoints release: {e}"))
            });
        match (result, released) {
            (Ok(Close::Commit(t)), Ok(())) => {
                match copy
                    .into_keyspaces()
                    .and_then(|keyspaces| parent.promote_from(keyspaces))
                {
                    Ok(()) => Ok(t),
                    Err(err) => {
                        phase.reset(journal);
                        Err(safe_copy_error(err).into())
                    }
                }
            }
            (result, released) => {
                let reverted = copy.revert_safe().map_err(safe_copy_error);
                phase.reset(journal);
                released?;
                reverted?;
                match result? {
                    Close::Commit(t) | Close::Revert(t) => Ok(t),
                }
            }
        }
    }
}

impl<'s, Host: KeySpaceLoader> OperationScope<'s, '_, Host> {
    /// The safe copy and the journal, for callees that need both at once.
    pub fn parts_mut(&mut self) -> (&mut RuntimeKeyspaces<'s, Host>, &mut TezosXJournal) {
        (&mut self.rk, &mut *self.journal)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_evm_runtime::runtime_keyspaces::{
        MockRuntimeKeyspaces, TEZ_ACCOUNTS_ROOT_PATH,
    };
    use tezos_evm_runtime::safe_storage::TMP_PATH;
    use tezos_smart_rollup_host::path::{concat, OwnedPath, RefPath};
    use tezos_smart_rollup_host::storage::StorageV1;
    use tezos_smart_rollup_keyspace::{Key, KeySpace};
    use tezosx_interfaces::RuntimeId;

    /// A key under the snapshotted subtree.
    fn probe() -> OwnedPath {
        concat(&TEZ_ACCOUNTS_ROOT_PATH, &RefPath::assert_from(b"/probe"))
            .expect("the probe path is built")
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
        let mut host = MockKernelHost::default();
        let mut rk = MockRuntimeKeyspaces::init(&mut host).expect("the handle is built");
        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        rk.host_mut()
            .store_write_all(&probe(), b"before")
            .expect("the probe is written");

        OperationScope::run(
            &mut rk,
            &mut journal,
            Phase::Application,
            |scope| -> Result<Close<()>, OperationError> {
                let (scoped_rk, _) = scope.parts_mut();
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
            &mut journal,
            Phase::Application,
            |scope| -> Result<Close<()>, OperationError> {
                let (scoped_rk, scoped_journal) = scope.parts_mut();
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
            &mut journal,
            Phase::Application,
            |scope| -> Result<Close<()>, OperationError> {
                let (_scoped_rk, scoped_journal) = scope.parts_mut();
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
            &mut journal,
            Phase::Validation,
            |scope| -> Result<Close<()>, OperationError> {
                let (_scoped_rk, scoped_journal) = scope.parts_mut();
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
        host.store_write_all(&ETH_ACCOUNTS_ROOT_PATH, b"seed")
            .expect("the root is written");
        let mut rk = MockRuntimeKeyspaces::init(&mut host).expect("the handle is built");
        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);

        OperationScope::run(
            &mut rk,
            &mut journal,
            Phase::Application,
            |scope| -> Result<Close<()>, OperationError> {
                let (scoped_rk, _journal) = scope.parts_mut();
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
        let mut host = MockKernelHost::default();
        let mut rk = MockRuntimeKeyspaces::init(&mut host).expect("the handle is built");
        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        rk.host_mut()
            .store_write_all(&probe(), b"before")
            .expect("the probe is written");
        let index_before = journal.michelson.origination_index();

        let result = OperationScope::run(
            &mut rk,
            &mut journal,
            Phase::Application,
            |scope| -> Result<Close<()>, OperationError> {
                let (scoped_rk, scoped_journal) = scope.parts_mut();
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

    /// The backup of the first savepoint of a safe copy's accounts.
    const COPY_BACKUP: RefPath =
        RefPath::assert_from(b"/tmp/__savepoint/0/tmp/tez/tez_accounts");

    /// Runs one application scope whose body opens a savepoint in the safe
    /// copy on top of the one the caller holds, writes to the accounts and
    /// closes with `close`. Returns the scope's result, what the live
    /// accounts hold at the probe, and the journal's savepoint count.
    fn run_with_a_savepoint_left_open(
        close: Result<Close<()>, OperationError>,
    ) -> (Result<(), OperationError>, Option<Vec<u8>>, usize) {
        let key = Key::from_static(b"/probe");
        let mut host = MockKernelHost::default();
        let mut rk = MockRuntimeKeyspaces::init(&mut host).expect("the handle is built");
        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        rk.tez_accounts_mut()
            .set(&key, b"before")
            .expect("the probe is written");
        journal
            .michelson
            .checkpoint(&mut rk)
            .expect("the caller's savepoint is open");

        let result = OperationScope::run(
            &mut rk,
            &mut journal,
            Phase::Application,
            |scope| -> Result<Close<()>, OperationError> {
                let (scoped_rk, scoped_journal) = scope.parts_mut();
                scoped_journal
                    .michelson
                    .checkpoint(scoped_rk)
                    .expect("the copy's savepoint is open");
                assert_eq!(scoped_journal.michelson.savepoint_count(), 2);
                scoped_rk
                    .tez_accounts_mut()
                    .set(&key, b"inside")
                    .expect("the write goes through");
                close
            },
        );

        assert_eq!(rk.tez_accounts_snapshot().len(), 1);
        assert_eq!(
            rk.host().store_has(&COPY_BACKUP).expect("readable"),
            None,
            "no backup of the copy is left"
        );
        assert_eq!(rk.host().store_has(&TMP_PATH).expect("readable"), None);
        let value = rk.tez_accounts().get(&key).map(|v| v.to_vec());
        (result, value, journal.michelson.savepoint_count())
    }

    #[test]
    fn a_commit_with_a_savepoint_left_open_keeps_the_writes() {
        let (result, value, count) =
            run_with_a_savepoint_left_open(Ok(Close::Commit(())));
        assert_eq!(result, Ok(()));
        assert_eq!(value, Some(b"inside".to_vec()));
        assert_eq!(count, 1);
    }

    #[test]
    fn a_failing_body_with_a_savepoint_left_open_drops_the_writes() {
        let (result, value, count) = run_with_a_savepoint_left_open(Err(
            OperationError::BlockAbort("the body fails".into()),
        ));
        assert_eq!(
            result,
            Err(OperationError::BlockAbort("the body fails".into()))
        );
        assert_eq!(value, Some(b"before".to_vec()));
        assert_eq!(count, 1);
    }

    /// A nested copy that the body drops without closing it leaves nothing
    /// behind: the scope keeps only its own copy's writes.
    #[test]
    fn a_nested_copy_dropped_by_the_body_is_discarded() {
        let nested = RefPath::assert_from(b"/tez/tez_accounts/nested");
        let mut host = MockKernelHost::default();
        let mut rk = MockRuntimeKeyspaces::init(&mut host).expect("the handle is built");
        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        rk.host_mut()
            .store_write_all(&probe(), b"before")
            .expect("the probe is written");

        OperationScope::run(
            &mut rk,
            &mut journal,
            Phase::Application,
            |scope| -> Result<Close<()>, OperationError> {
                let (scoped_rk, _) = scope.parts_mut();
                scoped_rk
                    .host_mut()
                    .store_write_all(&probe(), b"outer")
                    .expect("the write goes through");
                let mut copy = scoped_rk.start_safe().expect("the nested copy starts");
                copy.host_mut()
                    .store_write_all(&nested, b"nested")
                    .expect("the write goes through");
                copy.host_mut()
                    .store_write_all(&probe(), b"nested")
                    .expect("the write goes through");
                Ok(Close::Commit(()))
            },
        )
        .expect("the scope is committed");

        assert_eq!(
            rk.host().store_read_all(&probe()).expect("readable"),
            b"outer"
        );
        assert_eq!(rk.host().store_has(&nested).expect("readable"), None);
        assert_eq!(rk.host().store_has(&TMP_PATH).expect("readable"), None);
    }

    /// A savepoint opened in the copy behind the journal's back fails the
    /// release when the scope closes: the scope is reverted, the journal is
    /// reset, and the release error wins over the body's commit.
    #[test]
    fn a_failed_release_reverts_the_scope() {
        use tezosx_journal::OriginalSource;

        let mut host = MockKernelHost::default();
        let mut rk = MockRuntimeKeyspaces::init(&mut host).expect("the handle is built");
        let mut journal = TezosXJournal::mock(RuntimeId::Ethereum);
        rk.host_mut()
            .store_write_all(&probe(), b"before")
            .expect("the probe is written");
        let index_before = journal.michelson.origination_index();

        let result = OperationScope::run(
            &mut rk,
            &mut journal,
            Phase::Application,
            |scope| -> Result<Close<()>, OperationError> {
                let (scoped_rk, scoped_journal) = scope.parts_mut();
                scoped_rk
                    .tez_accounts_savepoint()
                    .expect("the untracked savepoint is open");
                scoped_rk
                    .host_mut()
                    .store_write_all(&probe(), b"inside")
                    .expect("the write goes through");
                scoped_journal.michelson.next_origination_index();
                scoped_journal.evm.set_crac_chain_depth(1);
                scoped_journal.set_original_source(OriginalSource::new(
                    RuntimeId::Tezos,
                    "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx".to_string(),
                ));
                Ok(Close::Commit(()))
            },
        );

        match result {
            Err(OperationError::BlockAbort(msg)) => {
                assert!(msg.starts_with("Batch savepoints release"), "{msg}")
            }
            other => panic!("expected a release error, got {other:?}"),
        }
        assert_eq!(
            rk.host().store_read_all(&probe()).expect("readable"),
            b"before"
        );
        assert_eq!(rk.tez_accounts_snapshot().len(), 0);
        assert_eq!(rk.host().store_has(&COPY_BACKUP).expect("readable"), None);
        assert_eq!(rk.host().store_has(&TMP_PATH).expect("readable"), None);
        assert_eq!(journal.michelson.origination_index(), index_before);
        assert_eq!(journal.evm.crac_chain_depth(), 0);
        assert_eq!(journal.original_source(), None);
    }
}
