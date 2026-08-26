// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Immutable registry state - the read-only half of the durable storage OCaml API.
//!
//! An immutable registry reaches OCaml as a single opaque handle, but it has two provenances:
//!
//! - a **snapshot** of a live registry, taken by [`ImmRegistryState::snapshot`]. It shares the
//!   working state copy-on-write, and so observes operations that are not committed yet.
//! - a **committed** state, read where it lies by [`ImmRegistryState::checkout_read_only`]. It
//!   makes no working copy at all: no databases are copied out of their commits, no Merkle tree is
//!   loaded and no worker thread is started.
//!
//! Both serve the same reads, so the distinction stays inside Rust. It resurfaces only in
//! [`ImmRegistryState::to_mut_state`], which is free for a snapshot and pays for the working copy
//! for a committed state.

use std::convert::Infallible;

use octez_riscv_api_common::OcamlFallible;
use octez_riscv_api_common::bytes::BytesWrapper;
use octez_riscv_api_common::move_semantics::CustomGcResource;
use octez_riscv_api_common::move_semantics::ImmutableState;
use octez_riscv_api_common::move_semantics::MutableState;
use octez_riscv_data::hash::Hash;
use octez_riscv_data::mode::Normal;
use octez_riscv_durable_storage::commit::CommitId;
use octez_riscv_durable_storage::errors as ds_errors;
use octez_riscv_durable_storage::errors::OperationalError;
use octez_riscv_durable_storage::registry::Registry;
use octez_riscv_durable_storage::repo::RegistryRepo;
use octez_riscv_durable_storage::storage::ReadOnlyKeyValueStore;

use crate::KeyParam;
use crate::SplitDsResult;
use crate::api_common;
use crate::api_common::BackgroundPersistentKeyValueStore;
use crate::api_common::BackgroundReadableKeyValueStore;
use crate::api_common::ReadableRegistryApply;
use crate::registry::DsRegistry;
use crate::registry::GcNames;
use crate::registry::RegistryState;

/// An [`ImmutableState`] can serve reads directly: it holds the registry behind an [`Arc`] and
/// never needs to clone it.
///
/// There is no [`RegistryApply`] counterpart - applying a mutating operation to an immutable state
/// produces a *new* state rather than modifying this one, which is
/// [`ImmutableState::apply`]'s own signature rather than a registry operation.
///
/// [`Arc`]: std::sync::Arc
/// [`RegistryApply`]: crate::api_common::RegistryApply
impl<KV, G> ReadableRegistryApply<KV, Normal> for ImmutableState<RegistryState<KV, G>>
where
    KV: BackgroundReadableKeyValueStore,
    KV::Repo: Send + Sync,
{
    type NotFoundError = Infallible;

    fn apply_ro<F, R>(&self, fun: F) -> Result<Result<R, Infallible>, OperationalError>
    where
        F: FnOnce(&Registry<KV, Normal>) -> R + Send + 'static,
        R: Send + 'static,
    {
        Ok(Ok(ImmutableState::apply_ro(self, fun)))
    }
}

/// A registry that cannot be written to, whatever it was made from.
///
/// `RO` is the read-only store; the writeable store a snapshot is taken over is derived from it as
/// [`ReadOnlyKeyValueStore::Writeable`], so the two variants are guaranteed to share a repository
/// type.
///
/// The variants are not visible to OCaml, which sees one opaque handle. Each holds its own
/// [`ImmutableState`] - rather than the enum sitting inside a single one - so that a snapshot's
/// [`Arc`] can be handed straight to [`MutableState::borrowed`] by
/// [`ImmRegistryState::to_mut_state`], keeping the copy-on-write behaviour of the snapshot path.
///
/// [`Arc`]: std::sync::Arc
pub enum ImmRegistryState<RO: ReadOnlyKeyValueStore, G> {
    /// Copy-on-write snapshot of a working registry.
    Snapshot(ImmutableState<RegistryState<RO::Writeable, G>>),

    /// A commit, read where it lies.
    Committed(ImmutableState<RegistryState<RO, G>>),
}

/// Run `$body` against whichever variant `$state` is bound from.
///
/// A macro rather than a function taking a closure: the two arms bind states of different types -
/// one over the writeable store, one over the read-only store - so the body has to be type-checked
/// once per arm.
macro_rules! dispatch {
    ($self:expr, |$state:ident| $body:expr) => {
        match $self {
            ImmRegistryState::Snapshot($state) => $body,
            ImmRegistryState::Committed($state) => $body,
        }
    };
}

impl<RO, G> ImmRegistryState<RO, G>
where
    RO: ReadOnlyKeyValueStore + BackgroundReadableKeyValueStore,
    RO::Repo: RegistryRepo + Send + Sync,
    RO::Writeable: BackgroundPersistentKeyValueStore,
    G: GcNames,
{
    /// Read the registry committed under `commit_id`, leaving it on disk where it lies.
    ///
    /// Where [`Registry::checkout`] copies every database commit into a working state, this one
    /// copies nothing.
    #[inline(always)]
    pub fn checkout_read_only(
        repo: RO::Repo,
        commit_id: CommitId,
    ) -> Result<Self, ds_errors::Error> {
        let registry = Registry::<RO, Normal>::checkout_read_only(repo, commit_id)?;

        Ok(Self::Committed(ImmutableState::new(RegistryState::from(
            registry,
        ))))
    }

    /// Capture the current contents of a live registry, including uncommitted operations.
    ///
    /// Copy-on-write: the snapshot shares the working state, and whichever side mutates first
    /// pays for the copy.
    #[inline(always)]
    pub fn snapshot(state: &DsRegistry<RO::Writeable, G>) -> Result<Self, OperationalError> {
        Ok(Self::Snapshot(state.to_imm_state()?))
    }

    /// Recover a live, writeable registry.
    ///
    /// Free for a snapshot - an [`Arc`] borrow whose copy is deferred to the next mutation. For a
    /// committed state this is where the working copy is finally paid for: every database is
    /// copied out of its commit and a Merkle worker is started over it.
    ///
    /// [`Arc`]: std::sync::Arc
    #[inline(always)]
    pub fn to_mut_state(&self) -> Result<DsRegistry<RO::Writeable, G>, OperationalError> {
        match self {
            Self::Snapshot(imm) => Ok(imm.to_mut_state()),
            Self::Committed(imm) => {
                let registry =
                    imm.apply_ro(|registry: &Registry<RO, Normal>| registry.to_writeable())?;

                Ok(MutableState::owned(RegistryState::from(registry)))
            }
        }
    }

    /// Compute the Merkle root hash of the registry.
    ///
    /// For a committed state this is the commit id it was checked out under - known without
    /// touching the data.
    #[inline(always)]
    pub fn hash(&self) -> OcamlFallible<BytesWrapper<Hash>> {
        dispatch!(self, |state| api_common::registry_hash(state))
    }

    /// Return the number of databases in the registry.
    #[inline(always)]
    pub fn size(&self) -> OcamlFallible<Result<u64, Infallible>> {
        dispatch!(self, |state| api_common::registry_size(state))
    }

    /// Check whether `key` exists in the database at `db_index`.
    #[inline(always)]
    pub fn database_exists<Err>(&self, db_index: u64, key: KeyParam) -> SplitDsResult<bool, Err>
    where
        Err: From<ds_errors::InvalidArgumentError> + From<Infallible>,
    {
        dispatch!(self, |state| api_common::database_exists(
            state, db_index, key
        ))
    }

    /// Read `len` bytes at `offset` from `key`'s value in the database at `db_index`.
    #[inline(always)]
    pub fn database_read<Err>(
        &self,
        db_index: u64,
        key: KeyParam,
        offset: u64,
        len: u64,
    ) -> SplitDsResult<BytesWrapper<Vec<u8>>, Err>
    where
        Err: From<ds_errors::InvalidArgumentError> + From<Infallible>,
    {
        dispatch!(self, |state| api_common::database_read(
            state, db_index, key, offset, len
        ))
    }

    /// Return the byte length of `key`'s value in the database at `db_index`.
    #[inline(always)]
    pub fn value_length<Err>(&self, db_index: u64, key: KeyParam) -> SplitDsResult<u64, Err>
    where
        Err: From<ds_errors::InvalidArgumentError> + From<Infallible>,
    {
        dispatch!(self, |state| api_common::value_length(state, db_index, key))
    }

    /// Compute the Merkle hash of the database at `db_index`.
    #[inline(always)]
    pub fn database_hash<Err>(&self, db_index: u64) -> SplitDsResult<BytesWrapper<Hash>, Err>
    where
        Err: From<ds_errors::InvalidArgumentError> + From<Infallible>,
    {
        dispatch!(self, |state| api_common::database_hash(state, db_index))
    }
}

// The immutable registry is what OCaml holds, so it is the type that carries the immutable custom
// block's name. The `ImmutableState`s inside the variants never reach OCaml.
octez_riscv_api_common::impl_ocaml_custom! {
    impl [RO, G] ImmRegistryState<RO, G>
    where [RO: ReadOnlyKeyValueStore, G: GcNames]
    {
        name: G::IMMUTABLE_NAME,
        used: <RegistryState<RO, G> as CustomGcResource>::IMMUTABLE_USED,
        max: <RegistryState<RO, G> as CustomGcResource>::IMMUTABLE_MAX,
    }
}
