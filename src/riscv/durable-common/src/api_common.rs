// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! This module defines common (routing) logic of OCaml API
//! for the RISC-V durable storage system.

use std::convert::Infallible;

use bytes::Bytes;
use octez_riscv_api_common::OcamlFallible;
use octez_riscv_api_common::bytes::BytesWrapper;
use octez_riscv_data::components::vector::VectorMode;
use octez_riscv_data::foldable::Foldable;
use octez_riscv_data::hash::Hash;
use octez_riscv_data::hash::HashFold;
use octez_riscv_data::merkle_proof::proof::Proof;
use octez_riscv_data::mode::Mode;
use octez_riscv_data::mode::Normal;
use octez_riscv_data::mode::Prove;
use octez_riscv_durable_storage::database::DatabaseMode;
use octez_riscv_durable_storage::errors as ds_errors;
use octez_riscv_durable_storage::errors::OperationalError;
use octez_riscv_durable_storage::registry as ds_registry;
use octez_riscv_durable_storage::registry::Registry;
use octez_riscv_durable_storage::registry::RegistryMode;
use octez_riscv_durable_storage::storage::KeyValueStore;
use trait_set::trait_set;

use crate::BytesParam;
use crate::KeyParam;
use crate::SplitDsResult;
use crate::map_fallible;
use crate::registry::DsProveRegistry;
use crate::registry::GcNames;
use crate::split_ds_errors;

trait_set! {
    /// [`KeyValueStore`] that can be used in a background thread
    pub trait BackgroundKeyValueStore = KeyValueStore + Send + Sync + 'static;
}

/// Trait allowing specialised implementations for the `MutableState<...<Registry>>` pattern by each mode.
///
/// This mirrors directly the API used by normal mode - with application directly within the current thread,
/// while issuing the operations to a background thread for Prove and Verify.
///
/// # Not-found handling
///
/// Verify mode replays operations against a proof. When an operation touches data that is absent
/// from the proof, the underlying state machine raises a [`NotFound`] panic via
/// [`not_found`](octez_riscv_data::mode::utils::not_found). The Verify implementation catches this
/// in its worker thread (see [`catch_not_found`](octez_riscv_data::mode::utils::catch_not_found))
/// and surfaces it through [`Self::NotFoundError`].
///
/// This does not occur for Normal and Prove mode (the full state is always available) so their
/// [`Self::NotFoundError`] is [`Infallible`].
pub trait RegistryApply<KV: KeyValueStore, M: Mode> {
    /// Deterministic divergence error captured while applying an operation.
    ///
    /// [`Infallible`] for Normal/Prove; [`NotFound`] for Verify.
    type NotFoundError;

    /// Apply a mutable operation over the contained registry. If the mutable state is 'borrowed', this
    /// will result in the underlying state being cloned, which may fail.
    fn apply<F, R>(&self, fun: F) -> Result<Result<R, Self::NotFoundError>, OperationalError>
    where
        F: FnOnce(&mut Registry<KV, M>) -> R + Send + 'static,
        R: Send + 'static,
        KV::Repo: Clone;

    /// Apply a readonly operation over the contained registry.
    fn apply_ro<F, R>(&self, fun: F) -> Result<Result<R, Self::NotFoundError>, OperationalError>
    where
        F: FnOnce(&Registry<KV, M>) -> R + Send + 'static,
        R: Send + 'static;
}

/// Construct a new Key, returning the correct error variant
/// of [`SplitDsResult`].
///
/// Use this rather than [`Key::try_from`]. Accidentally using the try operator (`?`) with
/// `Key::try_from` leads to invalid keys raising an Ocaml exception, rather than returning
/// the expected [`InvalidArgumentError`] error as part of the Ocaml result.
///
/// [`Key::try_from`]: octez_riscv_durable_storage::key::Key
/// [`InvalidArgumentError`]: ds_errors::InvalidArgumentError
macro_rules! key_from {
    ($key_param:expr, $err:ty) => {{
        use octez_riscv_durable_storage::key::Key;

        match Key::try_from($key_param) {
            Ok(key) => key,
            Err(err) => return Ok(Err(<$err>::from(err))),
        }
    }};
}

/// Compute the hash of the registry state.
///
/// Only available for modes that cannot diverge from a proof (Normal/Prove), and for whom
/// `Registry` implements `Foldable<HashFold>` (which again are Normal/Prove only)
#[inline(always)]
pub fn registry_hash<KV, M, S>(state: &S) -> OcamlFallible<BytesWrapper<Hash>>
where
    KV: BackgroundKeyValueStore,
    KV::Repo: Clone + Send + Sync,
    M: Mode,
    S: RegistryApply<KV, M, NotFoundError = Infallible>,
    ds_registry::Registry<KV, M>: Foldable<HashFold>,
{
    let Ok(hash) = state.apply_ro(move |registry| registry.fold(HashFold))?;

    Ok(BytesWrapper::from(hash))
}

/// Return the number of databases in the registry.
#[inline(always)]
pub fn registry_size<KV, M, S>(state: &S) -> OcamlFallible<Result<u64, S::NotFoundError>>
where
    KV: BackgroundKeyValueStore,
    KV::Repo: Send + Sync,
    M: VectorMode + 'static,
    S: RegistryApply<KV, M>,
{
    let res = match state.apply_ro(ds_registry::Registry::len)? {
        Ok(size) => Ok(u64::try_from(size)?),
        Err(not_found) => Err(not_found),
    };

    Ok(res)
}

/// Resize the registry to the given number of databases.
#[inline(always)]
pub fn registry_resize<KV, Err, M, S>(state: &S, size: u64) -> SplitDsResult<(), Err>
where
    KV: BackgroundKeyValueStore,
    KV::Repo: Clone + Send + Sync,
    Err: From<ds_errors::InvalidArgumentError> + From<S::NotFoundError>,
    M: RegistryMode + VectorMode,
    S: RegistryApply<KV, M>,
{
    let size = usize::try_from(size)?;
    let res = state.apply(move |registry| registry.resize_tick(size))?;

    split_ds_errors(res)
}

/// Copy the database at `src_index` to `dst_index`.
#[inline(always)]
pub fn registry_copy<KV, Err, M, S>(
    state: &S,
    src_index: u64,
    dst_index: u64,
) -> SplitDsResult<(), Err>
where
    KV: BackgroundKeyValueStore,
    KV::Repo: Clone + Send + Sync,
    Err: From<ds_errors::InvalidArgumentError> + From<S::NotFoundError>,
    M: RegistryMode + VectorMode,
    S: RegistryApply<KV, M>,
{
    let src_index = usize::try_from(src_index)?;
    let dst_index = usize::try_from(dst_index)?;

    let res = state.apply(move |registry| registry.copy_database(src_index, dst_index))?;

    split_ds_errors(res)
}

/// Move the database at `src_index` to `dst_index`.
#[inline(always)]
pub fn registry_move<KV, Err, M, S>(
    state: &S,
    src_index: u64,
    dst_index: u64,
) -> SplitDsResult<(), Err>
where
    KV: BackgroundKeyValueStore,
    KV::Repo: Clone + Send + Sync,
    Err: From<ds_errors::InvalidArgumentError> + From<S::NotFoundError>,
    M: RegistryMode + VectorMode,
    S: RegistryApply<KV, M>,
{
    let src_index = usize::try_from(src_index)?;
    let dst_index = usize::try_from(dst_index)?;

    let res = state.apply(move |registry| registry.move_database(src_index, dst_index))?;

    split_ds_errors(res)
}

/// Clear all entries from the database at `db_index`.
#[inline(always)]
pub fn registry_clear<KV, Err, M, S>(state: &S, db_index: u64) -> SplitDsResult<(), Err>
where
    KV: BackgroundKeyValueStore,
    KV::Repo: Clone + Send + Sync,
    Err: From<ds_errors::InvalidArgumentError> + From<S::NotFoundError>,
    M: RegistryMode + VectorMode,
    S: RegistryApply<KV, M>,
{
    let db_index = usize::try_from(db_index)?;

    let res = state.apply(move |registry| registry.clear_database(db_index))?;

    split_ds_errors(res)
}

/// Check whether `key` exists in the database at `db_index`.
#[inline(always)]
pub fn database_exists<KV, Err, M, S>(
    state: &S,
    db_index: u64,
    key: KeyParam,
) -> SplitDsResult<bool, Err>
where
    KV: BackgroundKeyValueStore,
    KV::Repo: Send + Sync,
    Err: From<ds_errors::InvalidArgumentError> + From<S::NotFoundError>,
    M: VectorMode + DatabaseMode,
    S: RegistryApply<KV, M>,
{
    let db_index = usize::try_from(db_index)?;
    let key = key_from!(key, Err);

    let res = state.apply_ro(move |registry| {
        let db = registry.database(db_index)?;

        db.exists(&key)
    })?;

    split_ds_errors(res)
}

/// Set the value for `key` in the database at `db_index`.
#[inline(always)]
pub fn database_set<KV, Err, M, S>(
    state: &S,
    db_index: u64,
    key: KeyParam,
    value: BytesParam,
) -> SplitDsResult<(), Err>
where
    KV: BackgroundKeyValueStore,
    KV::Repo: Clone + Send + Sync,
    Err: From<ds_errors::InvalidArgumentError> + From<S::NotFoundError>,
    M: VectorMode + DatabaseMode,
    S: RegistryApply<KV, M>,
{
    let db_index = usize::try_from(db_index)?;
    let key = key_from!(key, Err);
    let data = Bytes::from(value);

    let res = state.apply(move |registry| {
        let db = registry.database_mut(db_index)?;

        db.set(key, data)
    })?;

    split_ds_errors(res)
}

/// Write `value` at `offset` within `key`'s value in the database at `db_index`.
///
/// Returns the new total length of the value.
#[inline(always)]
pub fn database_write<KV, Err, M, S>(
    state: &S,
    db_index: u64,
    key: KeyParam,
    offset: u64,
    value: BytesParam,
) -> SplitDsResult<u64, Err>
where
    KV: BackgroundKeyValueStore,
    KV::Repo: Clone + Send + Sync,
    Err: From<ds_errors::InvalidArgumentError> + From<S::NotFoundError>,
    M: VectorMode + DatabaseMode,
    S: RegistryApply<KV, M>,
{
    let db_index = usize::try_from(db_index)?;
    let offset = usize::try_from(offset)?;
    let key = key_from!(key, Err);
    let data = Bytes::from(value);

    let res = state.apply(move |registry| {
        let db = registry.database_mut(db_index)?;

        db.write(key, offset, data)
    })?;

    let res = split_ds_errors(res)?;
    map_fallible(res, u64::try_from)
}

/// Read `len` bytes starting at `offset` from `key`'s value in the database at `db_index`.
#[inline(always)]
pub fn database_read<KV, Err, M, S>(
    state: &S,
    db_index: u64,
    key: KeyParam,
    offset: u64,
    len: u64,
) -> SplitDsResult<BytesWrapper<Vec<u8>>, Err>
where
    KV: BackgroundKeyValueStore,
    KV::Repo: Send + Sync,
    Err: From<ds_errors::InvalidArgumentError> + From<S::NotFoundError>,
    M: VectorMode + DatabaseMode,
    S: RegistryApply<KV, M>,
{
    let db_index = usize::try_from(db_index)?;
    let offset = usize::try_from(offset)?;
    let len = usize::try_from(len)?;
    let key = key_from!(key, Err);

    // TODO (RV-933): use `read_bytes` to prevent large allocations
    let mut read = vec![0; len];

    let res = state.apply_ro(move |registry| {
        let db = registry.database(db_index)?;

        // Pass a mutable slice to avoid Vec::BufMut which appends
        // instead of writing in-place.
        db.read(&key, offset, read.as_mut_slice())
            .map(|read_bytes| {
                read.truncate(read_bytes);
                BytesWrapper::from(read)
            })
    })?;

    let res = split_ds_errors(res)?;

    Ok(res)
}

/// Return the byte length of `key`'s value in the database at `db_index`.
#[inline(always)]
pub fn value_length<KV, Err, M, S>(
    state: &S,
    db_index: u64,
    key: KeyParam,
) -> SplitDsResult<u64, Err>
where
    KV: BackgroundKeyValueStore,
    KV::Repo: Send + Sync,
    Err: From<ds_errors::InvalidArgumentError> + From<S::NotFoundError>,
    M: VectorMode + DatabaseMode,
    S: RegistryApply<KV, M>,
{
    let db_index = usize::try_from(db_index)?;
    let key = key_from!(key, Err);

    let res = state.apply_ro(move |registry| {
        let db = registry.database(db_index)?;

        db.value_length(&key)
    })?;

    let res = split_ds_errors(res)?;
    map_fallible(res, u64::try_from)
}

/// Delete `key` from the database at `db_index`.
#[inline(always)]
pub fn database_delete<KV, Err, M, S>(
    state: &S,
    db_index: u64,
    key: KeyParam,
) -> SplitDsResult<(), Err>
where
    KV: BackgroundKeyValueStore,
    KV::Repo: Clone + Send + Sync,
    Err: From<ds_errors::InvalidArgumentError> + From<S::NotFoundError>,
    M: VectorMode + DatabaseMode,
    S: RegistryApply<KV, M>,
{
    let db_index = usize::try_from(db_index)?;
    let key = key_from!(key, Err);

    let res = state.apply(move |registry| {
        let db = registry.database_mut(db_index)?;

        Ok(db.delete(key)?)
    })?;

    split_ds_errors(res)
}

/// Compute the Merkle hash of the database at `db_index`.
#[inline(always)]
pub fn database_hash<KV, Err, M, S>(
    state: &S,
    db_index: u64,
) -> SplitDsResult<BytesWrapper<Hash>, Err>
where
    KV: BackgroundKeyValueStore,
    KV::Repo: Send + Sync,
    Err: From<ds_errors::InvalidArgumentError> + From<S::NotFoundError>,
    M: VectorMode + DatabaseMode,
    S: RegistryApply<KV, M>,
{
    let db_index = usize::try_from(db_index)?;

    let res = state.apply_ro(move |registry| {
        let db = registry.database(db_index)?;

        Ok(db.hash()?)
    })?;

    let res = split_ds_errors(res)?.map(BytesWrapper::from);
    Ok(res)
}

/// Convert a normal mode registry into a proof mode one
pub fn start_proof<KV, GcProve>(
    state: &impl RegistryApply<KV, Normal, NotFoundError = Infallible>,
) -> OcamlFallible<DsProveRegistry<KV, GcProve>>
where
    KV: BackgroundKeyValueStore,
    KV::Repo: Clone + Send + Sync,
    GcProve: GcNames,
{
    let Ok(prove_res) = state.apply_ro(|registry| DsProveRegistry::try_from(registry))?;

    Ok(prove_res?)
}

/// Produce a proof from the current state of a prove mode registry
pub fn produce_proof<KV>(
    state: &impl RegistryApply<KV, Prove<'static>, NotFoundError = Infallible>,
) -> OcamlFallible<Proof>
where
    KV: BackgroundKeyValueStore,
{
    let Ok(proof) = state.apply_ro(|registry| registry.produce_proof())?;

    Ok(proof)
}
