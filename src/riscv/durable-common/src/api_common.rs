// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! This module defines common (routing) logic of OCaml API
//! for the RISC-V durable storage system.

use bytes::Bytes;
use octez_riscv_api_common::OcamlFallible;
use octez_riscv_api_common::bytes::BytesWrapper;
use octez_riscv_api_common::move_semantics::MutableState;
use octez_riscv_api_common::safe_pointer::SafePointer;
use octez_riscv_data::foldable::Foldable;
use octez_riscv_data::hash::Hash;
use octez_riscv_data::hash::HashFold;
use octez_riscv_data::mode::Normal;
use octez_riscv_durable_storage::errors as ds_errors;
use octez_riscv_durable_storage::key::Key;
use octez_riscv_durable_storage::registry as ds_registry;
use octez_riscv_durable_storage::storage::KeyValueStore;

use crate::BytesParam;
use crate::KeyParam;
use crate::SplitDsResult;
use crate::map_fallible;
use crate::registry::RegistryState;
use crate::split_ds_errors;

/// Type alias for a mutable registry state backed by a generic key-value store.
pub type DsRegistry<KV, G> = MutableState<RegistryState<KV, G, Normal>>;

/// Compute the hash of the registry state.
#[inline(always)]
pub fn registry_hash<KV, G>(state: SafePointer<DsRegistry<KV, G>>) -> BytesWrapper<Hash>
where
    KV: KeyValueStore + Send + Sync + 'static,
    KV::Repo: Clone + Send + Sync,
    DsRegistry<KV, G>: Sync,
{
    let hash = state.apply_ro(|registry| registry.fold(HashFold));

    BytesWrapper::from(hash)
}

/// Return the number of databases in the registry.
#[inline(always)]
pub fn registry_size<KV, G>(state: SafePointer<DsRegistry<KV, G>>) -> OcamlFallible<u64>
where
    KV: KeyValueStore + Send + Sync + 'static,
    KV::Repo: Clone + Send + Sync,
    DsRegistry<KV, G>: Sync,
{
    let size: usize = state.apply_ro(ds_registry::Registry::len);

    Ok(u64::try_from(size)?)
}

/// Resize the registry to the given number of databases.
#[inline(always)]
pub fn registry_resize<KV, G, Err>(
    state: SafePointer<DsRegistry<KV, G>>,
    size: u64,
) -> SplitDsResult<(), Err>
where
    KV: KeyValueStore + Send + Sync + 'static,
    KV::Repo: Clone + Send + Sync,
    DsRegistry<KV, G>: Sync,
    Err: From<ds_errors::InvalidArgumentError>,
{
    let size = usize::try_from(size)?;
    let res = state.apply(|registry| registry.resize_tick(size))?;

    split_ds_errors(res)
}

/// Copy the database at `src_index` to `dst_index`.
#[inline(always)]
pub fn registry_copy<KV, G, Err>(
    state: SafePointer<DsRegistry<KV, G>>,
    src_index: u64,
    dst_index: u64,
) -> SplitDsResult<(), Err>
where
    KV: KeyValueStore + Send + Sync + 'static,
    KV::Repo: Clone + Send + Sync,
    DsRegistry<KV, G>: Sync,
    Err: From<ds_errors::InvalidArgumentError>,
{
    let src_index = usize::try_from(src_index)?;
    let dst_index = usize::try_from(dst_index)?;

    let res = state.apply(|registry| registry.copy_database(src_index, dst_index))?;

    split_ds_errors(res)
}

/// Move the database at `src_index` to `dst_index`.
#[inline(always)]
pub fn registry_move<KV, G, Err>(
    state: SafePointer<DsRegistry<KV, G>>,
    src_index: u64,
    dst_index: u64,
) -> SplitDsResult<(), Err>
where
    KV: KeyValueStore + Send + Sync + 'static,
    KV::Repo: Clone + Send + Sync,
    DsRegistry<KV, G>: Sync,
    Err: From<ds_errors::InvalidArgumentError>,
{
    let src_index = usize::try_from(src_index)?;
    let dst_index = usize::try_from(dst_index)?;

    let res = state.apply(|registry| registry.move_database(src_index, dst_index))?;

    split_ds_errors(res)
}

/// Clear all entries from the database at `db_index`.
#[inline(always)]
pub fn registry_clear<KV, G, Err>(
    state: SafePointer<DsRegistry<KV, G>>,
    db_index: u64,
) -> SplitDsResult<(), Err>
where
    KV: KeyValueStore + Send + Sync + 'static,
    KV::Repo: Clone + Send + Sync,
    DsRegistry<KV, G>: Sync,
    Err: From<ds_errors::InvalidArgumentError>,
{
    let db_index = usize::try_from(db_index)?;

    let res = state.apply(|registry| registry.clear_database(db_index))?;

    split_ds_errors(res)
}

/// Check whether `key` exists in the database at `db_index`.
#[inline(always)]
pub fn database_exists<KV, G, Err>(
    state: SafePointer<DsRegistry<KV, G>>,
    db_index: u64,
    key: KeyParam,
) -> SplitDsResult<bool, Err>
where
    KV: KeyValueStore + Send + Sync + 'static,
    KV::Repo: Clone + Send + Sync,
    DsRegistry<KV, G>: Sync,
    Err: From<ds_errors::InvalidArgumentError>,
{
    let db_index = usize::try_from(db_index)?;

    let res = state.apply_ro(|registry| {
        let key = Key::try_from(key)?;
        let db = registry.database(db_index)?;

        db.exists(&key)
    });

    split_ds_errors(res)
}

/// Set the value for `key` in the database at `db_index`.
#[inline(always)]
pub fn database_set<KV, G, Err>(
    state: SafePointer<DsRegistry<KV, G>>,
    db_index: u64,
    key: KeyParam,
    value: BytesParam,
) -> SplitDsResult<(), Err>
where
    KV: KeyValueStore + Send + Sync + 'static,
    KV::Repo: Clone + Send + Sync,
    DsRegistry<KV, G>: Sync,
    Err: From<ds_errors::InvalidArgumentError>,
{
    let db_index = usize::try_from(db_index)?;

    let res = state.apply(|registry| {
        let key = Key::try_from(key)?;
        let db = registry.database_mut(db_index)?;
        let data = Bytes::from(value);

        db.set(key, data)
    })?;

    split_ds_errors(res)
}

/// Write `value` at `offset` within `key`'s value in the database at `db_index`.
///
/// Returns the new total length of the value.
#[inline(always)]
pub fn database_write<KV, G, Err>(
    state: SafePointer<DsRegistry<KV, G>>,
    db_index: u64,
    key: KeyParam,
    offset: u64,
    value: BytesParam,
) -> SplitDsResult<u64, Err>
where
    KV: KeyValueStore + Send + Sync + 'static,
    KV::Repo: Clone + Send + Sync,
    DsRegistry<KV, G>: Sync,
    Err: From<ds_errors::InvalidArgumentError>,
{
    let db_index = usize::try_from(db_index)?;
    let offset = usize::try_from(offset)?;

    let res = state.apply(|registry| {
        let key = Key::try_from(key)?;
        let db = registry.database_mut(db_index)?;
        let data = Bytes::from(value);

        db.write(key, offset, data)
    })?;

    let res = split_ds_errors(res)?;
    map_fallible(res, u64::try_from)
}

/// Read `len` bytes starting at `offset` from `key`'s value in the database at `db_index`.
#[inline(always)]
pub fn database_read<KV, G, Err>(
    state: SafePointer<DsRegistry<KV, G>>,
    db_index: u64,
    key: KeyParam,
    offset: u64,
    len: u64,
) -> SplitDsResult<BytesWrapper<Vec<u8>>, Err>
where
    KV: KeyValueStore + Send + Sync + 'static,
    KV::Repo: Clone + Send + Sync,
    DsRegistry<KV, G>: Sync,
    Err: From<ds_errors::InvalidArgumentError>,
{
    let db_index = usize::try_from(db_index)?;
    let offset = usize::try_from(offset)?;
    let len = usize::try_from(len)?;

    // TODO (RV-933): use `read_bytes` to prevent large allocations
    let mut read = vec![0; len];

    let res = state.apply_ro(|registry| {
        let key = Key::try_from(key)?;
        let db = registry.database(db_index)?;

        // Pass a mutable slice to avoid Vec::BufMut which appends
        // instead of writing in-place.
        db.read(&key, offset, read.as_mut_slice())
    });

    let res = split_ds_errors(res)?.map(|read_bytes| {
        read.truncate(read_bytes);
        BytesWrapper::from(read)
    });

    Ok(res)
}

/// Return the byte length of `key`'s value in the database at `db_index`.
#[inline(always)]
pub fn value_length<KV, G, Err>(
    state: SafePointer<DsRegistry<KV, G>>,
    db_index: u64,
    key: KeyParam,
) -> SplitDsResult<u64, Err>
where
    KV: KeyValueStore + Send + Sync + 'static,
    KV::Repo: Clone + Send + Sync,
    DsRegistry<KV, G>: Sync,
    Err: From<ds_errors::InvalidArgumentError>,
{
    let db_index = usize::try_from(db_index)?;

    let res = state.apply_ro(|registry| {
        let key = Key::try_from(key)?;
        let db = registry.database(db_index)?;

        db.value_length(&key)
    });

    let res = split_ds_errors(res)?;
    map_fallible(res, u64::try_from)
}

/// Delete `key` from the database at `db_index`.
#[inline(always)]
pub fn database_delete<KV, G, Err>(
    state: SafePointer<DsRegistry<KV, G>>,
    db_index: u64,
    key: KeyParam,
) -> SplitDsResult<(), Err>
where
    KV: KeyValueStore + Send + Sync + 'static,
    KV::Repo: Clone + Send + Sync,
    DsRegistry<KV, G>: Sync,
    Err: From<ds_errors::InvalidArgumentError>,
{
    let db_index = usize::try_from(db_index)?;

    let res = state.apply(|registry| {
        let key = Key::try_from(key)?;
        let db = registry.database_mut(db_index)?;

        Ok(db.delete(key)?)
    })?;

    split_ds_errors(res)
}

/// Compute the Merkle hash of the database at `db_index`.
#[inline(always)]
pub fn database_hash<KV, G, Err>(
    state: SafePointer<DsRegistry<KV, G>>,
    db_index: u64,
) -> SplitDsResult<BytesWrapper<Hash>, Err>
where
    KV: KeyValueStore + Send + Sync + 'static,
    KV::Repo: Clone + Send + Sync,
    DsRegistry<KV, G>: Sync,
    Err: From<ds_errors::InvalidArgumentError>,
{
    let db_index = usize::try_from(db_index)?;

    let res = state.apply_ro(|registry| {
        let db = registry.database(db_index)?;

        Ok(db.hash()?)
    });

    let res = split_ds_errors(res)?.map(BytesWrapper::from);
    Ok(res)
}
