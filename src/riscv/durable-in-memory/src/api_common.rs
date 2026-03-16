// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! This module defines common (routing) logic of OCaml API
//! for the RISC-V durable storage system.

use bytes::Bytes;
use octez_riscv_api_common::OcamlFallible;
use octez_riscv_api_common::bytes::BytesWrapper;
use octez_riscv_api_common::safe_pointer::SafePointer;
use octez_riscv_data::foldable::Foldable;
use octez_riscv_data::hash::Hash;
use octez_riscv_data::hash::HashFold;
use octez_riscv_durable_storage::key::Key;
use octez_riscv_durable_storage::registry as ds_registry;

use crate::BytesParam;
use crate::KeyParam;
use crate::Registry;
use crate::SplitDsResult;
use crate::map_fallible;
use crate::split_ds_errors;

#[inline(always)]
pub fn registry_hash(state: SafePointer<Registry>) -> BytesWrapper<Hash> {
    let hash = state.apply_ro(|registry| registry.fold(HashFold));

    BytesWrapper::from(hash)
}

#[inline(always)]
pub fn registry_size(state: SafePointer<Registry>) -> OcamlFallible<u64> {
    let size: usize = state.apply_ro(ds_registry::Registry::len);

    Ok(u64::try_from(size)?)
}

#[inline(always)]
pub fn registry_resize(state: SafePointer<Registry>, size: u64) -> SplitDsResult<()> {
    let size = usize::try_from(size)?;
    let res = state.apply(|registry| registry.resize_tick(size))?;

    split_ds_errors(res)
}

#[inline(always)]
pub fn registry_copy(
    state: SafePointer<Registry>,
    src_index: u64,
    dst_index: u64,
) -> SplitDsResult<()> {
    let src_index = usize::try_from(src_index)?;
    let dst_index = usize::try_from(dst_index)?;

    let res = state.apply(|registry| registry.copy_database(src_index, dst_index))?;

    split_ds_errors(res)
}

#[inline(always)]
pub fn registry_move(
    state: SafePointer<Registry>,
    src_index: u64,
    dst_index: u64,
) -> SplitDsResult<()> {
    let src_index = usize::try_from(src_index)?;
    let dst_index = usize::try_from(dst_index)?;

    let res = state.apply(|registry| registry.move_database(src_index, dst_index))?;

    split_ds_errors(res)
}

#[inline(always)]
pub fn registry_clear(state: SafePointer<Registry>, db_index: u64) -> SplitDsResult<()> {
    let db_index = usize::try_from(db_index)?;

    let res = state.apply(|registry| registry.clear_database(db_index))?;

    split_ds_errors(res)
}

#[inline(always)]
pub fn database_exists(
    state: SafePointer<Registry>,
    db_index: u64,
    key: KeyParam,
) -> SplitDsResult<bool> {
    let db_index = usize::try_from(db_index)?;

    let res = state.apply_ro(|registry| {
        let key = Key::try_from(key)?;
        let db = registry.database(db_index)?;

        db.exists(&key)
    });

    split_ds_errors(res)
}

#[inline(always)]
pub fn database_set(
    state: SafePointer<Registry>,
    db_index: u64,
    key: KeyParam,
    value: BytesParam,
) -> SplitDsResult<()> {
    let db_index = usize::try_from(db_index)?;

    let res = state.apply(|registry| {
        let key = Key::try_from(key)?;
        let db = registry.database_mut(db_index)?;
        let data = Bytes::from(value);

        db.set(key, data)
    })?;

    split_ds_errors(res)
}

#[inline(always)]
pub fn database_write(
    state: SafePointer<Registry>,
    db_index: u64,
    key: KeyParam,
    offset: u64,
    value: BytesParam,
) -> SplitDsResult<u64> {
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

#[inline(always)]
pub fn database_read(
    state: SafePointer<Registry>,
    db_index: u64,
    key: KeyParam,
    offset: u64,
    len: u64,
) -> SplitDsResult<BytesWrapper<Vec<u8>>> {
    let db_index = usize::try_from(db_index)?;
    let offset = usize::try_from(offset)?;
    let len = usize::try_from(len)?;

    // TODO (RV-933): use `read_bytes` to prevent large allocations
    let mut read = vec![0; len];

    let res = state.apply_ro(|registry| {
        let key = Key::try_from(key)?;
        let db = registry.database(db_index)?;

        db.read(&key, offset, &mut read)
    });

    let res = split_ds_errors(res)?.map(|read_bytes| {
        read.truncate(read_bytes);
        BytesWrapper::from(read)
    });

    Ok(res)
}

#[inline(always)]
pub fn value_length(
    state: SafePointer<Registry>,
    db_index: u64,
    key: KeyParam,
) -> SplitDsResult<u64> {
    let db_index = usize::try_from(db_index)?;

    let res = state.apply_ro(|registry| {
        let key = Key::try_from(key)?;
        let db = registry.database(db_index)?;

        db.value_length(&key)
    });

    let res = split_ds_errors(res)?;
    map_fallible(res, u64::try_from)
}

#[inline(always)]
pub fn database_delete(
    state: SafePointer<Registry>,
    db_index: u64,
    key: KeyParam,
) -> SplitDsResult<()> {
    let db_index = usize::try_from(db_index)?;

    let res = state.apply(|registry| {
        let key = Key::try_from(key)?;
        let db = registry.database_mut(db_index)?;

        db.delete(key)
    })?;

    split_ds_errors(res)
}

#[inline(always)]
pub fn database_hash(
    state: SafePointer<Registry>,
    db_index: u64,
) -> SplitDsResult<BytesWrapper<Hash>> {
    let db_index = usize::try_from(db_index)?;

    let res = state.apply_ro(|registry| {
        let db = registry.database(db_index)?;

        Ok(db.hash()?)
    });

    let res = split_ds_errors(res)?.map(BytesWrapper::from);
    Ok(res)
}
