// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! This module defines the OCaml API for the RISC-V durable storage system (in memory variant).
//!
//! # Error handling
//!
//! There are two kinds of errors that may occur when dealing with Durable Storage:
//! - [`OperationalError`]
//! - [`InvalidArgumentError`]
//!
//! [`OperationalError`]: ds_errors::OperationalError
//! [`InvalidArgumentError`]: ds_errors::InvalidArgumentError
//!
//! The former are 'transient' and/or machine specific, the later are _deterministic_ and purely
//! arise as a result of logically incorrect arguments being passed.
//!
//! A good example of operational errors would be those arising from 'something going wrong' in
//! any persistence layers, that may or may not recurr on the same machine if the operation is
//! attempted again. Even if it does, it _may not_ occur on another machine. For this reason,
//! such errors are surfaced as OCaml Exceptions. Note that errors arising from converting between
//! 64-bit integers and `usize` are operational errors: they will always succeed on 64-bit machines,
//! but may fail on other architectures.
//!
//! Invalid argument errors more refer to attempting to read/write _beyond the end_ of a value, or
//! attempting to perform an operation on a database that doesn't exist, and such like.

use bytes::Bytes;
use octez_riscv_api_common::OcamlFallible;
use octez_riscv_api_common::bytes::BytesWrapper;
use octez_riscv_api_common::move_semantics::CustomGcResource;
use octez_riscv_api_common::move_semantics::MutableState;
use octez_riscv_api_common::safe_pointer::SafePointer;
use octez_riscv_api_common::try_clone::TryClone;
use octez_riscv_data::foldable::Foldable;
use octez_riscv_data::hash::Hash;
use octez_riscv_data::hash::HashFold;
use octez_riscv_data::mode::Normal;
use octez_riscv_durable_storage::errors as ds_errors;
use octez_riscv_durable_storage::key::Key;
use octez_riscv_durable_storage::registry;
use octez_riscv_durable_storage::storage::in_memory::InMemoryKeyValueStore;
use octez_riscv_durable_storage::storage::in_memory::InMemoryRepo;

/// Wrapper to enable customing OCaml GC's resource tracking.
#[repr(transparent)]
pub struct RegistryState(registry::Registry<InMemoryKeyValueStore, Normal>);

impl RegistryState {
    /// Construct a new in-memory registry
    pub fn new() -> Result<Self, ds_errors::OperationalError> {
        let reg = registry::Registry::new(InMemoryRepo)?;
        Ok(Self(reg))
    }
}

impl std::ops::Deref for RegistryState {
    type Target = registry::Registry<InMemoryKeyValueStore, Normal>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for RegistryState {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl TryClone for RegistryState {
    type Error = ds_errors::OperationalError;

    fn try_clone(&self) -> Result<Self, Self::Error> {
        Ok(RegistryState(self.0.try_clone()?))
    }
}

impl CustomGcResource for RegistryState {
    const IMMUTABLE_NAME: &'static str = "riscv.imm.registry_state";

    const MUTABLE_NAME: &'static str = "riscv.mut.registry_state";
}

#[ocaml::sig]
pub type Registry = MutableState<RegistryState>;

#[derive(ocaml::FromValue, ocaml::ToValue)]
#[ocaml::sig(
    "Key_not_found | Key_too_long | Offset_too_large | Database_index_out_of_bounds | Registry_resize_too_large"
)]
pub enum InvalidArgumentError {
    KeyNotFound,
    KeyTooLong,
    OffsetTooLarge,
    DatabaseIndexOutOfBounds,
    RegistryResizeTooLarge,
}

impl From<ds_errors::InvalidArgumentError> for InvalidArgumentError {
    fn from(value: ds_errors::InvalidArgumentError) -> Self {
        match value {
            ds_errors::InvalidArgumentError::KeyNotFound => Self::KeyNotFound,
            ds_errors::InvalidArgumentError::KeyTooLong => Self::KeyTooLong,
            ds_errors::InvalidArgumentError::OffsetTooLarge => Self::OffsetTooLarge,
            ds_errors::InvalidArgumentError::DatabaseIndexOutOfBounds => {
                Self::DatabaseIndexOutOfBounds
            }
            ds_errors::InvalidArgumentError::RegistryResizeTooLarge => Self::RegistryResizeTooLarge,
        }
    }
}

/// KeyParam receiving a `bytes` value from OCaml.
#[derive(ocaml::FromValue)]
pub struct KeyParam<'a>(&'a [u8]);

impl<'a> TryFrom<KeyParam<'a>> for Key {
    type Error = ds_errors::InvalidArgumentError;

    fn try_from(value: KeyParam) -> Result<Self, Self::Error> {
        Key::new(value.0)
    }
}

/// BytesParam receiving a `bytes` value from OCaml.
#[derive(ocaml::FromValue)]
pub struct BytesParam<'a>(&'a [u8]);

impl<'a> From<BytesParam<'a>> for Bytes {
    fn from(value: BytesParam<'a>) -> Self {
        Bytes::copy_from_slice(value.0)
    }
}

#[ocaml::func]
#[ocaml::sig("unit -> registry")]
pub fn octez_riscv_durable_in_memory_registry_new() -> OcamlFallible<SafePointer<Registry>> {
    let registry = RegistryState::new()?;

    Ok(SafePointer::from(MutableState::owned(registry)))
}

#[ocaml::func]
#[ocaml::sig("registry -> bytes")]
pub fn octez_riscv_durable_in_memory_registry_hash(
    state: SafePointer<Registry>,
) -> BytesWrapper<Hash> {
    let hash = state.apply_ro(|registry| registry.fold(HashFold));

    BytesWrapper::from(hash)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64")]
pub fn octez_riscv_durable_in_memory_registry_size(state: SafePointer<Registry>) -> i64 {
    state.apply_ro(registry::Registry::len) as i64
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_registry_resize(
    state: SafePointer<Registry>,
    size: u64,
) -> SplitDsResult<()> {
    let size = usize::try_from(size)?;
    let res = state.apply(|registry| registry.resize_tick(size))?;

    split_ds_errors(res)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_registry_copy(
    state: SafePointer<Registry>,
    src_index: i64,
    dst_index: i64,
) -> SplitDsResult<()> {
    let res =
        state.apply(|registry| registry.copy_database(src_index as usize, dst_index as usize))?;

    split_ds_errors(res)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_registry_move(
    state: SafePointer<Registry>,
    src_index: i64,
    dst_index: i64,
) -> SplitDsResult<()> {
    let res =
        state.apply(|registry| registry.move_database(src_index as usize, dst_index as usize))?;

    split_ds_errors(res)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_registry_clear(
    state: SafePointer<Registry>,
    db_index: u64,
) -> SplitDsResult<()> {
    let db_index = usize::try_from(db_index)?;

    let res = state.apply(|registry| registry.clear_database(db_index))?;

    split_ds_errors(res)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> bytes -> (bool, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_database_exists(
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

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> bytes -> bytes -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_database_set(
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

#[ocaml::func]
#[ocaml::sig(
    "registry -> int64 -> bytes -> int64 -> bytes -> (int64, invalid_argument_error) result"
)]
pub fn octez_riscv_durable_in_memory_database_write(
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

#[ocaml::func]
#[ocaml::sig(
    "registry -> int64 -> bytes -> int64 -> int64 -> (bytes, invalid_argument_error) result"
)]
pub fn octez_riscv_durable_in_memory_database_read(
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

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> bytes -> (int64, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_database_value_length(
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

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> bytes -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_database_delete(
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

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> (bytes, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_database_hash(
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

/// Split handling of durable storage errors.
///
/// - Operational errors are converted to OCaml exceptions. These *must not* be returned to the kernel.
/// - InvalidArgument errors are returned as the error variant of an OCaml result. These need to be returned
///   to the kernel.
pub type SplitDsResult<T> = OcamlFallible<Result<T, InvalidArgumentError>>;

fn split_ds_errors<T>(res: Result<T, ds_errors::Error>) -> SplitDsResult<T> {
    let inner_res = match res {
        Ok(inner) => Ok(inner),
        Err(ds_errors::Error::InvalidArgument(err)) => Err(err.into()),
        Err(ds_errors::Error::Operational(err)) => return Err(err.into()),
    };

    Ok(inner_res)
}

/// Map a fallible operation over inner value. Errors in mapping are converted to OCaml exceptions.
fn map_fallible<T, U, E: 'static + std::error::Error>(
    res: Result<T, InvalidArgumentError>,
    map: impl Fn(T) -> Result<U, E>,
) -> SplitDsResult<U> {
    let inner_res = match res {
        Ok(inner) => Ok(map(inner)?),
        Err(err) => Err(err),
    };

    Ok(inner_res)
}
