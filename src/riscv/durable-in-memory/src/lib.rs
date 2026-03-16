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

mod api_common;
mod registry;

use bytes::Bytes;
use octez_riscv_api_common::OcamlFallible;
use octez_riscv_api_common::bytes::BytesWrapper;
use octez_riscv_api_common::move_semantics::MutableState;
use octez_riscv_api_common::safe_pointer::SafePointer;
use octez_riscv_data::hash::Hash;
use octez_riscv_data::mode::Normal;
use octez_riscv_durable_storage::errors as ds_errors;
use octez_riscv_durable_storage::key::Key;
use registry::RegistryState;

#[ocaml::sig]
pub type Registry = MutableState<RegistryState<Normal>>;

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
    api_common::registry_hash(state)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64")]
pub fn octez_riscv_durable_in_memory_registry_size(
    state: SafePointer<Registry>,
) -> OcamlFallible<u64> {
    api_common::registry_size(state)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_registry_resize(
    state: SafePointer<Registry>,
    size: u64,
) -> SplitDsResult<()> {
    api_common::registry_resize(state, size)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_registry_copy(
    state: SafePointer<Registry>,
    src_index: u64,
    dst_index: u64,
) -> SplitDsResult<()> {
    api_common::registry_copy(state, src_index, dst_index)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_registry_move(
    state: SafePointer<Registry>,
    src_index: u64,
    dst_index: u64,
) -> SplitDsResult<()> {
    api_common::registry_move(state, src_index, dst_index)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_registry_clear(
    state: SafePointer<Registry>,
    db_index: u64,
) -> SplitDsResult<()> {
    api_common::registry_clear(state, db_index)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> bytes -> (bool, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_database_exists(
    state: SafePointer<Registry>,
    db_index: u64,
    key: KeyParam,
) -> SplitDsResult<bool> {
    api_common::database_exists(state, db_index, key)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> bytes -> bytes -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_database_set(
    state: SafePointer<Registry>,
    db_index: u64,
    key: KeyParam,
    value: BytesParam,
) -> SplitDsResult<()> {
    api_common::database_set(state, db_index, key, value)
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
    api_common::database_write(state, db_index, key, offset, value)
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
    api_common::database_read(state, db_index, key, offset, len)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> bytes -> (int64, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_database_value_length(
    state: SafePointer<Registry>,
    db_index: u64,
    key: KeyParam,
) -> SplitDsResult<u64> {
    api_common::value_length(state, db_index, key)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> bytes -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_database_delete(
    state: SafePointer<Registry>,
    db_index: u64,
    key: KeyParam,
) -> SplitDsResult<()> {
    api_common::database_delete(state, db_index, key)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> (bytes, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_database_hash(
    state: SafePointer<Registry>,
    db_index: u64,
) -> SplitDsResult<BytesWrapper<Hash>> {
    api_common::database_hash(state, db_index)
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
