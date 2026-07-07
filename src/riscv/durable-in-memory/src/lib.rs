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
//! [`OperationalError`]: octez_riscv_durable_storage::errors::OperationalError
//! [`InvalidArgumentError`]: octez_riscv_durable_storage::errors::InvalidArgumentError
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

use std::convert::Infallible;

use octez_riscv_api_common::OcamlFallible;
use octez_riscv_api_common::bytes::BytesWrapper;
use octez_riscv_api_common::move_semantics::ImmutableState;
use octez_riscv_api_common::move_semantics::MutableState;
use octez_riscv_api_common::safe_pointer::SafePointer;
use octez_riscv_data::hash::Hash;
use octez_riscv_data::merkle_proof::proof::Proof as NdsProof;
use octez_riscv_data::merkle_proof::proof::deserialise_proof;
use octez_riscv_data::merkle_proof::proof::serialise_proof;
use octez_riscv_data::mode::Prove;
use octez_riscv_data::mode::Verify;
use octez_riscv_data::mode::utils::NotFound;
use octez_riscv_durable_storage::errors as ds_errors;
use octez_riscv_durable_storage::registry::Registry as DsRegistry;
use octez_riscv_durable_storage::storage::in_memory::InMemoryKeyValueStore;
use octez_riscv_durable_storage::storage::in_memory::InMemoryRepo;
use octez_riscv_durable_storage_common::BytesParam;
use octez_riscv_durable_storage_common::KeyParam;
use octez_riscv_durable_storage_common::api_common;
use octez_riscv_durable_storage_common::registry::BackgroundRegistry;
use octez_riscv_durable_storage_common::registry::GcNames;
use octez_riscv_durable_storage_common::registry::RegistryState;

/// OCaml GC names for the in-memory registry state.
pub struct InMemoryGcNames;

impl GcNames for InMemoryGcNames {
    const IMMUTABLE_NAME: &'static str = "riscv.imm.registry_state.normal";
    const MUTABLE_NAME: &'static str = "riscv.mut.registry_state.normal";
}

/// In-memory durable storage registry, exposed as an OCaml custom block.
#[ocaml::sig]
pub type Registry = MutableState<RegistryState<InMemoryKeyValueStore, InMemoryGcNames>>;

/// Immutable snapshot of an in-memory durable storage registry.
///
/// Obtained from [`octez_riscv_durable_in_memory_registry_to_imm`]; mutations
/// on the source registry after the conversion are not observable through the
/// snapshot, and vice versa for registries recovered with
/// [`octez_riscv_durable_in_memory_registry_from_imm`] (copy-on-write via
/// [`ImmutableState`] / [`MutableState`]).
#[ocaml::sig]
pub type ImmRegistry = ImmutableState<RegistryState<InMemoryKeyValueStore, InMemoryGcNames>>;

/// OCaml GC names for the in-memory registry state (prove).
pub struct InMemoryProveGcNames;

impl GcNames for InMemoryProveGcNames {
    const IMMUTABLE_NAME: &'static str = "riscv.imm.registry_state.prove";
    const MUTABLE_NAME: &'static str = "riscv.mut.registry_state.prove";
}

/// In-memory prove-mode durable storage registry.
#[ocaml::sig]
pub type RegistryProve =
    BackgroundRegistry<InMemoryKeyValueStore, InMemoryProveGcNames, Prove<'static>>;

/// OCaml GC names for the in-memory registry state (verify).
pub struct InMemoryVerifyGcNames;

impl GcNames for InMemoryVerifyGcNames {
    const IMMUTABLE_NAME: &'static str = "riscv.imm.registry_state.verify";
    const MUTABLE_NAME: &'static str = "riscv.mut.registry_state.verify";
}

/// In-memory verify-mode durable storage registry, replaying operations against a proof.
#[ocaml::sig]
pub type RegistryVerify = BackgroundRegistry<InMemoryKeyValueStore, InMemoryVerifyGcNames, Verify>;

/// Proof produced by prove mode, can be used to construct the verify state.
#[ocaml::sig]
pub struct Proof(NdsProof);
ocaml::custom!(Proof);

/// Deterministic errors arising from logically invalid arguments.
///
/// These are returned to the kernel as the error variant of an OCaml result.
#[derive(ocaml::FromValue, ocaml::ToValue)]
#[ocaml::sig(
    "Key_not_found | Key_too_long | Io_request_too_large | Offset_too_large | Value_size_too_large | Database_index_out_of_bounds | Registry_resize_too_large"
)]
pub enum InvalidArgumentError {
    /// The requested key does not exist in the database.
    KeyNotFound,
    /// The key exceeded the maximum allowed length.
    KeyTooLong,
    /// IO requests cannot be larger than MAX_FILE_CHUNK_SIZE (2KiB)
    IoRequestTooLarge,
    /// The offset exceeded the length of the stored value.
    OffsetTooLarge,
    /// Maximum value size would be exceeded (64MiB)
    ValueSizeTooLarge,
    /// The database index was outside the bounds of the registry.
    DatabaseIndexOutOfBounds,
    /// The requested registry size exceeds the allowed limit.
    RegistryResizeTooLarge,
}

impl From<ds_errors::InvalidArgumentError> for InvalidArgumentError {
    fn from(value: ds_errors::InvalidArgumentError) -> Self {
        match value {
            ds_errors::InvalidArgumentError::KeyNotFound => Self::KeyNotFound,
            ds_errors::InvalidArgumentError::KeyTooLong => Self::KeyTooLong,
            ds_errors::InvalidArgumentError::IoRequestTooLarge => Self::IoRequestTooLarge,
            ds_errors::InvalidArgumentError::OffsetTooLarge => Self::OffsetTooLarge,
            ds_errors::InvalidArgumentError::ValueSizeTooLarge => Self::ValueSizeTooLarge,
            ds_errors::InvalidArgumentError::DatabaseIndexOutOfBounds => {
                Self::DatabaseIndexOutOfBounds
            }
            ds_errors::InvalidArgumentError::RegistryResizeTooLarge => Self::RegistryResizeTooLarge,
        }
    }
}

/// Result type for durable storage operations, defaulting to [`InvalidArgumentError`].
pub type SplitDsResult<T, Err = InvalidArgumentError> =
    octez_riscv_durable_storage_common::SplitDsResult<T, Err>;

/// Map a fallible conversion over the `Ok` variant; conversion errors become OCaml exceptions.
pub use octez_riscv_durable_storage_common::map_fallible;
/// Split [`ds_errors::Error`] into an OCaml exception (operational) or result error (invalid argument).
pub use octez_riscv_durable_storage_common::split_ds_errors;

/// Error returned by proof verification operations.
#[derive(ocaml::FromValue, ocaml::ToValue)]
#[ocaml::sig("Not_found")]
pub enum VerificationError {
    /// The requested element was not found in the proof.
    NotFound,
}

impl From<NotFound> for VerificationError {
    fn from(_value: NotFound) -> Self {
        Self::NotFound
    }
}

/// Combined error for operations that may fail with either an invalid argument or a verification error.
#[derive(ocaml::FromValue, ocaml::ToValue)]
#[ocaml::sig("Invalid_argument of invalid_argument_error | Verification of verification_error")]
pub enum VerificationArgumentError {
    /// The operation was given an invalid argument.
    InvalidArgument(InvalidArgumentError),
    /// The operation failed during proof verification.
    Verification(VerificationError),
}

impl From<ds_errors::InvalidArgumentError> for VerificationArgumentError {
    fn from(value: ds_errors::InvalidArgumentError) -> Self {
        Self::InvalidArgument(value.into())
    }
}

impl From<NotFound> for VerificationArgumentError {
    fn from(value: NotFound) -> Self {
        Self::Verification(value.into())
    }
}

// Normal/Prove modes apply 'infallibly' during operation - the state
// is always available for operations to take place (modulo
// any `OperationalError` that may occur).
//
// This impl allows all three modes to re-use the same pathway, where
// verify may indeed touch state not-present in a proof.
impl From<Infallible> for InvalidArgumentError {
    fn from(value: Infallible) -> Self {
        match value {}
    }
}

// Normal mode — registry

#[ocaml::func]
#[ocaml::sig("unit -> registry")]
pub fn octez_riscv_durable_in_memory_registry_new() -> OcamlFallible<SafePointer<Registry>> {
    let registry = RegistryState::new(InMemoryRepo::default())?;

    Ok(SafePointer::from(MutableState::owned(registry)))
}

#[ocaml::func]
#[ocaml::sig("registry -> imm_registry")]
pub fn octez_riscv_durable_in_memory_registry_to_imm(
    state: SafePointer<Registry>,
) -> OcamlFallible<SafePointer<ImmRegistry>> {
    Ok(SafePointer::from(state.to_imm_state()?))
}

#[ocaml::func]
#[ocaml::sig("imm_registry -> registry")]
pub fn octez_riscv_durable_in_memory_registry_from_imm(
    state: SafePointer<ImmRegistry>,
) -> SafePointer<Registry> {
    SafePointer::from(state.to_mut_state())
}

#[ocaml::func]
#[ocaml::sig("registry -> bytes")]
pub fn octez_riscv_durable_in_memory_registry_hash(
    state: SafePointer<Registry>,
) -> OcamlFallible<BytesWrapper<Hash>> {
    api_common::registry_hash(&*state)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64")]
pub fn octez_riscv_durable_in_memory_registry_size(
    state: SafePointer<Registry>,
) -> OcamlFallible<u64> {
    let Ok(size) = api_common::registry_size(&*state)?;

    Ok(size)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_registry_resize(
    state: SafePointer<Registry>,
    size: u64,
) -> SplitDsResult<()> {
    api_common::registry_resize(&*state, size)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_registry_copy(
    state: SafePointer<Registry>,
    src_index: u64,
    dst_index: u64,
) -> SplitDsResult<()> {
    api_common::registry_copy(&*state, src_index, dst_index)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_registry_move(
    state: SafePointer<Registry>,
    src_index: u64,
    dst_index: u64,
) -> SplitDsResult<()> {
    api_common::registry_move(&*state, src_index, dst_index)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_registry_clear(
    state: SafePointer<Registry>,
    db_index: u64,
) -> SplitDsResult<()> {
    api_common::registry_clear(&*state, db_index)
}

// Normal mode — database

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> bytes -> (bool, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_database_exists(
    state: SafePointer<Registry>,
    db_index: u64,
    key: KeyParam,
) -> SplitDsResult<bool> {
    api_common::database_exists(&*state, db_index, key)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> bytes -> bytes -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_database_set(
    state: SafePointer<Registry>,
    db_index: u64,
    key: KeyParam,
    value: BytesParam,
) -> SplitDsResult<()> {
    api_common::database_set(&*state, db_index, key, value)
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
    api_common::database_write(&*state, db_index, key, offset, value)
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
    api_common::database_read(&*state, db_index, key, offset, len)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> bytes -> (int64, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_database_value_length(
    state: SafePointer<Registry>,
    db_index: u64,
    key: KeyParam,
) -> SplitDsResult<u64> {
    api_common::value_length(&*state, db_index, key)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> bytes -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_database_delete(
    state: SafePointer<Registry>,
    db_index: u64,
    key: KeyParam,
) -> SplitDsResult<()> {
    api_common::database_delete(&*state, db_index, key)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> (bytes, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_database_hash(
    state: SafePointer<Registry>,
    db_index: u64,
) -> SplitDsResult<BytesWrapper<Hash>> {
    api_common::database_hash(&*state, db_index)
}

// Prove mode — registry

#[ocaml::func]
#[ocaml::sig("registry_prove -> bytes")]
pub fn octez_riscv_durable_in_memory_prove_registry_hash(
    state: SafePointer<RegistryProve>,
) -> OcamlFallible<BytesWrapper<Hash>> {
    api_common::registry_hash(&*state)
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64")]
pub fn octez_riscv_durable_in_memory_prove_registry_size(
    state: SafePointer<RegistryProve>,
) -> OcamlFallible<u64> {
    let Ok(size) = api_common::registry_size(&*state)?;

    Ok(size)
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_prove_registry_resize(
    state: SafePointer<RegistryProve>,
    size: u64,
) -> SplitDsResult<()> {
    api_common::registry_resize(&*state, size)
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_prove_registry_copy(
    state: SafePointer<RegistryProve>,
    src_index: u64,
    dst_index: u64,
) -> SplitDsResult<()> {
    api_common::registry_copy(&*state, src_index, dst_index)
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_prove_registry_move(
    state: SafePointer<RegistryProve>,
    src_index: u64,
    dst_index: u64,
) -> SplitDsResult<()> {
    api_common::registry_move(&*state, src_index, dst_index)
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_prove_registry_clear(
    state: SafePointer<RegistryProve>,
    db_index: u64,
) -> SplitDsResult<()> {
    api_common::registry_clear(&*state, db_index)
}

// Prove mode — database

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> bytes -> (bool, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_prove_database_exists(
    state: SafePointer<RegistryProve>,
    db_index: u64,
    key: KeyParam,
) -> SplitDsResult<bool> {
    api_common::database_exists(&*state, db_index, key)
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> bytes -> bytes -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_prove_database_set(
    state: SafePointer<RegistryProve>,
    db_index: u64,
    key: KeyParam,
    value: BytesParam,
) -> SplitDsResult<()> {
    api_common::database_set(&*state, db_index, key, value)
}

#[ocaml::func]
#[ocaml::sig(
    "registry_prove -> int64 -> bytes -> int64 -> bytes -> (int64, invalid_argument_error) result"
)]
pub fn octez_riscv_durable_in_memory_prove_database_write(
    state: SafePointer<RegistryProve>,
    db_index: u64,
    key: KeyParam,
    offset: u64,
    value: BytesParam,
) -> SplitDsResult<u64> {
    api_common::database_write(&*state, db_index, key, offset, value)
}

#[ocaml::func]
#[ocaml::sig(
    "registry_prove -> int64 -> bytes -> int64 -> int64 -> (bytes, invalid_argument_error) result"
)]
pub fn octez_riscv_durable_in_memory_prove_database_read(
    state: SafePointer<RegistryProve>,
    db_index: u64,
    key: KeyParam,
    offset: u64,
    len: u64,
) -> SplitDsResult<BytesWrapper<Vec<u8>>> {
    api_common::database_read(&*state, db_index, key, offset, len)
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> bytes -> (int64, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_prove_database_value_length(
    state: SafePointer<RegistryProve>,
    db_index: u64,
    key: KeyParam,
) -> SplitDsResult<u64> {
    api_common::value_length(&*state, db_index, key)
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> bytes -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_prove_database_delete(
    state: SafePointer<RegistryProve>,
    db_index: u64,
    key: KeyParam,
) -> SplitDsResult<()> {
    api_common::database_delete(&*state, db_index, key)
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> (bytes, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_prove_database_hash(
    state: SafePointer<RegistryProve>,
    db_index: u64,
) -> SplitDsResult<BytesWrapper<Hash>> {
    api_common::database_hash(&*state, db_index)
}

// Verify mode — registry

#[ocaml::func]
#[ocaml::sig("registry_verify -> (bytes, verification_error) result")]
pub fn octez_riscv_durable_in_memory_verify_registry_hash(
    state: SafePointer<RegistryVerify>,
) -> OcamlFallible<Result<BytesWrapper<Hash>, VerificationError>> {
    state.verify_hash()
}

#[ocaml::func]
#[ocaml::sig("registry_verify -> (int64, verification_error) result")]
pub fn octez_riscv_durable_in_memory_verify_registry_size(
    state: SafePointer<RegistryVerify>,
) -> OcamlFallible<Result<u64, VerificationError>> {
    let res = api_common::registry_size(&*state)?;

    Ok(res.map_err(VerificationError::from))
}

#[ocaml::func]
#[ocaml::sig("registry_verify -> int64 -> (unit, verification_argument_error) result")]
pub fn octez_riscv_durable_in_memory_verify_registry_resize(
    state: SafePointer<RegistryVerify>,
    size: u64,
) -> SplitDsResult<(), VerificationArgumentError> {
    api_common::registry_resize(&*state, size)
}

#[ocaml::func]
#[ocaml::sig("registry_verify -> int64 -> int64 -> (unit, verification_argument_error) result")]
pub fn octez_riscv_durable_in_memory_verify_registry_copy(
    state: SafePointer<RegistryVerify>,
    src_index: u64,
    dst_index: u64,
) -> SplitDsResult<(), VerificationArgumentError> {
    api_common::registry_copy(&*state, src_index, dst_index)
}

#[ocaml::func]
#[ocaml::sig("registry_verify -> int64 -> int64 -> (unit, verification_argument_error) result")]
pub fn octez_riscv_durable_in_memory_verify_registry_move(
    state: SafePointer<RegistryVerify>,
    src_index: u64,
    dst_index: u64,
) -> SplitDsResult<(), VerificationArgumentError> {
    api_common::registry_move(&*state, src_index, dst_index)
}

#[ocaml::func]
#[ocaml::sig("registry_verify -> int64 -> (unit, verification_argument_error) result")]
pub fn octez_riscv_durable_in_memory_verify_registry_clear(
    state: SafePointer<RegistryVerify>,
    db_index: u64,
) -> SplitDsResult<(), VerificationArgumentError> {
    api_common::registry_clear(&*state, db_index)
}

// Verify mode — database

#[ocaml::func]
#[ocaml::sig("registry_verify -> int64 -> bytes -> (bool, verification_argument_error) result")]
pub fn octez_riscv_durable_in_memory_verify_database_exists(
    state: SafePointer<RegistryVerify>,
    db_index: u64,
    key: KeyParam,
) -> SplitDsResult<bool, VerificationArgumentError> {
    api_common::database_exists(&*state, db_index, key)
}

#[ocaml::func]
#[ocaml::sig(
    "registry_verify -> int64 -> bytes -> bytes -> (unit, verification_argument_error) result"
)]
pub fn octez_riscv_durable_in_memory_verify_database_set(
    state: SafePointer<RegistryVerify>,
    db_index: u64,
    key: KeyParam,
    value: BytesParam,
) -> SplitDsResult<(), VerificationArgumentError> {
    api_common::database_set(&*state, db_index, key, value)
}

#[ocaml::func]
#[ocaml::sig(
    "registry_verify -> int64 -> bytes -> int64 -> bytes -> (int64, verification_argument_error) result"
)]
pub fn octez_riscv_durable_in_memory_verify_database_write(
    state: SafePointer<RegistryVerify>,
    db_index: u64,
    key: KeyParam,
    offset: u64,
    value: BytesParam,
) -> SplitDsResult<u64, VerificationArgumentError> {
    api_common::database_write(&*state, db_index, key, offset, value)
}

#[ocaml::func]
#[ocaml::sig(
    "registry_verify -> int64 -> bytes -> int64 -> int64 -> (bytes, verification_argument_error) result"
)]
pub fn octez_riscv_durable_in_memory_verify_database_read(
    state: SafePointer<RegistryVerify>,
    db_index: u64,
    key: KeyParam,
    offset: u64,
    len: u64,
) -> SplitDsResult<BytesWrapper<Vec<u8>>, VerificationArgumentError> {
    api_common::database_read(&*state, db_index, key, offset, len)
}

#[ocaml::func]
#[ocaml::sig("registry_verify -> int64 -> bytes -> (int64, verification_argument_error) result")]
pub fn octez_riscv_durable_in_memory_verify_database_value_length(
    state: SafePointer<RegistryVerify>,
    db_index: u64,
    key: KeyParam,
) -> SplitDsResult<u64, VerificationArgumentError> {
    api_common::value_length(&*state, db_index, key)
}

#[ocaml::func]
#[ocaml::sig("registry_verify -> int64 -> bytes -> (unit, verification_argument_error) result")]
pub fn octez_riscv_durable_in_memory_verify_database_delete(
    state: SafePointer<RegistryVerify>,
    db_index: u64,
    key: KeyParam,
) -> SplitDsResult<(), VerificationArgumentError> {
    api_common::database_delete(&*state, db_index, key)
}

#[ocaml::func]
#[ocaml::sig("registry_verify -> int64 -> (bytes, verification_argument_error) result")]
pub fn octez_riscv_durable_in_memory_verify_database_hash(
    state: SafePointer<RegistryVerify>,
    db_index: u64,
) -> SplitDsResult<BytesWrapper<Hash>, VerificationArgumentError> {
    api_common::database_hash(&*state, db_index)
}

// Proof utilities

#[ocaml::func]
#[ocaml::sig("registry -> registry_prove")]
pub fn octez_riscv_durable_in_memory_start_proof(
    state: SafePointer<Registry>,
) -> OcamlFallible<SafePointer<RegistryProve>> {
    api_common::start_proof(&*state).map(SafePointer::from)
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> proof")]
pub fn octez_riscv_durable_in_memory_produce_proof(
    state: SafePointer<RegistryProve>,
) -> OcamlFallible<SafePointer<Proof>> {
    api_common::produce_proof(&*state)
        .map(Proof)
        .map(SafePointer::from)
}

#[ocaml::func]
#[ocaml::sig("proof -> bytes")]
pub fn octez_riscv_durable_in_memory_proof_start_state(
    state: SafePointer<Proof>,
) -> BytesWrapper<Hash> {
    let hash = state.0.initial_state_hash();
    BytesWrapper::from(hash)
}

#[ocaml::func]
#[ocaml::sig("proof -> bytes")]
pub fn octez_riscv_durable_in_memory_proof_stop_state(
    state: SafePointer<Proof>,
) -> BytesWrapper<Hash> {
    let hash = state.0.final_state_hash();
    BytesWrapper::from(hash)
}

#[ocaml::func]
#[ocaml::sig("proof -> bytes")]
pub fn octez_riscv_durable_in_memory_serialise_proof(
    state: SafePointer<Proof>,
) -> BytesWrapper<Vec<u8>> {
    let bytes = serialise_proof(&state.0);
    BytesWrapper::from(bytes)
}

#[ocaml::func]
#[ocaml::sig("bytes -> (proof, string) result")]
pub fn octez_riscv_durable_in_memory_deserialise_proof(
    proof: BytesParam,
) -> Result<SafePointer<Proof>, String> {
    // The stream pass reconstructs the proof tree (and a verify-side state we discard);
    // `start_verify` then runs the proof-tree pass to obtain a verifiable registry.
    //
    // We discard the verify state, as it was deserialised with the stream deserialiser -
    // so currently doesn't have support for verify operations directly. (TZX-161)
    let (proof, _state) =
        deserialise_proof::<DsRegistry<InMemoryKeyValueStore, Verify>, _>(proof.0.iter().copied())
            .map_err(|err| err.to_string())?;

    Ok(SafePointer::from(Proof(proof)))
}

// Verify utilities

#[ocaml::func]
#[ocaml::sig("proof -> registry_verify")]
pub fn octez_riscv_durable_in_memory_start_verify(
    proof: SafePointer<Proof>,
) -> OcamlFallible<SafePointer<RegistryVerify>> {
    let registry = RegistryVerify::from_proof(proof.0.tree().clone())?;

    Ok(SafePointer::from(registry))
}
