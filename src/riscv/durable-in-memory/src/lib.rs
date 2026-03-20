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

use octez_riscv_api_common::OcamlFallible;
use octez_riscv_api_common::bytes::BytesWrapper;
use octez_riscv_api_common::move_semantics::MutableState;
use octez_riscv_api_common::safe_pointer::SafePointer;
use octez_riscv_data::hash::Hash;
use octez_riscv_data::mode::Normal;
use octez_riscv_data::mode::utils::NotFound;
use octez_riscv_durable_storage::errors as ds_errors;
use octez_riscv_durable_storage::storage::in_memory::InMemoryKeyValueStore;
use octez_riscv_durable_storage::storage::in_memory::InMemoryRepo;
use octez_riscv_durable_storage_common::BytesParam;
use octez_riscv_durable_storage_common::KeyParam;
use octez_riscv_durable_storage_common::api_common;
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
pub type Registry = MutableState<RegistryState<InMemoryKeyValueStore, InMemoryGcNames, Normal>>;

/// Stub registry for proof generation. See TZX-113.
// TODO (TZX-113): implement registry prove
#[ocaml::sig]
pub struct RegistryProve;
ocaml::custom!(RegistryProve);

/// Stub registry for proof verification. See TZX-114.
// TODO (TZX-114 wire-up verify mode): implement registry verify
#[ocaml::sig]
pub struct RegistryVerify;
ocaml::custom!(RegistryVerify);

/// Stub proof value. See TZX-113.
// TODO (TZX-113): implement proof
#[ocaml::sig]
pub struct Proof;
ocaml::custom!(Proof);

/// Deterministic errors arising from logically invalid arguments.
///
/// These are returned to the kernel as the error variant of an OCaml result.
#[derive(ocaml::FromValue, ocaml::ToValue)]
#[ocaml::sig(
    "Key_not_found | Key_too_long | Offset_too_large | Database_index_out_of_bounds | Registry_resize_too_large"
)]
pub enum InvalidArgumentError {
    /// The requested key does not exist in the database.
    KeyNotFound,
    /// The key exceeded the maximum allowed length.
    KeyTooLong,
    /// The offset exceeded the length of the stored value.
    OffsetTooLarge,
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
            ds_errors::InvalidArgumentError::OffsetTooLarge => Self::OffsetTooLarge,
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

// Normal mode — registry

#[ocaml::func]
#[ocaml::sig("unit -> registry")]
pub fn octez_riscv_durable_in_memory_registry_new() -> OcamlFallible<SafePointer<Registry>> {
    let registry = RegistryState::new(InMemoryRepo)?;

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

// Normal mode — database

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

// Prove mode — registry

#[ocaml::func]
#[ocaml::sig("registry_prove -> bytes")]
pub fn octez_riscv_durable_in_memory_prove_registry_hash(
    _state: SafePointer<RegistryProve>,
) -> BytesWrapper<Hash> {
    todo!("TZX-113 wire-up proof mode")
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64")]
pub fn octez_riscv_durable_in_memory_prove_registry_size(
    _state: SafePointer<RegistryProve>,
) -> OcamlFallible<u64> {
    todo!("TZX-113 wire-up proof mode")
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_prove_registry_resize(
    _state: SafePointer<RegistryProve>,
    _size: u64,
) -> SplitDsResult<()> {
    todo!("TZX-113 wire-up proof mode")
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_prove_registry_copy(
    _state: SafePointer<RegistryProve>,
    _src_index: u64,
    _dst_index: u64,
) -> SplitDsResult<()> {
    todo!("TZX-113 wire-up proof mode")
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_prove_registry_move(
    _state: SafePointer<RegistryProve>,
    _src_index: u64,
    _dst_index: u64,
) -> SplitDsResult<()> {
    todo!("TZX-113 wire-up proof mode")
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_prove_registry_clear(
    _state: SafePointer<RegistryProve>,
    _db_index: u64,
) -> SplitDsResult<()> {
    todo!("TZX-113 wire-up proof mode")
}

// Prove mode — database

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> bytes -> (bool, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_prove_database_exists(
    _state: SafePointer<RegistryProve>,
    _db_index: u64,
    _key: KeyParam,
) -> SplitDsResult<bool> {
    todo!("TZX-113 wire-up proof mode")
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> bytes -> bytes -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_prove_database_set(
    _state: SafePointer<RegistryProve>,
    _db_index: u64,
    _key: KeyParam,
    _value: BytesParam,
) -> SplitDsResult<()> {
    todo!("TZX-113 wire-up proof mode")
}

#[ocaml::func]
#[ocaml::sig(
    "registry_prove -> int64 -> bytes -> int64 -> bytes -> (int64, invalid_argument_error) result"
)]
pub fn octez_riscv_durable_in_memory_prove_database_write(
    _state: SafePointer<RegistryProve>,
    _db_index: u64,
    _key: KeyParam,
    _offset: u64,
    _value: BytesParam,
) -> SplitDsResult<u64> {
    todo!("TZX-113 wire-up proof mode")
}

#[ocaml::func]
#[ocaml::sig(
    "registry_prove -> int64 -> bytes -> int64 -> int64 -> (bytes, invalid_argument_error) result"
)]
pub fn octez_riscv_durable_in_memory_prove_database_read(
    _state: SafePointer<RegistryProve>,
    _db_index: u64,
    _key: KeyParam,
    _offset: u64,
    _len: u64,
) -> SplitDsResult<BytesWrapper<Vec<u8>>> {
    todo!("TZX-113 wire-up proof mode")
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> bytes -> (int64, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_prove_database_value_length(
    _state: SafePointer<RegistryProve>,
    _db_index: u64,
    _key: KeyParam,
) -> SplitDsResult<u64> {
    todo!("TZX-113 wire-up proof mode")
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> bytes -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_prove_database_delete(
    _state: SafePointer<RegistryProve>,
    _db_index: u64,
    _key: KeyParam,
) -> SplitDsResult<()> {
    todo!("TZX-113 wire-up proof mode")
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> (bytes, invalid_argument_error) result")]
pub fn octez_riscv_durable_in_memory_prove_database_hash(
    _state: SafePointer<RegistryProve>,
    _db_index: u64,
) -> SplitDsResult<BytesWrapper<Hash>> {
    todo!("TZX-113 wire-up proof mode")
}

// Verify mode — registry

#[ocaml::func]
#[ocaml::sig("registry_verify -> (bytes, verification_error) result")]
pub fn octez_riscv_durable_in_memory_verify_registry_hash(
    _state: SafePointer<RegistryVerify>,
) -> Result<BytesWrapper<Hash>, VerificationError> {
    todo!("TZX-114 wire-up verify mode")
}

#[ocaml::func]
#[ocaml::sig("registry_verify -> (int64, verification_error) result")]
pub fn octez_riscv_durable_in_memory_verify_registry_size(
    _state: SafePointer<RegistryVerify>,
) -> OcamlFallible<Result<u64, VerificationError>> {
    todo!("TZX-114 wire-up verify mode")
}

#[ocaml::func]
#[ocaml::sig("registry_verify -> int64 -> (unit, verification_argument_error) result")]
pub fn octez_riscv_durable_in_memory_verify_registry_resize(
    _state: SafePointer<RegistryVerify>,
    _size: u64,
) -> SplitDsResult<(), VerificationArgumentError> {
    todo!("TZX-114 wire-up verify mode")
}

#[ocaml::func]
#[ocaml::sig("registry_verify -> int64 -> int64 -> (unit, verification_argument_error) result")]
pub fn octez_riscv_durable_in_memory_verify_registry_copy(
    _state: SafePointer<RegistryVerify>,
    _src_index: u64,
    _dst_index: u64,
) -> SplitDsResult<(), VerificationArgumentError> {
    todo!("TZX-114 wire-up verify mode")
}

#[ocaml::func]
#[ocaml::sig("registry_verify -> int64 -> int64 -> (unit, verification_argument_error) result")]
pub fn octez_riscv_durable_in_memory_verify_registry_move(
    _state: SafePointer<RegistryVerify>,
    _src_index: u64,
    _dst_index: u64,
) -> SplitDsResult<(), VerificationArgumentError> {
    todo!("TZX-114 wire-up verify mode")
}

#[ocaml::func]
#[ocaml::sig("registry_verify -> int64 -> (unit, verification_argument_error) result")]
pub fn octez_riscv_durable_in_memory_verify_registry_clear(
    _state: SafePointer<RegistryVerify>,
    _db_index: u64,
) -> SplitDsResult<(), VerificationArgumentError> {
    todo!("TZX-114 wire-up verify mode")
}

// Verify mode — database

#[ocaml::func]
#[ocaml::sig("registry_verify -> int64 -> bytes -> (bool, verification_argument_error) result")]
pub fn octez_riscv_durable_in_memory_verify_database_exists(
    _state: SafePointer<RegistryVerify>,
    _db_index: u64,
    _key: KeyParam,
) -> SplitDsResult<bool, VerificationArgumentError> {
    todo!("TZX-114 wire-up verify mode")
}

#[ocaml::func]
#[ocaml::sig(
    "registry_verify -> int64 -> bytes -> bytes -> (unit, verification_argument_error) result"
)]
pub fn octez_riscv_durable_in_memory_verify_database_set(
    _state: SafePointer<RegistryVerify>,
    _db_index: u64,
    _key: KeyParam,
    _value: BytesParam,
) -> SplitDsResult<(), VerificationArgumentError> {
    todo!("TZX-114 wire-up verify mode")
}

#[ocaml::func]
#[ocaml::sig(
    "registry_verify -> int64 -> bytes -> int64 -> bytes -> (int64, verification_argument_error) result"
)]
pub fn octez_riscv_durable_in_memory_verify_database_write(
    _state: SafePointer<RegistryVerify>,
    _db_index: u64,
    _key: KeyParam,
    _offset: u64,
    _value: BytesParam,
) -> SplitDsResult<u64, VerificationArgumentError> {
    todo!("TZX-114 wire-up verify mode")
}

#[ocaml::func]
#[ocaml::sig(
    "registry_verify -> int64 -> bytes -> int64 -> int64 -> (bytes, verification_argument_error) result"
)]
pub fn octez_riscv_durable_in_memory_verify_database_read(
    _state: SafePointer<RegistryVerify>,
    _db_index: u64,
    _key: KeyParam,
    _offset: u64,
    _len: u64,
) -> SplitDsResult<BytesWrapper<Vec<u8>>, VerificationArgumentError> {
    todo!("TZX-114 wire-up verify mode")
}

#[ocaml::func]
#[ocaml::sig("registry_verify -> int64 -> bytes -> (int64, verification_argument_error) result")]
pub fn octez_riscv_durable_in_memory_verify_database_value_length(
    _state: SafePointer<RegistryVerify>,
    _db_index: u64,
    _key: KeyParam,
) -> SplitDsResult<u64, VerificationArgumentError> {
    todo!("TZX-114 wire-up verify mode")
}

#[ocaml::func]
#[ocaml::sig("registry_verify -> int64 -> bytes -> (unit, verification_argument_error) result")]
pub fn octez_riscv_durable_in_memory_verify_database_delete(
    _state: SafePointer<RegistryVerify>,
    _db_index: u64,
    _key: KeyParam,
) -> SplitDsResult<(), VerificationArgumentError> {
    todo!("TZX-114 wire-up verify mode")
}

#[ocaml::func]
#[ocaml::sig("registry_verify -> int64 -> (bytes, verification_argument_error) result")]
pub fn octez_riscv_durable_in_memory_verify_database_hash(
    _state: SafePointer<RegistryVerify>,
    _db_index: u64,
) -> SplitDsResult<BytesWrapper<Hash>, VerificationArgumentError> {
    todo!("TZX-114 wire-up verify mode")
}

// Proof utilities

#[ocaml::func]
#[ocaml::sig("registry -> registry_prove")]
pub fn octez_riscv_durable_in_memory_start_proof(
    _state: SafePointer<Registry>,
) -> SafePointer<RegistryProve> {
    // TODO (TZX-113): wire-up proof mode
    SafePointer::from(RegistryProve)
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> proof")]
pub fn octez_riscv_durable_in_memory_produce_proof(
    _state: SafePointer<RegistryProve>,
) -> SafePointer<Proof> {
    // TODO (TZX-113): wire-up proof mode
    SafePointer::from(Proof)
}

#[ocaml::func]
#[ocaml::sig("proof -> bytes")]
pub fn octez_riscv_durable_in_memory_proof_start_state(
    _state: SafePointer<Proof>,
) -> BytesWrapper<Hash> {
    // TODO (TZX-113): wire-up proof mode
    BytesWrapper::from(Hash::hash_bytes(&[]))
}

#[ocaml::func]
#[ocaml::sig("proof -> bytes")]
pub fn octez_riscv_durable_in_memory_proof_stop_state(
    _state: SafePointer<Proof>,
) -> BytesWrapper<Hash> {
    // TODO (TZX-113): wire-up proof mode
    BytesWrapper::from(Hash::hash_bytes(&[]))
}

#[ocaml::func]
#[ocaml::sig("proof -> bytes")]
pub fn octez_riscv_durable_in_memory_serialise_proof(
    _state: SafePointer<Proof>,
) -> BytesWrapper<Vec<u8>> {
    // TODO (TZX-113): wire-up proof mode
    BytesWrapper::from(vec![])
}

#[ocaml::func]
#[ocaml::sig("bytes -> (proof, string) result")]
pub fn octez_riscv_durable_in_memory_deserialise_proof(
    _proof: BytesParam,
) -> Result<SafePointer<Proof>, String> {
    todo!("TZX-114: wire-up verify mode")
}

// Verify utilities

#[ocaml::func]
#[ocaml::sig("proof -> registry_verify")]
pub fn octez_riscv_durable_in_memory_start_verify(
    _proof: SafePointer<Proof>,
) -> SafePointer<RegistryVerify> {
    // TODO (TZX-114): wire-up verification mode
    SafePointer::from(RegistryVerify)
}
