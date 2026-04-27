// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! This module defines the OCaml API for the RISC-V durable storage system (on-disk variant).
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

use std::path::Path;

use octez_riscv_api_common::OcamlFallible;
use octez_riscv_api_common::bytes::BytesWrapper;
use octez_riscv_api_common::move_semantics::MutableState;
use octez_riscv_api_common::safe_pointer::SafePointer;
use octez_riscv_data::hash::Hash;
use octez_riscv_data::mode::Normal;
use octez_riscv_data::mode::utils::NotFound;
use octez_riscv_durable_storage::commit::CommitId;
use octez_riscv_durable_storage::errors as ds_errors;
use octez_riscv_durable_storage::persistence_layer::PersistenceLayer;
use octez_riscv_durable_storage::registry as ds_registry;
use octez_riscv_durable_storage::repo::DirectoryManager;
use octez_riscv_durable_storage_common::BytesParam;
use octez_riscv_durable_storage_common::KeyParam;
use octez_riscv_durable_storage_common::api_common;
use octez_riscv_durable_storage_common::registry::GcNames;
use octez_riscv_durable_storage_common::registry::RegistryState;

/// OCaml GC names for the on-disk registry state.
pub struct OnDiskGcNames;

impl GcNames for OnDiskGcNames {
    const IMMUTABLE_NAME: &'static str = "riscv.imm.registry_state.on_disk";
    const MUTABLE_NAME: &'static str = "riscv.mut.registry_state.on_disk";
}

/// On-disk durable storage registry, exposed as an OCaml custom block.
#[ocaml::sig]
pub type Registry = MutableState<RegistryState<PersistenceLayer, OnDiskGcNames, Normal>>;

/// On-disk repository, wrapping a DirectoryManager.
#[derive(derive_more::Deref)]
#[ocaml::sig]
pub struct Repo(DirectoryManager);
ocaml::custom!(Repo);

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
#[ocaml::sig("bytes -> repo")]
pub fn octez_riscv_durable_on_disk_repo_new(path: BytesParam) -> OcamlFallible<SafePointer<Repo>> {
    let path_str = std::str::from_utf8(path.0)?;
    let dir = DirectoryManager::new(Path::new(path_str))?;
    Ok(SafePointer::from(Repo(dir)))
}

#[ocaml::func]
#[ocaml::sig("repo -> registry")]
pub fn octez_riscv_durable_on_disk_registry_new(
    repo: SafePointer<Repo>,
) -> OcamlFallible<SafePointer<Registry>> {
    let dir = repo.0.clone();
    let registry = RegistryState::new(dir)?;
    Ok(SafePointer::from(MutableState::owned(registry)))
}

#[ocaml::func]
#[ocaml::sig("registry -> bytes")]
pub fn octez_riscv_durable_on_disk_registry_commit(
    state: SafePointer<Registry>,
) -> OcamlFallible<BytesWrapper<Hash>> {
    state.apply_ro(|state| {
        let commit_id = state.commit()?;
        Ok(BytesWrapper::from(*commit_id.as_hash()))
    })
}

#[ocaml::func]
#[ocaml::sig("repo -> bytes -> registry")]
pub fn octez_riscv_durable_on_disk_registry_checkout(
    repo: SafePointer<Repo>,
    commit_id: BytesParam,
) -> OcamlFallible<SafePointer<Registry>> {
    let hash = <[u8; Hash::DIGEST_SIZE]>::try_from(commit_id.0)?;
    let commit_id = CommitId::from(Hash::from(hash));
    let repo = repo.clone();

    let registry = ds_registry::Registry::checkout(repo, commit_id)?;

    let state = RegistryState::from(registry);

    Ok(SafePointer::from(MutableState::owned(state)))
}

#[ocaml::func]
#[ocaml::sig("registry -> bytes")]
pub fn octez_riscv_durable_on_disk_registry_hash(
    state: SafePointer<Registry>,
) -> BytesWrapper<Hash> {
    api_common::registry_hash(state)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64")]
pub fn octez_riscv_durable_on_disk_registry_size(
    state: SafePointer<Registry>,
) -> OcamlFallible<u64> {
    api_common::registry_size(state)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_on_disk_registry_resize(
    state: SafePointer<Registry>,
    size: u64,
) -> SplitDsResult<()> {
    api_common::registry_resize(state, size)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_on_disk_registry_copy(
    state: SafePointer<Registry>,
    src_index: u64,
    dst_index: u64,
) -> SplitDsResult<()> {
    api_common::registry_copy(state, src_index, dst_index)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_on_disk_registry_move(
    state: SafePointer<Registry>,
    src_index: u64,
    dst_index: u64,
) -> SplitDsResult<()> {
    api_common::registry_move(state, src_index, dst_index)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_on_disk_registry_clear(
    state: SafePointer<Registry>,
    db_index: u64,
) -> SplitDsResult<()> {
    api_common::registry_clear(state, db_index)
}

// Normal mode — database

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> bytes -> (bool, invalid_argument_error) result")]
pub fn octez_riscv_durable_on_disk_database_exists(
    state: SafePointer<Registry>,
    db_index: u64,
    key: KeyParam,
) -> SplitDsResult<bool> {
    api_common::database_exists(state, db_index, key)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> bytes -> bytes -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_on_disk_database_set(
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
pub fn octez_riscv_durable_on_disk_database_write(
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
pub fn octez_riscv_durable_on_disk_database_read(
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
pub fn octez_riscv_durable_on_disk_database_value_length(
    state: SafePointer<Registry>,
    db_index: u64,
    key: KeyParam,
) -> SplitDsResult<u64> {
    api_common::value_length(state, db_index, key)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> bytes -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_on_disk_database_delete(
    state: SafePointer<Registry>,
    db_index: u64,
    key: KeyParam,
) -> SplitDsResult<()> {
    api_common::database_delete(state, db_index, key)
}

#[ocaml::func]
#[ocaml::sig("registry -> int64 -> (bytes, invalid_argument_error) result")]
pub fn octez_riscv_durable_on_disk_database_hash(
    state: SafePointer<Registry>,
    db_index: u64,
) -> SplitDsResult<BytesWrapper<Hash>> {
    api_common::database_hash(state, db_index)
}

// Prove mode — registry

#[ocaml::func]
#[ocaml::sig("registry_prove -> bytes")]
pub fn octez_riscv_durable_on_disk_prove_registry_hash(
    _state: SafePointer<RegistryProve>,
) -> BytesWrapper<Hash> {
    todo!("TZX-113 wire-up proof mode")
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64")]
pub fn octez_riscv_durable_on_disk_prove_registry_size(
    _state: SafePointer<RegistryProve>,
) -> OcamlFallible<u64> {
    todo!("TZX-113 wire-up proof mode")
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_on_disk_prove_registry_resize(
    _state: SafePointer<RegistryProve>,
    _size: u64,
) -> SplitDsResult<()> {
    todo!("TZX-113 wire-up proof mode")
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_on_disk_prove_registry_copy(
    _state: SafePointer<RegistryProve>,
    _src_index: u64,
    _dst_index: u64,
) -> SplitDsResult<()> {
    todo!("TZX-113 wire-up proof mode")
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_on_disk_prove_registry_move(
    _state: SafePointer<RegistryProve>,
    _src_index: u64,
    _dst_index: u64,
) -> SplitDsResult<()> {
    todo!("TZX-113 wire-up proof mode")
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_on_disk_prove_registry_clear(
    _state: SafePointer<RegistryProve>,
    _db_index: u64,
) -> SplitDsResult<()> {
    todo!("TZX-113 wire-up proof mode")
}

// Prove mode — database

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> bytes -> (bool, invalid_argument_error) result")]
pub fn octez_riscv_durable_on_disk_prove_database_exists(
    _state: SafePointer<RegistryProve>,
    _db_index: u64,
    _key: KeyParam,
) -> SplitDsResult<bool> {
    todo!("TZX-113 wire-up proof mode")
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> bytes -> bytes -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_on_disk_prove_database_set(
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
pub fn octez_riscv_durable_on_disk_prove_database_write(
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
pub fn octez_riscv_durable_on_disk_prove_database_read(
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
pub fn octez_riscv_durable_on_disk_prove_database_value_length(
    _state: SafePointer<RegistryProve>,
    _db_index: u64,
    _key: KeyParam,
) -> SplitDsResult<u64> {
    todo!("TZX-113 wire-up proof mode")
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> bytes -> (unit, invalid_argument_error) result")]
pub fn octez_riscv_durable_on_disk_prove_database_delete(
    _state: SafePointer<RegistryProve>,
    _db_index: u64,
    _key: KeyParam,
) -> SplitDsResult<()> {
    todo!("TZX-113 wire-up proof mode")
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> int64 -> (bytes, invalid_argument_error) result")]
pub fn octez_riscv_durable_on_disk_prove_database_hash(
    _state: SafePointer<RegistryProve>,
    _db_index: u64,
) -> SplitDsResult<BytesWrapper<Hash>> {
    todo!("TZX-113 wire-up proof mode")
}

// Verify mode — registry

#[ocaml::func]
#[ocaml::sig("registry_verify -> (bytes, verification_error) result")]
pub fn octez_riscv_durable_on_disk_verify_registry_hash(
    _state: SafePointer<RegistryVerify>,
) -> Result<BytesWrapper<Hash>, VerificationError> {
    todo!("TZX-114 wire-up verify mode")
}

#[ocaml::func]
#[ocaml::sig("registry_verify -> (int64, verification_error) result")]
pub fn octez_riscv_durable_on_disk_verify_registry_size(
    _state: SafePointer<RegistryVerify>,
) -> OcamlFallible<Result<u64, VerificationError>> {
    todo!("TZX-114 wire-up verify mode")
}

#[ocaml::func]
#[ocaml::sig("registry_verify -> int64 -> (unit, verification_argument_error) result")]
pub fn octez_riscv_durable_on_disk_verify_registry_resize(
    _state: SafePointer<RegistryVerify>,
    _size: u64,
) -> SplitDsResult<(), VerificationArgumentError> {
    todo!("TZX-114 wire-up verify mode")
}

#[ocaml::func]
#[ocaml::sig("registry_verify -> int64 -> int64 -> (unit, verification_argument_error) result")]
pub fn octez_riscv_durable_on_disk_verify_registry_copy(
    _state: SafePointer<RegistryVerify>,
    _src_index: u64,
    _dst_index: u64,
) -> SplitDsResult<(), VerificationArgumentError> {
    todo!("TZX-114 wire-up verify mode")
}

#[ocaml::func]
#[ocaml::sig("registry_verify -> int64 -> int64 -> (unit, verification_argument_error) result")]
pub fn octez_riscv_durable_on_disk_verify_registry_move(
    _state: SafePointer<RegistryVerify>,
    _src_index: u64,
    _dst_index: u64,
) -> SplitDsResult<(), VerificationArgumentError> {
    todo!("TZX-114 wire-up verify mode")
}

#[ocaml::func]
#[ocaml::sig("registry_verify -> int64 -> (unit, verification_argument_error) result")]
pub fn octez_riscv_durable_on_disk_verify_registry_clear(
    _state: SafePointer<RegistryVerify>,
    _db_index: u64,
) -> SplitDsResult<(), VerificationArgumentError> {
    todo!("TZX-114 wire-up verify mode")
}

// Verify mode — database

#[ocaml::func]
#[ocaml::sig("registry_verify -> int64 -> bytes -> (bool, verification_argument_error) result")]
pub fn octez_riscv_durable_on_disk_verify_database_exists(
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
pub fn octez_riscv_durable_on_disk_verify_database_set(
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
pub fn octez_riscv_durable_on_disk_verify_database_write(
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
pub fn octez_riscv_durable_on_disk_verify_database_read(
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
pub fn octez_riscv_durable_on_disk_verify_database_value_length(
    _state: SafePointer<RegistryVerify>,
    _db_index: u64,
    _key: KeyParam,
) -> SplitDsResult<u64, VerificationArgumentError> {
    todo!("TZX-114 wire-up verify mode")
}

#[ocaml::func]
#[ocaml::sig("registry_verify -> int64 -> bytes -> (unit, verification_argument_error) result")]
pub fn octez_riscv_durable_on_disk_verify_database_delete(
    _state: SafePointer<RegistryVerify>,
    _db_index: u64,
    _key: KeyParam,
) -> SplitDsResult<(), VerificationArgumentError> {
    todo!("TZX-114 wire-up verify mode")
}

#[ocaml::func]
#[ocaml::sig("registry_verify -> int64 -> (bytes, verification_argument_error) result")]
pub fn octez_riscv_durable_on_disk_verify_database_hash(
    _state: SafePointer<RegistryVerify>,
    _db_index: u64,
) -> SplitDsResult<BytesWrapper<Hash>, VerificationArgumentError> {
    todo!("TZX-114 wire-up verify mode")
}

// Proof utilities

#[ocaml::func]
#[ocaml::sig("registry -> registry_prove")]
pub fn octez_riscv_durable_on_disk_start_proof(
    _state: SafePointer<Registry>,
) -> SafePointer<RegistryProve> {
    // TODO (TZX-113): wire-up proof mode
    SafePointer::from(RegistryProve)
}

#[ocaml::func]
#[ocaml::sig("registry_prove -> proof")]
pub fn octez_riscv_durable_on_disk_produce_proof(
    _state: SafePointer<RegistryProve>,
) -> SafePointer<Proof> {
    // TODO (TZX-113): wire-up proof mode
    SafePointer::from(Proof)
}

#[ocaml::func]
#[ocaml::sig("proof -> bytes")]
pub fn octez_riscv_durable_on_disk_proof_start_state(
    _state: SafePointer<Proof>,
) -> BytesWrapper<Hash> {
    // TODO (TZX-113): wire-up proof mode
    BytesWrapper::from(Hash::hash_bytes(&[]))
}

#[ocaml::func]
#[ocaml::sig("proof -> bytes")]
pub fn octez_riscv_durable_on_disk_proof_stop_state(
    _state: SafePointer<Proof>,
) -> BytesWrapper<Hash> {
    // TODO (TZX-113): wire-up proof mode
    BytesWrapper::from(Hash::hash_bytes(&[]))
}

#[ocaml::func]
#[ocaml::sig("proof -> bytes")]
pub fn octez_riscv_durable_on_disk_serialise_proof(
    _state: SafePointer<Proof>,
) -> BytesWrapper<Vec<u8>> {
    // TODO (TZX-113): wire-up proof mode
    BytesWrapper::from(vec![])
}

#[ocaml::func]
#[ocaml::sig("bytes -> (proof, string) result")]
pub fn octez_riscv_durable_on_disk_deserialise_proof(
    _proof: BytesParam,
) -> Result<SafePointer<Proof>, String> {
    todo!("TZX-114: wire-up verify mode")
}

// Verify utilities

#[ocaml::func]
#[ocaml::sig("proof -> registry_verify")]
pub fn octez_riscv_durable_on_disk_start_verify(
    _proof: SafePointer<Proof>,
) -> SafePointer<RegistryVerify> {
    // TODO (TZX-114): wire-up verification mode
    SafePointer::from(RegistryVerify)
}
