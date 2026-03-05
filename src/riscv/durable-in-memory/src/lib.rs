// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! This module defines the OCaml API for the RISC-V durable storage system (in memory variant).

use octez_riscv_api_common::OcamlFallible;
use octez_riscv_api_common::move_semantics::CustomGcResource;
use octez_riscv_api_common::move_semantics::MutableState;
use octez_riscv_api_common::safe_pointer::SafePointer;
use octez_riscv_api_common::try_clone::TryClone;
use octez_riscv_data::mode::Normal;
use octez_riscv_durable_storage::errors as ds_errors;
use octez_riscv_durable_storage::registry;
use octez_riscv_durable_storage::storage::in_memory::InMemoryKeyValueStore;

/// Wrapper to enable customing OCaml GC's resource tracking.
#[repr(transparent)]
pub struct RegistryState(registry::Registry<InMemoryKeyValueStore, Normal>);

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
#[ocaml::sig("Key_not_found | Key_too_long | Offset_too_large | Database_index_out_of_bounds")]
pub enum InvalidArgumentError {
    KeyNotFound,
    KeyTooLong,
    OffsetTooLarge,
    DatabaseIndexOutOfBounds,
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
        }
    }
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
    let res = state.apply(|registry| registry.resize(size))?;

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
