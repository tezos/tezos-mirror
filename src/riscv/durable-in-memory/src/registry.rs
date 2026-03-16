// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! This module defines core (routing) logic of the 'host functions' portion of the API
//! for the RISC-V durable storage system.

use octez_riscv_api_common::move_semantics::CustomGcResource;
use octez_riscv_api_common::try_clone::TryClone;
use octez_riscv_data::mode::Mode;
use octez_riscv_data::mode::Normal;
use octez_riscv_durable_storage::errors as ds_errors;
use octez_riscv_durable_storage::registry;
use octez_riscv_durable_storage::registry::CloneRegistryMode;
use octez_riscv_durable_storage::storage::in_memory::InMemoryKeyValueStore;
use octez_riscv_durable_storage::storage::in_memory::InMemoryRepo;

/// Wrapper to enable customizing OCaml GC's resource tracking.
#[repr(transparent)]
pub struct RegistryState<M: Mode>(registry::Registry<InMemoryKeyValueStore, M>);

impl RegistryState<Normal> {
    /// Construct a new in-memory registry
    pub fn new() -> Result<Self, ds_errors::OperationalError> {
        let reg = registry::Registry::new(InMemoryRepo)?;
        Ok(Self(reg))
    }
}

impl<M: Mode> std::ops::Deref for RegistryState<M> {
    type Target = registry::Registry<InMemoryKeyValueStore, M>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<M: Mode> std::ops::DerefMut for RegistryState<M> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<M: CloneRegistryMode> TryClone for RegistryState<M> {
    type Error = ds_errors::OperationalError;

    fn try_clone(&self) -> Result<Self, Self::Error> {
        Ok(RegistryState(self.0.try_clone()?))
    }
}

impl CustomGcResource for RegistryState<Normal> {
    const IMMUTABLE_NAME: &'static str = "riscv.imm.registry_state.normal";

    const MUTABLE_NAME: &'static str = "riscv.mut.registry_state.normal";
}
