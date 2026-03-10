// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! This module defines the OCaml API for the RISC-V durable storage system (in memory variant).

use octez_riscv_api_common::move_semantics::CustomGcResource;
use octez_riscv_api_common::move_semantics::MutableState;
use octez_riscv_api_common::safe_pointer::SafePointer;
use octez_riscv_data::mode::Normal;
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

impl CustomGcResource for RegistryState {
    const IMMUTABLE_NAME: &'static str = "riscv.imm.registry_state";

    const MUTABLE_NAME: &'static str = "riscv.mut.registry_state";
}

#[ocaml::sig]
pub type Registry = MutableState<RegistryState>;

#[ocaml::func]
#[ocaml::sig("registry -> int64")]
pub fn octez_riscv_durable_in_memory_registry_size(state: SafePointer<Registry>) -> i64 {
    state.apply_ro(registry::Registry::len) as i64
}
