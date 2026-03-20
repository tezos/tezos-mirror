// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Generic registry state wrapper for OCaml GC resource tracking.

use std::marker::PhantomData;
use std::ops::Deref;
use std::ops::DerefMut;

use octez_riscv_api_common::move_semantics::CustomGcResource;
use octez_riscv_api_common::try_clone::TryClone;
use octez_riscv_data::mode::Mode;
use octez_riscv_data::mode::Normal;
use octez_riscv_durable_storage::errors::OperationalError;
use octez_riscv_durable_storage::registry;
use octez_riscv_durable_storage::registry::CloneRegistryMode;
use octez_riscv_durable_storage::storage::KeyValueStore;

/// Marker trait supplying OCaml GC resource names.
pub trait GcNames {
    /// Name used to register the immutable (read-only) OCaml custom block.
    const IMMUTABLE_NAME: &'static str;
    /// Name used to register the mutable OCaml custom block.
    const MUTABLE_NAME: &'static str;
}

/// Wrapper to enable customizing OCaml GC's resource tracking.
#[repr(transparent)]
pub struct RegistryState<KV: KeyValueStore, G, M: Mode> {
    inner: registry::Registry<KV, M>,
    _phantom: PhantomData<G>,
}

impl<KV: KeyValueStore + Send + Sync + 'static, G> RegistryState<KV, G, Normal> {
    /// Construct a new registry.
    pub fn new(repo: KV::Repo) -> Result<Self, OperationalError> {
        let reg = registry::Registry::new(repo)?;
        Ok(Self {
            inner: reg,
            _phantom: PhantomData,
        })
    }
}

impl<KV: KeyValueStore, G, M: Mode> Deref for RegistryState<KV, G, M> {
    type Target = registry::Registry<KV, M>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<KV: KeyValueStore, G, M: Mode> DerefMut for RegistryState<KV, G, M> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<KV, G, M> TryClone for RegistryState<KV, G, M>
where
    KV: KeyValueStore + Send + Sync + 'static,
    KV::Repo: Clone,
    M: CloneRegistryMode,
{
    type Error = OperationalError;

    fn try_clone(&self) -> Result<Self, Self::Error> {
        Ok(RegistryState {
            inner: self.inner.try_clone()?,
            _phantom: PhantomData,
        })
    }
}

impl<KV: KeyValueStore, G: GcNames, M: Mode> CustomGcResource for RegistryState<KV, G, M> {
    const IMMUTABLE_NAME: &'static str = G::IMMUTABLE_NAME;
    const MUTABLE_NAME: &'static str = G::MUTABLE_NAME;
}
