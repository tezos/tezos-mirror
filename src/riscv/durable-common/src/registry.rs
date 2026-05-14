// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Generic registry state wrapper for OCaml GC resource tracking.

use std::marker::PhantomData;
use std::ops::Deref;
use std::ops::DerefMut;

use octez_riscv_api_common::move_semantics::CustomGcResource;
use octez_riscv_api_common::move_semantics::MutableState;
use octez_riscv_api_common::try_clone::TryClone;
use octez_riscv_data::mode::Normal;
use octez_riscv_durable_storage::errors::OperationalError;
use octez_riscv_durable_storage::registry;
use octez_riscv_durable_storage::storage::KeyValueStore;

use crate::api_common::BackgroundKeyValueStore;

/// Marker trait supplying OCaml GC resource names.
pub trait GcNames {
    /// Name used to register the immutable (read-only) OCaml custom block.
    const IMMUTABLE_NAME: &'static str;
    /// Name used to register the mutable OCaml custom block.
    const MUTABLE_NAME: &'static str;
}

/// Wrapper to enable customizing OCaml GC's resource tracking.
#[repr(transparent)]
pub struct RegistryState<KV: KeyValueStore, G> {
    inner: registry::Registry<KV, Normal>,
    _phantom: PhantomData<G>,
}

impl<KV: BackgroundKeyValueStore, G> RegistryState<KV, G> {
    /// Construct a new registry.
    pub fn new(repo: KV::Repo) -> Result<Self, OperationalError> {
        let reg = registry::Registry::new(repo)?;
        Ok(Self {
            inner: reg,
            _phantom: PhantomData,
        })
    }
}

impl<KV: KeyValueStore, G> From<registry::Registry<KV, Normal>> for RegistryState<KV, G> {
    fn from(value: registry::Registry<KV, Normal>) -> Self {
        Self {
            inner: value,
            _phantom: PhantomData,
        }
    }
}

impl<KV: KeyValueStore, G> Deref for RegistryState<KV, G> {
    type Target = registry::Registry<KV, Normal>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<KV: KeyValueStore, G> DerefMut for RegistryState<KV, G> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<KV, G> TryClone for RegistryState<KV, G>
where
    KV: BackgroundKeyValueStore,
    KV::Repo: Clone,
{
    type Error = OperationalError;

    fn try_clone(&self) -> Result<Self, Self::Error> {
        Ok(RegistryState {
            inner: self.inner.try_clone()?,
            _phantom: PhantomData,
        })
    }
}

impl<KV: KeyValueStore, G: GcNames> CustomGcResource for RegistryState<KV, G> {
    const IMMUTABLE_NAME: &'static str = G::IMMUTABLE_NAME;
    const MUTABLE_NAME: &'static str = G::MUTABLE_NAME;
}

/// Type alias for a mutable registry state backed by a generic key-value store.
pub type DsRegistry<KV, G> = MutableState<RegistryState<KV, G>>;
