// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

mod config;
mod state;

use std::num::NonZeroU64;

use tezos_smart_rollup_constants::riscv::SbiError;
use thiserror::Error;

use super::registers::XValue;
use crate::state_backend::{
    AllocatedOf, CommitmentLayout, Elem, FnManager, ManagerBase, ManagerClone, ManagerDeserialise,
    ManagerRead, ManagerSerialise, ManagerWrite, ProofLayout, Ref,
};

/// Number of bits needed so you can address every byte in a page
pub const OFFSET_BITS: u64 = 12;

/// Size of a page
pub const PAGE_SIZE: NonZeroU64 = {
    const PAGE_SIZE: u64 = 1 << OFFSET_BITS;

    // Compile-time check: Page size must be positive
    const _: () = {
        if PAGE_SIZE < 1 {
            panic!()
        }
    };

    match NonZeroU64::new(PAGE_SIZE) {
        Some(page_size) => page_size,
        None => {
            // SAFETY: The compile-time check above ensures this branch cannot be reached
            unsafe { std::hint::unreachable_unchecked() }
        }
    }
};

/// Memory address
pub type Address = XValue;

/// Lowest address
pub const FIRST_ADDRESS: Address = 0;

/// Address out of bounds error
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Error, derive_more::Display)]
pub struct OutOfBounds;

impl From<OutOfBounds> for SbiError {
    fn from(_value: OutOfBounds) -> Self {
        SbiError::InvalidAddress
    }
}

/// Instance of memory
pub trait Memory<M: ManagerBase>: Sized {
    /// Read an element in the region. `address` is in bytes.
    fn read<E>(&self, address: Address) -> Result<E, OutOfBounds>
    where
        E: Elem,
        M: ManagerRead;

    /// Read elements from the region. `address` is in bytes.
    fn read_all<E>(&self, address: Address, values: &mut [E]) -> Result<(), OutOfBounds>
    where
        E: Elem,
        M: ManagerRead;

    /// Update an element in the region. `address` is in bytes.
    fn write<E>(&mut self, address: Address, value: E) -> Result<(), OutOfBounds>
    where
        E: Elem,
        M: ManagerWrite;

    /// Update multiple elements in the region. `address` is in bytes.
    fn write_all<E>(&mut self, address: Address, values: &[E]) -> Result<(), OutOfBounds>
    where
        E: Elem,
        M: ManagerWrite;

    /// Serialise memory.
    fn serialise<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        M: ManagerSerialise,
        S: serde::Serializer;

    /// Deserialise memory.
    fn deserialise<'de, D>(deserializer: D) -> Result<Self, D::Error>
    where
        M: ManagerDeserialise,
        D: serde::Deserializer<'de>;

    /// Clone all memory.
    fn clone(&self) -> Self
    where
        M: ManagerClone;

    /// Zero-out all memory.
    fn reset(&mut self)
    where
        M: ManagerWrite;
}

/// Memory configuration
pub trait MemoryConfig: 'static {
    /// Number of bytes in the memory
    const TOTAL_BYTES: usize;

    /// Layout for memory instance's state
    type Layout: CommitmentLayout + ProofLayout;

    /// Memory instance
    type State<M: ManagerBase>: Memory<M>;

    /// Bind the allocated regions to produce a memory instance.
    fn bind<M: ManagerBase>(space: AllocatedOf<Self::Layout, M>) -> Self::State<M>;

    /// Given a manager morphism `f : &M -> N`, return the memory instance layout's allocated
    /// structure containing the constituents of `N` that were produced from the constituents of
    /// `&M`.
    fn struct_ref<'a, M, F>(instance: &'a Self::State<M>) -> AllocatedOf<Self::Layout, F::Output>
    where
        M: ManagerBase,
        F: FnManager<Ref<'a, M>>;
}

// Re-export memory configurations
pub use config::{M1G, M1M, M4G, M4K, M8K, M64M};
