// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

mod config;
#[cfg(feature = "supervisor")]
mod protection;
mod state;

use std::num::NonZeroU64;

use tezos_smart_rollup_constants::riscv::SbiError;
use thiserror::Error;

use super::registers::XValue;
use crate::state_backend::AllocatedOf;
use crate::state_backend::CommitmentLayout;
use crate::state_backend::Elem;
use crate::state_backend::FnManager;
use crate::state_backend::ManagerBase;
use crate::state_backend::ManagerClone;
use crate::state_backend::ManagerRead;
use crate::state_backend::ManagerReadWrite;
use crate::state_backend::ManagerWrite;
use crate::state_backend::ProofLayout;
use crate::state_backend::Ref;

/// Number of bits needed so you can address every byte in a page
pub const OFFSET_BITS: u64 = 12;

/// Bit mask to keep only the page offset
pub const OFFSET_MASK: u64 = (1 << OFFSET_BITS) - 1;

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

/// Memory access permissions
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Permissions {
    None,
    Read,
    Write,
    ReadWrite,
    ReadExec,
}

impl Permissions {
    /// Do the permissions allow reading?
    pub const fn can_read(&self) -> bool {
        match self {
            Self::None | Self::Write => false,
            Self::Read | Self::ReadWrite | Self::ReadExec => true,
        }
    }

    /// Do the permissions allow writing?
    pub const fn can_write(&self) -> bool {
        match self {
            Self::None | Self::Read | Self::ReadExec => false,
            Self::ReadWrite | Self::Write => true,
        }
    }

    /// Do the permissions allow execution?
    pub const fn can_exec(&self) -> bool {
        match self {
            Self::None | Self::Read | Self::ReadWrite | Self::Write => false,
            Self::ReadExec => true,
        }
    }
}

#[cfg(feature = "supervisor")]
impl TryFrom<XValue> for Permissions {
    type Error = crate::pvm::linux::error::Error;

    fn try_from(value: XValue) -> Result<Self, Self::Error> {
        const READ: u64 = 0b1;
        const WRITE: u64 = 0b10;
        const EXEC: u64 = 0b100;
        const READ_WRITE: u64 = READ | WRITE;
        const READ_EXEC: u64 = READ | EXEC;
        const NONE: u64 = 0;

        match value {
            READ_WRITE => Ok(Self::ReadWrite),
            READ_EXEC => Ok(Self::ReadExec),
            READ => Ok(Self::Read),
            WRITE => Ok(Self::Write),
            NONE => Ok(Self::None),

            // Unknown protections value
            _ => Err(crate::pvm::linux::error::Error::InvalidArgument),
        }
    }
}

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

    /// Read an element in the region that will be used in execution. `address` is in bytes.
    fn read_exec<E>(&self, address: Address) -> Result<E, OutOfBounds>
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
        M: ManagerReadWrite;

    /// Update multiple elements in the region. `address` is in bytes.
    fn write_all<E>(&mut self, address: Address, values: &[E]) -> Result<(), OutOfBounds>
    where
        E: Elem,
        M: ManagerReadWrite;

    /// Clone all memory.
    fn clone(&self) -> Self
    where
        M: ManagerClone;

    /// Zero-out all memory.
    fn reset(&mut self)
    where
        M: ManagerWrite;

    /// Protect the pages that belong to the given address range.
    #[cfg(feature = "supervisor")]
    fn protect_pages(
        &mut self,
        address: Address,
        length: usize,
        perms: Permissions,
    ) -> Result<(), OutOfBounds>
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
pub use config::M1G;
pub use config::M1M;
pub use config::M4G;
pub use config::M4K;
pub use config::M8K;
pub use config::M64M;
