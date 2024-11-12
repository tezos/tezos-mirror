// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

pub mod main_memory;

use crate::machine_state::{backend, registers};
use derive_more::Error;
use tezos_smart_rollup_constants::riscv::SbiError;

/// Bus address
pub type Address = registers::XValue;

/// An address is out of bounds.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Error, derive_more::Display)]
pub struct OutOfBounds;

impl From<OutOfBounds> for SbiError {
    fn from(_value: OutOfBounds) -> Self {
        SbiError::InvalidAddress
    }
}

/// Addressable space for reading
pub trait AddressableRead<E: backend::Elem> {
    /// Read an element of type `E` from the given address.
    fn read(&self, addr: Address) -> Result<E, OutOfBounds>;

    /// Read elements of type `E` from the given address.
    fn read_all(&self, addr: Address, values: &mut [E]) -> Result<(), OutOfBounds>;
}

/// Addressable space for writing
pub trait AddressableWrite<E: backend::Elem> {
    /// Write an element of type `E` to the given address.
    fn write(&mut self, addr: Address, value: E) -> Result<(), OutOfBounds>;

    /// Write consecutive elements of type `E` starting from the given address.
    fn write_all(&mut self, addr: Address, values: &[E]) -> Result<(), OutOfBounds>;
}
