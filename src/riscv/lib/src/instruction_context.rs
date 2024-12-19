// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! The instruction context forms the building blocks used for executing RISC-V instructions.
//!
//! By providing these building blocks for various execution formats, the same implementation can
//! be used for both interpretation and compilation of instructions.

use crate::{
    machine_state::registers::{XRegister, XRegisters, XValue},
    state_backend::ManagerReadWrite,
};

/// Instruction Context Builder contains operations required to
/// execute RISC-V instructions.
pub trait ICB {
    /// A 64-bit value stored in [`XRegisters`].
    type XValue;

    /// Perform a read of an [`XRegister`], returning the value stored.
    fn xregister_read(&mut self, reg: XRegister) -> Self::XValue;

    /// Perform a write to an [`XRegister`], with the given value.
    fn xregister_write(&mut self, reg: XRegister, value: Self::XValue);

    /// Perform a wrapping add of two **XValues**, returning the new value.
    ///
    /// This behaves identically for both signed & unsigned values.
    fn xvalue_wrapping_add(&mut self, lhs: Self::XValue, rhs: Self::XValue) -> Self::XValue;
}

impl<M: ManagerReadWrite> ICB for XRegisters<M> {
    type XValue = XValue;

    fn xregister_read(&mut self, reg: XRegister) -> Self::XValue {
        self.read(reg)
    }

    fn xregister_write(&mut self, reg: XRegister, value: Self::XValue) {
        self.write(reg, value)
    }

    fn xvalue_wrapping_add(&mut self, lhs: Self::XValue, rhs: Self::XValue) -> Self::XValue {
        // Wrapped addition in two's complement behaves the same for signed and unsigned
        lhs.wrapping_add(rhs)
    }
}
