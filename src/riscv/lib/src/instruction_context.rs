// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! The instruction context forms the building blocks used for executing RISC-V instructions.
//!
//! By providing these building blocks for various execution formats, the same implementation can
//! be used for both interpretation and compilation of instructions.

use crate::{
    machine_state::{
        MachineCoreState, ProgramCounterUpdate,
        instruction::Args,
        main_memory::MainMemoryLayout,
        registers::{NonZeroXRegister, XValue},
    },
    state_backend::ManagerReadWrite,
    traps::Exception,
};

/// Type of function that may be used to lower [`Instructions`] to IR.
///
/// [`Instructions`]: crate::machine_state::instruction::Instruction
pub type IcbLoweringFn<I> = unsafe fn(&Args, &mut I) -> <I as ICB>::IResult<ProgramCounterUpdate>;

/// Instruction Context Builder contains operations required to
/// execute RISC-V instructions.
pub trait ICB {
    /// A 64-bit value stored in [`XRegisters`].
    ///
    /// [`XRegisters`]: crate::machine_state::registers::XRegisters
    type XValue;

    /// Perform a read to a [`NonZeroXRegister`], with the given value.
    /// This is a specialized version of `xregister_read` that is only used for
    /// registers that are guaranteed not to be x0.
    fn xregister_read(&mut self, reg: NonZeroXRegister) -> Self::XValue;

    /// Perform a write to a [`NonZeroXRegister`], with the given value.
    /// This is a specialized version of `xregister_write` that is only used for
    /// registers that are guaranteed not to be x0.
    fn xregister_write(&mut self, reg: NonZeroXRegister, value: Self::XValue);

    /// Perform a wrapping add of two **XValues**, returning the new value.
    ///
    /// This behaves identically for both signed & unsigned values.
    fn xvalue_wrapping_add(&mut self, lhs: Self::XValue, rhs: Self::XValue) -> Self::XValue;

    /// Representation for the manipulation of fallible operations.
    type IResult<Value>;

    /// Wrap a value as a fallible value.
    fn ok<Value>(&mut self, val: Value) -> Self::IResult<Value>;

    /// Map the fallible-value into a fallible-value of a different type.
    fn map<Value, Next, F>(res: Self::IResult<Value>, f: F) -> Self::IResult<Next>
    where
        F: FnOnce(Value) -> Next;

    /// Run a fallible operation over the fallible-value as input.
    fn and_then<Value, Next, F>(res: Self::IResult<Value>, f: F) -> Self::IResult<Next>
    where
        F: FnOnce(Value) -> Self::IResult<Next>;
}

impl<ML: MainMemoryLayout, M: ManagerReadWrite> ICB for MachineCoreState<ML, M> {
    type XValue = XValue;

    #[inline(always)]
    fn xregister_read(&mut self, reg: NonZeroXRegister) -> Self::XValue {
        self.hart.xregisters.read_nz(reg)
    }

    #[inline(always)]
    fn xregister_write(&mut self, reg: NonZeroXRegister, value: Self::XValue) {
        self.hart.xregisters.write_nz(reg, value)
    }

    #[inline(always)]
    fn xvalue_wrapping_add(&mut self, lhs: Self::XValue, rhs: Self::XValue) -> Self::XValue {
        // Wrapped addition in two's complement behaves the same for signed and unsigned
        lhs.wrapping_add(rhs)
    }

    type IResult<In> = Result<In, Exception>;

    #[inline(always)]
    fn ok<In>(&mut self, val: In) -> Self::IResult<In> {
        Ok(val)
    }

    #[inline(always)]
    fn map<In, Out, F>(res: Self::IResult<In>, f: F) -> Self::IResult<Out>
    where
        F: FnOnce(In) -> Out,
    {
        res.map(f)
    }

    #[inline(always)]
    fn and_then<In, Out, F>(res: Self::IResult<In>, f: F) -> Self::IResult<Out>
    where
        F: FnOnce(In) -> Self::IResult<Out>,
    {
        res.and_then(f)
    }
}
