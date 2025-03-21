// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! The instruction context forms the building blocks used for executing RISC-V instructions.
//!
//! By providing these building blocks for various execution formats, the same implementation can
//! be used for both interpretation and compilation of instructions.

use crate::machine_state::MachineCoreState;
use crate::machine_state::ProgramCounterUpdate;
use crate::machine_state::instruction::Args;
use crate::machine_state::memory::MemoryConfig;
use crate::machine_state::registers::NonZeroXRegister;
use crate::machine_state::registers::XRegister;
use crate::machine_state::registers::XValue;
use crate::parser::XRegisterParsed;
use crate::parser::instruction::InstrWidth;
use crate::parser::split_x0;
use crate::state_backend::ManagerReadWrite;
use crate::traps::Exception;

/// Type of function that may be used to lower [`Instructions`] to IR.
///
/// [`Instructions`]: crate::machine_state::instruction::Instruction
pub type IcbLoweringFn<I> = unsafe fn(&Args, &mut I) -> IcbFnResult<I>;

/// Result of lowering an instruction.
pub type IcbFnResult<I> = <I as ICB>::IResult<ProgramCounterUpdate<<I as ICB>::XValue>>;

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
    fn xregister_read_nz(&mut self, reg: NonZeroXRegister) -> Self::XValue;

    /// Perform a write to a [`NonZeroXRegister`], with the given value.
    /// This is a specialized version of `xregister_write` that is only used for
    /// registers that are guaranteed not to be x0.
    fn xregister_write_nz(&mut self, reg: NonZeroXRegister, value: Self::XValue);

    /// Construct an [`ICB::XValue`] from an `imm: i64`.
    fn xvalue_of_imm(&mut self, imm: i64) -> Self::XValue;

    /// Perform a wrapping add of two **XValues**, returning the new value.
    ///
    /// This behaves identically for both signed & unsigned values.
    fn xvalue_wrapping_add(&mut self, lhs: Self::XValue, rhs: Self::XValue) -> Self::XValue;

    /// Perform a wrapping sub of two **XValues**, returning the new value.
    ///
    /// This behaves identically for both signed & unsigned values.
    fn xvalue_wrapping_sub(&mut self, lhs: Self::XValue, rhs: Self::XValue) -> Self::XValue;

    /// Perform a wrapping mul of two **XValues**, returning the new value.
    ///
    /// This behaves identically for both signed & unsigned values.
    fn xvalue_wrapping_mul(&mut self, lhs: Self::XValue, rhs: Self::XValue) -> Self::XValue;

    /// Perform a bitwise and of two **XValues**, returning the new value.
    fn xvalue_bitwise_and(&mut self, lhs: Self::XValue, rhs: Self::XValue) -> Self::XValue;

    /// Perform a bitwise or of two **XValues**, returning the new value.
    fn xvalue_bitwise_or(&mut self, lhs: Self::XValue, rhs: Self::XValue) -> Self::XValue;

    /// Perform a read of the program counter.
    fn pc_read(&mut self) -> Self::XValue;

    /// Type for boolean operations.
    type Bool;

    /// Compare two values, given the operation to compare them with.
    fn xvalue_compare(
        &mut self,
        comparison: Predicate,
        lhs: Self::XValue,
        rhs: Self::XValue,
    ) -> Self::Bool;

    /// Convert a boolean value to an xvalue.
    ///
    /// Coerces to the following:
    /// - `true -> 1`
    /// - `false -> 0`
    fn xvalue_from_bool(&mut self, value: Self::Bool) -> Self::XValue;

    /// Branching instruction.
    ///
    /// If `predicate` is true, the branch will be taken. The PC update
    /// will be to the address returned by `take_branch`.
    ///
    /// If false, the PC update is to the next instruction.
    fn branch(
        &mut self,
        condition: Self::Bool,
        offset: i64,
        instr_width: InstrWidth,
    ) -> ProgramCounterUpdate<Self::XValue>;

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

    // ----------------
    // Provided Methods
    // ----------------

    /// Read a value from an [`XRegister`].
    ///
    /// If the register is `x0`, the value read is always zero.
    fn xregister_read(&mut self, reg: XRegister) -> Self::XValue {
        match split_x0(reg) {
            XRegisterParsed::X0 => self.xvalue_of_imm(0),
            XRegisterParsed::NonZero(reg) => self.xregister_read_nz(reg),
        }
    }

    /// Write a value to an [`XRegister`].
    ///
    /// If the register is `x0`, this is a no-op.
    fn xregister_write(&mut self, reg: XRegister, value: Self::XValue) {
        if let XRegisterParsed::NonZero(reg) = split_x0(reg) {
            self.xregister_write_nz(reg, value)
        }
    }

    /// Integer negation of the given `XValue`.
    fn xvalue_negate(&mut self, value: Self::XValue) -> Self::XValue {
        let zero = self.xvalue_of_imm(0);
        self.xvalue_wrapping_sub(zero, value)
    }
}

impl<MC: MemoryConfig, M: ManagerReadWrite> ICB for MachineCoreState<MC, M> {
    type XValue = XValue;

    #[inline(always)]
    fn xregister_read_nz(&mut self, reg: NonZeroXRegister) -> Self::XValue {
        self.hart.xregisters.read_nz(reg)
    }

    #[inline(always)]
    fn xregister_read(&mut self, reg: XRegister) -> Self::XValue {
        self.hart.xregisters.read(reg)
    }

    #[inline(always)]
    fn xregister_write_nz(&mut self, reg: NonZeroXRegister, value: Self::XValue) {
        self.hart.xregisters.write_nz(reg, value)
    }

    #[inline(always)]
    fn xregister_write(&mut self, reg: XRegister, value: Self::XValue) {
        self.hart.xregisters.write(reg, value)
    }

    #[inline(always)]
    fn xvalue_of_imm(&mut self, imm: i64) -> Self::XValue {
        imm as u64
    }

    #[inline(always)]
    fn xvalue_wrapping_add(&mut self, lhs: Self::XValue, rhs: Self::XValue) -> Self::XValue {
        // Wrapped addition in two's complement behaves the same for signed and unsigned
        lhs.wrapping_add(rhs)
    }

    #[inline(always)]
    fn xvalue_wrapping_sub(&mut self, lhs: Self::XValue, rhs: Self::XValue) -> Self::XValue {
        // Wrapped subtraction in two's complement behaves the same for signed and unsigned
        lhs.wrapping_sub(rhs)
    }

    #[inline(always)]
    fn xvalue_wrapping_mul(&mut self, lhs: Self::XValue, rhs: Self::XValue) -> Self::XValue {
        // Wrapped multiplication in two's complement behaves the same for signed and unsigned
        lhs.wrapping_mul(rhs)
    }

    #[inline(always)]
    fn xvalue_bitwise_and(&mut self, lhs: Self::XValue, rhs: Self::XValue) -> Self::XValue {
        lhs & rhs
    }

    #[inline(always)]
    fn xvalue_bitwise_or(&mut self, lhs: Self::XValue, rhs: Self::XValue) -> Self::XValue {
        lhs | rhs
    }

    #[inline(always)]
    fn pc_read(&mut self) -> Self::XValue {
        self.hart.pc.read()
    }

    type Bool = bool;

    #[inline(always)]
    fn xvalue_compare(
        &mut self,
        comparison: Predicate,
        lhs: Self::XValue,
        rhs: Self::XValue,
    ) -> Self::Bool {
        comparison.eval(lhs, rhs)
    }

    #[inline(always)]
    fn xvalue_from_bool(&mut self, value: Self::Bool) -> Self::XValue {
        value as XValue
    }

    #[inline(always)]
    fn branch(
        &mut self,
        predicate: Self::Bool,
        offset: i64,
        instr_width: InstrWidth,
    ) -> ProgramCounterUpdate<Self::XValue> {
        if predicate {
            let pc = self.pc_read();
            let address = pc.wrapping_add(offset as u64);
            ProgramCounterUpdate::Set(address)
        } else {
            ProgramCounterUpdate::Next(instr_width)
        }
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

/// Operators for producing a boolean from two values.
pub enum Predicate {
    Equal,
    NotEqual,
    LessThanSigned,
    LessThanUnsigned,
    LessThanOrEqualSigned,
    GreaterThanSigned,
    GreaterThanOrEqualSigned,
    GreaterThanOrEqualUnsigned,
}

impl Predicate {
    /// Run the given comparison operation over the given values.
    #[inline(always)]
    fn eval(self, lhs: XValue, rhs: XValue) -> bool {
        match self {
            Self::Equal => lhs == rhs,
            Self::NotEqual => lhs != rhs,
            Self::LessThanSigned => (lhs as i64) < (rhs as i64),
            Self::LessThanUnsigned => lhs < rhs,
            Self::LessThanOrEqualSigned => (lhs as i64) <= (rhs as i64),
            Self::GreaterThanSigned => (lhs as i64) > (rhs as i64),
            Self::GreaterThanOrEqualSigned => (lhs as i64) >= (rhs as i64),
            Self::GreaterThanOrEqualUnsigned => lhs >= rhs,
        }
    }
}
