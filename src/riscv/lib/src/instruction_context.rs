// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! The instruction context forms the building blocks used for executing RISC-V instructions.
//!
//! By providing these building blocks for various execution formats, the same implementation can
//! be used for both interpretation and compilation of instructions.

pub(super) mod arithmetic;
pub(super) mod comparable;

use arithmetic::Arithmetic;
use comparable::Comparable;
use cranelift::codegen::ir;
use cranelift::codegen::ir::types::I64;

use crate::jit::state_access::JitStateAccess;
use crate::jit::{self};
use crate::machine_state::MachineCoreState;
use crate::machine_state::ProgramCounterUpdate;
use crate::machine_state::instruction::Args;
use crate::machine_state::memory::Address;
use crate::machine_state::memory::BadMemoryAccess;
use crate::machine_state::memory::Memory;
use crate::machine_state::memory::MemoryConfig;
use crate::machine_state::registers::NonZeroXRegister;
use crate::machine_state::registers::XRegister;
use crate::machine_state::registers::XValue;
use crate::machine_state::registers::XValue32;
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

/// PhiValue allows the conversion of values to and from cranelift primitive
/// [`ir::Value`] when in the context of [`JIT`] compilation. It represents a chosen
/// correct value from multiple control flows possible in `ICB::branch_merge`.
///
/// These methods have no relevance in the context of interpreted mode.
///
/// [`JIT`]: crate::jit::JIT
pub(super) trait PhiValue {
    /// Represents the generic representation of the value in the ICB.
    type IcbValue<I: ICB + ?Sized>: Sized;

    /// In JIT, convert the value to an iterator of [`ir::Value`]s.
    fn to_ir_vals<MC: MemoryConfig, JSA: JitStateAccess>(
        icb_repr: Self::IcbValue<jit::builder::Builder<'_, MC, JSA>>,
    ) -> impl IntoIterator<Item = ir::Value>;

    /// Convert [`ir::Value`]s back to itself.
    fn from_ir_vals<'a, MC: MemoryConfig, JSA: JitStateAccess>(
        params: &[ir::Value],
    ) -> Self::IcbValue<jit::builder::Builder<'a, MC, JSA>>;

    /// The cranelift primitive types of the IR values representing this value in JIT.
    const IR_TYPES: &'static [ir::Type];
}

impl PhiValue for () {
    type IcbValue<I: ICB + ?Sized> = ();

    fn to_ir_vals<MC: MemoryConfig, JSA: JitStateAccess>(
        _: Self::IcbValue<jit::builder::Builder<'_, MC, JSA>>,
    ) -> impl IntoIterator<Item = ir::Value> {
        []
    }

    fn from_ir_vals<'a, MC: MemoryConfig, JSA: JitStateAccess>(
        _: &[ir::Value],
    ) -> Self::IcbValue<jit::builder::Builder<'a, MC, JSA>> {
    }

    const IR_TYPES: &'static [ir::Type] = &[];
}

impl PhiValue for XValue {
    type IcbValue<I: ICB + ?Sized> = I::XValue;

    fn to_ir_vals<MC: MemoryConfig, JSA: JitStateAccess>(
        icb_repr: Self::IcbValue<jit::builder::Builder<'_, MC, JSA>>,
    ) -> impl IntoIterator<Item = ir::Value> {
        [icb_repr.0]
    }

    fn from_ir_vals<'a, MC: MemoryConfig, JSA: JitStateAccess>(
        params: &[ir::Value],
    ) -> Self::IcbValue<jit::builder::Builder<'a, MC, JSA>> {
        jit::builder::X64(params[0])
    }

    const IR_TYPES: &'static [ir::Type] = &[I64];
}

/// Instruction Context Builder contains operations required to
/// execute RISC-V instructions.
#[expect(clippy::upper_case_acronyms, reason = "ICB looks cooler than Icb")]
pub(crate) trait ICB {
    /// A 64-bit value stored in [`XRegisters`].
    ///
    /// [`XRegisters`]: crate::machine_state::registers::XRegisters
    type XValue: Arithmetic<Self> + Comparable<Self>;

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

    /// Perform a read of the program counter.
    fn pc_read(&mut self) -> Self::XValue;

    /// Type for boolean operations.
    type Bool;

    /// Perform a logical `and` operation of two [`ICB::Bool`] values.
    fn bool_and(&mut self, lhs: Self::Bool, rhs: Self::Bool) -> Self::Bool;

    /// A 32-bit value to be used only in word-width operations.
    type XValue32: Arithmetic<Self> + Comparable<Self>;

    /// Convert an [`XValue`] to a [`XValue32`].
    fn narrow(&mut self, value: Self::XValue) -> Self::XValue32;

    /// Sign-extend an [`XValue32`] to an [`XValue`].
    fn extend_signed(&mut self, value: Self::XValue32) -> Self::XValue;

    /// Zero-extend an [`XValue32`] to an [`XValue`].
    #[expect(dead_code)]
    fn extend_unsigned(&mut self, value: Self::XValue32) -> Self::XValue;

    /// Multiply two [`XValue`] values and return the high 64 bits of the result, with
    /// the appropriate sign-extension passed in as 2 boolean arguments.
    fn mul_high(
        &mut self,
        lhs: Self::XValue,
        rhs: Self::XValue,
        mul_high_type: MulHighType,
    ) -> Self::XValue;

    /// Convert a boolean value to an xvalue.
    ///
    /// Coerces to the following:
    /// - `true -> 1`
    /// - `false -> 0`
    fn xvalue_from_bool(&mut self, value: Self::Bool) -> Self::XValue;

    /// Branching instruction.
    ///
    /// If `condition` is true, the branch will be taken. The PC update
    /// will be to the address returned by `take_branch`.
    ///
    /// If false, the PC update is to the next instruction.
    fn branch(
        &mut self,
        condition: Self::Bool,
        offset: i64,
        instr_width: InstrWidth,
    ) -> ProgramCounterUpdate<Self::XValue>;

    /// Take a branch based on the given condition and return to a common line of execution.
    ///
    /// This is used for situations where we have a common execution path following branching.
    /// The `cond` is the condition to branch on, and the `true_branch` and `false_branch` are the
    /// functions to execute for the left and right branches, respectively.
    ///
    /// Semantically, this function returns the caller into the context of the common
    /// execution path with the resulting value of the branch that was taken.
    fn branch_merge<Phi: PhiValue, OnTrue, OnFalse>(
        &mut self,
        cond: Self::Bool,
        true_branch: OnTrue,
        false_branch: OnFalse,
    ) -> Phi::IcbValue<Self>
    where
        OnTrue: FnOnce(&mut Self) -> Phi::IcbValue<Self>,
        OnFalse: FnOnce(&mut Self) -> Phi::IcbValue<Self>;

    /// Representation for the manipulation of fallible operations.
    type IResult<Value>;

    /// Wrap a value as a fallible value.
    fn ok<Value>(&mut self, val: Value) -> Self::IResult<Value>;

    /// Raise an [`Exception::IllegalInstruction`] error.
    fn err_illegal_instruction<In>(&mut self) -> Self::IResult<In>;

    /// Raise an [`Exception::StoreAMOAccessFault`] error if `address` is not
    /// aligned to the given [`LoadStoreWidth`].
    fn atomic_access_fault_guard(
        &mut self,
        address: Self::XValue,
        width: LoadStoreWidth,
    ) -> Self::IResult<()>;

    /// Map the fallible-value into a fallible-value of a different type.
    fn map<Value, Next, F>(res: Self::IResult<Value>, f: F) -> Self::IResult<Next>
    where
        F: FnOnce(Value) -> Next;

    /// Run a fallible operation over the fallible-value as input.
    fn and_then<Value, Next, F>(res: Self::IResult<Value>, f: F) -> Self::IResult<Next>
    where
        F: FnOnce(Value) -> Self::IResult<Next>;

    /// Exception to perform an ECall at the current mode
    fn ecall(&mut self) -> Self::IResult<ProgramCounterUpdate<Self::XValue>>;

    /// Write value to main memory, at the given address.
    ///
    /// The value is truncated to the width given by [`LoadStoreWidth`].
    fn main_memory_store(
        &mut self,
        phys_address: Self::XValue,
        value: Self::XValue,
        width: LoadStoreWidth,
    ) -> Self::IResult<()>;

    /// Read value from main memory, at the given address.
    ///
    /// The value is truncated to the width given by [`LoadStoreWidth`].
    fn main_memory_load(
        &mut self,
        phys_address: Self::XValue,
        signed: bool,
        width: LoadStoreWidth,
    ) -> Self::IResult<Self::XValue>;

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
    fn pc_read(&mut self) -> Self::XValue {
        self.hart.pc.read()
    }

    type Bool = bool;

    #[inline(always)]
    fn bool_and(&mut self, lhs: Self::Bool, rhs: Self::Bool) -> Self::Bool {
        lhs && rhs
    }

    type XValue32 = XValue32;

    #[inline(always)]
    fn narrow(&mut self, value: Self::XValue) -> Self::XValue32 {
        value as u32
    }

    #[inline(always)]
    fn extend_signed(&mut self, value: Self::XValue32) -> Self::XValue {
        value as i32 as u64
    }

    #[inline(always)]
    fn extend_unsigned(&mut self, value: Self::XValue32) -> Self::XValue {
        value as u64
    }

    #[inline(always)]
    fn mul_high(
        &mut self,
        lhs: Self::XValue,
        rhs: Self::XValue,
        mul_high_type: MulHighType,
    ) -> Self::XValue {
        let (lhs, rhs) = match mul_high_type {
            MulHighType::Signed => (lhs as i64 as i128 as u128, rhs as i64 as i128 as u128),
            MulHighType::Unsigned => (lhs as u128, rhs as u128),
            MulHighType::SignedUnsigned => (lhs as i64 as i128 as u128, rhs as u128),
        };
        let result = lhs.wrapping_mul(rhs);

        (result >> 64) as u64
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

    #[inline(always)]
    fn branch_merge<Phi: PhiValue, OnTrue, OnFalse>(
        &mut self,
        cond: Self::Bool,
        true_branch: OnTrue,
        false_branch: OnFalse,
    ) -> Phi::IcbValue<Self>
    where
        OnTrue: FnOnce(&mut Self) -> Phi::IcbValue<Self>,
        OnFalse: FnOnce(&mut Self) -> Phi::IcbValue<Self>,
    {
        if cond {
            true_branch(self)
        } else {
            false_branch(self)
        }
    }

    #[inline(always)]
    fn atomic_access_fault_guard(
        &mut self,
        address: Address,
        width: LoadStoreWidth,
    ) -> Self::IResult<()> {
        let width = self.xvalue_of_imm(width as i64);
        let remainder = address.modulus(width, self);
        let zero = self.xvalue_of_imm(0);

        if remainder.compare(zero, Predicate::NotEqual, self) {
            Err(Exception::StoreAMOAccessFault(address))
        } else {
            Ok(())
        }
    }

    type IResult<In> = Result<In, Exception>;

    #[inline(always)]
    fn ok<In>(&mut self, val: In) -> Self::IResult<In> {
        Ok(val)
    }

    #[inline(always)]
    fn err_illegal_instruction<In>(&mut self) -> Self::IResult<In> {
        Err(Exception::IllegalInstruction)
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

    fn ecall(&mut self) -> Self::IResult<ProgramCounterUpdate<Self::XValue>> {
        Err(Exception::EnvCall)
    }

    #[inline(always)]
    fn main_memory_store(
        &mut self,
        address: Self::XValue,
        value: Self::XValue,
        width: LoadStoreWidth,
    ) -> Self::IResult<()> {
        let res = match width {
            LoadStoreWidth::Byte => self.main_memory.write::<u8>(address, value as u8),
            LoadStoreWidth::Half => self.main_memory.write::<u16>(address, value as u16),
            LoadStoreWidth::Word => self.main_memory.write::<u32>(address, value as u32),
            LoadStoreWidth::Double => self.main_memory.write::<u64>(address, value),
        };

        res.map_err(|_: BadMemoryAccess| Exception::StoreAMOAccessFault(address))
    }

    #[inline(always)]
    fn main_memory_load(
        &mut self,
        address: Self::XValue,
        signed: bool,
        width: LoadStoreWidth,
    ) -> Self::IResult<Self::XValue> {
        let res = match (signed, width) {
            (true, LoadStoreWidth::Byte) => {
                self.main_memory.read::<u8>(address).map(|v| v as i8 as u64)
            }
            (true, LoadStoreWidth::Half) => self
                .main_memory
                .read::<u16>(address)
                .map(|v| v as i16 as u64),
            (true, LoadStoreWidth::Word) => self
                .main_memory
                .read::<u32>(address)
                .map(|v| v as i32 as u64),
            (_, LoadStoreWidth::Double) => self.main_memory.read::<u64>(address),
            (false, LoadStoreWidth::Byte) => self.main_memory.read::<u8>(address).map(|v| v as u64),
            (false, LoadStoreWidth::Half) => {
                self.main_memory.read::<u16>(address).map(|v| v as u64)
            }
            (false, LoadStoreWidth::Word) => {
                self.main_memory.read::<u32>(address).map(|v| v as u64)
            }
        };

        res.map_err(|_: BadMemoryAccess| Exception::LoadAccessFault(address))
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

/// The type of shift operation to perform.
pub enum Shift {
    /// Logical left shift. Zeroes are shifted into the least significant bits.
    Left,
    /// Logical right shift. Zeroes are shifted into the most significant bits.
    RightUnsigned,
    /// Arithmetic right shift. Sign-bits (ones) are shifted into the most significant bits.
    RightSigned,
}

/// The type of X64 mul_high operation to perform.
pub enum MulHighType {
    Signed,
    Unsigned,
    SignedUnsigned,
}

/// Supported value widths for loading from/storing to main memory for XRegisters.
///
/// **NB** This type may be passed over C-FFI. See [state_access] for more
/// information.
///
/// For now, the approach taken chooses to pass enums as integers, and parse
/// them back into the Enum variant on the rust side - to avoid potential UB
/// should an incorrect discriminant be parsed. We therefore choose explicit
/// constants for each - so that we know very precisely what values are expected.
///
/// [state_access]: crate::jit::state_access
#[derive(Debug)]
#[repr(u8)]
pub enum LoadStoreWidth {
    Byte = Self::BYTE_WIDTH,
    Half = Self::HALF_WIDTH,
    Word = Self::WORD_WIDTH,
    Double = Self::DOUBLE_WIDTH,
}

impl LoadStoreWidth {
    const BYTE_WIDTH: u8 = std::mem::size_of::<u8>() as u8;
    const HALF_WIDTH: u8 = std::mem::size_of::<u16>() as u8;
    const WORD_WIDTH: u8 = std::mem::size_of::<u32>() as u8;
    const DOUBLE_WIDTH: u8 = std::mem::size_of::<u64>() as u8;

    /// Convert a value-width in bytes to the appropriate
    /// `LoadStoreWidth`, if supported.
    pub fn new(val: u8) -> Option<Self> {
        match val {
            Self::BYTE_WIDTH => Some(Self::Byte),
            Self::HALF_WIDTH => Some(Self::Half),
            Self::WORD_WIDTH => Some(Self::Word),
            Self::DOUBLE_WIDTH => Some(Self::Double),
            _ => None,
        }
    }
}
