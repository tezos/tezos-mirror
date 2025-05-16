// SPDX-FileCopyrightText: 2023-2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

// We have several globals whose names we want to align with the RISC-V
// specification.
#![allow(non_upper_case_globals)]
#![expect(
    dead_code,
    reason = "Aliases for register ABI names might not be used everywhere"
)]

use std::fmt;

use arbitrary_int::u5;

use crate::default::ConstDefault;
use crate::machine_state::backend;
use crate::state::NewState;
use crate::state_backend::owned_backend::Owned;

/// Integer register index
#[expect(non_camel_case_types, reason = "Consistent with RISC-V spec")]
#[repr(u8)]
#[derive(
    Clone,
    Copy,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize,
    strum::EnumIter,
)]
pub enum XRegister {
    // The `usize` representation of these constructors shall be used as an
    // index into the 31-element array holding the registers.
    // x0 represents no entry in this array because it is handled separately.
    // Therefore we assign a dummy index to x0.
    x0 = u8::MAX,
    x1 = 0,
    x2,
    x3,
    x4,
    x5,
    x6,
    x7,
    x8,
    x9,
    x10,
    x11,
    x12,
    x13,
    x14,
    x15,
    x16,
    x17,
    x18,
    x19,
    x20,
    x21,
    x22,
    x23,
    x24,
    x25,
    x26,
    x27,
    x28,
    x29,
    x30,
    x31,
}

// We would like to have the constructors at the module top-level.
pub use XRegister::*;

// ABI register names
pub const zero: XRegister = x0;
pub const ra: XRegister = x1;
pub const sp: XRegister = x2;
pub const gp: XRegister = x3;
pub const tp: XRegister = x4;
pub const t0: XRegister = x5;
pub const t1: XRegister = x6;
pub const t2: XRegister = x7;
pub const s0: XRegister = x8;
pub const fp: XRegister = x8;
pub const s1: XRegister = x9;
pub const a0: XRegister = x10;
pub const a1: XRegister = x11;
pub const a2: XRegister = x12;
pub const a3: XRegister = x13;
pub const a4: XRegister = x14;
pub const a5: XRegister = x15;
pub const a6: XRegister = x16;
pub const a7: XRegister = x17;
pub const s2: XRegister = x18;
pub const s3: XRegister = x19;
pub const s4: XRegister = x20;
pub const s5: XRegister = x21;
pub const s6: XRegister = x22;
pub const s7: XRegister = x23;
pub const s8: XRegister = x24;
pub const s9: XRegister = x25;
pub const s10: XRegister = x26;
pub const s11: XRegister = x27;
pub const t3: XRegister = x28;
pub const t4: XRegister = x29;
pub const t5: XRegister = x30;
pub const t6: XRegister = x31;

#[inline(always)]
pub const fn parse_xregister(r: u5) -> XRegister {
    let r = r.value().wrapping_sub(1);

    // SAFETY: the bounds of u5, under a wrapping decrement
    // are known to correspond to valid values of
    // XRegister's underlying representation.
    unsafe { std::mem::transmute(r) }
}

impl fmt::Display for XRegister {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match *self {
            x0 => "zero",
            x1 => "ra",
            x2 => "sp",
            x3 => "gp",
            x4 => "tp",
            x5 => "t0",
            x6 => "t1",
            x7 => "t2",
            x8 => "s0",
            x9 => "s1",
            x10 => "a0",
            x11 => "a1",
            x12 => "a2",
            x13 => "a3",
            x14 => "a4",
            x15 => "a5",
            x16 => "a6",
            x17 => "a7",
            x18 => "s2",
            x19 => "s3",
            x20 => "s4",
            x21 => "s5",
            x22 => "s6",
            x23 => "s7",
            x24 => "s8",
            x25 => "s9",
            x26 => "s10",
            x27 => "s11",
            x28 => "t3",
            x29 => "t4",
            x30 => "t5",
            x31 => "t6",
        };
        f.write_str(name)
    }
}

impl XRegister {
    #[inline]
    pub fn is_zero(self) -> bool {
        self == x0
    }
}

impl From<NonZeroXRegister> for XRegister {
    fn from(r: NonZeroXRegister) -> Self {
        // SAFETY: XRegister is a superset of NonZeroXRegister,
        // so any value of NonZeroXRegister is known to be
        // a valid value of XRegister.
        unsafe { std::mem::transmute(r) }
    }
}

/// Integer register value
pub type XValue = u64;

/// Integer value for 32-bit operations.
pub type XValue32 = u32;

/// Layout for [XRegisters]
pub type XRegistersLayout = backend::Array<XValue, 31>;

/// Integer registers
pub struct XRegisters<M: backend::ManagerBase> {
    registers: backend::Cells<XValue, 31, M>,
}

impl<M: backend::ManagerBase> XRegisters<M> {
    /// Bind the integer register state to the given allocated space.
    pub fn bind(space: backend::AllocatedOf<XRegistersLayout, M>) -> Self {
        XRegisters { registers: space }
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub fn struct_ref<'a, F: backend::FnManager<backend::Ref<'a, M>>>(
        &'a self,
    ) -> backend::AllocatedOf<XRegistersLayout, F::Output> {
        self.registers.struct_ref::<F>()
    }

    /// Try to read a 64-bit value from the registers and coerce it to another type.
    #[inline]
    pub fn try_read<T: TryFrom<XValue>>(&self, reg: XRegister) -> Result<T, T::Error>
    where
        M: backend::ManagerRead,
    {
        self.read(reg).try_into()
    }

    /// Read an integer from the registers.
    #[inline]
    pub fn read(&self, reg: XRegister) -> XValue
    where
        M: backend::ManagerRead,
    {
        if reg.is_zero() {
            return 0;
        }

        self.registers.read(reg as usize)
    }

    /// Write an integer to the registers.
    #[inline]
    pub fn write(&mut self, reg: XRegister, val: XValue)
    where
        M: backend::ManagerWrite,
    {
        if reg.is_zero() {
            return;
        }

        self.registers.write(reg as usize, val)
    }

    /// Read an integer from the registers without checking if it is 0 first.
    #[inline]
    pub fn read_nz(&self, reg: NonZeroXRegister) -> XValue
    where
        M: backend::ManagerRead,
    {
        self.registers.read(reg as usize)
    }

    /// Write an integer to the registers without checking if it is 0 first.
    #[inline]
    pub fn write_nz(&mut self, reg: NonZeroXRegister, val: XValue)
    where
        M: backend::ManagerWrite,
    {
        self.registers.write(reg as usize, val)
    }

    /// Reset the integer registers.
    pub fn reset(&mut self)
    where
        M: backend::ManagerWrite,
    {
        for i in 0..31 {
            self.registers.write(i, XValue::default());
        }
    }
}

impl XRegisters<Owned> {
    /// Get the byte offset from a pointer to `XRegisters` to the memory of the value
    /// stored in the `reg` in question.
    pub(crate) const fn xregister_offset(reg: NonZeroXRegister) -> usize {
        std::mem::offset_of!(Self, registers)
            + backend::Cells::<XValue, 31, Owned>::region_elem_offset(reg as usize)
    }
}

impl<M: backend::ManagerBase> NewState<M> for XRegisters<M> {
    fn new(manager: &mut M) -> Self
    where
        M: backend::ManagerAlloc,
    {
        XRegisters {
            registers: backend::Cells::new(manager),
        }
    }
}

impl<M: backend::ManagerClone> Clone for XRegisters<M> {
    fn clone(&self) -> Self {
        Self {
            registers: self.registers.clone(),
        }
    }
}

/// Register index for integer registers known from the opcode to be `!=x0`.
#[expect(non_camel_case_types, reason = "Consistent with RISC-V spec")]
#[repr(u8)]
#[derive(
    Clone,
    Copy,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize,
    strum::EnumIter,
)]
pub enum NonZeroXRegister {
    // This enum represents XRegisters known from the opcode to be `!=x0`, hence omitting
    // x0 from the list.
    // The `usize` representation of these constructors shall be used as an
    // index into the 31-element array holding the XRegisters.
    x1 = 0,
    x2,
    x3,
    x4,
    x5,
    x6,
    x7,
    x8,
    x9,
    x10,
    x11,
    x12,
    x13,
    x14,
    x15,
    x16,
    x17,
    x18,
    x19,
    x20,
    x21,
    x22,
    x23,
    x24,
    x25,
    x26,
    x27,
    x28,
    x29,
    x30,
    x31,
}

impl fmt::Display for NonZeroXRegister {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", XRegister::from(*self))
    }
}

impl NonZeroXRegister {
    /// Convert an `XRegister` to a `NonZeroXRegister`, provided `r != x0`.
    ///
    /// # Panics
    /// Panics if `r == x0`.
    pub const fn assert_from(r: XRegister) -> Self {
        match r {
            x0 => panic!("x0 is not a non-zero register"),
            // SAFETY: Excluding x0, XRegister is a
            // direct map to NonZeroXRegister, so safe to convert.
            r => unsafe { std::mem::transmute::<XRegister, Self>(r) },
        }
    }
}

/// ABI register names for NonZeroXRegister types used in backend tests.
pub mod nz {
    use super::NonZeroXRegister;

    pub const ra: NonZeroXRegister = NonZeroXRegister::x1;
    pub const sp: NonZeroXRegister = NonZeroXRegister::x2;
    pub const gp: NonZeroXRegister = NonZeroXRegister::x3;
    pub const tp: NonZeroXRegister = NonZeroXRegister::x4;
    pub const t0: NonZeroXRegister = NonZeroXRegister::x5;
    pub const t1: NonZeroXRegister = NonZeroXRegister::x6;
    pub const t2: NonZeroXRegister = NonZeroXRegister::x7;
    pub const s0: NonZeroXRegister = NonZeroXRegister::x8;
    pub const fp: NonZeroXRegister = NonZeroXRegister::x8;
    pub const s1: NonZeroXRegister = NonZeroXRegister::x9;
    pub const a0: NonZeroXRegister = NonZeroXRegister::x10;
    pub const a1: NonZeroXRegister = NonZeroXRegister::x11;
    pub const a2: NonZeroXRegister = NonZeroXRegister::x12;
    pub const a3: NonZeroXRegister = NonZeroXRegister::x13;
    pub const a4: NonZeroXRegister = NonZeroXRegister::x14;
    pub const a5: NonZeroXRegister = NonZeroXRegister::x15;
    pub const a6: NonZeroXRegister = NonZeroXRegister::x16;
    pub const a7: NonZeroXRegister = NonZeroXRegister::x17;
    pub const s2: NonZeroXRegister = NonZeroXRegister::x18;
    pub const s3: NonZeroXRegister = NonZeroXRegister::x19;
    pub const s4: NonZeroXRegister = NonZeroXRegister::x20;
    pub const s5: NonZeroXRegister = NonZeroXRegister::x21;
    pub const s6: NonZeroXRegister = NonZeroXRegister::x22;
    pub const s7: NonZeroXRegister = NonZeroXRegister::x23;
    pub const s8: NonZeroXRegister = NonZeroXRegister::x24;
    pub const s9: NonZeroXRegister = NonZeroXRegister::x25;
    pub const s10: NonZeroXRegister = NonZeroXRegister::x26;
    pub const s11: NonZeroXRegister = NonZeroXRegister::x27;
    pub const t3: NonZeroXRegister = NonZeroXRegister::x28;
    pub const t4: NonZeroXRegister = NonZeroXRegister::x29;
    pub const t5: NonZeroXRegister = NonZeroXRegister::x30;
    pub const t6: NonZeroXRegister = NonZeroXRegister::x31;
}

/// Floating-point number register index
#[expect(non_camel_case_types, reason = "Consistent with RISC-V spec")]
#[repr(u8)]
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    strum::EnumIter,
    Hash,
    serde::Serialize,
    serde::Deserialize,
)]
pub enum FRegister {
    f0 = 0,
    f1,
    f2,
    f3,
    f4,
    f5,
    f6,
    f7,
    f8,
    f9,
    f10,
    f11,
    f12,
    f13,
    f14,
    f15,
    f16,
    f17,
    f18,
    f19,
    f20,
    f21,
    f22,
    f23,
    f24,
    f25,
    f26,
    f27,
    f28,
    f29,
    f30,
    f31,
}

#[inline(always)]
pub const fn parse_fregister(r: u5) -> FRegister {
    let r = r.value();

    // SAFETY: the possible values of u5 are known to correspond to
    // the possible values of the underlying representation of FRegister.
    unsafe { std::mem::transmute(r) }
}

// We want those constructors at the module top-level.
pub use FRegister::*;

// ABI register names
pub const ft0: FRegister = f0;
pub const ft1: FRegister = f1;
pub const ft2: FRegister = f2;
pub const ft3: FRegister = f3;
pub const ft4: FRegister = f4;
pub const ft5: FRegister = f5;
pub const ft6: FRegister = f6;
pub const ft7: FRegister = f7;
pub const fs0: FRegister = f8;
pub const fs1: FRegister = f9;
pub const fa0: FRegister = f10;
pub const fa1: FRegister = f11;
pub const fa2: FRegister = f12;
pub const fa3: FRegister = f13;
pub const fa4: FRegister = f14;
pub const fa5: FRegister = f15;
pub const fa6: FRegister = f16;
pub const fa7: FRegister = f17;
pub const fs2: FRegister = f18;
pub const fs3: FRegister = f19;
pub const fs4: FRegister = f20;
pub const fs5: FRegister = f21;
pub const fs6: FRegister = f22;
pub const fs7: FRegister = f23;
pub const fs8: FRegister = f24;
pub const fs9: FRegister = f25;
pub const fs10: FRegister = f26;
pub const fs11: FRegister = f27;
pub const ft8: FRegister = f28;
pub const ft9: FRegister = f29;
pub const ft10: FRegister = f30;
pub const ft11: FRegister = f31;

impl fmt::Display for FRegister {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match *self {
            f0 => "ft0",
            f1 => "ft1",
            f2 => "ft2",
            f3 => "ft3",
            f4 => "ft4",
            f5 => "ft5",
            f6 => "ft6",
            f7 => "ft7",
            f8 => "fs0",
            f9 => "fs1",
            f10 => "fa0",
            f11 => "fa1",
            f12 => "fa2",
            f13 => "fa3",
            f14 => "fa4",
            f15 => "fa5",
            f16 => "fa6",
            f17 => "fa7",
            f18 => "fs2",
            f19 => "fs3",
            f20 => "fs4",
            f21 => "fs5",
            f22 => "fs6",
            f23 => "fs7",
            f24 => "fs8",
            f25 => "fs9",
            f26 => "fs10",
            f27 => "fs11",
            f28 => "ft8",
            f29 => "ft9",
            f30 => "ft10",
            f31 => "ft11",
        };
        f.write_str(name)
    }
}

/// Floating-point number register value
#[repr(transparent)]
#[derive(
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Default,
    Debug,
    derive_more::From,
    derive_more::Into,
    serde::Serialize,
    serde::Deserialize,
)]
pub struct FValue(u64);

impl ConstDefault for FValue {
    const DEFAULT: Self = Self(0);
}

impl backend::Elem for FValue {
    #[inline(always)]
    fn store(&mut self, source: &Self) {
        *self = *source;
    }

    #[inline(always)]
    fn to_stored_in_place(&mut self) {}

    #[inline(always)]
    fn from_stored_in_place(&mut self) {}

    #[inline(always)]
    fn from_stored(source: &Self) -> Self {
        *source
    }
}

/// Layout for [FRegisters]
pub type FRegistersLayout = backend::Array<FValue, 32>;

/// Floating-point number registers
pub struct FRegisters<M: backend::ManagerBase> {
    registers: backend::Cells<FValue, 32, M>,
}

impl<M: backend::ManagerBase> FRegisters<M> {
    /// Allocate a new floating-point registers state.
    pub fn new(manager: &mut M) -> Self
    where
        M: backend::ManagerAlloc,
    {
        Self {
            registers: backend::Cells::new(manager),
        }
    }

    /// Bind the floating-point register space to the allocated space.
    pub fn bind(space: backend::AllocatedOf<FRegistersLayout, M>) -> Self {
        FRegisters { registers: space }
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub fn struct_ref<'a, F: backend::FnManager<backend::Ref<'a, M>>>(
        &'a self,
    ) -> backend::AllocatedOf<FRegistersLayout, F::Output> {
        self.registers.struct_ref::<F>()
    }

    /// Reset the floating-point registers.
    pub fn reset(&mut self)
    where
        M: backend::ManagerWrite,
    {
        for i in 0..32 {
            self.registers.write(i, FValue::default());
        }
    }

    /// Read a floating-point number from the registers.
    #[inline(always)]
    pub fn read(&self, reg: FRegister) -> FValue
    where
        M: backend::ManagerRead,
    {
        self.registers.read(reg as usize)
    }

    /// Write a floating-point number to the registers.
    #[inline(always)]
    pub fn write(&mut self, reg: FRegister, val: FValue)
    where
        M: backend::ManagerWrite,
    {
        self.registers.write(reg as usize, val)
    }
}

impl<M: backend::ManagerClone> Clone for FRegisters<M> {
    fn clone(&self) -> Self {
        Self {
            registers: self.registers.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use arbitrary_int::Number;
    use strum::IntoEnumIterator;

    use super::*;
    use crate::backend_test;
    use crate::state_backend::ManagerRead;

    backend_test!(test_zero, F, {
        let mut registers = XRegisters::new(&mut F::manager());

        // x0 should always read 0.
        assert_eq!(registers.read(x0), 0);

        // Writing anything to x0 must not have an effect.
        registers.write(x0, 1337);
        assert_eq!(registers.read(x0), 0);
    });

    const NONZERO_REGISTERS: [XRegister; 31] = [
        x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20,
        x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31,
    ];

    backend_test!(test_arbitrary_register, F, {
        let mut registers = XRegisters::new(&mut F::manager());

        // Initialise the registers with something.
        for reg in NONZERO_REGISTERS {
            let value = rand::random();
            registers.write(reg, value);
            assert_eq!(registers.read(reg), value);
        }

        // Check state transitions work as expected.
        for reg in NONZERO_REGISTERS {
            let value = rand::random();
            let expected = {
                let mut snapshot = NONZERO_REGISTERS.map(|r| registers.read(r));
                snapshot[reg as usize] = value;
                snapshot
            };

            registers.write(reg, value);

            let after = NONZERO_REGISTERS.map(|r| registers.read(r));
            assert_eq!(after, expected);
        }
    });

    backend_test!(test_try_read_u32, F, {
        let mut registers = XRegisters::new(&mut F::manager());

        // Reading an integer that is too large should fail
        registers.write(x1, 1 << 32);
        assert!(registers.try_read::<u32>(x1).is_err());

        // Reading an integer that is negative should fail
        registers.write(x1, -1i64 as u64);
        assert!(registers.try_read::<u32>(x1).is_err());

        registers.write(x1, 42);
        assert_eq!(registers.try_read::<u32>(x1), Ok(42));
    });

    #[test]
    fn parse_xregister_zero_to_x0() {
        assert_eq!(0_u8, (XRegister::x0 as u8).wrapping_add(1));

        assert_eq!(parse_xregister(u5::new(0)), XRegister::x0);
    }

    #[test]
    fn parse_xregister_nonzero() {
        for (idx, xreg) in NONZERO_REGISTERS.iter().enumerate() {
            assert_eq!(*xreg as usize, idx);

            assert_eq!(parse_xregister(u5::new((idx + 1) as u8)), *xreg);
        }
    }

    #[test]
    fn parse_fregister_all() {
        for (idx, freg) in FRegister::iter().enumerate() {
            assert_eq!(freg as usize, idx);

            assert_eq!(parse_fregister(u5::new(idx as u8)), freg);
        }
    }

    #[test]
    fn fregister_bounds() {
        assert_eq!(FRegister::f0 as usize, u5::new(0).value() as usize);
        assert_eq!(FRegister::f31 as usize, u5::MAX.value() as usize);
    }

    #[test]
    fn test_nonzeroxregister_to_xregister_conversion() {
        let nzreg = NonZeroXRegister::iter().collect::<Vec<_>>();
        let reg = XRegister::iter().filter(|r| r != &x0).collect::<Vec<_>>();

        assert_eq!(nzreg.len(), reg.len());
        for i in 0..nzreg.len() {
            assert_eq!(nzreg[i] as u8, reg[i] as u8)
        }
    }

    #[test]
    fn test_xregister_offsets() {
        let registers = XRegisters::new(&mut Owned);
        let registers_ptr = (&registers) as *const XRegisters<Owned>;

        for reg in NonZeroXRegister::iter() {
            let offset = XRegisters::<Owned>::xregister_offset(reg);
            let val: &XValue = Owned::region_ref(registers.registers.region_ref(), reg as usize);

            // Safety: both pointers are valid
            let offset_refs = unsafe { (val as *const XValue).byte_offset_from(registers_ptr) };

            assert_eq!(
                offset_refs, offset as isize,
                "Calculated offset to elem not equal to offset from references for {reg:?}"
            );
        }
    }
}
