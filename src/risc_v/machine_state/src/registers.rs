// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

// We have several globals whose names we want to align with the RISC-V
// specification.
#![allow(non_upper_case_globals)]

use crate::backend::{self, Region};

/// Integer register index
#[allow(non_camel_case_types)] // To make names consistent with specification
#[repr(usize)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum XRegister {
    // The `usize` representation of these constructors shall be used as an
    // index into the 31-element array holding the registers.
    // x0 represents no entry in this array because it is handled separately.
    // Therefore we assign a dummy index to x0.
    x0 = 0xFFFF,
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

impl XRegister {
    #[inline]
    pub fn is_zero(self) -> bool {
        self == x0
    }
}

/// Integer register value
pub type XValue = u64;

/// Integer registers
pub struct XRegisters<M: backend::Manager> {
    registers: M::Region<XValue, 31>,
}

impl<M: backend::Manager> XRegisters<M> {
    /// Read an integer from the registers.
    #[inline]
    pub fn read(&self, reg: XRegister) -> XValue {
        if reg.is_zero() {
            return 0;
        }

        self.registers.read(reg as usize)
    }

    /// Write an integer to the registers.
    #[inline]
    pub fn write(&mut self, reg: XRegister, val: XValue) {
        if reg.is_zero() {
            return;
        }

        self.registers.write(reg as usize, val)
    }
}

/// Layout for [XRegisters]
pub type XRegistersLayout = backend::Array<XValue, 31>;

impl<M: backend::Manager> XRegisters<M> {
    /// Bind the integer register state to the given allocated space.
    pub fn new_in(space: backend::AllocatedOf<XRegistersLayout, M>) -> Self {
        XRegisters { registers: space }
    }
}

/// Floating-point number register index
#[allow(non_camel_case_types)] // To make names consistent with specification
#[repr(usize)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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

/// Floating-point number register value
// XXX: We probably want this wrapper around f64 to implement deterministic
// floating-point arithmetic.
#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub struct FValue(f64);

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

/// Floating-point number registers
pub struct FRegisters<M: backend::Manager> {
    registers: M::Region<FValue, 32>,
}

impl<M: backend::Manager> FRegisters<M> {
    /// Read a floating-point number from the registers.
    #[inline(always)]
    pub fn read(&self, reg: FRegister) -> FValue {
        self.registers.read(reg as usize)
    }

    /// Write a floating-point number to the registers.
    #[inline(always)]
    pub fn write(&mut self, reg: FRegister, val: FValue) {
        self.registers.write(reg as usize, val)
    }
}

/// Layout for [FRegisters]
pub type FRegistersLayout = backend::Array<FValue, 32>;

impl<M: backend::Manager> FRegisters<M> {
    /// Bind the floating-point register space to the allocated space.
    pub fn new_in(space: backend::AllocatedOf<FRegistersLayout, M>) -> Self {
        FRegisters { registers: space }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::backend::{tests::TestBackendFactory, Backend, BackendManagement, Layout};

    pub fn test_backend(factory: &mut impl TestBackendFactory) {
        test_zero(factory);
        test_arbitrary_register(factory);
    }

    fn test_zero<F: TestBackendFactory>(factory: &mut F) {
        let mut backend = factory.make::<XRegistersLayout>();
        let mut registers: XRegisters<
            <F::Backend<XRegistersLayout> as BackendManagement>::Manager<'_>,
        > = XRegisters::new_in(backend.allocate(XRegistersLayout::placed().into_location()));

        // x0 should always read 0.
        assert_eq!(registers.read(x0), 0);

        // Writing anything to x0 must not have an effect.
        registers.write(x0, 1337);
        assert_eq!(registers.read(x0), 0);
    }

    const NONZERO_REGISTERS: [XRegister; 31] = [
        x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20,
        x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31,
    ];

    fn test_arbitrary_register<F: TestBackendFactory>(factory: &mut F) {
        let mut backend = factory.make::<XRegistersLayout>();
        let mut registers: XRegisters<
            <F::Backend<XRegistersLayout> as BackendManagement>::Manager<'_>,
        > = XRegisters::new_in(backend.allocate(XRegistersLayout::placed().into_location()));

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
    }
}
