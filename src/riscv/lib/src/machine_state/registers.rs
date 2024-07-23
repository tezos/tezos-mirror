// SPDX-FileCopyrightText: 2023-2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

// We have several globals whose names we want to align with the RISC-V
// specification.
#![allow(non_upper_case_globals)]

use crate::machine_state::backend::{self, Region};
use arbitrary_int::u5;
use std::fmt;

/// Integer register index
#[allow(non_camel_case_types)] // To make names consistent with specification
#[repr(usize)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
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

pub fn parse_xregister(r: u5) -> XRegister {
    use XRegister::*;
    match r.value() {
        0b0_0000 => x0,
        0b0_0001 => x1,
        0b0_0010 => x2,
        0b0_0011 => x3,
        0b0_0100 => x4,
        0b0_0101 => x5,
        0b0_0110 => x6,
        0b0_0111 => x7,
        0b0_1000 => x8,
        0b0_1001 => x9,
        0b0_1010 => x10,
        0b0_1011 => x11,
        0b0_1100 => x12,
        0b0_1101 => x13,
        0b0_1110 => x14,
        0b0_1111 => x15,
        0b1_0000 => x16,
        0b1_0001 => x17,
        0b1_0010 => x18,
        0b1_0011 => x19,
        0b1_0100 => x20,
        0b1_0101 => x21,
        0b1_0110 => x22,
        0b1_0111 => x23,
        0b1_1000 => x24,
        0b1_1001 => x25,
        0b1_1010 => x26,
        0b1_1011 => x27,
        0b1_1100 => x28,
        0b1_1101 => x29,
        0b1_1110 => x30,
        0b1_1111 => x31,
        _ => unreachable!("Invalid register"),
    }
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
    pub fn bind(space: backend::AllocatedOf<XRegistersLayout, M>) -> Self {
        XRegisters { registers: space }
    }

    /// Reset the integer registers.
    pub fn reset(&mut self) {
        for i in 0..31 {
            self.registers.write(i, XValue::default());
        }
    }
}

/// Floating-point number register index
#[allow(non_camel_case_types)] // To make names consistent with specification
#[repr(usize)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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

pub fn parse_fregister(r: u5) -> FRegister {
    use FRegister::*;
    match r.value() {
        0b0_0000 => f0,
        0b0_0001 => f1,
        0b0_0010 => f2,
        0b0_0011 => f3,
        0b0_0100 => f4,
        0b0_0101 => f5,
        0b0_0110 => f6,
        0b0_0111 => f7,
        0b0_1000 => f8,
        0b0_1001 => f9,
        0b0_1010 => f10,
        0b0_1011 => f11,
        0b0_1100 => f12,
        0b0_1101 => f13,
        0b0_1110 => f14,
        0b0_1111 => f15,
        0b1_0000 => f16,
        0b1_0001 => f17,
        0b1_0010 => f18,
        0b1_0011 => f19,
        0b1_0100 => f20,
        0b1_0101 => f21,
        0b1_0110 => f22,
        0b1_0111 => f23,
        0b1_1000 => f24,
        0b1_1001 => f25,
        0b1_1010 => f26,
        0b1_1011 => f27,
        0b1_1100 => f28,
        0b1_1101 => f29,
        0b1_1110 => f30,
        0b1_1111 => f31,
        _ => unreachable!("Invalid register"),
    }
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
    Clone, Copy, PartialEq, PartialOrd, Default, Debug, derive_more::From, derive_more::Into,
)]
pub struct FValue(u64);

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
    pub fn bind(space: backend::AllocatedOf<FRegistersLayout, M>) -> Self {
        FRegisters { registers: space }
    }

    /// Reset the floating-point registers.
    pub fn reset(&mut self) {
        for i in 0..32 {
            self.registers.write(i, FValue::default());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        backend_test,
        machine_state::backend::{
            tests::{test_determinism, ManagerFor},
            Backend, Layout,
        },
    };

    backend_test!(test_zero, F, {
        let mut backend = F::new::<XRegistersLayout>();
        let mut registers: XRegisters<ManagerFor<'_, F, XRegistersLayout>> =
            XRegisters::bind(backend.allocate(XRegistersLayout::placed().into_location()));

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
        let mut backend = F::new::<XRegistersLayout>();
        let mut registers: XRegisters<ManagerFor<'_, F, XRegistersLayout>> =
            XRegisters::bind(backend.allocate(XRegistersLayout::placed().into_location()));

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

    backend_test!(test_xregs_reset, F, {
        test_determinism::<F, XRegistersLayout, _>(|space| {
            let mut registers: XRegisters<ManagerFor<'_, F, XRegistersLayout>> =
                XRegisters::bind(space);
            registers.reset();
        });
    });

    backend_test!(test_fregs_reset, F, {
        test_determinism::<F, FRegistersLayout, _>(|space| {
            let mut registers: FRegisters<ManagerFor<'_, F, FRegistersLayout>> =
                FRegisters::bind(space);
            registers.reset();
        });
    });
}
