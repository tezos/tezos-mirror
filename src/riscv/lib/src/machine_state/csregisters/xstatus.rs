// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Module containing helpers for `mstatus` and `sstatus` registers.
//!
//! The `sstatus` register is a subset of the `mstatus` register.
//! This mechanism is described as "shadow" CSRs in RISC-V spec.

// Allow unused setters & getters
#![allow(dead_code)]
// Allow non snake case for setters & getters
#![allow(non_snake_case)]

use super::bits::NormaliseFields;
use crate::bits::Bits64;
use crate::bits::ConstantBits;
use crate::csr;
use crate::default::ConstDefault;
use crate::machine_state::mode::Mode;

#[derive(PartialEq, Eq, Clone, Copy, Debug, Default, serde::Serialize, serde::Deserialize)]
#[repr(u8)]
pub enum MPPValue {
    User = 0b00,
    #[default]
    Supervisor = 0b01,
    Machine = 0b11,
}

impl ConstDefault for MPPValue {
    const DEFAULT: Self = MPPValue::Supervisor;
}

impl From<MPPValue> for Mode {
    fn from(other: MPPValue) -> Mode {
        match other {
            MPPValue::User => Mode::User,
            MPPValue::Supervisor => Mode::Supervisor,
            MPPValue::Machine => Mode::Machine,
        }
    }
}

impl From<MPPValue> for u8 {
    fn from(value: MPPValue) -> Self {
        value as u8
    }
}

impl From<u8> for MPPValue {
    fn from(value: u8) -> Self {
        match value & 0b11 {
            0b00 => MPPValue::User,
            0b01 => MPPValue::Supervisor,
            0b11 => MPPValue::Machine,
            // WARL field, invalid value is considered User
            _ => MPPValue::User,
        }
    }
}

impl Bits64 for MPPValue {
    const WIDTH: usize = 2;

    fn from_bits(value: u64) -> Self {
        <MPPValue as From<u8>>::from(value as u8)
    }

    fn to_bits(&self) -> u64 {
        <MPPValue as Into<u8>>::into(*self) as u64
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Default, serde::Serialize, serde::Deserialize)]
#[repr(u8)]
pub enum SPPValue {
    User = 0b0,
    #[default]
    Supervisor = 0b1,
}

impl ConstDefault for SPPValue {
    const DEFAULT: Self = SPPValue::Supervisor;
}

impl From<u8> for SPPValue {
    fn from(value: u8) -> Self {
        match value & 1 {
            0b0 => SPPValue::User,
            0b1 => SPPValue::Supervisor,
            _ => unreachable!(),
        }
    }
}

impl From<SPPValue> for u8 {
    fn from(value: SPPValue) -> Self {
        value as u8
    }
}

impl Bits64 for SPPValue {
    const WIDTH: usize = 1;

    fn from_bits(value: u64) -> Self {
        <SPPValue as From<u8>>::from(value as u8)
    }

    fn to_bits(&self) -> u64 {
        <SPPValue as Into<u8>>::into(*self) as u64
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Default, serde::Serialize, serde::Deserialize)]
#[repr(u64)]
pub enum XLenValue {
    #[default]
    MXL64 = 0b10,
}

impl ConstDefault for XLenValue {
    const DEFAULT: Self = XLenValue::MXL64;
}

impl From<XLenValue> for u8 {
    fn from(value: XLenValue) -> Self {
        value as u8
    }
}

impl From<u8> for XLenValue {
    #[inline(always)]
    fn from(_value: u8) -> Self {
        Self::MXL64
    }
}

impl Bits64 for XLenValue {
    const WIDTH: usize = 2;

    #[inline(always)]
    fn from_bits(value: u64) -> Self {
        <XLenValue as From<u8>>::from(value as u8)
    }

    fn to_bits(&self) -> u64 {
        <XLenValue as Into<u8>>::into(*self) as u64
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Default, serde::Serialize, serde::Deserialize)]
#[repr(u64)]
pub enum ExtensionValue {
    Off = 0b00,
    #[default]
    Dirty = 0b11,
}

impl ConstDefault for ExtensionValue {
    const DEFAULT: Self = ExtensionValue::Dirty;
}

impl From<ExtensionValue> for u8 {
    fn from(value: ExtensionValue) -> Self {
        value as u8
    }
}

impl From<u8> for ExtensionValue {
    fn from(value: u8) -> Self {
        match value & 0b11 {
            0b00 => ExtensionValue::Off,
            _ => ExtensionValue::Dirty,
        }
    }
}

impl Bits64 for ExtensionValue {
    const WIDTH: usize = 2;

    fn from_bits(value: u64) -> Self {
        <ExtensionValue as From<u8>>::from(value as u8)
    }

    fn to_bits(&self) -> u64 {
        <ExtensionValue as Into<u8>>::into(*self) as u64
    }
}

csr! {
    pub struct MStatus {
        WPRI1: ConstantBits<1>,
        SIE: bool,
        WPRI2: ConstantBits<1>,
        MIE: bool,
        WPRI3: ConstantBits<1>,
        SPIE: bool,
        UBE: bool,
        MPIE: bool,
        SPP: SPPValue,
        VS: ConstantBits<2>,
        MPP: MPPValue,
        FS: ExtensionValue,
        XS: ExtensionValue,
        MPRV: bool,
        SUM: bool,
        MXR: bool,
        TVM: bool,
        TW: bool,
        TSR: bool,
        WPRI4: ConstantBits<9>,
        UXL: XLenValue,
        SXL: XLenValue,
        SBE: bool,
        MBE: bool,
        WPRI5: ConstantBits<25>,
        SD: bool,
    }
}

impl MStatus {
    pub fn to_sstatus(self) -> SStatus {
        SStatus::from_bits(self.to_bits())
    }
}

impl NormaliseFields for MStatus {
    fn normalise(self) -> Self {
        SStatus::normalise(self.to_sstatus()).to_mstatus(self)
    }
}

impl Default for MStatus {
    fn default() -> Self {
        MStatus::from_bits(0u64)
            // Interrupts are off
            .with_sie(false)
            .with_mie(false)
            // Interrupts were off before
            .with_spie(false)
            .with_mpie(false)
            // Previous privilege mode was supervisor
            .with_spp(SPPValue::default())
            .with_mpp(MPPValue::default())
            // Endianness is little-endian
            .with_ube(false)
            .with_sbe(false)
            .with_mbe(false)
            // Set register dirtiness
            .with_fs(ExtensionValue::default())
            .with_xs(ExtensionValue::default())
            .with_sd(false)
            // Registers are also 64-bit wide in user and supervisor mode
            .with_uxl(XLenValue::default())
            .with_sxl(XLenValue::default())
            // Load and stores should use current effective privilege
            .with_mprv(false)
            // Supervisor mode shall have access to user page mappings
            .with_sum(true)
            // Make instruction loads from executable pages fail
            .with_mxr(false)
            // Allow virtual-memory management configuration
            .with_tvm(false)
            // WFI instruction works normally
            .with_tw(false)
            // Allow SRET to work normally
            .with_tsr(false)
    }
}

csr! {
    pub struct SStatus {
        WPRI1: ConstantBits<1>,
        SIE: bool,
        WPRI2: ConstantBits<3>,
        SPIE: bool,
        UBE: bool,
        WPRI3: ConstantBits<1>,
        SPP: SPPValue,
        VS: ConstantBits<2>,
        WPRI4: ConstantBits<2>,
        FS: ExtensionValue,
        XS: ExtensionValue,
        WPRI5: ConstantBits<1>,
        SUM: bool,
        MXR: bool,
        WPRI6: ConstantBits<12>,
        UXL: XLenValue,
        WPRI7: ConstantBits<29>,
        SD: bool,
    }
}

impl Default for SStatus {
    fn default() -> Self {
        MStatus::default().to_sstatus()
    }
}

impl SStatus {
    pub fn to_mstatus(self, mstatus: MStatus) -> MStatus {
        mstatus
            .with_sie(self.sie())
            .with_spie(self.spie())
            .with_ube(self.ube())
            .with_spp(self.spp())
            .with_vs(self.vs())
            .with_fs(self.fs())
            .with_xs(self.xs())
            .with_sum(self.sum())
            .with_mxr(self.mxr())
            .with_uxl(self.uxl())
            .with_sd(self.sd())
    }
}

impl NormaliseFields for SStatus {
    fn normalise(self) -> Self {
        let any_dirty = self.fs() == ExtensionValue::Dirty || self.xs() == ExtensionValue::Dirty;
        self.with_sd(any_dirty)
    }
}

csr! {
    pub struct MNStatus {
        WPRI1: ConstantBits<3>,
        NMIE: ConstantBits<1, 1>,
        WPRI2: ConstantBits<3>,
        MNPV: ConstantBits<1, 0>,
        WPRI3: ConstantBits<3>,
        MNPP: MPPValue,
        WPRI4: ConstantBits<51>,
    }
}

impl NormaliseFields for MNStatus {
    fn normalise(self) -> Self {
        self
    }
}

#[cfg(test)]
mod tests {
    use crate::bits::Bits64;
    use crate::machine_state::csregisters::xstatus::ExtensionValue;
    use crate::machine_state::csregisters::xstatus::MPPValue;
    use crate::machine_state::csregisters::xstatus::SPPValue;
    use crate::machine_state::csregisters::xstatus::XLenValue;

    #[test]
    fn test_status_fields() {
        let field = bool::from_bits(0xF0F0_0000_AAAA_0001);
        assert!(field);

        let field = bool::from_bits(0x0002);
        assert!(!field);

        let field = ExtensionValue::from_bits(0b1111_0010);
        assert_eq!(field, ExtensionValue::Dirty);
        assert_eq!(field.to_bits(), 0b11);

        let field = XLenValue::from_bits(0b01);
        assert_eq!(field, XLenValue::MXL64);
        assert_eq!(field.to_bits(), 0b10);

        let field = MPPValue::from_bits(0b1010);
        assert_eq!(field, MPPValue::User);
        assert_eq!(field.to_bits(), 0b00);

        let field = SPPValue::from_bits(0b111);
        assert_eq!(field, SPPValue::Supervisor);
        assert_eq!(field.to_bits(), 0b1);
    }
}
