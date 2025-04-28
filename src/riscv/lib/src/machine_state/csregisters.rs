// SPDX-FileCopyrightText: 2023-2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

#![allow(non_upper_case_globals)]

pub mod bits;
pub mod effects;
mod root;
pub mod satp;
pub mod values;
pub mod xstatus;

use num_enum::TryFromPrimitive;
use root::RootCSRegister;
use strum::IntoEnumIterator;
use values::CSRValues;
use values::CSRValuesLayout;
use values::MStatusValue;

use self::bits::NormaliseFields;
use self::satp::Satp;
use self::values::CSRValue;
use self::xstatus::ExtensionValue;
use self::xstatus::MNStatus;
use self::xstatus::MStatus;
use self::xstatus::SStatus;
use super::hart_state::HartState;
use super::memory::Address;
use super::mode::TrapMode;
use crate::bits::Bits64;
use crate::bits::ones;
use crate::bits::u64;
use crate::machine_state::mode::Mode;
use crate::state::NewState;
use crate::state_backend as backend;
use crate::state_backend::ManagerRead;
use crate::traps::Exception;
use crate::traps::Interrupt;
use crate::traps::TrapContext;
use crate::traps::TrapKind;

/// Privilege required to access a CSR
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Privilege {
    Unprivileged = 0,
    Supervisor = 1,
    Hypervisor = 2,
    Machine = 3,
}

/// CSR index
#[expect(non_camel_case_types, reason = "Consistent with RISC-V spec")]
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    strum::EnumIter,
    TryFromPrimitive,
    strum::Display,
    Hash,
    serde::Serialize,
    serde::Deserialize,
)]
#[repr(usize)]
pub enum CSRegister {
    // Unprivileged Floating-Point CSRs
    fflags = 0x001,
    frm = 0x002,
    fcsr = 0x003,

    // Unprivileged Counter/Timers
    cycle = 0xC00,
    time = 0xC01,
    instret = 0xC02,
    hpmcounter3 = 0xC03,
    hpmcounter4 = 0xC04,
    hpmcounter5 = 0xC05,
    hpmcounter6 = 0xC06,
    hpmcounter7 = 0xC07,
    hpmcounter8 = 0xC08,
    hpmcounter9 = 0xC09,
    hpmcounter10 = 0xC0A,
    hpmcounter11 = 0xC0B,
    hpmcounter12 = 0xC0C,
    hpmcounter13 = 0xC0D,
    hpmcounter14 = 0xC0E,
    hpmcounter15 = 0xC0F,
    hpmcounter16 = 0xC10,
    hpmcounter17 = 0xC11,
    hpmcounter18 = 0xC12,
    hpmcounter19 = 0xC13,
    hpmcounter20 = 0xC14,
    hpmcounter21 = 0xC15,
    hpmcounter22 = 0xC16,
    hpmcounter23 = 0xC17,
    hpmcounter24 = 0xC18,
    hpmcounter25 = 0xC19,
    hpmcounter26 = 0xC1A,
    hpmcounter27 = 0xC1B,
    hpmcounter28 = 0xC1C,
    hpmcounter29 = 0xC1D,
    hpmcounter30 = 0xC1E,
    hpmcounter31 = 0xC1F,

    // TODO: Remove everything in CSRegisters that requires privilieged access
    // Supervisor Trap Setup
    sstatus = 0x100,
    sie = 0x104,
    stvec = 0x105,
    scounteren = 0x106,

    // Supervisor Configuration
    senvcfg = 0x10A,

    // Supervisor Trap Handling
    sscratch = 0x140,
    sepc = 0x141,
    scause = 0x142,
    stval = 0x143,
    sip = 0x144,

    // Supervisor Protection and Translation
    satp = 0x180,

    // Supervisor Debug/Trace Registers
    scontext = 0x5A8,

    // Hypervisor Trap Setup
    hstatus = 0x600,
    hedeleg = 0x602,
    hideleg = 0x603,
    hie = 0x604,
    hcounteren = 0x606,
    hgeie = 0x607,

    // Hypervisor Trap Handling
    htval = 0x643,
    hip = 0x644,
    hvip = 0x645,
    htinst = 0x64A,
    hgeip = 0xE12,

    // Hypervisor Configuration
    henvcfg = 0x60A,

    // Hypervisor Protection and Translation
    hgatp = 0x680,

    // Hypervisor Debug/ Trace Registers
    hcontext = 0x6A8,

    // Hypervisor Counter/Timer Virtualization Registers
    htimedelta = 0x605,

    // Virtual Supervisor Registers
    vsstatus = 0x200,
    vsie = 0x204,
    vstvec = 0x205,
    vsscratch = 0x240,
    vsepc = 0x241,
    vscause = 0x242,
    vstval = 0x243,
    vsip = 0x244,
    vsatp = 0x280,

    // Machine Information Registers
    mvendorid = 0xF11,
    marchid = 0xF12,
    mimpid = 0xF13,
    mhartid = 0xF14,
    mconfigptr = 0xF15,

    // Machine Trap Setup
    mstatus = 0x300,
    misa = 0x301,
    medeleg = 0x302,
    mideleg = 0x303,
    mie = 0x304,
    mtvec = 0x305,
    mcounteren = 0x306,

    // Machine Trap Handling
    mscratch = 0x340,
    mepc = 0x341,
    mcause = 0x342,
    mtval = 0x343,
    mip = 0x344,
    mtinst = 0x34A,
    mtval2 = 0x34B,

    // Machine Configuration
    menvcfg = 0x30A,
    mseccfg = 0x747,

    // Machine Memory Protection
    pmpcfg0 = 0x3A0,
    pmpcfg2 = 0x3A2,
    pmpcfg4 = 0x3A4,
    pmpcfg6 = 0x3A6,
    pmpcfg8 = 0x3A8,
    pmpcfg10 = 0x3AA,
    pmpcfg12 = 0x3AC,
    pmpcfg14 = 0x3AE,
    pmpaddr0 = 0x3B0,
    pmpaddr1 = 0x3B1,
    pmpaddr2 = 0x3B2,
    pmpaddr3 = 0x3B3,
    pmpaddr4 = 0x3B4,
    pmpaddr5 = 0x3B5,
    pmpaddr6 = 0x3B6,
    pmpaddr7 = 0x3B7,
    pmpaddr8 = 0x3B8,
    pmpaddr9 = 0x3B9,
    pmpaddr10 = 0x3BA,
    pmpaddr11 = 0x3BB,
    pmpaddr12 = 0x3BC,
    pmpaddr13 = 0x3BD,
    pmpaddr14 = 0x3BE,
    pmpaddr15 = 0x3BF,
    pmpaddr16 = 0x3C0,
    pmpaddr17 = 0x3C1,
    pmpaddr18 = 0x3C2,
    pmpaddr19 = 0x3C3,
    pmpaddr20 = 0x3C4,
    pmpaddr21 = 0x3C5,
    pmpaddr22 = 0x3C6,
    pmpaddr23 = 0x3C7,
    pmpaddr24 = 0x3C8,
    pmpaddr25 = 0x3C9,
    pmpaddr26 = 0x3CA,
    pmpaddr27 = 0x3CB,
    pmpaddr28 = 0x3CC,
    pmpaddr29 = 0x3CD,
    pmpaddr30 = 0x3CE,
    pmpaddr31 = 0x3CF,
    pmpaddr32 = 0x3D0,
    pmpaddr33 = 0x3D1,
    pmpaddr34 = 0x3D2,
    pmpaddr35 = 0x3D3,
    pmpaddr36 = 0x3D4,
    pmpaddr37 = 0x3D5,
    pmpaddr38 = 0x3D6,
    pmpaddr39 = 0x3D7,
    pmpaddr40 = 0x3D8,
    pmpaddr41 = 0x3D9,
    pmpaddr42 = 0x3DA,
    pmpaddr43 = 0x3DB,
    pmpaddr44 = 0x3DC,
    pmpaddr45 = 0x3DD,
    pmpaddr46 = 0x3DE,
    pmpaddr47 = 0x3DF,
    pmpaddr48 = 0x3E0,
    pmpaddr49 = 0x3E1,
    pmpaddr50 = 0x3E2,
    pmpaddr51 = 0x3E3,
    pmpaddr52 = 0x3E4,
    pmpaddr53 = 0x3E5,
    pmpaddr54 = 0x3E6,
    pmpaddr55 = 0x3E7,
    pmpaddr56 = 0x3E8,
    pmpaddr57 = 0x3E9,
    pmpaddr58 = 0x3EA,
    pmpaddr59 = 0x3EB,
    pmpaddr60 = 0x3EC,
    pmpaddr61 = 0x3ED,
    pmpaddr62 = 0x3EE,
    pmpaddr63 = 0x3EF,

    // Machine Non-Maskable Interrupt Handling
    mnscratch = 0x740,
    mnepc = 0x741,
    mncause = 0x742,
    mnstatus = 0x744,

    // Machine Counter/Timers
    mcycle = 0xB00,
    minstret = 0xB02,
    mhpmcounter3 = 0xB03,
    mhpmcounter4 = 0xB04,
    mhpmcounter5 = 0xB05,
    mhpmcounter6 = 0xB06,
    mhpmcounter7 = 0xB07,
    mhpmcounter8 = 0xB08,
    mhpmcounter9 = 0xB09,
    mhpmcounter10 = 0xB0A,
    mhpmcounter11 = 0xB0B,
    mhpmcounter12 = 0xB0C,
    mhpmcounter13 = 0xB0D,
    mhpmcounter14 = 0xB0E,
    mhpmcounter15 = 0xB0F,
    mhpmcounter16 = 0xB10,
    mhpmcounter17 = 0xB11,
    mhpmcounter18 = 0xB12,
    mhpmcounter19 = 0xB13,
    mhpmcounter20 = 0xB14,
    mhpmcounter21 = 0xB15,
    mhpmcounter22 = 0xB16,
    mhpmcounter23 = 0xB17,
    mhpmcounter24 = 0xB18,
    mhpmcounter25 = 0xB19,
    mhpmcounter26 = 0xB1A,
    mhpmcounter27 = 0xB1B,
    mhpmcounter28 = 0xB1C,
    mhpmcounter29 = 0xB1D,
    mhpmcounter30 = 0xB1E,
    mhpmcounter31 = 0xB1F,

    // Machine Counter Setup
    mcountinhibit = 0x320,
    mhpmevent3 = 0x323,
    mhpmevent4 = 0x324,
    mhpmevent5 = 0x325,
    mhpmevent6 = 0x326,
    mhpmevent7 = 0x327,
    mhpmevent8 = 0x328,
    mhpmevent9 = 0x329,
    mhpmevent10 = 0x32A,
    mhpmevent11 = 0x32B,
    mhpmevent12 = 0x32C,
    mhpmevent13 = 0x32D,
    mhpmevent14 = 0x32E,
    mhpmevent15 = 0x32F,
    mhpmevent16 = 0x330,
    mhpmevent17 = 0x331,
    mhpmevent18 = 0x332,
    mhpmevent19 = 0x333,
    mhpmevent20 = 0x334,
    mhpmevent21 = 0x335,
    mhpmevent22 = 0x336,
    mhpmevent23 = 0x337,
    mhpmevent24 = 0x338,
    mhpmevent25 = 0x339,
    mhpmevent26 = 0x33A,
    mhpmevent27 = 0x33B,
    mhpmevent28 = 0x33C,
    mhpmevent29 = 0x33D,
    mhpmevent30 = 0x33E,
    mhpmevent31 = 0x33F,

    // Debug/Trace Registers (shared with Debug Mode)
    tselect = 0x7A0,
    tdata1 = 0x7A1,
    tdata2 = 0x7A2,
    tdata3 = 0x7A3,
    tcontrol = 0x7A5,
    mcontext = 0x7A8,

    // Debug Mode Registers
    dcsr = 0x7B0,
    dpc = 0x7B1,
    dscratch0 = 0x7B2,
    dscratch1 = 0x7B3,
}

#[expect(
    clippy::identity_op,
    reason = "Shifts by 0 are allowed for clarity and consistency"
)]
impl CSRegister {
    // Since read-only misa.MXL = 0b10, we have MXLEN = 64 from table 3.1
    const MXLEN: u64 = 64;
    const SXLEN: u64 = CSRegister::MXLEN;
    const MXL_ENCODING: CSRRepr = 0b10;

    /// Determine the priviledge level required to access this CSR.
    #[inline(always)]
    pub fn privilege(self) -> Privilege {
        match self as usize {
            0x000..=0x0FF
            | 0x400..=0x4FF
            | 0x800..=0x8FF
            | 0xC00..=0xC7F
            | 0xC80..=0xCBF
            | 0xCC0..=0xCFF => Privilege::Unprivileged,

            0x100..=0x1FF
            | 0x500..=0x57F
            | 0x580..=0x5BF
            | 0x5C0..=0x5FF
            | 0x900..=0x97F
            | 0x980..=0x9BF
            | 0x9C0..=0x9FF
            | 0xD00..=0xD7F
            | 0xD80..=0xDBF
            | 0xDC0..=0xDFF => Privilege::Supervisor,

            0x200..=0x2FF
            | 0x600..=0x67F
            | 0x680..=0x6BF
            | 0x6C0..=0x6FF
            | 0xA00..=0xA7F
            | 0xA80..=0xABF
            | 0xAC0..=0xAFF
            | 0xE00..=0xE7F
            | 0xE80..=0xEBF
            | 0xEC0..=0xEFF => Privilege::Hypervisor,

            0x300..=0x3FF
            | 0x700..=0x77F
            | 0x780..=0x79F
            | 0x7A0..=0x7AF
            | 0x7B0..=0x7BF
            | 0x7C0..=0x7FF
            | 0xB00..=0xB7F
            | 0xB80..=0xBBF
            | 0xBC0..=0xBFF
            | 0xF00..=0xF7F
            | 0xF80..=0xFBF
            | 0xFC0..=0xFFF => Privilege::Machine,

            reg => unreachable!("Invalid CSR {reg:#x}",),
        }
    }

    /// Determines if the register is read-only
    #[inline(always)]
    pub fn is_read_only(self) -> bool {
        // Rules & Table of read-write / read-only ranges are in section 2.1 & table 2.1
        (self as usize >> 10) & 0b11 == 0b11
    }

    /// Enforce the WPRI and WLRL field specifications.
    ///
    /// Either return the value to be written, or None to signify that no write is necessary,
    /// leaving the existing value in its place.
    #[inline(always)]
    pub fn make_value_writable(self, value: CSRRepr) -> Option<CSRRepr> {
        // respect the reserved WPRI fields, setting them to 0
        let value = self.clear_wpri_fields(value);
        // apply WARL rules
        let value = self.transform_warl_fields(value)?;
        // check if value is legal w.r.t. WLRL fields
        self.is_legal(value).then_some(value)
    }

    const WPRI_MASK_EMPTY: CSRRepr = CSRRepr::MAX;

    const WPRI_MASK_MSTATUS: CSRRepr =
        !((ones(1) << 0) | (ones(1) << 2) | (ones(1) << 4) | (ones(9) << 23) | (ones(25) << 38));

    const WPRI_MASK_MENVCFG: CSRRepr = !((ones(3) << 1) | (ones(54) << 8));

    const WPRI_MASK_MSECCFG: CSRRepr = !((ones(5) << 3) | (ones(CSRegister::MXLEN - 10) << 10));

    const WPRI_MASK_SSTATUS: CSRRepr = !((ones(1) << 0)
        | (ones(3) << 2)
        | (ones(1) << 7)
        | (ones(2) << 11)
        | (ones(1) << 17)
        | (ones(12) << 20)
        | (ones(29) << 34));

    const WPRI_MASK_SENVCFG: CSRRepr = !((ones(3) << 1) | (ones(CSRegister::SXLEN - 8) << 8));

    const WPRI_MASK_MNCAUSE: CSRRepr = !(ones(1) << (CSRegister::MXLEN - 1));

    const WPRI_MASK_MNSTATUS: CSRRepr =
        !((ones(3) << 0) | (ones(3) << 4) | (ones(3) << 8) | (ones(CSRegister::MXLEN - 13) << 13));

    /// Return the mask of non reserved bits, (WPRI bits are 0)
    /// Relevant section 2.3 - privileged spec
    #[inline(always)]
    pub fn wpri_mask(self) -> CSRRepr {
        match self {
            CSRegister::mstatus => CSRegister::WPRI_MASK_MSTATUS,
            CSRegister::menvcfg => CSRegister::WPRI_MASK_MENVCFG,
            CSRegister::mseccfg => CSRegister::WPRI_MASK_MSECCFG,
            CSRegister::sstatus => CSRegister::WPRI_MASK_SSTATUS,
            CSRegister::senvcfg => CSRegister::WPRI_MASK_SENVCFG,
            CSRegister::mncause => CSRegister::WPRI_MASK_MNCAUSE,
            CSRegister::mnstatus => CSRegister::WPRI_MASK_MNSTATUS,
            _ => CSRegister::WPRI_MASK_EMPTY,
        }
    }

    /// Ensures that WPRI fields are kept read-only zero.
    ///
    /// Conforming to Section 2.3 - privileged spec
    #[inline(always)]
    pub fn clear_wpri_fields(self, new_value: CSRRepr) -> CSRRepr {
        new_value & self.wpri_mask()
    }

    /// Possible `mcause` values, table 3.6
    const WLRL_MCAUSE_VALUES: [CSRRepr; 20] = {
        const INTERRUPT_BIT: CSRRepr = 1 << (CSRRepr::BITS - 1);
        [
            // interrupt exception codes
            INTERRUPT_BIT | 1,  // Supervisor software interrupt
            INTERRUPT_BIT | 3,  // Machine software interrupt
            INTERRUPT_BIT | 5,  // Supervisor timer interrupt
            INTERRUPT_BIT | 7,  // Machine timer interrupt
            INTERRUPT_BIT | 9,  // Supervisor external interrupt
            INTERRUPT_BIT | 11, // Machine external interrupt
            // other values between 0-15 are reserved
            // values >= 16 for platform use, we treat them as reserved

            // non-interrupt exception codes
            0,  // Instruction address misaligned
            1,  // Instruction address fault
            2,  // Illegal instruction
            3,  // Breakpoint
            4,  // Load address misaligned
            5,  // Load access fault
            6,  // Store/AMO address misaligned
            7,  // Store/AMO access fault
            8,  // Environment call from U-mode
            9,  // Environment call from S-mode
            11, // Environment call from M-mode
            12, // Instruction page fault
            13, // Load page fault
            15, // Store/AMO page fault
                // other values between 0-15 are reserved
                // values 16-23, 32-47, >= 64 are reserved
                // values 24-31, 48-63 are for custom use, we treat them as reserved
        ]
    };

    /// Possible `scause` values, table 4.2
    const WLRL_SCAUSE_VALUES: [u64; 16] = {
        const INTERRUPT_BIT: u64 = 1 << 63;
        [
            // interrupt exception codes
            INTERRUPT_BIT | 1, // Supervisor software interrupt
            INTERRUPT_BIT | 5, // Supervisor timer interrupt
            INTERRUPT_BIT | 9, // Supervisor external interrupt
            // other values between 0-15 are reserved
            // values >= 16 for platform use, we treat them as reserved

            // non-interrupt exception codes
            0,  // Instruction address misaligned
            1,  // Instruction address fault
            2,  // Illegal instruction
            3,  // Breakpoint
            4,  // Load address misaligned
            5,  // Load access fault
            6,  // Store/AMO address misaligned
            7,  // Store/AMO access fault
            8,  // Environment call from U-mode
            9,  // Environment call from S-mode
            12, // Instruction page fault
            13, // Load page fault
            15, // Store/AMO page fault
                // other values between 0-15 are reserved
                // values 16-23, 32-47, >= 64 are reserved
                // values 24-31, 48-63 are for custom use, we treat them as reserved
        ]
    };

    /// Obtain the legal values for a register.
    ///
    /// If all the values are legal or the register is not WLRL, return an empty list.
    #[inline(always)]
    pub fn legal_values(self) -> &'static [u64] {
        match self {
            CSRegister::mcause => &CSRegister::WLRL_MCAUSE_VALUES,
            CSRegister::scause => &CSRegister::WLRL_SCAUSE_VALUES,
            _ => &[],
        }
    }

    /// If the register is WLRL, return if `new_value` is legal, false otherwise
    ///
    /// Section 2.3 - privileged spec
    #[inline(always)]
    pub fn is_legal(self, new_value: CSRRepr) -> bool {
        let legal_values = self.legal_values();
        // if no legal values are defined, then the register is not WLRL
        legal_values.is_empty() || legal_values.contains(&new_value)
    }

    /// Value for CSR `misa`, see section 3.1.1 & tables 3.1 (MXL) & 3.2 (Extensions)
    const WARL_MISA_VALUE: CSRRepr = {
        /* MXLEN encoding of 64 bits */
        const MXL_MASK: u64 = CSRegister::MXL_ENCODING << 62;
        /* Extensions (A + C + D + F + I + M + S + U) */
        const ATOMIC_EXT: u64 = 1 << 0;
        const COMPRESSED_EXT: u64 = 1 << 2;
        const DOUBLE_EXT: u64 = 1 << 3;
        const SINGLE_EXT: u64 = 1 << 5;
        const RV64I_ISA_EXT: u64 = 1 << 8;
        const MULT_DIV_EXT: u64 = 1 << 12;
        const SUPERVISOR_EXT: u64 = 1 << 18;
        const USER_EXT: u64 = 1 << 20;
        /* MXL */
        MXL_MASK |
        /* Extensions */
        ATOMIC_EXT |
        COMPRESSED_EXT |
        DOUBLE_EXT |
        SINGLE_EXT |
        RV64I_ISA_EXT |
        MULT_DIV_EXT |
        SUPERVISOR_EXT |
        USER_EXT
    };

    /// Ensures WARL registers / fields are respected
    ///
    /// Section 2.3 - privileged spec
    ///
    /// If `None` is returned, then no update must take place
    #[inline(always)]
    pub fn transform_warl_fields(self, new_value: CSRRepr) -> Option<CSRRepr> {
        let write_value = match self {
            CSRegister::misa => CSRegister::WARL_MISA_VALUE,
            CSRegister::medeleg => new_value & CSRegister::WARL_MASK_MEDELEG,
            CSRegister::mideleg => new_value & CSRegister::WARL_MASK_MIDELEG,
            CSRegister::mtvec | CSRegister::stvec => new_value & CSRegister::WARL_MASK_XTVEC,
            CSRegister::mip | CSRegister::mie => new_value & CSRegister::WARL_MASK_MIP_MIE,
            CSRegister::sip | CSRegister::sie => new_value & CSRegister::WARL_MASK_SIP_SIE,
            CSRegister::mepc | CSRegister::sepc | CSRegister::mnepc => {
                new_value & CSRegister::WARL_MASK_XEPC
            }
            CSRegister::satp => Satp::from_bits(new_value).normalise().to_bits(),
            CSRegister::mstatus => MStatus::from_bits(new_value).normalise().to_bits(),
            CSRegister::sstatus => SStatus::from_bits(new_value).normalise().to_bits(),
            CSRegister::mnstatus => MNStatus::from_bits(new_value).normalise().to_bits(),
            _ => new_value,
        };
        Some(write_value)
    }

    /// See section 3.1.8 and table 3.6
    ///
    /// Exception codes to delegate.
    /// If an exception can't be thrown from a lower privilege mode, set it here read-only 0
    const WARL_MASK_MEDELEG: CSRRepr = !(
        (ones(1) << 10) | (ones(1) << 11) | (ones(1) << 14) | (ones(CSRegister::MXLEN - 16) << 16)
        // reserved & custom use
    );

    /// See section 3.1.8 and table 3.6
    ///
    /// Interrupt codes to delegate.
    /// If an interrupt can't be thrown from a lower privilege mode, set it here read-only 0
    const WARL_MASK_MIDELEG: CSRRepr = !(
        (ones(1) << 0)
            | (ones(1) << 2)
            | (ones(1) << 4)
            | (ones(1) << 6)
            | (ones(1) << 8)
            | (ones(1) << 10)
            | (ones(4) << 12)
            | (ones(CSRegister::MXLEN - 16) << 16)
        // custom use
    );

    /// `mtvec.MODE = mtvec[1:0]`.
    /// Only `0` and `1` values are allowed for `MODE`, so we treat `MODE[1]` as read-only 0
    ///
    /// `mtvec.BASE = mtvec[MXLEN-1:2] << 2` (since it has to be 4-byte aligned).
    /// The same applies for stvec. Sections 3.1.7 & 4.1.2
    const WARL_MASK_XTVEC: CSRRepr = !(ones(1) << 1);

    /// WARL mask for mip/mie interrupt bits.
    ///
    /// 0-15 are for standard interrupts. The rest are for custom used and are treated as reserved
    const WARL_MASK_MIP_MIE: CSRRepr = Interrupt::MACHINE_BIT_MASK | Interrupt::SUPERVISOR_BIT_MASK;

    /// WARL mask for sip/sie interrupt bits.
    ///
    /// 0-15 are for standard interrupts. The rest are for custom used and are treated as reserved
    const WARL_MASK_SIP_SIE: CSRRepr = Interrupt::SUPERVISOR_BIT_MASK;

    /// WARL mask for mepc/sepc/mnepc addresses.
    ///
    /// Since extension C is supported, we only make the low bit read-only 0
    const WARL_MASK_XEPC: CSRRepr = !1;

    /// FCSR mask
    const FCSR_MASK: CSRRepr = Self::FRM_MASK | Self::FFLAGS_MASK;

    /// FRM mask
    const FRM_MASK: CSRRepr = 0b111 << Self::FRM_SHIFT;

    /// FRM is bits 5..7
    const FRM_SHIFT: usize = 5;

    /// FFLAGS mask
    const FFLAGS_MASK: CSRRepr = 0b11111;

    /// Get the default value for the register.
    fn default_value(&self) -> u64 {
        match self {
            CSRegister::mnscratch => 0,
            CSRegister::mnepc => 0,
            CSRegister::mnstatus => MNStatus::from_bits(0).normalise().to_bits(),
            CSRegister::mncause => {
                // The interrupt bit of mncause is always 1
                ones(1) << 31
            }

            CSRegister::cycle
            | CSRegister::time
            | CSRegister::instret
            | CSRegister::mcycle
            | CSRegister::minstret => {
                // Default is that the machine starts at 0
                0
            }

            CSRegister::hpmcounter3
            | CSRegister::hpmcounter4
            | CSRegister::hpmcounter5
            | CSRegister::hpmcounter6
            | CSRegister::hpmcounter7
            | CSRegister::hpmcounter8
            | CSRegister::hpmcounter9
            | CSRegister::hpmcounter10
            | CSRegister::hpmcounter11
            | CSRegister::hpmcounter12
            | CSRegister::hpmcounter13
            | CSRegister::hpmcounter14
            | CSRegister::hpmcounter15
            | CSRegister::hpmcounter16
            | CSRegister::hpmcounter17
            | CSRegister::hpmcounter18
            | CSRegister::hpmcounter19
            | CSRegister::hpmcounter20
            | CSRegister::hpmcounter21
            | CSRegister::hpmcounter22
            | CSRegister::hpmcounter23
            | CSRegister::hpmcounter24
            | CSRegister::hpmcounter25
            | CSRegister::hpmcounter26
            | CSRegister::hpmcounter27
            | CSRegister::hpmcounter28
            | CSRegister::hpmcounter29
            | CSRegister::hpmcounter30
            | CSRegister::hpmcounter31
            | CSRegister::mhpmcounter3
            | CSRegister::mhpmcounter4
            | CSRegister::mhpmcounter5
            | CSRegister::mhpmcounter6
            | CSRegister::mhpmcounter7
            | CSRegister::mhpmcounter8
            | CSRegister::mhpmcounter9
            | CSRegister::mhpmcounter10
            | CSRegister::mhpmcounter11
            | CSRegister::mhpmcounter12
            | CSRegister::mhpmcounter13
            | CSRegister::mhpmcounter14
            | CSRegister::mhpmcounter15
            | CSRegister::mhpmcounter16
            | CSRegister::mhpmcounter17
            | CSRegister::mhpmcounter18
            | CSRegister::mhpmcounter19
            | CSRegister::mhpmcounter20
            | CSRegister::mhpmcounter21
            | CSRegister::mhpmcounter22
            | CSRegister::mhpmcounter23
            | CSRegister::mhpmcounter24
            | CSRegister::mhpmcounter25
            | CSRegister::mhpmcounter26
            | CSRegister::mhpmcounter27
            | CSRegister::mhpmcounter28
            | CSRegister::mhpmcounter29
            | CSRegister::mhpmcounter30
            | CSRegister::mhpmcounter31 => {
                // All counters shall start at 0 again
                0
            }

            CSRegister::mhpmevent3
            | CSRegister::mhpmevent4
            | CSRegister::mhpmevent5
            | CSRegister::mhpmevent6
            | CSRegister::mhpmevent7
            | CSRegister::mhpmevent8
            | CSRegister::mhpmevent9
            | CSRegister::mhpmevent10
            | CSRegister::mhpmevent11
            | CSRegister::mhpmevent12
            | CSRegister::mhpmevent13
            | CSRegister::mhpmevent14
            | CSRegister::mhpmevent15
            | CSRegister::mhpmevent16
            | CSRegister::mhpmevent17
            | CSRegister::mhpmevent18
            | CSRegister::mhpmevent19
            | CSRegister::mhpmevent20
            | CSRegister::mhpmevent21
            | CSRegister::mhpmevent22
            | CSRegister::mhpmevent23
            | CSRegister::mhpmevent24
            | CSRegister::mhpmevent25
            | CSRegister::mhpmevent26
            | CSRegister::mhpmevent27
            | CSRegister::mhpmevent28
            | CSRegister::mhpmevent29
            | CSRegister::mhpmevent30
            | CSRegister::mhpmevent31 => {
                // Zero means "no event"
                0
            }

            CSRegister::mcountinhibit => {
                // All counter are enabled
                0
            }

            CSRegister::scounteren | CSRegister::mcounteren => {
                // All counters are readable in all privilege levels
                ones(32)
            }

            CSRegister::fflags => {
                // Resets accrued floating-point exceptions
                0b00000
            }

            CSRegister::frm => {
                // 000 = RNE aka "round to nearest, ties to even"
                0b000
            }

            CSRegister::fcsr => {
                // fcsr is a combination of fflags and fcsr
                CSRegister::fflags.default_value() & (CSRegister::frm.default_value() << 5)
            }

            CSRegister::pmpcfg0
            | CSRegister::pmpcfg2
            | CSRegister::pmpcfg4
            | CSRegister::pmpcfg6
            | CSRegister::pmpcfg8
            | CSRegister::pmpcfg10
            | CSRegister::pmpcfg12
            | CSRegister::pmpcfg14 => {
                // Physical-memory protection configuration is off initially
                0
            }

            CSRegister::pmpaddr0
            | CSRegister::pmpaddr1
            | CSRegister::pmpaddr2
            | CSRegister::pmpaddr3
            | CSRegister::pmpaddr4
            | CSRegister::pmpaddr5
            | CSRegister::pmpaddr6
            | CSRegister::pmpaddr7
            | CSRegister::pmpaddr8
            | CSRegister::pmpaddr9
            | CSRegister::pmpaddr10
            | CSRegister::pmpaddr11
            | CSRegister::pmpaddr12
            | CSRegister::pmpaddr13
            | CSRegister::pmpaddr14
            | CSRegister::pmpaddr15
            | CSRegister::pmpaddr16
            | CSRegister::pmpaddr17
            | CSRegister::pmpaddr18
            | CSRegister::pmpaddr19
            | CSRegister::pmpaddr20
            | CSRegister::pmpaddr21
            | CSRegister::pmpaddr22
            | CSRegister::pmpaddr23
            | CSRegister::pmpaddr24
            | CSRegister::pmpaddr25
            | CSRegister::pmpaddr26
            | CSRegister::pmpaddr27
            | CSRegister::pmpaddr28
            | CSRegister::pmpaddr29
            | CSRegister::pmpaddr30
            | CSRegister::pmpaddr31
            | CSRegister::pmpaddr32
            | CSRegister::pmpaddr33
            | CSRegister::pmpaddr34
            | CSRegister::pmpaddr35
            | CSRegister::pmpaddr36
            | CSRegister::pmpaddr37
            | CSRegister::pmpaddr38
            | CSRegister::pmpaddr39
            | CSRegister::pmpaddr40
            | CSRegister::pmpaddr41
            | CSRegister::pmpaddr42
            | CSRegister::pmpaddr43
            | CSRegister::pmpaddr44
            | CSRegister::pmpaddr45
            | CSRegister::pmpaddr46
            | CSRegister::pmpaddr47
            | CSRegister::pmpaddr48
            | CSRegister::pmpaddr49
            | CSRegister::pmpaddr50
            | CSRegister::pmpaddr51
            | CSRegister::pmpaddr52
            | CSRegister::pmpaddr53
            | CSRegister::pmpaddr54
            | CSRegister::pmpaddr55
            | CSRegister::pmpaddr56
            | CSRegister::pmpaddr57
            | CSRegister::pmpaddr58
            | CSRegister::pmpaddr59
            | CSRegister::pmpaddr60
            | CSRegister::pmpaddr61
            | CSRegister::pmpaddr62
            | CSRegister::pmpaddr63 => {
                // Physical-memory protection configuration is off initially
                0
            }

            // We're always on hart 0
            CSRegister::mhartid => 0,

            // Vendor ID is not implemented
            CSRegister::mvendorid => 0,

            // Arch ID is not implemented
            CSRegister::marchid => 0,

            // Implementation ID is not implemented
            CSRegister::mimpid => 0,

            // misa is pretty much fixed
            CSRegister::misa => CSRegister::WARL_MISA_VALUE,

            // Scratch registers are 0 by default
            CSRegister::mscratch | CSRegister::sscratch => 0,

            // Project view from mstatus
            CSRegister::sstatus => SStatus::default().to_bits(),
            CSRegister::mstatus => MStatus::default().to_bits(),

            // Trap handling shall not be set up initially
            CSRegister::stvec | CSRegister::mtvec => 0,

            // No interrupts are enabled
            CSRegister::sie | CSRegister::mie => 0,

            // No address translation initially
            CSRegister::satp => Satp::default().to_bits(),

            // No exception or trap inflight or pending
            CSRegister::scause | CSRegister::mcause => 0,
            CSRegister::sepc | CSRegister::mepc => 0,
            CSRegister::stval | CSRegister::mtval | CSRegister::mtval2 => 0,
            CSRegister::sip | CSRegister::mip => 0,
            CSRegister::mtinst => 0,

            // No specific environment configuration
            CSRegister::senvcfg | CSRegister::menvcfg => 0,

            // No hardware configuration supported
            CSRegister::mconfigptr => 0,

            // Delegate all exceptions and interrupts to S mode
            CSRegister::medeleg => ones(CSRegister::MXLEN),
            CSRegister::mideleg => ones(CSRegister::MXLEN),

            // Security extensions are not enabled
            CSRegister::mseccfg => 0,

            // Unsupported debug, hypervisor and virtual supervisor extensions
            CSRegister::scontext => 0,
            CSRegister::hstatus => 0,
            CSRegister::hedeleg => 0,
            CSRegister::hideleg => 0,
            CSRegister::hie => 0,
            CSRegister::hcounteren => 0,
            CSRegister::hgeie => 0,
            CSRegister::htval => 0,
            CSRegister::hip => 0,
            CSRegister::hvip => 0,
            CSRegister::htinst => 0,
            CSRegister::hgeip => 0,
            CSRegister::henvcfg => 0,
            CSRegister::hgatp => 0,
            CSRegister::hcontext => 0,
            CSRegister::htimedelta => 0,
            CSRegister::vsstatus => 0,
            CSRegister::vsie => 0,
            CSRegister::vstvec => 0,
            CSRegister::vsscratch => 0,
            CSRegister::vsepc => 0,
            CSRegister::vscause => 0,
            CSRegister::vstval => 0,
            CSRegister::vsip => 0,
            CSRegister::vsatp => 0,
            CSRegister::tselect => 0,
            CSRegister::tdata1 => 0,
            CSRegister::tdata2 => 0,
            CSRegister::tdata3 => 0,
            CSRegister::tcontrol => 0,
            CSRegister::mcontext => 0,
            CSRegister::dcsr => 0,
            CSRegister::dpc => 0,
            CSRegister::dscratch0 => 0,
            CSRegister::dscratch1 => 0,
        }
    }

    /// Attempt to parse the 32-bit integer as a register identifier.
    pub const fn try_parse(r: u32) -> Option<Self> {
        use CSRegister::*;

        match r {
            // Unprivileged Floating-Point CSRs
            0x001 => Some(fflags),
            0x002 => Some(frm),
            0x003 => Some(fcsr),

            // Unprivileged Counter/Timers
            0xC00 => Some(cycle),
            0xC01 => Some(time),
            0xC02 => Some(instret),
            0xC03 => Some(hpmcounter3),
            0xC04 => Some(hpmcounter4),
            0xC05 => Some(hpmcounter5),
            0xC06 => Some(hpmcounter6),
            0xC07 => Some(hpmcounter7),
            0xC08 => Some(hpmcounter8),
            0xC09 => Some(hpmcounter9),
            0xC0A => Some(hpmcounter10),
            0xC0B => Some(hpmcounter11),
            0xC0C => Some(hpmcounter12),
            0xC0D => Some(hpmcounter13),
            0xC0E => Some(hpmcounter14),
            0xC0F => Some(hpmcounter15),
            0xC10 => Some(hpmcounter16),
            0xC11 => Some(hpmcounter17),
            0xC12 => Some(hpmcounter18),
            0xC13 => Some(hpmcounter19),
            0xC14 => Some(hpmcounter20),
            0xC15 => Some(hpmcounter21),
            0xC16 => Some(hpmcounter22),
            0xC17 => Some(hpmcounter23),
            0xC18 => Some(hpmcounter24),
            0xC19 => Some(hpmcounter25),
            0xC1A => Some(hpmcounter26),
            0xC1B => Some(hpmcounter27),
            0xC1C => Some(hpmcounter28),
            0xC1D => Some(hpmcounter29),
            0xC1E => Some(hpmcounter30),
            0xC1F => Some(hpmcounter31),

            // Supervisor Trap Setup
            0x100 => Some(sstatus),
            0x104 => Some(sie),
            0x105 => Some(stvec),
            0x106 => Some(scounteren),

            // Supervisor Configuration
            0x10A => Some(senvcfg),

            // Supervisor Trap Handling
            0x140 => Some(sscratch),
            0x141 => Some(sepc),
            0x142 => Some(scause),
            0x143 => Some(stval),
            0x144 => Some(sip),

            // Supervisor Protection and Translation
            0x180 => Some(satp),

            // Supervisor Debug/Trace Registers
            0x5A8 => Some(scontext),

            // Hypervisor Trap Setup
            0x600 => Some(hstatus),
            0x602 => Some(hedeleg),
            0x603 => Some(hideleg),
            0x604 => Some(hie),
            0x606 => Some(hcounteren),
            0x607 => Some(hgeie),

            // Hypervisor Trap Handling
            0x643 => Some(htval),
            0x644 => Some(hip),
            0x645 => Some(hvip),
            0x64A => Some(htinst),
            0xE12 => Some(hgeip),

            // Hypervisor Configuration
            0x60A => Some(henvcfg),

            // Hypervisor Protection and Translation
            0x680 => Some(hgatp),

            // Hypervisor Debug/ Trace Registers
            0x6A8 => Some(hcontext),

            // Hypervisor Counter/Timer Virtualization Registers
            0x605 => Some(htimedelta),

            // Virtual Supervisor Registers
            0x200 => Some(vsstatus),
            0x204 => Some(vsie),
            0x205 => Some(vstvec),
            0x240 => Some(vsscratch),
            0x241 => Some(vsepc),
            0x242 => Some(vscause),
            0x243 => Some(vstval),
            0x244 => Some(vsip),
            0x280 => Some(vsatp),

            // Machine Information Registers
            0xF11 => Some(mvendorid),
            0xF12 => Some(marchid),
            0xF13 => Some(mimpid),
            0xF14 => Some(mhartid),
            0xF15 => Some(mconfigptr),

            // Machine Trap Setup
            0x300 => Some(mstatus),
            0x301 => Some(misa),
            0x302 => Some(medeleg),
            0x303 => Some(mideleg),
            0x304 => Some(mie),
            0x305 => Some(mtvec),
            0x306 => Some(mcounteren),

            // Machine Trap Handling
            0x340 => Some(mscratch),
            0x341 => Some(mepc),
            0x342 => Some(mcause),
            0x343 => Some(mtval),
            0x344 => Some(mip),
            0x34A => Some(mtinst),
            0x34B => Some(mtval2),

            // Machine Configuration
            0x30A => Some(menvcfg),
            0x747 => Some(mseccfg),

            // Machine Memory Protection
            0x3A0 => Some(pmpcfg0),
            0x3A2 => Some(pmpcfg2),
            0x3A4 => Some(pmpcfg4),
            0x3A6 => Some(pmpcfg6),
            0x3A8 => Some(pmpcfg8),
            0x3AA => Some(pmpcfg10),
            0x3AC => Some(pmpcfg12),
            0x3AE => Some(pmpcfg14),
            0x3B0 => Some(pmpaddr0),
            0x3B1 => Some(pmpaddr1),
            0x3B2 => Some(pmpaddr2),
            0x3B3 => Some(pmpaddr3),
            0x3B4 => Some(pmpaddr4),
            0x3B5 => Some(pmpaddr5),
            0x3B6 => Some(pmpaddr6),
            0x3B7 => Some(pmpaddr7),
            0x3B8 => Some(pmpaddr8),
            0x3B9 => Some(pmpaddr9),
            0x3BA => Some(pmpaddr10),
            0x3BB => Some(pmpaddr11),
            0x3BC => Some(pmpaddr12),
            0x3BD => Some(pmpaddr13),
            0x3BE => Some(pmpaddr14),
            0x3BF => Some(pmpaddr15),
            0x3C0 => Some(pmpaddr16),
            0x3C1 => Some(pmpaddr17),
            0x3C2 => Some(pmpaddr18),
            0x3C3 => Some(pmpaddr19),
            0x3C4 => Some(pmpaddr20),
            0x3C5 => Some(pmpaddr21),
            0x3C6 => Some(pmpaddr22),
            0x3C7 => Some(pmpaddr23),
            0x3C8 => Some(pmpaddr24),
            0x3C9 => Some(pmpaddr25),
            0x3CA => Some(pmpaddr26),
            0x3CB => Some(pmpaddr27),
            0x3CC => Some(pmpaddr28),
            0x3CD => Some(pmpaddr29),
            0x3CE => Some(pmpaddr30),
            0x3CF => Some(pmpaddr31),
            0x3D0 => Some(pmpaddr32),
            0x3D1 => Some(pmpaddr33),
            0x3D2 => Some(pmpaddr34),
            0x3D3 => Some(pmpaddr35),
            0x3D4 => Some(pmpaddr36),
            0x3D5 => Some(pmpaddr37),
            0x3D6 => Some(pmpaddr38),
            0x3D7 => Some(pmpaddr39),
            0x3D8 => Some(pmpaddr40),
            0x3D9 => Some(pmpaddr41),
            0x3DA => Some(pmpaddr42),
            0x3DB => Some(pmpaddr43),
            0x3DC => Some(pmpaddr44),
            0x3DD => Some(pmpaddr45),
            0x3DE => Some(pmpaddr46),
            0x3DF => Some(pmpaddr47),
            0x3E0 => Some(pmpaddr48),
            0x3E1 => Some(pmpaddr49),
            0x3E2 => Some(pmpaddr50),
            0x3E3 => Some(pmpaddr51),
            0x3E4 => Some(pmpaddr52),
            0x3E5 => Some(pmpaddr53),
            0x3E6 => Some(pmpaddr54),
            0x3E7 => Some(pmpaddr55),
            0x3E8 => Some(pmpaddr56),
            0x3E9 => Some(pmpaddr57),
            0x3EA => Some(pmpaddr58),
            0x3EB => Some(pmpaddr59),
            0x3EC => Some(pmpaddr60),
            0x3ED => Some(pmpaddr61),
            0x3EE => Some(pmpaddr62),
            0x3EF => Some(pmpaddr63),

            // Machine Non-Maskable Interrupt Handling
            // The draft `Smrnmi` extension is not supported in objdump, printing
            // CSR address directly instead
            0x740 => Some(mnscratch),
            0x741 => Some(mnepc),
            0x742 => Some(mncause),
            0x744 => Some(mnstatus),

            // Machine Counter/Timers
            0xB00 => Some(mcycle),
            0xB02 => Some(minstret),
            0xB03 => Some(mhpmcounter3),
            0xB04 => Some(mhpmcounter4),
            0xB05 => Some(mhpmcounter5),
            0xB06 => Some(mhpmcounter6),
            0xB07 => Some(mhpmcounter7),
            0xB08 => Some(mhpmcounter8),
            0xB09 => Some(mhpmcounter9),
            0xB0A => Some(mhpmcounter10),
            0xB0B => Some(mhpmcounter11),
            0xB0C => Some(mhpmcounter12),
            0xB0D => Some(mhpmcounter13),
            0xB0E => Some(mhpmcounter14),
            0xB0F => Some(mhpmcounter15),
            0xB10 => Some(mhpmcounter16),
            0xB11 => Some(mhpmcounter17),
            0xB12 => Some(mhpmcounter18),
            0xB13 => Some(mhpmcounter19),
            0xB14 => Some(mhpmcounter20),
            0xB15 => Some(mhpmcounter21),
            0xB16 => Some(mhpmcounter22),
            0xB17 => Some(mhpmcounter23),
            0xB18 => Some(mhpmcounter24),
            0xB19 => Some(mhpmcounter25),
            0xB1A => Some(mhpmcounter26),
            0xB1B => Some(mhpmcounter27),
            0xB1C => Some(mhpmcounter28),
            0xB1D => Some(mhpmcounter29),
            0xB1E => Some(mhpmcounter30),
            0xB1F => Some(mhpmcounter31),

            // Machine Counter Setup
            0x320 => Some(mcountinhibit),
            0x323 => Some(mhpmevent3),
            0x324 => Some(mhpmevent4),
            0x325 => Some(mhpmevent5),
            0x326 => Some(mhpmevent6),
            0x327 => Some(mhpmevent7),
            0x328 => Some(mhpmevent8),
            0x329 => Some(mhpmevent9),
            0x32A => Some(mhpmevent10),
            0x32B => Some(mhpmevent11),
            0x32C => Some(mhpmevent12),
            0x32D => Some(mhpmevent13),
            0x32E => Some(mhpmevent14),
            0x32F => Some(mhpmevent15),
            0x330 => Some(mhpmevent16),
            0x331 => Some(mhpmevent17),
            0x332 => Some(mhpmevent18),
            0x333 => Some(mhpmevent19),
            0x334 => Some(mhpmevent20),
            0x335 => Some(mhpmevent21),
            0x336 => Some(mhpmevent22),
            0x337 => Some(mhpmevent23),
            0x338 => Some(mhpmevent24),
            0x339 => Some(mhpmevent25),
            0x33A => Some(mhpmevent26),
            0x33B => Some(mhpmevent27),
            0x33C => Some(mhpmevent28),
            0x33D => Some(mhpmevent29),
            0x33E => Some(mhpmevent30),
            0x33F => Some(mhpmevent31),

            // Debug/Trace Registers (shared with Debug Mode)
            0x7A0 => Some(tselect),
            0x7A1 => Some(tdata1),
            0x7A2 => Some(tdata2),
            0x7A3 => Some(tdata3),
            0x7A5 => Some(tcontrol),
            0x7A8 => Some(mcontext),

            // Debug Mode Registers
            0x7B0 => Some(dcsr),
            0x7B1 => Some(dpc),
            0x7B2 => Some(dscratch0),
            0x7B3 => Some(dscratch1),
            _ => None,
        }
    }
}

/// Representation of a value in a CSR
pub use values::CSRRepr;

/// Return type of read/write operations
pub type Result<R> = core::result::Result<R, Exception>;

/// Checks that `mode` can access the register `reg`.
///
/// Throws [`Exception::IllegalInstruction`] in case of insufficient privilege.
/// Section 2.1 - privileged spec
#[inline(always)]
fn check_privilege(reg: CSRegister) -> Result<()> {
    if Privilege::Unprivileged < reg.privilege() {
        return Err(Exception::IllegalInstruction);
    }

    Ok(())
}

/// Checks that `reg` is write-able.
///
/// Throws [`Exception::IllegalInstruction`] in case of wrong access rights.
/// Section 2.1 - privileged spec
#[inline(always)]
pub fn check_write(reg: CSRegister) -> Result<()> {
    if reg.is_read_only() {
        return Err(Exception::IllegalInstruction);
    }

    Ok(())
}

/// Check if access to SATP is valid, conforming to TVM flag.
///
/// See section 3.1.6.5
fn check_satp_access(csr: CSRegister, tvm_field: bool) -> Result<()> {
    if tvm_field && csr == CSRegister::satp {
        return Err(Exception::IllegalInstruction);
    }

    Ok(())
}

fn check_fs_access(csr: CSRegister, fs_field: ExtensionValue) -> Result<()> {
    use CSRegister::*;

    if matches!(csr, fcsr | frm | fflags) && fs_field == ExtensionValue::Off {
        Err(Exception::IllegalInstruction)
    } else {
        Ok(())
    }
}

/// Perform general checks on all read or write operations on a CSR.
///
/// Examples of checks: Privilege checks, SATP trapping
pub fn access_checks(csr: CSRegister, hart_state: &HartState<impl ManagerRead>) -> Result<()> {
    check_privilege(csr)?;
    let mstatus = hart_state.csregisters.mstatus();
    let tvm = mstatus.tvm.read();
    check_satp_access(csr, tvm)?;
    let fs = mstatus.fs.read();
    check_fs_access(csr, fs)
}

/// CSRs
pub struct CSRegisters<M: backend::ManagerBase> {
    registers: CSRValues<M>,
}

impl<M: backend::ManagerBase> CSRegisters<M> {
    #[inline(always)]
    pub const fn mstatus(&self) -> &MStatusValue<M> {
        &self.registers.mstatus
    }

    #[inline(always)]
    pub fn mstatus_mut(&mut self) -> &mut MStatusValue<M> {
        &mut self.registers.mstatus
    }

    /// Transform the write operation to account for shadow registers.
    /// (e.g. `sstatus` register)
    ///
    /// Sections 3.1.6 & 4.1.1
    #[inline(always)]
    fn transform_write(&self, reg: CSRegister, value: CSRRepr) -> CSRRepr
    where
        M: backend::ManagerRead,
    {
        // the update of a shadow register follows the steps:
        // 1. keep the shadowed fields from [value]
        // 2. all the other, non-shadowed fields are the underlying register
        //    masked with the inverse of the shadowed fields mask
        // Note: This works because currently there are no shadowed WLRL registers
        match reg {
            CSRegister::sstatus => {
                let mstatus = self.registers.mstatus.read();
                let mstatus = MStatus::from_bits(mstatus);
                let sstatus = SStatus::from_bits(value);
                let mstatus = sstatus.to_mstatus(mstatus);
                mstatus.to_bits()
            }
            CSRegister::sip => {
                let mip = self.registers.mip.read();
                let sip_only = value & CSRegister::WARL_MASK_SIP_SIE;
                let mip_only = mip & !CSRegister::WARL_MASK_SIP_SIE;
                sip_only | mip_only
            }
            CSRegister::sie => {
                let mie = self.registers.mie.read();
                let sie_only = value & CSRegister::WARL_MASK_SIP_SIE;
                let mie_only = mie & !CSRegister::WARL_MASK_SIP_SIE;
                sie_only | mie_only
            }
            CSRegister::fcsr => value & CSRegister::FCSR_MASK,
            CSRegister::frm => {
                let fcsr = self.registers.fcsr.read();
                let fcsr = fcsr & !CSRegister::FRM_MASK;
                ((value << CSRegister::FRM_SHIFT) & CSRegister::FRM_MASK) | fcsr
            }
            CSRegister::fflags => {
                let fcsr = self.registers.fcsr.read();
                let fcsr = fcsr & !CSRegister::FFLAGS_MASK;
                (value & CSRegister::FFLAGS_MASK) | fcsr
            }
            _ => value,
        }
    }

    /// Transform a read operation to account for shadow registers.
    /// (e.g. `sstatus`, `sie` register)
    ///
    /// `source_reg_value` holds the value of the register which is the ground truth for `reg`
    /// if known, `None` otherwise.
    ///
    /// e.g.: `mstatus` is read only if `sstatus` is requested and `mstatus` is not known already
    ///
    /// Sections 3.1.6 & 4.1.1
    #[inline(always)]
    fn transform_read(&self, reg: CSRegister, source_reg_value: Option<CSRRepr>) -> CSRRepr
    where
        M: backend::ManagerRead,
    {
        let source_reg_value = source_reg_value.unwrap_or_else(|| {
            // If reg is a shadow, obtain the underlying ground truth for that register
            self.general_raw_read(reg.into())
        });

        // modify the value according to the shadowing rules of each register
        match reg {
            CSRegister::sstatus => MStatus::from_bits(source_reg_value).to_sstatus().to_bits(),
            CSRegister::sip => source_reg_value & CSRegister::WARL_MASK_SIP_SIE,
            CSRegister::sie => source_reg_value & CSRegister::WARL_MASK_SIP_SIE,
            CSRegister::fcsr => source_reg_value & CSRegister::FCSR_MASK,
            CSRegister::frm => (source_reg_value & CSRegister::FRM_MASK) >> CSRegister::FRM_SHIFT,
            CSRegister::fflags => source_reg_value & CSRegister::FFLAGS_MASK,
            _ => source_reg_value,
        }
    }

    /// Write to a CSR.
    #[inline(always)]
    pub fn write<V: Bits64>(&mut self, reg: CSRegister, value: V)
    where
        M: backend::ManagerReadWrite,
    {
        if let Some(value) = reg.make_value_writable(value.to_bits()) {
            let value = self.transform_write(reg, value);
            let source_reg: RootCSRegister = reg.into();
            self.general_raw_write(source_reg, value);
        }
    }

    /// Read from a CSR.
    #[inline(always)]
    pub fn read<V: Bits64>(&self, reg: CSRegister) -> V
    where
        M: backend::ManagerRead,
    {
        // sstatus is just a restricted view of mstatus.
        // to maintain consistency, when reading sstatus
        // just return mstatus with only the sstatus fields, making the other fields 0
        V::from_bits(self.transform_read(reg, None))
    }

    /// Replace the CSR value, returning the previous value.
    #[inline(always)]
    pub fn replace<V: Bits64>(&mut self, reg: CSRegister, value: V) -> V
    where
        M: backend::ManagerReadWrite,
    {
        if let Some(value) = reg.make_value_writable(value.to_bits()) {
            let value = self.transform_write(reg, value);
            let source_reg: RootCSRegister = reg.into();
            let old_value = self.general_raw_replace(source_reg, value);
            let old_value = self.transform_read(reg, Some(old_value));
            V::from_bits(old_value)
        } else {
            self.read(reg)
        }
    }

    /// Set bits in the CSR.
    #[inline(always)]
    pub fn set_bits(&mut self, reg: CSRegister, bits: CSRRepr) -> CSRValue
    where
        M: backend::ManagerReadWrite,
    {
        let old_value: CSRValue = self.read(reg);
        let new_value = old_value.repr() | bits;
        self.write(reg, new_value);
        old_value
    }

    /// Clear bits in the CSR.
    #[inline(always)]
    pub fn clear_bits(&mut self, reg: CSRegister, bits: CSRRepr) -> CSRValue
    where
        M: backend::ManagerReadWrite,
    {
        let old_value: CSRValue = self.read(reg);
        let new_value = old_value.repr() & !bits;
        self.write(reg, new_value);
        old_value
    }

    /// Get a mask of possible interrupts when in `current_mode`.
    pub fn possible_interrupts(&self, current_mode: Mode) -> CSRRepr
    where
        M: backend::ManagerRead,
    {
        // 3.1.6.1 Privilege and Global Interrupt-Enable Stack in mstatus register
        // "When a hart is executing in privilege mode x, interrupts are globally enabled when
        // xIE=1 and globally disabled when xIE=0.
        // Interrupts for lower-privilege modes, w<x, are always globally
        // disabled regardless of the setting of any global wIE bit
        // for the lower-privilege mode. Interrupts for
        // higher-privilege modes, y>x, are always globally enabled
        // regardless of the setting of the global yIE
        // bit for the higher-privilege mode."
        match current_mode {
            Mode::User => Interrupt::SUPERVISOR_BIT_MASK | Interrupt::MACHINE_BIT_MASK,
            Mode::Supervisor => {
                let ie_supervisor = match self.mstatus().sie.read() {
                    true => self.read(CSRegister::sie),
                    false => 0,
                };

                ie_supervisor | Interrupt::MACHINE_BIT_MASK
            }
            Mode::Machine => match self.mstatus().mie.read() {
                true => self.read(CSRegister::mie),
                false => 0,
            },
        }
    }

    /// Determine the mode where this trap would go to.
    pub fn get_trap_mode<TC: TrapContext>(&self, trap_source: &TC) -> TrapMode
    where
        M: backend::ManagerRead,
    {
        // Section 3.1.8: Machine Trap Delegation Registers (medeleg and mideleg)
        //
        // "By default, all traps at any privilege level are handled in machine mode"
        // "To increase performance, implementations can provide individual read/write bits within
        // medeleg and mideleg to indicate that certain exceptions and interrupts should be
        // processed directly by a lower privilege level."
        //
        // "medeleg has a bit position allocated for every synchronous exception
        // shown in Table 3.6, with the index of the bit position equal to the value
        // returned in the mcause register (i.e., setting bit 8 allows user-mode environment calls
        // to be delegated to a lower-privilege trap handler)."
        //
        // Traps never transition from a more-privileged mode to a
        // less-privileged mode. For example, if M-mode has delegated illegal instruction
        // exceptions to S-mode, and M-mode software later executes
        // an illegal instruction, the trap is taken in M-mode,
        // rather than being delegated to S-mode.

        // Section 3.1.9: An interrupt i will trap to M-mode
        // (causing the privilege mode to change to M-mode)
        // if all of the following are true:
        // (a) either the current privilege mode is M and the MIE bit in the mstatus
        //     register is set, or the current privilege mode has less privilege than M-mode;
        // (b) bit i is set in both mip and mie; and
        // (c) if register mideleg exists, bit i is not set in mideleg.

        // An interrupt i will trap to S-mode if both of the following are true:
        // (a) either the current privilege mode is S and
        //     the SIE bit in the sstatus register is set,
        //     or the current privilege mode has less privilege than S-mode; and
        // (b) bit i is set in both sip and sie.

        // The (b) check that the trap can be taken by looking at mip&mie / sip&sie
        // is already done by get_pending_interrupt()
        // only checking if delegation takes place is left.

        let deleg = match TC::kind() {
            TrapKind::Interrupt => CSRegister::mideleg,
            TrapKind::Exception => CSRegister::medeleg,
        };
        let deleg_val: CSRRepr = self.read(deleg);

        match u64::bit(deleg_val, trap_source.exception_code() as usize) {
            true => TrapMode::Supervisor,
            false => TrapMode::Machine,
        }
    }

    /// Retrieve the address of the trap handler.
    pub fn get_trap_handler<TC: TrapContext>(
        &self,
        trap_source: &TC,
        trap_mode: TrapMode,
    ) -> Address
    where
        M: backend::ManagerRead,
    {
        let xtvec = self.read(match trap_mode {
            TrapMode::Supervisor => CSRegister::stvec,
            TrapMode::Machine => CSRegister::mtvec,
        });
        trap_source.trap_handler_address(xtvec)
    }
}

/// Layout for [CSRegisters]
pub type CSRegistersLayout = CSRValuesLayout;

impl<M: backend::ManagerBase> CSRegisters<M> {
    /// Bind the CSR state to the allocated space.
    pub fn bind(space: backend::AllocatedOf<CSRegistersLayout, M>) -> Self {
        Self {
            registers: values::CSRValues::bind(space),
        }
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub fn struct_ref<'a, F: backend::FnManager<backend::Ref<'a, M>>>(
        &'a self,
    ) -> backend::AllocatedOf<CSRegistersLayout, F::Output> {
        self.registers.struct_ref::<F>()
    }

    /// Reset the control and state registers.
    pub fn reset(&mut self)
    where
        M: backend::ManagerWrite,
    {
        // Try to reset known CSRs to known default values.
        for reg in CSRegister::iter() {
            self.general_raw_write(reg.into(), reg.default_value());
        }
    }

    /// Check whether floating point extension is disabled.
    pub fn floating_disabled(&self) -> bool
    where
        M: backend::ManagerRead,
    {
        let fs = self.mstatus().fs.read();
        fs == ExtensionValue::Off
    }
}

impl<M: backend::ManagerBase> NewState<M> for CSRegisters<M> {
    fn new(manager: &mut M) -> Self
    where
        M: backend::ManagerAlloc,
    {
        Self {
            registers: CSRValues::new(manager),
        }
    }
}

impl<M: backend::ManagerClone> Clone for CSRegisters<M> {
    fn clone(&self) -> Self {
        Self {
            registers: self.registers.clone(),
        }
    }
}

#[cfg(test)]
#[expect(
    clippy::identity_op,
    reason = "Used for some bit-wise operations to make bit patterns more legible"
)]
mod tests {
    use strum::IntoEnumIterator;

    use crate::backend_test;
    use crate::bits::Bits64;
    use crate::machine_state::csregisters::CSRRepr;
    use crate::machine_state::csregisters::CSRegister;
    use crate::machine_state::csregisters::CSRegisters;
    use crate::machine_state::csregisters::values::CSRValue;
    use crate::machine_state::csregisters::xstatus::MStatus;
    use crate::state::NewState;
    use crate::traps::Interrupt;
    use crate::traps::TrapContext;

    #[test]
    fn test_read_write_access() {
        use crate::machine_state::csregisters::CSRegister as csreg;
        use crate::machine_state::csregisters::Exception;
        use crate::machine_state::csregisters::check_write as check;

        let is_illegal_instr = |e| -> bool { e == Exception::IllegalInstruction };

        // Machine registers
        assert!(check(csreg::mcause).is_ok());
        assert!(check(csreg::mhartid).is_err_and(is_illegal_instr));

        // Supervisor registers
        assert!(check(csreg::stvec).is_ok());

        // Hypervisor registers
        assert!(check(csreg::henvcfg).is_ok());
        assert!(check(csreg::hgeip).is_err_and(is_illegal_instr));

        // User registers
        assert!(check(csreg::fcsr).is_ok());
        assert!(check(csreg::instret).is_err_and(is_illegal_instr));
        assert!(check(csreg::cycle).is_err_and(is_illegal_instr));
    }

    #[test]
    fn test_wpri() {
        use crate::machine_state::csregisters::CSRegister as csreg;

        let check = |reg: csreg, value| reg.clear_wpri_fields(value);

        // Machine registers
        assert!(check(csreg::menvcfg, 0) == 0);
        assert!(check(csreg::mstatus, 0xFFFF_FFFF_FFFF_FFFF) == 0x8000_003F_007F_FFEA);

        // Supervisor registers
        assert!(check(csreg::senvcfg, 0b1010_0101_1010_0101) == 0b0000_0000_1010_0001);
        assert!(
            check(csreg::sstatus, 0b1100_0011_0101_1010_0110_1001)
                == 0b0000_0001_0100_0010_0110_0000
        );
    }

    #[test]
    fn test_wlrl() {
        use crate::machine_state::csregisters::CSRegister as csreg;

        // Additionally check if value remains legal after using `make_value_writable`
        let check =
            |reg: csreg, value| reg.is_legal(value) && reg.make_value_writable(value).is_some();

        // Registers that are not xcause should always be ok
        assert!(check(csreg::mstatus, 0xFFFF_FFFF_FFFF_FFFF));
        assert!(check(csreg::sstatus, 0x0));
        assert!(check(csreg::time, 0x0));

        // scause & mcause tests
        assert!(check(csreg::mcause, 0x8000_0000_0000_0003));
        assert!(!check(csreg::mcause, 0x8000_0000_0000_0008));
        assert!(check(csreg::mcause, 0x8000_0000_0000_000B));
        assert!(check(csreg::mcause, 0x0002));
        assert!(check(csreg::mcause, 0x000F));
        assert!(!check(csreg::mcause, 0x000A));
        assert!(!check(csreg::mcause, 0x0000_FFF0_00F0_0002));

        assert!(check(csreg::scause, 0x0000));
        assert!(check(csreg::scause, 0x8000_0000_0000_0001));
        assert!(!check(csreg::scause, 0x8000_F0F0_0000_0003));
        assert!(!check(csreg::scause, 0x8000_0000_0000_000B));
        assert!(!check(csreg::scause, 0x0000_0F00_0000_F0F0));
        assert!(!check(csreg::scause, 0x000A));
    }

    #[test]
    fn test_writable_warl() {
        use crate::machine_state::csregisters::CSRegister as csreg;

        let check_wrapped = |reg: csreg, value| reg.make_value_writable(value);
        let check = |reg: csreg, value| reg.make_value_writable(value).unwrap();

        // misa field
        assert_eq!(
            check(csreg::misa, 0xFFFF_FFFF_FFFF_FFFF),
            0x8000_0000_0014_112D
        );
        assert_eq!(check(csreg::misa, 0x0), 0x8000_0000_0014_112D);

        // medeleg / mideleg
        assert!(check(csreg::medeleg, 0x0) == 0x0);
        assert!(check(csreg::medeleg, 0x0000_FFFF_0000_FFFF) == 0x0000_0000_0000_B3FF);
        assert!(check(csreg::mideleg, 0x0) == 0x0);
        assert!(check(csreg::mideleg, 0xFFFF_0000_FFFF_FFFF) == 0x0000_0000_0000_0AAA);

        // mtvec / stvec field
        assert!(check(csreg::mtvec, 0x0) == 0x0);
        assert!(check(csreg::mtvec, 0xFFFF_FFFF_FFFF_FFFF) == 0xFFFF_FFFF_FFFF_FFFD);
        assert!(check(csreg::stvec, 0x0) == 0x0);
        assert!(check(csreg::stvec, 0xFFFF_FFFF_FFFF_FFFF) == 0xFFFF_FFFF_FFFF_FFFD);

        // mip / mie
        assert!(check(csreg::mip, 0x0) == 0x0);
        assert!(check(csreg::mip, 0xFFFF_FFFF_FFFF_FFFF) == 0x0000_0000_0000_0AAA);
        assert!(check(csreg::mie, 0x0) == 0x0);
        assert!(check(csreg::mie, 0xFFFF_FFFF_FFFF_FFFF) == 0x0000_0000_0000_0AAA);

        // sip / sie
        assert!(check(csreg::sip, 0x0) == 0x0);
        assert!(check(csreg::sip, 0xFFFF_FFFF_FFFF_FFFF) == 0x0000_0000_0000_0222);
        assert!(check(csreg::sie, 0x0) == 0x0);
        assert!(check(csreg::sie, 0xFFFF_FFFF_FFFF_FFFF) == 0x0000_0000_0000_0222);

        // mepc / sepc / mnepc
        assert!(check(csreg::mepc, 0x0) == 0x0);
        assert!(check(csreg::mepc, 0xFFFF_FFFF_FFFF_FFFF) == 0xFFFF_FFFF_FFFF_FFFE);
        assert!(check(csreg::sepc, 0x0) == 0x0);
        assert!(check(csreg::sepc, 0xFFFF_FFFF_FFFF_FFFF) == 0xFFFF_FFFF_FFFF_FFFE);
        assert!(check(csreg::mnepc, 0x0) == 0x0);
        assert!(check(csreg::mnepc, 0xFFFF_FFFF_FFFF_FFFF) == 0xFFFF_FFFF_FFFF_FFFE);

        // satp
        assert_eq!(check_wrapped(csreg::satp, 0x0), Some(0x0));
        assert_eq!(check_wrapped(csreg::satp, 0x0000_FFFF_0000_FFFF), Some(0x0));
        // Invalid mode is read as 0
        assert_eq!(check_wrapped(csreg::satp, 0x4200_FFFF_FFFF_FFFF), Some(0x0));
        assert_eq!(
            check_wrapped(csreg::satp, 0x90F0_0000_FFFF_0000),
            Some(0x90F0_0000_FFFF_0000)
        );

        // mstatus
        // uxl & sxl fields are set
        assert_eq!(check(csreg::mstatus, 0x0), 0x0000_000A_0000_0000);
        // besides uxl & sxl changing, wpri fields get set to 0
        assert_eq!(
            check(csreg::mstatus, !0u64),
            0b1000000000000000000000000011101000000000011111111111100111101010u64
        );
        // check SD bit set from XS=00, FS=10, VS=00, and MPP gets changed to 0b00 from 0b01,
        // and FS gets changed to 11 (dirty)
        assert_eq!(
            check(
                csreg::mstatus,
                0b111111111111000000000000000000000000000000000101011111111111u64
            ),
            0b1000000000000000000000000000101000000000000000000110000111101010u64
        );

        // sstatus
        // uxl & sxl fields are set
        assert_eq!(check(csreg::sstatus, 0x0), 0x0000_0002_0000_0000);
        // besides uxl changing, wpri fields get set to 0
        assert_eq!(
            check(csreg::sstatus, !0u64),
            0b1000000000000000000000000000001000000000000011011110000101100010u64,
        );
        // check sd bit set from XS=00, FS=10, VS=11
        assert_eq!(
            check(
                csreg::sstatus,
                0b111111111111000000000000000000000000000000000101011111111111u64
            ),
            0b1000000000000000000000000000001000000000000000000110000101100010u64,
        );

        // mnstatus
        // check NMIE bit is set
        assert_eq!(check(csreg::mnstatus, 0x0), 0x0000_0000_0000_0008);
        // check WPRI fields & MNPV read-only 0
        assert_eq!(
            check(csreg::mnstatus, 0xFFFF_FFFF_FFFF_FFFF),
            0x0000_0000_0000_1808
        );

        // non warl register
        assert!(check(csreg::instret, 0x42) == 0x42);
    }

    /// Ensure that parsing CSRs matches the values assigned in the enum.
    #[test]
    fn test_csr_parser_roundtrip() {
        for csr in CSRegister::iter() {
            let value = csr as usize;
            let result = CSRegister::try_parse(value as u32);

            assert_eq!(Some(csr), result, "Expected {csr}, got {result:?}");
        }
    }

    backend_test!(test_write_read, F, {
        let mut csrs = CSRegisters::new(&mut F::manager());

        // write to MBE, SXL, UXL, MPP, MPIE, XS, SPP (through mstatus)
        csrs.write(
            CSRegister::mstatus,
            (1u64 << 37)
                | (0b01 << 34)
                | (0b11 << 32)
                | (0b11 << 15)
                | (0b11 << 11)
                | (1 << 8)
                | (1 << 7),
        );
        // SXL, UXL should be set to MXL (WARL), SD bit should be 1
        let read_mstatus: MStatus = csrs.read(CSRegister::mstatus);
        assert_eq!(
            read_mstatus.to_bits(),
            (1u64 << 63)
                | (1 << 37)
                | (0b10 << 34)
                | (0b10 << 32)
                | (0b11 << 15)
                | (0b11 << 11)
                | (1 << 8)
                | (1 << 7)
        );
        // SXL should be 0 (WPRI), MBE, MPP, MPIE should be 0 (WPRI for sstatus), SD bit also 1
        let read_sstatus: CSRValue = csrs.read(CSRegister::sstatus);
        assert_eq!(
            read_sstatus.repr(),
            (1u64 << 63) | (0b10 << 32) | (0b11 << 15) | (1 << 8)
        );

        // write to MBE, SXL, UXL, MPP, MPIE, VS, SPP, (through sstatus, M-fields should be ignored, being WPRI)
        csrs.write(
            CSRegister::sstatus,
            (0u64 << 37)
                | (0b11 << 34)
                | (0b01 << 32)
                | (0b11 << 11)
                | (0 << 9)
                | (0 << 7)
                | (1 << 8),
        );
        // setting VS to 0, SD bit becomes 0. Otherwise, only UXL and SPP fields are non-zero.
        let second_read_sstatus: CSRValue = csrs.read(CSRegister::sstatus);
        assert_eq!(second_read_sstatus.repr(), (0b10u64 << 32) | (1 << 8));
        // MBE remained 1, SXL, UXL are constant, MPP remained 0b11, VS is 0 due to the sstatus change, SPP and MPIE remained 1,
        let read_mstatus: CSRValue = csrs.read(CSRegister::mstatus);
        assert_eq!(
            read_mstatus.repr(),
            (1u64 << 37)
                | (0b10 << 34)
                | (0b10 << 32)
                | (0b11 << 11)
                | (0 << 9)
                | (1 << 8)
                | (1 << 7)
        );

        assert_eq!(
            csrs.read::<CSRRepr>(CSRegister::sstatus),
            (0b10 << 32) | (1 << 8)
        );
        assert_eq!(
            csrs.read::<CSRRepr>(CSRegister::mstatus),
            (1u64 << 37)
                | (0b10 << 34)
                | (0b10 << 32)
                | (0b11 << 11)
                | (0 << 9)
                | (1 << 8)
                | (1 << 7)
        );

        // write to MBE, SXL, UXL, MPP, VS, SPP, MPIE (through sstatus)
        let old_sstatus: CSRValue = csrs.replace(
            CSRegister::sstatus,
            ((1u64 << 37)
                | (0b01 << 34)
                | (0b11 << 32)
                | (0b11 << 15)
                | (0b11 << 11)
                | (0 << 8)
                | (0 << 7))
                .into(),
        );
        assert_eq!(old_sstatus, second_read_sstatus);
        // SXL, UXL should be set to MXL (WARL), SD bit should be 1
        let read_mstatus: CSRValue = csrs.read(CSRegister::mstatus);
        assert_eq!(
            read_mstatus.repr(),
            (1u64 << 63)
                | (1 << 37)
                | (0b10 << 34)
                | (0b10 << 32)
                | (0b11 << 15)
                | (0b11 << 11)
                | (0 << 8)
                | (1 << 7)
        );
        // SXL should be 0 (WPRI), MBE, MPP, MPIE should be 0 (WPRI for sstatus), SD bit also 1
        let read_sstatus: CSRValue = csrs.read(CSRegister::sstatus);
        assert_eq!(
            read_sstatus.repr(),
            (1u64 << 63) | (0b10 << 32) | (0b11 << 15) | (0 << 8)
        );
    });

    backend_test!(test_xip_xie, F, {
        let mut csrs = CSRegisters::new(&mut F::manager());

        let mtip: u64 = 1 << Interrupt::MachineTimer.exception_code();
        let msip: u64 = 1 << Interrupt::MachineSoftware.exception_code();
        let seip: u64 = 1 << Interrupt::SupervisorExternal.exception_code();
        let stip: u64 = 1 << Interrupt::SupervisorTimer.exception_code();

        // Writing to MIP does nothing.
        csrs.write(CSRegister::mip, mtip | seip);
        assert_eq!(csrs.read::<CSRRepr>(CSRegister::mip), 0);
        assert_eq!(csrs.read::<CSRRepr>(CSRegister::sip), 0);

        // MSIP bit should not be written
        csrs.write(CSRegister::sie, stip | seip | msip);
        assert_eq!(csrs.read::<CSRRepr>(CSRegister::mie), stip | seip);
        assert_eq!(csrs.read::<CSRRepr>(CSRegister::sie), stip | seip);
    });

    backend_test!(test_fcsr, F, {
        let mut csrs = CSRegisters::new(&mut F::manager());

        // check starting values
        assert_eq!(0, csrs.read::<CSRRepr>(CSRegister::fcsr));
        assert_eq!(0, csrs.read::<CSRRepr>(CSRegister::frm));
        assert_eq!(0, csrs.read::<CSRRepr>(CSRegister::fflags));

        // writing to fcsr is reflected in frm/fflags
        csrs.write(CSRegister::fcsr, u64::MAX);

        assert_eq!(0xff, csrs.read::<CSRRepr>(CSRegister::fcsr));
        assert_eq!(0b111, csrs.read::<CSRRepr>(CSRegister::frm));
        assert_eq!(0b11111, csrs.read::<CSRRepr>(CSRegister::fflags));

        // writing to frm is reflected in fcsr
        csrs.write(CSRegister::frm, 0b010);

        assert_eq!(0b01011111, csrs.read::<CSRRepr>(CSRegister::fcsr));
        assert_eq!(0b010, csrs.read::<CSRRepr>(CSRegister::frm));
        assert_eq!(0b11111, csrs.read::<CSRRepr>(CSRegister::fflags));

        // writing to fflags is reflected in fcsr
        csrs.write(CSRegister::fflags, 0b01010);

        assert_eq!(0b01001010, csrs.read::<CSRRepr>(CSRegister::fcsr));
        assert_eq!(0b010, csrs.read::<CSRRepr>(CSRegister::frm));
        assert_eq!(0b01010, csrs.read::<CSRRepr>(CSRegister::fflags));
    });
}
