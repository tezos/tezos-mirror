// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::backend::{self, Region};
use crate::mode::Mode;

/// Privilege required to access a CSR
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Privilege {
    Unprivileged = 0,
    Supervisor = 1,
    Hypervisor = 2,
    Machine = 3,
}

/// Get the bitmask formed of `n` ones.
const fn ones(n: u64) -> u64 {
    !0 >> (64 - n)
}

/// CSR index
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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
    mcontext = 0x7A8,

    // Debug Mode Registers
    dcsr = 0x7B0,
    dpc = 0x7B1,
    dscratch0 = 0x7B2,
    dscratch1 = 0x7B3,
}

// We want to allow shifts by 0 for clarity and consistency.
#[allow(clippy::identity_op)]
impl CSRegister {
    // Since read-only misa.MXL = 0b10, we have MXLEN = 64 from table 3.1
    const MXLEN: u64 = 64;
    const SXLEN: u64 = CSRegister::MXLEN;

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

    const WPRI_MASK_EMPTY: CSRValue = CSRValue::MAX;

    const WPRI_MASK_MSTATUS: CSRValue =
        !(ones(1) << 0 | ones(1) << 2 | ones(1) << 4 | ones(9) << 23 | ones(25) << 38);

    const WPRI_MASK_MENVCFG: CSRValue = !(ones(3) << 1 | ones(54) << 8);

    const WPRI_MASK_MSECCFG: CSRValue = !(ones(5) << 3 | ones(CSRegister::MXLEN - 10) << 10);

    const WPRI_MASK_SSTATUS: CSRValue = !(ones(1) << 0
        | ones(3) << 2
        | ones(1) << 7
        | ones(2) << 11
        | ones(1) << 17
        | ones(12) << 20
        | ones(29) << 34);

    const WPRI_MASK_SENVCFG: CSRValue = !(ones(3) << 1 | ones(CSRegister::SXLEN - 8) << 8);

    /// Return the mask of non reserved bits, (WPRI bits are 0)
    /// Relevant section 2.3 - privileged spec
    #[inline(always)]
    pub fn wpri_mask(self) -> CSRValue {
        match self {
            CSRegister::mstatus => CSRegister::WPRI_MASK_MSTATUS,
            CSRegister::menvcfg => CSRegister::WPRI_MASK_MENVCFG,
            CSRegister::mseccfg => CSRegister::WPRI_MASK_MSECCFG,
            CSRegister::sstatus => CSRegister::WPRI_MASK_SSTATUS,
            CSRegister::senvcfg => CSRegister::WPRI_MASK_SENVCFG,
            _ => CSRegister::WPRI_MASK_EMPTY,
        }
    }

    /// Ensures that WPRI fields are kept read-only zero.
    ///
    /// Conforming to Section 2.3 - privileged spec
    #[inline(always)]
    pub fn clear_wpri_fields(self, new_value: CSRValue) -> CSRValue {
        new_value & self.wpri_mask()
    }

    /// Possible `mcause` values, table 3.6
    const WLRL_MCAUSE_VALUES: [u64; 20] = {
        const INTERRUPT_BIT: u64 = 1 << 63;
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
    fn is_legal(self, new_value: CSRValue) -> bool {
        let legal_values = self.legal_values();
        // if no legal values are defined, then the register is not WLRL
        legal_values.is_empty() || legal_values.contains(&new_value)
    }

    /// Value for CSR `misa`, see section 3.1.1 & tables 3.1 (MXL) & 3.2 (Extensions)
    const WARL_MISA_VALUE: CSRValue = {
        /* MXLEN encoding of 64 bits */
        const MXL_ENCODING: u64 = 0b10 << 62;
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
        MXL_ENCODING |
        /* Extensions */
        ATOMIC_EXT |
        COMPRESSED_EXT |
        DOUBLE_EXT |
        SINGLE_EXT |
        SINGLE_EXT |
        RV64I_ISA_EXT |
        MULT_DIV_EXT |
        SUPERVISOR_EXT |
        USER_EXT
    };

    /// Ensures WARL registers / fields are respected
    ///
    /// Section 2.3 - privileged spec
    #[inline(always)]
    pub fn transform_warl_fields(self, new_value: CSRValue) -> CSRValue {
        match self {
            CSRegister::misa => CSRegister::WARL_MISA_VALUE,
            CSRegister::medeleg => new_value & CSRegister::WARL_MASK_MEDELEG,
            CSRegister::mideleg => new_value & CSRegister::WARL_MASK_MIDELEG,
            CSRegister::mtvec | CSRegister::stvec => new_value & CSRegister::WARL_MASK_XTVEC,
            CSRegister::mip | CSRegister::mie => new_value & CSRegister::WARL_MASK_MIP_MIE,
            CSRegister::sip | CSRegister::sie => new_value & CSRegister::WARL_MASK_SIP_SIE,
            CSRegister::mepc | CSRegister::sepc => new_value & CSRegister::WARL_MASK_XEPC,
            _ => new_value,
        }
    }

    /// See section 3.1.8 and table 3.6
    ///
    /// Exception codes to delegate.
    /// If an exception can't be thrown from a lower privilege mode, set it here read-only 0
    const WARL_MASK_MEDELEG: CSRValue = !(
        ones(1) << 10 // reserved
        | ones(1) << 11 // environment call from M-mode
        | ones(1) << 14 // reserved
        | ones(CSRegister::MXLEN - 16) << 16
        // reserved & custom use
    );

    /// See section 3.1.8 and table 3.6
    ///
    /// Interrupt codes to delegate.
    /// If an interrupt can't be thrown from a lower privilege mode, set it here read-only 0
    const WARL_MASK_MIDELEG: CSRValue = !(
        ones(1) << 0    // reserved
        | ones(1) << 2  // reserved
        | ones(1) << 4  // reserved
        | ones(1) << 6  // reserved
        | ones(1) << 8  // reserved
        | ones(1) << 10 // reserved
        | ones(4) << 12 // reserved
        | ones(CSRegister::MXLEN - 16) << 16
        // custom use
    );

    /// `mtvec.MODE = mtvec[1:0]`.
    /// Only `0` and `1` values are allowed for `MODE`, so we treat `MODE[1]` as read-only 0
    ///
    /// `mtvec.BASE = mtvec[MXLEN-1:2] << 2` (since it has to be 4-byte aligned).
    /// The same applies for stvec. Sections 3.1.7 & 4.1.2
    const WARL_MASK_XTVEC: CSRValue = !(ones(1) << 1);

    // List of possible standard interrupt bits
    /// Supervisor software interrupt
    const SSIP_BIT: CSRValue = 1 << 1;
    /// Machine software interrupt
    const MSIP_BIT: CSRValue = 1 << 3;
    /// Supervisor timer interrupt
    const STIP_BIT: CSRValue = 1 << 5;
    /// Machine timer interrupt
    const MTIP_BIT: CSRValue = 1 << 7;
    /// Supervisor external interrupt
    const SEIP_BIT: CSRValue = 1 << 9;
    /// Machine external interrupt
    const MEIP_BIT: CSRValue = 1 << 11;

    /// WARL mask for mip/mie interrupt bits.
    ///
    /// 0-15 are for standard interrupts. The rest are for custom used and are treated as reserved
    const WARL_MASK_MIP_MIE: CSRValue = (CSRegister::SSIP_BIT
        | CSRegister::MSIP_BIT
        | CSRegister::STIP_BIT
        | CSRegister::MTIP_BIT
        | CSRegister::SEIP_BIT
        | CSRegister::MEIP_BIT);

    /// WARL mask for sip/sie interrupt bits.
    ///
    /// 0-15 are for standard interrupts. The rest are for custom used and are treated as reserved
    const WARL_MASK_SIP_SIE: CSRValue =
        (CSRegister::SSIP_BIT | CSRegister::STIP_BIT | CSRegister::SEIP_BIT);

    /// WARL mask for mepc/sepc addresses.
    ///
    /// Since extension C is supported, we only make the low bit read-only 0
    const WARL_MASK_XEPC: CSRValue = !1;
}

/// Value in a CSR
type CSRValue = u64;

/// RISC-V exceptions
#[derive(PartialEq, Eq)]
pub enum Exception {
    IllegalInstruction,
}

/// Return type of read/write operations
pub type Result<R> = core::result::Result<R, Exception>;

/// Checks that `mode` can access the register `reg`.
///
/// Throws [`Exception::IllegalInstruction`] in case of insufficient privilege.
/// Section 2.1 - privileged spec
#[inline(always)]
pub fn check_privilege(reg: CSRegister, mode: Mode) -> Result<()> {
    if mode.privilege() < reg.privilege() {
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

/// CSRs
pub struct CSRegisters<M: backend::Manager> {
    registers: M::Region<CSRValue, 4096>,
}

impl<M: backend::Manager> CSRegisters<M> {
    /// Write to a CSR.
    #[inline(always)]
    pub fn write(&mut self, reg: CSRegister, value: CSRValue) {
        // TODO: https://gitlab.com/tezos/tezos/-/issues/6594
        // Respect field specifications (e.g. WPRI, WLRL, WARL)

        if let Some(value) = self.make_value_writable(reg, value) {
            self.registers.write(reg as usize, value);
        }
    }

    /// Read from a CSR.
    #[inline(always)]
    pub fn read(&mut self, reg: CSRegister) -> CSRValue {
        // TODO: https://gitlab.com/tezos/tezos/-/issues/6594
        // Respect field specifications (e.g. WPRI, WLRL, WARL)
        self.registers.read(reg as usize)
    }

    /// Replace the CSR value, returning the previous value.
    #[inline(always)]
    pub fn replace(&mut self, reg: CSRegister, value: CSRValue) -> CSRValue {
        // TODO: https://gitlab.com/tezos/tezos/-/issues/6594
        // Respect field specifications (e.g. WPRI, WLRL, WARL)

        if let Some(value) = self.make_value_writable(reg, value) {
            self.registers.replace(reg as usize, value)
        } else {
            self.registers.read(reg as usize)
        }
    }

    /// Set bits in the CSR.
    #[inline(always)]
    pub fn set_bits(&mut self, reg: CSRegister, bits: CSRValue) -> CSRValue {
        // TODO: https://gitlab.com/tezos/tezos/-/issues/6594
        // Respect field specifications (e.g. WPRI, WLRL, WARL)
        let old_value = self.read(reg);
        let new_value = old_value | bits;
        self.write(reg, new_value);
        old_value
    }

    /// Clear bits in the CSR.
    #[inline(always)]
    pub fn clear_bits(&mut self, reg: CSRegister, bits: CSRValue) -> CSRValue {
        // TODO: https://gitlab.com/tezos/tezos/-/issues/6594
        // Respect field specifications (e.g. WPRI, WLRL, WARL)
        let old_value = self.read(reg);
        let new_value = old_value & !bits;
        self.write(reg, new_value);
        old_value
    }

    /// Enforce the WPRI and WLRL field specifications.
    ///
    /// Either return the value to be written, or None to signify that no write is necessary,
    /// to leave existing value in its place.
    #[inline(always)]
    fn make_value_writable(&self, reg: CSRegister, value: CSRValue) -> Option<CSRValue> {
        // respect the reserved WPRI fields, setting them 0
        let value = reg.clear_wpri_fields(value);
        // apply WARL rules
        let value = reg.transform_warl_fields(value);
        // check if value is legal w.r.t. WLRL fields
        reg.is_legal(value).then_some(value)
    }
}

/// Layout for [CSRegisters]
pub type CSRegistersLayout = backend::Array<CSRValue, 4096>;

impl<M: backend::Manager> CSRegisters<M> {
    /// Bind the CSR state to the allocated space.
    pub fn new_in(space: backend::AllocatedOf<CSRegistersLayout, M>) -> Self {
        Self { registers: space }
    }
}

#[cfg(test)]
pub mod tests {
    use crate::{
        csregisters::{CSRValue, Exception},
        mode::Mode,
    };

    #[test]
    pub fn test_privilege_access() {
        use crate::csregisters::check_privilege as check;
        use crate::csregisters::CSRegister as csreg;

        let is_illegal_instr = |e| -> bool { e == Exception::IllegalInstruction };

        // Access Machine registers
        assert!(check(csreg::mie, Mode::Debug).is_ok());
        assert!(check(csreg::mstatus, Mode::Machine).is_ok());
        assert!(check(csreg::medeleg, Mode::Supervisor).is_err_and(is_illegal_instr));
        assert!(check(csreg::mcause, Mode::User).is_err_and(is_illegal_instr));

        // Access Supervisor registers
        assert!(check(csreg::sstatus, Mode::Debug).is_ok());
        assert!(check(csreg::sip, Mode::Machine).is_ok());
        assert!(check(csreg::scontext, Mode::Supervisor).is_ok());
        assert!(check(csreg::stval, Mode::User).is_err_and(is_illegal_instr));

        // Access User registers
        assert!(check(csreg::fflags, Mode::Debug).is_ok());
        assert!(check(csreg::cycle, Mode::Machine).is_ok());
        assert!(check(csreg::frm, Mode::Supervisor).is_ok());
        assert!(check(csreg::fcsr, Mode::User).is_ok());
    }

    #[test]
    fn test_read_write_access() {
        use crate::csregisters::check_write as check;
        use crate::csregisters::{CSRegister as csreg, Exception};

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
        use crate::csregisters::CSRegister as csreg;

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
        use crate::csregisters::CSRegister as csreg;

        let check = |reg: csreg, value| reg.is_legal(value);

        // Registers that are not xcause should always be ok
        const ALL_BITS: CSRValue = 0xFFFF_FFFF_FFFF_FFFF;
        assert!(check(csreg::mstatus, ALL_BITS));
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
    fn test_warl() {
        use crate::csregisters::CSRegister as csreg;

        let check = |reg: csreg, value| reg.transform_warl_fields(value);

        // misa field
        assert!(check(csreg::misa, 0xFFFF_FFFF_FFFF_FFFF) == 0x8000_0000_0014_112D);
        assert!(check(csreg::misa, 0x0) == 0x8000_0000_0014_112D);

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

        // non warl register
        assert!(check(csreg::instret, 0x42) == 0x42);
    }
}
