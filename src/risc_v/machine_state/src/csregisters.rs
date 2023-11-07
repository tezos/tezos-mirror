// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::backend::{self, Region};

/// Privilege required to access a CSR
pub enum Privilege {
    Unprivileged,
    Supervisor,
    Hypervisor,
    Machine,
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

impl CSRegister {
    /// Determine the priviledge level required to access this CSR.
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
}

/// Value in a CSR
type CSRValue = u64;

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
        self.registers.write(reg as usize, value)
    }

    /// Read from a CSR.
    #[inline(always)]
    pub fn read(&mut self, reg: CSRegister) -> CSRValue {
        self.registers.read(reg as usize)
    }

    /// Replace the CSR value, returning the previous value.
    #[inline(always)]
    pub fn replace(&mut self, reg: CSRegister, value: CSRValue) -> CSRValue {
        // TODO: https://gitlab.com/tezos/tezos/-/issues/6594
        // Respect field specifications (e.g. WPRI, WLRL, WARL)
        self.registers.replace(reg as usize, value)
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
}

/// Layout for [CSRegisters]
pub type CSRegistersLayout = backend::Array<CSRValue, 4096>;

impl<M: backend::Manager> CSRegisters<M> {
    /// Bind the CSR state to the allocated space.
    pub fn new_in(space: backend::AllocatedOf<CSRegistersLayout, M>) -> Self {
        Self { registers: space }
    }
}
