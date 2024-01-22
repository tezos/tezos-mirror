// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

#![deny(rustdoc::broken_intra_doc_links)]

pub mod backend;
pub mod csregisters;
pub mod memory_backend;
pub mod registers;

/// RISC-V hart state
pub struct HartState<M: backend::Manager> {
    /// Integer registers
    pub xregisters: registers::XRegisters<M>,

    /// Floating-point number registers
    pub fregisters: registers::FRegisters<M>,

    /// Control and state registers
    pub csregisters: csregisters::CSRegisters<M>,
}

impl<M: backend::Manager> HartState<M> {
    /// Execute a CSRRW instruction.
    #[inline(always)]
    pub fn csrrw(
        &mut self,
        csr: csregisters::CSRegister,
        rs1: registers::XRegister,
        rd: registers::XRegister,
    ) {
        let value = self.xregisters.read(rs1);
        self.csr_replace(csr, value, rd);
    }

    /// Execute a CSRRWI instruction.
    #[inline(always)]
    pub fn csrrwi(
        &mut self,
        csr: csregisters::CSRegister,
        imm: registers::XValue,
        rd: registers::XRegister,
    ) {
        self.csr_replace(csr, imm & 0b11111, rd);
    }

    /// Replace the value in `csr` with `value` and write the previous value to `rd`.
    /// When `rd = x0`, no read side effects are triggered.
    #[inline(always)]
    fn csr_replace(
        &mut self,
        csr: csregisters::CSRegister,
        value: registers::XValue,
        rd: registers::XRegister,
    ) {
        // When `rd = x0`, we don't want to trigger any CSR read effects.
        if rd.is_zero() {
            self.csregisters.write(csr, value);
        } else {
            let old = self.csregisters.replace(csr, value);
            self.xregisters.write(rd, old);
        }
    }

    /// Execute the CSRRS instruction.
    #[inline(always)]
    pub fn csrrs(
        &mut self,
        csr: csregisters::CSRegister,
        rs1: registers::XRegister,
        rd: registers::XRegister,
    ) {
        // When `rs1 = x0`, we don't want to trigger any CSR write effects.
        let old = if rs1.is_zero() {
            self.csregisters.read(csr)
        } else {
            let value = self.xregisters.read(rs1);
            self.csregisters.set_bits(csr, value)
        };

        self.xregisters.write(rd, old);
    }

    /// Execute the CSRRSI instruction.
    #[inline(always)]
    pub fn csrrsi(
        &mut self,
        csr: csregisters::CSRegister,
        imm: registers::XValue,
        rd: registers::XRegister,
    ) {
        let imm = imm & 0b11111;

        // When `imm = 0`, we don't want to trigger any CSR write effects.
        let old = if imm == 0 {
            self.csregisters.read(csr)
        } else {
            self.csregisters.set_bits(csr, imm)
        };

        self.xregisters.write(rd, old);
    }

    /// Execute the CSRRC instruction.
    #[inline(always)]
    pub fn csrrc(
        &mut self,
        csr: csregisters::CSRegister,
        rs1: registers::XRegister,
        rd: registers::XRegister,
    ) {
        // When `rs1 = x0`, we don't want to trigger any CSR write effects.
        let old = if rs1.is_zero() {
            self.csregisters.read(csr)
        } else {
            let value = self.xregisters.read(rs1);
            self.csregisters.clear_bits(csr, value)
        };

        self.xregisters.write(rd, old);
    }

    /// Execute the CSRRCI instruction.
    #[inline(always)]
    pub fn csrrci(
        &mut self,
        csr: csregisters::CSRegister,
        imm: registers::XValue,
        rd: registers::XRegister,
    ) {
        let imm = imm & 0b11111;

        // When `imm = 0`, we don't want to trigger any CSR write effects.
        let old = if imm == 0 {
            self.csregisters.read(csr)
        } else {
            self.csregisters.clear_bits(csr, imm)
        };

        self.xregisters.write(rd, old);
    }
}

/// Layout of [HartState]
pub type HartStateLayout = (
    registers::XRegistersLayout,
    registers::FRegistersLayout,
    csregisters::CSRegistersLayout,
);

impl<M: backend::Manager> HartState<M> {
    /// Bind the hart state to the given allocated space.
    pub fn new_in(space: backend::AllocatedOf<HartStateLayout, M>) -> Self {
        Self {
            xregisters: registers::XRegisters::new_in(space.0),
            fregisters: registers::FRegisters::new_in(space.1),
            csregisters: csregisters::CSRegisters::new_in(space.2),
        }
    }
}

#[cfg(test)]
mod tests {}
