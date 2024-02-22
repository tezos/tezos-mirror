//! Implementation of Zicsr extension for RISC-V
//!
//! Chapter 9 - Unprivileged spec

use crate::{
    machine_state::{csregisters, hart_state::HartState, registers},
    state_backend as backend,
};

impl<M> HartState<M>
where
    M: backend::Manager,
{
    /// Execute a `CSRRW` instruction.
    #[inline(always)]
    pub fn run_csrrw(
        &mut self,
        csr: csregisters::CSRegister,
        rs1: registers::XRegister,
        rd: registers::XRegister,
    ) -> csregisters::Result<()> {
        let value = self.xregisters.read(rs1);
        self.csr_replace(csr, value, rd)
    }

    /// Execute a `CSRRWI` instruction.
    #[inline(always)]
    pub fn run_csrrwi(
        &mut self,
        csr: csregisters::CSRegister,
        imm: registers::XValue,
        rd: registers::XRegister,
    ) -> csregisters::Result<()> {
        self.csr_replace(csr, imm & 0b11111, rd)
    }

    /// Replace the value in `csr` with `value` and write the previous value to `rd`.
    /// When `rd = x0`, no read side effects are triggered.
    #[inline(always)]
    fn csr_replace(
        &mut self,
        csr: csregisters::CSRegister,
        value: registers::XValue,
        rd: registers::XRegister,
    ) -> csregisters::Result<()> {
        let mode = self.mode.read();
        csregisters::check_privilege(csr, mode)?;
        csregisters::check_write(csr)?;

        // When `rd = x0`, we don't want to trigger any CSR read effects.
        if rd.is_zero() {
            self.csregisters.write(csr, value);
        } else {
            let old = self.csregisters.replace(csr, value);
            self.xregisters.write(rd, old);
        }
        Ok(())
    }

    /// Execute the `CSRRS` instruction.
    #[inline(always)]
    pub fn run_csrrs(
        &mut self,
        csr: csregisters::CSRegister,
        rs1: registers::XRegister,
        rd: registers::XRegister,
    ) -> csregisters::Result<()> {
        let mode = self.mode.read();
        csregisters::check_privilege(csr, mode)?;

        // When `rs1 = x0`, we don't want to trigger any CSR write effects.
        let old = if rs1.is_zero() {
            self.csregisters.read(csr)
        } else {
            csregisters::check_write(csr)?;

            let value = self.xregisters.read(rs1);
            self.csregisters.set_bits(csr, value)
        };

        self.xregisters.write(rd, old);
        Ok(())
    }

    /// Execute the `CSRRSI` instruction.
    #[inline(always)]
    pub fn run_csrrsi(
        &mut self,
        csr: csregisters::CSRegister,
        imm: registers::XValue,
        rd: registers::XRegister,
    ) -> csregisters::Result<()> {
        let imm = imm & 0b11111;
        let mode = self.mode.read();
        csregisters::check_privilege(csr, mode)?;

        // When `imm = 0`, we don't want to trigger any CSR write effects.
        let old = if imm == 0 {
            self.csregisters.read(csr)
        } else {
            csregisters::check_write(csr)?;

            self.csregisters.set_bits(csr, imm)
        };

        self.xregisters.write(rd, old);
        Ok(())
    }

    /// Execute the `CSRRC` instruction.
    #[inline(always)]
    pub fn run_csrrc(
        &mut self,
        csr: csregisters::CSRegister,
        rs1: registers::XRegister,
        rd: registers::XRegister,
    ) -> csregisters::Result<()> {
        let mode = self.mode.read();
        csregisters::check_privilege(csr, mode)?;

        // When `rs1 = x0`, we don't want to trigger any CSR write effects.
        let old = if rs1.is_zero() {
            self.csregisters.read(csr)
        } else {
            csregisters::check_write(csr)?;

            let value = self.xregisters.read(rs1);
            self.csregisters.clear_bits(csr, value)
        };

        self.xregisters.write(rd, old);
        Ok(())
    }

    /// Execute the `CSRRCI` instruction.
    #[inline(always)]
    pub fn run_csrrci(
        &mut self,
        csr: csregisters::CSRegister,
        imm: registers::XValue,
        rd: registers::XRegister,
    ) -> csregisters::Result<()> {
        let imm = imm & 0b11111;
        let mode = self.mode.read();
        csregisters::check_privilege(csr, mode)?;

        // When `imm = 0`, we don't want to trigger any CSR write effects.
        let old = if imm == 0 {
            self.csregisters.read(csr)
        } else {
            csregisters::check_write(csr)?;

            self.csregisters.clear_bits(csr, imm)
        };

        self.xregisters.write(rd, old);
        Ok(())
    }
}
