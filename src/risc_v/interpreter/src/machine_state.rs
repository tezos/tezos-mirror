// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

#![deny(rustdoc::broken_intra_doc_links)]

pub mod backend;
pub mod bus;
pub mod csregisters;
pub mod memory_backend;
mod mode;
pub mod registers;

#[cfg(test)]
extern crate proptest;

use backend::{Atom, Cell};
use bus::{main_memory, Address, Bus};

/// RISC-V hart state
pub struct HartState<M: backend::Manager> {
    /// Integer registers
    pub xregisters: registers::XRegisters<M>,

    /// Floating-point number registers
    pub fregisters: registers::FRegisters<M>,

    /// Control and state registers
    pub csregisters: csregisters::CSRegisters<M>,

    /// Current running mode of hart
    pub mode: mode::ModeCell<M>,

    /// Program counter
    pub pc: Cell<Address, M>,
}

impl<M: backend::Manager> HartState<M> {
    /// Execute a CSRRW instruction.
    #[inline(always)]
    pub fn csrrw(
        &mut self,
        csr: csregisters::CSRegister,
        rs1: registers::XRegister,
        rd: registers::XRegister,
    ) -> csregisters::Result<()> {
        let value = self.xregisters.read(rs1);
        self.csr_replace(csr, value, rd)
    }

    /// Execute a CSRRWI instruction.
    #[inline(always)]
    pub fn csrrwi(
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

    /// Execute the CSRRS instruction.
    #[inline(always)]
    pub fn csrrs(
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

    /// Execute the CSRRSI instruction.
    #[inline(always)]
    pub fn csrrsi(
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

    /// Execute the CSRRC instruction.
    #[inline(always)]
    pub fn csrrc(
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

    /// Execute the CSRRCI instruction.
    #[inline(always)]
    pub fn csrrci(
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

/// Layout of [HartState]
pub type HartStateLayout = (
    registers::XRegistersLayout,
    registers::FRegistersLayout,
    csregisters::CSRegistersLayout,
    mode::ModeLayout,
    Atom<Address>, // Program counter layout
);

impl<M: backend::Manager> HartState<M> {
    /// Bind the hart state to the given allocated space.
    pub fn bind(space: backend::AllocatedOf<HartStateLayout, M>) -> Self {
        Self {
            xregisters: registers::XRegisters::bind(space.0),
            fregisters: registers::FRegisters::bind(space.1),
            csregisters: csregisters::CSRegisters::bind(space.2),
            mode: mode::ModeCell::bind(space.3),
            pc: Cell::bind(space.4),
        }
    }

    /// Reset the hart state.
    pub fn reset(&mut self, mode: mode::Mode, pc: Address) {
        self.xregisters.reset();
        self.fregisters.reset();
        self.csregisters.reset();
        self.mode.reset(mode);
        self.pc.write(pc);
    }
}

/// Layout for the machine state
pub type MachineStateLayout<ML> = (HartStateLayout, bus::BusLayout<ML>);

/// Machine state
pub struct MachineState<ML: main_memory::MainMemoryLayout, M: backend::Manager> {
    pub hart: HartState<M>,
    pub bus: Bus<ML, M>,
}

impl<ML: main_memory::MainMemoryLayout, M: backend::Manager> MachineState<ML, M> {
    /// Bind the machine state to the given allocated space.
    pub fn bind(space: backend::AllocatedOf<MachineStateLayout<ML>, M>) -> Self {
        Self {
            hart: HartState::bind(space.0),
            bus: Bus::bind(space.1),
        }
    }

    /// Reset the machine state.
    pub fn reset(&mut self, mode: mode::Mode, pc: Address) {
        self.hart.reset(mode, pc);
        self.bus.reset();
    }
}

#[cfg(test)]
mod tests {
    use super::{
        backend::tests::{test_determinism, ManagerFor},
        bus::main_memory::tests::T1K,
        mode, HartState, HartStateLayout, MachineState, MachineStateLayout,
    };
    use crate::backend_test;
    use strum::IntoEnumIterator;

    backend_test!(test_hart_state_reset, F, {
        mode::Mode::iter().for_each(|mode: mode::Mode| {
            proptest::proptest!(|(pc: u64)| {
                test_determinism::<F, HartStateLayout, _>(|space| {
                    let mut hart = HartState::bind(space);
                    hart.reset(mode, pc);
                });
            });
        });
    });

    backend_test!(test_machine_state_reset, F, {
        mode::Mode::iter().for_each(|mode: mode::Mode| {
            proptest::proptest!(|(pc: u64)| {
                test_determinism::<F, MachineStateLayout<T1K>, _>(|space| {
                    let mut hart: MachineState<T1K, ManagerFor<'_, F, MachineStateLayout<T1K>>> =
                        MachineState::bind(space);
                    hart.reset(mode, pc);
                });
            });
        });
    });
}
