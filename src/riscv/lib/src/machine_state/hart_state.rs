// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pub(super) mod interrupts_cache;

use super::csregisters::xstatus::MStatus;
use crate::{
    bits::u64,
    machine_state::{
        bus::Address,
        csregisters::{self, xstatus, CSRRepr, CSRegister},
        mode::{self, Mode, TrapMode},
        registers,
        reservation_set::{self, ReservationSet},
    },
    state_backend::{self as backend, Atom, Cell, CellWrite},
    traps::{Interrupt, TrapContext},
};
use interrupts_cache::InterruptsCacheResult;

/// RISC-V hart state
pub struct HartState<M: backend::ManagerBase> {
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

    /// Reservation set address
    pub reservation_set: ReservationSet<M>,
}

/// Layout of [HartState]
pub type HartStateLayout = (
    registers::XRegistersLayout,
    registers::FRegistersLayout,
    csregisters::CSRegistersLayout,
    mode::ModeLayout,
    Atom<Address>,                         // Program counter layout
    reservation_set::ReservationSetLayout, // Reservation set layout
);

impl<M: backend::Manager> HartState<M> {
    /// Bind the hart state to the given allocated space.
    pub fn bind(space: backend::AllocatedOf<HartStateLayout, M>) -> Self {
        Self {
            xregisters: registers::XRegisters::bind(space.0),
            fregisters: registers::FRegisters::bind(space.1),
            csregisters: csregisters::CSRegisters::bind(space.2),
            mode: mode::ModeCell::bind(space.3),
            pc: space.4,
            reservation_set: ReservationSet::bind(space.5),
        }
    }

    /// Reset the hart state.
    pub fn reset(&mut self, pc: Address) {
        self.xregisters.reset();
        self.fregisters.reset();
        self.csregisters.reset();
        self.mode.reset();
        self.pc.write(pc);
        self.reservation_set.reset();
    }

    /// Given a trap source and a return address, take a trap on the machine.
    pub fn take_trap<TC: TrapContext>(&mut self, trap_source: TC, return_pc: u64) -> u64 {
        self.take_trap_from_mode(trap_source, self.mode.read(), return_pc)
    }

    /// Given a trap source, a return address and a mode to trap from, take a
    /// trap on the machine.
    fn take_trap_from_mode<TC: TrapContext>(
        &mut self,
        trap_source: TC,
        current_mode: Mode,
        return_pc: Address,
    ) -> Address {
        let trap_mode = self.csregisters.get_trap_mode(&trap_source, current_mode);
        let (xtvec_reg, xepc_reg, xcause_reg, xtval_reg) = match trap_mode {
            TrapMode::Supervisor => (
                CSRegister::stvec,
                CSRegister::sepc,
                CSRegister::scause,
                CSRegister::stval,
            ),
            TrapMode::Machine => (
                CSRegister::mtvec,
                CSRegister::mepc,
                CSRegister::mcause,
                CSRegister::mtval,
            ),
        };

        // Setting xepc allows the trap handler to resume the previous computation
        self.csregisters.write(xepc_reg, return_pc);

        // The trap handler wants to know what caused the trap
        self.csregisters.write(xcause_reg, trap_source.xcause());
        self.csregisters.write(xtval_reg, trap_source.xtval());

        // Configure machine status for the trap handler
        let mstatus: MStatus = self.csregisters.read(CSRegister::mstatus);
        let mstatus = match trap_mode {
            TrapMode::Supervisor => {
                // Remember whether interupts were enabled before taking the trap
                let interrupts_enabled = mstatus.sie();
                let mstatus = mstatus.with_spie(interrupts_enabled);

                // Disable interrupts for the trap handler
                let mstatus = mstatus.with_sie(false);

                // Remember the previous privilege mode
                mstatus.with_spp(
                    match current_mode {
                        Mode::User => xstatus::SPPValue::User,
                        Mode::Supervisor => xstatus::SPPValue::Supervisor,
                        _ => unreachable!("Trapping to S-mode from Machine mode should already be an Environment Exception"),
                    },
                )
            }

            TrapMode::Machine => {
                // Remember whether interupts were enabled before taking the trap
                let interrupts_enabled = mstatus.mie();
                let mstatus = mstatus.with_mpie(interrupts_enabled);

                // Disable interrupts for the trap handler
                let mstatus = mstatus.with_mie(false);

                // Remember the previous privilege mode
                mstatus.with_mpp(match current_mode {
                    Mode::User => xstatus::MPPValue::User,
                    Mode::Supervisor => xstatus::MPPValue::Supervisor,
                    Mode::Machine => xstatus::MPPValue::Machine,
                })
            }
        };
        self.csregisters.write(CSRegister::mstatus, mstatus);

        // Escalate the privilege to the corresponding mode
        self.mode.write(trap_mode.as_mode());

        trap_source.trap_handler_address(self.csregisters.read(xtvec_reg))
    }

    /// Return the current [`Interrupt`] with highest priority to be handled
    /// or [`None`] if there isn't any available
    pub fn get_pending_interrupt(&mut self, current_mode: Mode) -> Option<Interrupt> {
        let possible = match self.get_interrupts_cache(current_mode) {
            InterruptsCacheResult::PendingInterrupt(result) => return result,
            InterruptsCacheResult::PossibleInterrupts(possible_interrupts) => possible_interrupts,
            InterruptsCacheResult::Miss => self.csregisters.possible_interrupts(current_mode),
        };
        let interrupt = if possible == 0 {
            None
        } else {
            // Normally, interrupts from devices / external sources are signaled to the CPU
            // by updating the MEIP,MTIP,MSIP,SEIP,STIP,SSIP interrupt bits in the MIP register.
            // In the hardware world, these CSRs updates would be done by PLIC / CLINT
            // based on memory written by devices on the bus

            // Section 3.1.9 MIP & MIE registers
            // Multiple simultaneous interrupts destined for M-mode are handled in the
            // following decreasing priority order: MEI, MSI, MTI, SEI, SSI, STI

            // sip is a shadow of mip and sie is a shadow of mie
            // hence we only need to look at mie to find out the interrupt bits
            let mip: CSRRepr = self.csregisters.read(CSRegister::mip);
            let active_interrupts = mip & possible;

            if u64::bit(
                active_interrupts,
                Interrupt::MACHINE_EXTERNAL_EXCEPTION_CODE as usize,
            ) {
                Some(Interrupt::MachineExternal)
            } else if u64::bit(
                active_interrupts,
                Interrupt::MACHINE_SOFTWARE_EXCEPTION_CODE as usize,
            ) {
                Some(Interrupt::MachineSoftware)
            } else if u64::bit(
                active_interrupts,
                Interrupt::MACHINE_TIMER_EXCEPTION_CODE as usize,
            ) {
                Some(Interrupt::MachineTimer)
            } else if u64::bit(
                active_interrupts,
                Interrupt::SUPERVISOR_EXTERNAL_EXCEPTION_CODE as usize,
            ) {
                Some(Interrupt::SupervisorExternal)
            } else if u64::bit(
                active_interrupts,
                Interrupt::SUPERVISOR_SOFTWARE_EXCEPTION_CODE as usize,
            ) {
                Some(Interrupt::SupervisorSoftware)
            } else if u64::bit(
                active_interrupts,
                Interrupt::SUPERVISOR_TIMER_EXCEPTION_CODE as usize,
            ) {
                Some(Interrupt::SupervisorTimer)
            } else {
                None
            }
        };
        self.update_interrupts_cache(current_mode, possible, interrupt);
        interrupt
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        backend_test,
        machine_state::hart_state::{HartState, HartStateLayout},
        state_backend::tests::test_determinism,
    };

    backend_test!(test_hart_state_reset, F, {
        proptest::proptest!(|(pc: u64)| {
            test_determinism::<F, HartStateLayout, _>(|space| {
                let mut hart = HartState::bind(space);
                hart.reset(pc);
            });
        });
    });
}
