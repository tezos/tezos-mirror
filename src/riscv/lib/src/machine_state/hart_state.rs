// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use super::csregisters::xstatus::MStatus;
use crate::{
    bits::u64,
    default::ConstDefault,
    machine_state::{
        bus::Address,
        csregisters::{self, xstatus, CSRegister},
        mode::{Mode, TrapMode},
        registers,
        reservation_set::{self, ReservationSet},
    },
    state_backend::{self as backend, Atom, Cell},
    traps::{Interrupt, TrapContext},
};

/// RISC-V hart state
pub struct HartState<M: backend::ManagerBase> {
    /// Integer registers
    pub xregisters: registers::XRegisters<M>,

    /// Floating-point number registers
    pub fregisters: registers::FRegisters<M>,

    /// Control and state registers
    pub csregisters: csregisters::CSRegisters<M>,

    /// Current running mode of hart
    pub mode: Cell<Mode, M>,

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
    Atom<Mode>,
    Atom<Address>,                         // Program counter layout
    reservation_set::ReservationSetLayout, // Reservation set layout
);

impl<M: backend::ManagerBase> HartState<M> {
    /// Bind the hart state to the given allocated space.
    pub fn bind(space: backend::AllocatedOf<HartStateLayout, M>) -> Self {
        Self {
            xregisters: registers::XRegisters::bind(space.0),
            fregisters: registers::FRegisters::bind(space.1),
            csregisters: csregisters::CSRegisters::bind(space.2),
            mode: space.3,
            pc: space.4,
            reservation_set: ReservationSet::bind(space.5),
        }
    }

    /// Obtain a structure with references to the bound regions of this type.
    pub fn struct_ref(&self) -> backend::AllocatedOf<HartStateLayout, backend::Ref<'_, M>> {
        (
            self.xregisters.struct_ref(),
            self.fregisters.struct_ref(),
            self.csregisters.struct_ref(),
            self.mode.struct_ref(),
            self.pc.struct_ref(),
            self.reservation_set.struct_ref(),
        )
    }

    /// Reset the hart state.
    pub fn reset(&mut self, pc: Address)
    where
        M: backend::ManagerWrite,
    {
        self.xregisters.reset();
        self.fregisters.reset();
        self.csregisters.reset();
        self.mode.write(Mode::DEFAULT);
        self.pc.write(pc);
        self.reservation_set.reset();
    }

    /// Given a trap source and a return address, take a trap on the machine.
    pub fn take_trap<TC: TrapContext>(&mut self, trap_source: TC, return_pc: u64) -> u64
    where
        M: backend::ManagerReadWrite,
    {
        self.take_trap_from_mode(trap_source, self.mode.read(), return_pc)
    }

    /// Given a trap source, a return address and a mode to trap from, take a
    /// trap on the machine.
    fn take_trap_from_mode<TC: TrapContext>(
        &mut self,
        trap_source: TC,
        current_mode: Mode,
        return_pc: Address,
    ) -> Address
    where
        M: backend::ManagerReadWrite,
    {
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
    #[inline(always)]
    pub fn get_pending_interrupt(&mut self, _current_mode: Mode) -> Option<Interrupt>
    where
        M: backend::ManagerRead,
    {
        // Interrupts are not implemented yet.
        None
    }
}

impl<M: backend::ManagerClone> Clone for HartState<M> {
    fn clone(&self) -> Self {
        Self {
            xregisters: self.xregisters.clone(),
            fregisters: self.fregisters.clone(),
            csregisters: self.csregisters.clone(),
            mode: self.mode.clone(),
            pc: self.pc.clone(),
            reservation_set: self.reservation_set.clone(),
        }
    }
}
