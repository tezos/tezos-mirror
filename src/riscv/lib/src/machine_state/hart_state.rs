// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use super::csregisters::xstatus::MStatus;
use crate::bits::u64;
use crate::machine_state::csregisters;
use crate::machine_state::csregisters::CSRegister;
use crate::machine_state::csregisters::xstatus;
use crate::machine_state::memory::Address;
use crate::machine_state::mode::Mode;
use crate::machine_state::mode::TrapMode;
use crate::machine_state::registers;
use crate::machine_state::reservation_set;
use crate::machine_state::reservation_set::ReservationSet;
use crate::state::NewState;
use crate::state_backend as backend;
use crate::state_backend::Atom;
use crate::state_backend::Cell;
use crate::traps::Interrupt;
use crate::traps::TrapContext;

/// RISC-V hart state
pub struct HartState<M: backend::ManagerBase> {
    /// Integer registers
    pub xregisters: registers::XRegisters<M>,

    /// Floating-point number registers
    pub fregisters: registers::FRegisters<M>,

    /// Control and state registers
    pub csregisters: csregisters::CSRegisters<M>,

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
            pc: space.3,
            reservation_set: ReservationSet::bind(space.4),
        }
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub fn struct_ref<'a, F: backend::FnManager<backend::Ref<'a, M>>>(
        &'a self,
    ) -> backend::AllocatedOf<HartStateLayout, F::Output> {
        (
            self.xregisters.struct_ref::<F>(),
            self.fregisters.struct_ref::<F>(),
            self.csregisters.struct_ref::<F>(),
            self.pc.struct_ref::<F>(),
            self.reservation_set.struct_ref::<F>(),
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
        self.pc.write(pc);
        self.reservation_set.reset();
    }

    /// Given a trap source and a return address, take a trap on the machine.
    pub fn take_trap<TC: TrapContext>(&mut self, trap_source: TC, return_pc: u64) -> u64
    where
        M: backend::ManagerReadWrite,
    {
        let trap_mode = self.csregisters.get_trap_mode(&trap_source);
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
                mstatus.with_spp(xstatus::SPPValue::User)
            }

            TrapMode::Machine => {
                // Remember whether interupts were enabled before taking the trap
                let interrupts_enabled = mstatus.mie();
                let mstatus = mstatus.with_mpie(interrupts_enabled);

                // Disable interrupts for the trap handler
                let mstatus = mstatus.with_mie(false);

                // Remember the previous privilege mode
                mstatus.with_mpp(xstatus::MPPValue::User)
            }
        };
        self.csregisters.write(CSRegister::mstatus, mstatus);

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

impl<M: backend::ManagerBase> NewState<M> for HartState<M> {
    fn new(manager: &mut M) -> Self
    where
        M: backend::ManagerAlloc,
    {
        Self {
            xregisters: registers::XRegisters::new(manager),
            fregisters: registers::FRegisters::new(manager),
            csregisters: csregisters::CSRegisters::new(manager),
            pc: Cell::new(manager),
            reservation_set: ReservationSet::new(manager),
        }
    }
}

impl<M: backend::ManagerClone> Clone for HartState<M> {
    fn clone(&self) -> Self {
        Self {
            xregisters: self.xregisters.clone(),
            fregisters: self.fregisters.clone(),
            csregisters: self.csregisters.clone(),
            pc: self.pc.clone(),
            reservation_set: self.reservation_set.clone(),
        }
    }
}
