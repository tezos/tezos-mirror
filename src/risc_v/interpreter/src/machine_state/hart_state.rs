use crate::{
    machine_state::{
        bus::Address,
        csregisters::{self, xstatus, CSRegister},
        mode::{self, Mode, TrapMode},
        registers,
    },
    state_backend::{self as backend, Atom, Cell},
    traps::TrapContext,
};

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
    pub fn reset(&mut self, pc: Address) {
        self.xregisters.reset();
        self.fregisters.reset();
        self.csregisters.reset();
        self.mode.reset();
        self.pc.write(pc);
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
        let mstatus = self.csregisters.read(CSRegister::mstatus);
        let mstatus = match trap_mode {
            TrapMode::Supervisor => {
                // Remember whether interupts were enabled before taking the trap
                let interrupts_enabled = xstatus::get_SIE(mstatus);
                let mstatus = xstatus::set_SPIE(mstatus, interrupts_enabled);

                // Disable interrupts for the trap handler
                let mstatus = xstatus::set_SIE(mstatus, false);

                // Remember the previous privilege mode
                xstatus::set_SPP(
                    mstatus,
                    match current_mode {
                        Mode::User => xstatus::SPPValue::User,
                        Mode::Supervisor => xstatus::SPPValue::Supervisor,
                        _ => unreachable!("Trapping to S-mode from Machine mode should already be an Environment Exception"),
                    },
                )
            }

            TrapMode::Machine => {
                // Remember whether interupts were enabled before taking the trap
                let interrupts_enabled = xstatus::get_MIE(mstatus);
                let mstatus = xstatus::set_MPIE(mstatus, interrupts_enabled);

                // Disable interrupts for the trap handler
                let mstatus = xstatus::set_MIE(mstatus, false);

                // Remember the previous privilege mode
                xstatus::set_MPP(
                    mstatus,
                    match current_mode {
                        Mode::User => xstatus::MPPValue::User,
                        Mode::Supervisor => xstatus::MPPValue::Supervisor,
                        Mode::Machine => xstatus::MPPValue::Machine,
                    },
                )
            }
        };
        self.csregisters.write(CSRegister::mstatus, mstatus);

        // Escalate the privilege to the corresponding mode
        self.mode.write(trap_mode.as_mode());

        trap_source.trap_handler_address(self.csregisters.read(xtvec_reg))
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
