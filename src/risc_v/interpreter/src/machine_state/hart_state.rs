use crate::{
    machine_state::{
        bus::Address,
        csregisters::{self, xstatus, CSRegister},
        mode::{self, Mode, TrapMode},
        registers,
    },
    state_backend::{self as backend, Atom, Cell},
    traps::{EnvironException, Exception, Interrupt, TrapContext},
};
use twiddle::Twiddle;

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

    /// Figure out the mode a trap is taken into.
    fn get_trap_mode<TC: TrapContext>(
        &self,
        trap: &TC,
        current_mode: Mode,
        deleg: CSRegister,
    ) -> Result<TrapMode, EnvironException> {
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

        let deleg_val = self.csregisters.read(deleg);
        let delegated = deleg_val.bit(trap.exception_code() as usize);

        if current_mode <= Mode::Supervisor && delegated {
            Ok(TrapMode::Supervisor)
        } else if current_mode <= Mode::Machine {
            Ok(TrapMode::Machine)
        } else {
            Err(EnvironException::TrapFromDMode)
        }
    }

    /// Returns the mode a trap should be taken into for an exception (synchronous one).
    ///
    /// Exceptions in `Debug` mode will throw [`EnvironException::TrapFromDMode`].
    pub fn get_exception_mode(
        &self,
        exception: &Exception,
        current_mode: Mode,
    ) -> Result<TrapMode, EnvironException> {
        self.get_trap_mode(exception, current_mode, CSRegister::medeleg)
    }

    /// Return the [`Mode`] the interrupt trap is taken into.
    ///
    /// Assumes `interrupt` is enabled in `mie` and pending in `mip`.
    ///
    /// Interrupts in `Debug` mode will throw [`EnvironException::TrapFromDMode`].
    pub fn get_interrupt_mode(
        &self,
        interrupt: &Interrupt,
        current_mode: Mode,
    ) -> Result<TrapMode, EnvironException> {
        self.get_trap_mode(interrupt, current_mode, CSRegister::mideleg)
    }

    /// Procedure to start trap handling, either for an exception or an interrupt in S-mode.
    /// See sections 3.1.6.1, 5.1.1, 3.1.8, 3.1.14, 3.1.16, 5.1.9
    ///
    /// Assumes `current_mode` is [`Mode::User`] / [`Mode::Supervisor`] / [`Mode::Machine`].
    ///
    /// Returns value of the new program counter.
    pub fn take_trap_machine<TC: TrapContext>(
        &mut self,
        current_mode: Mode,
        current_pc: Address,
        trap: TC,
    ) -> Address {
        // Set MEPC to current pc
        self.csregisters.write(CSRegister::mepc, current_pc);

        // Set MCAUSE
        self.csregisters.write(CSRegister::mcause, trap.xcause());

        // Set MTVAL (Machine trap value)
        self.csregisters.write(CSRegister::mtval, trap.xtval());

        let mstatus = self.csregisters.read(CSRegister::mstatus);
        // Set MPIE to current MIE value in mstatus
        let mie_bit = xstatus::get_MIE(mstatus);
        let mstatus = xstatus::set_MPIE(mstatus, mie_bit);

        // Set MIE to 0 (Disable machine interrupt-enable bit)
        let mstatus = xstatus::set_MIE(mstatus, false);

        // Set MPP with current mode
        let mstatus = xstatus::set_MPP(
            mstatus,
            match current_mode {
                Mode::User => xstatus::MPPValue::User,
                Mode::Supervisor => xstatus::MPPValue::Supervisor,
                Mode::Machine => xstatus::MPPValue::Machine,
                _ => unreachable!(
                    "Trapping to M-mode from Debug should already be an Environment Exception "
                ),
            },
        );

        self.csregisters.write(CSRegister::mstatus, mstatus);
        self.mode.write(Mode::Machine);

        trap.trap_handler_address(self.csregisters.read(CSRegister::mtvec))
    }

    /// Procedure to start trap handling, either for an exception or an interrupt in S-mode.
    /// See sections 3.1.6.1 7 5.1.1 & 3.1.8 & 3.1.14 & 3.1.16 & 5.1.9
    ///
    /// Assumes `current_mode` is [`Mode::User`] or [`Mode::Supervisor`]
    ///
    /// Returns value of the new program counter.
    pub fn take_trap_supervisor<TC: TrapContext>(
        &mut self,
        current_mode: Mode,
        current_pc: Address,
        trap: TC,
    ) -> Address {
        // Set SEPC to current PC
        // TODO: Investigate why rvemu for some instructions sets pc + 4
        self.csregisters.write(CSRegister::sepc, current_pc);

        // Set SCAUSE
        self.csregisters.write(CSRegister::scause, trap.xcause());

        // Set STVAL (Supervisor trap value)
        self.csregisters.write(CSRegister::stval, trap.xtval());

        let mstatus = self.csregisters.read(CSRegister::mstatus);
        // Set SPIE to current SIE value in mstatus
        let sie_bit = xstatus::get_SIE(mstatus);
        let mstatus = xstatus::set_SPIE(mstatus, sie_bit);

        // Set SIE to 0 (Disable supervisor interrupt-enable bit)
        let mstatus = xstatus::set_SIE(mstatus, false);

        // Set SPP with current mode
        let mstatus = xstatus::set_SPP(
            mstatus,
            match current_mode {
                Mode::User => xstatus::SPPValue::User,
                Mode::Supervisor => xstatus::SPPValue::Supervisor,
                _ => unreachable!("Trapping to S-mode from Machine or Debug mode should already be an Environment Exception"),
            },
        );

        self.csregisters.write(CSRegister::mstatus, mstatus);
        self.mode.write(Mode::Supervisor);

        trap.trap_handler_address(self.csregisters.read(CSRegister::stvec))
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
