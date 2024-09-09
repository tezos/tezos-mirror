// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::{
    machine_state::{
        bus::{main_memory::MainMemoryLayout, Address},
        csregisters::{
            effects::handle_csr_effect,
            xstatus::{MPPValue, SPPValue},
            CSRegister,
        },
        hart_state::HartState,
        instruction_cache::InstructionCacheLayout,
        mode::Mode,
        registers::XRegister,
        AccessType, MachineState,
    },
    state_backend::{self as backend},
    traps::Exception,
};
use strum::IntoEnumIterator;

impl<M> HartState<M>
where
    M: backend::ManagerReadWrite,
{
    /// `MRET` instruction
    ///
    /// If successful, returns next instruction address to be executed from `MEPC`
    pub fn run_mret(&mut self) -> Result<Address, Exception> {
        // Only M-mode (and Debug) can run mret
        match self.mode.read() {
            Mode::User | Mode::Supervisor => return Err(Exception::IllegalInstruction),
            Mode::Machine => (),
        }

        let csrs = &mut self.csregisters;
        // get MPP
        let prev_privilege = csrs.mstatus().mpp.read();
        // Set MIE to MPIE
        let prev_mie = csrs.mstatus().mpie.read();
        let effect = csrs.mstatus_mut().mie.write(prev_mie);
        handle_csr_effect(csrs, effect);
        // set MPIE to 1
        csrs.mstatus_mut().mpie.write(true);
        // Set MPP to least p.with_ivilege-mode supported
        csrs.mstatus_mut().mpp.write(MPPValue::User);
        // Set MPRV to 0 when leaving M-mode. (MPP != M-mode)
        if prev_privilege != MPPValue::Machine {
            csrs.mstatus_mut().mprv.write(false)
        }

        // Set the mode after handling mret, according to MPP read initially
        self.mode.write(match prev_privilege {
            MPPValue::User => Mode::User,
            MPPValue::Supervisor => Mode::Supervisor,
            MPPValue::Machine => Mode::Machine,
        });

        // set pc to MEPC (we just have to return it)
        Ok(self.csregisters.read(CSRegister::mepc))
    }

    /// `SRET` instruction
    ///
    /// If successful, returns next instruction address to be executed from `SEPC`
    pub fn run_sret(&mut self) -> Result<Address, Exception> {
        // Only M and S mode (and Debug) can run SRET
        match self.mode.read() {
            Mode::User => return Err(Exception::IllegalInstruction),
            Mode::Supervisor | Mode::Machine => (),
        }
        let csrs = &mut self.csregisters;
        // Section 3.1.6.5
        // SRET raises IllegalInstruction exception when TSR (Trap SRET) bit is on.
        if csrs.mstatus().tsr.read() {
            return Err(Exception::IllegalInstruction);
        }
        // get SPP
        let prev_privilege = csrs.mstatus().spp.read();
        // Set SIE to SPIE
        let prev_sie = csrs.mstatus().spie.read();
        let effect = csrs.mstatus_mut().sie.write(prev_sie);
        handle_csr_effect(csrs, effect);
        // set SPIE to 1
        csrs.mstatus_mut().spie.write(true);
        // Set SPP to least privilege-mode supported
        csrs.mstatus_mut().spp.write(SPPValue::User);
        // Set MPRV to 0 when leaving M-mode. (SPP != M-mode)
        // Since SPP can only hold User / Supervisor, it is always set to 0
        csrs.mstatus_mut().mprv.write(false);

        // Set the mode after handling sret, according to SPP read initially
        self.mode.write(match prev_privilege {
            SPPValue::User => Mode::User,
            SPPValue::Supervisor => Mode::Supervisor,
        });

        // set pc to SEPC (we just have to return it)
        Ok(self.csregisters.read(CSRegister::sepc))
    }
}

impl<ML, ICL, M> MachineState<ML, ICL, M>
where
    ML: MainMemoryLayout,
    ICL: InstructionCacheLayout,
    M: backend::ManagerReadWrite,
{
    /// `WFI` instruction
    ///
    /// It is a no-op for us since interrupt detection is currently based
    /// only on checking mip/mie/mstatus
    pub fn run_wfi(&self) {
        // no-op
    }

    /// `SFENCE.VMA` instruction
    ///
    /// The supervisor memory-management fence instruction SFENCE.VMA is used to
    /// synchronize updates to in-memory memory-management data structures
    /// with current execution. See sections 3.1.6.5, 3.7.2.
    ///
    /// Section 5.2.1: It is always legal to over-fence.
    #[inline(always)]
    pub fn run_sfence_vma(&mut self, _asid: XRegister, _vaddr: XRegister) -> Result<(), Exception> {
        // Invalidate all cached address translations.
        self.translation_cache.invalidate(AccessType::iter());

        let mode = self.hart.mode.read();
        let tvm = self.hart.csregisters.mstatus().tvm.read();

        if tvm && mode == Mode::Supervisor {
            return Err(Exception::IllegalInstruction);
        }

        // Even if we over-fence, thus ignoring asid and vaddr, this instruction
        // is still no-op since memory loads/stores are not cached currently.
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        backend_test, create_backend, create_state,
        machine_state::{
            bus::main_memory::tests::T1K,
            csregisters::{xstatus, CSRRepr, CSRegister},
            instruction_cache::TestInstructionCacheLayout,
            mode::Mode,
            registers::{a0, t0},
            MachineState, MachineStateLayout,
        },
        traps::Exception,
    };

    backend_test!(test_sfence, F, {
        type L = MachineStateLayout<T1K, TestInstructionCacheLayout>;
        let mut backend = create_backend!(L, F);
        let mut state = create_state!(MachineState, L, F, backend, T1K, TestInstructionCacheLayout);

        let run_test = |state: &mut MachineState<_, _, _>,
                        mode: Mode,
                        bit: bool,
                        result: Result<(), Exception>| {
            state.hart.mode.write(mode);
            state.hart.csregisters.set_bits(
                CSRegister::mstatus,
                (bit as CSRRepr) << xstatus::MStatus::TVM_OFFSET,
            );
            let r = state.run_sfence_vma(t0, a0);
            assert_eq!(r, result);
        };

        run_test(&mut state, Mode::User, false, Ok(()));
        run_test(&mut state, Mode::Supervisor, false, Ok(()));
        run_test(&mut state, Mode::Machine, false, Ok(()));
        run_test(&mut state, Mode::User, true, Ok(()));
        run_test(
            &mut state,
            Mode::Supervisor,
            true,
            Err(Exception::IllegalInstruction),
        );
        run_test(&mut state, Mode::Machine, true, Ok(()));
    });
}
