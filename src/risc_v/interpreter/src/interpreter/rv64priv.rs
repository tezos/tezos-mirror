use crate::{
    machine_state::{
        bus::{main_memory, Address},
        csregisters::{
            xstatus::{self, MPPValue, SPPValue},
            CSRegister,
        },
        hart_state::HartState,
        mode::Mode,
        MachineState,
    },
    state_backend as backend,
    traps::Exception,
};

impl<M> HartState<M>
where
    M: backend::Manager,
{
    /// `MRET` instruction
    ///
    /// If successful, returns next instruction address to be executed from `MEPC`
    pub fn run_mret(&mut self) -> Result<Address, Exception> {
        // println!("run mret?");
        // Only M-mode (and Debug) can run mret
        match self.mode.read() {
            Mode::User | Mode::Supervisor => return Err(Exception::IllegalInstruction),
            Mode::Machine => (),
        }

        let mstatus = self.csregisters.read(CSRegister::mstatus);
        // get MPP
        let prev_privilege = xstatus::get_MPP(mstatus);
        // Set MIE to MPIE
        let prev_mie = xstatus::get_MPIE(mstatus);
        let mstatus = xstatus::set_MIE(mstatus, prev_mie);
        // set MPIE to 1
        let mstatus = xstatus::set_MPIE(mstatus, true);
        // Set MPP to least privilege-mode supported
        let mstatus = xstatus::set_MPP(mstatus, MPPValue::User);
        // Set MPRV to 0 when leaving M-mode. (MPP != M-mode)
        let mstatus = if prev_privilege != MPPValue::Machine {
            xstatus::set_MPRV(mstatus, false)
        } else {
            mstatus
        };

        // Commit the mstatus
        self.csregisters.write(CSRegister::mstatus, mstatus);

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
        // Section 3.1.6.5
        // SRET raises IllegalInstruction exception when TSR (Trap SRET) bit is on.
        let mstatus = self.csregisters.read(CSRegister::mstatus);
        if xstatus::get_TSR(mstatus) {
            return Err(Exception::IllegalInstruction);
        }
        // get SPP
        let prev_privilege = xstatus::get_SPP(mstatus);
        // Set SIE to SPIE
        let prev_sie = xstatus::get_SPIE(mstatus);
        let mstatus = xstatus::set_SIE(mstatus, prev_sie);
        // set SPIE to 1
        let mstatus = xstatus::set_SPIE(mstatus, true);
        // Set SPP to least privilege-mode supported
        let mstatus = xstatus::set_SPP(mstatus, SPPValue::User);
        // Set MPRV to 0 when leaving M-mode. (SPP != M-mode)
        // Since SPP can only hold User / Supervisor, it is always set to 0
        let mstatus = xstatus::set_MPRV(mstatus, false);

        // Commit the mstatus
        self.csregisters.write(CSRegister::mstatus, mstatus);

        // Set the mode after handling sret, according to SPP read initially
        self.mode.write(match prev_privilege {
            SPPValue::User => Mode::User,
            SPPValue::Supervisor => Mode::Supervisor,
        });

        // set pc to SEPC (we just have to return it)
        Ok(self.csregisters.read(CSRegister::sepc))
    }
}

impl<ML, M> MachineState<ML, M>
where
    ML: main_memory::MainMemoryLayout,
    M: backend::Manager,
{
    /// `WFI` instruction
    ///
    /// It is a no-op for us since interrupt detection is currently based
    /// only on checking mip/mie/mstatus
    pub fn run_wfi(&self) {
        // no-op
    }
}
