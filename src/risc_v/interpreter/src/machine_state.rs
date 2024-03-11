// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

#![deny(rustdoc::broken_intra_doc_links)]

pub mod bus;
pub mod csregisters;
pub mod hart_state;
pub mod mode;
pub mod registers;

#[cfg(test)]
extern crate proptest;

use crate::{
    devicetree,
    machine_state::{
        bus::{main_memory, Address, Addressable, Bus, OutOfBounds},
        csregisters::CSRegister,
        hart_state::{HartState, HartStateLayout},
        mode::TrapMode,
    },
    parser::{instruction::Instr, parse},
    program::Program,
    state_backend as backend,
    traps::{EnvironException, Exception, Interrupt, TrapContext},
};
use twiddle::Twiddle;

/// Layout for the machine state
pub type MachineStateLayout<ML> = (HartStateLayout, bus::BusLayout<ML>);

/// Machine state
pub struct MachineState<ML: main_memory::MainMemoryLayout, M: backend::Manager> {
    pub hart: HartState<M>,
    pub bus: Bus<ML, M>,
}

/// How to modify the program counter
enum ProgramCounterUpdate {
    /// Jump to a fixed address
    Set(Address),
    /// Offset the program counter by a certain value
    Add(u64),
}

/// Result type when running multiple steps at a time with [`MachineState::step_many`]
pub struct StepManyResult {
    pub steps: usize,
    pub exception: Option<EnvironException>,
}

/// Runs an R-type instruction over [`XRegisters`]
macro_rules! run_r_type_instr {
    ($state: ident, $instr: ident, $args: ident, $run_fn: ident) => {{
        $state
            .hart
            .xregisters
            .$run_fn($args.rs1, $args.rs2, $args.rd);
        Ok(Add($instr.width()))
    }};
}

/// Runs an I-type instruction over [`XRegisters`]
macro_rules! run_i_type_instr {
    ($state: ident, $instr: ident, $args: ident, $run_fn: ident) => {{
        $state
            .hart
            .xregisters
            .$run_fn($args.imm, $args.rs1, $args.rd);
        Ok(Add($instr.width()))
    }};
}

/// Runs a B-type instruction over [`HartState`]
macro_rules! run_b_type_instr {
    ($state: ident, $args: ident, $run_fn: ident) => {{
        Ok(Set($state.hart.$run_fn($args.imm, $args.rs1, $args.rs2)))
    }};
}

/// Runs an U-type instruction over [`HartState`]
macro_rules! run_u_type_instr {
    ($state: ident, $instr:ident, $args: ident, $($run_fn:ident).+) => {{
        // XXX: Funky syntax to capture xregister.run_fn identifier
        // correctly since Rust doesn't like dots in macro arguments
        $state.hart.$($run_fn).+($args.imm, $args.rd);
        Ok(Add($instr.width()))
    }};
}

/// Runs a load instruction
macro_rules! run_load_instr {
    ($state: ident, $instr: ident, $args: ident, $run_fn: ident) => {{
        $state
            .$run_fn($args.imm, $args.rs1, $args.rd)
            .map(|_| Add($instr.width()))
    }};
}

/// Runs a store instruction
macro_rules! run_store_instr {
    ($state: ident, $instr: ident, $args: ident, $run_fn: ident) => {{
        $state
            .$run_fn($args.imm, $args.rs1, $args.rs2)
            .map(|_| Add($instr.width()))
    }};
}

/// Runs a CSR instruction
macro_rules! run_csr_instr {
    ($state: ident, $instr: ident, $args: ident, $run_fn: ident) => {{
        $state
            .hart
            .$run_fn($args.csr, $args.rs1, $args.rd)
            .map(|_| Add($instr.width()))
    }};
}

/// Runs a CSR imm instruction
macro_rules! run_csr_imm_instr {
    ($state: ident, $instr: ident, $args: ident, $run_fn: ident) => {{
        $state
            .hart
            .$run_fn($args.csr, $args.imm as u64, $args.rd)
            .map(|_| Add($instr.width()))
    }};
}

/// Runs a syscall instruction (ecall, ebreak)
macro_rules! run_syscall_instr {
    ($state: ident, $run_fn: ident) => {{
        Err($state.hart.$run_fn())
    }};
}

/// Runs a xret instruction (mret, sret, mnret)
macro_rules! run_xret_instr {
    ($state: ident, $run_fn: ident) => {{
        $state.hart.$run_fn().map(Set)
    }};
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
    pub fn reset(&mut self) {
        self.hart.reset(bus::start_of_main_memory::<ML>());
        self.bus.reset();
    }

    /// Fetch instruction from the address given by program counter
    fn fetch_instr(&self, pc: Address) -> Result<Instr, Exception> {
        // The resons to provide the second half in the lambda is
        // because those bytes may be inaccessible or may trigger an exception when read.
        // Hence we can't read eagerly all 4 bytes.
        let fetch_result = (|| {
            let half_instr = self.bus.read(pc)?;
            parse(half_instr, || self.bus.read(pc + 2))
        })();

        // Transform the out of bounds read error into a
        // RISC-V instruction access fault exception
        fetch_result.map_err(|_: OutOfBounds| Exception::InstructionAccessFault(pc))
    }

    /// Advance [`MachineState`] by executing an [`Instr`]
    fn run_instr(&mut self, instr: Instr) -> Result<ProgramCounterUpdate, Exception> {
        use ProgramCounterUpdate::{Add, Set};

        match instr {
            // RV64I R-type instructions
            Instr::Add(args) => run_r_type_instr!(self, instr, args, run_add),
            Instr::Sub(args) => run_r_type_instr!(self, instr, args, run_sub),
            Instr::Xor(args) => run_r_type_instr!(self, instr, args, run_xor),
            Instr::Or(args) => run_r_type_instr!(self, instr, args, run_or),
            Instr::And(args) => run_r_type_instr!(self, instr, args, run_and),
            Instr::Sll(args) => run_r_type_instr!(self, instr, args, run_sll),
            Instr::Srl(args) => run_r_type_instr!(self, instr, args, run_srl),
            Instr::Sra(args) => run_r_type_instr!(self, instr, args, run_sra),
            Instr::Slt(args) => run_r_type_instr!(self, instr, args, run_slt),
            Instr::Sltu(args) => run_r_type_instr!(self, instr, args, run_sltu),
            Instr::Addw(args) => run_r_type_instr!(self, instr, args, run_addw),
            Instr::Subw(args) => run_r_type_instr!(self, instr, args, run_subw),
            Instr::Sllw(args) => run_r_type_instr!(self, instr, args, run_sllw),
            Instr::Srlw(args) => run_r_type_instr!(self, instr, args, run_srlw),
            Instr::Sraw(args) => run_r_type_instr!(self, instr, args, run_sraw),

            // RV64I I-type instructions
            Instr::Addi(args) => run_i_type_instr!(self, instr, args, run_addi),
            Instr::Addiw(args) => run_i_type_instr!(self, instr, args, run_addiw),
            Instr::Xori(args) => run_i_type_instr!(self, instr, args, run_xori),
            Instr::Ori(args) => run_i_type_instr!(self, instr, args, run_ori),
            Instr::Andi(args) => run_i_type_instr!(self, instr, args, run_andi),
            Instr::Slli(args) => run_i_type_instr!(self, instr, args, run_slli),
            Instr::Srli(args) => run_i_type_instr!(self, instr, args, run_srli),
            Instr::Srai(args) => run_i_type_instr!(self, instr, args, run_srai),
            Instr::Slliw(args) => run_i_type_instr!(self, instr, args, run_slliw),
            Instr::Srliw(args) => run_i_type_instr!(self, instr, args, run_srliw),
            Instr::Sraiw(args) => run_i_type_instr!(self, instr, args, run_sraiw),
            Instr::Slti(args) => run_i_type_instr!(self, instr, args, run_slti),
            Instr::Sltiu(args) => run_i_type_instr!(self, instr, args, run_sltiu),
            Instr::Lb(args) => run_load_instr!(self, instr, args, run_lb),
            Instr::Lh(args) => run_load_instr!(self, instr, args, run_lh),
            Instr::Lw(args) => run_load_instr!(self, instr, args, run_lw),
            Instr::Lbu(args) => run_load_instr!(self, instr, args, run_lbu),
            Instr::Lhu(args) => run_load_instr!(self, instr, args, run_lhu),
            Instr::Lwu(args) => run_load_instr!(self, instr, args, run_lwu),
            Instr::Ld(args) => run_load_instr!(self, instr, args, run_ld),
            Instr::Fence(args) => {
                self.run_fence(args.pred, args.succ);
                Ok(Add(instr.width()))
            }
            Instr::FenceTso(_args) => Err(Exception::IllegalInstruction),
            Instr::Ecall => run_syscall_instr!(self, run_ecall),
            Instr::Ebreak => run_syscall_instr!(self, run_ebreak),

            // RV64I S-type instructions
            Instr::Sb(args) => run_store_instr!(self, instr, args, run_sb),
            Instr::Sh(args) => run_store_instr!(self, instr, args, run_sh),
            Instr::Sw(args) => run_store_instr!(self, instr, args, run_sw),
            Instr::Sd(args) => run_store_instr!(self, instr, args, run_sd),

            // RV64I B-type instructions
            Instr::Beq(args) => run_b_type_instr!(self, args, run_beq),
            Instr::Bne(args) => run_b_type_instr!(self, args, run_bne),
            Instr::Blt(args) => run_b_type_instr!(self, args, run_blt),
            Instr::Bge(args) => run_b_type_instr!(self, args, run_bge),
            Instr::Bltu(args) => run_b_type_instr!(self, args, run_bltu),
            Instr::Bgeu(args) => run_b_type_instr!(self, args, run_bgeu),

            // RV64I U-type instructions
            Instr::Lui(args) => run_u_type_instr!(self, instr, args, xregisters.run_lui),
            Instr::Auipc(args) => run_u_type_instr!(self, instr, args, run_auipc),

            // RV64I jump instructions
            Instr::Jal(args) => Ok(Set(self.hart.run_jal(args.imm, args.rd))),
            Instr::Jalr(args) => Ok(Set(self.hart.run_jalr(args.imm, args.rs1, args.rd))),

            // Zicsr instructions
            Instr::Csrrw(args) => run_csr_instr!(self, instr, args, run_csrrw),
            Instr::Csrrs(args) => run_csr_instr!(self, instr, args, run_csrrs),
            Instr::Csrrc(args) => run_csr_instr!(self, instr, args, run_csrrc),
            Instr::Csrrwi(args) => run_csr_imm_instr!(self, instr, args, run_csrrwi),
            Instr::Csrrsi(args) => run_csr_imm_instr!(self, instr, args, run_csrrsi),
            Instr::Csrrci(args) => run_csr_imm_instr!(self, instr, args, run_csrrci),

            // Privileged instructions
            Instr::Mret => run_xret_instr!(self, run_mret),
            Instr::Sret => run_xret_instr!(self, run_sret),
            // Currently not implemented instruction (part of Smrnmi extension)
            Instr::Mnret => Err(Exception::IllegalInstruction),

            Instr::Unknown { instr: _ } => Err(Exception::IllegalInstruction),
            Instr::UnknownCompressed { instr: _ } => Err(Exception::IllegalInstruction),
        }
    }

    /// Fetch & run the instruction located at address `instr_pc`
    fn run_instr_at(&mut self, instr_pc: u64) -> Result<ProgramCounterUpdate, Exception> {
        let instr = self.fetch_instr(instr_pc)?;
        self.run_instr(instr)
    }

    /// Return the current [`Interrupt`] with highest priority to be handled
    /// or [`None`] if there isn't any available
    fn get_pending_interrupt(&self) -> Option<Interrupt> {
        let current_mode = self.hart.mode.read();

        let possible = match self.hart.csregisters.possible_interrupts(current_mode) {
            0 => return None,
            possible => possible,
        };

        // Normally, interrupts from devices / external sources are signaled to the CPU
        // by updating the MEIP,MTIP,MSIP,SEIP,STIP,SSIP interrupt bits in the MIP register.
        // In the hardware world, these CSRs updates would be done by PLIC / CLINT
        // based on memory written by devices on the bus

        // Section 3.1.9 MIP & MIE registers
        // Multiple simultaneous interrupts destined for M-mode are handled in the
        // following decreasing priority order: MEI, MSI, MTI, SEI, SSI, STI

        // sip is a shadow of mip and sie is a shadow of mie
        // hence we only need to look at mie to find out the interrupt bits
        let mip = self.hart.csregisters.read(CSRegister::mip);
        let active_interrupts = mip & possible;

        if active_interrupts.bit(Interrupt::MachineExternal.exception_code() as usize) {
            return Some(Interrupt::MachineExternal);
        }
        if active_interrupts.bit(Interrupt::MachineSoftware.exception_code() as usize) {
            return Some(Interrupt::MachineSoftware);
        }
        if active_interrupts.bit(Interrupt::MachineTimer.exception_code() as usize) {
            return Some(Interrupt::MachineTimer);
        }
        if active_interrupts.bit(Interrupt::SupervisorExternal.exception_code() as usize) {
            return Some(Interrupt::SupervisorExternal);
        }
        if active_interrupts.bit(Interrupt::SupervisorSoftware.exception_code() as usize) {
            return Some(Interrupt::SupervisorSoftware);
        }
        if active_interrupts.bit(Interrupt::SupervisorTimer.exception_code() as usize) {
            return Some(Interrupt::SupervisorTimer);
        }

        None
    }

    /// Handle interrupts (also known as asynchronous exceptions)
    /// by taking a trap for the given interrupt.
    ///
    /// If trap is taken, return new address of program counter.
    /// Throw [`EnvironException`] if the interrupt has to be treated by the execution enviroment.
    fn trap_interrupt(&mut self, interrupt: Interrupt) -> Result<Address, EnvironException> {
        let current_mode = self.hart.mode.read();
        let current_pc = self.hart.pc.read();
        let mip = self.hart.csregisters.read(CSRegister::mip);

        // make the interrupt pending bit zero for the haandled interrupt
        self.hart.csregisters.write(
            CSRegister::mip,
            mip.set_bit(interrupt.exception_code() as usize, false),
        );

        let trap_mode = self.hart.get_interrupt_mode(&interrupt, current_mode)?;
        let new_pc = match trap_mode {
            TrapMode::Machine => self
                .hart
                .take_trap_machine(current_mode, current_pc, interrupt),
            TrapMode::Supervisor => {
                self.hart
                    .take_trap_supervisor(current_mode, current_pc, interrupt)
            }
        };

        Ok(new_pc)
    }

    /// Handle an [`Exception`] if one was risen during execution
    /// of an instruction (also known as synchronous exception) by taking a trap.
    ///
    /// Return the new address of the program counter, becoming the address of a trap handler.
    /// Throw [`EnvironException`] if the exception needs to be treated by the execution enviroment.
    fn trap_exception(
        &mut self,
        exception: Exception,
        current_pc: Address,
    ) -> Result<Address, EnvironException> {
        if let Ok(exc) = EnvironException::try_from(&exception) {
            return Err(exc);
        }

        let current_mode = self.hart.mode.read();
        let trap_mode = self.hart.get_exception_mode(&exception, current_mode)?;
        let new_pc = match trap_mode {
            TrapMode::Supervisor => {
                self.hart
                    .take_trap_supervisor(current_mode, current_pc, exception)
            }
            TrapMode::Machine => self
                .hart
                .take_trap_machine(current_mode, current_pc, exception),
        };

        Ok(new_pc)
    }

    /// Take an interrupt if available, and then
    /// perform precisely one [`Instr`] and handle the traps that may rise as a side-effect.
    ///
    /// The [`Err`] case represents an [`Exception`] to be handled by
    /// the execution environment, narrowed down by the type [`EnvironException`].
    pub fn step(&mut self) -> Result<(), EnvironException> {
        // Try to take an interrupt if available, and then
        // obtain the pc for the next instruction to be executed
        let instr_pc = match self.get_pending_interrupt() {
            None => self.hart.pc.read(),
            Some(interrupt) => self.trap_interrupt(interrupt)?,
        };

        // Fetch & run the instruction
        let instr_result = self.run_instr_at(instr_pc);

        // Take exception if needed
        let pc_update = match instr_result {
            Err(exc) => ProgramCounterUpdate::Set(self.trap_exception(exc, instr_pc)?),
            Ok(upd) => upd,
        };

        // Update program couter
        match pc_update {
            ProgramCounterUpdate::Set(address) => self.hart.pc.write(address),
            ProgramCounterUpdate::Add(width) => self.hart.pc.write(instr_pc + width),
        };

        Ok(())
    }

    /// Perform at most `max` instructions. Returns the number of retired instructions.
    ///
    /// See `octez_risc_v_pvm::state::Pvm`
    pub fn step_many<F>(&mut self, max: usize, mut should_continue: F) -> StepManyResult
    where
        F: FnMut(&Self) -> bool,
    {
        let mut steps_done = 0;

        while steps_done < max && should_continue(self) {
            match self.step() {
                Ok(_) => {}
                Err(e) => {
                    return StepManyResult {
                        steps: steps_done,
                        exception: Some(e),
                    }
                }
            };
            steps_done += 1;
        }

        StepManyResult {
            steps: steps_done,
            exception: None,
        }
    }

    /// Install a program and set the program counter to its start.
    pub fn setup_boot(
        &mut self,
        program: &Program<ML>,
        initrd: Option<&[u8]>,
        mode: mode::Mode,
    ) -> Result<(), MachineError> {
        // Write program to main memory and point the PC at its start
        for (addr, data) in program.segments.iter() {
            self.bus.write_all(*addr, data)?;
        }
        self.hart.pc.write(program.entrypoint);

        // Set booting Hart ID (a0) to 0
        self.hart.xregisters.write(registers::a0, 0);

        // Load the initial program into memory
        let initrd_addr = program
            .segments
            .iter()
            .map(|(base, data)| base + data.len() as Address)
            .max()
            .unwrap_or(bus::start_of_main_memory::<ML>());

        // Write initial ramdisk, if any
        let (dtb_addr, initrd) = if let Some(initrd) = initrd {
            self.bus.write_all(initrd_addr, initrd)?;
            let length = initrd.len() as u64;
            let dtb_options = devicetree::InitialRamDisk {
                start: initrd_addr,
                length,
            };
            let dtb_addr = initrd_addr + length;
            (dtb_addr, Some(dtb_options))
        } else {
            (initrd_addr, None)
        };

        // Write device tree to memory
        let fdt = devicetree::generate::<ML>(initrd)?;
        self.bus.write_all(dtb_addr, fdt.as_slice())?;

        // Point DTB boot argument (a1) at the written device tree
        self.hart.xregisters.write(registers::a1, dtb_addr);

        // Start in supervisor mode
        self.hart.mode.write(mode);

        // Make sure to forward all exceptions and interrupts to supervisor mode
        self.hart
            .csregisters
            .write(csregisters::CSRegister::medeleg, !0);
        self.hart
            .csregisters
            .write(csregisters::CSRegister::mideleg, !0);

        Ok(())
    }
}

/// Errors that occur from interacting with the [MachineState]
#[derive(Debug, derive_more::Display, derive_more::From, thiserror::Error)]
pub enum MachineError {
    #[display(fmt = "Address out of bounds")]
    AddressError(OutOfBounds),
    DeviceTreeError(vm_fdt::Error),
}

#[cfg(test)]
mod tests {
    use super::{
        backend::tests::{test_determinism, ManagerFor},
        bus,
        bus::main_memory::tests::T1K,
        MachineState, MachineStateLayout,
    };
    use crate::{
        backend_test, create_backend, create_state,
        machine_state::{
            csregisters::{xstatus, CSRegister},
            mode::Mode,
            registers::{a1, a2, t0, t2},
        },
        traps::{EnvironException, Exception, Interrupt, TrapContext},
    };
    use proptest::{prop_assert_eq, proptest};
    use twiddle::Twiddle;

    backend_test!(test_machine_state_reset, F, {
        test_determinism::<F, MachineStateLayout<T1K>, _>(|space| {
            let mut machine: MachineState<T1K, ManagerFor<'_, F, MachineStateLayout<T1K>>> =
                MachineState::bind(space);
            machine.reset();
        });
    });

    backend_test!(test_step, F, {
        proptest!(|(
            pc_addr_offset in 0..250_u64,
            jump_addr in 0..250_u64,
        )| {
            let mut backend = create_backend!(MachineStateLayout<T1K>, F);
            let mut state = create_state!(MachineState, MachineStateLayout<T1K>, F, backend, T1K);

            let init_pc_addr = bus::start_of_main_memory::<T1K>() + pc_addr_offset * 4;
            let jump_addr = bus::start_of_main_memory::<T1K>() + jump_addr * 4;

            // Instruction which performs a unit op (AUIPC with t0)
            const T2_ENC: u64 = 0b0_0111; // x7

            state.hart.pc.write(init_pc_addr);
            state.hart.xregisters.write(a1, T2_ENC << 7 | 0b0010111);
            state.hart.xregisters.write(a2, init_pc_addr);
            state.run_sw(0, a2, a1).expect("Storing instruction should succeed");
            state.step().expect("should not raise trap to EE");
            prop_assert_eq!(state.hart.xregisters.read(t2), init_pc_addr);
            prop_assert_eq!(state.hart.pc.read(), init_pc_addr + 4);

            // Instruction which updates pc by returning an address
            // t3 = jump_addr, (JALR imm=0, rs1=t3, rd=t0)
            const T0_ENC: u64 = 0b00101; // x5
            const OP_JALR: u64 = 0b110_0111;
            const F3_0: u64 = 0b000;

            state.hart.pc.write(init_pc_addr);
            state.hart.xregisters.write(a1, T2_ENC << 15 | F3_0 << 12 | T0_ENC << 7 | OP_JALR);
            state.hart.xregisters.write(a2, init_pc_addr);
            state.run_sw(0, a2, a1).expect("Storing instruction should succeed");
            state.hart.xregisters.write(t2, jump_addr);
            state.step().expect("should not raise trap to EE");
            prop_assert_eq!(state.hart.xregisters.read(t0), init_pc_addr + 4);
            prop_assert_eq!(state.hart.pc.read(), jump_addr);
        });
    });

    backend_test!(test_step_env_exc, F, {
        proptest!(|(
            pc_addr_offset in 0..200_u64,
            stvec_offset in 10..20_u64,
            mtvec_offset in 25..35_u64,
        )| {
            let mut backend = create_backend!(MachineStateLayout<T1K>, F);
            let mut state = create_state!(MachineState, MachineStateLayout<T1K>, F, backend, T1K);

            let init_pc_addr = bus::start_of_main_memory::<T1K>() + pc_addr_offset * 4;
            let stvec_addr = init_pc_addr + 4 * stvec_offset;
            let mtvec_addr = init_pc_addr + 4 * mtvec_offset;

            const ECALL: u64 = 0b111_0011;

            // stvec is in DIRECT mode
            state.hart.csregisters.write(CSRegister::stvec, stvec_addr);
            // mtvec is in VECTORED mode
            state.hart.csregisters.write(CSRegister::mtvec, mtvec_addr | 1);

            // TEST: Raise ECALL exception ==>> environment exception
            state.hart.mode.write(Mode::Machine);
            state.hart.pc.write(init_pc_addr);
            state.hart.xregisters.write(a1, ECALL);
            state.hart.xregisters.write(a2, init_pc_addr);
            state.run_sw(0, a2, a1).expect("Storing instruction should succeed");
            let e = state.step()
                .expect_err("should raise Environment Exception");
            assert_eq!(e, EnvironException::EnvCallFromMMode);
            prop_assert_eq!(state.hart.pc.read(), init_pc_addr);
        });
    });

    backend_test!(test_step_exc_mm, F, {
        proptest!(|(
            pc_addr_offset in 0..200_u64,
            mtvec_offset in 25..35_u64,
        )| {
            let mut backend = create_backend!(MachineStateLayout<T1K>, F);
            let mut state = create_state!(MachineState, MachineStateLayout<T1K>, F, backend, T1K);

            let init_pc_addr = bus::start_of_main_memory::<T1K>() + pc_addr_offset * 4;
            let mtvec_addr = init_pc_addr + 4 * mtvec_offset;
            const EBREAK: u64 = 1 << 20 | 0b111_0011;

            // mtvec is in VECTORED mode
            state.hart.csregisters.write(CSRegister::mtvec, mtvec_addr | 1);

            // TEST: Raise exception, (and no interrupt before) take trap from M-mode to M-mode
            // (test no delegation takes place, even if delegation is on, traps never lower privilege)
            let medeleg_val = 1 << Exception::IllegalInstruction.exception_code() |
                1 << Exception::EnvCallFromSMode.exception_code() |
                1 << Exception::EnvCallFromMMode.exception_code() |
                1 << Exception::Breakpoint.exception_code();
            state.hart.mode.write(Mode::Machine);
            state.hart.pc.write(init_pc_addr);
            state.hart.csregisters.write(CSRegister::medeleg, medeleg_val);

            state.hart.xregisters.write(a1, EBREAK);
            state.hart.xregisters.write(a2, init_pc_addr);
            state.run_sw(0, a2, a1).expect("Storing instruction should succeed");
            state.step().expect("should not raise environment exception");
            // pc should be mtvec_addr since exceptions aren't offset (by VECTORED mode)
            // even in VECTORED mode, only interrupts
            let mstatus = state.hart.csregisters.read(CSRegister::mstatus);
            assert_eq!(state.hart.mode.read(), Mode::Machine);
            assert_eq!(state.hart.pc.read(), mtvec_addr);
            assert_eq!(xstatus::get_MPP(mstatus), xstatus::MPPValue::Machine);
            assert_eq!(state.hart.csregisters.read(CSRegister::mepc), init_pc_addr);
            assert_eq!(state.hart.csregisters.read(CSRegister::mcause), 3);
        });
    });

    backend_test!(test_step_inter_mm, F, {
        proptest!(|(
            pc_addr_offset in 0..200_u64,
            stvec_offset in 10..20_u64,
            mtvec_offset in 25..35_u64,
        )| {
            // Raise interrupt, take trap from M-mode to M-mode
            // (test delegation doesn't take place even if enabled by registers)
            let mut backend = create_backend!(MachineStateLayout<T1K>, F);
            let mut state = create_state!(MachineState, MachineStateLayout<T1K>, F, backend, T1K);

            let init_pc_addr = bus::start_of_main_memory::<T1K>() + pc_addr_offset * 4;
            let stvec_addr = init_pc_addr + 4 * stvec_offset;
            let mtvec_addr = init_pc_addr + 4 * mtvec_offset;
            const AUIPC: u64 = 0b001_0111;

            // stvec is in BASE mode
            state.hart.csregisters.write(CSRegister::stvec, stvec_addr);
            // mtvec is in VECTORED mode
            state.hart.csregisters.write(CSRegister::mtvec, mtvec_addr | 1);
            let mie = 0
                .set_bit(Interrupt::MachineExternal.exception_code() as usize, true)
                .set_bit(Interrupt::MachineTimer.exception_code() as usize, true);
            let mip = 0
                .set_bit(Interrupt::SupervisorSoftware.exception_code() as usize, true)
                .set_bit(Interrupt::MachineExternal.exception_code() as usize, true)
                .set_bit(Interrupt::SupervisorTimer.exception_code() as usize, true);
            let mideleg_val = 1 << Interrupt::MachineExternal.exception_code() |
                1 << Interrupt::SupervisorSoftware.exception_code() |
                1 << Interrupt::MachineTimer.exception_code();
            // The interrupt taken is MachineExternal. theoretically, mideleg delegates this trap,
            // but because it is a machine interupt it will still be handled in M-mode.
            state.hart.csregisters.write(CSRegister::mip, mip);
            state.hart.csregisters.write(CSRegister::mie, mie);
            state.hart.csregisters.write(CSRegister::mideleg, mideleg_val);
            let mstatus = xstatus::set_MIE(state.hart.csregisters.read(CSRegister::mstatus), true);
            state.hart.csregisters.write(CSRegister::mstatus, mstatus);
            state.hart.mode.write(Mode::Machine);
            // it doesn't really matter where the pc is since we will take an interrupt
            state.hart.pc.write(init_pc_addr);
            state.hart.xregisters.write(a1, AUIPC);
            state.hart.xregisters.write(a2, mtvec_addr + 4 * 11);
            state.run_sw(0, a2, a1).expect("Storing instruction should succeed");
            state.step().expect("should not raise environment exception");
            let mstatus = state.hart.csregisters.read(CSRegister::mstatus);
            assert_eq!(state.hart.mode.read(), Mode::Machine);
            assert_eq!(state.hart.pc.read(), mtvec_addr + 4 * 11 + 4);
            // We are coming from the interrupt handler actually
            assert_eq!(xstatus::get_MPP(mstatus), xstatus::MPPValue::Machine);
            assert_eq!(state.hart.csregisters.read(CSRegister::mepc), init_pc_addr);
            assert_eq!(state.hart.csregisters.read(CSRegister::mcause), 1 << 63 | 11);
            assert_eq!(state.hart.csregisters.read(CSRegister::mip), mip ^ 1 << 11);
        });
    });

    backend_test!(test_step_exc_us, F, {
        proptest!(|(
            pc_addr_offset in 0..200_u64,
            stvec_offset in 10..20_u64,
        )| {
            // Raise exception, take trap from U-mode to S-mode (test delegation takes place)
            let mut backend = create_backend!(MachineStateLayout<T1K>, F);
            let mut state = create_state!(MachineState, MachineStateLayout<T1K>, F, backend, T1K);

            let init_pc_addr = bus::start_of_main_memory::<T1K>() + pc_addr_offset * 4;
            let stvec_addr = init_pc_addr + 4 * stvec_offset;

            // stvec is in VECTORED mode
            state.hart.csregisters.write(CSRegister::stvec, stvec_addr | 1);

            let bad_address = bus::start_of_main_memory::<T1K>() - (pc_addr_offset + 10) * 4;
            let medeleg_val = 1 << Exception::IllegalInstruction.exception_code() |
                1 << Exception::EnvCallFromSMode.exception_code() |
                1 << Exception::EnvCallFromMMode.exception_code() |
                1 << Exception::InstructionAccessFault(bad_address).exception_code();
            state.hart.mode.write(Mode::User);
            state.hart.pc.write(bad_address);
            state.hart.csregisters.write(CSRegister::medeleg, medeleg_val);

            state.step().expect("should not raise environment exception");
            // pc should be stvec_addr since exceptions aren't offsetted
            // even in VECTORED mode, only interrupts
            let mstatus = state.hart.csregisters.read(CSRegister::mstatus);
            assert_eq!(state.hart.mode.read(), Mode::Supervisor);
            assert_eq!(state.hart.pc.read(), stvec_addr);
            assert_eq!(xstatus::get_SPP(mstatus), xstatus::SPPValue::User);
            assert_eq!(state.hart.csregisters.read(CSRegister::sepc), bad_address);
            assert_eq!(state.hart.csregisters.read(CSRegister::scause), 1);
        });
    });

    backend_test!(test_step_trap_usm, F, {
        proptest!(|(
            pc_addr_offset in 0..200_u64,
            stvec_offset in 10..20_u64,
            mtvec_offset in 25..35_u64,
        )| {
            // TEST: Raise interrupt from U to S, then exception from S to M
            // this will be tested by reading mcause and scause, the interrupt will set scause
            // and the exception will set mcause
            // interrupt delegation will delegate the SEI, but not MSI, testing the priority as well
            let mut backend = create_backend!(MachineStateLayout<T1K>, F);
            let mut state = create_state!(MachineState, MachineStateLayout<T1K>, F, backend, T1K);

            let init_pc_addr = bus::start_of_main_memory::<T1K>() + pc_addr_offset * 4;
            let stvec_addr = init_pc_addr + 4 * stvec_offset;
            let mtvec_addr = init_pc_addr + 4 * mtvec_offset;
            const AUIPC: u64 = 0b001_0111;
            const T1_ENC: u64 = 0b110;
            const T2_ENC: u64 = 0b111;
            const OP_LB: u64 = 0b000_0011;
            // stvec is in VECTORED mode
            state.hart.csregisters.write(CSRegister::stvec, stvec_addr | 1);
            // mtvec is in BASE mode
            state.hart.csregisters.write(CSRegister::mtvec, mtvec_addr);
            let mie = 0
                .set_bit(Interrupt::SupervisorExternal.exception_code() as usize, true)
                .set_bit(Interrupt::MachineSoftware.exception_code() as usize, true);
            let mip = 0
                .set_bit(Interrupt::SupervisorExternal.exception_code() as usize, true)
                .set_bit(Interrupt::SupervisorTimer.exception_code() as usize, true);
            let mideleg_val = 1 << Interrupt::SupervisorExternal.exception_code() |
                1 << Interrupt::MachineExternal.exception_code() |
                1 << Interrupt::MachineTimer.exception_code();
            let medeleg_val = 1 << Exception::IllegalInstruction.exception_code() |
                1 << Exception::EnvCallFromSMode.exception_code() |
                1 << Exception::EnvCallFromMMode.exception_code();
            state.hart.csregisters.write(CSRegister::mideleg, mideleg_val);
            state.hart.csregisters.write(CSRegister::medeleg, medeleg_val);
            state.hart.csregisters.write(CSRegister::mip, mip);
            state.hart.csregisters.write(CSRegister::mie, mie);
            let mstatus = state.hart.csregisters.read(CSRegister::mstatus);
            let mstatus = xstatus::set_SIE(mstatus, true);
            state.hart.csregisters.write(CSRegister::mstatus, mstatus);
            state.hart.mode.write(Mode::User);
            state.hart.pc.write(init_pc_addr);

            // normally this instruction shouldnt raise exception
            state.hart.xregisters.write(a1, AUIPC);
            state.hart.xregisters.write(a2, init_pc_addr);
            state.run_sw(0, a2, a1).expect("Storing instruction should succeed");
            // but the interrupt will jump here which will further raise an exception
            // this is LB instruction trying to load into t1 the byte at t2 (0x333).
            // this instruction raises LoadAccessFault(0x333)
            // load in t2 the address 0x333
            state.hart.xregisters.write(t2, 0x333);
            state.hart.xregisters.write(a1, T2_ENC << 15 | T1_ENC << 7 | OP_LB);
            state.hart.xregisters.write(a2, stvec_addr + 4 * 9);
            state.run_sw(0, a2, a1).expect("Storing instruction should succeed");

            state.step().expect("should not raise environment exception");
            // pc should be mtvec_addr since exceptions aren't offset (by VECTORED mode)
            // even in VECTORED mode, only interrupts
            let mstatus = state.hart.csregisters.read(CSRegister::mstatus);
            assert_eq!(state.hart.mode.read(), Mode::Machine);
            assert_eq!(state.hart.pc.read(), mtvec_addr);
            // We are coming from the interrupt handler actually
            assert_eq!(xstatus::get_MPP(mstatus), xstatus::MPPValue::Supervisor);
            assert_eq!(xstatus::get_SPP(mstatus), xstatus::SPPValue::User);
            assert_eq!(state.hart.csregisters.read(CSRegister::sepc), init_pc_addr);
            assert_eq!(state.hart.csregisters.read(CSRegister::mepc), stvec_addr + 4 * 9);
            assert_eq!(state.hart.csregisters.read(CSRegister::scause), 1 << 63 | 9);
            assert_eq!(state.hart.csregisters.read(CSRegister::mcause), 5);
            assert_eq!(state.hart.csregisters.read(CSRegister::mtval), 0x333);
            assert_eq!(state.hart.csregisters.read(CSRegister::stval), 0);
            assert_eq!(state.hart.csregisters.read(CSRegister::mip), mip ^ 1 << 9);
        });
    });
}
