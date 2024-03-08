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
    machine_state::{
        bus::{main_memory, Address, Addressable, Bus, OutOfBounds},
        hart_state::{HartState, HartStateLayout},
    },
    parser::{instruction::Instr, parse},
    program::Program,
    state_backend as backend,
    traps::{EnvironException, Exception, Interrupt},
};

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
        fetch_result.map_err(|_: OutOfBounds| Exception::InstructionAccessFault)
    }

    /// Advance [`MachineState`] by executing an [`Instr`]
    fn run_instr(&mut self, instr: Instr) -> Result<ProgramCounterUpdate, Exception> {
        use ProgramCounterUpdate::{Add, Set};

        match instr {
            // TODO: Make macros also write the => part
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

    /// Return the current interrupt (with highest priority) to be handled
    /// or None if there isn't any available
    fn get_pending_interrupt(&self) -> Option<Interrupt> {
        // TODO: https://gitlab.com/tezos/tezos/-/issues/7009
        None
    }

    /// Handle interrupts (also known as asynchronous exceptions)
    /// by taking a trap if an interrupt is available.
    ///
    /// If trap is taken, return new address of program counter,
    /// otherwise if no trap is taken, return [`None`]
    fn trap_interrupt(&self, _i: Interrupt) -> Option<Address> {
        // TODO: https://gitlab.com/tezos/tezos/-/issues/7009
        todo!()
    }

    /// Handle an [`Exception`] if one was risen during execution
    /// of an instruction (also known as synchronous exception) by taking a trap
    fn trap_exception(&mut self, exception: Exception) -> Result<Address, EnvironException> {
        if let Ok(exc) = EnvironException::try_from(&exception) {
            return Err(exc);
        }

        // TODO: https://gitlab.com/tezos/tezos/-/issues/7010
        todo!("Trap handling for synchronous exception not implemented")
    }

    /// Take an interrupt if available, and then
    /// perform precisely one [`Instr`] and handle the traps that may rise as a side-effect.
    ///
    /// The [`Err`] case represents an [`Exception`] to be handled by
    /// the execution environment, narrowed down by the type [`EnvironException`].
    pub fn step(&mut self) -> Result<(), EnvironException> {
        // Try to take an interrupt if available, and then
        // obtain the pc for the next instruction to be executed
        let instr_pc = self
            .get_pending_interrupt()
            // interrupt pending, try to take it
            .and_then(|inter| self.trap_interrupt(inter))
            // interrupt not pending or not taken
            // (e.g. some interrupt bits are off)
            .unwrap_or_else(|| self.hart.pc.read());

        // Fetch & run the instruction
        let instr_result = self.run_instr_at(instr_pc);

        // Take exception if needed
        let pc_update = match instr_result {
            Err(exc) => ProgramCounterUpdate::Set(self.trap_exception(exc)?),
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
        mode: mode::Mode,
    ) -> Result<(), MachineError> {
        // Write program to main memory and point the PC at its start
        for (addr, data) in program.segments.iter() {
            self.bus.write_all(*addr, data)?;
        }
        self.hart.pc.write(program.entrypoint);

        // Set booting Hart ID (a0) to 0
        self.hart.xregisters.write(registers::a0, 0);

        // TODO: https://gitlab.com/tezos/tezos/-/issues/6941
        // Write device tree
        let dtb_addr = program
            .segments
            .iter()
            .map(|(base, data)| base + data.len() as Address)
            .max()
            .unwrap_or(bus::start_of_main_memory::<ML>());

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
#[derive(Debug, Clone, derive_more::From)]
pub enum MachineError {
    AddressError(OutOfBounds),
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
        machine_state::registers::{a1, a2, t0, t2},
    };
    use proptest::{prop_assert_eq, proptest};

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

            // TEST: Instruction which performs a unit op (AUIPC with t0)
            const T2_ENC: u64 = 0b0_0111; // x7

            state.hart.pc.write(init_pc_addr);
            state.hart.xregisters.write(a1, T2_ENC << 7 | 0b0010111);
            state.hart.xregisters.write(a2, init_pc_addr);
            state.run_sw(0, a2, a1).expect("Storing instruction should succeed");
            state.step().expect("should not raise trap to EE");
            prop_assert_eq!(state.hart.xregisters.read(t2), init_pc_addr);
            prop_assert_eq!(state.hart.pc.read(), init_pc_addr + 4);

            // TEST: Instruction which updates pc by returning an address
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

            // XXX: Implement the following synchronous exception tests when
            //      `step_trap_exception` and `step_trap_interrupt` are implemented
            // TEST: Raise exception, take trap from M-mode to M-mode (test no delegation takes place, even if delegation is on for other codes)

            // TEST: Raise exception, take trap from M-mode to S-mode (test delegation should not lower privilege)

            // TEST: Raise exception, take trap from S-mode to S-mode (test delegation takes place)

            // TEST: Raise exception, take trap from S-mode to M-mode (test that for an exception code i, delegation does not take place, but it is active for others)
        });
    });
}
