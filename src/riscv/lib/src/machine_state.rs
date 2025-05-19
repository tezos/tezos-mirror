// SPDX-FileCopyrightText: 2023-2025 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pub mod block_cache;
mod cache_layouts;
pub(crate) mod csregisters;
pub(crate) mod hart_state;
pub mod instruction;
pub mod memory;
pub(crate) mod mode;
pub(crate) mod registers;
pub(crate) mod reservation_set;

#[cfg(test)]
extern crate proptest;

use std::ops::Bound;

use block_cache::BlockCache;
use block_cache::block::Block;
pub use cache_layouts::CacheLayouts;
pub use cache_layouts::DefaultCacheLayouts;
pub use cache_layouts::TestCacheLayouts;
use csregisters::values::CSRValue;
use hart_state::HartState;
use hart_state::HartStateLayout;
use instruction::Instruction;
use memory::Address;
use memory::BadMemoryAccess;
use memory::Memory;
use memory::MemoryConfig;
use memory::MemoryGovernanceError;

use crate::bits::u64;
use crate::devicetree;
use crate::parser::instruction::Instr;
use crate::parser::instruction::InstrCacheable;
use crate::parser::instruction::InstrUncacheable;
use crate::parser::instruction::InstrWidth;
use crate::parser::is_compressed;
use crate::parser::parse_compressed_instruction;
use crate::parser::parse_uncompressed_instruction;
use crate::program::Program;
use crate::range_utils::bound_saturating_sub;
use crate::range_utils::less_than_bound;
use crate::range_utils::unwrap_bound;
use crate::state::NewState;
use crate::state_backend as backend;
use crate::state_backend::ManagerReadWrite;
use crate::traps::EnvironException;
use crate::traps::Exception;

/// Layout for the machine 'run state' - which contains everything required for the running of
/// instructions.
pub type MachineCoreStateLayout<MC> = (HartStateLayout, <MC as MemoryConfig>::Layout);

/// The part of the machine state required to run (almost all) instructions.
///
/// Namely, things that are required to fetch instructions, but not run them, should be placed
/// elsewhere in [`MachineState`].
///
/// Certain instructions (e.g. `FENCE.I` may invalidate other parts of the state, but this are
/// small in number).
pub struct MachineCoreState<MC: memory::MemoryConfig, M: backend::ManagerBase> {
    pub hart: HartState<M>,
    pub main_memory: MC::State<M>,
}

impl<MC: memory::MemoryConfig, M: backend::ManagerBase> MachineCoreState<MC, M> {
    /// Handle an [`Exception`] if one was risen during execution
    /// of an instruction (also known as synchronous exception) by taking a trap.
    ///
    /// Return the new address of the program counter, becoming the address of a trap handler.
    /// Throw [`EnvironException`] if the exception needs to be treated by the execution enviroment.
    pub(crate) fn address_on_exception(
        &mut self,
        exception: Exception,
        current_pc: Address,
    ) -> Result<Address, EnvironException>
    where
        M: backend::ManagerReadWrite,
    {
        if let Ok(exc) = EnvironException::try_from(&exception) {
            // We need to commit the PC before returning because the caller (e.g.
            // [step]) doesn't commit it eagerly.
            self.hart.pc.write(current_pc);

            return Err(exc);
        }

        Ok(self.hart.take_trap(exception, current_pc))
    }

    #[inline]
    fn handle_step_result(
        &mut self,
        instr_pc: Address,
        result: Result<ProgramCounterUpdate<Address>, Exception>,
    ) -> Result<Address, EnvironException>
    where
        M: backend::ManagerReadWrite,
    {
        let pc_update = match result {
            Err(exc) => ProgramCounterUpdate::Set(self.address_on_exception(exc, instr_pc)?),
            Ok(upd) => upd,
        };

        // Update program couter
        let pc = match pc_update {
            ProgramCounterUpdate::Set(address) => address,
            ProgramCounterUpdate::Next(width) => instr_pc + width as u64,
        };

        self.hart.pc.write(pc);

        Ok(pc)
    }

    /// Bind the machine state to the given allocated space.
    pub fn bind(space: backend::AllocatedOf<MachineCoreStateLayout<MC>, M>) -> Self {
        Self {
            hart: HartState::bind(space.0),
            main_memory: MC::bind(space.1),
        }
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub fn struct_ref<'a, F: backend::FnManager<backend::Ref<'a, M>>>(
        &'a self,
    ) -> backend::AllocatedOf<MachineCoreStateLayout<MC>, F::Output> {
        (
            self.hart.struct_ref::<F>(),
            MC::struct_ref::<_, F>(&self.main_memory),
        )
    }

    /// Reset the machine state.
    pub fn reset(&mut self)
    where
        M: backend::ManagerReadWrite,
    {
        self.hart.reset(memory::FIRST_ADDRESS);
        self.main_memory.reset();
    }

    /// Advance [`MachineState`] by executing an [`InstrCacheable`].
    fn run_instr_cacheable(
        &mut self,
        instr: &InstrCacheable,
    ) -> Result<ProgramCounterUpdate<Address>, Exception>
    where
        M: backend::ManagerReadWrite,
    {
        Instruction::from(instr).run(self)
    }
}

impl<MC: memory::MemoryConfig, M: backend::ManagerBase> NewState<M> for MachineCoreState<MC, M> {
    fn new(manager: &mut M) -> Self
    where
        M: backend::ManagerAlloc,
    {
        Self {
            hart: HartState::new(manager),
            main_memory: NewState::new(manager),
        }
    }
}

impl<MC: memory::MemoryConfig, M: backend::ManagerClone> Clone for MachineCoreState<MC, M> {
    fn clone(&self) -> Self {
        Self {
            hart: self.hart.clone(),
            main_memory: self.main_memory.clone(),
        }
    }
}

/// Layout for the machine state - everything required to fetch & run instructions.
pub type MachineStateLayout<MC, CL> = (
    MachineCoreStateLayout<MC>,
    <CL as CacheLayouts>::BlockCacheLayout,
);

/// The machine state contains everything required to fetch & run instructions.
pub struct MachineState<
    MC: memory::MemoryConfig,
    CL: CacheLayouts,
    B: Block<MC, M>,
    M: backend::ManagerBase,
> {
    pub core: MachineCoreState<MC, M>,
    pub block_cache: BlockCache<CL::BlockCacheLayout, B, MC, M>,
}

impl<MC: memory::MemoryConfig, CL: CacheLayouts, B: Block<MC, M> + Clone, M: backend::ManagerClone>
    Clone for MachineState<MC, CL, B, M>
{
    fn clone(&self) -> Self {
        Self {
            core: self.core.clone(),
            block_cache: self.block_cache.clone(),
        }
    }
}

/// How to modify the program counter
#[derive(Debug, PartialEq)]
pub enum ProgramCounterUpdate<AddressRepr> {
    /// Jump to a fixed address
    Set(AddressRepr),
    /// Offset to the next instruction by current instruction width
    Next(InstrWidth),
}

/// Result type when running multiple steps at a time with [`MachineState::step_max`]
#[derive(Debug)]
pub struct StepManyResult<E> {
    pub steps: usize,
    pub error: Option<E>,
}

/// Runs a syscall instruction (ecall, ebreak)
macro_rules! run_syscall_instr {
    ($state: ident, $run_fn: ident) => {{ Err($state.hart.$run_fn()) }};
}

/// Runs a no-arguments instruction (wfi, fenceI)
macro_rules! run_no_args_instr {
    ($state: ident, $instr: ident, $run_fn: ident) => {{
        $state.$run_fn();
        Ok(Next($instr.width()))
    }};
}

impl<MC: memory::MemoryConfig, CL: CacheLayouts, B: Block<MC, M>, M: backend::ManagerBase>
    MachineState<MC, CL, B, M>
{
    /// Allocate a new machine state.
    pub fn new(manager: &mut M, block_builder: B::BlockBuilder) -> Self
    where
        M: backend::ManagerAlloc,
    {
        Self {
            core: MachineCoreState::new(manager),
            block_cache: BlockCache::new(manager, block_builder),
        }
    }

    /// Bind the block cache to the given allocated state and the given [block builder].
    ///
    /// [block builder]: Block::BlockBuilder
    pub fn bind(
        space: backend::AllocatedOf<MachineStateLayout<MC, CL>, M>,
        block_builder: B::BlockBuilder,
    ) -> Self
    where
        M::ManagerRoot: ManagerReadWrite,
    {
        Self {
            core: MachineCoreState::bind(space.0),
            block_cache: BlockCache::bind(space.1, block_builder),
        }
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub fn struct_ref<'a, F: backend::FnManager<backend::Ref<'a, M>>>(
        &'a self,
    ) -> backend::AllocatedOf<MachineStateLayout<MC, CL>, F::Output> {
        (
            self.core.struct_ref::<F>(),
            self.block_cache.struct_ref::<F>(),
        )
    }

    /// Reset the machine state.
    pub fn reset(&mut self)
    where
        M: backend::ManagerReadWrite,
    {
        self.core.reset();
        self.block_cache.reset();
    }

    /// Fetch the 16 bits of an instruction at the given physical address.
    #[inline(always)]
    fn fetch_instr_halfword(&self, phys_addr: Address) -> Result<u16, Exception>
    where
        M: backend::ManagerRead,
    {
        // TODO: RV-527: Enforce that `Instruction` came from executable part of memory.
        // The link to an `Instruction` is too weak. We want statically-typed guarantees that these
        // bytes are only used for instructions.
        self.core
            .main_memory
            .read_exec(phys_addr)
            .map_err(|_: BadMemoryAccess| Exception::InstructionAccessFault(phys_addr))
    }

    /// Fetch instruction from the address given by program counter
    /// The spec stipulates translation is performed for each byte respectively.
    /// However, we assume the `raw_pc` is 2-byte aligned.
    #[inline]
    fn fetch_instr(&mut self, addr: Address) -> Result<Instr, Exception>
    where
        M: backend::ManagerReadWrite,
    {
        let first_halfword = self.fetch_instr_halfword(addr)?;

        // The reasons to provide the second half in the lambda is
        // because those bytes may be inaccessible or may trigger an exception when read.
        // Hence we can't read all 4 bytes eagerly.
        let instr = if is_compressed(first_halfword) {
            let instr = parse_compressed_instruction(first_halfword);
            if let Instr::Cacheable(instr) = instr {
                let instr = Instruction::from(&instr);
                self.block_cache.push_instr_compressed(addr, instr);
            }

            instr
        } else {
            let next_addr = addr + 2;
            let upper = self.fetch_instr_halfword(next_addr)?;

            let combined = ((upper as u32) << 16) | (first_halfword as u32);
            let instr = parse_uncompressed_instruction(combined);
            if let Instr::Cacheable(instr) = instr {
                let instr = Instruction::from(&instr);
                self.block_cache.push_instr_uncompressed(addr, instr);
            }

            instr
        };

        Ok(instr)
    }

    /// Advance [`MachineState`] by executing an [`InstrUncacheable`].
    fn run_instr_uncacheable(
        &mut self,
        instr: &InstrUncacheable,
    ) -> Result<ProgramCounterUpdate<Address>, Exception>
    where
        M: backend::ManagerReadWrite,
    {
        use ProgramCounterUpdate::Next;

        let core = &mut self.core;

        match instr {
            InstrUncacheable::Fence(args) => {
                self.core.run_fence(args.pred, args.succ);
                Ok(Next(instr.width()))
            }
            InstrUncacheable::FenceTso(_args) => Err(Exception::IllegalInstruction),
            InstrUncacheable::Ebreak => run_syscall_instr!(core, run_ebreak),

            // Privileged instructions
            // Trap-Return
            InstrUncacheable::Mret => Err(Exception::IllegalInstruction),
            InstrUncacheable::Sret => Err(Exception::IllegalInstruction),
            // Currently not implemented instruction (part of Smrnmi extension)
            InstrUncacheable::Mnret => Err(Exception::IllegalInstruction),
            // Supervisor Memory-Management
            InstrUncacheable::SFenceVma { asid, vaddr } => {
                self.core.run_sfence_vma(*asid, *vaddr)?;
                Ok(ProgramCounterUpdate::Next(instr.width()))
            }

            // RV32C compressed instructions
            InstrUncacheable::CEbreak => run_syscall_instr!(core, run_cebreak),

            // Zifencei instructions
            InstrUncacheable::FenceI => run_no_args_instr!(self, instr, run_fencei),
        }
    }

    /// Advance [`MachineState`] by executing an [`Instr`]
    fn run_instr(&mut self, instr: &Instr) -> Result<ProgramCounterUpdate<Address>, Exception>
    where
        M: backend::ManagerReadWrite,
    {
        match instr {
            Instr::Cacheable(i) => self.core.run_instr_cacheable(i),
            Instr::Uncacheable(i) => self.run_instr_uncacheable(i),
        }
    }

    /// Fetch & run the instruction located at address `instr_pc`
    fn run_instr_at(&mut self, addr: Address) -> Result<ProgramCounterUpdate<Address>, Exception>
    where
        M: backend::ManagerReadWrite,
    {
        self.fetch_instr(addr)
            .and_then(|instr| self.run_instr(&instr))
    }

    /// Take an interrupt if available, and then
    /// perform precisely one [`Instr`] and handle the traps that may rise as a side-effect.
    ///
    /// The [`Err`] case represents an [`Exception`] to be handled by
    /// the execution environment, narrowed down by the type [`EnvironException`].
    #[inline]
    pub fn step(&mut self) -> Result<(), EnvironException>
    where
        M: ManagerReadWrite,
    {
        self.step_max_inner(&mut 0, 1)
    }

    fn step_max_inner(
        &mut self,
        steps: &mut usize,
        max_steps: usize,
    ) -> Result<(), EnvironException>
    where
        M: backend::ManagerReadWrite,
    {
        self.block_cache
            .complete_current_block(&mut self.core, steps, max_steps)?;

        'run: while *steps < max_steps {
            // Obtain the pc for the next instruction to be executed
            let instr_pc = self.core.hart.pc.read();

            let res = match self.block_cache.get_block(instr_pc) {
                Some(mut block) => {
                    block.run_block(&mut self.core, instr_pc, steps, max_steps)?;

                    continue 'run;
                }
                None => self.run_instr_at(instr_pc),
            };

            self.core.handle_step_result(instr_pc, res)?;
            *steps += 1;
        }

        Ok(())
    }

    /// Perform as many steps as the given `max_steps` bound allows. Returns the number of retired
    /// instructions.
    #[inline]
    pub fn step_max(&mut self, max_steps: Bound<usize>) -> StepManyResult<EnvironException>
    where
        M: backend::ManagerReadWrite,
    {
        let mut steps = 0;

        loop {
            match self.step_max_inner(&mut steps, unwrap_bound(max_steps)) {
                Ok(_) => {}
                Err(e) => {
                    return StepManyResult {
                        steps,
                        error: Some(e),
                    };
                }
            };

            if !less_than_bound(steps, max_steps) {
                break;
            }
        }

        StepManyResult { steps, error: None }
    }

    /// Similar to [`Self::step_max`] but lets the user handle environment exceptions inside the
    /// inner step loop.
    #[inline]
    pub fn step_max_handle<E>(
        &mut self,
        mut step_bounds: Bound<usize>,
        mut handle: impl FnMut(&mut Self, EnvironException) -> Result<bool, E>,
    ) -> StepManyResult<E>
    where
        M: backend::ManagerReadWrite,
    {
        let mut steps = 0usize;

        let error = loop {
            let result = self.step_max(step_bounds);

            steps = steps.saturating_add(result.steps);
            step_bounds = bound_saturating_sub(step_bounds, result.steps);

            match result.error {
                Some(cause) => {
                    // Raising the exception is not a completed step. Trying to handle it is.
                    // We don't have to check against `max_steps` because running the
                    // instruction that triggered the exception meant that `max_steps > 0`.
                    steps = steps.saturating_add(1);
                    step_bounds = bound_saturating_sub(step_bounds, 1);

                    match handle(self, cause) {
                        Ok(may_continue) => {
                            if !may_continue {
                                break None;
                            }
                        }

                        Err(error) => break Some(error),
                    }
                }
                None => break None,
            }
        };

        StepManyResult { steps, error }
    }

    /// Install a program and set the program counter to its start.
    pub fn setup_boot(
        &mut self,
        program: &Program<MC>,
        initrd: Option<&[u8]>,
    ) -> Result<(), MachineError>
    where
        M: backend::ManagerReadWrite,
    {
        // Reset hart state & set pc to entrypoint
        self.core.hart.reset(program.entrypoint);
        // Write program to main memory and point the PC at its start
        for (addr, data) in program.segments.iter() {
            self.core.main_memory.write_all(*addr, data)?;
        }

        // Set booting Hart ID (a0) to 0
        self.core.hart.xregisters.write(registers::a0, 0);

        // Load the initial program into memory
        let initrd_addr = program
            .segments
            .iter()
            .map(|(base, data)| base + data.len() as Address)
            .max()
            .unwrap_or(memory::FIRST_ADDRESS);

        // Write initial ramdisk, if any
        let (dtb_addr, initrd) = if let Some(initrd) = initrd {
            self.core.main_memory.write_all(initrd_addr, initrd)?;
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
        let fdt = devicetree::generate::<MC>(initrd)?;
        self.core.main_memory.write_all(dtb_addr, fdt.as_slice())?;

        // Point DTB boot argument (a1) at the written device tree
        self.core.hart.xregisters.write(registers::a1, dtb_addr);

        // Make sure to forward all exceptions and interrupts to supervisor mode
        self.core
            .hart
            .csregisters
            .write(csregisters::CSRegister::medeleg, CSRValue::from(!0u64));
        self.core
            .hart
            .csregisters
            .write(csregisters::CSRegister::mideleg, CSRValue::from(!0u64));

        Ok(())
    }
}

/// Errors that occur from interacting with the [MachineState]
#[derive(Debug, thiserror::Error)]
pub enum MachineError {
    #[error("Error while accessing memory")]
    MemoryError(#[from] BadMemoryAccess),

    #[error("Error while govering memory")]
    MemoryGovernanceError(#[from] MemoryGovernanceError),

    #[error("Device tree error: {0}")]
    DeviceTreeError(#[from] vm_fdt::Error),

    #[error("Memory too small to properly configure the machine")]
    MemoryTooSmall,
}

#[cfg(test)]
mod tests {
    use std::ops::Bound;

    use proptest::prop_assert_eq;
    use proptest::proptest;

    use super::MachineState;
    use super::MachineStateLayout;
    use super::block_cache::block::Interpreted;
    use super::block_cache::block::InterpretedBlockBuilder;
    use super::instruction::Instruction;
    use super::instruction::OpCode;
    use super::instruction::tagged_instruction::TaggedArgs;
    use super::instruction::tagged_instruction::TaggedInstruction;
    use super::instruction::tagged_instruction::TaggedRegister;
    use super::registers::XRegister;
    use crate::backend_test;
    use crate::bits::u16;
    use crate::default::ConstDefault;
    use crate::machine_state::DefaultCacheLayouts;
    use crate::machine_state::TestCacheLayouts;
    use crate::machine_state::csregisters::CSRRepr;
    use crate::machine_state::csregisters::CSRegister;
    use crate::machine_state::csregisters::xstatus;
    use crate::machine_state::csregisters::xstatus::MStatus;
    use crate::machine_state::memory;
    use crate::machine_state::memory::M1M;
    use crate::machine_state::memory::M4K;
    use crate::machine_state::memory::M8K;
    use crate::machine_state::memory::Memory;
    use crate::machine_state::memory::PAGE_SIZE;
    use crate::machine_state::registers::a0;
    use crate::machine_state::registers::a1;
    use crate::machine_state::registers::nz;
    use crate::machine_state::registers::ra;
    use crate::machine_state::registers::t0;
    use crate::machine_state::registers::t1;
    use crate::machine_state::registers::t2;
    use crate::parser::XRegisterParsed::*;
    use crate::parser::instruction::Instr;
    use crate::parser::instruction::InstrCacheable;
    use crate::parser::instruction::InstrWidth;
    use crate::parser::instruction::SBTypeArgs;
    use crate::parser::instruction::SplitITypeArgs;
    use crate::parser::parse_block;
    use crate::state_backend::FnManagerIdent;
    use crate::state_backend::test_helpers::TestBackendFactory;
    use crate::state_backend::test_helpers::assert_eq_struct;
    use crate::state_backend::test_helpers::copy_via_serde;
    use crate::traps::EnvironException;
    use crate::traps::Exception;
    use crate::traps::TrapContext;

    backend_test!(test_step, F, {
        let state = MachineState::<M4K, DefaultCacheLayouts, Interpreted<M4K, _>, _>::new(
            &mut F::manager(),
            InterpretedBlockBuilder,
        );

        let state_cell = std::cell::RefCell::new(state);

        proptest!(|(
            pc_addr_offset in 0..250_u64,
            jump_addr in 0..250_u64,
        )| {
            let mut state = state_cell.borrow_mut();
            state.reset();

            let init_pc_addr = memory::FIRST_ADDRESS + pc_addr_offset * 4;
            let jump_addr = memory::FIRST_ADDRESS + jump_addr * 4;

            // Instruction which performs a unit op (AUIPC with t0)
            const T2_ENC: u32 = 0b0_0111; // x7

            state.core.hart.pc.write(init_pc_addr);
            state.core.main_memory.write_instruction_unchecked::<u32>(init_pc_addr, (T2_ENC << 7) |
                0b0010111).expect("Storing instruction should succeed");
            state.step().expect("should not raise trap to EE");
            prop_assert_eq!(state.core.hart.xregisters.read(t2), init_pc_addr);
            prop_assert_eq!(state.core.hart.pc.read(), init_pc_addr + 4);

            // Instruction which updates pc by returning an address
            // t3 = jump_addr, (JALR imm=0, rs1=t3, rd=t0)
            const T0_ENC: u32 = 0b00101; // x5
            const OP_JALR: u32 = 0b110_0111;
            const F3_0: u32 = 0b000;

            state.core.hart.pc.write(init_pc_addr);
            state.core.main_memory.write_instruction_unchecked(init_pc_addr, (T2_ENC << 15) | (F3_0 << 12) | (T0_ENC << 7) | OP_JALR).unwrap();
            state.core.hart.xregisters.write(t2, jump_addr);

            // Since we've written a new instruction to the init_pc_addr - we need to
            // invalidate the instruction cache.
            state.run_fencei();

            state.step().expect("should not raise trap to EE");
            prop_assert_eq!(state.core.hart.xregisters.read(t0), init_pc_addr + 4);
            prop_assert_eq!(state.core.hart.pc.read(), jump_addr);
        });
    });

    backend_test!(test_step_env_exc, F, {
        let state = MachineState::<M4K, DefaultCacheLayouts, Interpreted<M4K, _>, _>::new(
            &mut F::manager(),
            InterpretedBlockBuilder,
        );

        let state_cell = std::cell::RefCell::new(state);

        proptest!(|(
            pc_addr_offset in 0..200_u64,
            stvec_offset in 10..20_u64,
            mtvec_offset in 25..35_u64,
        )| {
            let mut state = state_cell.borrow_mut();
            state.reset();

            let init_pc_addr = memory::FIRST_ADDRESS + pc_addr_offset * 4;
            let stvec_addr = init_pc_addr + 4 * stvec_offset;
            let mtvec_addr = init_pc_addr + 4 * mtvec_offset;

            const ECALL: u32 = 0b111_0011;

            // stvec is in DIRECT mode
            state.core.hart.csregisters.write(CSRegister::stvec, stvec_addr);
            // mtvec is in VECTORED mode
            state.core.hart.csregisters.write(CSRegister::mtvec, mtvec_addr | 1);

            // TEST: Raise ECALL exception ==>> environment exception
            state.core.hart.pc.write(init_pc_addr);
            state.core.main_memory.write_instruction_unchecked(init_pc_addr, ECALL).unwrap();
            let e = state.step()
                .expect_err("should raise Environment Exception");
            assert_eq!(e, EnvironException::EnvCall);
            prop_assert_eq!(state.core.hart.pc.read(), init_pc_addr);
        });
    });

    backend_test!(test_step_exc_us, F, {
        let state = MachineState::<M4K, DefaultCacheLayouts, Interpreted<M4K, _>, _>::new(
            &mut F::manager(),
            InterpretedBlockBuilder,
        );

        let state_cell = std::cell::RefCell::new(state);

        proptest!(|(
            pc_addr_offset in 0..200_u64,
            stvec_offset in 10..20_u64,
        )| {
            // Raise exception, take trap from U-mode to S-mode (test delegation takes place)
            let mut state = state_cell.borrow_mut();
            state.reset();

            let init_pc_addr = memory::FIRST_ADDRESS + pc_addr_offset * 4;
            let stvec_addr = init_pc_addr + 4 * stvec_offset;

            // stvec is in VECTORED mode
            state.core.hart.csregisters.write(CSRegister::stvec, stvec_addr | 1);

            let bad_address = memory::FIRST_ADDRESS.wrapping_sub((pc_addr_offset + 10) * 4);
            let medeleg_val = (1 << Exception::IllegalInstruction.exception_code()) | (1 << Exception::InstructionAccessFault(bad_address).exception_code());
            state.core.hart.pc.write(bad_address);
            state.core.hart.csregisters.write(CSRegister::medeleg, medeleg_val);

            state.step().expect("should not raise environment exception");
            // pc should be stvec_addr since exceptions aren't offsetted
            // even in VECTORED mode, only interrupts
            let mstatus: MStatus = state.core.hart.csregisters.read(CSRegister::mstatus);
            assert_eq!(state.core.hart.pc.read(), stvec_addr);
            assert_eq!(mstatus.spp(), xstatus::SPPValue::User);
            assert_eq!(state.core.hart.csregisters.read::<CSRRepr>(CSRegister::sepc), bad_address);
            assert_eq!(state.core.hart.csregisters.read::<CSRRepr>(CSRegister::scause), 1);
        });
    });

    // Test that the machine state does not behave differently when potential ephermeral state is
    // reset that may impact instruction caching.
    backend_test!(test_instruction_cache, F, {
        // Instruction that writes the value in t1 to the address t0.
        const I_WRITE_T1_TO_ADDRESS_T0: u32 = 0b0011000101010000000100011;
        assert_eq!(parse_block(&I_WRITE_T1_TO_ADDRESS_T0.to_le_bytes()), [
            Instr::Cacheable(InstrCacheable::Sw(SBTypeArgs {
                rs1: t0,
                rs2: t1,
                imm: 0,
            }))
        ]);

        // Instruction that loads 6 into t2.
        const I_LOAD_6_INTO_T2: u32 = 0b11000000000001110010011;
        assert_eq!(parse_block(&I_LOAD_6_INTO_T2.to_le_bytes()), [
            Instr::Cacheable(InstrCacheable::Addi(SplitITypeArgs {
                rd: NonZero(nz::t2),
                rs1: X0,
                imm: 6,
            }))
        ]);

        // Instruction that loads 5 into t2.
        const I_LOAD_5_INTO_T2: u32 = 0b10100000000001110010011;
        assert_eq!(parse_block(&I_LOAD_5_INTO_T2.to_le_bytes()), [
            Instr::Cacheable(InstrCacheable::Addi(SplitITypeArgs {
                rd: NonZero(nz::t2),
                rs1: X0,
                imm: 5,
            }))
        ]);

        type LocalLayout = MachineStateLayout<M4K, TestCacheLayouts>;

        type BlockRunner<F> = Interpreted<M4K, <F as TestBackendFactory>::Manager>;

        type LocalMachineState<F> =
            MachineState<M4K, TestCacheLayouts, BlockRunner<F>, <F as TestBackendFactory>::Manager>;

        // Configure the machine state.
        let base_state = {
            let mut state = MachineState::<M4K, TestCacheLayouts, BlockRunner<F>, _>::new(
                &mut F::manager(),
                InterpretedBlockBuilder,
            );

            state.reset();
            state.core.main_memory.set_all_readable_writeable();

            let start_ram = memory::FIRST_ADDRESS;

            // Write the instructions to the beginning of the main memory and point the program
            // counter at the first instruction.
            state.core.hart.pc.write(start_ram);
            let instrs: [u8; 8] = [I_WRITE_T1_TO_ADDRESS_T0, I_LOAD_5_INTO_T2]
                .into_iter()
                .flat_map(u32::to_le_bytes)
                .collect::<Vec<_>>()
                .try_into()
                .unwrap();
            state
                .core
                .main_memory
                .write_all(start_ram, &instrs)
                .unwrap();

            // Configure the machine in such a way that the first instruction will override the
            // second instruction. The second instruction behaves differently.
            let address_to_write = start_ram + 4;
            state.core.hart.xregisters.write(t0, address_to_write);
            state
                .core
                .hart
                .xregisters
                .write(t1, I_LOAD_6_INTO_T2 as u64);

            state
        };

        // Perform 2 steps consecutively in one backend.
        let state = {
            let mut state = LocalMachineState::<F>::bind(
                copy_via_serde::<LocalLayout, _, _>(&base_state.struct_ref::<FnManagerIdent>()),
                InterpretedBlockBuilder,
            );

            state.step().unwrap();
            state.step().unwrap();

            state
        };

        // Perform 2 steps separately in another backend by re-binding the state between steps.
        let alt_state = {
            let alt_state = {
                let mut state = LocalMachineState::<F>::bind(
                    copy_via_serde::<LocalLayout, _, _>(&base_state.struct_ref::<FnManagerIdent>()),
                    InterpretedBlockBuilder,
                );
                state.step().unwrap();
                state
            };

            {
                let mut state = LocalMachineState::<F>::bind(
                    copy_via_serde::<LocalLayout, _, _>(&alt_state.struct_ref::<FnManagerIdent>()),
                    InterpretedBlockBuilder,
                );
                state.step().unwrap();
                state
            }
        };

        // The two backends should have the same state.
        assert_eq!(
            state.core.hart.xregisters.read(t2),
            alt_state.core.hart.xregisters.read(t2)
        );

        assert_eq_struct(
            &state.struct_ref::<FnManagerIdent>(),
            &alt_state.struct_ref::<FnManagerIdent>(),
        );
    });

    // Ensure that cloning the machine state does not result in a stack overflow
    backend_test!(test_machine_state_cloneable, F, {
        let state = MachineState::<M1M, DefaultCacheLayouts, Interpreted<M1M, _>, _>::new(
            &mut F::manager(),
            InterpretedBlockBuilder,
        );

        let second = state.clone();

        assert_eq_struct(
            &state.struct_ref::<FnManagerIdent>(),
            &second.struct_ref::<FnManagerIdent>(),
        );
    });

    backend_test!(test_block_cache_crossing_pages_creates_new_block, F, {
        let mut state = MachineState::<M8K, DefaultCacheLayouts, Interpreted<M8K, _>, _>::new(
            &mut F::manager(),
            InterpretedBlockBuilder,
        );

        let uncompressed_bytes = 0x5307b3;

        let uncompressed = Instruction::try_from(TaggedInstruction {
            opcode: OpCode::Add,
            args: TaggedArgs {
                rd: nz::a5.into(),
                rs1: nz::t1.into(),
                rs2: nz::t0.into(),
                ..TaggedArgs::DEFAULT
            },
        })
        .unwrap();

        let start_ram = memory::FIRST_ADDRESS;

        // Write the instructions to the beginning of the main memory and point the program
        // counter at the first instruction.
        let phys_addr = start_ram + PAGE_SIZE.get() - 6;

        state.core.hart.pc.write(phys_addr);

        for offset in 0..3 {
            state
                .core
                .main_memory
                .write_instruction_unchecked(phys_addr + offset * 4, uncompressed_bytes)
                .unwrap();
        }

        state.step_max(Bound::Included(3));

        assert!(state.block_cache.get_block(phys_addr).is_some());
        assert_eq!(
            vec![uncompressed],
            state.block_cache.get_block_instr(phys_addr)
        );

        assert!(state.block_cache.get_block(phys_addr + 4).is_none());

        assert!(state.block_cache.get_block(phys_addr + 8).is_some());
        assert_eq!(
            vec![uncompressed],
            state.block_cache.get_block_instr(phys_addr + 8)
        );
    });

    // The block cache & instruction cache have separate cache invalidation/overwriting.
    // While running with an unbounded number of steps this is fine, there is a potential for
    // divergence when the number of steps to be run is less than the block size of the next block.
    //
    // Namely:
    // - we create a block at A, which jumps to address B
    // - fetch instructions at B overwrites some of the _instruction cache_ entries of Block(A),
    //   without affecting the block itself, and then jumps back to A
    // - we don't have enough steps to execute Block(A) directly, so fall back to fetch/parse/run
    // - A different set of instructions is executed than would be if there were enough steps
    //   remaining to run Block(A).
    backend_test!(test_block_cache_instruction_cache_determinism, F, {
        // We make the wrap-around of the instruction cache much smaller than the wrap-around
        // of the block-cache, allowing instruction cache entries to be overwritten without
        // overwriting the related block-cache entries.
        //
        // Note that we are jumping 2 * CACHE_SIZE to achieve the wrap-around, as non u16-aligned
        // addresses cannot be cached, so CACHE_SIZE corresponds to 2*CACHE_SIZE addresses.
        let auipc_bytes: u32 = 0x517;
        let cj_bytes: u16 = 0xa8b5;

        let block_a = [
            // Store current instruction counter
            Instruction::try_from(TaggedInstruction {
                opcode: OpCode::AddImmediateToPC,
                args: TaggedArgs {
                    rd: nz::a0.into(),
                    imm: 0,
                    rs1: TaggedRegister::X(XRegister::x1),
                    rs2: TaggedRegister::X(XRegister::x1),
                    ..TaggedArgs::DEFAULT
                },
            })
            .unwrap(),
            // this represents a CJ instruction.
            Instruction::try_from(TaggedInstruction {
                opcode: OpCode::J,
                args: TaggedArgs {
                    imm: 128 - 4,
                    width: InstrWidth::Compressed,
                    rd: nz::ra.into(),
                    rs1: nz::ra.into(),
                    rs2: nz::ra.into(),
                    ..TaggedArgs::DEFAULT
                },
            })
            .unwrap(),
        ];

        // InstrCacheable::CJ(CJTypeArgs { imm: 1024 })
        let overwrite_bytes = 0xa101;

        let clui_bytes: u16 = 0x65a9;
        let addiw_bytes: u32 = 0x1015859b;
        let csw_bytes: u16 = 0xc10c;
        let jalr_bytes: u32 = 0x50067;
        let block_b = [
            Instruction::try_from(TaggedInstruction {
                opcode: OpCode::Li,
                args: TaggedArgs {
                    rd: nz::a1.into(),
                    imm: (u16::bits_subset(overwrite_bytes, 15, 12) as i64) << 12,
                    rs1: nz::ra.into(),
                    rs2: nz::ra.into(),
                    width: InstrWidth::Compressed,
                    ..TaggedArgs::DEFAULT
                },
            })
            .unwrap(),
            Instruction::try_from(TaggedInstruction {
                opcode: OpCode::AddWordImmediate,
                args: TaggedArgs {
                    rd: nz::a1.into(),
                    rs1: a1.into(),
                    imm: u16::bits_subset(overwrite_bytes, 11, 0) as i64,
                    rs2: TaggedRegister::X(XRegister::x1),
                    ..TaggedArgs::DEFAULT
                },
            })
            .unwrap(),
            Instruction::try_from(TaggedInstruction {
                opcode: OpCode::X32Store,
                args: TaggedArgs {
                    rd: ra.into(),
                    rs1: a0.into(),
                    rs2: a1.into(),
                    imm: 0,
                    width: InstrWidth::Compressed,
                    ..TaggedArgs::DEFAULT
                },
            })
            .unwrap(),
            Instruction::try_from(TaggedInstruction {
                opcode: OpCode::Jr,
                args: TaggedArgs {
                    rd: nz::ra.into(),
                    rs1: nz::a0.into(),
                    ..TaggedArgs::DEFAULT
                },
            })
            .unwrap(),
        ];

        let phys_addr = memory::FIRST_ADDRESS;

        let block_b_addr = phys_addr + 128;

        let mut state = MachineState::<M8K, DefaultCacheLayouts, Interpreted<M8K, _>, _>::new(
            &mut F::manager(),
            InterpretedBlockBuilder,
        );

        state.core.main_memory.set_all_readable_writeable();

        // Write the instructions to the beginning of the main memory and point the program
        // counter at the first instruction.
        state.core.hart.pc.write(phys_addr);

        // block A
        state
            .core
            .main_memory
            .write_instruction_unchecked(phys_addr, auipc_bytes)
            .unwrap();
        state
            .core
            .main_memory
            .write_instruction_unchecked(phys_addr + 4, cj_bytes)
            .unwrap();

        // block B
        state
            .core
            .main_memory
            .write_instruction_unchecked(block_b_addr, clui_bytes)
            .unwrap();
        state
            .core
            .main_memory
            .write_instruction_unchecked(block_b_addr + 2, addiw_bytes)
            .unwrap();
        state
            .core
            .main_memory
            .write_instruction_unchecked(block_b_addr + 6, csw_bytes)
            .unwrap();
        state
            .core
            .main_memory
            .write_instruction_unchecked(block_b_addr + 8, jalr_bytes)
            .unwrap();

        // Overwritten jump dest
        state
            .core
            .main_memory
            .write_instruction_unchecked(phys_addr + 1024, overwrite_bytes)
            .unwrap();

        state.step_max(Bound::Included(block_a.len()));
        assert_eq!(block_b_addr, state.core.hart.pc.read());

        state.step_max(Bound::Included(block_b.len()));
        assert_eq!(phys_addr, state.core.hart.pc.read());

        assert!(state.block_cache.get_block(phys_addr).is_some());
        assert_eq!(
            block_a.as_slice(),
            state.block_cache.get_block_instr(phys_addr).as_slice()
        );

        assert!(state.block_cache.get_block(block_b_addr).is_some());
        assert_eq!(
            block_b.as_slice(),
            state.block_cache.get_block_instr(block_b_addr).as_slice()
        );

        let mut state_step = state.clone();

        let result = state.step_max(Bound::Included(block_a.len()));
        assert_eq!(result.steps, block_a.len());

        for _ in 0..block_a.len() {
            let result = state_step.step_max(Bound::Included(1));
            assert_eq!(result.steps, 1);
        }

        assert_eq_struct(
            &state.struct_ref::<FnManagerIdent>(),
            &state_step.struct_ref::<FnManagerIdent>(),
        );
    });
}
