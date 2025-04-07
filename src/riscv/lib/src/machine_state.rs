// SPDX-FileCopyrightText: 2023-2025 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

pub mod address_translation;
pub mod block_cache;
mod cache_layouts;
pub mod csregisters;
pub mod hart_state;
pub mod instruction;
pub mod main_memory;
pub mod mode;
pub mod registers;
pub mod reservation_set;

#[cfg(test)]
extern crate proptest;

use crate::{
    bits::u64,
    devicetree,
    parser::{
        instruction::{Instr, InstrCacheable, InstrUncacheable, InstrWidth},
        is_compressed, parse_compressed_instruction, parse_uncompressed_instruction,
    },
    program::Program,
    range_utils::{bound_saturating_sub, less_than_bound, unwrap_bound},
    state_backend::{self as backend, ManagerReadWrite},
    traps::{EnvironException, Exception},
};
pub use address_translation::AccessType;
use address_translation::{
    PAGE_SIZE,
    translation_cache::{TranslationCache, TranslationCacheLayout},
};
use block_cache::{
    BlockCache,
    bcall::{BCall, Block},
};
pub use cache_layouts::{CacheLayouts, DefaultCacheLayouts, TestCacheLayouts};
use csregisters::CSRegister;
use csregisters::{CSRRepr, values::CSRValue};
use hart_state::{HartState, HartStateLayout};
use instruction::Instruction;
use main_memory::{Address, MainMemory, OutOfBounds};
use mode::Mode;
use std::ops::Bound;

/// Layout for the machine 'run state' - which contains everything required for the running of
/// instructions.
pub type MachineCoreStateLayout<ML> = (HartStateLayout, ML, TranslationCacheLayout);

/// The part of the machine state required to run (almost all) instructions.
///
/// Namely, things that are required to fetch instructions, but not run them, should be placed
/// elsewhere in [`MachineState`].
///
/// Certain instructions (e.g. `FENCE.I` may invalidate other parts of the state, but this are
/// small in number).
pub struct MachineCoreState<ML: main_memory::MainMemoryLayout, M: backend::ManagerBase> {
    pub hart: HartState<M>,
    pub main_memory: MainMemory<ML, M>,
    pub translation_cache: TranslationCache<M>,
}

impl<ML: main_memory::MainMemoryLayout, M: backend::ManagerClone> Clone
    for MachineCoreState<ML, M>
{
    fn clone(&self) -> Self {
        Self {
            hart: self.hart.clone(),
            main_memory: self.main_memory.clone(),
            translation_cache: self.translation_cache.clone(),
        }
    }
}

/// Layout for the machine state - everything required to fetch & run instructions.
pub type MachineStateLayout<ML, CL> = (
    MachineCoreStateLayout<ML>,
    <CL as CacheLayouts>::BlockCacheLayout<ML>,
);

/// The machine state contains everything required to fetch & run instructions.
pub struct MachineState<
    ML: main_memory::MainMemoryLayout,
    CL: CacheLayouts,
    B: Block<ML, M>,
    M: backend::ManagerBase,
> {
    pub core: MachineCoreState<ML, M>,
    pub block_cache: BlockCache<CL::BlockCacheLayout<ML>, B, ML, M>,
}

impl<
    ML: main_memory::MainMemoryLayout,
    CL: CacheLayouts,
    B: Block<ML, M> + Clone,
    M: backend::ManagerClone,
> Clone for MachineState<ML, CL, B, M>
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
pub enum ProgramCounterUpdate {
    /// Jump to a fixed address
    Set(Address),
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

/// Runs a xret instruction (mret, sret, mnret)
macro_rules! run_xret_instr {
    ($state: ident, $run_fn: ident) => {{ $state.hart.$run_fn().map(Set) }};
}

/// Runs a no-arguments instruction (wfi, fenceI)
macro_rules! run_no_args_instr {
    ($state: ident, $instr: ident, $run_fn: ident) => {{
        $state.$run_fn();
        Ok(Next($instr.width()))
    }};
}

impl<ML: main_memory::MainMemoryLayout, M: backend::ManagerBase> MachineCoreState<ML, M> {
    /// Handle an [`Exception`] if one was risen during execution
    /// of an instruction (also known as synchronous exception) by taking a trap.
    ///
    /// Return the new address of the program counter, becoming the address of a trap handler.
    /// Throw [`EnvironException`] if the exception needs to be treated by the execution enviroment.
    fn address_on_exception(
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
        result: Result<ProgramCounterUpdate, Exception>,
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
    pub fn bind(space: backend::AllocatedOf<MachineCoreStateLayout<ML>, M>) -> Self {
        Self {
            hart: HartState::bind(space.0),
            main_memory: MainMemory::bind(space.1),
            translation_cache: TranslationCache::bind(space.2),
        }
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub fn struct_ref<'a, F: backend::FnManager<backend::Ref<'a, M>>>(
        &'a self,
    ) -> backend::AllocatedOf<MachineCoreStateLayout<ML>, F::Output> {
        (
            self.hart.struct_ref::<F>(),
            self.main_memory.struct_ref::<F>(),
            self.translation_cache.struct_ref::<F>(),
        )
    }

    /// Reset the machine state.
    pub fn reset(&mut self)
    where
        M: backend::ManagerReadWrite,
    {
        self.hart.reset(main_memory::FIRST_ADDRESS);
        self.main_memory.reset();
        self.translation_cache.reset();
    }

    /// Advance [`MachineState`] by executing an [`InstrCacheable`].
    fn run_instr_cacheable(
        &mut self,
        instr: &InstrCacheable,
    ) -> Result<ProgramCounterUpdate, Exception>
    where
        M: backend::ManagerReadWrite,
    {
        Instruction::from(instr).run(self)
    }
}

impl<ML: main_memory::MainMemoryLayout, CL: CacheLayouts, B: Block<ML, M>, M: backend::ManagerBase>
    MachineState<ML, CL, B, M>
{
    /// Bind the block cache to the given allocated state and the given [block builder].
    ///
    /// [block builder]: Block::BlockBuilder
    pub fn bind(
        space: backend::AllocatedOf<MachineStateLayout<ML, CL>, M>,
        block_builder: B::BlockBuilder,
    ) -> Self {
        Self {
            core: MachineCoreState::bind(space.0),
            block_cache: BlockCache::bind(space.1, block_builder),
        }
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub fn struct_ref<'a, F: backend::FnManager<backend::Ref<'a, M>>>(
        &'a self,
    ) -> backend::AllocatedOf<MachineStateLayout<ML, CL>, F::Output> {
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

    /// Translate an instruction address.
    #[inline]
    fn translate_instr_address(
        &mut self,
        mode: Mode,
        satp: CSRRepr,
        virt_addr: Address,
    ) -> Result<Address, Exception>
    where
        M: backend::ManagerReadWrite,
    {
        // Chapter: P:S-ISA-1.9 & P:M-ISA-1.16
        // If mtval is written with a nonzero value when a
        // breakpoint, address-misaligned, access-fault, or page-fault exception
        // occurs on an instruction fetch, load, or store, then mtval will contain the
        // faulting virtual address.

        let phys_addr = if let Some(phys_addr) = self.core.translation_cache.try_translate(
            mode,
            satp,
            AccessType::Instruction,
            virt_addr,
        ) {
            phys_addr
        } else {
            let phys_addr = self.core.translate_with_prefetch(
                mode,
                satp,
                virt_addr,
                AccessType::Instruction,
            )?;

            self.core.translation_cache.cache_translation(
                mode,
                satp,
                AccessType::Instruction,
                virt_addr,
                phys_addr,
            );

            phys_addr
        };

        Ok(phys_addr)
    }

    /// Fetch the 16 bits of an instruction at the given physical address.
    #[inline(always)]
    fn fetch_instr_halfword(&self, phys_addr: Address) -> Result<u16, Exception>
    where
        M: backend::ManagerRead,
    {
        self.core
            .main_memory
            .read(phys_addr)
            .map_err(|_: OutOfBounds| Exception::InstructionAccessFault(phys_addr))
    }

    /// Fetch instruction from the address given by program counter
    /// The spec stipulates translation is performed for each byte respectively.
    /// However, we assume the `raw_pc` is 2-byte aligned.
    #[inline]
    fn fetch_instr(
        &mut self,
        mode: Mode,
        satp: CSRRepr,
        virt_addr: Address,
        phys_addr: Address,
    ) -> Result<Instr, Exception>
    where
        M: backend::ManagerReadWrite,
    {
        let first_halfword = self.fetch_instr_halfword(phys_addr)?;

        // The reasons to provide the second half in the lambda is
        // because those bytes may be inaccessible or may trigger an exception when read.
        // Hence we can't read all 4 bytes eagerly.
        let instr = if is_compressed(first_halfword) {
            let instr = parse_compressed_instruction(first_halfword);
            if let Instr::Cacheable(instr) = instr {
                let instr = Instruction::from(&instr);
                self.block_cache.push_instr_compressed(phys_addr, instr);
            }

            instr
        } else {
            let upper = {
                let next_addr = phys_addr + 2;

                // Optimization to skip an extra address translation lookup:
                // If the last 2 bytes of the instruction are in the same page
                // as the first 2 bytes, then we already know the translated address
                let phys_addr = if next_addr % PAGE_SIZE == 0 {
                    self.translate_instr_address(mode, satp, virt_addr + 2)?
                } else {
                    next_addr
                };

                self.fetch_instr_halfword(phys_addr)?
            };

            let combined = (upper as u32) << 16 | (first_halfword as u32);
            let instr = parse_uncompressed_instruction(combined);
            if let Instr::Cacheable(instr) = instr {
                let instr = Instruction::from(&instr);
                self.block_cache.push_instr_uncompressed(phys_addr, instr);
            }

            instr
        };

        Ok(instr)
    }

    /// Advance [`MachineState`] by executing an [`InstrUncacheable`].
    fn run_instr_uncacheable(
        &mut self,
        instr: &InstrUncacheable,
    ) -> Result<ProgramCounterUpdate, Exception>
    where
        M: backend::ManagerReadWrite,
    {
        use ProgramCounterUpdate::{Next, Set};

        let core = &mut self.core;

        match instr {
            InstrUncacheable::Fence(args) => {
                self.core.run_fence(args.pred, args.succ);
                Ok(Next(instr.width()))
            }
            InstrUncacheable::FenceTso(_args) => Err(Exception::IllegalInstruction),
            InstrUncacheable::Ecall => run_syscall_instr!(core, run_ecall),
            InstrUncacheable::Ebreak => run_syscall_instr!(core, run_ebreak),

            // Privileged instructions
            // Trap-Return
            InstrUncacheable::Mret => run_xret_instr!(core, run_mret),
            InstrUncacheable::Sret => run_xret_instr!(core, run_sret),
            // Currently not implemented instruction (part of Smrnmi extension)
            InstrUncacheable::Mnret => Err(Exception::IllegalInstruction),
            // Interrupt-Management
            InstrUncacheable::Wfi => run_no_args_instr!(core, instr, run_wfi),
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
    fn run_instr(&mut self, instr: &Instr) -> Result<ProgramCounterUpdate, Exception>
    where
        M: backend::ManagerReadWrite,
    {
        match instr {
            Instr::Cacheable(i) => self.core.run_instr_cacheable(i),
            Instr::Uncacheable(i) => self.run_instr_uncacheable(i),
        }
    }

    /// Fetch & run the instruction located at address `instr_pc`
    fn run_instr_at(
        &mut self,
        current_mode: Mode,
        satp: CSRRepr,
        instr_pc: Address,
        phys_addr: Address,
    ) -> Result<ProgramCounterUpdate, Exception>
    where
        M: backend::ManagerReadWrite,
    {
        self.fetch_instr(current_mode, satp, instr_pc, phys_addr)
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
            let current_mode = self.core.hart.mode.read();

            // Obtain the pc for the next instruction to be executed
            let instr_pc = self.core.hart.pc.read();
            let satp: CSRRepr = self.core.hart.csregisters.read(CSRegister::satp);

            let res = match self.translate_instr_address(current_mode, satp, instr_pc) {
                Err(e) => Err(e),
                Ok(phys_addr) => match self.block_cache.get_block(phys_addr) {
                    Some(block) if block.num_instr() <= max_steps - *steps => {
                        block.run_block(&mut self.core, instr_pc, steps)?;

                        continue 'run;
                    }
                    Some(_) => {
                        self.block_cache.run_block_partial(
                            &mut self.core,
                            phys_addr,
                            steps,
                            max_steps,
                        )?;

                        continue 'run;
                    }
                    None => self.run_instr_at(current_mode, satp, instr_pc, phys_addr),
                },
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
        program: &Program<ML>,
        initrd: Option<&[u8]>,
        mode: mode::Mode,
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
            .unwrap_or(main_memory::FIRST_ADDRESS);

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
        let fdt = devicetree::generate::<ML>(initrd)?;
        self.core.main_memory.write_all(dtb_addr, fdt.as_slice())?;

        // Point DTB boot argument (a1) at the written device tree
        self.core.hart.xregisters.write(registers::a1, dtb_addr);

        // Start in supervisor mode
        self.core.hart.mode.write(mode);

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
#[derive(Debug, derive_more::From, thiserror::Error)]
pub enum MachineError {
    #[error("Address out of bounds")]
    AddressError(OutOfBounds),

    #[error("Device tree error: {0}")]
    DeviceTreeError(vm_fdt::Error),
}

#[cfg(test)]
mod tests {
    use super::{
        MachineState, MachineStateLayout,
        block_cache::bcall::{Interpreted, InterpretedBlockBuilder},
        instruction::{
            Instruction, OpCode,
            tagged_instruction::{TaggedArgs, TaggedInstruction, TaggedRegister},
        },
        main_memory::tests::T1K,
        registers::XRegister,
    };
    use crate::{
        backend_test,
        bits::{Bits64, FixedWidthBits, u16},
        create_state,
        default::ConstDefault,
        machine_state::{
            DefaultCacheLayouts, TestCacheLayouts,
            address_translation::{
                PAGE_SIZE,
                pte::{PPNField, PageTableEntry},
            },
            csregisters::{
                CSRRepr, CSRegister,
                satp::{Satp, TranslationAlgorithm},
                xstatus::{self, MStatus},
            },
            main_memory::{self, M1M, M8K},
            mode::Mode,
            registers::{a0, a1, a2, nz, t0, t1, t2, zero},
        },
        parser::{
            XRegisterParsed::*,
            instruction::{
                CIBNZTypeArgs, Instr, InstrCacheable, InstrWidth, SBTypeArgs, SplitITypeArgs,
            },
            parse_block,
        },
        state_backend::{
            FnManagerIdent,
            test_helpers::{TestBackendFactory, assert_eq_struct, copy_via_serde},
        },
        traps::{EnvironException, Exception, TrapContext},
    };
    use crate::{bits::u64, machine_state::main_memory::M1K};
    use proptest::{prop_assert_eq, proptest};
    use std::ops::Bound;

    backend_test!(test_step, F, {
        let state = create_state!(
            MachineState, 
            MachineStateLayout<T1K, DefaultCacheLayouts>, 
            F, 
            T1K, 
            DefaultCacheLayouts, 
            Interpreted<T1K, F::Manager>, || InterpretedBlockBuilder);

        let state_cell = std::cell::RefCell::new(state);

        proptest!(|(
            pc_addr_offset in 0..250_u64,
            jump_addr in 0..250_u64,
        )| {
            let mut state = state_cell.borrow_mut();
            state.reset();

            let init_pc_addr = main_memory::FIRST_ADDRESS + pc_addr_offset * 4;
            let jump_addr = main_memory::FIRST_ADDRESS + jump_addr * 4;

            // Instruction which performs a unit op (AUIPC with t0)
            const T2_ENC: u64 = 0b0_0111; // x7

            state.core.hart.pc.write(init_pc_addr);
            state.core.hart.xregisters.write(a1, T2_ENC << 7 | 0b0010111);
            state.core.hart.xregisters.write(a2, init_pc_addr);
            state.core.run_sw(0, a2, a1).expect("Storing instruction should succeed");
            state.step().expect("should not raise trap to EE");
            prop_assert_eq!(state.core.hart.xregisters.read(t2), init_pc_addr);
            prop_assert_eq!(state.core.hart.pc.read(), init_pc_addr + 4);

            // Instruction which updates pc by returning an address
            // t3 = jump_addr, (JALR imm=0, rs1=t3, rd=t0)
            const T0_ENC: u64 = 0b00101; // x5
            const OP_JALR: u64 = 0b110_0111;
            const F3_0: u64 = 0b000;

            state.core.hart.pc.write(init_pc_addr);
            state.core.hart.xregisters.write(a1, T2_ENC << 15 | F3_0 << 12 | T0_ENC << 7 | OP_JALR);
            state.core.hart.xregisters.write(a2, init_pc_addr);
            state.core.run_sw(0, a2, a1).expect("Storing instruction should succeed");
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
        let state = create_state!(
            MachineState, 
            MachineStateLayout<T1K, DefaultCacheLayouts>, 
            F, 
            T1K, 
            DefaultCacheLayouts, 
            Interpreted<T1K, F::Manager>, || InterpretedBlockBuilder);

        let state_cell = std::cell::RefCell::new(state);

        proptest!(|(
            pc_addr_offset in 0..200_u64,
            stvec_offset in 10..20_u64,
            mtvec_offset in 25..35_u64,
        )| {
            let mut state = state_cell.borrow_mut();
            state.reset();

            let init_pc_addr = main_memory::FIRST_ADDRESS + pc_addr_offset * 4;
            let stvec_addr = init_pc_addr + 4 * stvec_offset;
            let mtvec_addr = init_pc_addr + 4 * mtvec_offset;

            const ECALL: u64 = 0b111_0011;

            // stvec is in DIRECT mode
            state.core.hart.csregisters.write(CSRegister::stvec, stvec_addr);
            // mtvec is in VECTORED mode
            state.core.hart.csregisters.write(CSRegister::mtvec, mtvec_addr | 1);

            // TEST: Raise ECALL exception ==>> environment exception
            state.core.hart.mode.write(Mode::Machine);
            state.core.hart.pc.write(init_pc_addr);
            state.core.hart.xregisters.write(a1, ECALL);
            state.core.hart.xregisters.write(a2, init_pc_addr);
            state.core.run_sw(0, a2, a1).expect("Storing instruction should succeed");
            let e = state.step()
                .expect_err("should raise Environment Exception");
            assert_eq!(e, EnvironException::EnvCallFromMMode);
            prop_assert_eq!(state.core.hart.pc.read(), init_pc_addr);
        });
    });

    backend_test!(test_step_exc_mm, F, {
        let state = create_state!(
            MachineState, 
            MachineStateLayout<T1K, DefaultCacheLayouts>, 
            F, 
            T1K, 
            DefaultCacheLayouts, 
            Interpreted<T1K, F::Manager>, || InterpretedBlockBuilder);

        let state_cell = std::cell::RefCell::new(state);

        proptest!(|(
            pc_addr_offset in 0..200_u64,
            mtvec_offset in 25..35_u64,
        )| {
            let mut state = state_cell.borrow_mut();
            state.reset();

            let init_pc_addr = main_memory::FIRST_ADDRESS + pc_addr_offset * 4;
            let mtvec_addr = init_pc_addr + 4 * mtvec_offset;
            const EBREAK: u64 = 1 << 20 | 0b111_0011;

            // mtvec is in VECTORED mode
            state.core.hart.csregisters.write(CSRegister::mtvec, mtvec_addr | 1);

            // TEST: Raise exception, (and no interrupt before) take trap from M-mode to M-mode
            // (test no delegation takes place, even if delegation is on, traps never lower privilege)
            let medeleg_val = 1 << Exception::IllegalInstruction.exception_code() |
                1 << Exception::EnvCallFromSMode.exception_code() |
                1 << Exception::EnvCallFromMMode.exception_code() |
                1 << Exception::Breakpoint.exception_code();
            state.core.hart.mode.write(Mode::Machine);
            state.core.hart.pc.write(init_pc_addr);
            state.core.hart.csregisters.write(CSRegister::medeleg, medeleg_val);

            state.core.hart.xregisters.write(a1, EBREAK);
            state.core.hart.xregisters.write(a2, init_pc_addr);
            state.core.run_sw(0, a2, a1).expect("Storing instruction should succeed");
            state.step().expect("should not raise environment exception");
            // pc should be mtvec_addr since exceptions aren't offset (by VECTORED mode)
            // even in VECTORED mode, only interrupts
            let mstatus: MStatus = state.core.hart.csregisters.read(CSRegister::mstatus);
            assert_eq!(state.core.hart.mode.read(), Mode::Machine);
            assert_eq!(state.core.hart.pc.read(), mtvec_addr);
            assert_eq!(mstatus.mpp(), xstatus::MPPValue::Machine);
            assert_eq!(state.core.hart.csregisters.read::<CSRRepr>(CSRegister::mepc), init_pc_addr);
            assert_eq!(state.core.hart.csregisters.read::<CSRRepr>(CSRegister::mcause), 3);
        });
    });

    backend_test!(test_step_exc_us, F, {
        let state = create_state!(
            MachineState, 
            MachineStateLayout<T1K, DefaultCacheLayouts>, 
            F, 
            T1K, 
            DefaultCacheLayouts, 
            Interpreted<T1K, F::Manager>, || InterpretedBlockBuilder);

        let state_cell = std::cell::RefCell::new(state);

        proptest!(|(
            pc_addr_offset in 0..200_u64,
            stvec_offset in 10..20_u64,
        )| {
            // Raise exception, take trap from U-mode to S-mode (test delegation takes place)
            let mut state = state_cell.borrow_mut();
            state.reset();

            let init_pc_addr = main_memory::FIRST_ADDRESS + pc_addr_offset * 4;
            let stvec_addr = init_pc_addr + 4 * stvec_offset;

            // stvec is in VECTORED mode
            state.core.hart.csregisters.write(CSRegister::stvec, stvec_addr | 1);

            let bad_address = main_memory::FIRST_ADDRESS.wrapping_sub((pc_addr_offset + 10) * 4);
            let medeleg_val = 1 << Exception::IllegalInstruction.exception_code() |
                1 << Exception::EnvCallFromSMode.exception_code() |
                1 << Exception::EnvCallFromMMode.exception_code() |
                1 << Exception::InstructionAccessFault(bad_address).exception_code();
            state.core.hart.mode.write(Mode::User);
            state.core.hart.pc.write(bad_address);
            state.core.hart.csregisters.write(CSRegister::medeleg, medeleg_val);

            state.step().expect("should not raise environment exception");
            // pc should be stvec_addr since exceptions aren't offsetted
            // even in VECTORED mode, only interrupts
            let mstatus: MStatus = state.core.hart.csregisters.read(CSRegister::mstatus);
            assert_eq!(state.core.hart.mode.read(), Mode::Supervisor);
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

        type LocalLayout = MachineStateLayout<M1K, TestCacheLayouts>;

        type BlockRunner<F> = Interpreted<M1K, <F as TestBackendFactory>::Manager>;

        type LocalMachineState<F> =
            MachineState<M1K, TestCacheLayouts, BlockRunner<F>, <F as TestBackendFactory>::Manager>;

        // Configure the machine state.
        let base_state = {
            let mut state = create_state!(
                MachineState,
                LocalLayout,
                F,
                M1K,
                TestCacheLayouts,
                BlockRunner<F>,
                || InterpretedBlockBuilder
            );
            state.reset();

            let start_ram = main_memory::FIRST_ADDRESS;

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

    // Test that the machine state does not behave differently when potential ephermeral state is
    // reset that may impact instruction address translation caching.
    backend_test!(test_instruction_address_cache, F, {
        // Specify the physcal memory layout.
        let main_mem_addr = main_memory::FIRST_ADDRESS;
        let code0_addr = main_mem_addr;
        let code1_addr = code0_addr + 4096;
        let root_page_table_addr = main_mem_addr + 16384;

        // Initially we'll map the instructions to virtual address 4096.
        let code_virt_addr = 4096;

        // The root page table will be mapped to virtual address 16384.
        let root_page_table_virt_addr = 16384;

        // We will use Sv39 translation.
        let satp = Satp::new(
            FixedWidthBits::from_bits(root_page_table_addr >> 12),
            FixedWidthBits::from_bits(0),
            TranslationAlgorithm::Sv39,
        );

        // Generate the initial page table.
        let init_page_table = {
            let mut root_pt = vec![0u8; 4096];

            // Generate the first page table entry which is used to loop back to the root page
            // table for addresses where vpn[2] = 0 or vpn[1] = 0. This effectively disables
            // translation for all VPN indices but the first (index 0).
            let pte = PageTableEntry::new(
                true,
                false,
                false,
                false,
                false,
                true,
                true,
                true,
                crate::bits::ConstantBits,
                PPNField::from_bits(root_page_table_addr >> 12),
                crate::bits::ConstantBits,
                crate::bits::ConstantBits,
                crate::bits::ConstantBits,
            );
            let pte_bytes = pte.to_bits().to_le_bytes();
            root_pt[..8].copy_from_slice(&pte_bytes);

            // Generate the page table entry for code block. This maps [code_virt_addr] to
            // [code0_addr] initially.
            let pte = PageTableEntry::new(
                true,
                true,
                false,
                true,
                false,
                true,
                true,
                true,
                crate::bits::ConstantBits,
                PPNField::from_bits(code0_addr >> 12),
                crate::bits::ConstantBits,
                crate::bits::ConstantBits,
                crate::bits::ConstantBits,
            );
            let vpn = code_virt_addr >> 12;
            let pte_offset = (8 * vpn) as usize;
            let pte_bytes = pte.to_bits().to_le_bytes();
            root_pt[pte_offset..pte_offset + 8].copy_from_slice(&pte_bytes);

            // Generate the page table entry for the page table itself so we can modify it from
            // user space. Maps [root_page_table_virt_addr] to [root_page_table_addr].
            let pte = PageTableEntry::new(
                true,
                true,
                true,
                false,
                false,
                true,
                true,
                true,
                crate::bits::ConstantBits,
                PPNField::from_bits(root_page_table_addr >> 12),
                crate::bits::ConstantBits,
                crate::bits::ConstantBits,
                crate::bits::ConstantBits,
            );
            let vpn = root_page_table_virt_addr >> 12;
            let pte_offset = (8 * vpn) as usize;
            let pte_bytes = pte.to_bits().to_le_bytes();
            root_pt[pte_offset..pte_offset + 8].copy_from_slice(&pte_bytes);

            root_pt
        };

        // Generate a new PTE that will be used to override the root page table at index 1.
        // It'll effectively remap [code_virt_addr] from [code0_addr] to [code1_addr].
        let new_pte = PageTableEntry::new(
            true,
            true,
            false,
            true,
            false,
            true,
            true,
            true,
            crate::bits::ConstantBits,
            PPNField::from_bits(code1_addr >> 12),
            crate::bits::ConstantBits,
            crate::bits::ConstantBits,
            crate::bits::ConstantBits,
        );

        // Instructions at [code0_addr].
        let instrs0 = [
            0x00533423, // sd t0, 8(t1); change the root page table
            0x4505,     // c.li a0, 1;   "return" 1
        ]
        .into_iter()
        .flat_map(u32::to_le_bytes)
        .collect::<Vec<_>>();
        assert_eq!(parse_block(instrs0.as_slice()), [
            Instr::Cacheable(InstrCacheable::Sd(SBTypeArgs {
                rs1: t1,
                rs2: t0,
                imm: 8,
            })),
            Instr::Cacheable(InstrCacheable::CLi(CIBNZTypeArgs {
                rd_rs1: nz::a0,
                imm: 1
            })),
            Instr::Cacheable(InstrCacheable::UnknownCompressed { instr: 0 })
        ]);

        // Instructions at [code1_addr].
        let instrs1 = [
            0x00533423, // sd t0, 8(t1); change the root page table
            0x4509,     // c.li a0, 2;   "return" 2
        ]
        .into_iter()
        .flat_map(u32::to_le_bytes)
        .collect::<Vec<_>>();
        assert_eq!(parse_block(instrs1.as_slice()), [
            Instr::Cacheable(InstrCacheable::Sd(SBTypeArgs {
                rs1: t1,
                rs2: t0,
                imm: 8,
            })),
            Instr::Cacheable(InstrCacheable::CLi(CIBNZTypeArgs {
                rd_rs1: nz::a0,
                imm: 2
            })),
            Instr::Cacheable(InstrCacheable::UnknownCompressed { instr: 0 })
        ]);

        type LocalLayout = MachineStateLayout<M1M, TestCacheLayouts>;

        type BlockRunner<F> = Interpreted<M1M, <F as TestBackendFactory>::Manager>;

        type LocalMachineState<F> =
            MachineState<M1M, TestCacheLayouts, BlockRunner<F>, <F as TestBackendFactory>::Manager>;

        // Configure the state backend.
        let base_state = {
            let mut state = create_state!(
                MachineState,
                LocalLayout,
                F,
                M1M,
                TestCacheLayouts,
                BlockRunner<F>,
                || InterpretedBlockBuilder
            );
            state.reset();

            state
                .core
                .main_memory
                .write_all(root_page_table_addr, &init_page_table)
                .unwrap();
            state
                .core
                .main_memory
                .write_all(code0_addr, &instrs0)
                .unwrap();
            state
                .core
                .main_memory
                .write_all(code1_addr, &instrs1)
                .unwrap();

            state.core.hart.pc.write(code_virt_addr);

            state.core.hart.mode.write(Mode::User);
            state.core.hart.csregisters.write(CSRegister::satp, satp);

            state.core.hart.xregisters.write(t0, new_pte.to_bits());
            state
                .core
                .hart
                .xregisters
                .write(t1, root_page_table_virt_addr);

            state
        };

        // Run 2 steps consecutively against one backend.
        let state = {
            let mut state: LocalMachineState<F> = MachineState::bind(
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
                let mut state: LocalMachineState<F> = MachineState::bind(
                    copy_via_serde::<LocalLayout, _, _>(&base_state.struct_ref::<FnManagerIdent>()),
                    InterpretedBlockBuilder,
                );
                state.step().unwrap();
                state
            };
            {
                let mut state: LocalMachineState<F> = MachineState::bind(
                    copy_via_serde::<LocalLayout, _, _>(&alt_state.struct_ref::<FnManagerIdent>()),
                    InterpretedBlockBuilder,
                );
                state.step().unwrap();
                state
            }
        };

        // Both backends should have transitioned to the same state.
        assert_eq!(state.core.hart.xregisters.read(a0), 1);
        assert_eq!(
            state.core.hart.xregisters.read(a0),
            alt_state.core.hart.xregisters.read(a0)
        );
        assert_eq_struct(
            &state.struct_ref::<FnManagerIdent>(),
            &alt_state.struct_ref::<FnManagerIdent>(),
        );
    });

    // Ensure that cloning the machine state does not result in a stack overflow
    backend_test!(test_machine_state_cloneable, F, {
        let state = create_state!(
            MachineState, 
            MachineStateLayout<M1M, DefaultCacheLayouts>, 
            F, 
            M1M, 
            DefaultCacheLayouts, 
            Interpreted<M1M, F::Manager>, || InterpretedBlockBuilder);

        let second = state.clone();

        assert_eq_struct(
            &state.struct_ref::<FnManagerIdent>(),
            &second.struct_ref::<FnManagerIdent>(),
        );
    });

    backend_test!(test_block_cache_crossing_pages_creates_new_block, F, {
        let mut state = create_state!(
            MachineState, 
            MachineStateLayout<M8K, DefaultCacheLayouts>, 
            F, 
            M8K, 
            DefaultCacheLayouts, 
            Interpreted<M8K, F::Manager>, || InterpretedBlockBuilder);

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

        let start_ram = main_memory::FIRST_ADDRESS;

        // Write the instructions to the beginning of the main memory and point the program
        // counter at the first instruction.
        let phys_addr = start_ram + PAGE_SIZE - 6;

        state.core.hart.pc.write(phys_addr);
        state.core.hart.mode.write(Mode::Machine);

        for offset in 0..3 {
            state
                .core
                .main_memory
                .write(phys_addr + offset * 4, uncompressed_bytes)
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
                opcode: OpCode::Auipc,
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
                opcode: OpCode::CLui,
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
                opcode: OpCode::Addiw,
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
                opcode: OpCode::CSw,
                args: TaggedArgs {
                    rs1: a0.into(),
                    rs2: a1.into(),
                    imm: 0,
                    width: InstrWidth::Compressed,
                    rd: TaggedRegister::X(XRegister::x1),
                    ..TaggedArgs::DEFAULT
                },
            })
            .unwrap(),
            Instruction::try_from(TaggedInstruction {
                opcode: OpCode::Jalr,
                args: TaggedArgs {
                    rd: zero.into(),
                    rs1: a0.into(),
                    imm: 0,
                    rs2: TaggedRegister::X(XRegister::x1),
                    ..TaggedArgs::DEFAULT
                },
            })
            .unwrap(),
        ];

        let phys_addr = main_memory::FIRST_ADDRESS;

        let block_b_addr = phys_addr + 128;

        let mut state = create_state!(
            MachineState, 
            MachineStateLayout<M8K, DefaultCacheLayouts>, 
            F, 
            M8K, 
            DefaultCacheLayouts, 
            Interpreted<M8K, F::Manager>, || InterpretedBlockBuilder);

        // Write the instructions to the beginning of the main memory and point the program
        // counter at the first instruction.
        state.core.hart.pc.write(phys_addr);
        state.core.hart.mode.write(Mode::Machine);

        // block A
        state
            .core
            .main_memory
            .write(phys_addr, auipc_bytes)
            .unwrap();
        state
            .core
            .main_memory
            .write(phys_addr + 4, cj_bytes)
            .unwrap();

        // block B
        state
            .core
            .main_memory
            .write(block_b_addr, clui_bytes)
            .unwrap();
        state
            .core
            .main_memory
            .write(block_b_addr + 2, addiw_bytes)
            .unwrap();
        state
            .core
            .main_memory
            .write(block_b_addr + 6, csw_bytes)
            .unwrap();
        state
            .core
            .main_memory
            .write(block_b_addr + 8, jalr_bytes)
            .unwrap();

        // Overwritten jump dest
        state
            .core
            .main_memory
            .write(phys_addr + 1024, overwrite_bytes)
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
