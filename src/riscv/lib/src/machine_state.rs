// SPDX-FileCopyrightText: 2023-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

#![deny(rustdoc::broken_intra_doc_links)]

pub mod address_translation;
pub mod bus;
pub mod csregisters;
pub mod hart_state;
pub mod mode;
pub mod registers;
pub mod reservation_set;

#[cfg(test)]
extern crate proptest;

use crate::{
    bits::u64,
    devicetree,
    machine_state::{
        bus::{main_memory, Address, AddressableRead, AddressableWrite, Bus, OutOfBounds},
        csregisters::CSRegister,
        hart_state::{HartState, HartStateLayout},
    },
    parser::{instruction::Instr, parse},
    program::Program,
    range_utils::{bound_saturating_sub, less_than_bound, unwrap_bound},
    state_backend::{self as backend},
    traps::{EnvironException, Exception, Interrupt, TrapContext},
};
pub use address_translation::AccessType;
use address_translation::{
    translation_cache::{TranslationCache, TranslationCacheLayout},
    PAGE_SIZE,
};
use csregisters::{values::CSRValue, CSRRepr};
use mode::Mode;
use std::ops::Bound;

/// Layout for the machine state
pub type MachineStateLayout<ML> = (HartStateLayout, bus::BusLayout<ML>, TranslationCacheLayout);

/// Machine state
pub struct MachineState<ML: main_memory::MainMemoryLayout, M: backend::ManagerBase> {
    pub hart: HartState<M>,
    pub bus: Bus<ML, M>,
    pub translation_cache: TranslationCache<M>,
}

/// How to modify the program counter
#[derive(Debug)]
enum ProgramCounterUpdate {
    /// Jump to a fixed address
    Set(Address),
    /// Offset the program counter by a certain value
    Add(u64),
}

/// Result type when running multiple steps at a time with [`MachineState::step_many`]
#[derive(Debug)]
pub struct StepManyResult<E> {
    pub steps: usize,
    pub error: Option<E>,
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

/// Runs a no-arguments instruction (wfi, fenceI)
macro_rules! run_no_args_instr {
    ($state: ident, $instr: ident, $run_fn: ident) => {{
        $state.$run_fn();
        Ok(Add($instr.width()))
    }};
}

/// Runs a F/D instruction over the hart state, touching both F & X registers.
macro_rules! run_f_x_instr {
    ($state: ident, $instr: ident, $args: ident, $run_fn: ident) => {{
        $state.hart.$run_fn($args.rs1, $args.rd)?;
        Ok(Add($instr.width()))
    }};

    ($state: ident, $instr: ident, $args: ident, $run_fn: ident, $rm:ident) => {{
        $state.hart.$run_fn($args.rs1, $args.$rm, $args.rd)?;
        Ok(Add($instr.width()))
    }};
}

/// Runs a F/D instruction over the hart state, touching both F & fcsr registers.
macro_rules! run_f_r_instr {
    ($state: ident, $instr: ident, $args: ident, $run_fn: ident) => {{
        $state.hart.$run_fn($args.rs1, $args.rs2, $args.rd)?;
        Ok(Add($instr.width()))
    }};
    ($state: ident, $instr: ident, $args: ident, $run_fn: ident, $($field: ident),+) => {{
        $state.hart.$run_fn($args.rs1, $($args.$field,)* $args.rd)?;
        Ok(Add($instr.width()))
    }};
}

/// Runs an atomic instruction
/// Similar to R-type instructions, additionally passing the `rl` and `aq` bits
macro_rules! run_amo_instr {
    ($state: ident, $instr: ident, $args: ident, $run_fn: ident) => {{
        $state
            .$run_fn($args.rs1, $args.rs2, $args.rd, $args.rl, $args.aq)
            .map(|_| Add($instr.width()))
    }};
}

/// Runs a CR-type compressed instruction
macro_rules! run_cr_type_instr {
    ($state: ident, $instr:ident, $args:ident, $run_fn: ident) => {{
        $state.hart.xregisters.$run_fn($args.rd_rs1, $args.rs2);
        Ok(ProgramCounterUpdate::Add($instr.width()))
    }};
}

/// Runs a CI-type compressed instruction
macro_rules! run_ci_type_instr {
    ($state: ident, $instr:ident, $args:ident, $run_fn: ident) => {{
        $state.hart.xregisters.$run_fn($args.imm, $args.rd_rs1);
        Ok(ProgramCounterUpdate::Add($instr.width()))
    }};
}

/// Runs a CI-type compressed load instruction
macro_rules! run_ci_load_sp_instr {
    ($state: ident, $instr: ident, $args: ident, $run_fn: ident) => {{
        $state
            .$run_fn($args.imm, $args.rd_rs1)
            .map(|_| Add($instr.width()))
    }};
}

/// Runs a CI-type compressed load instruction
macro_rules! run_css_instr {
    ($state: ident, $instr: ident, $args: ident, $run_fn: ident) => {{
        $state
            .$run_fn($args.imm, $args.rs2)
            .map(|_| Add($instr.width()))
    }};
}

/// Runs a CB-type compressed instruction
macro_rules! run_cb_type_instr {
    ($state: ident, $args: ident, $run_fn: ident) => {{
        Ok(Set($state.hart.$run_fn($args.imm, $args.rd_rs1)))
    }};
}

impl<ML: main_memory::MainMemoryLayout, M: backend::ManagerBase> MachineState<ML, M> {
    /// Bind the machine state to the given allocated space.
    pub fn bind(space: backend::AllocatedOf<MachineStateLayout<ML>, M>) -> Self {
        Self {
            hart: HartState::bind(space.0),
            bus: Bus::bind(space.1),
            translation_cache: TranslationCache::bind(space.2),
        }
    }

    /// Reset the machine state.
    pub fn reset(&mut self)
    where
        M: backend::ManagerWrite,
    {
        self.hart.reset(bus::start_of_main_memory::<ML>());
        self.bus.reset();
        self.translation_cache.reset();
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

        let phys_addr = if let Some(phys_addr) =
            self.translation_cache
                .try_translate(mode, satp, AccessType::Instruction, virt_addr)
        {
            phys_addr
        } else {
            let phys_addr =
                self.translate_with_prefetch(mode, satp, virt_addr, AccessType::Instruction)?;

            self.translation_cache.cache_translation(
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
        self.bus
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
        parse(first_halfword, || {
            let next_addr = phys_addr + 2;

            // Optimization to skip an extra address translation lookup:
            // If the last 2 bytes of the instruction are in the same page
            // as the first 2 bytes, then we already know the translated address
            let phys_addr = if next_addr % PAGE_SIZE == 0 {
                self.translate_instr_address(mode, satp, virt_addr + 2)?
            } else {
                next_addr
            };

            self.fetch_instr_halfword(phys_addr)
        })
    }

    /// Advance [`MachineState`] by executing an [`Instr`]
    fn run_instr(&mut self, instr: Instr) -> Result<ProgramCounterUpdate, Exception>
    where
        M: backend::ManagerReadWrite,
    {
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

            // RV64A atomic instructions
            Instr::Lrw(args) => run_amo_instr!(self, instr, args, run_lrw),
            Instr::Scw(args) => run_amo_instr!(self, instr, args, run_scw),
            Instr::Amoswapw(args) => run_amo_instr!(self, instr, args, run_amoswapw),
            Instr::Amoaddw(args) => run_amo_instr!(self, instr, args, run_amoaddw),
            Instr::Amoxorw(args) => run_amo_instr!(self, instr, args, run_amoxorw),
            Instr::Amoandw(args) => run_amo_instr!(self, instr, args, run_amoandw),
            Instr::Amoorw(args) => run_amo_instr!(self, instr, args, run_amoorw),
            Instr::Amominw(args) => run_amo_instr!(self, instr, args, run_amominw),
            Instr::Amomaxw(args) => run_amo_instr!(self, instr, args, run_amomaxw),
            Instr::Amominuw(args) => run_amo_instr!(self, instr, args, run_amominuw),
            Instr::Amomaxuw(args) => run_amo_instr!(self, instr, args, run_amomaxuw),
            Instr::Lrd(args) => run_amo_instr!(self, instr, args, run_lrd),
            Instr::Scd(args) => run_amo_instr!(self, instr, args, run_scd),
            Instr::Amoswapd(args) => run_amo_instr!(self, instr, args, run_amoswapd),
            Instr::Amoaddd(args) => run_amo_instr!(self, instr, args, run_amoaddd),
            Instr::Amoxord(args) => run_amo_instr!(self, instr, args, run_amoxord),
            Instr::Amoandd(args) => run_amo_instr!(self, instr, args, run_amoandd),
            Instr::Amoord(args) => run_amo_instr!(self, instr, args, run_amoord),
            Instr::Amomind(args) => run_amo_instr!(self, instr, args, run_amomind),
            Instr::Amomaxd(args) => run_amo_instr!(self, instr, args, run_amomaxd),
            Instr::Amominud(args) => run_amo_instr!(self, instr, args, run_amominud),
            Instr::Amomaxud(args) => run_amo_instr!(self, instr, args, run_amomaxud),

            // RV64M multiplication and division instructions
            Instr::Rem(args) => run_r_type_instr!(self, instr, args, run_rem),
            Instr::Remu(args) => run_r_type_instr!(self, instr, args, run_remu),
            Instr::Remw(args) => run_r_type_instr!(self, instr, args, run_remw),
            Instr::Remuw(args) => run_r_type_instr!(self, instr, args, run_remuw),
            Instr::Div(args) => run_r_type_instr!(self, instr, args, run_div),
            Instr::Divu(args) => run_r_type_instr!(self, instr, args, run_divu),
            Instr::Divw(args) => run_r_type_instr!(self, instr, args, run_divw),
            Instr::Divuw(args) => run_r_type_instr!(self, instr, args, run_divuw),
            Instr::Mul(args) => run_r_type_instr!(self, instr, args, run_mul),
            Instr::Mulh(args) => run_r_type_instr!(self, instr, args, run_mulh),
            Instr::Mulhsu(args) => run_r_type_instr!(self, instr, args, run_mulhsu),
            Instr::Mulhu(args) => run_r_type_instr!(self, instr, args, run_mulhu),
            Instr::Mulw(args) => run_r_type_instr!(self, instr, args, run_mulw),

            // RV64F instructions
            Instr::FclassS(args) => run_f_x_instr!(self, instr, args, run_fclass_s),
            Instr::Feqs(args) => run_f_r_instr!(self, instr, args, run_feq_s),
            Instr::Fles(args) => run_f_r_instr!(self, instr, args, run_fle_s),
            Instr::Flts(args) => run_f_r_instr!(self, instr, args, run_flt_s),
            Instr::Fadds(args) => run_f_r_instr!(self, instr, args, run_fadd_s, rs2, rm),
            Instr::Fsubs(args) => run_f_r_instr!(self, instr, args, run_fsub_s, rs2, rm),
            Instr::Fmuls(args) => run_f_r_instr!(self, instr, args, run_fmul_s, rs2, rm),
            Instr::Fdivs(args) => run_f_r_instr!(self, instr, args, run_fdiv_s, rs2, rm),
            Instr::Fsqrts(args) => run_f_r_instr!(self, instr, args, run_fsqrt_s, rm),
            Instr::Fmins(args) => run_f_r_instr!(self, instr, args, run_fmin_s),
            Instr::Fmaxs(args) => run_f_r_instr!(self, instr, args, run_fmax_s),
            Instr::Fmadds(args) => run_f_r_instr!(self, instr, args, run_fmadd_s, rs2, rs3, rm),
            Instr::Fmsubs(args) => run_f_r_instr!(self, instr, args, run_fmsub_s, rs2, rs3, rm),
            Instr::Fnmsubs(args) => run_f_r_instr!(self, instr, args, run_fnmsub_s, rs2, rs3, rm),
            Instr::Fnmadds(args) => run_f_r_instr!(self, instr, args, run_fnmadd_s, rs2, rs3, rm),
            Instr::Flw(args) => run_load_instr!(self, instr, args, run_flw),
            Instr::Fsw(args) => run_store_instr!(self, instr, args, run_fsw),
            Instr::Fcvtsw(args) => run_f_x_instr!(self, instr, args, run_fcvt_s_w, rm),
            Instr::Fcvtswu(args) => run_f_x_instr!(self, instr, args, run_fcvt_s_wu, rm),
            Instr::Fcvtsl(args) => run_f_x_instr!(self, instr, args, run_fcvt_s_l, rm),
            Instr::Fcvtslu(args) => run_f_x_instr!(self, instr, args, run_fcvt_s_lu, rm),
            Instr::Fcvtws(args) => run_f_x_instr!(self, instr, args, run_fcvt_w_s, rm),
            Instr::Fcvtwus(args) => run_f_x_instr!(self, instr, args, run_fcvt_wu_s, rm),
            Instr::Fcvtls(args) => run_f_x_instr!(self, instr, args, run_fcvt_l_s, rm),
            Instr::Fcvtlus(args) => run_f_x_instr!(self, instr, args, run_fcvt_lu_s, rm),
            Instr::Fsgnjs(args) => run_f_r_instr!(self, instr, args, run_fsgnj_s),
            Instr::Fsgnjns(args) => run_f_r_instr!(self, instr, args, run_fsgnjn_s),
            Instr::Fsgnjxs(args) => run_f_r_instr!(self, instr, args, run_fsgnjx_s),
            Instr::FmvXW(args) => run_f_x_instr!(self, instr, args, run_fmv_x_w),
            Instr::FmvWX(args) => run_f_x_instr!(self, instr, args, run_fmv_w_x),

            // RV64D instructions
            Instr::FclassD(args) => run_f_x_instr!(self, instr, args, run_fclass_d),
            Instr::Feqd(args) => run_f_r_instr!(self, instr, args, run_feq_d),
            Instr::Fled(args) => run_f_r_instr!(self, instr, args, run_fle_d),
            Instr::Fltd(args) => run_f_r_instr!(self, instr, args, run_flt_d),
            Instr::Faddd(args) => run_f_r_instr!(self, instr, args, run_fadd_d, rs2, rm),
            Instr::Fsubd(args) => run_f_r_instr!(self, instr, args, run_fsub_d, rs2, rm),
            Instr::Fmuld(args) => run_f_r_instr!(self, instr, args, run_fmul_d, rs2, rm),
            Instr::Fdivd(args) => run_f_r_instr!(self, instr, args, run_fdiv_d, rs2, rm),
            Instr::Fsqrtd(args) => run_f_r_instr!(self, instr, args, run_fsqrt_d, rm),
            Instr::Fmind(args) => run_f_r_instr!(self, instr, args, run_fmin_d),
            Instr::Fmaxd(args) => run_f_r_instr!(self, instr, args, run_fmax_d),
            Instr::Fmaddd(args) => run_f_r_instr!(self, instr, args, run_fmadd_d, rs2, rs3, rm),
            Instr::Fmsubd(args) => run_f_r_instr!(self, instr, args, run_fmsub_d, rs2, rs3, rm),
            Instr::Fnmsubd(args) => run_f_r_instr!(self, instr, args, run_fnmsub_d, rs2, rs3, rm),
            Instr::Fnmaddd(args) => run_f_r_instr!(self, instr, args, run_fnmadd_d, rs2, rs3, rm),
            Instr::Fld(args) => run_load_instr!(self, instr, args, run_fld),
            Instr::Fsd(args) => run_store_instr!(self, instr, args, run_fsd),
            Instr::Fcvtdw(args) => run_f_x_instr!(self, instr, args, run_fcvt_d_w, rm),
            Instr::Fcvtdwu(args) => run_f_x_instr!(self, instr, args, run_fcvt_d_wu, rm),
            Instr::Fcvtdl(args) => run_f_x_instr!(self, instr, args, run_fcvt_d_l, rm),
            Instr::Fcvtdlu(args) => run_f_x_instr!(self, instr, args, run_fcvt_d_lu, rm),
            Instr::Fcvtds(args) => run_f_r_instr!(self, instr, args, run_fcvt_d_s, rm),
            Instr::Fcvtsd(args) => run_f_r_instr!(self, instr, args, run_fcvt_s_d, rm),
            Instr::Fcvtwd(args) => run_f_x_instr!(self, instr, args, run_fcvt_w_d, rm),
            Instr::Fcvtwud(args) => run_f_x_instr!(self, instr, args, run_fcvt_wu_d, rm),
            Instr::Fcvtld(args) => run_f_x_instr!(self, instr, args, run_fcvt_l_d, rm),
            Instr::Fcvtlud(args) => run_f_x_instr!(self, instr, args, run_fcvt_lu_d, rm),
            Instr::Fsgnjd(args) => run_f_r_instr!(self, instr, args, run_fsgnj_d),
            Instr::Fsgnjnd(args) => run_f_r_instr!(self, instr, args, run_fsgnjn_d),
            Instr::Fsgnjxd(args) => run_f_r_instr!(self, instr, args, run_fsgnjx_d),
            Instr::FmvXD(args) => run_f_x_instr!(self, instr, args, run_fmv_x_d),
            Instr::FmvDX(args) => run_f_x_instr!(self, instr, args, run_fmv_d_x),

            // Zicsr instructions
            Instr::Csrrw(args) => run_csr_instr!(self, instr, args, run_csrrw),
            Instr::Csrrs(args) => run_csr_instr!(self, instr, args, run_csrrs),
            Instr::Csrrc(args) => run_csr_instr!(self, instr, args, run_csrrc),
            Instr::Csrrwi(args) => run_csr_imm_instr!(self, instr, args, run_csrrwi),
            Instr::Csrrsi(args) => run_csr_imm_instr!(self, instr, args, run_csrrsi),
            Instr::Csrrci(args) => run_csr_imm_instr!(self, instr, args, run_csrrci),

            // Zifencei instructions
            Instr::FenceI => run_no_args_instr!(self, instr, run_fencei),

            // Privileged instructions
            // Trap-Return
            Instr::Mret => run_xret_instr!(self, run_mret),
            Instr::Sret => run_xret_instr!(self, run_sret),
            // Currently not implemented instruction (part of Smrnmi extension)
            Instr::Mnret => Err(Exception::IllegalInstruction),
            // Interrupt-Management
            Instr::Wfi => run_no_args_instr!(self, instr, run_wfi),
            // Supervisor Memory-Management
            Instr::SFenceVma { asid, vaddr } => {
                self.run_sfence_vma(asid, vaddr)?;
                Ok(ProgramCounterUpdate::Add(instr.width()))
            }

            // RV32C compressed instructions
            Instr::CLw(args) => run_load_instr!(self, instr, args, run_clw),
            Instr::CLwsp(args) => run_ci_load_sp_instr!(self, instr, args, run_clwsp),
            Instr::CSw(args) => run_store_instr!(self, instr, args, run_csw),
            Instr::CSwsp(args) => run_css_instr!(self, instr, args, run_cswsp),
            Instr::CJ(args) => Ok(Set(self.hart.run_cj(args.imm))),
            Instr::CJr(args) => Ok(Set(self.hart.run_cjr(args.rs1))),
            Instr::CJalr(args) => Ok(Set(self.hart.run_cjalr(args.rs1))),
            Instr::CBeqz(args) => run_cb_type_instr!(self, args, run_cbeqz),
            Instr::CBnez(args) => run_cb_type_instr!(self, args, run_cbnez),
            Instr::CLi(args) => run_ci_type_instr!(self, instr, args, run_cli),
            Instr::CLui(args) => run_ci_type_instr!(self, instr, args, run_clui),
            Instr::CAddi(args) => run_ci_type_instr!(self, instr, args, run_caddi),
            Instr::CAddi16sp(args) => {
                self.hart.xregisters.run_caddi16sp(args.imm);
                Ok(ProgramCounterUpdate::Add(instr.width()))
            }
            Instr::CAddi4spn(args) => run_ci_type_instr!(self, instr, args, run_caddi4spn),
            Instr::CSlli(args) => run_ci_type_instr!(self, instr, args, run_cslli),
            Instr::CSrli(args) => run_ci_type_instr!(self, instr, args, run_csrli),
            Instr::CSrai(args) => run_ci_type_instr!(self, instr, args, run_csrai),
            Instr::CAndi(args) => run_ci_type_instr!(self, instr, args, run_candi),
            Instr::CMv(args) => run_cr_type_instr!(self, instr, args, run_cmv),
            Instr::CAdd(args) => run_cr_type_instr!(self, instr, args, run_cadd),
            Instr::CAnd(args) => run_cr_type_instr!(self, instr, args, run_cand),
            Instr::CXor(args) => run_cr_type_instr!(self, instr, args, run_cxor),
            Instr::COr(args) => run_cr_type_instr!(self, instr, args, run_cor),
            Instr::CSub(args) => run_cr_type_instr!(self, instr, args, run_csub),
            Instr::CEbreak => run_syscall_instr!(self, run_cebreak),
            Instr::CNop => {
                self.run_cnop();
                Ok(ProgramCounterUpdate::Add(instr.width()))
            }

            // RV64C compressed instructions
            Instr::CLd(args) => run_load_instr!(self, instr, args, run_cld),
            Instr::CLdsp(args) => run_ci_load_sp_instr!(self, instr, args, run_cldsp),
            Instr::CSd(args) => run_store_instr!(self, instr, args, run_csd),
            Instr::CSdsp(args) => run_css_instr!(self, instr, args, run_csdsp),
            Instr::CAddiw(args) => run_ci_type_instr!(self, instr, args, run_caddiw),
            Instr::CAddw(args) => run_cr_type_instr!(self, instr, args, run_caddw),
            Instr::CSubw(args) => run_cr_type_instr!(self, instr, args, run_csubw),

            // RV64DC compressed instructions
            Instr::CFld(args) => run_load_instr!(self, instr, args, run_cfld),
            Instr::CFldsp(args) => run_ci_load_sp_instr!(self, instr, args, run_cfldsp),
            Instr::CFsd(args) => run_store_instr!(self, instr, args, run_cfsd),
            Instr::CFsdsp(args) => run_css_instr!(self, instr, args, run_cfsdsp),

            Instr::Unknown { instr: _ } => Err(Exception::IllegalInstruction),
            Instr::UnknownCompressed { instr: _ } => Err(Exception::IllegalInstruction),
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
        match self.fetch_instr(current_mode, satp, instr_pc, phys_addr) {
            Ok(instr) => self.run_instr(instr),
            Err(e) => Err(e),
        }
    }

    /// Handle interrupts (also known as asynchronous exceptions)
    /// by taking a trap for the given interrupt.
    ///
    /// If trap is taken, return new address of program counter.
    /// Throw [`EnvironException`] if the interrupt has to be treated by the execution enviroment.
    fn address_on_interrupt(&mut self, interrupt: Interrupt) -> Result<Address, EnvironException>
    where
        M: backend::ManagerReadWrite,
    {
        let current_pc = self.hart.pc.read();
        let mip: CSRRepr = self.hart.csregisters.read(CSRegister::mip);

        // Clear the bit in the set of pending interrupt, marking it as handled
        self.hart.csregisters.write(
            CSRegister::mip,
            u64::set_bit(mip, interrupt.exception_code() as usize, false),
        );

        let new_pc = self.hart.take_trap(interrupt, current_pc);
        // Commit pc, it may be read by other instructions in this step
        self.hart.pc.write(new_pc);
        Ok(new_pc)
    }

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

    /// Take an interrupt if available, and then
    /// perform precisely one [`Instr`] and handle the traps that may rise as a side-effect.
    ///
    /// The [`Err`] case represents an [`Exception`] to be handled by
    /// the execution environment, narrowed down by the type [`EnvironException`].
    #[inline]
    pub fn step(&mut self) -> Result<(), EnvironException>
    where
        M: backend::ManagerReadWrite,
    {
        self.step_max_inner(&mut 0, 1)
    }

    #[inline]
    fn handle_step_result(
        &mut self,
        instr_pc: Address,
        result: Result<ProgramCounterUpdate, Exception>,
    ) -> Result<(), EnvironException>
    where
        M: backend::ManagerReadWrite,
    {
        let pc_update = match result {
            Err(exc) => ProgramCounterUpdate::Set(self.address_on_exception(exc, instr_pc)?),
            Ok(upd) => upd,
        };

        // Update program couter
        match pc_update {
            ProgramCounterUpdate::Set(address) => self.hart.pc.write(address),
            ProgramCounterUpdate::Add(width) => self.hart.pc.write(instr_pc + width),
        };

        Ok(())
    }

    fn step_max_inner(
        &mut self,
        steps: &mut usize,
        max_steps: usize,
    ) -> Result<(), EnvironException>
    where
        M: backend::ManagerReadWrite,
    {
        while *steps < max_steps {
            let current_mode = self.hart.mode.read();

            // Try to take an interrupt if available, and then
            // obtain the pc for the next instruction to be executed
            let instr_pc = match self.hart.get_pending_interrupt(current_mode) {
                None => self.hart.pc.read(),
                Some(interrupt) => self.address_on_interrupt(interrupt)?,
            };

            let satp: CSRRepr = self.hart.csregisters.read(CSRegister::satp);
            let instr_result = self
                .translate_instr_address(current_mode, satp, instr_pc)
                .and_then(|phys_addr| self.run_instr_at(current_mode, satp, instr_pc, phys_addr));

            self.handle_step_result(instr_pc, instr_result)?;
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
                    }
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
        self.hart.reset(program.entrypoint);
        // Write program to main memory and point the PC at its start
        for (addr, data) in program.segments.iter() {
            self.bus.write_all(*addr, data)?;
        }

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
            .write(csregisters::CSRegister::medeleg, CSRValue::from(!0u64));
        self.hart
            .csregisters
            .write(csregisters::CSRegister::mideleg, CSRValue::from(!0u64));

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
        backend_test,
        bits::{Bits64, FixedWidthBits},
        create_backend, create_state,
        machine_state::{
            address_translation::pte::{PPNField, PageTableEntry},
            bus::{main_memory::M1M, start_of_main_memory, AddressableWrite},
            csregisters::{
                satp::{Satp, TranslationAlgorithm},
                xstatus::{self, MStatus},
                CSRRepr, CSRegister,
            },
            mode::Mode,
            registers::{a0, a1, a2, t0, t1, t2, zero},
        },
        parser::{
            instruction::{CIBTypeArgs, ITypeArgs, Instr, SBTypeArgs},
            parse_block,
        },
        traps::{EnvironException, Exception, Interrupt, TrapContext},
    };
    use crate::{bits::u64, machine_state::bus::main_memory::M1K};
    use proptest::{prop_assert_eq, proptest};

    backend_test!(test_machine_state_reset, F, {
        test_determinism::<F, MachineStateLayout<T1K>, _>(|space| {
            let mut machine: MachineState<T1K, ManagerFor<'_, F, MachineStateLayout<T1K>>> =
                MachineState::bind(space);
            machine.reset();
        });
    });

    backend_test!(test_step, F, {
        let mut backend = create_backend!(MachineStateLayout<T1K>, F);
        let state = create_state!(MachineState, MachineStateLayout<T1K>, F, backend, T1K);
        let state_cell = std::cell::RefCell::new(state);

        proptest!(|(
            pc_addr_offset in 0..250_u64,
            jump_addr in 0..250_u64,
        )| {
            let mut state = state_cell.borrow_mut();
            state.reset();

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
        let mut backend = create_backend!(MachineStateLayout<T1K>, F);
        let state = create_state!(MachineState, MachineStateLayout<T1K>, F, backend, T1K);
        let state_cell = std::cell::RefCell::new(state);

        proptest!(|(
            pc_addr_offset in 0..200_u64,
            stvec_offset in 10..20_u64,
            mtvec_offset in 25..35_u64,
        )| {
            let mut state = state_cell.borrow_mut();
            state.reset();

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
        let mut backend = create_backend!(MachineStateLayout<T1K>, F);
        let state = create_state!(MachineState, MachineStateLayout<T1K>, F, backend, T1K);
        let state_cell = std::cell::RefCell::new(state);

        proptest!(|(
            pc_addr_offset in 0..200_u64,
            mtvec_offset in 25..35_u64,
        )| {
            let mut state = state_cell.borrow_mut();
            state.reset();

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
            let mstatus: MStatus = state.hart.csregisters.read(CSRegister::mstatus);
            assert_eq!(state.hart.mode.read(), Mode::Machine);
            assert_eq!(state.hart.pc.read(), mtvec_addr);
            assert_eq!(mstatus.mpp(), xstatus::MPPValue::Machine);
            assert_eq!(state.hart.csregisters.read::<CSRRepr>(CSRegister::mepc), init_pc_addr);
            assert_eq!(state.hart.csregisters.read::<CSRRepr>(CSRegister::mcause), 3);
        });
    });

    backend_test!(test_step_inter_mm, F, {
        let mut backend = create_backend!(MachineStateLayout<T1K>, F);
        let state = create_state!(MachineState, MachineStateLayout<T1K>, F, backend, T1K);
        let state_cell = std::cell::RefCell::new(state);

        proptest!(|(
            pc_addr_offset in 0..200_u64,
            stvec_offset in 10..20_u64,
            mtvec_offset in 25..35_u64,
        )| {
            // Raise interrupt, take trap from M-mode to M-mode
            // (test delegation doesn't take place even if enabled by registers)

            let mut state = state_cell.borrow_mut();
            state.reset();

            let init_pc_addr = bus::start_of_main_memory::<T1K>() + pc_addr_offset * 4;
            let stvec_addr = init_pc_addr + 4 * stvec_offset;
            let mtvec_addr = init_pc_addr + 4 * mtvec_offset;
            const AUIPC: u64 = 0b001_0111;

            // stvec is in BASE mode
            state.hart.csregisters.write(CSRegister::stvec, stvec_addr);
            // mtvec is in VECTORED mode
            state.hart.csregisters.write(CSRegister::mtvec, mtvec_addr | 1);
            let mie = u64::set_bit(u64::set_bit(0u64, Interrupt::MachineTimer.exception_code() as usize, true), Interrupt::MachineExternal.exception_code() as usize, true);
            let mip = u64::set_bit(u64::set_bit(u64::set_bit(0u64, Interrupt::SupervisorTimer.exception_code() as usize, true), Interrupt::MachineExternal.exception_code() as usize, true), Interrupt::SupervisorSoftware.exception_code() as usize, true);
            let mideleg_val = 1 << Interrupt::MachineExternal.exception_code() |
                1 << Interrupt::SupervisorSoftware.exception_code() |
                1 << Interrupt::MachineTimer.exception_code();
            // The interrupt taken is MachineExternal. theoretically, mideleg delegates this trap,
            // but because it is a machine interupt it will still be handled in M-mode.
            state.hart.csregisters.write(CSRegister::mip, mip);
            state.hart.csregisters.write(CSRegister::mie, mie);
            state.hart.csregisters.write(CSRegister::mideleg, mideleg_val);
            let mstatus = state.hart.csregisters.read::<MStatus>(CSRegister::mstatus).with_mie(true);
            state.hart.csregisters.write(CSRegister::mstatus, mstatus);
            state.hart.mode.write(Mode::Machine);
            // it doesn't really matter where the pc is since we will take an interrupt
            state.hart.pc.write(init_pc_addr);
            state.hart.xregisters.write(a1, AUIPC);
            state.hart.xregisters.write(a2, mtvec_addr + 4 * 11);
            state.run_sw(0, a2, a1).expect("Storing instruction should succeed");
            state.step().expect("should not raise environment exception");
            let mstatus: MStatus = state.hart.csregisters.read(CSRegister::mstatus);
            assert_eq!(state.hart.mode.read(), Mode::Machine);
            assert_eq!(state.hart.pc.read(), mtvec_addr + 4 * 11 + 4);
            // We are coming from the interrupt handler actually
            assert_eq!(mstatus.mpp(), xstatus::MPPValue::Machine);
            assert_eq!(state.hart.csregisters.read::<CSRRepr>(CSRegister::mepc), init_pc_addr);
            assert_eq!(state.hart.csregisters.read::<CSRRepr>(CSRegister::mcause), 1 << 63 | 11);
            assert_eq!(state.hart.csregisters.read::<CSRRepr>(CSRegister::mip), mip ^ 1 << 11);
        });
    });

    backend_test!(test_step_exc_us, F, {
        let mut backend = create_backend!(MachineStateLayout<T1K>, F);
        let state = create_state!(MachineState, MachineStateLayout<T1K>, F, backend, T1K);
        let state_cell = std::cell::RefCell::new(state);

        proptest!(|(
            pc_addr_offset in 0..200_u64,
            stvec_offset in 10..20_u64,
        )| {
            // Raise exception, take trap from U-mode to S-mode (test delegation takes place)

            let mut state = state_cell.borrow_mut();
            state.reset();

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
            let mstatus: MStatus = state.hart.csregisters.read(CSRegister::mstatus);
            assert_eq!(state.hart.mode.read(), Mode::Supervisor);
            assert_eq!(state.hart.pc.read(), stvec_addr);
            assert_eq!(mstatus.spp(), xstatus::SPPValue::User);
            assert_eq!(state.hart.csregisters.read::<CSRRepr>(CSRegister::sepc), bad_address);
            assert_eq!(state.hart.csregisters.read::<CSRRepr>(CSRegister::scause), 1);
        });
    });

    backend_test!(test_step_trap_usm, F, {
        let mut backend = create_backend!(MachineStateLayout<T1K>, F);
        let state = create_state!(MachineState, MachineStateLayout<T1K>, F, backend, T1K);
        let state_cell = std::cell::RefCell::new(state);

        proptest!(|(
            pc_addr_offset in 0..200_u64,
            stvec_offset in 10..20_u64,
            mtvec_offset in 25..35_u64,
        )| {
            // TEST: Raise interrupt from U to S, then exception from S to M
            // this will be tested by reading mcause and scause, the interrupt will set scause
            // and the exception will set mcause
            // interrupt delegation will delegate the SEI, but not MSI, testing the priority as well

            let mut state = state_cell.borrow_mut();
            state.reset();

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
            let mie = u64::set_bit(u64::set_bit(0u64, Interrupt::MachineSoftware.exception_code() as usize, true), Interrupt::SupervisorExternal.exception_code() as usize, true);
            let mip = u64::set_bit(u64::set_bit(0u64, Interrupt::SupervisorTimer.exception_code() as usize, true), Interrupt::SupervisorExternal.exception_code() as usize, true);
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
            let mstatus: MStatus = state.hart.csregisters.read(CSRegister::mstatus);
            let mstatus = mstatus.with_sie(true);
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
            let mstatus: MStatus = state.hart.csregisters.read(CSRegister::mstatus);
            assert_eq!(state.hart.mode.read(), Mode::Machine);
            assert_eq!(state.hart.pc.read(), mtvec_addr);
            // We are coming from the interrupt handler actually
            assert_eq!(mstatus.mpp(), xstatus::MPPValue::Supervisor);
            assert_eq!(mstatus.spp(), xstatus::SPPValue::User);
            assert_eq!(state.hart.csregisters.read::<CSRRepr>(CSRegister::sepc), init_pc_addr);
            assert_eq!(state.hart.csregisters.read::<CSRRepr>(CSRegister::mepc), stvec_addr + 4 * 9);
            assert_eq!(state.hart.csregisters.read::<CSRRepr>(CSRegister::scause), 1 << 63 | 9);
            assert_eq!(state.hart.csregisters.read::<CSRRepr>(CSRegister::mcause), 5);
            assert_eq!(state.hart.csregisters.read::<CSRRepr>(CSRegister::mtval), 0x333);
            assert_eq!(state.hart.csregisters.read::<CSRRepr>(CSRegister::stval), 0);
            assert_eq!(state.hart.csregisters.read::<CSRRepr>(CSRegister::mip), mip ^ 1 << 9);
        });
    });

    backend_test!(test_reset, F, {
        test_determinism::<F, MachineStateLayout<M1K>, _>(|space| {
            let mut machine_state: MachineState<M1K, ManagerFor<'_, F, MachineStateLayout<M1K>>> =
                MachineState::bind(space);
            machine_state.reset();
        });
    });

    // Test that the machine state does not behave differently when potential ephermeral state is
    // reset that may impact instruction caching.
    backend_test!(test_instruction_cache, F, {
        // Instruction that writes the value in t1 to the address t0.
        const I_WRITE_T1_TO_ADDRESS_T0: u32 = 0b0011000101010000000100011;
        assert_eq!(
            parse_block(&I_WRITE_T1_TO_ADDRESS_T0.to_le_bytes()),
            [Instr::Sw(SBTypeArgs {
                rs1: t0,
                rs2: t1,
                imm: 0
            })]
        );

        // Instruction that loads 6 into t2.
        const I_LOAD_6_INTO_T2: u32 = 0b11000000000001110010011;
        assert_eq!(
            parse_block(&I_LOAD_6_INTO_T2.to_le_bytes()),
            [Instr::Addi(ITypeArgs {
                rd: t2,
                rs1: zero,
                imm: 6
            })]
        );

        // Instruction that loads 5 into t2.
        const I_LOAD_5_INTO_T2: u32 = 0b10100000000001110010011;
        assert_eq!(
            parse_block(&I_LOAD_5_INTO_T2.to_le_bytes()),
            [Instr::Addi(ITypeArgs {
                rd: t2,
                rs1: zero,
                imm: 5
            })]
        );

        let mut backend = create_backend!(MachineStateLayout<M1K>, F);

        // Configure the machine state.
        {
            let mut state = create_state!(MachineState, MachineStateLayout<M1K>, F, backend, M1K);
            state.reset();

            let start_ram = start_of_main_memory::<M1K>();

            // Write the instructions to the beginning of the main memory and point the program
            // counter at the first instruction.
            state.hart.pc.write(start_ram);
            let instrs: [u8; 8] = [I_WRITE_T1_TO_ADDRESS_T0, I_LOAD_5_INTO_T2]
                .into_iter()
                .flat_map(u32::to_le_bytes)
                .collect::<Vec<_>>()
                .try_into()
                .unwrap();
            state.bus.write_all(start_ram, &instrs).unwrap();

            // Configure the machine in such a way that the first instruction will override the
            // second instruction. The second instruction behaves differently.
            let address_to_write = start_ram + 4;
            state.hart.xregisters.write(t0, address_to_write);
            state.hart.xregisters.write(t1, I_LOAD_6_INTO_T2 as u64);
        }

        let mut alt_backend = backend.clone();

        // Perform 2 steps consecutively in one backend.
        let result = {
            let mut state = create_state!(MachineState, MachineStateLayout<M1K>, F, backend, M1K);
            state.step().unwrap();
            state.step().unwrap();
            state.hart.xregisters.read(t2)
        };

        // Perform 2 steps separately in another backend by re-binding the state between steps.
        let alt_result = {
            {
                let mut state =
                    create_state!(MachineState, MachineStateLayout<M1K>, F, alt_backend, M1K);
                state.step().unwrap();
            }

            {
                let mut state =
                    create_state!(MachineState, MachineStateLayout<M1K>, F, alt_backend, M1K);
                state.step().unwrap();
                state.hart.xregisters.read(t2)
            }
        };

        // The two backends should have the same state.
        assert_eq!(result, alt_result);
        assert_eq!(backend, alt_backend);
    });

    // Test that the machine state does not behave differently when potential ephermeral state is
    // reset that may impact instruction address translation caching.
    backend_test!(test_instruction_address_cache, F, {
        let mut backend = create_backend!(MachineStateLayout<M1M>, F);

        // Specify the physcal memory layout.
        let main_mem_addr = start_of_main_memory::<M1M>();
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
        assert_eq!(
            parse_block(instrs0.as_slice()),
            [
                Instr::Sd(SBTypeArgs {
                    rs1: t1,
                    rs2: t0,
                    imm: 8
                }),
                Instr::CLi(CIBTypeArgs { rd_rs1: a0, imm: 1 }),
                Instr::UnknownCompressed { instr: 0 }
            ]
        );

        // Instructions at [code1_addr].
        let instrs1 = [
            0x00533423, // sd t0, 8(t1); change the root page table
            0x4509,     // c.li a0, 2;   "return" 2
        ]
        .into_iter()
        .flat_map(u32::to_le_bytes)
        .collect::<Vec<_>>();
        assert_eq!(
            parse_block(instrs1.as_slice()),
            [
                Instr::Sd(SBTypeArgs {
                    rs1: t1,
                    rs2: t0,
                    imm: 8
                }),
                Instr::CLi(CIBTypeArgs { rd_rs1: a0, imm: 2 }),
                Instr::UnknownCompressed { instr: 0 }
            ]
        );

        // Configure the state backend.
        {
            let mut state = create_state!(MachineState, MachineStateLayout<M1M>, F, backend, M1M);
            state.reset();

            state
                .bus
                .write_all(root_page_table_addr, &init_page_table)
                .unwrap();
            state.bus.write_all(code0_addr, &instrs0).unwrap();
            state.bus.write_all(code1_addr, &instrs1).unwrap();

            state.hart.pc.write(code_virt_addr);

            state.hart.mode.write(Mode::User);
            state.hart.csregisters.write(CSRegister::satp, satp);

            state.hart.xregisters.write(t0, new_pte.to_bits());
            state.hart.xregisters.write(t1, root_page_table_virt_addr);
        }

        let mut alt_backend = backend.clone();

        // Run 2 steps consecutively against one backend.
        let result = {
            let mut state = create_state!(MachineState, MachineStateLayout<M1M>, F, backend, M1M);
            state.step().unwrap();
            state.step().unwrap();
            state.hart.xregisters.read(a0)
        };

        // Perform 2 steps separately in another backend by re-binding the state between steps.
        let alt_result = {
            {
                let mut state =
                    create_state!(MachineState, MachineStateLayout<M1M>, F, alt_backend, M1M);
                state.step().unwrap();
            }
            {
                let mut state =
                    create_state!(MachineState, MachineStateLayout<M1M>, F, alt_backend, M1M);
                state.step().unwrap();
                state.hart.xregisters.read(a0)
            }
        };

        // Both backends should have transitioned to the same state.
        assert_eq!(result, 1);
        assert_eq!(result, alt_result);
        assert_eq!(backend, alt_backend);
    });
}
