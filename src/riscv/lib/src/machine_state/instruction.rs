// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! A replacement for [`InstrCacheable`] instructions.
//!
//! Rather than dispatching on a giant instruction enum, we instead split the instruction into
//! two: an [`OpCode`] and an [`Args`].
//!
//! This allows us to dispatch the operation over the state directly from the opcode - both a
//! simpler match statement and, ultimately, paves the way to pre-dispatch these functions
//! when blocks are built in the block cache. This avoids the runtime overhead caused by
//! dispatching every time an instruction is run.

mod constructors;
pub(crate) mod tagged_instruction;

use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;

use serde::Deserialize;
use serde::Serialize;
use tagged_instruction::ArgsShape;
use tagged_instruction::TaggedInstruction;
use tagged_instruction::opcode_to_argsshape;

use super::MachineCoreState;
use super::ProgramCounterUpdate;
use super::csregisters::CSRegister;
use super::memory::Address;
use super::memory::MemoryConfig;
use super::registers::FRegister;
use super::registers::NonZeroXRegister;
use super::registers::XRegister;
use super::registers::nz;
use super::registers::sp;
use crate::default::ConstDefault;
use crate::instruction_context::ICB;
use crate::instruction_context::IcbFnResult;
use crate::instruction_context::IcbLoweringFn;
use crate::instruction_context::LoadStoreWidth;
use crate::instruction_context::MulHighType;
use crate::instruction_context::Predicate;
use crate::instruction_context::Shift;
use crate::interpreter::atomics;
use crate::interpreter::branching;
use crate::interpreter::integer;
use crate::interpreter::load_store;
use crate::machine_state::ProgramCounterUpdate::Next;
use crate::parser::instruction::AmoArgs;
use crate::parser::instruction::CIBDTypeArgs;
use crate::parser::instruction::CIBNZTypeArgs;
use crate::parser::instruction::CIBTypeArgs;
use crate::parser::instruction::CJTypeArgs;
use crate::parser::instruction::CNZRTypeArgs;
use crate::parser::instruction::CRJTypeArgs;
use crate::parser::instruction::CRTypeArgs;
use crate::parser::instruction::CSSDTypeArgs;
use crate::parser::instruction::CSSTypeArgs;
use crate::parser::instruction::CsrArgs;
use crate::parser::instruction::CsriArgs;
use crate::parser::instruction::FCmpArgs;
use crate::parser::instruction::FLoadArgs;
use crate::parser::instruction::FR1ArgWithRounding;
use crate::parser::instruction::FR2ArgsWithRounding;
use crate::parser::instruction::FR3ArgsWithRounding;
use crate::parser::instruction::FRArgs;
use crate::parser::instruction::FRegToXRegArgs;
use crate::parser::instruction::FRegToXRegArgsWithRounding;
use crate::parser::instruction::FStoreArgs;
use crate::parser::instruction::InstrCacheable;
use crate::parser::instruction::InstrRoundingMode;
use crate::parser::instruction::InstrWidth;
use crate::parser::instruction::NonZeroRdRTypeArgs;
use crate::parser::instruction::NonZeroRdUJTypeArgs;
use crate::parser::instruction::RTypeArgs;
use crate::parser::instruction::UJTypeArgs;
use crate::parser::instruction::XRegToFRegArgs;
use crate::parser::instruction::XRegToFRegArgsWithRounding;
use crate::state_backend::ManagerReadWrite;
use crate::traps::Exception;

/// An instruction formed of an opcode and flat arguments.
///
/// This is preferred within the caches, as it enables 'pre-dispatch' of functions
///
/// Instructions are constructable from [`InstrCacheable`] instructions.
#[derive(Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
#[serde(try_from = "TaggedInstruction", into = "TaggedInstruction")]
pub struct Instruction {
    /// The operation (over the machine state) that this instruction represents.
    pub opcode: OpCode,
    /// Arguments that are passed to the opcode-function. As a flat structure, it contains
    /// all possible arguments. Each instruction will only use a subset.
    args: Args,
}

impl Debug for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        struct DebugArgs<'a>(&'a dyn Fn(&mut Formatter<'_>) -> fmt::Result);

        impl Debug for DebugArgs<'_> {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                (self.0)(f)
            }
        }

        let debug_args = |f: &mut Formatter<'_>| {
            let mut debug_struct = f.debug_struct("Args");
            // SAFETY: The variants used are validated on construction and deserialisation
            // to be the opcode's associated register types, so this is safe.
            unsafe {
                match opcode_to_argsshape(&self.opcode) {
                    ArgsShape::XSrcXDest => {
                        debug_struct.field("rd", &self.args.rd.x);
                        debug_struct.field("rs1", &self.args.rs1.x);
                        debug_struct.field("rs2", &self.args.rs2.x);
                    }
                    ArgsShape::FSrcFDest => {
                        debug_struct.field("rd", &self.args.rd.f);
                        debug_struct.field("rs1", &self.args.rs1.f);
                        debug_struct.field("rs2", &self.args.rs2.f);
                    }
                    ArgsShape::XSrcFDest => {
                        debug_struct.field("rd", &self.args.rd.f);
                        debug_struct.field("rs1", &self.args.rs1.x);
                        debug_struct.field("rs2", &self.args.rs2.x);
                    }
                    ArgsShape::FSrcXDest => {
                        debug_struct.field("rd", &self.args.rd.x);
                        debug_struct.field("rs1", &self.args.rs1.f);
                        debug_struct.field("rs2", &self.args.rs2.f);
                    }
                    ArgsShape::XSrcFSrc => {
                        debug_struct.field("rd", &self.args.rd.x);
                        debug_struct.field("rs1", &self.args.rs1.x);
                        debug_struct.field("rs2", &self.args.rs2.f);
                    }
                    ArgsShape::NZXSrcNZXDest => {
                        debug_struct.field("rd", &self.args.rd.nzx);
                        debug_struct.field("rs1", &self.args.rs1.nzx);
                        debug_struct.field("rs2", &self.args.rs2.nzx);
                    }
                    ArgsShape::XSrcNZXDest => {
                        debug_struct.field("rd", &self.args.rd.nzx);
                        debug_struct.field("rs1", &self.args.rs1.x);
                        debug_struct.field("rs2", &self.args.rs2.x);
                    }
                }
            }
            debug_struct
                .field("imm", &self.args.imm)
                .field("csr", &self.args.csr)
                .field("rs3f", &self.args.rs3f)
                .field("rm", &self.args.rm)
                .field("aq", &self.args.aq)
                .field("rl", &self.args.rl)
                .field("width", &self.args.width)
                .finish()
        };
        let debug_args = DebugArgs(&debug_args);

        f.debug_struct("Instruction")
            .field("opcode", &self.opcode)
            .field("args", &debug_args)
            .finish()
    }
}

impl Instruction {
    /// Returns the width of the instruction: either compressed or uncompressed.
    pub const fn width(&self) -> InstrWidth {
        self.args.width
    }

    /// Returns a reference to the arguments of an instruction.
    pub fn args(&self) -> &Args {
        &self.args
    }
}

impl ConstDefault for Instruction {
    const DEFAULT: Self = Instruction {
        opcode: OpCode::Unknown,
        args: Args::DEFAULT,
    };
}

/// alias for the function signature of an instruction run.
///
/// SAFETY: This function must be called with an `Args` belonging to the same `OpCode` as
/// the one used to dispatch this function.
pub type RunInstr<MC, M> = unsafe fn(
    &Args,
    &mut MachineCoreState<MC, M>,
) -> Result<ProgramCounterUpdate<Address>, Exception>;

/// Opcodes map to the operation performed over the state - allowing us to
/// decouple these from the parsed instructions down the line.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum OpCode {
    Unknown,

    // RV64I R-type instructions
    Add,
    Sub,
    X64Xor,
    Or,
    And,
    ShiftLeft,
    ShiftRightUnsigned,
    ShiftRightSigned,
    SetLessThanSigned,
    SetLessThanUnsigned,
    AddWord,
    SubWord,
    X32ShiftLeft,
    X32ShiftRightSigned,
    X32ShiftRightUnsigned,

    // RV64I I-type instructions
    Addi,
    AddWordImmediate,
    X64XorImm,
    X64OrImm,
    Andi,
    ShiftLeftImmediate,
    ShiftRightImmediateUnsigned,
    ShiftRightImmediateSigned,
    X32ShiftLeftImm,
    X32ShiftRightImmSigned,
    X32ShiftRightImmUnsigned,
    SetLessThanImmediateSigned,
    SetLessThanImmediateUnsigned,
    X8LoadSigned,
    X16LoadSigned,
    X32LoadSigned,
    X8LoadUnsigned,
    X16LoadUnsigned,
    X32LoadUnsigned,
    X64LoadSigned,

    // RV64I S-type instructions
    X8Store,
    X16Store,
    X32Store,
    X64Store,

    // RV64I B-type instructions
    BranchEqual,
    BranchNotEqual,
    BranchLessThanSigned,
    BranchGreaterThanOrEqualSigned,
    BranchLessThanUnsigned,
    BranchGreaterThanOrEqualUnsigned,

    // RV64I U-type instructions
    AddImmediateToPC,

    // RV64I jump instructions
    Jal,
    /// Previous `Jalr`. Same as current `Jalr` except jump to `val(rs1) + imm`.
    JalrImm,

    // RV64A R-type atomic instructions
    Lrw,
    Scw,
    Amoswapw,
    Amoaddw,
    Amoxorw,
    Amoandw,
    Amoorw,
    Amominw,
    Amomaxw,
    Amominuw,
    Amomaxuw,
    Lrd,
    Scd,
    Amoswapd,
    X64AtomicAdd,
    Amoxord,
    Amoandd,
    Amoord,
    Amomind,
    Amomaxd,
    Amominud,
    Amomaxud,

    // RV64M division instructions
    Rem,
    Remu,
    Remw,
    Remuw,
    X64DivSigned,
    Divu,
    Divw,
    Divuw,
    Mul,
    X64MulHighSigned,
    X64MulHighSignedUnsigned,
    X64MulHighUnsigned,
    X32Mul,

    // RV64F instructions
    FclassS,
    Feqs,
    Fles,
    Flts,
    Fadds,
    Fsubs,
    Fmuls,
    Fdivs,
    Fsqrts,
    Fmins,
    Fmaxs,
    Fmadds,
    Fmsubs,
    Fnmsubs,
    Fnmadds,
    Flw,
    Fsw,
    Fcvtsw,
    Fcvtswu,
    Fcvtsl,
    Fcvtslu,
    Fcvtws,
    Fcvtwus,
    Fcvtls,
    Fcvtlus,
    Fsgnjs,
    Fsgnjns,
    Fsgnjxs,
    FmvXW,
    FmvWX,

    // RV64D instructions
    FclassD,
    Feqd,
    Fled,
    Fltd,
    Faddd,
    Fsubd,
    Fmuld,
    Fdivd,
    Fsqrtd,
    Fmind,
    Fmaxd,
    Fmaddd,
    Fmsubd,
    Fnmsubd,
    Fnmaddd,
    Fld,
    Fsd,
    Fcvtdw,
    Fcvtdwu,
    Fcvtdl,
    Fcvtdlu,
    Fcvtds,
    Fcvtsd,
    Fcvtwd,
    Fcvtwud,
    Fcvtld,
    Fcvtlud,
    Fsgnjd,
    Fsgnjnd,
    Fsgnjxd,
    FmvXD,
    FmvDX,

    // Zicsr instructions
    Csrrw,
    Csrrs,
    Csrrc,
    Csrrwi,
    Csrrsi,
    Csrrci,

    /// Jumps to val(rs1)
    Jr,
    /// Effects are to store the next instruction address in rd and jump to val(rs1).
    Jalr,

    // RV64DC compressed instructions
    CFld,
    CFldsp,
    CFsd,
    CFsdsp,

    // Internal OpCodes
    BranchEqualZero,
    BranchNotEqualZero,
    J,
    Mv,
    Li,
    Nop,
    Neg,
    /// Jump to absolute address (internal `J` opcode jumps to `val(rs1) + imm`,
    /// whilst this just jumps to `imm`).
    JAbsolute,
    /// Jump to absolute address `imm` and link register.
    /// Same as `JAbsolute` but also stores next instr address in rd.
    JalrAbsolute,
    /// Same as `Jr` but jumps to `val(rs1) + imm`.
    JrImm,
    /// Jump to `pc + imm` if `val(rs2) < 0`.
    BranchLessThanZero,
    /// Jump to `pc + imm` if `val(rs2) >= 0`.
    BranchGreaterThanOrEqualZero,
    /// Jump to `pc + imm` if `val(rs2) <= 0`.
    BranchLessThanOrEqualZero,
    /// Jump to `pc + imm` if `val(rs2) > 0`.
    BranchGreaterThanZero,

    /// Performs an environment call, from the current
    /// machine mode.
    ECall,
}

impl OpCode {
    /// Dispatch an opcode to the function that will run over the machine state.
    ///
    /// # SAFETY
    /// Calling the returned function **must** correspond to an `Args` belonging to an
    /// instruction where the `OpCode` is the same as the `OpCode` of the current instruction.
    #[inline(always)]
    pub(super) fn to_run<MC: MemoryConfig, M: ManagerReadWrite>(self) -> RunInstr<MC, M> {
        match self {
            Self::Add => Args::run_add,
            Self::Sub => Args::run_sub,
            Self::Neg => Args::run_neg,
            Self::X64Xor => Args::run_x64_xor,
            Self::Or => Args::run_or,
            Self::And => Args::run_and,
            Self::ShiftLeft => Args::run_shift_left,
            Self::ShiftRightUnsigned => Args::run_shift_right_unsigned,
            Self::ShiftRightSigned => Args::run_shift_right_signed,
            Self::SetLessThanSigned => Args::run_set_less_than_signed,
            Self::SetLessThanUnsigned => Args::run_set_less_than_unsigned,
            Self::AddWord => Args::run_add_word,
            Self::SubWord => Args::run_sub_word,
            Self::X32ShiftLeft => Args::run_x32_shift_left,
            Self::X32ShiftRightUnsigned => Args::run_x32_shift_right_unsigned,
            Self::X32ShiftRightSigned => Args::run_x32_shift_right_signed,
            Self::Addi => Args::run_addi,
            Self::AddWordImmediate => Args::run_add_word_immediate,
            Self::X64XorImm => Args::run_x64_xor_immediate,
            Self::X64OrImm => Args::run_x64_or_immediate,
            Self::Andi => Args::run_andi,
            Self::ShiftLeftImmediate => Args::run_shift_left_immediate,
            Self::ShiftRightImmediateUnsigned => Args::run_shift_right_immediate_unsigned,
            Self::ShiftRightImmediateSigned => Args::run_shift_right_immediate_signed,
            Self::X32ShiftLeftImm => Args::run_x32_shift_left_imm,
            Self::X32ShiftRightImmUnsigned => Args::run_x32_shift_right_imm_unsigned,
            Self::X32ShiftRightImmSigned => Args::run_x32_shift_right_imm_signed,
            Self::SetLessThanImmediateSigned => Args::run_set_less_than_immediate_signed,
            Self::SetLessThanImmediateUnsigned => Args::run_set_less_than_immediate_unsigned,
            Self::X8LoadSigned => Args::run_x8_load_signed,
            Self::X16LoadSigned => Args::run_x16_load_signed,
            Self::X32LoadSigned => Args::run_x32_load_signed,
            Self::X8LoadUnsigned => Args::run_x8_load_unsigned,
            Self::X16LoadUnsigned => Args::run_x16_load_unsigned,
            Self::X32LoadUnsigned => Args::run_x32_load_unsigned,
            Self::X64LoadSigned => Args::run_x64_load_signed,
            Self::X8Store => Args::run_x8_store,
            Self::X16Store => Args::run_x16_store,
            Self::X32Store => Args::run_x32_store,
            Self::X64Store => Args::run_x64_store,
            Self::BranchEqual => Args::run_branch_equal,
            Self::BranchNotEqual => Args::run_branch_not_equal,
            Self::BranchLessThanSigned => Args::run_branch_less_than_signed,
            Self::BranchGreaterThanOrEqualSigned => Args::run_branch_greater_than_or_equal_signed,
            Self::BranchLessThanZero => Args::run_branch_less_than_zero,
            Self::BranchGreaterThanOrEqualZero => Args::run_branch_greater_than_or_equal_zero,
            Self::BranchLessThanOrEqualZero => Args::run_branch_less_than_equal_zero,
            Self::BranchGreaterThanZero => Args::run_branch_greater_than_zero,
            Self::BranchLessThanUnsigned => Args::run_branch_less_than_unsigned,
            Self::BranchGreaterThanOrEqualUnsigned => {
                Args::run_branch_greater_than_or_equal_unsigned
            }
            Self::AddImmediateToPC => Args::run_add_immediate_to_pc,
            Self::Jal => Args::run_jal,
            Self::JalrImm => Args::run_jalr_imm,
            Self::JrImm => Args::run_jr_imm,
            Self::JalrAbsolute => Args::run_jalr_absolute,
            Self::Lrw => Args::run_lrw,
            Self::Scw => Args::run_scw,
            Self::Amoswapw => Args::run_amoswapw,
            Self::Amoaddw => Args::run_amoaddw,
            Self::Amoxorw => Args::run_amoxorw,
            Self::Amoandw => Args::run_amoandw,
            Self::Amoorw => Args::run_amoorw,
            Self::Amominw => Args::run_amominw,
            Self::Amomaxw => Args::run_amomaxw,
            Self::Amominuw => Args::run_amominuw,
            Self::Amomaxuw => Args::run_amomaxuw,
            Self::Lrd => Args::run_lrd,
            Self::Scd => Args::run_scd,
            Self::Amoswapd => Args::run_amoswapd,
            Self::X64AtomicAdd => Args::run_x64_atomic_add,
            Self::Amoxord => Args::run_amoxord,
            Self::Amoandd => Args::run_amoandd,
            Self::Amoord => Args::run_amoord,
            Self::Amomind => Args::run_amomind,
            Self::Amomaxd => Args::run_amomaxd,
            Self::Amominud => Args::run_amominud,
            Self::Amomaxud => Args::run_amomaxud,
            Self::Rem => Args::run_rem,
            Self::Remu => Args::run_remu,
            Self::Remw => Args::run_remw,
            Self::Remuw => Args::run_remuw,
            Self::X64DivSigned => Args::run_x64_div_signed,
            Self::Divu => Args::run_divu,
            Self::Divw => Args::run_divw,
            Self::Divuw => Args::run_divuw,
            Self::Mul => Args::run_mul,
            Self::X64MulHighSigned => Args::run_x64_mul_high_signed,
            Self::X64MulHighSignedUnsigned => Args::run_x64_mul_high_signed_unsigned,
            Self::X64MulHighUnsigned => Args::run_x64_mul_high_unsigned,
            Self::X32Mul => Args::run_x32_mul,
            Self::FclassS => Args::run_fclass_s,
            Self::Feqs => Args::run_feq_s,
            Self::Fles => Args::run_fle_s,
            Self::Flts => Args::run_flt_s,
            Self::Fadds => Args::run_fadd_s,
            Self::Fsubs => Args::run_fsub_s,
            Self::Fmuls => Args::run_fmul_s,
            Self::Fdivs => Args::run_fdiv_s,
            Self::Fsqrts => Args::run_fsqrt_s,
            Self::Fmins => Args::run_fmin_s,
            Self::Fmaxs => Args::run_fmax_s,
            Self::Fmadds => Args::run_fmadd_s,
            Self::Fmsubs => Args::run_fmsub_s,
            Self::Fnmsubs => Args::run_fnmsub_s,
            Self::Fnmadds => Args::run_fnmadd_s,
            Self::Flw => Args::run_flw,
            Self::Fsw => Args::run_fsw,
            Self::Fcvtsw => Args::run_fcvt_s_w,
            Self::Fcvtswu => Args::run_fcvt_s_wu,
            Self::Fcvtsl => Args::run_fcvt_s_l,
            Self::Fcvtslu => Args::run_fcvt_s_lu,
            Self::Fcvtws => Args::run_fcvt_w_s,
            Self::Fcvtwus => Args::run_fcvt_wu_s,
            Self::Fcvtls => Args::run_fcvt_l_s,
            Self::Fcvtlus => Args::run_fcvt_lu_s,
            Self::Fsgnjs => Args::run_fsgnj_s,
            Self::Fsgnjns => Args::run_fsgnjn_s,
            Self::Fsgnjxs => Args::run_fsgnjx_s,
            Self::FmvXW => Args::run_fmv_x_w,
            Self::FmvWX => Args::run_fmv_w_x,
            Self::FclassD => Args::run_fclass_d,
            Self::Feqd => Args::run_feq_d,
            Self::Fled => Args::run_fle_d,
            Self::Fltd => Args::run_flt_d,
            Self::Faddd => Args::run_fadd_d,
            Self::Fsubd => Args::run_fsub_d,
            Self::Fmuld => Args::run_fmul_d,
            Self::Fdivd => Args::run_fdiv_d,
            Self::Fsqrtd => Args::run_fsqrt_d,
            Self::Fmind => Args::run_fmin_d,
            Self::Fmaxd => Args::run_fmax_d,
            Self::Fmaddd => Args::run_fmadd_d,
            Self::Fmsubd => Args::run_fmsub_d,
            Self::Fnmsubd => Args::run_fnmsub_d,
            Self::Fnmaddd => Args::run_fnmadd_d,
            Self::Fld => Args::run_fld,
            Self::Fsd => Args::run_fsd,
            Self::Fcvtdw => Args::run_fcvt_d_w,
            Self::Fcvtdwu => Args::run_fcvt_d_wu,
            Self::Fcvtdl => Args::run_fcvt_d_l,
            Self::Fcvtdlu => Args::run_fcvt_d_lu,
            Self::Fcvtds => Args::run_fcvt_d_s,
            Self::Fcvtsd => Args::run_fcvt_s_d,
            Self::Fcvtwd => Args::run_fcvt_w_d,
            Self::Fcvtwud => Args::run_fcvt_wu_d,
            Self::Fcvtld => Args::run_fcvt_l_d,
            Self::Fcvtlud => Args::run_fcvt_lu_d,
            Self::Fsgnjd => Args::run_fsgnj_d,
            Self::Fsgnjnd => Args::run_fsgnjn_d,
            Self::Fsgnjxd => Args::run_fsgnjx_d,
            Self::FmvXD => Args::run_fmv_x_d,
            Self::FmvDX => Args::run_fmv_d_x,
            Self::Csrrw => Args::run_csrrw,
            Self::Csrrs => Args::run_csrrs,
            Self::Csrrc => Args::run_csrrc,
            Self::Csrrwi => Args::run_csrrwi,
            Self::Csrrsi => Args::run_csrrsi,
            Self::Csrrci => Args::run_csrrci,
            Self::J => Args::run_j,
            Self::JAbsolute => Args::run_j_absolute,
            Self::Jr => Args::run_jr,
            Self::Jalr => Args::run_jalr,
            Self::BranchEqualZero => Args::run_branch_equal_zero,
            Self::BranchNotEqualZero => Args::run_branch_not_equal_zero,
            Self::Li => Args::run_li,
            Self::Mv => Args::run_mv,
            Self::Nop => Args::run_nop,
            Self::CFld => Args::run_cfld,
            Self::CFldsp => Args::run_cfldsp,
            Self::CFsd => Args::run_cfsd,
            Self::CFsdsp => Args::run_cfsdsp,
            Self::Unknown => Args::run_illegal,
            Self::ECall => Args::run_ecall,
        }
    }

    /// Dispatch an opcode to the function that can 'lower' the instruction to the JIT IR.
    ///
    /// This mechanism leverages the [InstructionContextBuilder] to do so.
    ///
    /// TODO (RV-394): this can be removed once all opcodes are supported, with [`OpCode::to_run`] being
    /// used instead.
    ///
    /// # SAFETY
    /// Calling the returned function **must** correspond to an `Args` belonging to an
    /// instruction where the `OpCode` is the same as the `OpCode` of the current instruction.
    ///
    /// [InstructionContextBuilder]: ICB
    #[inline(always)]
    pub(crate) fn to_lowering<I: ICB>(self) -> Option<IcbLoweringFn<I>> {
        match self {
            Self::Mv => Some(Args::run_mv),
            Self::Neg => Some(Args::run_neg),
            Self::Nop => Some(Args::run_nop),
            Self::Add => Some(Args::run_add),
            Self::AddWord => Some(Args::run_add_word),
            Self::AddWordImmediate => Some(Args::run_add_word_immediate),
            Self::Sub => Some(Args::run_sub),
            Self::SubWord => Some(Args::run_sub_word),
            Self::And => Some(Args::run_and),
            Self::Or => Some(Args::run_or),
            Self::X64OrImm => Some(Args::run_x64_or_immediate),
            Self::X64Xor => Some(Args::run_x64_xor),
            Self::X64XorImm => Some(Args::run_x64_xor_immediate),
            Self::Mul => Some(Args::run_mul),
            Self::X32Mul => Some(Args::run_x32_mul),
            Self::X64MulHighSigned => Some(Args::run_x64_mul_high_signed),
            Self::X64MulHighSignedUnsigned => Some(Args::run_x64_mul_high_signed_unsigned),
            Self::X64MulHighUnsigned => Some(Args::run_x64_mul_high_unsigned),
            Self::X64DivSigned => Some(Args::run_x64_div_signed),
            Self::Li => Some(Args::run_li),
            Self::AddImmediateToPC => Some(Args::run_add_immediate_to_pc),
            Self::J => Some(Args::run_j),
            Self::Jr => Some(Args::run_jr),
            Self::JrImm => Some(Args::run_jr_imm),
            Self::JAbsolute => Some(Args::run_j_absolute),
            Self::Jal => Some(Args::run_jal),
            Self::Jalr => Some(Args::run_jalr),
            Self::JalrImm => Some(Args::run_jalr_imm),
            Self::JalrAbsolute => Some(Args::run_jalr_absolute),
            Self::Addi => Some(Args::run_addi),
            Self::Andi => Some(Args::run_andi),
            Self::SetLessThanSigned => Some(Args::run_set_less_than_signed),
            Self::SetLessThanUnsigned => Some(Args::run_set_less_than_unsigned),
            Self::SetLessThanImmediateSigned => Some(Args::run_set_less_than_immediate_signed),
            Self::SetLessThanImmediateUnsigned => Some(Args::run_set_less_than_immediate_unsigned),
            // Branching instructions
            Self::BranchEqual => Some(Args::run_branch_equal),
            Self::BranchEqualZero => Some(Args::run_branch_equal_zero),
            Self::BranchNotEqual => Some(Args::run_branch_not_equal),
            Self::BranchNotEqualZero => Some(Args::run_branch_not_equal_zero),

            Self::BranchLessThanSigned => Some(Args::run_branch_less_than_signed),
            Self::BranchLessThanUnsigned => Some(Args::run_branch_less_than_unsigned),
            Self::BranchLessThanZero => Some(Args::run_branch_less_than_zero),
            Self::BranchLessThanOrEqualZero => Some(Args::run_branch_less_than_equal_zero),

            Self::BranchGreaterThanOrEqualSigned => {
                Some(Args::run_branch_greater_than_or_equal_signed)
            }
            Self::BranchGreaterThanOrEqualUnsigned => {
                Some(Args::run_branch_greater_than_or_equal_unsigned)
            }
            Self::BranchGreaterThanOrEqualZero => Some(Args::run_branch_greater_than_or_equal_zero),
            Self::BranchGreaterThanZero => Some(Args::run_branch_greater_than_zero),

            Self::ShiftLeft => Some(Args::run_shift_left),
            Self::ShiftRightUnsigned => Some(Args::run_shift_right_unsigned),
            Self::ShiftRightSigned => Some(Args::run_shift_right_signed),
            Self::ShiftLeftImmediate => Some(Args::run_shift_left_immediate),
            Self::ShiftRightImmediateUnsigned => Some(Args::run_shift_right_immediate_unsigned),
            Self::ShiftRightImmediateSigned => Some(Args::run_shift_right_immediate_signed),
            Self::X32ShiftLeft => Some(Args::run_x32_shift_left),
            Self::X32ShiftRightUnsigned => Some(Args::run_x32_shift_right_unsigned),
            Self::X32ShiftRightSigned => Some(Args::run_x32_shift_right_signed),
            Self::X32ShiftLeftImm => Some(Args::run_x32_shift_left_imm),
            Self::X32ShiftRightImmUnsigned => Some(Args::run_x32_shift_right_imm_unsigned),
            Self::X32ShiftRightImmSigned => Some(Args::run_x32_shift_right_imm_signed),

            // Stores
            Self::X64Store => Some(Args::run_x64_store),
            Self::X32Store => Some(Args::run_x32_store),
            Self::X16Store => Some(Args::run_x16_store),
            Self::X8Store => Some(Args::run_x8_store),

            // Loads
            Self::X64LoadSigned => Some(Args::run_x64_load_signed),
            Self::X32LoadSigned => Some(Args::run_x32_load_signed),
            Self::X32LoadUnsigned => Some(Args::run_x32_load_unsigned),
            Self::X16LoadSigned => Some(Args::run_x16_load_signed),
            Self::X16LoadUnsigned => Some(Args::run_x16_load_unsigned),
            Self::X8LoadSigned => Some(Args::run_x8_load_signed),
            Self::X8LoadUnsigned => Some(Args::run_x8_load_unsigned),

            // Atomic instructions
            Self::X64AtomicAdd => Some(Args::run_x64_atomic_add),

            // Errors
            Self::Unknown => Some(Args::run_illegal),
            Self::ECall => Some(Args::run_ecall),
            _ => None,
        }
    }
}

impl Instruction {
    /// Run an instruction over the machine core state.
    pub(super) fn run<MC: MemoryConfig, M: ManagerReadWrite>(
        &self,
        core: &mut MachineCoreState<MC, M>,
    ) -> Result<ProgramCounterUpdate<Address>, Exception> {
        // SAFETY: Unsafe accesses in this function are due to using the [Register] union,
        // which is safe as the registers used are validated against the opcode.
        unsafe { (self.opcode.to_run())(&self.args, core) }
    }
}

/// A union of the X and F registers, which are both represented as u8.
#[derive(Clone, Copy)]
pub union Register {
    pub x: XRegister,
    pub f: FRegister,
    pub nzx: NonZeroXRegister,
}

impl ConstDefault for Register {
    const DEFAULT: Self = Self {
        nzx: NonZeroXRegister::x1,
    };
}

impl From<XRegister> for Register {
    fn from(x: XRegister) -> Self {
        Self { x }
    }
}

impl From<FRegister> for Register {
    fn from(f: FRegister) -> Self {
        Self { f }
    }
}

impl From<NonZeroXRegister> for Register {
    fn from(nzx: NonZeroXRegister) -> Self {
        Self { nzx }
    }
}

impl PartialEq for Register {
    fn eq(&self, other: &Self) -> bool {
        // SAFETY: XRegister and FRegister are the same size as u8 and directly mappable to it.
        unsafe {
            std::mem::transmute::<&Self, &u8>(self) == std::mem::transmute::<&Self, &u8>(other)
        }
    }
}

impl Eq for Register {}

/// Contains all possible arguments used by opcode-functions.
///
/// Each opcode will only touch a subset of these.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Args {
    pub rd: Register,
    pub rs1: Register,
    pub rs2: Register,
    pub imm: i64,
    pub csr: CSRegister,
    pub rs3f: FRegister,
    pub rm: InstrRoundingMode,
    pub aq: bool,
    pub rl: bool,
    pub width: InstrWidth,
}

impl ConstDefault for Args {
    const DEFAULT: Self = Self {
        rd: Register::DEFAULT,
        rs1: Register::DEFAULT,
        rs2: Register::DEFAULT,
        imm: 0,
        csr: CSRegister::fflags,
        rs3f: FRegister::f0,
        rm: InstrRoundingMode::Dynamic,
        aq: false,
        rl: false,
        width: InstrWidth::Uncompressed,
    };
}

macro_rules! impl_r_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<MC: MemoryConfig, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<MC, M>,
        ) -> Result<ProgramCounterUpdate<Address>, Exception> {
            core.hart
                .xregisters
                .$fn(unsafe { self.rs1.x }, unsafe { self.rs2.x }, unsafe {
                    self.rd.x
                });
            Ok(Next(self.width))
        }
    };

    ($fn: ident, non_zero) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<MC: MemoryConfig, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<MC, M>,
        ) -> Result<ProgramCounterUpdate<Address>, Exception> {
            core.hart
                .xregisters
                .$fn(unsafe { self.rs1.nzx }, unsafe { self.rs2.nzx }, unsafe {
                    self.rd.nzx
                });
            Ok(Next(self.width))
        }
    };

    ($fn: ident, non_zero_rd) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<MC: MemoryConfig, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<MC, M>,
        ) -> Result<ProgramCounterUpdate<Address>, Exception> {
            core.hart
                .xregisters
                .$fn(unsafe { self.rs1.x }, unsafe { self.rs2.x }, unsafe {
                    self.rd.nzx
                });
            Ok(Next(self.width))
        }
    };

    ($impl: path, $fn: ident, non_zero) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
            $impl(
                icb,
                unsafe { self.rs1.nzx },
                unsafe { self.rs2.nzx },
                unsafe { self.rd.nzx },
            );
            let pcu = ProgramCounterUpdate::Next(self.width);
            icb.ok(pcu)
        }
    };

    ($impl: path, $fn: ident, non_zero_rd) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
            $impl(icb, unsafe { self.rs1.x }, unsafe { self.rs2.x }, unsafe {
                self.rd.nzx
            });
            icb.ok(Next(self.width))
        }
    };

    ($fn: ident, $shift: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
            integer::run_shift(
                icb,
                Shift::$shift,
                unsafe { self.rs1.nzx },
                unsafe { self.rs2.nzx },
                unsafe { self.rd.nzx },
            );
            icb.ok(Next(self.width))
        }
    };
}

macro_rules! impl_x32_shift_type {
    ($shift: ident, $fn: ident, reg) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
            let rs1 = unsafe { self.rs1.x };
            let rs2 = unsafe { self.rs2.x };
            let rd = unsafe { self.rd.nzx };
            integer::run_x32_shift(icb, Shift::$shift, rs1, rs2, rd);
            icb.ok(Next(self.width))
        }
    };

    ($shift: ident, $fn: ident, imm) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
            let rs1 = unsafe { self.rs1.nzx };
            let rd = unsafe { self.rd.nzx };
            integer::run_x32_shift_immediate(icb, Shift::$shift, rs1, self.imm, rd);
            icb.ok(Next(self.width))
        }
    };
}

macro_rules! impl_x64_mul_high_type {
    ($fn: ident, $mul_high_type: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
            integer::run_x64_mul_high(
                icb,
                unsafe { self.rs1.nzx },
                unsafe { self.rs2.nzx },
                unsafe { self.rd.nzx },
                MulHighType::$mul_high_type,
            );
            icb.ok(Next(self.width))
        }
    };
}

macro_rules! impl_i_type {
    ($fn: ident, non_zero) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<MC: MemoryConfig, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<MC, M>,
        ) -> Result<ProgramCounterUpdate<Address>, Exception> {
            core.hart
                .xregisters
                .$fn(self.imm, unsafe { self.rs1.nzx }, unsafe { self.rd.nzx });
            Ok(Next(self.width))
        }
    };

    ($fn: ident, non_zero_rd) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<MC: MemoryConfig, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<MC, M>,
        ) -> Result<ProgramCounterUpdate<Address>, Exception> {
            core.hart
                .xregisters
                .$fn(self.imm, unsafe { self.rs1.x }, unsafe { self.rd.nzx });
            Ok(Next(self.width))
        }
    };

    ($impl: path, $fn: ident, non_zero) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
            $impl(icb, self.imm, unsafe { self.rs1.nzx }, unsafe {
                self.rd.nzx
            });
            icb.ok(Next(self.width))
        }
    };

    ($impl: path, $fn: ident, non_zero_rd) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
            $impl(icb, self.imm, unsafe { self.rs1.x }, unsafe { self.rd.nzx });
            icb.ok(Next(self.width))
        }
    };

    ($fn: ident, $shift: path) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
            integer::run_shift_immediate(icb, $shift, self.imm, unsafe { self.rs1.nzx }, unsafe {
                self.rd.nzx
            });
            icb.ok(Next(self.width))
        }
    };
}

macro_rules! impl_fload_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<MC: MemoryConfig, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<MC, M>,
        ) -> Result<ProgramCounterUpdate<Address>, Exception> {
            core.$fn(self.imm, unsafe { self.rs1.x }, unsafe { self.rd.f })
                .map(|_| Next(self.width))
        }
    };
}
macro_rules! impl_load_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<MC: MemoryConfig, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<MC, M>,
        ) -> Result<ProgramCounterUpdate<Address>, Exception> {
            core.$fn(self.imm, unsafe { self.rs1.x }, unsafe { self.rd.x })
                .map(|_| Next(self.width))
        }
    };

    ($fn: ident, $width: expr, $signed: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
            let res = load_store::run_load(
                icb,
                self.imm,
                unsafe { self.rs1.x },
                unsafe { self.rd.x },
                $signed,
                $width,
            );
            I::map(res, |_| Next(self.width))
        }
    };
}

macro_rules! impl_cfload_sp_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<MC: MemoryConfig, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<MC, M>,
        ) -> Result<ProgramCounterUpdate<Address>, Exception> {
            core.$fn(self.imm, unsafe { self.rd.f })
                .map(|_| Next(self.width))
        }
    };
}

macro_rules! impl_store_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<MC: MemoryConfig, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<MC, M>,
        ) -> Result<ProgramCounterUpdate<Address>, Exception> {
            core.$fn(self.imm, unsafe { self.rs1.x }, unsafe { self.rs2.x })
                .map(|_| Next(self.width))
        }
    };

    ($fn: ident, $width: expr) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
            let res = load_store::run_store(
                icb,
                self.imm,
                unsafe { self.rs1.x },
                unsafe { self.rs2.x },
                $width,
            );
            I::map(res, |_| Next(self.width))
        }
    };
}
macro_rules! impl_fstore_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<MC: MemoryConfig, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<MC, M>,
        ) -> Result<ProgramCounterUpdate<Address>, Exception> {
            core.$fn(self.imm, unsafe { self.rs1.x }, unsafe { self.rs2.f })
                .map(|_| Next(self.width))
        }
    };
}

macro_rules! impl_branch {
    ($fn: ident, $predicate: expr) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
            let pcu = branching::run_branch(
                icb,
                $predicate,
                self.imm,
                unsafe { self.rs1.nzx },
                unsafe { self.rs2.nzx },
                self.width,
            );
            icb.ok(pcu)
        }
    };
}

macro_rules! impl_branch_compare_zero {
    ($fn: ident, $predicate: expr) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
            let pcu = branching::run_branch_compare_zero(
                icb,
                $predicate,
                self.imm,
                unsafe { self.rs1.nzx },
                self.width,
            );
            icb.ok(pcu)
        }
    };
}

macro_rules! impl_amo_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<MC: MemoryConfig, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<MC, M>,
        ) -> Result<ProgramCounterUpdate<Address>, Exception> {
            core.$fn(
                unsafe { self.rs1.x },
                unsafe { self.rs2.x },
                unsafe { self.rd.x },
                self.rl,
                self.aq,
            )
            .map(|_| Next(self.width))
        }
    };

    ($impl: path, $fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
            let res = $impl(
                icb,
                unsafe { self.rs1.x },
                unsafe { self.rs2.x },
                unsafe { self.rd.x },
                self.rl,
                self.aq,
            );
            I::map(res, |_| Next(self.width))
        }
    };
}

macro_rules! impl_ci_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<MC: MemoryConfig, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<MC, M>,
        ) -> Result<ProgramCounterUpdate<Address>, Exception> {
            core.hart.xregisters.$fn(self.imm, unsafe { self.rd.x });
            Ok(ProgramCounterUpdate::Next(self.width))
        }
    };

    ($fn: ident, non_zero) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<MC: MemoryConfig, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<MC, M>,
        ) -> Result<ProgramCounterUpdate<Address>, Exception> {
            core.hart.xregisters.$fn(self.imm, unsafe { self.rd.nzx });
            Ok(ProgramCounterUpdate::Next(self.width))
        }
    };

    ($impl: path, $fn: ident, non_zero) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
            $impl(icb, self.imm, unsafe { self.rd.nzx });
            let pcu = ProgramCounterUpdate::Next(self.width);
            icb.ok(pcu)
        }
    };
}

macro_rules! impl_cr_nz_type {
    ($impl: path, $fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
            $impl(icb, unsafe { self.rd.nzx }, unsafe { self.rs2.nzx });
            let pcu = ProgramCounterUpdate::Next(self.width);
            icb.ok(pcu)
        }
    };
}

macro_rules! impl_fcss_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<MC: MemoryConfig, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<MC, M>,
        ) -> Result<ProgramCounterUpdate<Address>, Exception> {
            core.$fn(self.imm, unsafe { self.rs2.f })
                .map(|_| Next(self.width))
        }
    };
}

macro_rules! impl_csr_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<MC: MemoryConfig, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<MC, M>,
        ) -> Result<ProgramCounterUpdate<Address>, Exception> {
            core.hart
                .$fn(self.csr, unsafe { self.rs1.x }, unsafe { self.rd.x })
                .map(|_| Next(self.width))
        }
    };
}

macro_rules! impl_csr_imm_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<MC: MemoryConfig, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<MC, M>,
        ) -> Result<ProgramCounterUpdate<Address>, Exception> {
            core.hart
                .$fn(self.csr, self.imm as u64, unsafe { self.rd.x })
                .map(|_| Next(self.width))
        }
    };
}

macro_rules! impl_f_x_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<MC: MemoryConfig, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<MC, M>,
        ) -> Result<ProgramCounterUpdate<Address>, Exception> {
            core.hart
                .$fn(unsafe { self.rs1.x }, unsafe { self.rd.f })
                .map(|_| Next(self.width))
        }
    };

    ($fn:ident, rm) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<MC: MemoryConfig, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<MC, M>,
        ) -> Result<ProgramCounterUpdate<Address>, Exception> {
            core.hart
                .$fn(unsafe { self.rs1.x }, self.rm, unsafe { self.rd.f })
                .map(|_| Next(self.width))
        }
    };
}

macro_rules! impl_x_f_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<MC: MemoryConfig, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<MC, M>,
        ) -> Result<ProgramCounterUpdate<Address>, Exception> {
            core.hart
                .$fn(unsafe { self.rs1.f }, unsafe { self.rd.x })
                .map(|_| Next(self.width))
        }
    };

    ($fn:ident, rm) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<MC: MemoryConfig, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<MC, M>,
        ) -> Result<ProgramCounterUpdate<Address>, Exception> {
            core.hart
                .$fn(unsafe { self.rs1.f }, self.rm, unsafe { self.rd.x })
                .map(|_| Next(self.width))
        }
    };
}

macro_rules! impl_f_r_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<MC: MemoryConfig, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<MC, M>,
        ) -> Result<ProgramCounterUpdate<Address>, Exception> {
            core.hart
                .$fn(unsafe { self.rs1.f }, unsafe { self.rs2.f }, unsafe { self.rd.f })
                .map(|_| Next(self.width))
        }
    };

    ($fn: ident, (rd, x)) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<MC: MemoryConfig, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<MC, M>,
        ) -> Result<ProgramCounterUpdate<Address>, Exception> {
            core.hart
                .$fn(unsafe { self.rs1.f }, unsafe { self.rs2.f }, unsafe { self.rd.x })
                .map(|_| Next(self.width))
        }
    };

    ($fn: ident, rm) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<MC: MemoryConfig, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<MC, M>,
        ) -> Result<ProgramCounterUpdate<Address>, Exception> {
            core.hart
                .$fn(unsafe { self.rs1.f }, self.rm, unsafe { self.rd.f })
                .map(|_| Next(self.width))
        }
    };

    ($fn: ident, (rs2, f), $($field: ident),+) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<MC: MemoryConfig, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<MC, M>,
        ) -> Result<ProgramCounterUpdate<Address>, Exception> {
            core.hart
                .$fn(unsafe { self.rs1.f }, unsafe { self.rs2.f }, $(self.$field,)* unsafe { self.rd.f })
                .map(|_| Next(self.width))
        }
    };
}

impl Args {
    // RV64I R-type instructions
    impl_r_type!(integer::run_add, run_add, non_zero);
    impl_r_type!(integer::run_sub, run_sub, non_zero);
    impl_r_type!(integer::run_x64_xor, run_x64_xor, non_zero);
    impl_r_type!(integer::run_and, run_and, non_zero);
    impl_r_type!(integer::run_or, run_or, non_zero);
    impl_r_type!(run_shift_left, Left);
    impl_r_type!(run_shift_right_unsigned, RightUnsigned);
    impl_r_type!(run_shift_right_signed, RightSigned);
    impl_r_type!(
        integer::run_set_less_than_signed,
        run_set_less_than_signed,
        non_zero_rd
    );
    impl_r_type!(
        integer::run_set_less_than_unsigned,
        run_set_less_than_unsigned,
        non_zero_rd
    );
    impl_r_type!(integer::run_add_word, run_add_word, non_zero_rd);
    impl_r_type!(integer::run_sub_word, run_sub_word, non_zero_rd);
    impl_x32_shift_type!(Left, run_x32_shift_left, reg);
    impl_x32_shift_type!(RightUnsigned, run_x32_shift_right_unsigned, reg);
    impl_x32_shift_type!(RightSigned, run_x32_shift_right_signed, reg);

    // RV64I I-type instructions
    impl_i_type!(integer::run_addi, run_addi, non_zero);
    impl_i_type!(
        integer::run_add_word_immediate,
        run_add_word_immediate,
        non_zero_rd
    );
    impl_i_type!(
        integer::run_x64_xor_immediate,
        run_x64_xor_immediate,
        non_zero
    );
    impl_i_type!(
        integer::run_x64_or_immediate,
        run_x64_or_immediate,
        non_zero
    );
    impl_i_type!(integer::run_andi, run_andi, non_zero);
    impl_i_type!(run_shift_left_immediate, Shift::Left);
    impl_i_type!(run_shift_right_immediate_unsigned, Shift::RightUnsigned);
    impl_i_type!(run_shift_right_immediate_signed, Shift::RightSigned);
    impl_x32_shift_type!(Left, run_x32_shift_left_imm, imm);
    impl_x32_shift_type!(RightUnsigned, run_x32_shift_right_imm_unsigned, imm);
    impl_x32_shift_type!(RightSigned, run_x32_shift_right_imm_signed, imm);
    impl_i_type!(
        integer::run_set_less_than_immediate_signed,
        run_set_less_than_immediate_signed,
        non_zero_rd
    );
    impl_i_type!(
        integer::run_set_less_than_immediate_unsigned,
        run_set_less_than_immediate_unsigned,
        non_zero_rd
    );
    impl_load_type!(run_x8_load_unsigned, LoadStoreWidth::Byte, false);
    impl_load_type!(run_x16_load_unsigned, LoadStoreWidth::Half, false);
    impl_load_type!(run_x32_load_unsigned, LoadStoreWidth::Word, false);
    impl_load_type!(run_x64_load_signed, LoadStoreWidth::Double, true);
    impl_load_type!(run_x32_load_signed, LoadStoreWidth::Word, true);
    impl_load_type!(run_x16_load_signed, LoadStoreWidth::Half, true);
    impl_load_type!(run_x8_load_signed, LoadStoreWidth::Byte, true);

    // RV64I S-type instructions
    impl_store_type!(run_x64_store, LoadStoreWidth::Double);
    impl_store_type!(run_x32_store, LoadStoreWidth::Word);
    impl_store_type!(run_x16_store, LoadStoreWidth::Half);
    impl_store_type!(run_x8_store, LoadStoreWidth::Byte);

    // Branching instructions
    impl_branch!(run_branch_equal, Predicate::Equal);
    impl_branch!(run_branch_not_equal, Predicate::NotEqual);
    impl_branch!(run_branch_less_than_signed, Predicate::LessThanSigned);
    impl_branch!(run_branch_less_than_unsigned, Predicate::LessThanUnsigned);
    impl_branch!(
        run_branch_greater_than_or_equal_signed,
        Predicate::GreaterThanOrEqualSigned
    );
    impl_branch!(
        run_branch_greater_than_or_equal_unsigned,
        Predicate::GreaterThanOrEqualUnsigned
    );
    impl_branch_compare_zero!(run_branch_equal_zero, Predicate::Equal);
    impl_branch_compare_zero!(run_branch_not_equal_zero, Predicate::NotEqual);
    impl_branch_compare_zero!(run_branch_less_than_zero, Predicate::LessThanSigned);
    impl_branch_compare_zero!(
        run_branch_greater_than_or_equal_zero,
        Predicate::GreaterThanOrEqualSigned
    );
    impl_branch_compare_zero!(
        run_branch_less_than_equal_zero,
        Predicate::LessThanOrEqualSigned
    );
    impl_branch_compare_zero!(run_branch_greater_than_zero, Predicate::GreaterThanSigned);

    // RV64I U-type instructions
    /// SAFETY: This function must only be called on an `Args` belonging
    /// to the same OpCode as the OpCode used to derive this function.
    unsafe fn run_add_immediate_to_pc<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
        branching::run_add_immediate_to_pc(icb, self.imm, unsafe { self.rd.nzx });
        icb.ok(Next(self.width))
    }

    // RV64A atomic instructions
    impl_amo_type!(run_lrw);
    impl_amo_type!(run_scw);
    impl_amo_type!(run_amoswapw);
    impl_amo_type!(run_amoaddw);
    impl_amo_type!(run_amoxorw);
    impl_amo_type!(run_amoandw);
    impl_amo_type!(run_amoorw);
    impl_amo_type!(run_amominw);
    impl_amo_type!(run_amomaxw);
    impl_amo_type!(run_amominuw);
    impl_amo_type!(run_amomaxuw);
    impl_amo_type!(run_lrd);
    impl_amo_type!(run_scd);
    impl_amo_type!(run_amoswapd);
    impl_amo_type!(atomics::run_x64_atomic_add, run_x64_atomic_add);
    impl_amo_type!(run_amoxord);
    impl_amo_type!(run_amoandd);
    impl_amo_type!(run_amoord);
    impl_amo_type!(run_amomind);
    impl_amo_type!(run_amomaxd);
    impl_amo_type!(run_amominud);
    impl_amo_type!(run_amomaxud);

    // RV64M multiplication and division instructions
    impl_r_type!(run_rem);
    impl_r_type!(run_remu);
    impl_r_type!(run_remw);
    impl_r_type!(run_remuw);
    impl_r_type!(integer::run_x64_div_signed, run_x64_div_signed, non_zero_rd);
    impl_r_type!(run_divu);
    impl_r_type!(run_divw);
    impl_r_type!(run_divuw);
    impl_r_type!(integer::run_mul, run_mul, non_zero);
    impl_x64_mul_high_type!(run_x64_mul_high_signed, Signed);
    impl_x64_mul_high_type!(run_x64_mul_high_signed_unsigned, SignedUnsigned);
    impl_x64_mul_high_type!(run_x64_mul_high_unsigned, Unsigned);
    impl_r_type!(integer::run_x32_mul, run_x32_mul, non_zero_rd);

    // RV64F instructions
    impl_fload_type!(run_flw);
    impl_fstore_type!(run_fsw);
    impl_f_r_type!(run_feq_s, (rd, x));
    impl_f_r_type!(run_fle_s, (rd, x));
    impl_f_r_type!(run_flt_s, (rd, x));
    impl_f_r_type!(run_fadd_s, (rs2, f), rm);
    impl_f_r_type!(run_fsub_s, (rs2, f), rm);
    impl_f_r_type!(run_fmul_s, (rs2, f), rm);
    impl_f_r_type!(run_fdiv_s, (rs2, f), rm);
    impl_f_r_type!(run_fsqrt_s, rm);
    impl_f_r_type!(run_fmin_s);
    impl_f_r_type!(run_fmax_s);
    impl_f_r_type!(run_fsgnj_s);
    impl_f_r_type!(run_fsgnjn_s);
    impl_f_r_type!(run_fsgnjx_s);
    impl_f_r_type!(run_fmadd_s, (rs2, f), rs3f, rm);
    impl_f_r_type!(run_fmsub_s, (rs2, f), rs3f, rm);
    impl_f_r_type!(run_fnmsub_s, (rs2, f), rs3f, rm);
    impl_f_r_type!(run_fnmadd_s, (rs2, f), rs3f, rm);
    impl_x_f_type!(run_fclass_s);
    impl_x_f_type!(run_fmv_x_w);
    impl_f_x_type!(run_fmv_w_x);
    impl_f_x_type!(run_fcvt_s_w, rm);
    impl_f_x_type!(run_fcvt_s_wu, rm);
    impl_f_x_type!(run_fcvt_s_l, rm);
    impl_f_x_type!(run_fcvt_s_lu, rm);
    impl_x_f_type!(run_fcvt_w_s, rm);
    impl_x_f_type!(run_fcvt_wu_s, rm);
    impl_x_f_type!(run_fcvt_l_s, rm);
    impl_x_f_type!(run_fcvt_lu_s, rm);

    // RV64D instructions
    impl_fload_type!(run_fld);
    impl_fstore_type!(run_fsd);
    impl_f_r_type!(run_feq_d, (rd, x));
    impl_f_r_type!(run_fle_d, (rd, x));
    impl_f_r_type!(run_flt_d, (rd, x));
    impl_f_r_type!(run_fadd_d, (rs2, f), rm);
    impl_f_r_type!(run_fsub_d, (rs2, f), rm);
    impl_f_r_type!(run_fmul_d, (rs2, f), rm);
    impl_f_r_type!(run_fdiv_d, (rs2, f), rm);
    impl_f_r_type!(run_fsqrt_d, rm);
    impl_f_r_type!(run_fmin_d);
    impl_f_r_type!(run_fmax_d);
    impl_f_r_type!(run_fsgnj_d);
    impl_f_r_type!(run_fsgnjn_d);
    impl_f_r_type!(run_fsgnjx_d);
    impl_f_r_type!(run_fcvt_d_s, rm);
    impl_f_r_type!(run_fcvt_s_d, rm);
    impl_f_r_type!(run_fmadd_d, (rs2, f), rs3f, rm);
    impl_f_r_type!(run_fmsub_d, (rs2, f), rs3f, rm);
    impl_f_r_type!(run_fnmsub_d, (rs2, f), rs3f, rm);
    impl_f_r_type!(run_fnmadd_d, (rs2, f), rs3f, rm);
    impl_x_f_type!(run_fclass_d);
    impl_f_x_type!(run_fcvt_d_w, rm);
    impl_f_x_type!(run_fcvt_d_wu, rm);
    impl_f_x_type!(run_fcvt_d_l, rm);
    impl_f_x_type!(run_fcvt_d_lu, rm);
    impl_x_f_type!(run_fcvt_w_d, rm);
    impl_x_f_type!(run_fcvt_wu_d, rm);
    impl_x_f_type!(run_fcvt_l_d, rm);
    impl_x_f_type!(run_fcvt_lu_d, rm);
    impl_x_f_type!(run_fmv_x_d);
    impl_f_x_type!(run_fmv_d_x);

    // Zicsr instructions
    impl_csr_type!(run_csrrw);
    impl_csr_type!(run_csrrs);
    impl_csr_type!(run_csrrc);
    impl_csr_imm_type!(run_csrrwi);
    impl_csr_imm_type!(run_csrrsi);
    impl_csr_imm_type!(run_csrrci);

    // RV32C compressed instructions
    impl_cr_nz_type!(integer::run_mv, run_mv);
    impl_cr_nz_type!(integer::run_neg, run_neg);
    impl_ci_type!(load_store::run_li, run_li, non_zero);

    fn run_j<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
        let addr = branching::run_j(icb, self.imm);
        let pcu = ProgramCounterUpdate::Set(addr);
        icb.ok(pcu)
    }

    fn run_j_absolute<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
        let addr = branching::run_j_absolute(icb, self.imm);
        let pcu = ProgramCounterUpdate::Set(addr);
        icb.ok(pcu)
    }

    /// SAFETY: This function must only be called on an `Args` belonging
    /// to the same OpCode as the OpCode used to derive this function.
    unsafe fn run_jr<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
        let rs1 = unsafe { self.rs1.nzx };
        let addr = branching::run_jr(icb, rs1);
        let pcu = ProgramCounterUpdate::Set(addr);
        icb.ok(pcu)
    }

    /// SAFETY: This function must only be called on an `Args` belonging
    /// to the same OpCode as the OpCode used to derive this function.
    unsafe fn run_jr_imm<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
        let rs1 = unsafe { self.rs1.nzx };
        let addr = branching::run_jr_imm(icb, self.imm, rs1);
        let pcu = ProgramCounterUpdate::Set(addr);
        icb.ok(pcu)
    }

    /// SAFETY: This function must only be called on an `Args` belonging
    /// to the same OpCode as the OpCode used to derive this function.
    unsafe fn run_jal<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
        let rd = unsafe { self.rd.nzx };
        let addr = branching::run_jal(icb, self.imm, rd, self.width);
        let pcu = ProgramCounterUpdate::Set(addr);
        icb.ok(pcu)
    }

    /// SAFETY: This function must only be called on an `Args` belonging
    /// to the same OpCode as the OpCode used to derive this function.
    unsafe fn run_jalr<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
        let rd = unsafe { self.rd.nzx };
        let rs1 = unsafe { self.rs1.nzx };
        let addr = branching::run_jalr(icb, rd, rs1, self.width);
        let pcu = ProgramCounterUpdate::Set(addr);
        icb.ok(pcu)
    }

    /// SAFETY: This function must only be called on an `Args` belonging
    /// to the same OpCode as the OpCode used to derive this function.
    unsafe fn run_jalr_imm<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
        let rs1 = unsafe { self.rs1.nzx };
        let rd = unsafe { self.rd.nzx };
        let addr = branching::run_jalr_imm(icb, self.imm, rs1, rd, self.width);
        let pcu = ProgramCounterUpdate::Set(addr);
        icb.ok(pcu)
    }

    /// SAFETY: This function must only be called on an `Args` belonging
    /// to the same OpCode as the OpCode used to derive this function.
    unsafe fn run_jalr_absolute<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
        let rd = unsafe { self.rd.nzx };
        let addr = branching::run_jalr_absolute(icb, self.imm, rd, self.width);
        let pcu = ProgramCounterUpdate::Set(addr);
        icb.ok(pcu)
    }

    fn run_nop<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
        integer::run_nop(icb);
        let pcu = ProgramCounterUpdate::Next(self.width);
        icb.ok(pcu)
    }

    fn run_ecall<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
        icb.ecall()
    }

    // RV64C compressed instructions
    impl_fload_type!(run_cfld);
    impl_cfload_sp_type!(run_cfldsp);
    impl_fstore_type!(run_cfsd);
    impl_fcss_type!(run_cfsdsp);

    // Unknown
    fn run_illegal<I: ICB>(&self, icb: &mut I) -> IcbFnResult<I> {
        icb.err_illegal_instruction()
    }
}

impl From<&InstrCacheable> for Instruction {
    fn from(value: &InstrCacheable) -> Self {
        match value {
            // RV64I R-type instructions
            InstrCacheable::Add(args) => Instruction::from_ic_add(args),
            InstrCacheable::Sub(args) => Instruction::from_ic_sub(args),
            InstrCacheable::Xor(args) => Instruction::from_ic_xor(args),
            InstrCacheable::Or(args) => Instruction::from_ic_or(args),
            InstrCacheable::And(args) => Instruction::from_ic_and(args),
            InstrCacheable::Sll(args) => Instruction::from_ic_sll(args),
            InstrCacheable::Srl(args) => Instruction::from_ic_srl(args),
            InstrCacheable::Sra(args) => Instruction::from_ic_sra(args),
            InstrCacheable::Slt(args) => {
                Instruction::new_set_less_than_signed(args.rd, args.rs1, args.rs2)
            }
            InstrCacheable::Sltu(args) => {
                Instruction::new_set_less_than_unsigned(args.rd, args.rs1, args.rs2)
            }
            InstrCacheable::Addw(args) => {
                Instruction::new_add_word(args.rd, args.rs1, args.rs2, InstrWidth::Uncompressed)
            }
            InstrCacheable::Subw(args) => {
                Instruction::new_sub_word(args.rd, args.rs1, args.rs2, InstrWidth::Uncompressed)
            }
            InstrCacheable::Sllw(args) => Instruction::new_x32_shift_left(
                args.rd,
                args.rs1,
                args.rs2,
                InstrWidth::Uncompressed,
            ),
            InstrCacheable::Srlw(args) => Instruction::new_x32_shift_right_unsigned(
                args.rd,
                args.rs1,
                args.rs2,
                InstrWidth::Uncompressed,
            ),
            InstrCacheable::Sraw(args) => Instruction::new_x32_shift_right_signed(
                args.rd,
                args.rs1,
                args.rs2,
                InstrWidth::Uncompressed,
            ),

            // RV64I I-type instructions
            InstrCacheable::Addi(args) => Instruction::from_ic_addi(args),
            InstrCacheable::Addiw(args) => Instruction::new_add_word_immediate(
                args.rd,
                args.rs1,
                args.imm,
                InstrWidth::Uncompressed,
            ),
            InstrCacheable::Xori(args) => Instruction::from_ic_xori(args),
            InstrCacheable::Ori(args) => Instruction::from_ic_ori(args),
            InstrCacheable::Andi(args) => Instruction::from_ic_andi(args),
            InstrCacheable::Slli(args) => Instruction::from_ic_slli(args),
            InstrCacheable::Srli(args) => Instruction::from_ic_srli(args),
            InstrCacheable::Srai(args) => Instruction::from_ic_srai(args),
            InstrCacheable::Slliw(args) => Instruction::from_ic_x32_shift_left_immediate(args),
            InstrCacheable::Srliw(args) => {
                Instruction::from_ic_x32_shift_right_immediate_unsigned(args)
            }
            InstrCacheable::Sraiw(args) => {
                Instruction::from_ic_x32_shift_right_immediate_signed(args)
            }
            InstrCacheable::Slti(args) => {
                Instruction::new_set_less_than_immediate_signed(args.rd, args.rs1, args.imm)
            }
            InstrCacheable::Sltiu(args) => {
                Instruction::new_set_less_than_immediate_unsigned(args.rd, args.rs1, args.imm)
            }
            InstrCacheable::Lb(args) => Instruction::new_x8_load_signed(
                args.rd,
                args.rs1,
                args.imm,
                InstrWidth::Uncompressed,
            ),
            InstrCacheable::Lh(args) => Instruction::new_x16_load_signed(
                args.rd,
                args.rs1,
                args.imm,
                InstrWidth::Uncompressed,
            ),
            InstrCacheable::Lw(args) => Instruction::new_x32_load_signed(
                args.rd,
                args.rs1,
                args.imm,
                InstrWidth::Uncompressed,
            ),
            InstrCacheable::Lbu(args) => Instruction::new_x8_load_unsigned(
                args.rd,
                args.rs1,
                args.imm,
                InstrWidth::Uncompressed,
            ),
            InstrCacheable::Lhu(args) => Instruction::new_x16_load_unsigned(
                args.rd,
                args.rs1,
                args.imm,
                InstrWidth::Uncompressed,
            ),
            InstrCacheable::Lwu(args) => Instruction::new_x32_load_unsigned(
                args.rd,
                args.rs1,
                args.imm,
                InstrWidth::Uncompressed,
            ),
            InstrCacheable::Ld(args) => Instruction::new_x64_load_signed(
                args.rd,
                args.rs1,
                args.imm,
                InstrWidth::Uncompressed,
            ),
            // RV64I S-type instructions
            InstrCacheable::Sb(args) => {
                Instruction::new_x8_store(args.rs1, args.rs2, args.imm, InstrWidth::Uncompressed)
            }
            InstrCacheable::Sh(args) => {
                Instruction::new_x16_store(args.rs1, args.rs2, args.imm, InstrWidth::Uncompressed)
            }
            InstrCacheable::Sw(args) => {
                Instruction::new_x32_store(args.rs1, args.rs2, args.imm, InstrWidth::Uncompressed)
            }
            InstrCacheable::Sd(args) => {
                Instruction::new_x64_store(args.rs1, args.rs2, args.imm, InstrWidth::Uncompressed)
            }

            // RV64I B-type instructions
            InstrCacheable::Beq(args) => Instruction::from_ic_beq(args),
            InstrCacheable::Bne(args) => Instruction::from_ic_bne(args),
            InstrCacheable::Blt(args) => Instruction::from_ic_blt(args),
            InstrCacheable::Bge(args) => Instruction::from_ic_bge(args),
            InstrCacheable::Bltu(args) => Instruction::from_ic_bltu(args),
            InstrCacheable::Bgeu(args) => Instruction::from_ic_bgeu(args),

            // RV64I U-type instructions
            InstrCacheable::Lui(args) => {
                Instruction::new_li(args.rd, args.imm, InstrWidth::Uncompressed)
            }
            InstrCacheable::Auipc(args) => {
                Instruction::new_add_immediate_to_pc(args.rd, args.imm, InstrWidth::Uncompressed)
            }

            // RV64I jump instructions
            InstrCacheable::Jal(args) => Instruction::from_ic_jal(args),
            InstrCacheable::Jalr(args) => Instruction::from_ic_jalr(args),

            // RV64A atomic instructions
            InstrCacheable::Lrw(args) => Instruction {
                opcode: OpCode::Lrw,
                args: args.into(),
            },
            InstrCacheable::Scw(args) => Instruction {
                opcode: OpCode::Scw,
                args: args.into(),
            },
            InstrCacheable::Amoswapw(args) => Instruction {
                opcode: OpCode::Amoswapw,
                args: args.into(),
            },
            InstrCacheable::Amoaddw(args) => Instruction {
                opcode: OpCode::Amoaddw,
                args: args.into(),
            },
            InstrCacheable::Amoxorw(args) => Instruction {
                opcode: OpCode::Amoxorw,
                args: args.into(),
            },
            InstrCacheable::Amoandw(args) => Instruction {
                opcode: OpCode::Amoandw,
                args: args.into(),
            },
            InstrCacheable::Amoorw(args) => Instruction {
                opcode: OpCode::Amoorw,
                args: args.into(),
            },
            InstrCacheable::Amominw(args) => Instruction {
                opcode: OpCode::Amominw,
                args: args.into(),
            },
            InstrCacheable::Amomaxw(args) => Instruction {
                opcode: OpCode::Amomaxw,
                args: args.into(),
            },
            InstrCacheable::Amominuw(args) => Instruction {
                opcode: OpCode::Amominuw,
                args: args.into(),
            },
            InstrCacheable::Amomaxuw(args) => Instruction {
                opcode: OpCode::Amomaxuw,
                args: args.into(),
            },
            InstrCacheable::Lrd(args) => Instruction {
                opcode: OpCode::Lrd,
                args: args.into(),
            },
            InstrCacheable::Scd(args) => Instruction {
                opcode: OpCode::Scd,
                args: args.into(),
            },
            InstrCacheable::Amoswapd(args) => Instruction {
                opcode: OpCode::Amoswapd,
                args: args.into(),
            },
            InstrCacheable::Amoaddd(args) => Instruction::new_x64_atomic_add(
                args.rd,
                args.rs1,
                args.rs2,
                args.aq,
                args.rl,
                InstrWidth::Uncompressed,
            ),
            InstrCacheable::Amoxord(args) => Instruction {
                opcode: OpCode::Amoxord,
                args: args.into(),
            },
            InstrCacheable::Amoandd(args) => Instruction {
                opcode: OpCode::Amoandd,
                args: args.into(),
            },
            InstrCacheable::Amoord(args) => Instruction {
                opcode: OpCode::Amoord,
                args: args.into(),
            },
            InstrCacheable::Amomind(args) => Instruction {
                opcode: OpCode::Amomind,
                args: args.into(),
            },
            InstrCacheable::Amomaxd(args) => Instruction {
                opcode: OpCode::Amomaxd,
                args: args.into(),
            },
            InstrCacheable::Amominud(args) => Instruction {
                opcode: OpCode::Amominud,
                args: args.into(),
            },
            InstrCacheable::Amomaxud(args) => Instruction {
                opcode: OpCode::Amomaxud,
                args: args.into(),
            },

            // RV64M multiplication and division instructions
            InstrCacheable::Rem(args) => Instruction {
                opcode: OpCode::Rem,
                args: args.into(),
            },
            InstrCacheable::Remu(args) => Instruction {
                opcode: OpCode::Remu,
                args: args.into(),
            },
            InstrCacheable::Remw(args) => Instruction {
                opcode: OpCode::Remw,
                args: args.into(),
            },
            InstrCacheable::Remuw(args) => Instruction {
                opcode: OpCode::Remuw,
                args: args.into(),
            },
            InstrCacheable::Div(args) => Instruction::from_ic_div(args),
            InstrCacheable::Divu(args) => Instruction {
                opcode: OpCode::Divu,
                args: args.into(),
            },
            InstrCacheable::Divw(args) => Instruction {
                opcode: OpCode::Divw,
                args: args.into(),
            },
            InstrCacheable::Divuw(args) => Instruction {
                opcode: OpCode::Divuw,
                args: args.into(),
            },
            InstrCacheable::Mul(args) => Instruction::from_ic_mul(args),
            InstrCacheable::Mulh(args) => Instruction::from_ic_mulh(args),
            InstrCacheable::Mulhsu(args) => Instruction::from_ic_mulhsu(args),
            InstrCacheable::Mulhu(args) => Instruction::from_ic_mulhu(args),
            InstrCacheable::Mulw(args) => {
                Instruction::new_x32_mul(args.rd, args.rs1, args.rs2, InstrWidth::Uncompressed)
            }

            // RV64F instructions
            InstrCacheable::Flw(args) => Instruction {
                opcode: OpCode::Flw,
                args: args.to_args(InstrWidth::Uncompressed),
            },
            InstrCacheable::Fsw(args) => Instruction {
                opcode: OpCode::Fsw,
                args: args.to_args(InstrWidth::Uncompressed),
            },
            InstrCacheable::Feqs(args) => Instruction {
                opcode: OpCode::Feqs,
                args: args.into(),
            },
            InstrCacheable::Fles(args) => Instruction {
                opcode: OpCode::Fles,
                args: args.into(),
            },
            InstrCacheable::Flts(args) => Instruction {
                opcode: OpCode::Flts,
                args: args.into(),
            },
            InstrCacheable::Fadds(args) => Instruction {
                opcode: OpCode::Fadds,
                args: args.into(),
            },
            InstrCacheable::Fsubs(args) => Instruction {
                opcode: OpCode::Fsubs,
                args: args.into(),
            },
            InstrCacheable::Fmuls(args) => Instruction {
                opcode: OpCode::Fmuls,
                args: args.into(),
            },
            InstrCacheable::Fdivs(args) => Instruction {
                opcode: OpCode::Fdivs,
                args: args.into(),
            },
            InstrCacheable::Fsqrts(args) => Instruction {
                opcode: OpCode::Fsqrts,
                args: args.into(),
            },
            InstrCacheable::Fmins(args) => Instruction {
                opcode: OpCode::Fmins,
                args: args.into(),
            },
            InstrCacheable::Fmaxs(args) => Instruction {
                opcode: OpCode::Fmaxs,
                args: args.into(),
            },
            InstrCacheable::Fsgnjs(args) => Instruction {
                opcode: OpCode::Fsgnjs,
                args: args.into(),
            },
            InstrCacheable::Fsgnjns(args) => Instruction {
                opcode: OpCode::Fsgnjns,
                args: args.into(),
            },
            InstrCacheable::Fsgnjxs(args) => Instruction {
                opcode: OpCode::Fsgnjxs,
                args: args.into(),
            },
            InstrCacheable::Fmadds(args) => Instruction {
                opcode: OpCode::Fmadds,
                args: args.into(),
            },
            InstrCacheable::Fmsubs(args) => Instruction {
                opcode: OpCode::Fmsubs,
                args: args.into(),
            },
            InstrCacheable::Fnmsubs(args) => Instruction {
                opcode: OpCode::Fnmsubs,
                args: args.into(),
            },
            InstrCacheable::Fnmadds(args) => Instruction {
                opcode: OpCode::Fnmadds,
                args: args.into(),
            },
            InstrCacheable::FclassS(args) => Instruction {
                opcode: OpCode::FclassS,
                args: args.into(),
            },
            InstrCacheable::FmvXW(args) => Instruction {
                opcode: OpCode::FmvXW,
                args: args.into(),
            },
            InstrCacheable::FmvWX(args) => Instruction {
                opcode: OpCode::FmvWX,
                args: args.into(),
            },
            InstrCacheable::Fcvtsw(args) => Instruction {
                opcode: OpCode::Fcvtsw,
                args: args.into(),
            },
            InstrCacheable::Fcvtswu(args) => Instruction {
                opcode: OpCode::Fcvtswu,
                args: args.into(),
            },
            InstrCacheable::Fcvtsl(args) => Instruction {
                opcode: OpCode::Fcvtsl,
                args: args.into(),
            },
            InstrCacheable::Fcvtslu(args) => Instruction {
                opcode: OpCode::Fcvtslu,
                args: args.into(),
            },
            InstrCacheable::Fcvtws(args) => Instruction {
                opcode: OpCode::Fcvtws,
                args: args.into(),
            },
            InstrCacheable::Fcvtwus(args) => Instruction {
                opcode: OpCode::Fcvtwus,
                args: args.into(),
            },
            InstrCacheable::Fcvtls(args) => Instruction {
                opcode: OpCode::Fcvtls,
                args: args.into(),
            },
            InstrCacheable::Fcvtlus(args) => Instruction {
                opcode: OpCode::Fcvtlus,
                args: args.into(),
            },

            // RV64D instructions
            InstrCacheable::Fld(args) => Instruction {
                opcode: OpCode::Fld,
                args: args.to_args(InstrWidth::Uncompressed),
            },
            InstrCacheable::Fsd(args) => Instruction {
                opcode: OpCode::Fsd,
                args: args.to_args(InstrWidth::Uncompressed),
            },
            InstrCacheable::Feqd(args) => Instruction {
                opcode: OpCode::Feqd,
                args: args.into(),
            },
            InstrCacheable::Fled(args) => Instruction {
                opcode: OpCode::Fled,
                args: args.into(),
            },
            InstrCacheable::Fltd(args) => Instruction {
                opcode: OpCode::Fltd,
                args: args.into(),
            },
            InstrCacheable::Faddd(args) => Instruction {
                opcode: OpCode::Faddd,
                args: args.into(),
            },
            InstrCacheable::Fsubd(args) => Instruction {
                opcode: OpCode::Fsubd,
                args: args.into(),
            },
            InstrCacheable::Fmuld(args) => Instruction {
                opcode: OpCode::Fmuld,
                args: args.into(),
            },
            InstrCacheable::Fdivd(args) => Instruction {
                opcode: OpCode::Fdivd,
                args: args.into(),
            },
            InstrCacheable::Fsqrtd(args) => Instruction {
                opcode: OpCode::Fsqrtd,
                args: args.into(),
            },
            InstrCacheable::Fmind(args) => Instruction {
                opcode: OpCode::Fmind,
                args: args.into(),
            },
            InstrCacheable::Fmaxd(args) => Instruction {
                opcode: OpCode::Fmaxd,
                args: args.into(),
            },
            InstrCacheable::Fsgnjd(args) => Instruction {
                opcode: OpCode::Fsgnjd,
                args: args.into(),
            },
            InstrCacheable::Fsgnjnd(args) => Instruction {
                opcode: OpCode::Fsgnjnd,
                args: args.into(),
            },
            InstrCacheable::Fsgnjxd(args) => Instruction {
                opcode: OpCode::Fsgnjxd,
                args: args.into(),
            },
            InstrCacheable::Fcvtds(args) => Instruction {
                opcode: OpCode::Fcvtds,
                args: args.into(),
            },
            InstrCacheable::Fcvtsd(args) => Instruction {
                opcode: OpCode::Fcvtsd,
                args: args.into(),
            },
            InstrCacheable::Fmaddd(args) => Instruction {
                opcode: OpCode::Fmaddd,
                args: args.into(),
            },
            InstrCacheable::Fmsubd(args) => Instruction {
                opcode: OpCode::Fmsubd,
                args: args.into(),
            },
            InstrCacheable::Fnmsubd(args) => Instruction {
                opcode: OpCode::Fnmsubd,
                args: args.into(),
            },
            InstrCacheable::Fnmaddd(args) => Instruction {
                opcode: OpCode::Fnmaddd,
                args: args.into(),
            },
            InstrCacheable::FclassD(args) => Instruction {
                opcode: OpCode::FclassD,
                args: args.into(),
            },
            InstrCacheable::Fcvtdw(args) => Instruction {
                opcode: OpCode::Fcvtdw,
                args: args.into(),
            },
            InstrCacheable::Fcvtdwu(args) => Instruction {
                opcode: OpCode::Fcvtdwu,
                args: args.into(),
            },
            InstrCacheable::Fcvtdl(args) => Instruction {
                opcode: OpCode::Fcvtdl,
                args: args.into(),
            },
            InstrCacheable::Fcvtdlu(args) => Instruction {
                opcode: OpCode::Fcvtdlu,
                args: args.into(),
            },
            InstrCacheable::Fcvtwd(args) => Instruction {
                opcode: OpCode::Fcvtwd,
                args: args.into(),
            },
            InstrCacheable::Fcvtwud(args) => Instruction {
                opcode: OpCode::Fcvtwud,
                args: args.into(),
            },
            InstrCacheable::Fcvtld(args) => Instruction {
                opcode: OpCode::Fcvtld,
                args: args.into(),
            },
            InstrCacheable::Fcvtlud(args) => Instruction {
                opcode: OpCode::Fcvtlud,
                args: args.into(),
            },
            InstrCacheable::FmvXD(args) => Instruction {
                opcode: OpCode::FmvXD,
                args: args.into(),
            },
            InstrCacheable::FmvDX(args) => Instruction {
                opcode: OpCode::FmvDX,
                args: args.into(),
            },

            // Zicsr instructions
            InstrCacheable::Csrrw(args) => Instruction {
                opcode: OpCode::Csrrw,
                args: args.into(),
            },
            InstrCacheable::Csrrs(args) => Instruction {
                opcode: OpCode::Csrrs,
                args: args.into(),
            },
            InstrCacheable::Csrrc(args) => Instruction {
                opcode: OpCode::Csrrc,
                args: args.into(),
            },
            InstrCacheable::Csrrwi(args) => Instruction {
                opcode: OpCode::Csrrwi,
                args: args.into(),
            },
            InstrCacheable::Csrrsi(args) => Instruction {
                opcode: OpCode::Csrrsi,
                args: args.into(),
            },
            InstrCacheable::Csrrci(args) => Instruction {
                opcode: OpCode::Csrrci,
                args: args.into(),
            },

            // RV32C compressed instructions
            InstrCacheable::CLw(args) => {
                debug_assert!(args.imm >= 0 && args.imm % 4 == 0);
                Instruction::new_x32_load_signed(
                    args.rd.into(),
                    args.rs1.into(),
                    args.imm,
                    InstrWidth::Compressed,
                )
            }
            InstrCacheable::CLwsp(args) => {
                debug_assert!(args.imm >= 0 && args.imm % 4 == 0);
                Instruction::new_x32_load_signed(
                    args.rd_rs1.into(),
                    sp,
                    args.imm,
                    InstrWidth::Compressed,
                )
            }
            InstrCacheable::CSw(args) => {
                debug_assert!(args.imm >= 0 && args.imm % 4 == 0);
                Instruction::new_x32_store(
                    args.rs1.into(),
                    args.rs2.into(),
                    args.imm,
                    InstrWidth::Compressed,
                )
            }
            InstrCacheable::CSwsp(args) => {
                debug_assert!(args.imm >= 0 && args.imm % 4 == 0);
                Instruction::new_x32_store(sp, args.rs2, args.imm, InstrWidth::Compressed)
            }
            InstrCacheable::CJ(args) => Instruction::new_j(args.imm, InstrWidth::Compressed),
            InstrCacheable::CJr(args) => Instruction::new_jr(args.rs1, InstrWidth::Compressed),
            InstrCacheable::CJalr(args) => {
                Instruction::new_jalr(nz::ra, args.rs1, InstrWidth::Compressed)
            }
            InstrCacheable::CBeqz(args) => Instruction::from_ic_cbeqz(args),
            InstrCacheable::CBnez(args) => Instruction::from_ic_cbnez(args),
            InstrCacheable::CLi(args) => {
                Instruction::new_li(args.rd_rs1, args.imm, InstrWidth::Compressed)
            }
            InstrCacheable::CLui(args) => {
                Instruction::new_li(args.rd_rs1, args.imm, InstrWidth::Compressed)
            }
            InstrCacheable::CAddi(args) => {
                Instruction::new_addi(args.rd_rs1, args.rd_rs1, args.imm, InstrWidth::Compressed)
            }
            InstrCacheable::CAddi16sp(args) => Instruction::new_addi(
                NonZeroXRegister::x2,
                NonZeroXRegister::x2,
                args.imm,
                InstrWidth::Compressed,
            ),
            InstrCacheable::CAddi4spn(args) => Instruction::from_ic_caddi4spn(args),
            InstrCacheable::CSlli(args) => Instruction::new_shift_left_immediate(
                args.rd_rs1,
                args.rd_rs1,
                args.imm,
                InstrWidth::Compressed,
            ),
            InstrCacheable::CSrli(args) => Instruction::from_ic_csrli(args),
            InstrCacheable::CSrai(args) => Instruction::from_ic_csrai(args),
            InstrCacheable::CAndi(args) => Instruction::from_ic_candi(args),
            InstrCacheable::CMv(args) => {
                Instruction::new_mv(args.rd_rs1, args.rs2, InstrWidth::Compressed)
            }
            InstrCacheable::CAdd(args) => {
                Instruction::new_add(args.rd_rs1, args.rd_rs1, args.rs2, InstrWidth::Compressed)
            }
            InstrCacheable::CAnd(args) => Instruction::from_ic_cand(args),
            InstrCacheable::CXor(args) => Instruction::from_ic_cxor(args),
            InstrCacheable::COr(args) => Instruction::from_ic_cor(args),
            InstrCacheable::CSub(args) => Instruction::from_ic_csub(args),
            InstrCacheable::CNop => Instruction::new_nop(InstrWidth::Compressed),

            // RV64C compressed instructions
            InstrCacheable::CLd(args) => {
                debug_assert!(args.imm >= 0 && args.imm % 8 == 0);
                Instruction::new_x64_load_signed(
                    args.rd.into(),
                    args.rs1.into(),
                    args.imm,
                    InstrWidth::Compressed,
                )
            }
            InstrCacheable::CLdsp(args) => {
                debug_assert!(args.imm >= 0 && args.imm % 8 == 0);
                Instruction::new_x64_load_signed(
                    args.rd_rs1.into(),
                    sp,
                    args.imm,
                    InstrWidth::Compressed,
                )
            }
            InstrCacheable::CSd(args) => {
                debug_assert!(args.imm >= 0 && args.imm % 8 == 0);
                Instruction::new_x64_store(
                    args.rs1.into(),
                    args.rs2.into(),
                    args.imm,
                    InstrWidth::Compressed,
                )
            }
            InstrCacheable::CSdsp(args) => {
                debug_assert!(args.imm >= 0 && args.imm % 8 == 0);
                Instruction::new_x64_store(sp, args.rs2, args.imm, InstrWidth::Compressed)
            }
            InstrCacheable::CAddiw(args) => Instruction::new_add_word_immediate(
                args.rd_rs1,
                args.rd_rs1.into(),
                args.imm,
                InstrWidth::Compressed,
            ),
            InstrCacheable::CAddw(args) => Instruction::from_ic_caddw(args),
            InstrCacheable::CSubw(args) => Instruction::from_ic_csubw(args),

            // RV64DC compressed instructions
            InstrCacheable::CFld(args) => Instruction {
                opcode: OpCode::CFld,
                args: args.to_args(InstrWidth::Compressed),
            },
            InstrCacheable::CFldsp(args) => Instruction {
                opcode: OpCode::CFldsp,
                args: args.into(),
            },
            InstrCacheable::CFsd(args) => Instruction {
                opcode: OpCode::CFsd,
                args: args.to_args(InstrWidth::Compressed),
            },
            InstrCacheable::CFsdsp(args) => Instruction {
                opcode: OpCode::CFsdsp,
                args: args.into(),
            },

            InstrCacheable::Unknown { instr: _ } => {
                Instruction::new_unknown(InstrWidth::Uncompressed)
            }
            InstrCacheable::UnknownCompressed { instr: _ } => {
                Instruction::new_unknown(InstrWidth::Compressed)
            }

            InstrCacheable::Hint { instr: _ } => Instruction::new_nop(InstrWidth::Uncompressed),
            InstrCacheable::HintCompressed { instr: _ } => {
                Instruction::new_nop(InstrWidth::Compressed)
            }

            // Interrupt-Management
            InstrCacheable::Wfi => Instruction::new_nop(InstrWidth::Uncompressed),

            InstrCacheable::Ecall => Instruction::new_ecall(),
        }
    }
}

impl From<&RTypeArgs> for Args {
    fn from(value: &RTypeArgs) -> Self {
        Self {
            rd: value.rd.into(),
            rs1: value.rs1.into(),
            rs2: value.rs2.into(),
            width: InstrWidth::Uncompressed,
            ..Self::DEFAULT
        }
    }
}

impl From<&NonZeroRdRTypeArgs> for Args {
    fn from(value: &NonZeroRdRTypeArgs) -> Self {
        Self {
            rd: value.rd.into(),
            rs1: value.rs1.into(),
            rs2: value.rs2.into(),
            width: InstrWidth::Uncompressed,
            ..Self::DEFAULT
        }
    }
}

impl From<&UJTypeArgs> for Args {
    fn from(value: &UJTypeArgs) -> Self {
        Self {
            rd: value.rd.into(),
            imm: value.imm,
            width: InstrWidth::Uncompressed,
            ..Self::DEFAULT
        }
    }
}

impl From<&NonZeroRdUJTypeArgs> for Args {
    fn from(value: &NonZeroRdUJTypeArgs) -> Self {
        Self {
            rd: value.rd.into(),
            imm: value.imm,
            width: InstrWidth::Uncompressed,
            ..Self::DEFAULT
        }
    }
}

impl From<&AmoArgs> for Args {
    fn from(value: &AmoArgs) -> Self {
        Self {
            rd: value.rd.into(),
            rs1: value.rs1.into(),
            rs2: value.rs2.into(),
            aq: value.aq,
            rl: value.rl,
            width: InstrWidth::Uncompressed,
            ..Self::DEFAULT
        }
    }
}

impl From<&CIBTypeArgs> for Args {
    fn from(value: &CIBTypeArgs) -> Self {
        Self {
            rd: value.rd_rs1.into(),
            imm: value.imm,
            rs1: value.rd_rs1.into(),
            width: InstrWidth::Compressed,
            ..Self::DEFAULT
        }
    }
}

impl From<&CIBNZTypeArgs> for Args {
    fn from(value: &CIBNZTypeArgs) -> Self {
        Self {
            rd: value.rd_rs1.into(),
            imm: value.imm,
            width: InstrWidth::Compressed,
            ..Self::DEFAULT
        }
    }
}

impl From<&CRTypeArgs> for Args {
    fn from(value: &CRTypeArgs) -> Self {
        Self {
            rd: value.rd_rs1.into(),
            rs2: value.rs2.into(),
            width: InstrWidth::Compressed,
            ..Self::DEFAULT
        }
    }
}

impl From<&CNZRTypeArgs> for Args {
    fn from(value: &CNZRTypeArgs) -> Self {
        Self {
            rd: value.rd_rs1.into(),
            rs2: value.rs2.into(),
            width: InstrWidth::Compressed,
            ..Self::DEFAULT
        }
    }
}

impl From<&CJTypeArgs> for Args {
    fn from(value: &CJTypeArgs) -> Self {
        Self {
            imm: value.imm,
            width: InstrWidth::Compressed,
            ..Self::DEFAULT
        }
    }
}

impl From<&CRJTypeArgs> for Args {
    fn from(value: &CRJTypeArgs) -> Self {
        Self {
            rs1: value.rs1.into(),
            width: InstrWidth::Compressed,
            ..Self::DEFAULT
        }
    }
}

impl From<&CSSTypeArgs> for Args {
    fn from(value: &CSSTypeArgs) -> Self {
        Self {
            rs2: value.rs2.into(),
            imm: value.imm,
            width: InstrWidth::Compressed,
            ..Self::DEFAULT
        }
    }
}

impl From<&CsrArgs> for Args {
    fn from(value: &CsrArgs) -> Self {
        Self {
            rd: value.rd.into(),
            rs1: value.rs1.into(),
            csr: value.csr,
            width: InstrWidth::Uncompressed,
            ..Self::DEFAULT
        }
    }
}

impl From<&CsriArgs> for Args {
    fn from(value: &CsriArgs) -> Self {
        Self {
            rd: value.rd.into(),
            imm: value.imm,
            csr: value.csr,
            width: InstrWidth::Uncompressed,
            ..Self::DEFAULT
        }
    }
}

impl FLoadArgs {
    fn to_args(self, width: InstrWidth) -> Args {
        Args {
            rd: self.rd.into(),
            rs1: self.rs1.into(),
            imm: self.imm,
            width,
            ..Args::DEFAULT
        }
    }
}

impl FStoreArgs {
    fn to_args(self, width: InstrWidth) -> Args {
        Args {
            rs1: self.rs1.into(),
            rs2: self.rs2.into(),
            imm: self.imm,
            width,
            ..Args::DEFAULT
        }
    }
}

impl From<&CSSDTypeArgs> for Args {
    fn from(value: &CSSDTypeArgs) -> Self {
        Self {
            imm: value.imm,
            rs2: value.rs2.into(),
            width: InstrWidth::Compressed,
            ..Self::DEFAULT
        }
    }
}

impl From<&CIBDTypeArgs> for Args {
    fn from(value: &CIBDTypeArgs) -> Self {
        Self {
            imm: value.imm,
            rd: value.rd_rs1.into(),
            width: InstrWidth::Compressed,
            ..Self::DEFAULT
        }
    }
}

impl From<&XRegToFRegArgs> for Args {
    fn from(value: &XRegToFRegArgs) -> Self {
        Self {
            rd: value.rd.into(),
            rs1: value.rs1.into(),
            width: InstrWidth::Uncompressed,
            ..Self::DEFAULT
        }
    }
}

impl From<&XRegToFRegArgsWithRounding> for Args {
    fn from(value: &XRegToFRegArgsWithRounding) -> Self {
        Self {
            rd: value.rd.into(),
            rs1: value.rs1.into(),
            rm: value.rm,
            width: InstrWidth::Uncompressed,
            ..Self::DEFAULT
        }
    }
}

impl From<&FRegToXRegArgs> for Args {
    fn from(value: &FRegToXRegArgs) -> Self {
        Self {
            rd: value.rd.into(),
            rs1: value.rs1.into(),
            width: InstrWidth::Uncompressed,
            ..Self::DEFAULT
        }
    }
}

impl From<&FRegToXRegArgsWithRounding> for Args {
    fn from(value: &FRegToXRegArgsWithRounding) -> Self {
        Self {
            rd: value.rd.into(),
            rs1: value.rs1.into(),
            rm: value.rm,
            width: InstrWidth::Uncompressed,
            ..Self::DEFAULT
        }
    }
}

impl From<&FR3ArgsWithRounding> for Args {
    fn from(value: &FR3ArgsWithRounding) -> Self {
        Self {
            rd: value.rd.into(),
            rs1: value.rs1.into(),
            rs2: value.rs2.into(),
            rs3f: value.rs3,
            rm: value.rm,
            width: InstrWidth::Uncompressed,
            ..Self::DEFAULT
        }
    }
}

impl From<&FRArgs> for Args {
    fn from(value: &FRArgs) -> Self {
        Self {
            rd: value.rd.into(),
            rs1: value.rs1.into(),
            rs2: value.rs2.into(),
            width: InstrWidth::Uncompressed,
            ..Self::DEFAULT
        }
    }
}

impl From<&FR1ArgWithRounding> for Args {
    fn from(value: &FR1ArgWithRounding) -> Self {
        Self {
            rd: value.rd.into(),
            rs1: value.rs1.into(),
            rm: value.rm,
            width: InstrWidth::Uncompressed,
            ..Self::DEFAULT
        }
    }
}

impl From<&FR2ArgsWithRounding> for Args {
    fn from(value: &FR2ArgsWithRounding) -> Self {
        Self {
            rd: value.rd.into(),
            rs1: value.rs1.into(),
            rs2: value.rs2.into(),
            rm: value.rm,
            width: InstrWidth::Uncompressed,
            ..Self::DEFAULT
        }
    }
}

impl From<&FCmpArgs> for Args {
    fn from(value: &FCmpArgs) -> Self {
        Self {
            rd: value.rd.into(),
            rs1: value.rs1.into(),
            rs2: value.rs2.into(),
            width: InstrWidth::Uncompressed,
            ..Self::DEFAULT
        }
    }
}

#[cfg(test)]
mod test {
    use super::Register;
    use crate::default::ConstDefault;
    use crate::machine_state::registers::FRegister;
    use crate::machine_state::registers::NonZeroXRegister;
    use crate::machine_state::registers::XRegister;

    // Ensure no undefined behaviour when reading any field from `Register::DEFAULT`
    #[test]
    #[cfg(miri)]
    fn miri_test_register_default_safety() {
        let default = Register::DEFAULT;

        assert_eq!(XRegister::x1, unsafe { default.x });
        assert_eq!(NonZeroXRegister::x1, unsafe { default.nzx });
        // FRegisters are not offset by one as f0 is stored in state, unlike x0
        assert_eq!(FRegister::f0, unsafe { default.f });
    }

    // The default register should be valid for all variants
    #[test]
    fn miri_test_register_default() {
        let default = Register::DEFAULT;

        assert!(default == Register { x: XRegister::x1 });
        assert!(
            default
                == Register {
                    nzx: NonZeroXRegister::x1
                }
        );
        // FRegisters are not offset by one as f0 is stored in state, unlike x0
        assert!(default == Register { f: FRegister::f0 });
    }
}
