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
pub mod tagged_instruction;

use super::{
    MachineCoreState, ProgramCounterUpdate,
    csregisters::CSRegister,
    main_memory::MainMemoryLayout,
    registers::{FRegister, NonZeroXRegister, XRegister},
};
use crate::{
    default::ConstDefault,
    instruction_context::{ICB, IcbLoweringFn},
    interpreter::{c, i},
    machine_state::ProgramCounterUpdate::{Next, Set},
    parser::instruction::{
        AmoArgs, CIBDTypeArgs, CIBNZTypeArgs, CIBTypeArgs, CJTypeArgs, CNZRTypeArgs, CRJTypeArgs,
        CRTypeArgs, CSSDTypeArgs, CSSTypeArgs, CsrArgs, CsriArgs, FCmpArgs, FLoadArgs,
        FR1ArgWithRounding, FR2ArgsWithRounding, FR3ArgsWithRounding, FRArgs, FRegToXRegArgs,
        FRegToXRegArgsWithRounding, FStoreArgs, ITypeArgs, InstrCacheable, InstrRoundingMode,
        InstrWidth, NonZeroRdITypeArgs, NonZeroRdRTypeArgs, NonZeroRdUJTypeArgs, RTypeArgs,
        SBTypeArgs, UJTypeArgs, XRegToFRegArgs, XRegToFRegArgsWithRounding,
    },
    state_backend::{ManagerBase, ManagerReadWrite},
    traps::Exception,
};
use serde::{Deserialize, Serialize};
use std::fmt::{self, Debug, Formatter};
use tagged_instruction::{ArgsShape, TaggedInstruction, opcode_to_argsshape};

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

        impl<'a> Debug for DebugArgs<'a> {
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

/// Opcodes map to the operation performed over the state - allowing us to
/// decouple these from the parsed instructions down the line.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum OpCode {
    Unknown,

    // RV64I R-type instructions
    Add,
    Sub,
    Xor,
    Or,
    And,
    Sll,
    Srl,
    Sra,
    Slt,
    Sltu,
    Addw,
    Subw,
    Sllw,
    Srlw,
    Sraw,

    // RV64I I-type instructions
    Addi,
    Addiw,
    Xori,
    Ori,
    Andi,
    Slli,
    Srli,
    Srai,
    Slliw,
    Srliw,
    Sraiw,
    Slti,
    Sltiu,
    Lb,
    Lh,
    Lw,
    Lbu,
    Lhu,
    Lwu,
    Ld,

    // RV64I S-type instructions
    Sb,
    Sh,
    Sw,
    Sd,

    // RV64I B-type instructions
    Beq,
    Bne,
    Blt,
    Bge,
    Bltu,
    Bgeu,

    // RV64I U-type instructions
    Lui,
    Auipc,

    // RV64I jump instructions
    Jal,
    Jalr,

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
    Amoaddd,
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
    Div,
    Divu,
    Divw,
    Divuw,
    Mul,
    Mulh,
    Mulhsu,
    Mulhu,
    Mulw,

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

    // RV32C compressed instructions
    CLw,
    CLwsp,
    CSw,
    CSwsp,
    CJr,
    CJalr,
    CLui,
    CAddw,
    CSubw,

    // RV64C compressed instructions
    CLd,
    CLdsp,
    CSd,
    CSdsp,
    CAddiw,

    // RV64DC compressed instructions
    CFld,
    CFldsp,
    CFsd,
    CFsdsp,

    // Internal OpCodes
    Beqz,
    Bnez,
    J,
    Mv,
    Li,
    Nop,
    Neg,
}

impl OpCode {
    /// Dispatch an opcode to the function that will run over the machine state.
    ///
    /// # SAFETY
    /// Calling the returned function **must** correspond to an `Args` belonging to an
    /// instruction where the `OpCode` is the same as the `OpCode` of the current instruction.
    #[inline(always)]
    pub(super) fn to_run<ML: MainMemoryLayout, M: ManagerReadWrite>(
        self,
    ) -> unsafe fn(&Args, &mut MachineCoreState<ML, M>) -> Result<ProgramCounterUpdate, Exception>
    {
        match self {
            Self::Add => Args::run_add,
            Self::Sub => Args::run_sub,
            Self::Neg => Args::run_neg,
            Self::Xor => Args::run_xor,
            Self::Or => Args::run_or,
            Self::And => Args::run_and,
            Self::Sll => Args::run_sll,
            Self::Srl => Args::run_srl,
            Self::Sra => Args::run_sra,
            Self::Slt => Args::run_slt,
            Self::Sltu => Args::run_sltu,
            Self::Addw => Args::run_addw,
            Self::Subw => Args::run_subw,
            Self::Sllw => Args::run_sllw,
            Self::Srlw => Args::run_srlw,
            Self::Sraw => Args::run_sraw,
            Self::Addi => Args::run_addi,
            Self::Addiw => Args::run_addiw,
            Self::Xori => Args::run_xori,
            Self::Ori => Args::run_ori,
            Self::Andi => Args::run_andi,
            Self::Slli => Args::run_slli,
            Self::Srli => Args::run_srli,
            Self::Srai => Args::run_srai,
            Self::Slliw => Args::run_slliw,
            Self::Srliw => Args::run_srliw,
            Self::Sraiw => Args::run_sraiw,
            Self::Slti => Args::run_slti,
            Self::Sltiu => Args::run_sltiu,
            Self::Lb => Args::run_lb,
            Self::Lh => Args::run_lh,
            Self::Lw => Args::run_lw,
            Self::Lbu => Args::run_lbu,
            Self::Lhu => Args::run_lhu,
            Self::Lwu => Args::run_lwu,
            Self::Ld => Args::run_ld,
            Self::Sb => Args::run_sb,
            Self::Sh => Args::run_sh,
            Self::Sw => Args::run_sw,
            Self::Sd => Args::run_sd,
            Self::Beq => Args::run_beq,
            Self::Bne => Args::run_bne,
            Self::Blt => Args::run_blt,
            Self::Bge => Args::run_bge,
            Self::Bltu => Args::run_bltu,
            Self::Bgeu => Args::run_bgeu,
            Self::Lui => Args::run_lui,
            Self::Auipc => Args::run_auipc,
            Self::Jal => Args::run_jal,
            Self::Jalr => Args::run_jalr,
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
            Self::Amoaddd => Args::run_amoaddd,
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
            Self::Div => Args::run_div,
            Self::Divu => Args::run_divu,
            Self::Divw => Args::run_divw,
            Self::Divuw => Args::run_divuw,
            Self::Mul => Args::run_mul,
            Self::Mulh => Args::run_mulh,
            Self::Mulhsu => Args::run_mulhsu,
            Self::Mulhu => Args::run_mulhu,
            Self::Mulw => Args::run_mulw,
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
            Self::CLw => Args::run_clw,
            Self::CLwsp => Args::run_clwsp,
            Self::CSw => Args::run_csw,
            Self::CSwsp => Args::run_cswsp,
            Self::J => Args::run_j,
            Self::CJr => Args::run_cjr,
            Self::CJalr => Args::run_cjalr,
            Self::Beqz => Args::run_beqz,
            Self::Bnez => Args::run_bnez,
            Self::Li => Args::run_li,
            Self::CLui => Args::run_clui,
            Self::Mv => Args::run_mv,
            Self::CAddw => Args::run_caddw,
            Self::CSubw => Args::run_csubw,
            Self::Nop => Args::run_nop,
            Self::CLd => Args::run_cld,
            Self::CLdsp => Args::run_cldsp,
            Self::CSd => Args::run_csd,
            Self::CSdsp => Args::run_csdsp,
            Self::CAddiw => Args::run_caddiw,
            Self::CFld => Args::run_cfld,
            Self::CFldsp => Args::run_cfldsp,
            Self::CFsd => Args::run_cfsd,
            Self::CFsdsp => Args::run_cfsdsp,
            Self::Unknown => Args::run_illegal,
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
    pub fn to_lowering<I: ICB>(self) -> Option<IcbLoweringFn<I>> {
        match self {
            Self::Mv => Some(Args::run_mv),
            Self::Nop => Some(Args::run_nop),
            Self::Add => Some(Args::run_add),
            _ => None,
        }
    }
}

impl Instruction {
    /// Run an instruction over the machine core state.
    pub(super) fn run<ML: MainMemoryLayout, M: ManagerReadWrite>(
        &self,
        core: &mut MachineCoreState<ML, M>,
    ) -> Result<ProgramCounterUpdate, Exception> {
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
        unsafe { std::mem::transmute::<_, &u8>(self) == std::mem::transmute::<_, &u8>(other) }
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
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart.xregisters.$fn(self.rs1.x, self.rs2.x, self.rd.x);
            Ok(Next(self.width))
        }
    };

    ($fn: ident, non_zero) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart
                .xregisters
                .$fn(self.rs1.nzx, self.rs2.nzx, self.rd.nzx);
            Ok(Next(self.width))
        }
    };

    ($fn: ident, non_zero_rd) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart
                .xregisters
                .$fn(self.rs1.x, self.rs2.x, self.rd.nzx);
            Ok(Next(self.width))
        }
    };

    ($impl: path, $fn: ident, non_zero) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<I: ICB>(&self, icb: &mut I) -> <I as ICB>::IResult<ProgramCounterUpdate> {
            $impl(icb, self.rs1.nzx, self.rs2.nzx, self.rd.nzx);
            icb.ok(Next(self.width))
        }
    };
}

macro_rules! impl_i_type {
    ($fn: ident, non_zero) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart
                .xregisters
                .$fn(self.imm, self.rs1.nzx, self.rd.nzx);
            Ok(Next(self.width))
        }
    };

    ($fn: ident, non_zero_rd) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart.xregisters.$fn(self.imm, self.rs1.x, self.rd.nzx);
            Ok(Next(self.width))
        }
    };
}

macro_rules! impl_fload_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.$fn(self.imm, self.rs1.x, self.rd.f)
                .map(|_| Next(self.width))
        }
    };
}
macro_rules! impl_load_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.$fn(self.imm, self.rs1.x, self.rd.x)
                .map(|_| Next(self.width))
        }
    };
}
macro_rules! impl_cload_sp_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.$fn(self.imm, self.rd.nzx).map(|_| Next(self.width))
        }
    };
}
macro_rules! impl_cfload_sp_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.$fn(self.imm, self.rd.f).map(|_| Next(self.width))
        }
    };
}

macro_rules! impl_store_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.$fn(self.imm, self.rs1.x, self.rs2.x)
                .map(|_| Next(self.width))
        }
    };
}
macro_rules! impl_fstore_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.$fn(self.imm, self.rs1.x, self.rs2.f)
                .map(|_| Next(self.width))
        }
    };
}

macro_rules! impl_b_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            Ok(core.hart.$fn(self.imm, self.rs1.x, self.rs2.x, self.width))
        }
    };

    ($fn: ident, non_zero) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            Ok(core
                .hart
                .$fn(self.imm, self.rs1.nzx, self.rs2.nzx, self.width))
        }
    };
}

macro_rules! impl_amo_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.$fn(self.rs1.x, self.rs2.x, self.rd.x, self.rl, self.aq)
                .map(|_| Next(self.width))
        }
    };
}

macro_rules! impl_ci_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart.xregisters.$fn(self.imm, self.rd.x);
            Ok(ProgramCounterUpdate::Next(self.width))
        }
    };

    ($fn: ident, non_zero) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart.xregisters.$fn(self.imm, self.rd.nzx);
            Ok(ProgramCounterUpdate::Next(self.width))
        }
    };
}

macro_rules! impl_cr_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart.xregisters.$fn(self.rd.x, self.rs2.x);
            Ok(ProgramCounterUpdate::Next(self.width))
        }
    };

    ($fn: ident, non_zero) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart.xregisters.$fn(self.rd.nzx, self.rs2.nzx);
            Ok(ProgramCounterUpdate::Next(self.width))
        }
    };
}

macro_rules! impl_cr_nz_type {
    ($impl: path, $fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<I: ICB>(&self, icb: &mut I) -> <I as ICB>::IResult<ProgramCounterUpdate> {
            $impl(icb, self.rd.nzx, self.rs2.nzx);
            icb.ok(Next(self.width))
        }
    };
}

macro_rules! impl_cb_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            Ok(core.hart.$fn(self.imm, self.rs1.nzx, self.width))
        }
    };
}

macro_rules! impl_css_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.$fn(self.imm, self.rs2.x).map(|_| Next(self.width))
        }
    };
}

macro_rules! impl_fcss_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.$fn(self.imm, self.rs2.f).map(|_| Next(self.width))
        }
    };
}

macro_rules! impl_csr_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart
                .$fn(self.csr, self.rs1.x, self.rd.x)
                .map(|_| Next(self.width))
        }
    };
}

macro_rules! impl_csr_imm_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart
                .$fn(self.csr, self.imm as u64, self.rd.x)
                .map(|_| Next(self.width))
        }
    };
}

macro_rules! impl_f_x_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart
                .$fn(self.rs1.x, self.rd.f)
                .map(|_| Next(self.width))
        }
    };

    ($fn:ident, rm) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart
                .$fn(self.rs1.x, self.rm, self.rd.f)
                .map(|_| Next(self.width))
        }
    };
}

macro_rules! impl_x_f_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart
                .$fn(self.rs1.f, self.rd.x)
                .map(|_| Next(self.width))
        }
    };

    ($fn:ident, rm) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart
                .$fn(self.rs1.f, self.rm, self.rd.x)
                .map(|_| Next(self.width))
        }
    };
}

macro_rules! impl_f_r_type {
    ($fn: ident) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart
                .$fn(self.rs1.f, self.rs2.f, self.rd.f)
                .map(|_| Next(self.width))
        }
    };

    ($fn: ident, (rd, x)) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart
                .$fn(self.rs1.f, self.rs2.f, self.rd.x)
                .map(|_| Next(self.width))
        }
    };

    ($fn: ident, rm) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart
                .$fn(self.rs1.f, self.rm, self.rd.f)
                .map(|_| Next(self.width))
        }
    };

    ($fn: ident, (rs2, f), $($field: ident),+) => {
        /// SAFETY: This function must only be called on an `Args` belonging
        /// to the same OpCode as the OpCode used to derive this function.
        unsafe fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart
                .$fn(self.rs1.f, self.rs2.f, $(self.$field,)* self.rd.f)
                .map(|_| Next(self.width))
        }
    };
}

impl Args {
    // RV64I R-type instructions
    impl_r_type!(i::run_add, run_add, non_zero);
    impl_r_type!(run_sub, non_zero);
    impl_r_type!(run_xor, non_zero);
    impl_r_type!(run_or, non_zero);
    impl_r_type!(run_and, non_zero);
    impl_r_type!(run_sll, non_zero);
    impl_r_type!(run_srl, non_zero);
    impl_r_type!(run_sra, non_zero);
    impl_r_type!(run_slt, non_zero_rd);
    impl_r_type!(run_sltu, non_zero_rd);
    impl_r_type!(run_addw, non_zero_rd);
    impl_r_type!(run_subw, non_zero_rd);
    impl_r_type!(run_sllw, non_zero_rd);
    impl_r_type!(run_srlw, non_zero_rd);
    impl_r_type!(run_sraw, non_zero_rd);

    // RV64I I-type instructions
    impl_i_type!(run_addi, non_zero);
    impl_i_type!(run_addiw, non_zero_rd);
    impl_i_type!(run_xori, non_zero);
    impl_i_type!(run_ori, non_zero);
    impl_i_type!(run_andi, non_zero);
    impl_i_type!(run_slli, non_zero);
    impl_i_type!(run_srli, non_zero);
    impl_i_type!(run_srai, non_zero);
    impl_i_type!(run_slliw, non_zero_rd);
    impl_i_type!(run_srliw, non_zero_rd);
    impl_i_type!(run_sraiw, non_zero_rd);
    impl_i_type!(run_slti, non_zero_rd);
    impl_i_type!(run_sltiu, non_zero_rd);
    impl_load_type!(run_lb);
    impl_load_type!(run_lh);
    impl_load_type!(run_lw);
    impl_load_type!(run_lbu);
    impl_load_type!(run_lhu);
    impl_load_type!(run_lwu);
    impl_load_type!(run_ld);

    // RV64I S-type instructions
    impl_store_type!(run_sb);
    impl_store_type!(run_sh);
    impl_store_type!(run_sw);
    impl_store_type!(run_sd);

    // RV64I B-type instructions
    impl_b_type!(run_beq, non_zero);
    impl_b_type!(run_bne, non_zero);
    impl_b_type!(run_blt);
    impl_b_type!(run_bge);
    impl_b_type!(run_bltu);
    impl_b_type!(run_bgeu);

    // RV64I U-type instructions
    //
    /// SAFETY: This function must only be called on an `Args` belonging
    /// to the same OpCode as the OpCode used to derive this function.
    unsafe fn run_lui<ML: MainMemoryLayout, M: ManagerReadWrite>(
        &self,
        core: &mut MachineCoreState<ML, M>,
    ) -> Result<ProgramCounterUpdate, Exception> {
        core.hart.xregisters.run_lui(self.imm, self.rd.nzx);
        Ok(Next(self.width))
    }

    /// SAFETY: This function must only be called on an `Args` belonging
    /// to the same OpCode as the OpCode used to derive this function.
    unsafe fn run_auipc<ML: MainMemoryLayout, M: ManagerReadWrite>(
        &self,
        core: &mut MachineCoreState<ML, M>,
    ) -> Result<ProgramCounterUpdate, Exception> {
        core.hart.run_auipc(self.imm, self.rd.nzx);
        Ok(Next(self.width))
    }

    // RV64I jump instructions
    //
    /// SAFETY: This function must only be called on an `Args` belonging
    /// to the same OpCode as the OpCode used to derive this function.
    unsafe fn run_jal<ML: MainMemoryLayout, M: ManagerReadWrite>(
        &self,
        core: &mut MachineCoreState<ML, M>,
    ) -> Result<ProgramCounterUpdate, Exception> {
        Ok(Set(core.hart.run_jal(self.imm, self.rd.nzx, self.width)))
    }

    /// SAFETY: This function must only be called on an `Args` belonging
    /// to the same OpCode as the OpCode used to derive this function.
    unsafe fn run_jalr<ML: MainMemoryLayout, M: ManagerReadWrite>(
        &self,
        core: &mut MachineCoreState<ML, M>,
    ) -> Result<ProgramCounterUpdate, Exception> {
        Ok(Set(core.hart.run_jalr(self.imm, self.rs1.x, self.rd.x)))
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
    impl_amo_type!(run_amoaddd);
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
    impl_r_type!(run_div);
    impl_r_type!(run_divu);
    impl_r_type!(run_divw);
    impl_r_type!(run_divuw);
    impl_r_type!(run_mul);
    impl_r_type!(run_mulh);
    impl_r_type!(run_mulhsu);
    impl_r_type!(run_mulhu);
    impl_r_type!(run_mulw);

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
    impl_cr_nz_type!(c::run_mv, run_mv);
    impl_load_type!(run_clw);
    impl_cload_sp_type!(run_clwsp);
    impl_store_type!(run_csw);
    impl_cb_type!(run_beqz);
    impl_cb_type!(run_bnez);
    impl_ci_type!(run_li, non_zero);
    impl_ci_type!(run_clui, non_zero);
    impl_cr_type!(run_neg, non_zero);
    impl_css_type!(run_cswsp);

    fn run_j<ML: MainMemoryLayout, M: ManagerReadWrite>(
        &self,
        core: &mut MachineCoreState<ML, M>,
    ) -> Result<ProgramCounterUpdate, Exception> {
        Ok(Set(core.hart.run_j(self.imm)))
    }

    /// SAFETY: This function must only be called on an `Args` belonging
    /// to the same OpCode as the OpCode used to derive this function.
    unsafe fn run_cjr<ML: MainMemoryLayout, M: ManagerReadWrite>(
        &self,
        core: &mut MachineCoreState<ML, M>,
    ) -> Result<ProgramCounterUpdate, Exception> {
        Ok(Set(core.hart.run_cjr(self.rs1.nzx)))
    }

    /// SAFETY: This function must only be called on an `Args` belonging
    /// to the same OpCode as the OpCode used to derive this function.
    unsafe fn run_cjalr<ML: MainMemoryLayout, M: ManagerReadWrite>(
        &self,
        core: &mut MachineCoreState<ML, M>,
    ) -> Result<ProgramCounterUpdate, Exception> {
        Ok(Set(core.hart.run_cjalr(self.rs1.nzx)))
    }

    fn run_nop<I: ICB>(&self, icb: &mut I) -> <I as ICB>::IResult<ProgramCounterUpdate> {
        c::run_nop(icb);
        icb.ok(Next(self.width))
    }

    // RV64C compressed instructions
    impl_store_type!(run_csd);
    impl_css_type!(run_csdsp);
    impl_load_type!(run_cld);
    impl_cload_sp_type!(run_cldsp);
    impl_ci_type!(run_caddiw, non_zero);
    impl_cr_type!(run_caddw);
    impl_cr_type!(run_csubw);

    // RV64C compressed instructions
    impl_fload_type!(run_cfld);
    impl_cfload_sp_type!(run_cfldsp);
    impl_fstore_type!(run_cfsd);
    impl_fcss_type!(run_cfsdsp);

    // Unknown
    fn run_illegal<ML: MainMemoryLayout, M: ManagerBase>(
        &self,
        _core: &mut MachineCoreState<ML, M>,
    ) -> Result<ProgramCounterUpdate, Exception> {
        Err(Exception::IllegalInstruction)
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
            InstrCacheable::Slt(args) => Instruction {
                opcode: OpCode::Slt,
                args: args.into(),
            },
            InstrCacheable::Sltu(args) => Instruction {
                opcode: OpCode::Sltu,
                args: args.into(),
            },
            InstrCacheable::Addw(args) => Instruction {
                opcode: OpCode::Addw,
                args: args.into(),
            },
            InstrCacheable::Subw(args) => Instruction {
                opcode: OpCode::Subw,
                args: args.into(),
            },
            InstrCacheable::Sllw(args) => Instruction {
                opcode: OpCode::Sllw,
                args: args.into(),
            },
            InstrCacheable::Srlw(args) => Instruction {
                opcode: OpCode::Srlw,
                args: args.into(),
            },
            InstrCacheable::Sraw(args) => Instruction {
                opcode: OpCode::Sraw,
                args: args.into(),
            },

            // RV64I I-type instructions
            InstrCacheable::Addi(args) => Instruction::from_ic_addi(args),
            InstrCacheable::Addiw(args) => Instruction {
                opcode: OpCode::Addiw,
                args: args.to_args(InstrWidth::Uncompressed),
            },
            InstrCacheable::Xori(args) => Instruction::from_ic_xori(args),
            InstrCacheable::Ori(args) => Instruction::from_ic_ori(args),
            InstrCacheable::Andi(args) => Instruction::from_ic_andi(args),
            InstrCacheable::Slli(args) => Instruction::from_ic_slli(args),
            InstrCacheable::Srli(args) => Instruction::from_ic_srli(args),
            InstrCacheable::Srai(args) => Instruction::from_ic_srai(args),
            InstrCacheable::Slliw(args) => Instruction {
                opcode: OpCode::Slliw,
                args: args.to_args(InstrWidth::Uncompressed),
            },
            InstrCacheable::Srliw(args) => Instruction {
                opcode: OpCode::Srliw,
                args: args.to_args(InstrWidth::Uncompressed),
            },
            InstrCacheable::Sraiw(args) => Instruction {
                opcode: OpCode::Sraiw,
                args: args.to_args(InstrWidth::Uncompressed),
            },
            InstrCacheable::Slti(args) => Instruction {
                opcode: OpCode::Slti,
                args: args.to_args(InstrWidth::Uncompressed),
            },
            InstrCacheable::Sltiu(args) => Instruction {
                opcode: OpCode::Sltiu,
                args: args.to_args(InstrWidth::Uncompressed),
            },
            InstrCacheable::Lb(args) => Instruction {
                opcode: OpCode::Lb,
                args: args.to_args(InstrWidth::Uncompressed),
            },
            InstrCacheable::Lh(args) => Instruction {
                opcode: OpCode::Lh,
                args: args.to_args(InstrWidth::Uncompressed),
            },
            InstrCacheable::Lw(args) => Instruction {
                opcode: OpCode::Lw,
                args: args.to_args(InstrWidth::Uncompressed),
            },
            InstrCacheable::Lbu(args) => Instruction {
                opcode: OpCode::Lbu,
                args: args.to_args(InstrWidth::Uncompressed),
            },
            InstrCacheable::Lhu(args) => Instruction {
                opcode: OpCode::Lhu,
                args: args.to_args(InstrWidth::Uncompressed),
            },
            InstrCacheable::Lwu(args) => Instruction {
                opcode: OpCode::Lwu,
                args: args.to_args(InstrWidth::Uncompressed),
            },
            InstrCacheable::Ld(args) => Instruction {
                opcode: OpCode::Ld,
                args: args.to_args(InstrWidth::Uncompressed),
            },
            // RV64I S-type instructions
            InstrCacheable::Sb(args) => Instruction {
                opcode: OpCode::Sb,
                args: args.to_args(InstrWidth::Uncompressed),
            },
            InstrCacheable::Sh(args) => Instruction {
                opcode: OpCode::Sh,
                args: args.to_args(InstrWidth::Uncompressed),
            },
            InstrCacheable::Sw(args) => Instruction {
                opcode: OpCode::Sw,
                args: args.to_args(InstrWidth::Uncompressed),
            },
            InstrCacheable::Sd(args) => Instruction {
                opcode: OpCode::Sd,
                args: args.to_args(InstrWidth::Uncompressed),
            },

            // RV64I B-type instructions
            InstrCacheable::Beq(args) => Instruction::from_ic_beq(args),
            InstrCacheable::Bne(args) => Instruction::from_ic_bne(args),
            InstrCacheable::Blt(args) => Instruction {
                opcode: OpCode::Blt,
                args: args.to_args(InstrWidth::Uncompressed),
            },
            InstrCacheable::Bge(args) => Instruction {
                opcode: OpCode::Bge,
                args: args.to_args(InstrWidth::Uncompressed),
            },
            InstrCacheable::Bltu(args) => Instruction {
                opcode: OpCode::Bltu,
                args: args.to_args(InstrWidth::Uncompressed),
            },
            InstrCacheable::Bgeu(args) => Instruction {
                opcode: OpCode::Bgeu,
                args: args.to_args(InstrWidth::Uncompressed),
            },

            // RV64I U-type instructions
            InstrCacheable::Lui(args) => Instruction {
                opcode: OpCode::Lui,
                args: args.into(),
            },
            InstrCacheable::Auipc(args) => Instruction {
                opcode: OpCode::Auipc,
                args: args.into(),
            },

            // RV64I jump instructions
            InstrCacheable::Jal(args) => Instruction::from_ic_jal(args),
            InstrCacheable::Jalr(args) => Instruction {
                opcode: OpCode::Jalr,
                args: args.to_args(InstrWidth::Uncompressed),
            },

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
            InstrCacheable::Amoaddd(args) => Instruction {
                opcode: OpCode::Amoaddd,
                args: args.into(),
            },
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
            InstrCacheable::Div(args) => Instruction {
                opcode: OpCode::Div,
                args: args.into(),
            },
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
            InstrCacheable::Mul(args) => Instruction {
                opcode: OpCode::Mul,
                args: args.into(),
            },
            InstrCacheable::Mulh(args) => Instruction {
                opcode: OpCode::Mulh,
                args: args.into(),
            },
            InstrCacheable::Mulhsu(args) => Instruction {
                opcode: OpCode::Mulhsu,
                args: args.into(),
            },
            InstrCacheable::Mulhu(args) => Instruction {
                opcode: OpCode::Mulhu,
                args: args.into(),
            },
            InstrCacheable::Mulw(args) => Instruction {
                opcode: OpCode::Mulw,
                args: args.into(),
            },

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
            InstrCacheable::CLw(args) => Instruction {
                opcode: OpCode::CLw,
                args: args.to_args(InstrWidth::Compressed),
            },
            InstrCacheable::CLwsp(args) => Instruction {
                opcode: OpCode::CLwsp,
                args: args.into(),
            },
            InstrCacheable::CSw(args) => Instruction {
                opcode: OpCode::CSw,
                args: args.to_args(InstrWidth::Compressed),
            },
            InstrCacheable::CSwsp(args) => Instruction {
                opcode: OpCode::CSwsp,
                args: args.into(),
            },
            InstrCacheable::CJ(args) => Instruction::new_j(args.imm, InstrWidth::Compressed),
            InstrCacheable::CJr(args) => Instruction {
                opcode: OpCode::CJr,
                args: args.into(),
            },
            InstrCacheable::CJalr(args) => Instruction {
                opcode: OpCode::CJalr,
                args: args.into(),
            },
            InstrCacheable::CBeqz(args) => Instruction::from_ic_cbeqz(args),
            InstrCacheable::CBnez(args) => Instruction::from_ic_cbnez(args),
            InstrCacheable::CLi(args) => {
                Instruction::new_li(args.rd_rs1, args.imm, InstrWidth::Compressed)
            }
            InstrCacheable::CLui(args) => Instruction {
                opcode: OpCode::CLui,
                args: args.into(),
            },
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
            InstrCacheable::CSlli(args) => {
                Instruction::new_slli(args.rd_rs1, args.rd_rs1, args.imm, InstrWidth::Compressed)
            }
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
            InstrCacheable::CLd(args) => Instruction {
                opcode: OpCode::CLd,
                args: args.to_args(InstrWidth::Compressed),
            },
            InstrCacheable::CLdsp(args) => Instruction {
                opcode: OpCode::CLdsp,
                args: args.into(),
            },
            InstrCacheable::CSd(args) => Instruction {
                opcode: OpCode::CSd,
                args: args.to_args(InstrWidth::Compressed),
            },
            InstrCacheable::CSdsp(args) => Instruction {
                opcode: OpCode::CSdsp,
                args: args.into(),
            },
            InstrCacheable::CAddiw(args) => Instruction {
                opcode: OpCode::CAddiw,
                args: args.into(),
            },
            InstrCacheable::CAddw(args) => Instruction {
                opcode: OpCode::CAddw,
                args: args.into(),
            },
            InstrCacheable::CSubw(args) => Instruction {
                opcode: OpCode::CSubw,
                args: args.into(),
            },

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

            InstrCacheable::Unknown { instr: _ } => Instruction {
                opcode: OpCode::Unknown,
                args: Args::DEFAULT,
            },
            InstrCacheable::UnknownCompressed { instr: _ } => Instruction {
                opcode: OpCode::Unknown,
                args: Args {
                    width: InstrWidth::Compressed,
                    ..Args::DEFAULT
                },
            },

            InstrCacheable::Hint { instr: _ } => Instruction::new_nop(InstrWidth::Uncompressed),
            InstrCacheable::HintCompressed { instr: _ } => {
                Instruction::new_nop(InstrWidth::Compressed)
            }
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

impl ITypeArgs {
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

impl NonZeroRdITypeArgs {
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

impl SBTypeArgs {
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
    use crate::{
        default::ConstDefault,
        machine_state::registers::{FRegister, NonZeroXRegister, XRegister},
    };

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
