// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
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

use super::{
    bus::main_memory::MainMemoryLayout,
    csregisters::CSRegister,
    registers::{FRegister, XRegister},
    MachineCoreState, ProgramCounterUpdate,
};
use crate::{
    machine_state::ProgramCounterUpdate::{Next, Set},
    parser::instruction::{
        AmoArgs, CIBDTypeArgs, CIBTypeArgs, CJTypeArgs, CRJTypeArgs, CRTypeArgs, CSSDTypeArgs,
        CSSTypeArgs, CsrArgs, CsriArgs, FCmpArgs, FLoadArgs, FR1ArgWithRounding,
        FR2ArgsWithRounding, FR3ArgsWithRounding, FRArgs, FRegToXRegArgs,
        FRegToXRegArgsWithRounding, FStoreArgs, ITypeArgs, InstrCacheable, InstrRoundingMode,
        InstrWidth, RTypeArgs, SBTypeArgs, UJTypeArgs, XRegToFRegArgs, XRegToFRegArgsWithRounding,
    },
    state_backend::{ManagerBase, ManagerReadWrite},
    traps::Exception,
};
use serde::{Deserialize, Serialize};

/// An instruction formed of an opcode and flat arguments.
///
/// This is preferred within the caches, as it enables 'pre-dispatch' of functions
/// at block construction, rather than during block execution.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Instruction {
    /// The operation (over the machine state) that this instruction represents.
    pub opcode: OpCode,
    /// Arguments that are passed to the opcode-function. As a flat structure, it contains
    /// all possible arguments. Each instruction will only use a subset.
    pub args: Args,
}

/// Opcodes map to the operation performed over the state - allowing us to
/// decouple these from the parsed instructions down the line.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum OpCode {
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
    CJ,
    CJr,
    CJalr,
    CBeqz,
    CBnez,
    CLi,
    CLui,
    CAddi,
    CAddi16sp,
    CAddi4spn,
    CSlli,
    CSrli,
    CSrai,
    CAndi,
    CMv,
    CAdd,
    CAnd,
    COr,
    CXor,
    CSub,
    CAddw,
    CSubw,
    CNop,

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

    Unknown,
    UnknownCompressed,
}

impl OpCode {
    /// Dispatch an opcode to the function that will run over the machine state.
    #[inline(always)]
    fn to_run<ML: MainMemoryLayout, M: ManagerReadWrite>(
        self,
    ) -> fn(&Args, &mut MachineCoreState<ML, M>) -> Result<ProgramCounterUpdate, Exception> {
        match self {
            Self::Add => Args::run_add,
            Self::Sub => Args::run_sub,
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
            Self::CJ => Args::run_cj,
            Self::CJr => Args::run_cjr,
            Self::CJalr => Args::run_cjalr,
            Self::CBeqz => Args::run_cbeqz,
            Self::CBnez => Args::run_cbnez,
            Self::CLi => Args::run_cli,
            Self::CLui => Args::run_clui,
            Self::CAddi => Args::run_caddi,
            Self::CAddi16sp => Args::run_caddi16spn,
            Self::CAddi4spn => Args::run_caddi4spn,
            Self::CSlli => Args::run_cslli,
            Self::CSrli => Args::run_csrli,
            Self::CSrai => Args::run_csrai,
            Self::CAndi => Args::run_candi,
            Self::CMv => Args::run_cmv,
            Self::CAdd => Args::run_cadd,
            Self::CAnd => Args::run_cand,
            Self::COr => Args::run_cor,
            Self::CXor => Args::run_cxor,
            Self::CSub => Args::run_csub,
            Self::CAddw => Args::run_caddw,
            Self::CSubw => Args::run_csubw,
            Self::CNop => Args::run_cnop,
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
            Self::UnknownCompressed => Args::run_illegal,
        }
    }
}

impl Instruction {
    /// Run an instruction over the machine core state.
    pub(super) fn run<ML: MainMemoryLayout, M: ManagerReadWrite>(
        &self,
        core: &mut MachineCoreState<ML, M>,
    ) -> Result<ProgramCounterUpdate, Exception> {
        (self.opcode.to_run())(&self.args, core)
    }
}

/// Contains all possible arguments used by opcode-functions.
///
/// Each opcode will only touch a subset of these.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub struct Args {
    pub rd: XRegister,
    pub rs1: XRegister,
    pub rs2: XRegister,
    pub imm: i64,
    pub csr: CSRegister,
    pub rdf: FRegister,
    pub rs1f: FRegister,
    pub rs2f: FRegister,
    pub rs3f: FRegister,
    pub rm: InstrRoundingMode,
    pub aq: bool,
    pub rl: bool,
}

impl Default for Args {
    fn default() -> Self {
        Self {
            rd: XRegister::x0,
            rs1: XRegister::x0,
            rs2: XRegister::x0,
            imm: 0,
            csr: CSRegister::fflags,
            rdf: FRegister::f0,
            rs1f: FRegister::f0,
            rs2f: FRegister::f0,
            rs3f: FRegister::f0,
            rm: InstrRoundingMode::Dynamic,
            aq: false,
            rl: false,
        }
    }
}

macro_rules! impl_r_type {
    ($fn: ident) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart.xregisters.$fn(self.rs1, self.rs2, self.rd);

            Ok(Next(InstrWidth::Uncompressed))
        }
    };
}

macro_rules! impl_i_type {
    ($fn: ident) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart.xregisters.$fn(self.imm, self.rs1, self.rd);

            Ok(Next(InstrWidth::Uncompressed))
        }
    };
}

macro_rules! impl_fload_type {
    ($fn: ident) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.$fn(self.imm, self.rs1, self.rdf)
                .map(|_| Next(InstrWidth::Uncompressed))
        }
    };
    ($fn: ident, $width: expr) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.$fn(self.imm, self.rs1, self.rdf).map(|_| Next($width))
        }
    };
}
macro_rules! impl_load_type {
    ($fn: ident) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.$fn(self.imm, self.rs1, self.rd)
                .map(|_| Next(InstrWidth::Uncompressed))
        }
    };
    ($fn: ident, $width: expr) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.$fn(self.imm, self.rs1, self.rd).map(|_| Next($width))
        }
    };
}
macro_rules! impl_cload_sp_type {
    ($fn: ident) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.$fn(self.imm, self.rd)
                .map(|_| Next(InstrWidth::Compressed))
        }
    };
}
macro_rules! impl_cfload_sp_type {
    ($fn: ident) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.$fn(self.imm, self.rdf)
                .map(|_| Next(InstrWidth::Compressed))
        }
    };
}

macro_rules! impl_store_type {
    ($fn: ident) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.$fn(self.imm, self.rs1, self.rs2)
                .map(|_| Next(InstrWidth::Uncompressed))
        }
    };
    ($fn: ident, $width: expr) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.$fn(self.imm, self.rs1, self.rs2).map(|_| Next($width))
        }
    };
}
macro_rules! impl_fstore_type {
    ($fn: ident) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.$fn(self.imm, self.rs1, self.rs2f)
                .map(|_| Next(InstrWidth::Uncompressed))
        }
    };
    ($fn: ident, $width: expr) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.$fn(self.imm, self.rs1, self.rs2f)
                .map(|_| Next($width))
        }
    };
}

macro_rules! impl_b_type {
    ($fn: ident) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            Ok(core.hart.$fn(self.imm, self.rs1, self.rs2))
        }
    };
}

macro_rules! impl_amo_type {
    ($fn: ident) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.$fn(self.rs1, self.rs2, self.rd, self.rl, self.aq)
                .map(|_| Next(InstrWidth::Uncompressed))
        }
    };
}

macro_rules! impl_ci_type {
    ($fn: ident) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart.xregisters.$fn(self.imm, self.rd);
            Ok(ProgramCounterUpdate::Next(InstrWidth::Compressed))
        }
    };
}

macro_rules! impl_cr_type {
    ($fn: ident) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart.xregisters.$fn(self.rd, self.rs2);
            Ok(ProgramCounterUpdate::Next(InstrWidth::Compressed))
        }
    };
}

macro_rules! impl_cb_type {
    ($fn: ident) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            Ok(core.hart.$fn(self.imm, self.rd))
        }
    };
}

macro_rules! impl_css_type {
    ($fn: ident) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.$fn(self.imm, self.rs2)
                .map(|_| Next(InstrWidth::Compressed))
        }
    };
}

macro_rules! impl_fcss_type {
    ($fn: ident) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.$fn(self.imm, self.rs2f)
                .map(|_| Next(InstrWidth::Compressed))
        }
    };
}

macro_rules! impl_csr_type {
    ($fn: ident) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart
                .$fn(self.csr, self.rs1, self.rd)
                .map(|_| Next(InstrWidth::Uncompressed))
        }
    };
}

macro_rules! impl_csr_imm_type {
    ($fn: ident) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart
                .$fn(self.csr, self.imm as u64, self.rd)
                .map(|_| Next(InstrWidth::Uncompressed))
        }
    };
}

macro_rules! impl_f_x_type {
    ($fn: ident) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart
                .$fn(self.rs1, self.rdf)
                .map(|_| Next(InstrWidth::Uncompressed))
        }
    };

    ($fn:ident, rm) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart
                .$fn(self.rs1, self.rm, self.rdf)
                .map(|_| Next(InstrWidth::Uncompressed))
        }
    };
}

macro_rules! impl_x_f_type {
    ($fn: ident) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart
                .$fn(self.rs1f, self.rd)
                .map(|_| Next(InstrWidth::Uncompressed))
        }
    };

    ($fn:ident, rm) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
        ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart
                .$fn(self.rs1f, self.rm, self.rd)
                .map(|_| Next(InstrWidth::Uncompressed))
        }
    };
}

macro_rules! impl_f_r_type {
    ($fn: ident) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
            ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart.$fn(self.rs1f, self.rs2f, self.rdf).map(|_| Next(InstrWidth::Uncompressed))
        }
    };

    ($fn: ident, rd) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
            ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart.$fn(self.rs1f, self.rs2f, self.rd).map(|_| Next(InstrWidth::Uncompressed))
        }
    };

    ($fn:ident, $($field: ident),+) => {
        fn $fn<ML: MainMemoryLayout, M: ManagerReadWrite>(
            &self,
            core: &mut MachineCoreState<ML, M>,
            ) -> Result<ProgramCounterUpdate, Exception> {
            core.hart.$fn(self.rs1f, $(self.$field,)* self.rdf).map(|_| Next(InstrWidth::Uncompressed))
        }
    };
}

impl Args {
    // RV64I R-type instructions
    impl_r_type!(run_add);
    impl_r_type!(run_sub);
    impl_r_type!(run_xor);
    impl_r_type!(run_or);
    impl_r_type!(run_and);
    impl_r_type!(run_sll);
    impl_r_type!(run_srl);
    impl_r_type!(run_sra);
    impl_r_type!(run_slt);
    impl_r_type!(run_sltu);
    impl_r_type!(run_addw);
    impl_r_type!(run_subw);
    impl_r_type!(run_sllw);
    impl_r_type!(run_srlw);
    impl_r_type!(run_sraw);

    // RV64I I-type instructions
    impl_i_type!(run_addi);
    impl_i_type!(run_addiw);
    impl_i_type!(run_xori);
    impl_i_type!(run_ori);
    impl_i_type!(run_andi);
    impl_i_type!(run_slli);
    impl_i_type!(run_srli);
    impl_i_type!(run_srai);
    impl_i_type!(run_slliw);
    impl_i_type!(run_srliw);
    impl_i_type!(run_sraiw);
    impl_i_type!(run_slti);
    impl_i_type!(run_sltiu);
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
    impl_b_type!(run_beq);
    impl_b_type!(run_bne);
    impl_b_type!(run_blt);
    impl_b_type!(run_bge);
    impl_b_type!(run_bltu);
    impl_b_type!(run_bgeu);

    // RV64I U-type instructions
    fn run_lui<ML: MainMemoryLayout, M: ManagerReadWrite>(
        &self,
        core: &mut MachineCoreState<ML, M>,
    ) -> Result<ProgramCounterUpdate, Exception> {
        core.hart.xregisters.run_lui(self.imm, self.rd);
        Ok(Next(InstrWidth::Uncompressed))
    }

    fn run_auipc<ML: MainMemoryLayout, M: ManagerReadWrite>(
        &self,
        core: &mut MachineCoreState<ML, M>,
    ) -> Result<ProgramCounterUpdate, Exception> {
        core.hart.run_auipc(self.imm, self.rd);
        Ok(Next(InstrWidth::Uncompressed))
    }

    // RV64I jump instructions
    fn run_jal<ML: MainMemoryLayout, M: ManagerReadWrite>(
        &self,
        core: &mut MachineCoreState<ML, M>,
    ) -> Result<ProgramCounterUpdate, Exception> {
        Ok(Set(core.hart.run_jal(self.imm, self.rd)))
    }

    fn run_jalr<ML: MainMemoryLayout, M: ManagerReadWrite>(
        &self,
        core: &mut MachineCoreState<ML, M>,
    ) -> Result<ProgramCounterUpdate, Exception> {
        Ok(Set(core.hart.run_jalr(self.imm, self.rs1, self.rd)))
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
    impl_f_r_type!(run_feq_s, rd);
    impl_f_r_type!(run_fle_s, rd);
    impl_f_r_type!(run_flt_s, rd);
    impl_f_r_type!(run_fadd_s, rs2f, rm);
    impl_f_r_type!(run_fsub_s, rs2f, rm);
    impl_f_r_type!(run_fmul_s, rs2f, rm);
    impl_f_r_type!(run_fdiv_s, rs2f, rm);
    impl_f_r_type!(run_fsqrt_s, rm);
    impl_f_r_type!(run_fmin_s);
    impl_f_r_type!(run_fmax_s);
    impl_f_r_type!(run_fsgnj_s);
    impl_f_r_type!(run_fsgnjn_s);
    impl_f_r_type!(run_fsgnjx_s);
    impl_f_r_type!(run_fmadd_s, rs2f, rs3f, rm);
    impl_f_r_type!(run_fmsub_s, rs2f, rs3f, rm);
    impl_f_r_type!(run_fnmsub_s, rs2f, rs3f, rm);
    impl_f_r_type!(run_fnmadd_s, rs2f, rs3f, rm);
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
    impl_f_r_type!(run_feq_d, rd);
    impl_f_r_type!(run_fle_d, rd);
    impl_f_r_type!(run_flt_d, rd);
    impl_f_r_type!(run_fadd_d, rs2f, rm);
    impl_f_r_type!(run_fsub_d, rs2f, rm);
    impl_f_r_type!(run_fmul_d, rs2f, rm);
    impl_f_r_type!(run_fdiv_d, rs2f, rm);
    impl_f_r_type!(run_fsqrt_d, rm);
    impl_f_r_type!(run_fmin_d);
    impl_f_r_type!(run_fmax_d);
    impl_f_r_type!(run_fsgnj_d);
    impl_f_r_type!(run_fsgnjn_d);
    impl_f_r_type!(run_fsgnjx_d);
    impl_f_r_type!(run_fcvt_d_s, rm);
    impl_f_r_type!(run_fcvt_s_d, rm);
    impl_f_r_type!(run_fmadd_d, rs2f, rs3f, rm);
    impl_f_r_type!(run_fmsub_d, rs2f, rs3f, rm);
    impl_f_r_type!(run_fnmsub_d, rs2f, rs3f, rm);
    impl_f_r_type!(run_fnmadd_d, rs2f, rs3f, rm);
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
    impl_load_type!(run_clw, InstrWidth::Compressed);
    impl_cload_sp_type!(run_clwsp);
    impl_store_type!(run_csw, InstrWidth::Compressed);
    impl_cb_type!(run_cbeqz);
    impl_cb_type!(run_cbnez);
    impl_ci_type!(run_cli);
    impl_ci_type!(run_clui);
    impl_ci_type!(run_caddi);
    impl_ci_type!(run_caddi4spn);
    impl_ci_type!(run_cslli);
    impl_ci_type!(run_csrli);
    impl_ci_type!(run_csrai);
    impl_ci_type!(run_candi);
    impl_cr_type!(run_cmv);
    impl_cr_type!(run_cadd);
    impl_cr_type!(run_cand);
    impl_cr_type!(run_cxor);
    impl_cr_type!(run_cor);
    impl_cr_type!(run_csub);
    impl_css_type!(run_cswsp);

    fn run_caddi16spn<ML: MainMemoryLayout, M: ManagerReadWrite>(
        &self,
        core: &mut MachineCoreState<ML, M>,
    ) -> Result<ProgramCounterUpdate, Exception> {
        core.hart.xregisters.run_caddi16sp(self.imm);
        Ok(Next(InstrWidth::Compressed))
    }

    fn run_cj<ML: MainMemoryLayout, M: ManagerReadWrite>(
        &self,
        core: &mut MachineCoreState<ML, M>,
    ) -> Result<ProgramCounterUpdate, Exception> {
        Ok(Set(core.hart.run_cj(self.imm)))
    }

    fn run_cjr<ML: MainMemoryLayout, M: ManagerReadWrite>(
        &self,
        core: &mut MachineCoreState<ML, M>,
    ) -> Result<ProgramCounterUpdate, Exception> {
        Ok(Set(core.hart.run_cjr(self.rs1)))
    }

    fn run_cjalr<ML: MainMemoryLayout, M: ManagerReadWrite>(
        &self,
        core: &mut MachineCoreState<ML, M>,
    ) -> Result<ProgramCounterUpdate, Exception> {
        Ok(Set(core.hart.run_cjalr(self.rs1)))
    }

    fn run_cnop<ML: MainMemoryLayout, M: ManagerReadWrite>(
        &self,
        core: &mut MachineCoreState<ML, M>,
    ) -> Result<ProgramCounterUpdate, Exception> {
        core.run_cnop();
        Ok(Next(InstrWidth::Compressed))
    }

    // RV64C compressed instructions
    impl_store_type!(run_csd, InstrWidth::Compressed);
    impl_css_type!(run_csdsp);
    impl_load_type!(run_cld, InstrWidth::Compressed);
    impl_cload_sp_type!(run_cldsp);
    impl_ci_type!(run_caddiw);
    impl_cr_type!(run_caddw);
    impl_cr_type!(run_csubw);

    // RV64C compressed instructions
    impl_fload_type!(run_cfld, InstrWidth::Compressed);
    impl_cfload_sp_type!(run_cfldsp);
    impl_fstore_type!(run_cfsd, InstrWidth::Compressed);
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
            InstrCacheable::Add(args) => Instruction {
                opcode: OpCode::Add,
                args: args.into(),
            },
            InstrCacheable::Sub(args) => Instruction {
                opcode: OpCode::Sub,
                args: args.into(),
            },
            InstrCacheable::Xor(args) => Instruction {
                opcode: OpCode::Xor,
                args: args.into(),
            },
            InstrCacheable::Or(args) => Instruction {
                opcode: OpCode::Or,
                args: args.into(),
            },
            InstrCacheable::And(args) => Instruction {
                opcode: OpCode::And,
                args: args.into(),
            },
            InstrCacheable::Sll(args) => Instruction {
                opcode: OpCode::Sll,
                args: args.into(),
            },
            InstrCacheable::Srl(args) => Instruction {
                opcode: OpCode::Srl,
                args: args.into(),
            },
            InstrCacheable::Sra(args) => Instruction {
                opcode: OpCode::Sra,
                args: args.into(),
            },
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
            InstrCacheable::Addi(args) => Instruction {
                opcode: OpCode::Addi,
                args: args.into(),
            },
            InstrCacheable::Addiw(args) => Instruction {
                opcode: OpCode::Addiw,
                args: args.into(),
            },
            InstrCacheable::Xori(args) => Instruction {
                opcode: OpCode::Xori,
                args: args.into(),
            },
            InstrCacheable::Ori(args) => Instruction {
                opcode: OpCode::Ori,
                args: args.into(),
            },
            InstrCacheable::Andi(args) => Instruction {
                opcode: OpCode::Andi,
                args: args.into(),
            },
            InstrCacheable::Slli(args) => Instruction {
                opcode: OpCode::Slli,
                args: args.into(),
            },
            InstrCacheable::Srli(args) => Instruction {
                opcode: OpCode::Srli,
                args: args.into(),
            },
            InstrCacheable::Srai(args) => Instruction {
                opcode: OpCode::Srai,
                args: args.into(),
            },
            InstrCacheable::Slliw(args) => Instruction {
                opcode: OpCode::Slliw,
                args: args.into(),
            },
            InstrCacheable::Srliw(args) => Instruction {
                opcode: OpCode::Srliw,
                args: args.into(),
            },
            InstrCacheable::Sraiw(args) => Instruction {
                opcode: OpCode::Sraiw,
                args: args.into(),
            },
            InstrCacheable::Slti(args) => Instruction {
                opcode: OpCode::Slti,
                args: args.into(),
            },
            InstrCacheable::Sltiu(args) => Instruction {
                opcode: OpCode::Sltiu,
                args: args.into(),
            },
            InstrCacheable::Lb(args) => Instruction {
                opcode: OpCode::Lb,
                args: args.into(),
            },
            InstrCacheable::Lh(args) => Instruction {
                opcode: OpCode::Lh,
                args: args.into(),
            },
            InstrCacheable::Lw(args) => Instruction {
                opcode: OpCode::Lw,
                args: args.into(),
            },
            InstrCacheable::Lbu(args) => Instruction {
                opcode: OpCode::Lbu,
                args: args.into(),
            },
            InstrCacheable::Lhu(args) => Instruction {
                opcode: OpCode::Lhu,
                args: args.into(),
            },
            InstrCacheable::Lwu(args) => Instruction {
                opcode: OpCode::Lwu,
                args: args.into(),
            },
            InstrCacheable::Ld(args) => Instruction {
                opcode: OpCode::Ld,
                args: args.into(),
            },
            // RV64I S-type instructions
            InstrCacheable::Sb(args) => Instruction {
                opcode: OpCode::Sb,
                args: args.into(),
            },
            InstrCacheable::Sh(args) => Instruction {
                opcode: OpCode::Sh,
                args: args.into(),
            },
            InstrCacheable::Sw(args) => Instruction {
                opcode: OpCode::Sw,
                args: args.into(),
            },
            InstrCacheable::Sd(args) => Instruction {
                opcode: OpCode::Sd,
                args: args.into(),
            },

            // RV64I B-type instructions
            InstrCacheable::Beq(args) => Instruction {
                opcode: OpCode::Beq,
                args: args.into(),
            },
            InstrCacheable::Bne(args) => Instruction {
                opcode: OpCode::Bne,
                args: args.into(),
            },
            InstrCacheable::Blt(args) => Instruction {
                opcode: OpCode::Blt,
                args: args.into(),
            },
            InstrCacheable::Bge(args) => Instruction {
                opcode: OpCode::Bge,
                args: args.into(),
            },
            InstrCacheable::Bltu(args) => Instruction {
                opcode: OpCode::Bltu,
                args: args.into(),
            },
            InstrCacheable::Bgeu(args) => Instruction {
                opcode: OpCode::Bgeu,
                args: args.into(),
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
            InstrCacheable::Jal(args) => Instruction {
                opcode: OpCode::Jal,
                args: args.into(),
            },
            InstrCacheable::Jalr(args) => Instruction {
                opcode: OpCode::Jalr,
                args: args.into(),
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
                args: args.into(),
            },
            InstrCacheable::Fsw(args) => Instruction {
                opcode: OpCode::Fsw,
                args: args.into(),
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
                args: args.into(),
            },
            InstrCacheable::Fsd(args) => Instruction {
                opcode: OpCode::Fsd,
                args: args.into(),
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
                args: args.into(),
            },
            InstrCacheable::CLwsp(args) => Instruction {
                opcode: OpCode::CLwsp,
                args: args.into(),
            },
            InstrCacheable::CSw(args) => Instruction {
                opcode: OpCode::CSw,
                args: args.into(),
            },
            InstrCacheable::CSwsp(args) => Instruction {
                opcode: OpCode::CSwsp,
                args: args.into(),
            },
            InstrCacheable::CJ(args) => Instruction {
                opcode: OpCode::CJ,
                args: args.into(),
            },
            InstrCacheable::CJr(args) => Instruction {
                opcode: OpCode::CJr,
                args: args.into(),
            },
            InstrCacheable::CJalr(args) => Instruction {
                opcode: OpCode::CJalr,
                args: args.into(),
            },
            InstrCacheable::CBeqz(args) => Instruction {
                opcode: OpCode::CBeqz,
                args: args.into(),
            },
            InstrCacheable::CBnez(args) => Instruction {
                opcode: OpCode::CBnez,
                args: args.into(),
            },
            InstrCacheable::CLi(args) => Instruction {
                opcode: OpCode::CLi,
                args: args.into(),
            },
            InstrCacheable::CLui(args) => Instruction {
                opcode: OpCode::CLui,
                args: args.into(),
            },
            InstrCacheable::CAddi(args) => Instruction {
                opcode: OpCode::CAddi,
                args: args.into(),
            },
            InstrCacheable::CAddi16sp(args) => Instruction {
                opcode: OpCode::CAddi16sp,
                args: args.into(),
            },
            InstrCacheable::CAddi4spn(args) => Instruction {
                opcode: OpCode::CAddi4spn,
                args: args.into(),
            },
            InstrCacheable::CSlli(args) => Instruction {
                opcode: OpCode::CSlli,
                args: args.into(),
            },
            InstrCacheable::CSrli(args) => Instruction {
                opcode: OpCode::CSrli,
                args: args.into(),
            },
            InstrCacheable::CSrai(args) => Instruction {
                opcode: OpCode::CSrai,
                args: args.into(),
            },
            InstrCacheable::CAndi(args) => Instruction {
                opcode: OpCode::CAndi,
                args: args.into(),
            },
            InstrCacheable::CMv(args) => Instruction {
                opcode: OpCode::CMv,
                args: args.into(),
            },
            InstrCacheable::CAdd(args) => Instruction {
                opcode: OpCode::CAdd,
                args: args.into(),
            },
            InstrCacheable::CAnd(args) => Instruction {
                opcode: OpCode::CAnd,
                args: args.into(),
            },
            InstrCacheable::CXor(args) => Instruction {
                opcode: OpCode::CXor,
                args: args.into(),
            },
            InstrCacheable::COr(args) => Instruction {
                opcode: OpCode::COr,
                args: args.into(),
            },
            InstrCacheable::CSub(args) => Instruction {
                opcode: OpCode::CSub,
                args: args.into(),
            },
            InstrCacheable::CNop => Instruction {
                opcode: OpCode::CNop,
                args: Default::default(),
            },

            // RV64C compressed instructions
            InstrCacheable::CLd(args) => Instruction {
                opcode: OpCode::CLd,
                args: args.into(),
            },
            InstrCacheable::CLdsp(args) => Instruction {
                opcode: OpCode::CLdsp,
                args: args.into(),
            },
            InstrCacheable::CSd(args) => Instruction {
                opcode: OpCode::CSd,
                args: args.into(),
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
                args: args.into(),
            },
            InstrCacheable::CFldsp(args) => Instruction {
                opcode: OpCode::CFldsp,
                args: args.into(),
            },
            InstrCacheable::CFsd(args) => Instruction {
                opcode: OpCode::CFsd,
                args: args.into(),
            },
            InstrCacheable::CFsdsp(args) => Instruction {
                opcode: OpCode::CFsdsp,
                args: args.into(),
            },

            InstrCacheable::Unknown { instr: _ } => Instruction {
                opcode: OpCode::Unknown,
                args: Default::default(),
            },
            InstrCacheable::UnknownCompressed { instr: _ } => Instruction {
                opcode: OpCode::UnknownCompressed,
                args: Default::default(),
            },
        }
    }
}

impl From<&RTypeArgs> for Args {
    fn from(value: &RTypeArgs) -> Self {
        Self {
            rd: value.rd,
            rs1: value.rs1,
            rs2: value.rs2,
            ..Default::default()
        }
    }
}

impl From<&ITypeArgs> for Args {
    fn from(value: &ITypeArgs) -> Self {
        Self {
            rd: value.rd,
            rs1: value.rs1,
            imm: value.imm,
            ..Default::default()
        }
    }
}

impl From<&SBTypeArgs> for Args {
    fn from(value: &SBTypeArgs) -> Self {
        Self {
            rs1: value.rs1,
            rs2: value.rs2,
            imm: value.imm,
            ..Default::default()
        }
    }
}

impl From<&UJTypeArgs> for Args {
    fn from(value: &UJTypeArgs) -> Self {
        Self {
            rd: value.rd,
            imm: value.imm,
            ..Default::default()
        }
    }
}

impl From<&AmoArgs> for Args {
    fn from(value: &AmoArgs) -> Self {
        Self {
            rd: value.rd,
            rs1: value.rs1,
            rs2: value.rs2,
            aq: value.aq,
            rl: value.rl,
            ..Default::default()
        }
    }
}

impl From<&CIBTypeArgs> for Args {
    fn from(value: &CIBTypeArgs) -> Self {
        Self {
            rd: value.rd_rs1,
            imm: value.imm,
            ..Default::default()
        }
    }
}

impl From<&CRTypeArgs> for Args {
    fn from(value: &CRTypeArgs) -> Self {
        Self {
            rd: value.rd_rs1,
            rs2: value.rs2,
            ..Default::default()
        }
    }
}

impl From<&CJTypeArgs> for Args {
    fn from(value: &CJTypeArgs) -> Self {
        Self {
            imm: value.imm,
            ..Default::default()
        }
    }
}

impl From<&CRJTypeArgs> for Args {
    fn from(value: &CRJTypeArgs) -> Self {
        Self {
            rs1: value.rs1,
            ..Default::default()
        }
    }
}

impl From<&CSSTypeArgs> for Args {
    fn from(value: &CSSTypeArgs) -> Self {
        Self {
            rs2: value.rs2,
            imm: value.imm,
            ..Default::default()
        }
    }
}

impl From<&CsrArgs> for Args {
    fn from(value: &CsrArgs) -> Self {
        Self {
            rd: value.rd,
            rs1: value.rs1,
            csr: value.csr,
            ..Default::default()
        }
    }
}

impl From<&CsriArgs> for Args {
    fn from(value: &CsriArgs) -> Self {
        Self {
            rd: value.rd,
            imm: value.imm,
            csr: value.csr,
            ..Default::default()
        }
    }
}

impl From<&FLoadArgs> for Args {
    fn from(value: &FLoadArgs) -> Self {
        Self {
            imm: value.imm,
            rdf: value.rd,
            rs1: value.rs1,
            ..Default::default()
        }
    }
}

impl From<&FStoreArgs> for Args {
    fn from(value: &FStoreArgs) -> Self {
        Self {
            imm: value.imm,
            rs1: value.rs1,
            rs2f: value.rs2,
            ..Default::default()
        }
    }
}

impl From<&CSSDTypeArgs> for Args {
    fn from(value: &CSSDTypeArgs) -> Self {
        Self {
            imm: value.imm,
            rs2f: value.rs2,
            ..Default::default()
        }
    }
}

impl From<&CIBDTypeArgs> for Args {
    fn from(value: &CIBDTypeArgs) -> Self {
        Self {
            imm: value.imm,
            rdf: value.rd_rs1,
            ..Default::default()
        }
    }
}

impl From<&XRegToFRegArgs> for Args {
    fn from(value: &XRegToFRegArgs) -> Self {
        Self {
            rdf: value.rd,
            rs1: value.rs1,
            ..Default::default()
        }
    }
}

impl From<&XRegToFRegArgsWithRounding> for Args {
    fn from(value: &XRegToFRegArgsWithRounding) -> Self {
        Self {
            rdf: value.rd,
            rs1: value.rs1,
            rm: value.rm,
            ..Default::default()
        }
    }
}

impl From<&FRegToXRegArgs> for Args {
    fn from(value: &FRegToXRegArgs) -> Self {
        Self {
            rd: value.rd,
            rs1f: value.rs1,
            ..Default::default()
        }
    }
}

impl From<&FRegToXRegArgsWithRounding> for Args {
    fn from(value: &FRegToXRegArgsWithRounding) -> Self {
        Self {
            rd: value.rd,
            rs1f: value.rs1,
            rm: value.rm,
            ..Default::default()
        }
    }
}

impl From<&FR3ArgsWithRounding> for Args {
    fn from(value: &FR3ArgsWithRounding) -> Self {
        Self {
            rdf: value.rd,
            rs1f: value.rs1,
            rs2f: value.rs2,
            rs3f: value.rs3,
            rm: value.rm,
            ..Default::default()
        }
    }
}

impl From<&FRArgs> for Args {
    fn from(value: &FRArgs) -> Self {
        Self {
            rdf: value.rd,
            rs1f: value.rs1,
            rs2f: value.rs2,
            ..Default::default()
        }
    }
}

impl From<&FR1ArgWithRounding> for Args {
    fn from(value: &FR1ArgWithRounding) -> Self {
        Self {
            rdf: value.rd,
            rs1f: value.rs1,
            rm: value.rm,
            ..Default::default()
        }
    }
}

impl From<&FR2ArgsWithRounding> for Args {
    fn from(value: &FR2ArgsWithRounding) -> Self {
        Self {
            rdf: value.rd,
            rs1f: value.rs1,
            rs2f: value.rs2,
            rm: value.rm,
            ..Default::default()
        }
    }
}

impl From<&FCmpArgs> for Args {
    fn from(value: &FCmpArgs) -> Self {
        Self {
            rd: value.rd,
            rs1f: value.rs1,
            rs2f: value.rs2,
            ..Default::default()
        }
    }
}
