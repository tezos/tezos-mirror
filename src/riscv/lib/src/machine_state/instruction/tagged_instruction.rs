// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::fmt::Debug;

use serde::Deserialize;
use serde::Serialize;
use thiserror::Error;

use super::Args;
use super::CSRegister;
use super::FRegister;
use super::InstrRoundingMode;
use super::Instruction;
use super::NonZeroXRegister;
use super::OpCode;
use super::XRegister;
use crate::default::ConstDefault;
use crate::parser::instruction::InstrWidth;

#[derive(Error, Debug)]
/// Errors that may be returned when converting between tagged and untagged registers.
pub enum TaggedError {
    #[error("Expected XRegister got {0:?}")]
    UnwrapX(TaggedRegister),
    #[error("Expected FRegister got {0:?}")]
    UnwrapF(TaggedRegister),
    #[error("Expected NonZeroXRegister got {0:?}")]
    UnwrapNZX(TaggedRegister),
}

impl TryFrom<TaggedInstruction> for Instruction {
    type Error = TaggedError;

    fn try_from(value: TaggedInstruction) -> Result<Self, Self::Error> {
        let args = match opcode_to_argsshape(&value.opcode) {
            ArgsShape::XSrcXDest => Args {
                rd: value.args.rd.unwrap_x()?.into(),
                rs1: value.args.rs1.unwrap_x()?.into(),
                rs2: value.args.rs2.unwrap_x()?.into(),
                imm: value.args.imm,
                csr: value.args.csr,
                rs3f: value.args.rs3f,
                rm: value.args.rm,
                aq: value.args.aq,
                rl: value.args.rl,
                width: value.args.width,
            },
            ArgsShape::FSrcFDest => Args {
                rd: value.args.rd.unwrap_f()?.into(),
                rs1: value.args.rs1.unwrap_f()?.into(),
                rs2: value.args.rs2.unwrap_f()?.into(),
                imm: value.args.imm,
                csr: value.args.csr,
                rs3f: value.args.rs3f,
                rm: value.args.rm,
                aq: value.args.aq,
                rl: value.args.rl,
                width: value.args.width,
            },
            ArgsShape::XSrcFDest => Args {
                rd: value.args.rd.unwrap_f()?.into(),
                rs1: value.args.rs1.unwrap_x()?.into(),
                rs2: value.args.rs2.unwrap_x()?.into(),
                imm: value.args.imm,
                csr: value.args.csr,
                rs3f: value.args.rs3f,
                rm: value.args.rm,
                aq: value.args.aq,
                rl: value.args.rl,
                width: value.args.width,
            },
            ArgsShape::FSrcXDest => Args {
                rd: value.args.rd.unwrap_x()?.into(),
                rs1: value.args.rs1.unwrap_f()?.into(),
                rs2: value.args.rs2.unwrap_f()?.into(),
                imm: value.args.imm,
                csr: value.args.csr,
                rs3f: value.args.rs3f,
                rm: value.args.rm,
                aq: value.args.aq,
                rl: value.args.rl,
                width: value.args.width,
            },
            ArgsShape::XSrcFSrc => Args {
                rd: value.args.rd.unwrap_x()?.into(),
                rs1: value.args.rs1.unwrap_x()?.into(),
                rs2: value.args.rs2.unwrap_f()?.into(),
                imm: value.args.imm,
                csr: value.args.csr,
                rs3f: value.args.rs3f,
                rm: value.args.rm,
                aq: value.args.aq,
                rl: value.args.rl,
                width: value.args.width,
            },
            ArgsShape::NZXSrcNZXDest => Args {
                rd: value.args.rd.unwrap_nzx()?.into(),
                rs1: value.args.rs1.unwrap_nzx()?.into(),
                rs2: value.args.rs2.unwrap_nzx()?.into(),
                imm: value.args.imm,
                csr: value.args.csr,
                rs3f: value.args.rs3f,
                rm: value.args.rm,
                aq: value.args.aq,
                rl: value.args.rl,
                width: value.args.width,
            },
            ArgsShape::XSrcNZXDest => Args {
                rd: value.args.rd.unwrap_nzx()?.into(),
                rs1: value.args.rs1.unwrap_x()?.into(),
                rs2: value.args.rs2.unwrap_x()?.into(),
                imm: value.args.imm,
                csr: value.args.csr,
                rs3f: value.args.rs3f,
                rm: value.args.rm,
                aq: value.args.aq,
                rl: value.args.rl,
                width: value.args.width,
            },
        };
        Ok(Instruction {
            opcode: value.opcode,
            args,
        })
    }
}

/// An intermediate object for creating and deserialising Instructions.
/// This is necessary because we can't directly create or deserialise into
/// an [Instruction] due to the [Register] Union.
///
/// [Register]: super::Register
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct TaggedInstruction {
    pub opcode: OpCode,
    pub args: TaggedArgs,
}

impl From<Instruction> for TaggedInstruction {
    fn from(value: Instruction) -> Self {
        let args = match opcode_to_argsshape(&value.opcode) {
            ArgsShape::XSrcXDest => TaggedArgs {
                rd: unsafe { value.args.rd.x.into() },
                rs1: unsafe { value.args.rs1.x.into() },
                rs2: unsafe { value.args.rs2.x.into() },
                imm: value.args.imm,
                csr: value.args.csr,
                rs3f: value.args.rs3f,
                rm: value.args.rm,
                aq: value.args.aq,
                rl: value.args.rl,
                width: value.args.width,
            },
            ArgsShape::FSrcFDest => TaggedArgs {
                rd: unsafe { value.args.rd.f.into() },
                rs1: unsafe { value.args.rs1.f.into() },
                rs2: unsafe { value.args.rs2.f.into() },
                imm: value.args.imm,
                csr: value.args.csr,
                rs3f: value.args.rs3f,
                rm: value.args.rm,
                aq: value.args.aq,
                rl: value.args.rl,
                width: value.args.width,
            },
            ArgsShape::XSrcFDest => TaggedArgs {
                rd: unsafe { value.args.rd.f.into() },
                rs1: unsafe { value.args.rs1.x.into() },
                rs2: unsafe { value.args.rs2.x.into() },
                imm: value.args.imm,
                csr: value.args.csr,
                rs3f: value.args.rs3f,
                rm: value.args.rm,
                aq: value.args.aq,
                rl: value.args.rl,
                width: value.args.width,
            },
            ArgsShape::FSrcXDest => TaggedArgs {
                rd: unsafe { value.args.rd.x.into() },
                rs1: unsafe { value.args.rs1.f.into() },
                rs2: unsafe { value.args.rs2.f.into() },
                imm: value.args.imm,
                csr: value.args.csr,
                rs3f: value.args.rs3f,
                rm: value.args.rm,
                aq: value.args.aq,
                rl: value.args.rl,
                width: value.args.width,
            },
            ArgsShape::XSrcFSrc => TaggedArgs {
                rd: unsafe { value.args.rd.x.into() },
                rs1: unsafe { value.args.rs1.x.into() },
                rs2: unsafe { value.args.rs2.f.into() },
                imm: value.args.imm,
                csr: value.args.csr,
                rs3f: value.args.rs3f,
                rm: value.args.rm,
                aq: value.args.aq,
                rl: value.args.rl,
                width: value.args.width,
            },
            ArgsShape::NZXSrcNZXDest => TaggedArgs {
                rd: unsafe { value.args.rd.nzx.into() },
                rs1: unsafe { value.args.rs1.nzx.into() },
                rs2: unsafe { value.args.rs2.nzx.into() },
                imm: value.args.imm,
                csr: value.args.csr,
                rs3f: value.args.rs3f,
                rm: value.args.rm,
                aq: value.args.aq,
                rl: value.args.rl,
                width: value.args.width,
            },
            ArgsShape::XSrcNZXDest => TaggedArgs {
                rd: unsafe { value.args.rd.nzx.into() },
                rs1: unsafe { value.args.rs1.x.into() },
                rs2: unsafe { value.args.rs2.x.into() },
                imm: value.args.imm,
                csr: value.args.csr,
                rs3f: value.args.rs3f,
                rm: value.args.rm,
                aq: value.args.aq,
                rl: value.args.rl,
                width: value.args.width,
            },
        };
        TaggedInstruction {
            opcode: value.opcode,
            args,
        }
    }
}

/// Integer or floating-point register.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TaggedRegister {
    X(XRegister),
    F(FRegister),
    NonZeroX(NonZeroXRegister),
}

impl From<XRegister> for TaggedRegister {
    fn from(value: XRegister) -> Self {
        TaggedRegister::X(value)
    }
}

impl From<FRegister> for TaggedRegister {
    fn from(value: FRegister) -> Self {
        TaggedRegister::F(value)
    }
}

impl From<NonZeroXRegister> for TaggedRegister {
    fn from(value: NonZeroXRegister) -> Self {
        TaggedRegister::NonZeroX(value)
    }
}

impl TaggedRegister {
    fn unwrap_x(self) -> Result<XRegister, TaggedError> {
        match self {
            Self::X(x) => Ok(x),
            _ => Err(TaggedError::UnwrapX(self)),
        }
    }

    fn unwrap_f(self) -> Result<FRegister, TaggedError> {
        match self {
            Self::F(f) => Ok(f),
            _ => Err(TaggedError::UnwrapF(self)),
        }
    }

    fn unwrap_nzx(self) -> Result<NonZeroXRegister, TaggedError> {
        match self {
            Self::NonZeroX(nzx) => Ok(nzx),
            _ => Err(TaggedError::UnwrapNZX(self)),
        }
    }
}

/// Intermediate object for instantiating [Instruction] objects and
/// serialising [Args]. This is necessary because we can't
/// directly serialise Args due to the [super::Register] Union.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct TaggedArgs {
    pub rd: TaggedRegister,
    pub rs1: TaggedRegister,
    pub rs2: TaggedRegister,
    pub imm: i64,
    pub csr: CSRegister,
    pub rs3f: FRegister,
    pub rm: InstrRoundingMode,
    pub aq: bool,
    pub rl: bool,
    pub width: InstrWidth,
}

impl ConstDefault for TaggedArgs {
    const DEFAULT: Self = Self {
        rd: TaggedRegister::NonZeroX(NonZeroXRegister::x1),
        rs1: TaggedRegister::NonZeroX(NonZeroXRegister::x1),
        rs2: TaggedRegister::NonZeroX(NonZeroXRegister::x1),
        imm: 0,
        csr: CSRegister::fflags,
        rs3f: FRegister::f0,
        rm: InstrRoundingMode::Dynamic,
        aq: false,
        rl: false,
        width: InstrWidth::Uncompressed,
    };
}

/// This enum represents the different forms of registers that are present
/// in the args for each opcode.
pub enum ArgsShape {
    // rd, rs1, rs2 => X
    XSrcXDest,
    // rd, rs1, rs2 => F
    FSrcFDest,
    // rd => F | rs1, rs2 => X
    XSrcFDest,
    // rd => X | rs1, rs2 => F
    FSrcXDest,
    // rd, rs1 => X | rs2 => F
    XSrcFSrc,
    // rd, rs1, rs2 => NZX
    NZXSrcNZXDest,
    // rd => NZX | rs1, rs2 => X
    XSrcNZXDest,
}

/// This function maps each opcode to the corresponding ArgsShape that the opcode uses
/// i.e. the types of registers used.
/// This is used to determine how to serialise and deserialise the Args for each opcode.
pub fn opcode_to_argsshape(opcode: &OpCode) -> ArgsShape {
    use OpCode::*;
    match opcode {
        X8LoadSigned | X16LoadSigned | X32LoadSigned | X8LoadUnsigned | X16LoadUnsigned
        | X32LoadUnsigned | X64LoadSigned | X8Store | X16Store | X32Store | X64Store | Lrw
        | Scw | Amoswapw | Amoaddw | Amoxorw | Amoandw | Amoorw | Amominw | Amomaxw | Amominuw
        | Amomaxuw | Lrd | Scd | Amoswapd | X64AtomicAdd | Amoxord | Amoandd | Amoord | Amomind
        | Amomaxd | Amominud | Amomaxud | Rem | Remu | Remw | Remuw | Divu | Divw | Divuw
        | X32Mul | Csrrw | Csrrs | Csrrc | Csrrwi | Csrrsi | Csrrci => ArgsShape::XSrcXDest,

        Fadds | Fsubs | Fmuls | Fdivs | Fsqrts | Fmins | Fmaxs | Fsgnjs | Fsgnjns | Fsgnjxs
        | Fmadds | Fmsubs | Fnmsubs | Fnmadds | Faddd | Fsubd | Fmuld | Fdivd | Fsqrtd | Fmind
        | Fmaxd | Fsgnjd | Fsgnjnd | Fsgnjxd | Fcvtsd | Fcvtds | Fmaddd | Fmsubd | Fnmsubd
        | Fnmaddd => ArgsShape::FSrcFDest,

        Flw | Fld | FmvWX | Fcvtsw | Fcvtswu | Fcvtsl | Fcvtslu | FmvDX | Fcvtdw | Fcvtdwu
        | Fcvtdl | Fcvtdlu | CFld | CFldsp => ArgsShape::XSrcFDest,

        Feqs | Fles | Flts | Feqd | Fled | Fltd | FclassS | FmvXW | Fcvtws | Fcvtwus | Fcvtls
        | Fcvtlus | FclassD | FmvXD | Fcvtwd | Fcvtwud | Fcvtld | Fcvtlud => ArgsShape::FSrcXDest,

        Fsw | Fsd | CFsd | CFsdsp => ArgsShape::XSrcFSrc,

        Addi
        | Andi
        | X64OrImm
        | X64XorImm
        | ShiftLeftImmediate
        | ShiftRightImmediateUnsigned
        | ShiftRightImmediateSigned
        | Add
        | Sub
        | Mul
        | X64MulHighSigned
        | X64MulHighSignedUnsigned
        | X64MulHighUnsigned
        | Mv
        | Neg
        | And
        | Or
        | X64Xor
        | ShiftLeft
        | ShiftRightUnsigned
        | ShiftRightSigned
        | X32ShiftLeftImm
        | X32ShiftRightImmUnsigned
        | X32ShiftRightImmSigned
        | Jal
        | J
        | JrImm
        | JAbsolute
        | JalrAbsolute
        | Jr
        | Jalr
        | Li
        | BranchEqual
        | BranchEqualZero
        | BranchNotEqual
        | BranchNotEqualZero
        | BranchLessThanSigned
        | BranchGreaterThanOrEqualSigned
        | BranchLessThanUnsigned
        | BranchGreaterThanOrEqualUnsigned
        | BranchLessThanZero
        | BranchGreaterThanOrEqualZero
        | BranchLessThanOrEqualZero
        | BranchGreaterThanZero
        | JalrImm
        | Nop
        | ECall
        | Unknown => ArgsShape::NZXSrcNZXDest,

        AddWordImmediate
        | AddWord
        | SubWord
        | X32ShiftLeft
        | X32ShiftRightUnsigned
        | X32ShiftRightSigned
        | SetLessThanSigned
        | SetLessThanUnsigned
        | SetLessThanImmediateSigned
        | SetLessThanImmediateUnsigned
        | AddImmediateToPC
        | X64DivSigned => ArgsShape::XSrcNZXDest,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::default::ConstDefault;
    use crate::machine_state::instruction::Args;
    use crate::machine_state::registers::FRegister;
    use crate::machine_state::registers::XRegister;

    #[test]
    fn test_miri_instruction_serde() {
        let instr_xsrc_xdst = Instruction {
            opcode: OpCode::Add,
            args: Args {
                rd: NonZeroXRegister::x1.into(),
                rs1: NonZeroXRegister::x2.into(),
                rs2: NonZeroXRegister::x2.into(),
                ..Args::DEFAULT
            },
        };

        let instr_xsrc_xdst_ser = bincode::serialize(&instr_xsrc_xdst).unwrap();
        let instr_xsrc_xdst_de: Instruction = bincode::deserialize(&instr_xsrc_xdst_ser).unwrap();

        assert_eq!(instr_xsrc_xdst, instr_xsrc_xdst_de);

        let instr_fsrc_fdst = Instruction {
            opcode: OpCode::Fadds,
            args: Args {
                rd: FRegister::f0.into(),
                rs1: FRegister::f0.into(),
                rs2: FRegister::f0.into(),
                ..Args::DEFAULT
            },
        };

        let instr_fsrc_fdst_ser = bincode::serialize(&instr_fsrc_fdst).unwrap();
        let instr_fsrc_fdst_de: Instruction = bincode::deserialize(&instr_fsrc_fdst_ser).unwrap();

        assert_eq!(instr_fsrc_fdst, instr_fsrc_fdst_de);

        let instr_xsrc_fdst = Instruction {
            opcode: OpCode::Flw,
            args: Args {
                rd: FRegister::f0.into(),
                rs1: XRegister::x0.into(),
                ..Args::DEFAULT
            },
        };

        let instr_xsrc_fdst_ser = bincode::serialize(&instr_xsrc_fdst).unwrap();
        let instr_xsrc_fdst_de: Instruction = bincode::deserialize(&instr_xsrc_fdst_ser).unwrap();

        assert_eq!(instr_xsrc_fdst, instr_xsrc_fdst_de);

        let instr_fsrc_xdst = Instruction {
            opcode: OpCode::Feqs,
            args: Args {
                rd: XRegister::x0.into(),
                rs1: FRegister::f0.into(),
                rs2: FRegister::f0.into(),
                ..Args::DEFAULT
            },
        };

        let instr_fsrc_xdst_ser = bincode::serialize(&instr_fsrc_xdst).unwrap();
        let instr_fsrc_xdst_de: Instruction = bincode::deserialize(&instr_fsrc_xdst_ser).unwrap();

        assert_eq!(instr_fsrc_xdst, instr_fsrc_xdst_de);

        let instr_xsrc_fsrc = Instruction {
            opcode: OpCode::Fsw,
            args: Args {
                rs1: XRegister::x0.into(),
                rs2: FRegister::f0.into(),
                ..Args::DEFAULT
            },
        };

        let instr_xsrc_fsrc_ser = bincode::serialize(&instr_xsrc_fsrc).unwrap();
        let instr_xsrc_fsrc_de: Instruction = bincode::deserialize(&instr_xsrc_fsrc_ser).unwrap();

        assert_eq!(instr_xsrc_fsrc, instr_xsrc_fsrc_de);

        let instr_nzxsrc_nzxdest = Instruction {
            opcode: OpCode::Jr,
            args: Args {
                rs1: NonZeroXRegister::x2.into(),
                ..Args::DEFAULT
            },
        };

        let instr_nzxsrc_nzxdest_ser = bincode::serialize(&instr_nzxsrc_nzxdest).unwrap();
        let instr_nzxsrc_nzxdest_de: Instruction =
            bincode::deserialize(&instr_nzxsrc_nzxdest_ser).unwrap();

        assert_eq!(instr_nzxsrc_nzxdest, instr_nzxsrc_nzxdest_de);

        // ensure width of all serialised instructions are the same
        let ser_len = instr_xsrc_xdst_ser.len();
        assert_eq!(instr_fsrc_fdst_ser.len(), ser_len);
        assert_eq!(instr_xsrc_fdst_ser.len(), ser_len);
        assert_eq!(instr_fsrc_xdst_ser.len(), ser_len);
        assert_eq!(instr_xsrc_fsrc_ser.len(), ser_len);
        assert_eq!(instr_nzxsrc_nzxdest_ser.len(), ser_len);
    }

    #[test]
    fn test_incorrectly_tagged_instructions_fail_conversion_fx() {
        let tagged_instr = TaggedInstruction {
            opcode: OpCode::Add,
            args: TaggedArgs {
                rd: FRegister::f0.into(),
                rs1: FRegister::f0.into(),
                rs2: FRegister::f0.into(),
                imm: 0,
                csr: CSRegister::fflags,
                rs3f: FRegister::f0,
                rm: InstrRoundingMode::Dynamic,
                aq: false,
                rl: false,
                width: InstrWidth::Uncompressed,
            },
        };

        let instr = Instruction::try_from(tagged_instr);
        assert!(instr.is_err());
    }

    #[test]
    fn test_incorrectly_tagged_instructions_fail_conversion_nzxx() {
        let tagged_instr = TaggedInstruction {
            opcode: OpCode::Add,
            args: TaggedArgs {
                rd: XRegister::x0.into(),
                rs1: XRegister::x0.into(),
                rs2: NonZeroXRegister::x1.into(),
                imm: 0,
                csr: CSRegister::fflags,
                rs3f: FRegister::f0,
                rm: InstrRoundingMode::Dynamic,
                aq: false,
                rl: false,
                width: InstrWidth::Uncompressed,
            },
        };

        let instr = Instruction::try_from(tagged_instr);
        assert!(instr.is_err());
    }
}
