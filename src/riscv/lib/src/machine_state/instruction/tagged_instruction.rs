// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{Args, CSRegister, FRegister, InstrRoundingMode, Instruction, OpCode, XRegister};
use crate::default::ConstDefault;
use serde::{Deserialize, Serialize};
use std::fmt::Debug;
use thiserror::Error;

#[derive(Error, Debug)]
/// Errors that may be returned when converting between tagged and untagged registers.
pub enum TaggedError {
    #[error("Expected XRegister got {0:?}")]
    UnwrapX(TaggedRegister),
    #[error("Expected FRegister got {0:?}")]
    UnwrapF(TaggedRegister),
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
}

/// Intermediate object for instantiating [Instruction] objects and
/// serialising [Args]. This is necessary because we can't
/// directly serialise Args due to the [Register] Union.
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
}

impl ConstDefault for TaggedArgs {
    const DEFAULT: Self = Self {
        rd: TaggedRegister::X(XRegister::x0),
        rs1: TaggedRegister::X(XRegister::x0),
        rs2: TaggedRegister::X(XRegister::x0),
        imm: 0,
        csr: CSRegister::fflags,
        rs3f: FRegister::f0,
        rm: InstrRoundingMode::Dynamic,
        aq: false,
        rl: false,
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
}

/// This function maps each opcode to the corresponding ArgsShape that the opcode uses
/// i.e. the types of registers used.
/// This is used to determine how to serialise and deserialise the Args for each opcode.
pub fn opcode_to_argsshape(opcode: &OpCode) -> ArgsShape {
    use OpCode::*;
    match opcode {
        Add | Sub | Xor | Or | And | Sll | Srl | Sra | Slt | Sltu | Addw | Subw | Sllw | Srlw
        | Sraw | Addi | Addiw | Xori | Ori | Andi | Slli | Srli | Srai | Slliw | Srliw | Sraiw
        | Slti | Sltiu | Lb | Lh | Lw | Lbu | Lhu | Lwu | Ld | Sb | Sh | Sw | Sd | Beq | Bne
        | Blt | Bge | Bltu | Bgeu | Lui | Auipc | Jal | Jalr | Lrw | Scw | Amoswapw | Amoaddw
        | Amoxorw | Amoandw | Amoorw | Amominw | Amomaxw | Amominuw | Amomaxuw | Lrd | Scd
        | Amoswapd | Amoaddd | Amoxord | Amoandd | Amoord | Amomind | Amomaxd | Amominud
        | Amomaxud | Rem | Remu | Remw | Remuw | Div | Divu | Divw | Divuw | Mul | Mulh
        | Mulhsu | Mulhu | Mulw | Csrrw | Csrrs | Csrrc | Csrrwi | Csrrsi | Csrrci | CLw
        | CLwsp | CSw | CSwsp | CJ | CJr | CJalr | CBeqz | CBnez | CLi | CLui | CAddi
        | CAddi16sp | CAddi4spn | CSlli | CSrli | CSrai | CAndi | CMv | CAdd | CAnd | COr
        | CXor | CSub | CAddw | CSubw | CNop | CLd | CLdsp | CSd | CSdsp | CAddiw | Unknown
        | UnknownCompressed => ArgsShape::XSrcXDest,

        Fadds | Fsubs | Fmuls | Fdivs | Fsqrts | Fmins | Fmaxs | Fsgnjs | Fsgnjns | Fsgnjxs
        | Fmadds | Fmsubs | Fnmsubs | Fnmadds | Faddd | Fsubd | Fmuld | Fdivd | Fsqrtd | Fmind
        | Fmaxd | Fsgnjd | Fsgnjnd | Fsgnjxd | Fcvtsd | Fcvtds | Fmaddd | Fmsubd | Fnmsubd
        | Fnmaddd => ArgsShape::FSrcFDest,

        Flw | Fld | FmvWX | Fcvtsw | Fcvtswu | Fcvtsl | Fcvtslu | FmvDX | Fcvtdw | Fcvtdwu
        | Fcvtdl | Fcvtdlu | CFld | CFldsp => ArgsShape::XSrcFDest,

        Feqs | Fles | Flts | Feqd | Fled | Fltd | FclassS | FmvXW | Fcvtws | Fcvtwus | Fcvtls
        | Fcvtlus | FclassD | FmvXD | Fcvtwd | Fcvtwud | Fcvtld | Fcvtlud => ArgsShape::FSrcXDest,

        Fsw | Fsd | CFsd | CFsdsp => ArgsShape::XSrcFSrc,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        default::ConstDefault,
        machine_state::{
            instruction::Args,
            registers::{FRegister, XRegister},
        },
    };

    #[test]
    fn test_instruction_serde() {
        let instr_xsrc_xdst = Instruction {
            opcode: OpCode::Add,
            args: Args {
                rd: XRegister::x0.into(),
                rs1: XRegister::x0.into(),
                rs2: XRegister::x0.into(),
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
                rs2: XRegister::x0.into(),
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
                rd: FRegister::f0.into(),
                rs1: XRegister::x0.into(),
                rs2: FRegister::f0.into(),
                ..Args::DEFAULT
            },
        };

        let instr_xsrc_fsrc_ser = bincode::serialize(&instr_xsrc_fsrc).unwrap();
        let instr_xsrc_fsrc_de: Instruction = bincode::deserialize(&instr_xsrc_fsrc_ser).unwrap();

        assert_eq!(instr_xsrc_fsrc, instr_xsrc_fsrc_de);

        // ensure width of all serialised instructions are the same
        let ser_len = instr_xsrc_xdst_ser.len();
        assert_eq!(instr_fsrc_fdst_ser.len(), ser_len);
        assert_eq!(instr_xsrc_fdst_ser.len(), ser_len);
        assert_eq!(instr_fsrc_xdst_ser.len(), ser_len);
        assert_eq!(instr_xsrc_fsrc_ser.len(), ser_len);
    }

    #[test]
    fn test_incorrect_tagged_instructions_fail_conversion() {
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
            },
        };

        let instr = Instruction::try_from(tagged_instr);
        assert!(instr.is_err());
    }
}
