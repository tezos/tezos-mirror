// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::Args;
use super::Instruction;
use super::OpCode;
use crate::default::ConstDefault;
use crate::machine_state::registers::NonZeroXRegister;
use crate::machine_state::registers::XRegister;
use crate::machine_state::registers::nz;
use crate::machine_state::registers::sp;
use crate::machine_state::registers::x0;
use crate::parser::XRegisterParsed;
use crate::parser::instruction::CIBTypeArgs;
use crate::parser::instruction::CRTypeArgs;
use crate::parser::instruction::CSSTypeArgs;
use crate::parser::instruction::ITypeArgs;
use crate::parser::instruction::InstrWidth;
use crate::parser::instruction::NonZeroRdITypeArgs;
use crate::parser::instruction::NonZeroRdRTypeArgs;
use crate::parser::instruction::RTypeArgs;
use crate::parser::instruction::SBTypeArgs;
use crate::parser::instruction::SplitITypeArgs;
use crate::parser::instruction::UJTypeArgs;
use crate::parser::split_x0;

impl Instruction {
    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for the [`OpCode::Add`].
    pub(crate) fn new_add(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Add,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for the [`OpCode::Sub`].
    pub(crate) fn new_sub(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Sub,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for the [`OpCode::Neg`].
    pub(crate) fn new_neg(rd: NonZeroXRegister, rs2: NonZeroXRegister, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Neg,
            args: Args {
                rd: rd.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Mv`].
    pub(crate) fn new_mv(rd: NonZeroXRegister, rs2: NonZeroXRegister, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Mv,
            args: Args {
                rd: rd.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Li`].
    pub(crate) fn new_li(rd: NonZeroXRegister, imm: i64, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Li,
            args: Args {
                rd: rd.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for  [`OpCode::Nop`].
    pub(crate) fn new_nop(width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Nop,
            args: Args {
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Addi`].
    pub(crate) fn new_addi(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Addi,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Andi`].
    pub(crate) fn new_andi(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Andi,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Ori`].
    pub(crate) fn new_ori(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Ori,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Xori`].
    pub(crate) fn new_xori(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Xori,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Slli`].
    pub(crate) fn new_slli(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Slli,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Srli`].
    pub(crate) fn new_srli(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Srli,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Srai`].
    pub(crate) fn new_srai(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Srai,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for
    /// [`OpCode::SetLessThanSigned`].
    pub(crate) fn new_set_less_than_signed(
        rd: NonZeroXRegister,
        rs1: XRegister,
        rs2: XRegister,
    ) -> Self {
        Self {
            opcode: OpCode::SetLessThanSigned,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width: InstrWidth::Uncompressed,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::SetLessThanUnsigned`].
    pub(crate) fn new_set_less_than_unsigned(
        rd: NonZeroXRegister,
        rs1: XRegister,
        rs2: XRegister,
    ) -> Self {
        Self {
            opcode: OpCode::SetLessThanUnsigned,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width: InstrWidth::Uncompressed,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::SetLessThanImmediateSigned`].
    pub(crate) fn new_set_less_than_immediate_signed(
        rd: NonZeroXRegister,
        rs1: XRegister,
        imm: i64,
    ) -> Self {
        Self {
            opcode: OpCode::SetLessThanImmediateSigned,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width: InstrWidth::Uncompressed,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::SetLessThanImmediateUnsigned`].
    pub(crate) fn new_set_less_than_immediate_unsigned(
        rd: NonZeroXRegister,
        rs1: XRegister,
        imm: i64,
    ) -> Self {
        Self {
            opcode: OpCode::SetLessThanImmediateUnsigned,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width: InstrWidth::Uncompressed,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::And`].
    pub(crate) fn new_and(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::And,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Or`].
    pub(crate) fn new_or(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Or,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Xor`].
    pub(crate) fn new_xor(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Xor,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Sll`].
    pub(crate) fn new_sll(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Sll,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Srl`].
    pub(crate) fn new_srl(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Srl,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Sra`].
    pub(crate) fn new_sra(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Sra,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::J`].
    pub(crate) fn new_j(imm: i64, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::J,
            args: Args {
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Jal`].
    pub(crate) fn new_jal(rd: NonZeroXRegister, imm: i64, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Jal,
            args: Args {
                rd: rd.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Beq`].
    pub(crate) fn new_beq(
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Beq,
            args: Args {
                rs1: rs1.into(),
                rs2: rs2.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Beqz`].
    pub(crate) fn new_beqz(rs1: NonZeroXRegister, imm: i64, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Beqz,
            args: Args {
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Bne`].
    pub(crate) fn new_bne(
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Bne,
            args: Args {
                rs1: rs1.into(),
                rs2: rs2.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Bnez`].
    pub(crate) fn new_bnez(rs1: NonZeroXRegister, imm: i64, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Bnez,
            args: Args {
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::JAbsolute`].
    pub(crate) fn new_j_absolute(imm: i64, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::JAbsolute,
            args: Args {
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::JalrImm`].
    pub(crate) fn new_jalr_imm(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::JalrImm,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::JalrAbsolute`].
    pub(crate) fn new_jalr_absolute(rd: NonZeroXRegister, imm: i64, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::JalrAbsolute,
            args: Args {
                rd: rd.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::JrImm`].
    pub(crate) fn new_jr_imm(rs1: NonZeroXRegister, imm: i64, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::JrImm,
            args: Args {
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Jr`].
    pub(crate) fn new_jr(rs1: NonZeroXRegister, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Jr,
            args: Args {
                rs1: rs1.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Jalr`].
    pub(crate) fn new_jalr(rd: NonZeroXRegister, rs1: NonZeroXRegister, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Jalr,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Ld`].
    pub(crate) fn new_ld(rd: XRegister, rs1: XRegister, imm: i64, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Ld,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Ldnz`].
    pub(crate) fn new_ldnz(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Ldnz,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Sd`].
    pub(crate) fn new_sd(rs1: XRegister, rs2: XRegister, imm: i64, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Sd,
            args: Args {
                rs1: rs1.into(),
                rs2: rs2.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Sdnz`].
    pub(crate) fn new_sdnz(
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Sdnz,
            args: Args {
                rs1: rs1.into(),
                rs2: rs2.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Lw`].
    pub(crate) fn new_lw(rd: XRegister, rs1: XRegister, imm: i64, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Lw,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Lwnz`].
    pub(crate) fn new_lwnz(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Lwnz,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Sw`].
    pub(crate) fn new_sw(rs1: XRegister, rs2: XRegister, imm: i64, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Sw,
            args: Args {
                rs1: rs1.into(),
                rs2: rs2.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Swnz`].
    pub(crate) fn new_swnz(
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Swnz,
            args: Args {
                rs1: rs1.into(),
                rs2: rs2.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Lh`].
    pub(crate) fn new_lh(rd: XRegister, rs1: XRegister, imm: i64, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Lh,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Lhnz`].
    pub(crate) fn new_lhnz(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Lhnz,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Sh`].
    pub(crate) fn new_sh(rs1: XRegister, rs2: XRegister, imm: i64, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Sh,
            args: Args {
                rs1: rs1.into(),
                rs2: rs2.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Shnz`].
    pub(crate) fn new_shnz(
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Shnz,
            args: Args {
                rs1: rs1.into(),
                rs2: rs2.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Lb`].
    pub(crate) fn new_lb(rd: XRegister, rs1: XRegister, imm: i64, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Lb,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Lbnz`].
    pub(crate) fn new_lbnz(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Lbnz,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Sb`].
    pub(crate) fn new_sb(rs1: XRegister, rs2: XRegister, imm: i64, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Sb,
            args: Args {
                rs1: rs1.into(),
                rs2: rs2.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Sbnz`].
    pub(crate) fn new_sbnz(
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Sbnz,
            args: Args {
                rs1: rs1.into(),
                rs2: rs2.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Blt`].
    pub(crate) fn new_blt(
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Blt,
            args: Args {
                rs1: rs1.into(),
                rs2: rs2.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Bltz`].
    pub(crate) fn new_bltz(rs1: NonZeroXRegister, imm: i64, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Bltz,
            args: Args {
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Bltez`].
    pub(crate) fn new_bltez(rs1: NonZeroXRegister, imm: i64, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Bltez,
            args: Args {
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Bge`].
    pub(crate) fn new_bge(
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Bge,
            args: Args {
                rs1: rs1.into(),
                rs2: rs2.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Bgez`].
    pub(crate) fn new_bgez(rs1: NonZeroXRegister, imm: i64, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Bgez,
            args: Args {
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Bgz`].
    pub(crate) fn new_bgz(rs1: NonZeroXRegister, imm: i64, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Bgz,
            args: Args {
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Bltu`].
    pub(crate) fn new_bltu(
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Bltu,
            args: Args {
                rs1: rs1.into(),
                rs2: rs2.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Bgeu`].
    pub(crate) fn new_bgeu(
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Bgeu,
            args: Args {
                rs1: rs1.into(),
                rs2: rs2.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::AddImmediateToPC`].
    pub(crate) fn new_add_immediate_to_pc(
        rd: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::AddImmediateToPC,
            args: Args {
                rd: rd.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Mul`].
    pub(crate) fn new_mul(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Mul,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }
}

impl Instruction {
    /// Convert [`InstrCacheable::Add`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::Add`]: crate::parser::instruction::InstrCacheable::Add
    pub(super) fn from_ic_add(args: &NonZeroRdRTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rs1), split_x0(args.rs2)) {
            (X::X0, X::X0) => Instruction::new_li(args.rd, 0, InstrWidth::Uncompressed),
            (X::NonZero(rs1), X::X0) | (X::X0, X::NonZero(rs1)) => {
                Instruction::new_mv(args.rd, rs1, InstrWidth::Uncompressed)
            }
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_add(args.rd, rs1, rs2, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Sub`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::Sub`]: crate::parser::instruction::InstrCacheable::Sub
    pub(super) fn from_ic_sub(args: &NonZeroRdRTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rs1), split_x0(args.rs2)) {
            (X::X0, X::X0) => Instruction::new_li(args.rd, 0, InstrWidth::Uncompressed),
            // `rd = rs1 - 0` is equivalent to moving rs1 to rd.
            (X::NonZero(rs2), X::X0) => Instruction::new_mv(args.rd, rs2, InstrWidth::Uncompressed),
            // `rd = 0 - rs2` is equivalent to negating rs2 and moving to rd.
            (X::X0, X::NonZero(rs2)) => {
                Instruction::new_neg(args.rd, rs2, InstrWidth::Uncompressed)
            }
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_sub(args.rd, rs1, rs2, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Csub`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::CSub`]: crate::parser::instruction::InstrCacheable::CSub
    pub(super) fn from_ic_csub(args: &CRTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rd_rs1), split_x0(args.rs2)) {
            // Storing anything in x0 or Subtracting 0 from anything is a NOP.
            (X::X0, _) | (_, X::X0) => Instruction::new_nop(InstrWidth::Compressed),
            (X::NonZero(rd_rs1), X::NonZero(rs2)) => {
                Instruction::new_sub(rd_rs1, rd_rs1, rs2, InstrWidth::Compressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Addi`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::Addi`]: crate::parser::instruction::InstrCacheable::Addi
    pub(super) fn from_ic_addi(args: &SplitITypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (args.rd, args.rs1) {
            (X::X0, _) => Instruction::new_nop(InstrWidth::Uncompressed),
            (X::NonZero(rd), X::X0) => Instruction::new_li(rd, args.imm, InstrWidth::Uncompressed),
            (X::NonZero(rd), X::NonZero(rs1)) => {
                Instruction::new_addi(rd, rs1, args.imm, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::CAddi4spn`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::CAddi4spn`]: crate::parser::instruction::InstrCacheable::CAddi4spn
    pub(super) fn from_ic_caddi4spn(args: &CIBTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rd_rs1) {
            X::X0 => Instruction::new_nop(InstrWidth::Compressed),
            X::NonZero(rd_rs1) => {
                Instruction::new_addi(rd_rs1, nz::sp, args.imm, InstrWidth::Compressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Andi`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Andi`]: crate::parser::instruction::InstrCacheable::Andi
    pub(super) fn from_ic_andi(args: &NonZeroRdITypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rs1) {
            // Bitwise AND with zero is zero: `x & 0 == 0`
            X::X0 => Instruction::new_li(args.rd, 0, InstrWidth::Uncompressed),
            X::NonZero(rs1) => {
                Instruction::new_andi(args.rd, rs1, args.imm, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::CAndi`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::CAndi`]: crate::parser::instruction::InstrCacheable::CAndi
    pub(super) fn from_ic_candi(args: &CIBTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rd_rs1) {
            X::X0 => Instruction::new_nop(InstrWidth::Compressed),
            X::NonZero(rd_rs1) => {
                Instruction::new_andi(rd_rs1, rd_rs1, args.imm, InstrWidth::Compressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Ori`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Ori`]: crate::parser::instruction::InstrCacheable::Ori
    pub(super) fn from_ic_ori(args: &NonZeroRdITypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rs1) {
            // Bitwise OR with zero is identity: `x | 0 == x`
            X::X0 => Instruction::new_li(args.rd, args.imm, InstrWidth::Uncompressed),
            X::NonZero(rs1) => {
                Instruction::new_ori(args.rd, rs1, args.imm, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Xori`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Xori`]: crate::parser::instruction::InstrCacheable::Xori
    pub(super) fn from_ic_xori(args: &NonZeroRdITypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rs1) {
            // Bitwise XOR with zero is identity: `x ^ 0 == x`
            X::X0 => Instruction::new_li(args.rd, args.imm, InstrWidth::Uncompressed),
            X::NonZero(rs1) => {
                Instruction::new_xori(args.rd, rs1, args.imm, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Slli`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Slli`]: crate::parser::instruction::InstrCacheable::Slli
    pub(super) fn from_ic_slli(args: &NonZeroRdITypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rs1) {
            // Shifting 0 by any amount is 0.
            X::X0 => Instruction::new_li(args.rd, 0, InstrWidth::Uncompressed),
            X::NonZero(rs1) => {
                Instruction::new_slli(args.rd, rs1, args.imm, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Srli`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::Srli`]: crate::parser::instruction::InstrCacheable::Srli
    pub(super) fn from_ic_srli(args: &NonZeroRdITypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rs1) {
            // shifting 0 by any amount is 0.
            X::X0 => Instruction::new_li(args.rd, 0, InstrWidth::Uncompressed),
            X::NonZero(rs1) => {
                Instruction::new_srli(args.rd, rs1, args.imm, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::CSrli`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::CSrli`]: crate::parser::instruction::InstrCacheable::CSrli
    pub(super) fn from_ic_csrli(args: &CIBTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rd_rs1) {
            X::X0 => Instruction::new_nop(InstrWidth::Compressed),
            X::NonZero(rd_rs1) => {
                Instruction::new_srli(rd_rs1, rd_rs1, args.imm, InstrWidth::Compressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Srai`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::Srai`]: crate::parser::instruction::InstrCacheable::Srai
    pub(super) fn from_ic_srai(args: &NonZeroRdITypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rs1) {
            // shifting 0 by any amount is 0.
            X::X0 => Instruction::new_li(args.rd, 0, InstrWidth::Uncompressed),
            X::NonZero(rs1) => {
                Instruction::new_srai(args.rd, rs1, args.imm, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::CSrai`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::CSrai`]: crate::parser::instruction::InstrCacheable::CSrai
    pub(super) fn from_ic_csrai(args: &CIBTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rd_rs1) {
            X::X0 => Instruction::new_nop(InstrWidth::Compressed),
            X::NonZero(rd_rs1) => {
                Instruction::new_srai(rd_rs1, rd_rs1, args.imm, InstrWidth::Compressed)
            }
        }
    }

    /// Convert [`InstrCacheable::And`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::And`]: crate::parser::instruction::InstrCacheable::And
    pub(super) fn from_ic_and(args: &NonZeroRdRTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rs1), split_x0(args.rs2)) {
            // Bitwise AND with zero is zero: `x & 0 == 0`
            (X::X0, _) | (_, X::X0) => Instruction::new_li(args.rd, 0, InstrWidth::Uncompressed),
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_and(args.rd, rs1, rs2, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::CAnd`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::CAnd`]: crate::parser::instruction::InstrCacheable::CAnd
    pub(super) fn from_ic_cand(args: &CRTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rd_rs1), split_x0(args.rs2)) {
            (X::X0, _) => Instruction::new_nop(InstrWidth::Compressed),
            // Bitwise AND with zero is zero: `x & 0 == 0`
            (X::NonZero(rd_rs1), X::X0) => Instruction::new_li(rd_rs1, 0, InstrWidth::Compressed),
            (X::NonZero(rd_rs1), X::NonZero(rs2)) => {
                Instruction::new_and(rd_rs1, rd_rs1, rs2, InstrWidth::Compressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Or`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Or`]: crate::parser::instruction::InstrCacheable::Or
    pub(super) fn from_ic_or(args: &NonZeroRdRTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rs1), split_x0(args.rs2)) {
            (X::X0, X::X0) => Instruction::new_li(args.rd, 0, InstrWidth::Uncompressed),
            (X::NonZero(rs1), X::X0) | (X::X0, X::NonZero(rs1)) => {
                Instruction::new_mv(args.rd, rs1, InstrWidth::Uncompressed)
            }
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_or(args.rd, rs1, rs2, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::COr`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::COr`]: crate::parser::instruction::InstrCacheable::COr
    pub(super) fn from_ic_cor(args: &CRTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rd_rs1), split_x0(args.rs2)) {
            // if rd is 0, then the instruction is a NOP.
            // if rs2 is 0, it is the same as moving rs1 to rd, which are the same register.
            (X::X0, _) | (_, X::X0) => Instruction::new_nop(InstrWidth::Compressed),
            (X::NonZero(rd_rs1), X::NonZero(rs2)) => {
                Instruction::new_or(rd_rs1, rd_rs1, rs2, InstrWidth::Compressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Xor`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Xor`]: crate::parser::instruction::InstrCacheable::Xor
    pub(super) fn from_ic_xor(args: &NonZeroRdRTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rs1), split_x0(args.rs2)) {
            (X::X0, X::X0) => Instruction::new_li(args.rd, 0, InstrWidth::Uncompressed),
            (X::NonZero(rs1), X::X0) | (X::X0, X::NonZero(rs1)) => {
                Instruction::new_mv(args.rd, rs1, InstrWidth::Uncompressed)
            }
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_xor(args.rd, rs1, rs2, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::CXor`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::CXor`]: crate::parser::instruction::InstrCacheable::CXor
    pub(super) fn from_ic_cxor(args: &CRTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rd_rs1), split_x0(args.rs2)) {
            // if rd is 0, then the instruction is a NOP.
            // if rs2 is 0, it is the same as moving rs1 to rd, which are the same register.
            (X::X0, _) | (_, X::X0) => Instruction::new_nop(InstrWidth::Compressed),
            (X::NonZero(rd_rs1), X::NonZero(rs2)) => {
                Instruction::new_xor(rd_rs1, rd_rs1, rs2, InstrWidth::Compressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Sll`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Sll`]: crate::parser::instruction::InstrCacheable::Sll
    pub(super) fn from_ic_sll(args: &NonZeroRdRTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rs1), split_x0(args.rs2)) {
            // Shifting 0 by any amount is 0.
            (X::X0, _) => Instruction::new_li(args.rd, 0, InstrWidth::Uncompressed),
            // Shifting by 0 and storing in rd is equivalent to moving the value to rd.
            (X::NonZero(rs1), X::X0) => Instruction::new_mv(args.rd, rs1, InstrWidth::Uncompressed),
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_sll(args.rd, rs1, rs2, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Srl`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Srl`]: crate::parser::instruction::InstrCacheable::Srl
    pub(super) fn from_ic_srl(args: &NonZeroRdRTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rs1), split_x0(args.rs2)) {
            // Shifting 0 by any amount is 0.
            (X::X0, _) => Instruction::new_li(args.rd, 0, InstrWidth::Uncompressed),
            // Shifting by 0 and storing in rd is equivalent to moving the value to rd.
            (X::NonZero(rs1), X::X0) => Instruction::new_mv(args.rd, rs1, InstrWidth::Uncompressed),
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_srl(args.rd, rs1, rs2, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Sra`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Sra`]: crate::parser::instruction::InstrCacheable::Sra
    pub(super) fn from_ic_sra(args: &NonZeroRdRTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rs1), split_x0(args.rs2)) {
            // Shifting 0 by any amount is 0.
            (X::X0, _) => Instruction::new_li(args.rd, 0, InstrWidth::Uncompressed),
            // Shifting by 0 and storing in rd is equivalent to moving the value to rd.
            (X::NonZero(rs1), X::X0) => Instruction::new_mv(args.rd, rs1, InstrWidth::Uncompressed),
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_sra(args.rd, rs1, rs2, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert ['InstrCacheable::Jal'] according to whether register is non-zero.
    ///
    /// ['InstrCacheable::Jal']: crate::parser::instruction::InstrCacheable::Jal
    pub(super) fn from_ic_jal(args: &UJTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rd) {
            // If rd is 0, we are just doing an unconditional jump and not storing the current pc.
            X::X0 => Instruction::new_j(args.imm, InstrWidth::Uncompressed),
            X::NonZero(rd) => Instruction::new_jal(rd, args.imm, InstrWidth::Uncompressed),
        }
    }

    /// Convert [`InstrCacheable::Beq`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::Beq`]: crate::parser::instruction::InstrCacheable::Beq
    pub(super) fn from_ic_beq(args: &SBTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rs1), split_x0(args.rs2)) {
            // If the registers are the same, then the instruction is an unconditional jump.
            (X::X0, X::X0) => Instruction::new_j(args.imm, InstrWidth::Uncompressed),
            (X::NonZero(rs1), X::NonZero(rs2)) if rs1 == rs2 => {
                Instruction::new_j(args.imm, InstrWidth::Uncompressed)
            }
            // If either register is x0, then the condition to branch is whether the other register stores 0.
            (X::NonZero(rs1), X::X0) | (X::X0, X::NonZero(rs1)) => {
                Instruction::new_beqz(rs1, args.imm, InstrWidth::Uncompressed)
            }
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_beq(rs1, rs2, args.imm, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::CBeqz`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::CBeqz`]: crate::parser::instruction::InstrCacheable::CBeqz
    pub(super) fn from_ic_cbeqz(args: &CIBTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rd_rs1) {
            // If `rd_rs1` is zero, the result is an unconditional jump
            X::X0 => Instruction::new_j(args.imm, InstrWidth::Compressed),
            X::NonZero(rd_rs1) => Instruction::new_beqz(rd_rs1, args.imm, InstrWidth::Compressed),
        }
    }

    /// Convert [`InstrCacheable::Bne`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::Bne`]: crate::parser::instruction::InstrCacheable::Bne
    pub(super) fn from_ic_bne(args: &SBTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rs1), split_x0(args.rs2)) {
            // If the registers are the same, they are equal so we don't branch.
            (X::X0, X::X0) => Instruction::new_nop(InstrWidth::Uncompressed),
            (X::NonZero(rs1), X::NonZero(rs2)) if rs1 == rs2 => {
                Instruction::new_nop(InstrWidth::Uncompressed)
            }
            // If either register is x0, then the condition to branch is whether the other register doesn't store 0.
            (X::NonZero(rs1), X::X0) | (X::X0, X::NonZero(rs1)) => {
                Instruction::new_bnez(rs1, args.imm, InstrWidth::Uncompressed)
            }
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_bne(rs1, rs2, args.imm, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::CBnez`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::CBnez`]: crate::parser::instruction::InstrCacheable::CBnez
    pub(super) fn from_ic_cbnez(args: &CIBTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rd_rs1) {
            // If `rd_rs1 == x0`, this will never branch.
            X::X0 => Instruction::new_nop(InstrWidth::Compressed),
            X::NonZero(rd_rs1) => Instruction::new_bnez(rd_rs1, args.imm, InstrWidth::Compressed),
        }
    }

    /// Convert [`InstrCacheable::Jalr`] according to whether registers and imm are non-zero.
    ///
    /// [`InstrCacheable::Jalr`]: crate::parser::instruction::InstrCacheable::Jalr
    pub(super) fn from_ic_jalr(args: &ITypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rd), split_x0(args.rs1), args.imm) {
            // if rd and rs1 are x0, the only effect is an unconditional jump to the absolute address `imm`.
            (X::X0, X::X0, imm) => Instruction::new_j_absolute(imm, InstrWidth::Uncompressed),
            // if rd is x0 and imm is 0, the only effect is an unconditional jump to the address stored in rs1.
            (X::X0, X::NonZero(rs1), 0) => Instruction::new_jr(rs1, InstrWidth::Uncompressed),
            // if rd is x0 and imm is non-zero, the only effect is an unconditional jump to the address stored in rs1 + imm.
            (X::X0, X::NonZero(rs1), imm) => {
                Instruction::new_jr_imm(rs1, imm, InstrWidth::Uncompressed)
            }
            // if rd is non-x0 and imm is 0, the effect is a jump to the absolute address `imm`
            // and storing `pc + 4` in the register `rd`.
            (X::NonZero(rd), X::X0, imm) => {
                Instruction::new_jalr_absolute(rd, imm, InstrWidth::Uncompressed)
            }
            // if rd and rs1 are non-x0 and imm is 0, the effect is a jump to the address stored in rs1
            // and storing `pc + 4` in the register `rd`.
            (X::NonZero(rd), X::NonZero(rs1), 0) => {
                Instruction::new_jalr(rd, rs1, InstrWidth::Uncompressed)
            }
            // if all non-zero, the effect is a jump to the address stored in rs1 + imm
            // and storing `pc + 4` in the register `rd`.
            (X::NonZero(rd), X::NonZero(rs1), imm) => {
                Instruction::new_jalr_imm(rd, rs1, imm, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Ld`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Ld`]: crate::parser::instruction::InstrCacheable::Ld
    pub(super) fn from_ic_ld(args: &ITypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rd), split_x0(args.rs1)) {
            (X::NonZero(rd), X::NonZero(rs1)) => {
                Instruction::new_ldnz(rd, rs1, args.imm, InstrWidth::Uncompressed)
            }
            _ => Instruction::new_ld(args.rd, args.rs1, args.imm, InstrWidth::Uncompressed),
        }
    }

    /// Convert [`InstrCacheable::Sd`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::Sd`]: crate::parser::instruction::InstrCacheable::Sd
    pub(super) fn from_ic_sd(args: &SBTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rs1), split_x0(args.rs2)) {
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_sdnz(rs1, rs2, args.imm, InstrWidth::Uncompressed)
            }
            _ => Instruction::new_sd(args.rs1, args.rs2, args.imm, InstrWidth::Uncompressed),
        }
    }

    /// Convert [`InstrCacheable::CSdsp`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::CSdsp`]: crate::parser::instruction::InstrCacheable::CSdsp
    pub(super) fn from_ic_csdsp(args: &CSSTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        debug_assert!(args.imm >= 0 && args.imm % 8 == 0);
        match split_x0(args.rs2) {
            X::NonZero(rs2) => Instruction::new_sdnz(nz::sp, rs2, args.imm, InstrWidth::Compressed),
            X::X0 => Instruction::new_sd(sp, x0, args.imm, InstrWidth::Compressed),
        }
    }

    /// Convert [`InstrCacheable::Lw`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Lw`]: crate::parser::instruction::InstrCacheable::Lw
    pub(super) fn from_ic_lw(args: &ITypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rd), split_x0(args.rs1)) {
            (X::NonZero(rd), X::NonZero(rs1)) => {
                Instruction::new_lwnz(rd, rs1, args.imm, InstrWidth::Uncompressed)
            }
            _ => Instruction::new_lw(args.rd, args.rs1, args.imm, InstrWidth::Uncompressed),
        }
    }

    /// Convert [`InstrCacheable::Sw`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::Sw`]: crate::parser::instruction::InstrCacheable::Sw
    pub(super) fn from_ic_sw(args: &SBTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rs1), split_x0(args.rs2)) {
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_swnz(rs1, rs2, args.imm, InstrWidth::Uncompressed)
            }
            _ => Instruction::new_sw(args.rs1, args.rs2, args.imm, InstrWidth::Uncompressed),
        }
    }

    /// Convert [`InstrCacheable::CSwsp`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::CSwsp`]: crate::parser::instruction::InstrCacheable::CSwsp
    pub(super) fn from_ic_cswsp(args: &CSSTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        debug_assert!(args.imm >= 0 && args.imm % 4 == 0);
        match split_x0(args.rs2) {
            X::NonZero(rs2) => Instruction::new_swnz(nz::sp, rs2, args.imm, InstrWidth::Compressed),
            X::X0 => Instruction::new_sw(sp, x0, args.imm, InstrWidth::Compressed),
        }
    }

    /// Convert [`InstrCacheable::Lh`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Lh`]: crate::parser::instruction::InstrCacheable::Lh
    pub(super) fn from_ic_lh(args: &ITypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rd), split_x0(args.rs1)) {
            (X::NonZero(rd), X::NonZero(rs1)) => {
                Instruction::new_lhnz(rd, rs1, args.imm, InstrWidth::Uncompressed)
            }
            _ => Instruction::new_lh(args.rd, args.rs1, args.imm, InstrWidth::Uncompressed),
        }
    }

    /// Convert [`InstrCacheable::Sh`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::Sh`]: crate::parser::instruction::InstrCacheable::Sh
    pub(super) fn from_ic_sh(args: &SBTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rs1), split_x0(args.rs2)) {
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_shnz(rs1, rs2, args.imm, InstrWidth::Uncompressed)
            }
            _ => Instruction::new_sh(args.rs1, args.rs2, args.imm, InstrWidth::Uncompressed),
        }
    }

    /// Convert [`InstrCacheable::Lb`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Lb`]: crate::parser::instruction::InstrCacheable::Lb
    pub(super) fn from_ic_lb(args: &ITypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rd), split_x0(args.rs1)) {
            (X::NonZero(rd), X::NonZero(rs1)) => {
                Instruction::new_lbnz(rd, rs1, args.imm, InstrWidth::Uncompressed)
            }
            _ => Instruction::new_lb(args.rd, args.rs1, args.imm, InstrWidth::Uncompressed),
        }
    }

    /// Convert [`InstrCacheable::Sb`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::Sb`]: crate::parser::instruction::InstrCacheable::Sb
    pub(super) fn from_ic_sb(args: &SBTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rs1), split_x0(args.rs2)) {
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_sbnz(rs1, rs2, args.imm, InstrWidth::Uncompressed)
            }
            _ => Instruction::new_sb(args.rs1, args.rs2, args.imm, InstrWidth::Uncompressed),
        }
    }

    /// Convert [`InstrCacheable::Blt`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Blt`]: crate::parser::instruction::InstrCacheable::Blt
    pub(super) fn from_ic_blt(args: &SBTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rs1), split_x0(args.rs2)) {
            // If the registers are the same, the values are the same so we don't branch.
            (X::X0, X::X0) => Instruction::new_nop(InstrWidth::Uncompressed),
            (X::NonZero(rs1), X::NonZero(rs2)) if rs1 == rs2 => {
                Instruction::new_nop(InstrWidth::Uncompressed)
            }
            // If rs1 is x0, the condition to branch is whether `val(rs2) > 0`.
            (X::X0, X::NonZero(rs1)) => {
                Instruction::new_bgz(rs1, args.imm, InstrWidth::Uncompressed)
            }
            // If rs2 is x0, the condition to branch is whether `val(rs1) < 0`.
            (X::NonZero(rs1), X::X0) => {
                Instruction::new_bltz(rs1, args.imm, InstrWidth::Uncompressed)
            }
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_blt(rs1, rs2, args.imm, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Bge`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Bge`]: crate::parser::instruction::InstrCacheable::Bge
    pub(super) fn from_ic_bge(args: &SBTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rs1), split_x0(args.rs2)) {
            // If the registers are the same, the values are the same so we always branch.
            (X::X0, X::X0) => Instruction::new_j(args.imm, InstrWidth::Uncompressed),
            (X::NonZero(rs1), X::NonZero(rs2)) if rs1 == rs2 => {
                Instruction::new_j(args.imm, InstrWidth::Uncompressed)
            }
            // If rs1 is x0, the condition to branch is whether `val(rs2) <= 0`.
            (X::X0, X::NonZero(rs1)) => {
                Instruction::new_bltez(rs1, args.imm, InstrWidth::Uncompressed)
            }
            // If rs2 is x0, the condition to branch is whether `val(rs1) >= 0`.
            (X::NonZero(rs1), X::X0) => {
                Instruction::new_bgez(rs1, args.imm, InstrWidth::Uncompressed)
            }
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_bge(rs1, rs2, args.imm, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Bltu`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Bltu`]: crate::parser::instruction::InstrCacheable::Bltu
    pub(super) fn from_ic_bltu(args: &SBTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rs1), split_x0(args.rs2)) {
            // If rs2 is x0, rs1 is either the same or greater than rs2, so we don't branch.
            (_, X::X0) => Instruction::new_nop(InstrWidth::Uncompressed),
            // If the registers are the same, the values are the same so we don't branch.
            (X::NonZero(rs1), X::NonZero(rs2)) if rs1 == rs2 => {
                Instruction::new_nop(InstrWidth::Uncompressed)
            }
            // If rs1 is x0, the condition to branch is whether `val(rs2) != 0`.
            (X::X0, X::NonZero(rs1)) => {
                Instruction::new_bnez(rs1, args.imm, InstrWidth::Uncompressed)
            }
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_bltu(rs1, rs2, args.imm, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Bgeu`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Bgeu`]: crate::parser::instruction::InstrCacheable::Bgeu
    pub(super) fn from_ic_bgeu(args: &SBTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rs1), split_x0(args.rs2)) {
            // If rs2 is x0, rs1 is either the same or more than rs2, so we always branch.
            (_, X::X0) => Instruction::new_j(args.imm, InstrWidth::Uncompressed),
            // If the registers are the same, the values are the same so we always branch.
            (X::NonZero(rs1), X::NonZero(rs2)) if rs1 == rs2 => {
                Instruction::new_j(args.imm, InstrWidth::Uncompressed)
            }
            // If rs1 is x0, the condition to branch is whether `val(rs2) == 0`.
            (X::X0, X::NonZero(rs1)) => {
                Instruction::new_beqz(rs1, args.imm, InstrWidth::Uncompressed)
            }
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_bgeu(rs1, rs2, args.imm, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Mul`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::Mul`]: crate::parser::instruction::InstrCacheable::Mul
    pub(super) fn from_ic_mul(args: &RTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rd), split_x0(args.rs1), split_x0(args.rs2)) {
            (X::X0, _, _) => Instruction::new_nop(InstrWidth::Uncompressed),
            (X::NonZero(rd), X::X0, _) | (X::NonZero(rd), _, X::X0) => {
                Instruction::new_li(rd, 0, InstrWidth::Uncompressed)
            }
            (X::NonZero(rd), X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_mul(rd, rs1, rs2, InstrWidth::Uncompressed)
            }
        }
    }
}
