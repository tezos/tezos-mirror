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
use crate::parser::XRegisterParsed;
use crate::parser::instruction::CIBTypeArgs;
use crate::parser::instruction::CRTypeArgs;
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

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::AddWord`].
    pub(crate) fn new_add_word(
        rd: NonZeroXRegister,
        rs1: XRegister,
        rs2: XRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::AddWord,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::AddWordImmediate`].
    pub(crate) fn new_add_word_immediate(
        rd: NonZeroXRegister,
        rs1: XRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::AddWordImmediate,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
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

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::SubWord`].
    pub(crate) fn new_sub_word(
        rd: NonZeroXRegister,
        rs1: XRegister,
        rs2: XRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::SubWord,
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

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for
    /// [`OpCode::Unknown`].
    pub(crate) fn new_unknown(width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Unknown,
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

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X64OrImm`].
    pub(crate) fn new_x64_or_immediate(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X64OrImm,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X64XorImm`].
    pub(crate) fn new_x64_xor_immediate(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X64XorImm,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::ShiftLeftImmediate`].
    pub(crate) fn new_shift_left_immediate(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::ShiftLeftImmediate,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::ShiftRightImmediateUnsigned`].
    pub(crate) fn new_shift_right_immediate_unsigned(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::ShiftRightImmediateUnsigned,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::ShiftRightImmediateSigned`].
    pub(crate) fn new_shift_right_immediate_signed(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::ShiftRightImmediateSigned,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X32ShiftLeft`].
    pub(crate) fn new_x32_shift_left(
        rd: NonZeroXRegister,
        rs1: XRegister,
        rs2: XRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X32ShiftLeft,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X32ShiftRightUnsigned`].
    pub(crate) fn new_x32_shift_right_unsigned(
        rd: NonZeroXRegister,
        rs1: XRegister,
        rs2: XRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X32ShiftRightUnsigned,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X32ShiftRightSigned`].
    pub(crate) fn new_x32_shift_right_signed(
        rd: NonZeroXRegister,
        rs1: XRegister,
        rs2: XRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X32ShiftRightSigned,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X32ShiftLeftImm`].
    pub(crate) fn new_x32_shift_left_immediate(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X32ShiftLeftImm,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X32ShiftRightImmUnsigned`].
    pub(crate) fn new_x32_shift_right_immediate_unsigned(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X32ShiftRightImmUnsigned,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X32ShiftRightImmSigned`].
    pub(crate) fn new_x32_shift_right_immediate_signed(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X32ShiftRightImmSigned,
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

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X64Xor`].
    pub(crate) fn new_x64_xor(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X64Xor,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::ShiftLeft`].
    pub(crate) fn new_shift_left(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::ShiftLeft,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::ShiftRightUnsigned`].
    pub(crate) fn new_shift_right_unsigned(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::ShiftRightUnsigned,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::ShiftRightSigned`].
    pub(crate) fn new_shift_right_signed(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::ShiftRightSigned,
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

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for
    /// [`OpCode::BranchEqual`].
    pub(crate) fn new_branch_equal(
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::BranchEqual,
            args: Args {
                rs1: rs1.into(),
                rs2: rs2.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for
    /// [`OpCode::BranchEqualZero`].
    pub(crate) fn new_branch_equal_zero(
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::BranchEqualZero,
            args: Args {
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for
    /// [`OpCode::BranchNotEqual`].
    pub(crate) fn new_branch_not_equal(
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::BranchNotEqual,
            args: Args {
                rs1: rs1.into(),
                rs2: rs2.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for
    /// [`OpCode::BranchNotEqualZero`].
    pub(crate) fn new_branch_not_equal_zero(
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::BranchNotEqualZero,
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

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X64LoadSigned`].
    pub(crate) fn new_x64_load_signed(
        rd: XRegister,
        rs1: XRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X64LoadSigned,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X64Store`].
    pub(crate) fn new_x64_store(
        rs1: XRegister,
        rs2: XRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X64Store,
            args: Args {
                rs1: rs1.into(),
                rs2: rs2.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X32LoadSigned`].
    pub(crate) fn new_x32_load_signed(
        rd: XRegister,
        rs1: XRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X32LoadSigned,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X32LoadUnsigned`].
    pub(crate) fn new_x32_load_unsigned(
        rd: XRegister,
        rs1: XRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X32LoadUnsigned,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X32Store`].
    pub(crate) fn new_x32_store(
        rs1: XRegister,
        rs2: XRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X32Store,
            args: Args {
                rs1: rs1.into(),
                rs2: rs2.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X16LoadSigned`].
    pub(crate) fn new_x16_load_signed(
        rd: XRegister,
        rs1: XRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X16LoadSigned,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X16LoadUnsigned`].
    pub(crate) fn new_x16_load_unsigned(
        rd: XRegister,
        rs1: XRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X16LoadUnsigned,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X16Store`].
    pub(crate) fn new_x16_store(
        rs1: XRegister,
        rs2: XRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X16Store,
            args: Args {
                rs1: rs1.into(),
                rs2: rs2.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X8LoadSigned`].
    pub(crate) fn new_x8_load_signed(
        rd: XRegister,
        rs1: XRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X8LoadSigned,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X8LoadUnsigned`].
    pub(crate) fn new_x8_load_unsigned(
        rd: XRegister,
        rs1: XRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X8LoadUnsigned,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X8Store`].
    pub(crate) fn new_x8_store(
        rs1: XRegister,
        rs2: XRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X8Store,
            args: Args {
                rs1: rs1.into(),
                rs2: rs2.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for
    /// [`OpCode::BranchLessThanSigned`].
    pub(crate) fn new_branch_less_than_signed(
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::BranchLessThanSigned,
            args: Args {
                rs1: rs1.into(),
                rs2: rs2.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for
    /// [`OpCode::BranchLessThanZero`].
    pub(crate) fn new_branch_less_than_zero(
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::BranchLessThanZero,
            args: Args {
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for
    /// [`OpCode::BranchLessThanOrEqualZero`].
    pub(crate) fn new_branch_less_than_or_equal_zero(
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::BranchLessThanOrEqualZero,
            args: Args {
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for
    /// [`OpCode::BranchGreaterThanOrEqualSigned`].
    pub(crate) fn new_branch_greater_than_or_equal_signed(
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::BranchGreaterThanOrEqualSigned,
            args: Args {
                rs1: rs1.into(),
                rs2: rs2.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for
    /// [`OpCode::BranchGreaterThanOrEqualZero`].
    pub(crate) fn new_branch_greater_than_or_equal_zero(
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::BranchGreaterThanOrEqualZero,
            args: Args {
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for
    /// [`OpCode::BranchGreaterThanZero`].
    pub(crate) fn new_branch_greater_than_zero(
        rs1: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::BranchGreaterThanZero,
            args: Args {
                rs1: rs1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for
    /// [`OpCode::BranchLessThanUnsigned`].
    pub(crate) fn new_branch_less_than_unsigned(
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::BranchLessThanUnsigned,
            args: Args {
                rs1: rs1.into(),
                rs2: rs2.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for
    /// [`OpCode::BranchGreaterThanOrEqualUnsigned`].
    pub(crate) fn new_branch_greater_than_or_equal_unsigned(
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        imm: i64,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::BranchGreaterThanOrEqualUnsigned,
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

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X32Mul`].
    pub(crate) fn new_x32_mul(
        rd: XRegister,
        rs1: XRegister,
        rs2: XRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X32Mul,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X64MulHighSigned`].
    pub(crate) fn new_x64_mul_high_signed(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X64MulHighSigned,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X64MulHighSignedUnsigned`].
    pub(crate) fn new_x64_mul_high_signed_unsigned(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X64MulHighSignedUnsigned,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X64MulHighUnsigned`].
    pub(crate) fn new_x64_mul_high_unsigned(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X64MulHighUnsigned,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X64DivSigned`].
    pub(crate) fn new_x64_div_signed(
        rd: NonZeroXRegister,
        rs1: XRegister,
        rs2: XRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X64DivSigned,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::ECall`].
    pub(crate) fn new_ecall() -> Self {
        Self {
            opcode: OpCode::ECall,
            args: Args {
                width: InstrWidth::Uncompressed,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::Fadds`].
    /// The fadds instruction has not been lowered yet, and this function is only used for testing.
    #[cfg(test)]
    pub(crate) fn new_fadds(
        rd: NonZeroXRegister,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::Fadds,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    /// Create a new [`Instruction`] with the appropriate [`super::ArgsShape`] for [`OpCode::X64AtomicAdd`].
    pub(crate) fn new_x64_atomic_add(
        rd: XRegister,
        rs1: XRegister,
        rs2: XRegister,
        aq: bool,
        rl: bool,
        width: InstrWidth,
    ) -> Self {
        Self {
            opcode: OpCode::X64AtomicAdd,
            args: Args {
                rd: rd.into(),
                rs1: rs1.into(),
                rs2: rs2.into(),
                aq,
                rl,
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

    /// Convert [`InstrCacheable::CAddw`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::CAddw`]: crate::parser::instruction::InstrCacheable::CAddw    
    pub(super) fn from_ic_caddw(args: &CRTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rd_rs1) {
            X::X0 => Instruction::new_nop(InstrWidth::Compressed),
            X::NonZero(rd_rs1) => {
                Instruction::new_add_word(rd_rs1, args.rd_rs1, args.rs2, InstrWidth::Compressed)
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

    /// Convert [`InstrCacheable::CSubw`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::CSubw`]: crate::parser::instruction::InstrCacheable::CSubw
    pub(super) fn from_ic_csubw(args: &CRTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rd_rs1) {
            X::X0 => Instruction::new_nop(InstrWidth::Compressed),
            X::NonZero(rd_rs1) => {
                Instruction::new_sub_word(rd_rs1, args.rd_rs1, args.rs2, InstrWidth::Compressed)
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
                Instruction::new_x64_or_immediate(args.rd, rs1, args.imm, InstrWidth::Uncompressed)
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
                Instruction::new_x64_xor_immediate(args.rd, rs1, args.imm, InstrWidth::Uncompressed)
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
            X::NonZero(rs1) => Instruction::new_shift_left_immediate(
                args.rd,
                rs1,
                args.imm,
                InstrWidth::Uncompressed,
            ),
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
            X::NonZero(rs1) => Instruction::new_shift_right_immediate_unsigned(
                args.rd,
                rs1,
                args.imm,
                InstrWidth::Uncompressed,
            ),
        }
    }

    /// Convert [`InstrCacheable::CSrli`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::CSrli`]: crate::parser::instruction::InstrCacheable::CSrli
    pub(super) fn from_ic_csrli(args: &CIBTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rd_rs1) {
            X::X0 => Instruction::new_nop(InstrWidth::Compressed),
            X::NonZero(rd_rs1) => Instruction::new_shift_right_immediate_unsigned(
                rd_rs1,
                rd_rs1,
                args.imm,
                InstrWidth::Compressed,
            ),
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
            X::NonZero(rs1) => Instruction::new_shift_right_immediate_signed(
                args.rd,
                rs1,
                args.imm,
                InstrWidth::Uncompressed,
            ),
        }
    }

    /// Convert [`InstrCacheable::CSrai`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::CSrai`]: crate::parser::instruction::InstrCacheable::CSrai
    pub(super) fn from_ic_csrai(args: &CIBTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rd_rs1) {
            X::X0 => Instruction::new_nop(InstrWidth::Compressed),
            X::NonZero(rd_rs1) => Instruction::new_shift_right_immediate_signed(
                rd_rs1,
                rd_rs1,
                args.imm,
                InstrWidth::Compressed,
            ),
        }
    }

    /// Convert [`InstrCacheable::Slliw`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Slliw`]: crate::parser::instruction::InstrCacheable::Slliw
    pub(super) fn from_ic_x32_shift_left_immediate(args: &NonZeroRdITypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rs1) {
            // Shifting 0 by any amount is 0.
            X::X0 => Instruction::new_li(args.rd, 0, InstrWidth::Uncompressed),
            // For non-zero rs1, perform the actual shift operation
            X::NonZero(rs1) => Instruction::new_x32_shift_left_immediate(
                args.rd,
                rs1,
                args.imm,
                InstrWidth::Uncompressed,
            ),
        }
    }

    /// Convert [`InstrCacheable::Srliw`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Srliw`]: crate::parser::instruction::InstrCacheable::Srliw
    pub(super) fn from_ic_x32_shift_right_immediate_unsigned(
        args: &NonZeroRdITypeArgs,
    ) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rs1) {
            // Shifting 0 by any amount is 0.
            X::X0 => Instruction::new_li(args.rd, 0, InstrWidth::Uncompressed),
            // For non-zero rs1, perform the actual shift operation
            X::NonZero(rs1) => Instruction::new_x32_shift_right_immediate_unsigned(
                args.rd,
                rs1,
                args.imm,
                InstrWidth::Uncompressed,
            ),
        }
    }

    /// Convert [`InstrCacheable::Sraiw`] according to whether register is non-zero.
    ///
    /// [`InstrCacheable::Sraiw`]: crate::parser::instruction::InstrCacheable::Sraiw
    pub(super) fn from_ic_x32_shift_right_immediate_signed(
        args: &NonZeroRdITypeArgs,
    ) -> Instruction {
        use XRegisterParsed as X;
        match split_x0(args.rs1) {
            // Shifting 0 by any amount is 0.
            X::X0 => Instruction::new_li(args.rd, 0, InstrWidth::Uncompressed),
            // For non-zero rs1, perform the actual shift operation
            X::NonZero(rs1) => Instruction::new_x32_shift_right_immediate_signed(
                args.rd,
                rs1,
                args.imm,
                InstrWidth::Uncompressed,
            ),
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
                Instruction::new_x64_xor(args.rd, rs1, rs2, InstrWidth::Uncompressed)
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
                Instruction::new_x64_xor(rd_rs1, rd_rs1, rs2, InstrWidth::Compressed)
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
                Instruction::new_shift_left(args.rd, rs1, rs2, InstrWidth::Uncompressed)
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
                Instruction::new_shift_right_unsigned(args.rd, rs1, rs2, InstrWidth::Uncompressed)
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
                Instruction::new_shift_right_signed(args.rd, rs1, rs2, InstrWidth::Uncompressed)
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
                Instruction::new_branch_equal_zero(rs1, args.imm, InstrWidth::Uncompressed)
            }
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_branch_equal(rs1, rs2, args.imm, InstrWidth::Uncompressed)
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
            X::NonZero(rd_rs1) => {
                Instruction::new_branch_equal_zero(rd_rs1, args.imm, InstrWidth::Compressed)
            }
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
                Instruction::new_branch_not_equal_zero(rs1, args.imm, InstrWidth::Uncompressed)
            }
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_branch_not_equal(rs1, rs2, args.imm, InstrWidth::Uncompressed)
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
            X::NonZero(rd_rs1) => {
                Instruction::new_branch_not_equal_zero(rd_rs1, args.imm, InstrWidth::Compressed)
            }
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
                Instruction::new_branch_greater_than_zero(rs1, args.imm, InstrWidth::Uncompressed)
            }
            // If rs2 is x0, the condition to branch is whether `val(rs1) < 0`.
            (X::NonZero(rs1), X::X0) => {
                Instruction::new_branch_less_than_zero(rs1, args.imm, InstrWidth::Uncompressed)
            }
            (X::NonZero(rs1), X::NonZero(rs2)) => Instruction::new_branch_less_than_signed(
                rs1,
                rs2,
                args.imm,
                InstrWidth::Uncompressed,
            ),
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
            (X::X0, X::NonZero(rs1)) => Instruction::new_branch_less_than_or_equal_zero(
                rs1,
                args.imm,
                InstrWidth::Uncompressed,
            ),
            // If rs2 is x0, the condition to branch is whether `val(rs1) >= 0`.
            (X::NonZero(rs1), X::X0) => Instruction::new_branch_greater_than_or_equal_zero(
                rs1,
                args.imm,
                InstrWidth::Uncompressed,
            ),
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_branch_greater_than_or_equal_signed(
                    rs1,
                    rs2,
                    args.imm,
                    InstrWidth::Uncompressed,
                )
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
                Instruction::new_branch_not_equal_zero(rs1, args.imm, InstrWidth::Uncompressed)
            }
            (X::NonZero(rs1), X::NonZero(rs2)) => Instruction::new_branch_less_than_unsigned(
                rs1,
                rs2,
                args.imm,
                InstrWidth::Uncompressed,
            ),
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
                Instruction::new_branch_equal_zero(rs1, args.imm, InstrWidth::Uncompressed)
            }
            (X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_branch_greater_than_or_equal_unsigned(
                    rs1,
                    rs2,
                    args.imm,
                    InstrWidth::Uncompressed,
                )
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

    /// Converts [`InstrCacheable::Mulh`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::Mulh`]: crate::parser::instruction::InstrCacheable::Mulh
    pub(crate) fn from_ic_mulh(args: &RTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rd), split_x0(args.rs1), split_x0(args.rs2)) {
            (X::X0, _, _) => Instruction::new_nop(InstrWidth::Uncompressed),
            (X::NonZero(rd), X::X0, _) | (X::NonZero(rd), _, X::X0) => {
                Instruction::new_li(rd, 0, InstrWidth::Uncompressed)
            }
            (X::NonZero(rd), X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_x64_mul_high_signed(rd, rs1, rs2, InstrWidth::Uncompressed)
            }
        }
    }

    /// Converts [`InstrCacheable::Mulhsu`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::Mulhsu`]: crate::parser::instruction::InstrCacheable::Mulhsu
    pub(crate) fn from_ic_mulhsu(args: &RTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rd), split_x0(args.rs1), split_x0(args.rs2)) {
            (X::X0, _, _) => Instruction::new_nop(InstrWidth::Uncompressed),
            (X::NonZero(rd), X::X0, _) | (X::NonZero(rd), _, X::X0) => {
                Instruction::new_li(rd, 0, InstrWidth::Uncompressed)
            }
            (X::NonZero(rd), X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_x64_mul_high_signed_unsigned(
                    rd,
                    rs1,
                    rs2,
                    InstrWidth::Uncompressed,
                )
            }
        }
    }

    /// Converts [`InstrCacheable::Mulhu`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::Mulhu`]: crate::parser::instruction::InstrCacheable::Mulhu
    pub(crate) fn from_ic_mulhu(args: &RTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rd), split_x0(args.rs1), split_x0(args.rs2)) {
            (X::X0, _, _) => Instruction::new_nop(InstrWidth::Uncompressed),
            (X::NonZero(rd), X::X0, _) | (X::NonZero(rd), _, X::X0) => {
                Instruction::new_li(rd, 0, InstrWidth::Uncompressed)
            }
            (X::NonZero(rd), X::NonZero(rs1), X::NonZero(rs2)) => {
                Instruction::new_x64_mul_high_unsigned(rd, rs1, rs2, InstrWidth::Uncompressed)
            }
        }
    }

    /// Convert [`InstrCacheable::Div`] according to whether registers are non-zero.
    ///
    /// [`InstrCacheable::Div`]: crate::parser::instruction::InstrCacheable::Div
    pub(super) fn from_ic_div(args: &RTypeArgs) -> Instruction {
        use XRegisterParsed as X;
        match (split_x0(args.rd), split_x0(args.rs1), split_x0(args.rs2)) {
            // This holds as `div` is non-trapping in the case of division by zero.
            (X::X0, _, _) => Instruction::new_nop(InstrWidth::Uncompressed),
            (X::NonZero(rd), _, _) => {
                Instruction::new_x64_div_signed(rd, args.rs1, args.rs2, InstrWidth::Uncompressed)
            }
        }
    }
}
