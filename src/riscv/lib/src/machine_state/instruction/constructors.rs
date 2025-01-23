// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{Args, Instruction, OpCode};
use crate::{
    default::ConstDefault, machine_state::registers::NonZeroXRegister,
    parser::instruction::InstrWidth,
};

impl Instruction {
    pub(super) fn new_mv(rd: NonZeroXRegister, rs2: NonZeroXRegister, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Mv,
            args: Args {
                rd: rd.into(),
                // We are adding a default value for rs1 as NonZeroXRegister::x1
                // to be explicit that it is of NonZeroXRegister type.
                rs1: NonZeroXRegister::x1.into(),
                rs2: rs2.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }

    pub(super) fn new_li(rd: NonZeroXRegister, imm: i64, width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Li,
            args: Args {
                rd: rd.into(),
                // We are adding a default values for rs1 and rs2 as NonZeroXRegister::x1
                // to be explicit that it is of NonZeroXRegister type.
                rs1: NonZeroXRegister::x1.into(),
                rs2: NonZeroXRegister::x1.into(),
                imm,
                width,
                ..Args::DEFAULT
            },
        }
    }

    pub(super) fn new_nop(width: InstrWidth) -> Self {
        Self {
            opcode: OpCode::Nop,
            args: Args {
                // We are adding a default values for rd, rs1 and rs2 as NonZeroXRegister::x1
                // to be explicit that they are of NonZeroXRegister type.
                rd: NonZeroXRegister::x1.into(),
                rs1: NonZeroXRegister::x1.into(),
                rs2: NonZeroXRegister::x1.into(),
                width,
                ..Args::DEFAULT
            },
        }
    }
}
