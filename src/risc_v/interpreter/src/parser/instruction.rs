// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::machine_state::registers::XRegister;

#[derive(Debug, PartialEq)]
pub struct RTypeArgs {
    pub rd: XRegister,
    pub rs1: XRegister,
    pub rs2: XRegister,
}

#[derive(Debug, PartialEq)]
pub struct ITypeArgs {
    pub rd: XRegister,
    pub rs1: XRegister,
    pub imm: i64,
}

#[derive(Debug, PartialEq)]
pub struct SBTypeArgs {
    pub rs1: XRegister,
    pub rs2: XRegister,
    pub imm: i64,
}

#[derive(Debug, PartialEq)]
pub struct UJTypeArgs {
    pub rd: XRegister,
    pub imm: i64,
}

/// RISC-V parsed instructions. Along with legal instructions, potentially
/// illegal instructions are parsed as `Unknown` or `UnknownCompressed`.
/// These instructions are successfully parsed, but must not be interpreted.
#[derive(Debug, PartialEq)]
pub enum Instr {
    // RV64I R-type instructions
    Add(RTypeArgs),
    Sub(RTypeArgs),
    Xor(RTypeArgs),
    Or(RTypeArgs),
    And(RTypeArgs),
    Sll(RTypeArgs),
    Srl(RTypeArgs),
    Sra(RTypeArgs),
    Slt(RTypeArgs),
    Sltu(RTypeArgs),
    Addw(RTypeArgs),
    Subw(RTypeArgs),
    Sllw(RTypeArgs),
    Srlw(RTypeArgs),
    Sraw(RTypeArgs),

    // RV64I I-type instructions
    Addi(ITypeArgs),
    Addiw(ITypeArgs),
    Xori(ITypeArgs),
    Ori(ITypeArgs),
    Andi(ITypeArgs),
    Slli(ITypeArgs),
    Srli(ITypeArgs),
    Srai(ITypeArgs),
    Slliw(ITypeArgs),
    Srliw(ITypeArgs),
    Sraiw(ITypeArgs),
    Slti(ITypeArgs),
    Sltiu(ITypeArgs),
    Lb(ITypeArgs),
    Lh(ITypeArgs),
    Lw(ITypeArgs),
    Lbu(ITypeArgs),
    Lhu(ITypeArgs),
    Lwu(ITypeArgs),
    Ld(ITypeArgs),
    Fence(ITypeArgs),
    Ecall,
    Ebreak,

    // RV64I S-type instructions
    Sb(SBTypeArgs),
    Sh(SBTypeArgs),
    Sw(SBTypeArgs),
    Sd(SBTypeArgs),

    // RV64I B-type instructions
    Beq(SBTypeArgs),
    Bne(SBTypeArgs),
    Blt(SBTypeArgs),
    Bge(SBTypeArgs),
    Bltu(SBTypeArgs),
    Bgeu(SBTypeArgs),

    // RV64I U-type instructions
    Lui(UJTypeArgs),
    Auipc(UJTypeArgs),

    // RV64I jump instructions
    Jal(UJTypeArgs),
    Jalr(ITypeArgs),

    Unknown { instr: u32 },
    UnknownCompressed { instr: u16 },
}
