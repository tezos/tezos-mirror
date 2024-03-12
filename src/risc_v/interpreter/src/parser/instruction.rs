// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::fmt;

use crate::machine_state::{csregisters::CSRegister, registers::XRegister};

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct RTypeArgs {
    pub rd: XRegister,
    pub rs1: XRegister,
    pub rs2: XRegister,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ITypeArgs {
    pub rd: XRegister,
    pub rs1: XRegister,
    pub imm: i64,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct SBTypeArgs {
    pub rs1: XRegister,
    pub rs2: XRegister,
    pub imm: i64,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct UJTypeArgs {
    pub rd: XRegister,
    pub imm: i64,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct CsrArgs {
    pub rd: XRegister,
    pub rs1: XRegister,
    pub csr: CSRegister,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct CsriArgs {
    pub rd: XRegister,
    pub imm: i64,
    pub csr: CSRegister,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct FenceSet {
    pub i: bool,
    pub o: bool,
    pub r: bool,
    pub w: bool,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct FenceArgs {
    pub pred: FenceSet,
    pub succ: FenceSet,
}

/// RISC-V parsed instructions. Along with legal instructions, potentially
/// illegal instructions are parsed as `Unknown` or `UnknownCompressed`.
/// These instructions are successfully parsed, but must not be interpreted.
#[derive(Debug, PartialEq, Clone, Copy)]
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
    Fence(FenceArgs),
    FenceTso(FenceArgs),
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

    // Zicsr instructions
    Csrrw(CsrArgs),
    Csrrs(CsrArgs),
    Csrrc(CsrArgs),
    Csrrwi(CsriArgs),
    Csrrsi(CsriArgs),
    Csrrci(CsriArgs),

    // Zifencei instructions
    FenceI,

    // Privileged instructions
    Mret,
    Sret,
    Mnret,

    Unknown { instr: u32 },
    UnknownCompressed { instr: u16 },
}

use Instr::*;

impl Instr {
    /// Return the width of the instruction in bytes.
    pub fn width(&self) -> u64 {
        match self {
            // 4 bytes instructions
            Add(_)
            | Sub(_)
            | Xor(_)
            | Or(_)
            | And(_)
            | Sll(_)
            | Srl(_)
            | Sra(_)
            | Slt(_)
            | Sltu(_)
            | Addw(_)
            | Subw(_)
            | Sllw(_)
            | Srlw(_)
            | Sraw(_)
            | Addi(_)
            | Addiw(_)
            | Xori(_)
            | Ori(_)
            | Andi(_)
            | Slli(_)
            | Srli(_)
            | Srai(_)
            | Slliw(_)
            | Srliw(_)
            | Sraiw(_)
            | Slti(_)
            | Sltiu(_)
            | Lb(_)
            | Lh(_)
            | Lw(_)
            | Lbu(_)
            | Lhu(_)
            | Lwu(_)
            | Ld(_)
            | Fence(_)
            | FenceTso(_)
            | Ecall
            | Ebreak
            | Sb(_)
            | Sh(_)
            | Sw(_)
            | Sd(_)
            | Beq(_)
            | Bne(_)
            | Blt(_)
            | Bge(_)
            | Bltu(_)
            | Bgeu(_)
            | Lui(_)
            | Auipc(_)
            | Jal(_)
            | Jalr(_)
            | Csrrw(_)
            | Csrrs(_)
            | Csrrc(_)
            | Csrrwi(_)
            | Csrrsi(_)
            | Csrrci(_)
            | FenceI
            | Mret
            | Sret
            | Mnret
            | Unknown { instr: _ } => 4,

            // 2 bytes instructions (compressed instructions)
            UnknownCompressed { instr: _ } => 2,
        }
    }
}

macro_rules! r_instr {
    ($f:expr, $op:expr, $args:expr) => {
        write!($f, "{} {:?},{:?},{:?}", $op, $args.rd, $args.rs1, $args.rs2)
    };
}

macro_rules! i_instr {
    ($f:expr, $op:expr, $args:expr) => {
        write!($f, "{} {:?},{:?},{}", $op, $args.rd, $args.rs1, $args.imm)
    };
}

macro_rules! i_instr_hex {
    ($f:expr, $op:expr, $args:expr) => {
        write!(
            $f,
            "{} {:?},{:?},0x{:x}",
            $op, $args.rd, $args.rs1, $args.imm
        )
    };
}

macro_rules! i_instr_load {
    ($f:expr, $op:expr, $args:expr) => {
        write!($f, "{} {:?},{}({:?})", $op, $args.rd, $args.imm, $args.rs1)
    };
}

macro_rules! j_instr {
    ($f:expr, $op:expr, $args:expr) => {
        write!($f, "{} {:?},0x{:x}", $op, $args.rd, $args.imm)
    };
}

macro_rules! s_instr {
    ($f:expr, $op:expr, $args:expr) => {
        write!($f, "{} {:?},{}({:?})", $op, $args.rs2, $args.imm, $args.rs1)
    };
}

macro_rules! b_instr {
    ($f:expr, $op:expr, $args:expr) => {
        write!($f, "{} {:?},{:?},{}", $op, $args.rs1, $args.rs2, $args.imm)
    };
}

macro_rules! u_instr {
    ($f:expr, $op:expr, $args:expr) => {
        write!($f, "{} {:?},{}", $op, $args.rd, $args.imm)
    };
}

macro_rules! fence_instr {
    ($f:expr, $op:expr, $args:expr) => {
        write!($f, "{} {},{}", $op, $args.pred, $args.succ)
    };
}

macro_rules! csr_instr {
    ($f:expr, $op:expr, $args:expr) => {
        write!($f, "{} {:?},{},{:?}", $op, $args.rd, $args.csr, $args.rs1)
    };
}

macro_rules! csri_instr {
    ($f:expr, $op:expr, $args:expr) => {
        write!($f, "{} {:?},{},{}", $op, $args.rd, $args.csr, $args.imm)
    };
}

impl fmt::Display for FenceSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = String::new();
        if self.i {
            out.push('i')
        };
        if self.o {
            out.push('o')
        };
        if self.r {
            out.push('r')
        };
        if self.w {
            out.push('w')
        };
        if out.is_empty() {
            write!(f, "unknown")
        } else {
            write!(f, "{}", out)
        }
    }
}

/// An objdump-style prettyprinter for parsed instructions, used in testing
/// the parser against objdump.
impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // RV64I R-type instructions
            Add(args) => r_instr!(f, "add", args),
            Sub(args) => r_instr!(f, "sub", args),
            Xor(args) => r_instr!(f, "xor", args),
            Or(args) => r_instr!(f, "or", args),
            And(args) => r_instr!(f, "and", args),
            Sll(args) => r_instr!(f, "sll", args),
            Srl(args) => r_instr!(f, "srl", args),
            Sra(args) => r_instr!(f, "sra", args),
            Slt(args) => r_instr!(f, "slt", args),
            Sltu(args) => r_instr!(f, "sltu", args),
            Addw(args) => r_instr!(f, "addw", args),
            Subw(args) => r_instr!(f, "subw", args),
            Sllw(args) => r_instr!(f, "sllw", args),
            Srlw(args) => r_instr!(f, "srlw", args),
            Sraw(args) => r_instr!(f, "sraw", args),

            // RV64I I-type instructions
            Addi(args) => i_instr!(f, "addi", args),
            Addiw(args) => i_instr!(f, "addiw", args),
            Xori(args) => i_instr!(f, "xori", args),
            Ori(args) => i_instr!(f, "ori", args),
            Andi(args) => i_instr!(f, "andi", args),
            Slli(args) => i_instr_hex!(f, "slli", args),
            Srli(args) => i_instr_hex!(f, "srli", args),
            // For consistency with objdump, only the shift amount is printed
            Srai(args) => {
                i_instr_hex!(
                    f,
                    "srai",
                    ITypeArgs {
                        imm: args.imm & !(1 << 10),
                        ..*args
                    }
                )
            }
            Slliw(args) => i_instr_hex!(f, "slliw", args),
            Srliw(args) => i_instr_hex!(f, "srliw", args),
            Sraiw(args) => {
                i_instr_hex!(
                    f,
                    "sraiw",
                    ITypeArgs {
                        imm: args.imm & !(1 << 10),
                        ..*args
                    }
                )
            }
            Slti(args) => i_instr!(f, "slti", args),
            Sltiu(args) => i_instr!(f, "sltiu", args),
            Lb(args) => i_instr_load!(f, "lb", args),
            Lh(args) => i_instr_load!(f, "lh", args),
            Lw(args) => i_instr_load!(f, "lw", args),
            Lbu(args) => i_instr_load!(f, "lbu", args),
            Lhu(args) => i_instr_load!(f, "lhu", args),
            Lwu(args) => i_instr_load!(f, "lwu", args),
            Ld(args) => i_instr_load!(f, "ld", args),

            Fence(args) => fence_instr!(f, "fence", args),
            FenceTso(args) => fence_instr!(f, "fence.tso", args),

            Ecall => write!(f, "ecall"),
            Ebreak => write!(f, "ebreak"),

            // RV64I S-type instructions
            Sb(args) => s_instr!(f, "sb", args),
            Sh(args) => s_instr!(f, "sh", args),
            Sw(args) => s_instr!(f, "sw", args),
            Sd(args) => s_instr!(f, "sd", args),

            // RV64I B-type instructions
            Beq(args) => b_instr!(f, "beq", args),
            Bne(args) => b_instr!(f, "bne", args),
            Blt(args) => b_instr!(f, "blt", args),
            Bge(args) => b_instr!(f, "bge", args),
            Bltu(args) => b_instr!(f, "bltu", args),
            Bgeu(args) => b_instr!(f, "bgeu", args),

            // RV64I U-type instructions
            // For consistency with objdump, upper immediates are shifted down
            Lui(args) => j_instr!(
                f,
                "lui",
                UJTypeArgs {
                    rd: args.rd,
                    imm: (args.imm >> 12) & ((0b1 << 20) - 1),
                }
            ),
            Auipc(args) => j_instr!(
                f,
                "auipc",
                UJTypeArgs {
                    rd: args.rd,
                    imm: (args.imm >> 12) & ((0b1 << 20) - 1),
                }
            ),

            // RV64I jump instructions
            Jal(args) => u_instr!(f, "jal", args),
            Jalr(args) => i_instr_load!(f, "jalr", args),

            // Zicsr instructions
            Csrrw(args) => csr_instr!(f, "csrrw", args),
            Csrrs(args) => csr_instr!(f, "csrrs", args),
            Csrrc(args) => csr_instr!(f, "csrrc", args),
            Csrrwi(args) => csri_instr!(f, "csrrwi", args),
            Csrrsi(args) => csri_instr!(f, "csrrsi", args),
            Csrrci(args) => csri_instr!(f, "csrrci", args),

            // Zifencei instructions
            FenceI => write!(f, "fence.i"),

            // Privileged instructions
            Mret => write!(f, "mret"),
            Sret => write!(f, "sret"),
            Mnret => write!(f, "mnret"),

            Unknown { instr } => write!(f, "unknown {:x}", instr),
            UnknownCompressed { instr } => write!(f, "unknown.c {:x}", instr),
        }
    }
}
