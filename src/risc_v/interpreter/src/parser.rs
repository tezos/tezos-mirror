// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

pub mod instruction;

use crate::machine_state::{
    csregisters::{try_parse_csregister, CSRegister},
    registers::{parse_fregister, parse_xregister, FRegister, XRegister},
};
use core::ops::Range;
use instruction::*;

/// Given an instruction encoded as a little-endian `u32`, extract `n` bits
/// starting at `pos`.
#[inline(always)]
const fn bits(bytes: u32, pos: usize, n: usize) -> u32 {
    (bytes >> pos) & (!0 >> (32 - n))
}

#[inline(always)]
fn bit(bytes: u32, pos: usize) -> bool {
    bytes & (1 << pos) != 0
}

#[inline(always)]
fn opcode(instr: u32) -> u32 {
    bits(instr, 0, 7)
}

#[inline(always)]
fn funct3(instr: u32) -> u32 {
    bits(instr, 12, 3)
}

#[inline(always)]
const fn funct5(instr: u32) -> u32 {
    bits(instr, 27, 5)
}

#[inline(always)]
fn funct7(instr: u32) -> u32 {
    bits(instr, 25, 7)
}

#[inline(always)]
fn rd(instr: u32) -> XRegister {
    parse_xregister(bits(instr, 7, 5))
}

#[inline(always)]
fn rd_f(instr: u32) -> FRegister {
    parse_fregister(bits(instr, 7, 5))
}

#[inline(always)]
fn rs1_bits(instr: u32) -> u32 {
    bits(instr, 15, 5)
}

#[inline(always)]
fn rs1(instr: u32) -> XRegister {
    parse_xregister(rs1_bits(instr))
}

#[inline(always)]
fn rs1_f(instr: u32) -> FRegister {
    parse_fregister(rs1_bits(instr))
}

#[inline(always)]
fn rs2_bits(instr: u32) -> u32 {
    bits(instr, 20, 5)
}

#[inline(always)]
fn rs2(instr: u32) -> XRegister {
    parse_xregister(rs2_bits(instr))
}

#[inline(always)]
fn rs2_f(instr: u32) -> FRegister {
    parse_fregister(rs2_bits(instr))
}

#[inline(always)]
fn imm_11_6(instr: u32) -> u32 {
    bits(instr, 26, 6) << 1
}

#[inline(always)]
fn fm(instr: u32) -> u32 {
    bits(instr, 28, 4)
}

/// Rounding mode for F/D
#[allow(non_upper_case_globals)]
const rm: fn(u32) -> u32 = funct3;

/// Floating-point format field encoding
#[inline(always)]
const fn fmt(instr: u32) -> u32 {
    bits(instr, 25, 2)
}

/// Floating-point width field encoding
#[inline(always)]
const fn width(instr: u32) -> u32 {
    bits(instr, 12, 3)
}

fn csr(instr: u32) -> Option<CSRegister> {
    try_parse_csregister(bits(instr, 20, 12))
}

// Immediates are produced by extracting the relevant bits according to the
// instruction format (c.f. Section 2.3), then shifting them into place.
// The sign bit is always bit 31 of the instruction. Sign extension is
// performed by first casting each segment to i32, then casting the produced
// immediate to i64.

fn i_imm(instr: u32) -> i64 {
    // instr[31:20]
    (((instr & 0b1111_1111_1111_0000_0000_0000_0000_0000) as i32) >> 20) as i64
}

fn s_imm(instr: u32) -> i64 {
    // instr[31:25] | instr[11:7]
    let instr_31_25 = (instr & 0b1111_1110_0000_0000_0000_0000_0000_0000) as i32;
    let instr_11_7 = (instr & 0b0000_0000_0000_0000_0000_1111_1000_0000) as i32;
    ((instr_31_25 >> 20) | (instr_11_7 >> 7)) as i64
}

fn b_imm(instr: u32) -> i64 {
    // instr[31] | instr[7] | instr[30:25] | instr[11:8] | 0
    let instr_31 = (instr & 0b1000_0000_0000_0000_0000_0000_0000_0000) as i32;
    let instr_7 = (instr & 0b0000_0000_0000_0000_0000_0000_1000_0000) as i32;
    let instr_30_25 = (instr & 0b0111_1110_0000_0000_0000_0000_0000_0000) as i32;
    let instr_11_8 = (instr & 0b0000_0000_0000_0000_0000_1111_0000_0000) as i32;
    ((instr_31 >> 19) | (instr_7 << 4) | (instr_30_25 >> 20) | (instr_11_8 >> 7)) as i64
}

fn u_imm(instr: u32) -> i64 {
    // instr[31:12] | 0000_0000_0000
    ((instr & 0b1111_1111_1111_1111_1111_0000_0000_0000) as i32) as i64
}

fn j_imm(instr: u32) -> i64 {
    // instr[31] | instr[19:12] | instr[20] | instr[30:21] | 0
    let instr_31 = (instr & 0b1000_0000_0000_0000_0000_0000_0000_0000) as i32;
    let instr_19_12 = (instr & 0b0000_0000_0000_1111_1111_0000_0000_0000) as i32;
    let instr_20 = (instr & 0b0000_0000_0001_0000_0000_0000_0000_0000) as i32;
    let instr_30_21 = (instr & 0b0111_1111_1110_0000_0000_0000_0000_0000) as i32;
    ((instr_31 >> 11) | instr_19_12 | (instr_20 >> 9) | (instr_30_21 >> 20)) as i64
}

#[allow(non_upper_case_globals)]
const fl_imm: fn(u32) -> i64 = i_imm;
#[allow(non_upper_case_globals)]
const fs_imm: fn(u32) -> i64 = s_imm;

macro_rules! r_instr {
    ($enum_variant:ident, $instr:expr) => {
        $enum_variant(instruction::RTypeArgs {
            rd: rd($instr),
            rs1: rs1($instr),
            rs2: rs2($instr),
        })
    };
}

macro_rules! i_instr {
    ($enum_variant:ident, $instr:expr) => {
        $enum_variant(instruction::ITypeArgs {
            rd: rd($instr),
            rs1: rs1($instr),
            imm: i_imm($instr),
        })
    };
}

macro_rules! s_instr {
    ($enum_variant:ident, $instr:expr) => {
        $enum_variant(instruction::SBTypeArgs {
            rs1: rs1($instr),
            rs2: rs2($instr),
            imm: s_imm($instr),
        })
    };
}

macro_rules! b_instr {
    ($enum_variant:ident, $instr:expr) => {
        $enum_variant(instruction::SBTypeArgs {
            rs1: rs1($instr),
            rs2: rs2($instr),
            imm: b_imm($instr),
        })
    };
}

macro_rules! u_instr {
    ($enum_variant:ident, $instr:expr) => {
        $enum_variant(instruction::UJTypeArgs {
            rd: rd($instr),
            imm: u_imm($instr),
        })
    };
}

macro_rules! j_instr {
    ($enum_variant:ident, $instr:expr) => {
        $enum_variant(instruction::UJTypeArgs {
            rd: rd($instr),
            imm: j_imm($instr),
        })
    };
}

macro_rules! fl_instr {
    ($enum_variant:ident, $instr:expr) => {
        $enum_variant(instruction::FLoadArgs {
            rd: rd_f($instr),
            rs1: rs1($instr),
            imm: fl_imm($instr),
        })
    };
}

macro_rules! fs_instr {
    ($enum_variant:ident, $instr:expr) => {
        $enum_variant(instruction::FStoreArgs {
            rs1: rs1($instr),
            rs2: rs2_f($instr),
            imm: fs_imm($instr),
        })
    };
}

macro_rules! f_cmp_instr {
    ($enum_variant:ident, $instr:expr, $rs2_bits:expr) => {
        $enum_variant(instruction::FCmpArgs {
            rd: rd($instr),
            rs1: rs1_f($instr),
            rs2: parse_fregister($rs2_bits),
        })
    };
}

macro_rules! f_r_instr {
    ($enum_variant:ident, $instr:expr, $rs2_bits:expr) => {
        $enum_variant(instruction::FRArgs {
            rd: rd_f($instr),
            rs1: rs1_f($instr),
            rs2: parse_fregister($rs2_bits),
        })
    };
}

macro_rules! fence_instr {
    ($enum_variant:ident, $instr:expr) => {
        $enum_variant(instruction::FenceArgs {
            pred: FenceSet {
                i: bit($instr, 27),
                o: bit($instr, 26),
                r: bit($instr, 25),
                w: bit($instr, 24),
            },
            succ: FenceSet {
                i: bit($instr, 23),
                o: bit($instr, 22),
                r: bit($instr, 21),
                w: bit($instr, 20),
            },
        })
    };
}

macro_rules! amo_instr {
    ($enum_variant:ident, $instr:expr) => {
        $enum_variant(instruction::AmoArgs {
            rd: rd($instr),
            rs1: rs1($instr),
            rs2: rs2($instr),
            aq: bit($instr, 26),
            rl: bit($instr, 25),
        })
    };
}

macro_rules! csr_instr {
    ($enum_variant:ident, $instr:expr) => {
        match csr($instr) {
            Some(csr) => $enum_variant(instruction::CsrArgs {
                rd: rd($instr),
                rs1: rs1($instr),
                csr,
            }),
            None => Unknown { instr: $instr },
        }
    };
}

macro_rules! csri_instr {
    ($enum_variant:ident, $instr:expr) => {
        match csr($instr) {
            Some(csr) => $enum_variant(instruction::CsriArgs {
                rd: rd($instr),
                imm: bits($instr, 15, 5) as i64,
                csr,
            }),
            None => Unknown { instr: $instr },
        }
    };
}

const OP_ARITH: u32 = 0b011_0011;
const OP_ARITH_W: u32 = 0b011_1011;
const OP_ARITH_I: u32 = 0b001_0011;
const OP_LOAD: u32 = 0b000_0011;
const OP_ARITH_IW: u32 = 0b001_1011;
const OP_SYNCH: u32 = 0b000_1111;
const OP_SYS: u32 = 0b111_0011;
const OP_STORE: u32 = 0b010_0011;
const OP_BRANCH: u32 = 0b110_0011;
const OP_LUI: u32 = 0b011_0111;
const OP_AUIPC: u32 = 0b001_0111;
const OP_JAL: u32 = 0b110_1111;
const OP_JALR: u32 = 0b110_0111;
const OP_FP: u32 = 0b1010011;
const OP_FP_LOAD: u32 = 0b000_0111;
const OP_FP_STORE: u32 = 0b010_0111;
const OP_AMO: u32 = 0b010_1111;

const F3_0: u32 = 0b000;
const F3_1: u32 = 0b001;
const F3_2: u32 = 0b010;
const F3_3: u32 = 0b011;
const F3_4: u32 = 0b100;
const F3_5: u32 = 0b101;
const F3_6: u32 = 0b110;
const F3_7: u32 = 0b111;

const F5_0: u32 = 0b0_0000;
const F5_1: u32 = 0b0_0001;
const F5_4: u32 = 0b0_0100;
const F5_5: u32 = 0b0_0101;
const F5_8: u32 = 0b0_1000;
const F5_12: u32 = 0b0_1100;
const F5_16: u32 = 0b1_0000;
const F5_20: u32 = 0b1_0100;
const F5_24: u32 = 0b1_1000;
const F5_28: u32 = 0b1_1100;
const F5_30: u32 = 0b1_1110;

const F7_0: u32 = 0b0;
const F7_1: u32 = 0b1;
const F7_9: u32 = 0b1001;
const F7_8: u32 = 0b000_1000;
const F7_20: u32 = 0b10_0000;
const F7_24: u32 = 0b001_1000;
const F7_56: u32 = 0b011_1000;

const FMT_S: u32 = 0b0;
const FMT_D: u32 = 0b01;

const WIDTH_W: u32 = 0b010;
const WIDTH_D: u32 = 0b011;

const RM_0: u32 = 0b0;
const RM_1: u32 = 0b1;
const RM_EQ: u32 = 0b10;
const RM_LT: u32 = RM_1;
const RM_LE: u32 = RM_0;
const RM_MIN: u32 = RM_0;
const RM_MAX: u32 = RM_1;

const RS1_0: u32 = 0b0;
const RS2_0: u32 = 0b0;
const RS2_1: u32 = 0b1;
const RS2_2: u32 = 0b10;
const RS2_5: u32 = 0b101;

const FM_0: u32 = 0b0;
const FM_8: u32 = 0b1000;

fn parse_uncompressed_instruction(instr: u32) -> Instr {
    use Instr::*;
    match opcode(instr) {
        // R-type instructions
        OP_ARITH => match funct3(instr) {
            F3_0 => match funct7(instr) {
                F7_0 => r_instr!(Add, instr),
                F7_1 => r_instr!(Mul, instr),
                F7_20 => r_instr!(Sub, instr),
                _ => Unknown { instr },
            },
            F3_4 => match funct7(instr) {
                F7_0 => r_instr!(Xor, instr),
                F7_1 => r_instr!(Div, instr),
                _ => Unknown { instr },
            },
            F3_6 => match funct7(instr) {
                F7_0 => r_instr!(Or, instr),
                F7_1 => r_instr!(Rem, instr),
                _ => Unknown { instr },
            },
            F3_7 => match funct7(instr) {
                F7_0 => r_instr!(And, instr),
                F7_1 => r_instr!(Remu, instr),
                _ => Unknown { instr },
            },
            F3_1 => match funct7(instr) {
                F7_0 => r_instr!(Sll, instr),
                F7_1 => r_instr!(Mulh, instr),
                _ => Unknown { instr },
            },
            F3_5 => match funct7(instr) {
                F7_0 => r_instr!(Srl, instr),
                F7_1 => r_instr!(Divu, instr),
                F7_20 => r_instr!(Sra, instr),
                _ => Unknown { instr },
            },

            F3_2 => match funct7(instr) {
                F7_0 => r_instr!(Slt, instr),
                F7_1 => r_instr!(Mulhsu, instr),
                _ => Unknown { instr },
            },

            F3_3 => match funct7(instr) {
                F7_0 => r_instr!(Sltu, instr),
                F7_1 => r_instr!(Mulhu, instr),
                _ => Unknown { instr },
            },

            _ => Unknown { instr },
        },
        OP_ARITH_W => match funct3(instr) {
            F3_0 => match funct7(instr) {
                F7_0 => r_instr!(Addw, instr),
                F7_1 => r_instr!(Mulw, instr),
                F7_20 => r_instr!(Subw, instr),
                _ => Unknown { instr },
            },
            F3_1 => r_instr!(Sllw, instr),
            F3_4 => match funct7(instr) {
                F7_1 => r_instr!(Divw, instr),
                _ => Unknown { instr },
            },
            F3_5 => match funct7(instr) {
                F7_0 => r_instr!(Srlw, instr),
                F7_1 => r_instr!(Divuw, instr),
                F7_20 => r_instr!(Sraw, instr),
                _ => Unknown { instr },
            },

            F3_6 => match funct7(instr) {
                F7_1 => r_instr!(Remw, instr),
                _ => Unknown { instr },
            },
            F3_7 => match funct7(instr) {
                F7_1 => r_instr!(Remuw, instr),
                _ => Unknown { instr },
            },

            _ => Unknown { instr },
        },

        // I-type instructions
        OP_ARITH_I => match funct3(instr) {
            F3_0 => i_instr!(Addi, instr),
            F3_4 => i_instr!(Xori, instr),
            F3_6 => i_instr!(Ori, instr),
            F3_7 => i_instr!(Andi, instr),
            F3_1 => match imm_11_6(instr) {
                // imm[0:5] -> shift amount
                F7_0 => i_instr!(Slli, instr),
                _ => Unknown { instr },
            },
            F3_5 => match imm_11_6(instr) {
                // imm[6:11] -> type of shift, imm[0:5] -> shift amount
                F7_0 => i_instr!(Srli, instr),
                F7_20 => i_instr!(Srai, instr),
                _ => Unknown { instr },
            },
            F3_2 => i_instr!(Slti, instr),
            F3_3 => i_instr!(Sltiu, instr),
            _ => Unknown { instr },
        },
        OP_LOAD => match funct3(instr) {
            F3_0 => i_instr!(Lb, instr),
            F3_1 => i_instr!(Lh, instr),
            F3_2 => i_instr!(Lw, instr),
            F3_3 => i_instr!(Ld, instr),
            F3_4 => i_instr!(Lbu, instr),
            F3_5 => i_instr!(Lhu, instr),
            F3_6 => i_instr!(Lwu, instr),
            _ => Unknown { instr },
        },
        OP_ARITH_IW => match funct3(instr) {
            F3_0 => i_instr!(Addiw, instr),
            F3_1 => match imm_11_6(instr) {
                // imm[0:4] -> shift amount
                F7_0 => i_instr!(Slliw, instr),
                _ => Unknown { instr },
            },
            F3_5 => match imm_11_6(instr) {
                // imm[6:11] -> type of shift, imm[0:4] -> shift amount
                F7_0 => i_instr!(Srliw, instr),
                F7_20 => i_instr!(Sraiw, instr),
                _ => Unknown { instr },
            },
            _ => Unknown { instr },
        },
        OP_SYNCH => match funct3(instr) {
            F3_0 => match fm(instr) {
                FM_0 => fence_instr!(Fence, instr),
                FM_8 => fence_instr!(FenceTso, instr),
                _ => Unknown { instr },
            },
            F3_1 => FenceI,
            _ => Unknown { instr },
        },
        OP_SYS => match funct3(instr) {
            F3_0 => match funct7(instr) {
                F7_0 => match (rs1_bits(instr), rs2_bits(instr)) {
                    (RS1_0, RS2_0) => Ecall,
                    (RS1_0, RS2_1) => Ebreak,
                    _ => Unknown { instr },
                },
                F7_9 => SFenceVma {
                    vaddr: rs1(instr),
                    asid: rs2(instr),
                },
                F7_8 => match (rs1_bits(instr), rs2_bits(instr)) {
                    (RS1_0, RS2_2) => Sret,
                    (RS1_0, RS2_5) => Wfi,
                    _ => Unknown { instr },
                },
                F7_24 => match (rs1_bits(instr), rs2_bits(instr)) {
                    (RS1_0, RS2_2) => Mret,
                    _ => Unknown { instr },
                },
                F7_56 => match (rs1_bits(instr), rs2_bits(instr)) {
                    (RS1_0, RS2_2) => Mnret,
                    _ => Unknown { instr },
                },
                _ => Unknown { instr },
            },
            F3_1 => csr_instr!(Csrrw, instr),
            F3_2 => csr_instr!(Csrrs, instr),
            F3_3 => csr_instr!(Csrrc, instr),
            F3_5 => csri_instr!(Csrrwi, instr),
            F3_6 => csri_instr!(Csrrsi, instr),
            F3_7 => csri_instr!(Csrrci, instr),
            _ => Unknown { instr },
        },
        OP_AMO => match funct3(instr) {
            F3_2 => match funct5(instr) {
                F5_1 => amo_instr!(Amoswapw, instr),
                F5_0 => amo_instr!(Amoaddw, instr),
                F5_4 => amo_instr!(Amoxorw, instr),
                F5_12 => amo_instr!(Amoandw, instr),
                F5_8 => amo_instr!(Amoorw, instr),
                F5_16 => amo_instr!(Amominw, instr),
                F5_20 => amo_instr!(Amomaxw, instr),
                F5_24 => amo_instr!(Amominuw, instr),
                F5_28 => amo_instr!(Amomaxuw, instr),
                _ => Unknown { instr },
            },
            _ => Unknown { instr },
        },

        // S-type instructions
        OP_STORE => match funct3(instr) {
            F3_0 => s_instr!(Sb, instr),
            F3_1 => s_instr!(Sh, instr),
            F3_2 => s_instr!(Sw, instr),
            F3_3 => s_instr!(Sd, instr),
            _ => Unknown { instr },
        },

        // B-type instructions
        OP_BRANCH => match funct3(instr) {
            F3_0 => b_instr!(Beq, instr),
            F3_1 => b_instr!(Bne, instr),
            F3_4 => b_instr!(Blt, instr),
            F3_5 => b_instr!(Bge, instr),
            F3_6 => b_instr!(Bltu, instr),
            F3_7 => b_instr!(Bgeu, instr),
            _ => Unknown { instr },
        },

        // U-type instructions
        OP_LUI => u_instr!(Lui, instr),
        OP_AUIPC => u_instr!(Auipc, instr),

        // F/D-type instructions
        OP_FP => match fmt(instr) {
            FMT_S => match (funct5(instr), rm(instr), rs2_bits(instr)) {
                (F5_5, RM_MIN, rs2_bits) => f_r_instr!(Fmins, instr, rs2_bits),
                (F5_5, RM_MAX, rs2_bits) => f_r_instr!(Fmaxs, instr, rs2_bits),
                (F5_20, RM_EQ, rs2_bits) => f_cmp_instr!(Feqs, instr, rs2_bits),
                (F5_20, RM_LE, rs2_bits) => f_cmp_instr!(Fles, instr, rs2_bits),
                (F5_20, RM_LT, rs2_bits) => f_cmp_instr!(Flts, instr, rs2_bits),
                (F5_28, RM_0, RS2_0) => FmvXW(FRegToXRegArgs {
                    rd: rd(instr),
                    rs1: rs1_f(instr),
                }),
                (F5_28, RM_1, RS2_0) => FclassS(FRegToXRegArgs {
                    rd: rd(instr),
                    rs1: rs1_f(instr),
                }),
                (F5_30, RM_0, RS2_0) => FmvWX(XRegToFRegArgs {
                    rd: rd_f(instr),
                    rs1: rs1(instr),
                }),
                _ => Unknown { instr },
            },
            FMT_D => match (funct5(instr), rm(instr), rs2_bits(instr)) {
                (F5_5, RM_MIN, rs2_bits) => f_r_instr!(Fmind, instr, rs2_bits),
                (F5_5, RM_MAX, rs2_bits) => f_r_instr!(Fmaxd, instr, rs2_bits),
                (F5_20, RM_EQ, rs2_bits) => f_cmp_instr!(Feqd, instr, rs2_bits),
                (F5_20, RM_LE, rs2_bits) => f_cmp_instr!(Fled, instr, rs2_bits),
                (F5_20, RM_LT, rs2_bits) => f_cmp_instr!(Fltd, instr, rs2_bits),
                (F5_28, RM_0, RS2_0) => FmvXD(FRegToXRegArgs {
                    rd: rd(instr),
                    rs1: rs1_f(instr),
                }),
                (F5_28, RM_1, RS2_0) => FclassD(FRegToXRegArgs {
                    rd: rd(instr),
                    rs1: rs1_f(instr),
                }),
                (F5_30, RM_0, RS2_0) => FmvDX(XRegToFRegArgs {
                    rd: rd_f(instr),
                    rs1: rs1(instr),
                }),
                _ => Unknown { instr },
            },
            _ => Unknown { instr },
        },

        // F/D-type load
        OP_FP_LOAD => match width(instr) {
            WIDTH_W => fl_instr!(Flw, instr),
            WIDTH_D => fl_instr!(Fld, instr),
            _ => Unknown { instr },
        },
        OP_FP_STORE => match width(instr) {
            WIDTH_W => fs_instr!(Fsw, instr),
            WIDTH_D => fs_instr!(Fsd, instr),
            _ => Unknown { instr },
        },
        // Jump instructions
        OP_JAL => j_instr!(Jal, instr),
        OP_JALR => match funct3(instr) {
            F3_0 => i_instr!(Jalr, instr),
            _ => Unknown { instr },
        },
        _ => Unknown { instr },
    }
}

fn parse_compressed_instruction(bytes: u16) -> Instr {
    // TODO parse a compressed instruction
    Instr::UnknownCompressed { instr: bytes }
}

/// Attempt to parse `bytes` into an instruction. If `bytes` encodes a 2-byte
/// compressed instruction, parse it immediately. If it encodes a 4-byte
/// uncompressed instruction, request 2 extra bytes via `more`.
pub fn parse<E>(bytes: u16, more: impl FnOnce() -> Result<u16, E>) -> Result<Instr, E> {
    if bytes & 0b11 != 0b11 {
        Ok(parse_compressed_instruction(bytes))
    } else {
        let upper = more()?;
        let combined = (upper as u32) << 16 | (bytes as u32);
        Ok(parse_uncompressed_instruction(combined))
    }
}

pub fn u16_iter_from_u8_iter(mut iter: impl Iterator<Item = u8>) -> impl Iterator<Item = u16> {
    std::iter::from_fn(move || {
        let lower = iter.next()?;
        iter.next().map(|upper| u16::from_le_bytes([lower, upper]))
    })
}

pub fn instr_iter_from_u16_iter(
    mut iter: impl Iterator<Item = u16>,
) -> impl Iterator<Item = Instr> {
    std::iter::from_fn(move || parse(iter.next()?, || iter.next().ok_or(())).ok())
}

pub fn parse_block(bytes: &[u8]) -> Vec<Instr> {
    let iter = bytes.iter().copied();
    instr_iter_from_u16_iter(u16_iter_from_u8_iter(iter)).collect()
}

pub fn parse_segment(contents: &[u8], range: Range<usize>) -> Vec<Instr> {
    parse_block(&contents[range])
}

#[cfg(test)]
mod tests {
    use super::{
        instruction::{CsrArgs, ITypeArgs, Instr::*, SBTypeArgs, UJTypeArgs},
        parse_block,
    };
    use crate::machine_state::{csregisters::CSRegister::mcause, registers::XRegister::*};

    // rv64ui-p-addiw
    // 0000000080000000 <_start>:
    //     80000000:	0500006f          	jal	zero,80000050 <reset_vector>

    // 0000000080000004 <trap_vector>:
    //     80000004:	34202f73          	csrrs	t5,mcause,zero
    //     80000008:	00800f93          	addi	t6,zero,8
    #[test]
    fn test_1() {
        let bytes: [u8; 12] = [
            0x6f, 0x0, 0x0, 0x5, 0x73, 0x2f, 0x20, 0x34, 0x93, 0xf, 0x80, 0x0,
        ];

        let expected = [
            Jal(UJTypeArgs { rd: x0, imm: 0x50 }),
            Csrrs(CsrArgs {
                rd: x30,
                rs1: x0,
                csr: mcause,
            }),
            Addi(ITypeArgs {
                rd: x31,
                rs1: x0,
                imm: 0x8,
            }),
        ];
        let instructions = parse_block(&bytes);
        assert_eq!(instructions, expected)
    }

    // rv64uc-p-rvc
    // 0000000080002190 <test_21>:
    // 80002190:	01500193          	addi	gp,zero,21
    // 80002194:	6405                	c.lui	s0,0x1
    // 80002196:	2344041b          	addiw	s0,s0,564 # 1234 <_start-0x7fffedcc>
    // 8000219a:	0412                	c.slli	s0,0x4
    // 8000219c:	000123b7          	lui	t2,0x12
    // 800021a0:	3403839b          	addiw	t2,t2,832 # 12340 <_start-0x7ffedcc0>
    // 800021a4:	12741063          	bne	s0,t2,800022c4 <fail>
    #[test]
    fn test_2() {
        let bytes: [u8; 24] = [
            0x93, 0x1, 0x50, 0x1, 0x5, 0x64, 0x1b, 0x4, 0x44, 0x23, 0x12, 0x4, 0xb7, 0x23, 0x1,
            0x0, 0x9b, 0x83, 0x3, 0x34, 0x63, 0x10, 0x74, 0x12,
        ];
        let expected = [
            Addi(ITypeArgs {
                rd: x3,
                rs1: x0,
                imm: 21,
            }),
            UnknownCompressed {
                instr: u16::from_le_bytes([0x05, 0x64]),
            },
            Addiw(ITypeArgs {
                rd: x8,
                rs1: x8,
                imm: 564,
            }),
            UnknownCompressed {
                instr: u16::from_le_bytes([0x12, 0x04]),
            },
            Lui(UJTypeArgs {
                rd: x7,
                imm: 0x12 << 12,
            }),
            Addiw(ITypeArgs {
                rd: x7,
                rs1: x7,
                imm: 832,
            }),
            Bne(SBTypeArgs {
                rs1: x8,
                rs2: x7,
                imm: 288,
            }),
        ];
        let instructions = parse_block(&bytes);
        assert_eq!(instructions, expected)
    }

    // Misaligned buffer, last byte ignored
    #[test]
    fn test_3() {
        let bytes: [u8; 5] = [0x1, 0x5, 0x64, 0x1b, 0x4];
        let expected = [
            UnknownCompressed {
                instr: u16::from_le_bytes([0x1, 0x5]),
            },
            UnknownCompressed {
                instr: u16::from_le_bytes([0x64, 0x1b]),
            },
        ];
        let instructions = parse_block(&bytes);
        assert_eq!(instructions, expected)
    }

    // Expected uncompressed instruction, only 2 bytes left
    #[test]
    fn test_4() {
        let bytes: [u8; 6] = [0x6f, 0x0, 0x0, 0x5, 0x73, 0x2f];
        let expected = [Jal(UJTypeArgs { rd: x0, imm: 0x50 })];
        let instructions = parse_block(&bytes);
        assert_eq!(instructions, expected)
    }

    // A valid `slli	x10,x10,0x1f` instruction, followed by an invalid one,
    // in which one of the upper 6 bits (bit 29) has been set to 1.
    #[test]
    fn test_5() {
        let bytes: [u8; 8] = [0x13, 0x15, 0xf5, 0x01, 0x13, 0x15, 0xf5, 0x21];
        let expected = [
            Slli(ITypeArgs {
                rd: x10,
                rs1: x10,
                imm: 31,
            }),
            Unknown {
                instr: u32::from_le_bytes([0x13, 0x15, 0xf5, 0x21]),
            },
        ];
        let instructions = parse_block(&bytes);
        assert_eq!(instructions, expected)
    }

    // Instructions not covered in integration tests
    #[test]
    fn test_6() {
        let bytes: [u8; 4] = [0x73, 0x00, 0x20, 0x70];
        let expected = [Mnret];
        let instructions = parse_block(&bytes);
        assert_eq!(instructions, expected)
    }
}
