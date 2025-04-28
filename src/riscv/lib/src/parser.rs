// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

#![allow(clippy::reversed_empty_ranges)]

pub mod instruction;

use core::ops::Range;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

use arbitrary_int::u3;
use arbitrary_int::u5;
use instruction::*;

use crate::bits::u16;
use crate::machine_state::csregisters::CSRegister;
use crate::machine_state::registers::FRegister;
use crate::machine_state::registers::NonZeroXRegister;
use crate::machine_state::registers::XRegister;
use crate::machine_state::registers::parse_fregister;
use crate::machine_state::registers::parse_xregister;
use crate::machine_state::registers::x0;

/// Given an instruction encoded as a little-endian `u32`, extract `n` bits
/// starting at `pos`.
#[inline(always)]
const fn bits(bytes: u32, pos: usize, n: usize) -> u32 {
    (bytes >> pos) & (!0 >> (32 - n))
}

#[inline(always)]
const fn bit(bytes: u32, pos: usize) -> bool {
    bytes & (1 << pos) != 0
}

#[inline(always)]
const fn opcode(instr: u32) -> u32 {
    bits(instr, 0, 7)
}

#[inline(always)]
const fn funct3(instr: u32) -> u32 {
    bits(instr, 12, 3)
}

#[inline(always)]
const fn funct5(instr: u32) -> u32 {
    bits(instr, 27, 5)
}

#[inline(always)]
const fn funct7(instr: u32) -> u32 {
    bits(instr, 25, 7)
}

#[inline(always)]
const fn rd(instr: u32) -> XRegister {
    parse_xregister(u5::extract_u32(instr, 7))
}

#[inline(always)]
const fn rd_f(instr: u32) -> FRegister {
    parse_fregister(u5::extract_u32(instr, 7))
}

#[inline(always)]
const fn rs1_bits(instr: u32) -> u32 {
    bits(instr, 15, 5)
}

#[inline(always)]
const fn rs1_bits_u5(instr: u32) -> u5 {
    u5::extract_u32(instr, 15)
}

#[inline(always)]
const fn rs1(instr: u32) -> XRegister {
    parse_xregister(rs1_bits_u5(instr))
}

#[inline(always)]
const fn rs1_f(instr: u32) -> FRegister {
    parse_fregister(rs1_bits_u5(instr))
}

#[inline(always)]
const fn rs2_bits(instr: u32) -> u32 {
    bits(instr, 20, 5)
}

#[inline(always)]
const fn rs2_bits_u5(instr: u32) -> u5 {
    u5::extract_u32(instr, 20)
}

#[inline(always)]
const fn rs2(instr: u32) -> XRegister {
    parse_xregister(rs2_bits_u5(instr))
}

#[inline(always)]
const fn rs2_f(instr: u32) -> FRegister {
    parse_fregister(rs2_bits_u5(instr))
}

#[inline(always)]
const fn rs3_f(instr: u32) -> FRegister {
    parse_fregister(u5::extract_u32(instr, 27))
}

#[inline(always)]
const fn imm_11_6(instr: u32) -> u32 {
    bits(instr, 26, 6) << 1
}

#[inline(always)]
const fn fm(instr: u32) -> u32 {
    bits(instr, 28, 4)
}

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

const fn csr(instr: u32) -> Option<CSRegister> {
    CSRegister::try_parse(bits(instr, 20, 12))
}

// Immediates are produced by extracting the relevant bits according to the
// instruction format (c.f. Section 2.3), then shifting them into place.
// The sign bit is always bit 31 of the instruction. Sign extension is
// performed by first casting each segment to i32, then casting the produced
// immediate to i64.

const fn i_imm(instr: u32) -> i64 {
    // instr[31:20]
    (((instr & 0b1111_1111_1111_0000_0000_0000_0000_0000) as i32) >> 20) as i64
}

const fn shift_imm(instr: u32) -> i64 {
    let imm = i_imm(instr);
    imm & SHIFT_BITMASK
}

const fn s_imm(instr: u32) -> i64 {
    // instr[31:25] | instr[11:7]
    let instr_31_25 = (instr & 0b1111_1110_0000_0000_0000_0000_0000_0000) as i32;
    let instr_11_7 = (instr & 0b0000_0000_0000_0000_0000_1111_1000_0000) as i32;
    ((instr_31_25 >> 20) | (instr_11_7 >> 7)) as i64
}

const fn b_imm(instr: u32) -> i64 {
    // instr[31] | instr[7] | instr[30:25] | instr[11:8] | 0
    let instr_31 = (instr & 0b1000_0000_0000_0000_0000_0000_0000_0000) as i32;
    let instr_7 = (instr & 0b0000_0000_0000_0000_0000_0000_1000_0000) as i32;
    let instr_30_25 = (instr & 0b0111_1110_0000_0000_0000_0000_0000_0000) as i32;
    let instr_11_8 = (instr & 0b0000_0000_0000_0000_0000_1111_0000_0000) as i32;
    ((instr_31 >> 19) | (instr_7 << 4) | (instr_30_25 >> 20) | (instr_11_8 >> 7)) as i64
}

const fn u_imm(instr: u32) -> i64 {
    // instr[31:12] | 0000_0000_0000
    ((instr & 0b1111_1111_1111_1111_1111_0000_0000_0000) as i32) as i64
}

const fn j_imm(instr: u32) -> i64 {
    // instr[31] | instr[19:12] | instr[20] | instr[30:21] | 0
    let instr_31 = (instr & 0b1000_0000_0000_0000_0000_0000_0000_0000) as i32;
    let instr_19_12 = (instr & 0b0000_0000_0000_1111_1111_0000_0000_0000) as i32;
    let instr_20 = (instr & 0b0000_0000_0001_0000_0000_0000_0000_0000) as i32;
    let instr_30_21 = (instr & 0b0111_1111_1110_0000_0000_0000_0000_0000) as i32;
    ((instr_31 >> 11) | instr_19_12 | (instr_20 >> 9) | (instr_30_21 >> 20)) as i64
}

macro_rules! r_instr {
    ($enum_variant:ident, $instr:expr) => {
        $enum_variant(instruction::RTypeArgs {
            rd: rd($instr),
            rs1: rs1($instr),
            rs2: rs2($instr),
        })
    };

    ($enum_variant:ident, $instr:expr, $rd:expr) => {
        $enum_variant(instruction::NonZeroRdRTypeArgs {
            rd: $rd,
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

    ($enum_variant:ident, $instr:expr, $rd:expr) => {
        $enum_variant(instruction::NonZeroRdITypeArgs {
            rd: $rd,
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
    ($enum_variant:ident, $instr:expr, $rd:expr) => {
        $enum_variant(instruction::NonZeroRdUJTypeArgs {
            rd: $rd,
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
            imm: i_imm($instr),
        })
    };
}

macro_rules! fs_instr {
    ($enum_variant:ident, $instr:expr) => {
        $enum_variant(instruction::FStoreArgs {
            rs1: rs1($instr),
            rs2: rs2_f($instr),
            imm: s_imm($instr),
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

macro_rules! f_r_rm_1_instr {
    ($enum_variant:ident, $instr:expr, $rm:expr) => {
        if let Some(rounding) = InstrRoundingMode::from_rm($rm) {
            $enum_variant(instruction::FR1ArgWithRounding {
                rd: rd_f($instr),
                rs1: rs1_f($instr),
                rm: rounding,
            })
        } else {
            Unknown { instr: $instr }
        }
    };
}

macro_rules! f_r_fmt_int_instr {
    ($enum_variant:ident, $instr:expr, $rm:expr) => {
        if let Some(rounding) = InstrRoundingMode::from_rm($rm) {
            $enum_variant(instruction::XRegToFRegArgsWithRounding {
                rd: rd_f($instr),
                rs1: rs1($instr),
                rm: rounding,
            })
        } else {
            Unknown { instr: $instr }
        }
    };
}

macro_rules! f_r_int_fmt_instr {
    ($enum_variant:ident, $instr:expr, $rm:expr) => {
        if let Some(rounding) = InstrRoundingMode::from_rm($rm) {
            $enum_variant(instruction::FRegToXRegArgsWithRounding {
                rd: rd($instr),
                rs1: rs1_f($instr),
                rm: rounding,
            })
        } else {
            Unknown { instr: $instr }
        }
    };
}

macro_rules! f_r_rm_2_instr {
    ($enum_variant:ident, $instr:expr, $rs2_bits:expr, $rm:expr) => {{
        if let Some(rounding) = InstrRoundingMode::from_rm($rm) {
            $enum_variant(instruction::FR2ArgsWithRounding {
                rd: rd_f($instr),
                rs1: rs1_f($instr),
                rs2: parse_fregister($rs2_bits),
                rm: rounding,
            })
        } else {
            Unknown { instr: $instr }
        }
    }};
}

macro_rules! f_r_rm_3_instr {
    ($enum_variant:ident, $instr:expr) => {
        if let Some(rounding) = InstrRoundingMode::from_rm(funct3($instr)) {
            $enum_variant(instruction::FR3ArgsWithRounding {
                rd: rd_f($instr),
                rs1: rs1_f($instr),
                rs2: rs2_f($instr),
                rs3: rs3_f($instr),
                rm: rounding,
            })
        } else {
            Unknown { instr: $instr }
        }
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
const OP_FMADD: u32 = 0b100_0011;
const OP_FMSUB: u32 = 0b100_0111;
const OP_FNMSUB: u32 = 0b100_1011;
const OP_FNMADD: u32 = 0b100_1111;
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
const F5_2: u32 = 0b0_0010;
const F5_3: u32 = 0b0_0011;
const F5_4: u32 = 0b0_0100;
const F5_5: u32 = 0b0_0101;
const F5_8: u32 = 0b0_1000;
const F5_11: u32 = 0b0_1011;
const F5_12: u32 = 0b0_1100;
const F5_16: u32 = 0b1_0000;
const F5_20: u32 = 0b1_0100;
const F5_24: u32 = 0b1_1000;
const F5_26: u32 = 0b1_1010;
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

const RM_0: u32 = 0b000;
const RM_1: u32 = 0b001;
const RM_2: u32 = 0b010;
const RM_EQ: u32 = RM_2;
const RM_LT: u32 = RM_1;
const RM_LE: u32 = RM_0;
const RM_MIN: u32 = RM_0;
const RM_MAX: u32 = RM_1;

const RS1_0: u32 = 0b0;
const RS2_0: u32 = 0b0;
const RS2_1: u32 = 0b1;
const RS2_2: u32 = 0b10;
const RS2_5: u32 = 0b101;

const RS2_0_U5: u5 = u5::new(0b0);
const RS2_1_U5: u5 = u5::new(0b1);
const RS2_2_U5: u5 = u5::new(0b10);
const RS2_3_U5: u5 = u5::new(0b11);

const FM_0: u32 = 0b0;
const FM_8: u32 = 0b1000;

pub(crate) const SHIFT_BITMASK: i64 = 0b11_1111;

/// Parse an uncompressed instruction from a u32.
#[inline]
pub const fn parse_uncompressed_instruction(instr: u32) -> Instr {
    use InstrCacheable::*;
    use InstrUncacheable::*;
    // RV64I (Chapter 5.4) and RV64C (Chapter 16.7) describe the code points associated
    // with HINT instructions. We do not implement any HINT logic, but decode all these
    // as `Hint` or `HintCompresssed` opcodes, which we translate to NOPs in `machine_state`.
    use XRegisterParsed::*;
    let i = match opcode(instr) {
        // R-type instructions
        OP_ARITH => match funct3(instr) {
            F3_0 => match (funct7(instr), split_x0(rd(instr))) {
                (F7_0, X0) => Hint { instr },
                (F7_0, NonZero(rd)) => r_instr!(Add, instr, rd),
                (F7_1, _) => r_instr!(Mul, instr),
                (F7_20, X0) => Hint { instr },
                (F7_20, NonZero(rd)) => r_instr!(Sub, instr, rd),
                _ => Unknown { instr },
            },
            F3_4 => match (funct7(instr), split_x0(rd(instr))) {
                (F7_0, X0) => Hint { instr },
                (F7_0, NonZero(rd)) => r_instr!(Xor, instr, rd),
                (F7_1, _) => r_instr!(Div, instr),
                _ => Unknown { instr },
            },
            F3_6 => match (funct7(instr), split_x0(rd(instr))) {
                (F7_0, X0) => Hint { instr },
                (F7_0, NonZero(rd)) => r_instr!(Or, instr, rd),
                (F7_1, _) => r_instr!(Rem, instr),
                _ => Unknown { instr },
            },
            F3_7 => match (funct7(instr), split_x0(rd(instr))) {
                (F7_0, X0) => Hint { instr },
                (F7_0, NonZero(rd)) => r_instr!(And, instr, rd),
                (F7_1, _) => r_instr!(Remu, instr),
                _ => Unknown { instr },
            },
            F3_1 => match (funct7(instr), split_x0(rd(instr))) {
                (F7_0, X0) => Hint { instr },
                (F7_0, NonZero(rd)) => r_instr!(Sll, instr, rd),
                (F7_1, _) => r_instr!(Mulh, instr),
                _ => Unknown { instr },
            },
            F3_5 => match (funct7(instr), split_x0(rd(instr))) {
                (F7_0, X0) => Hint { instr },
                (F7_0, NonZero(rd)) => r_instr!(Srl, instr, rd),
                (F7_1, _) => r_instr!(Divu, instr),
                (F7_20, X0) => Hint { instr },
                (F7_20, NonZero(rd)) => r_instr!(Sra, instr, rd),
                _ => Unknown { instr },
            },

            F3_2 => match (funct7(instr), split_x0(rd(instr))) {
                (F7_0, X0) => Hint { instr },
                (F7_0, NonZero(rd)) => r_instr!(Slt, instr, rd),
                (F7_1, _) => r_instr!(Mulhsu, instr),
                _ => Unknown { instr },
            },

            F3_3 => match (funct7(instr), split_x0(rd(instr))) {
                (F7_0, X0) => Hint { instr },
                (F7_0, NonZero(rd)) => r_instr!(Sltu, instr, rd),
                (F7_1, _) => r_instr!(Mulhu, instr),
                _ => Unknown { instr },
            },

            _ => Unknown { instr },
        },
        OP_ARITH_W => match funct3(instr) {
            F3_0 => match (funct7(instr), split_x0(rd(instr))) {
                (F7_0, X0) => Hint { instr },
                (F7_0, NonZero(rd)) => r_instr!(Addw, instr, rd),
                (F7_1, _) => r_instr!(Mulw, instr),
                (F7_20, X0) => Hint { instr },
                (F7_20, NonZero(rd)) => r_instr!(Subw, instr, rd),
                _ => Unknown { instr },
            },
            F3_1 => match split_x0(rd(instr)) {
                X0 => Hint { instr },
                NonZero(rd) => r_instr!(Sllw, instr, rd),
            },
            F3_4 => match funct7(instr) {
                F7_1 => r_instr!(Divw, instr),
                _ => Unknown { instr },
            },
            F3_5 => match (funct7(instr), split_x0(rd(instr))) {
                (F7_0, X0) => Hint { instr },
                (F7_0, NonZero(rd)) => r_instr!(Srlw, instr, rd),
                (F7_1, NonZero(_)) => r_instr!(Divuw, instr),
                (F7_20, X0) => Hint { instr },
                (F7_20, NonZero(rd)) => r_instr!(Sraw, instr, rd),
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
        OP_ARITH_I => match (funct3(instr), split_x0(rd(instr))) {
            (F3_0, X0) => match (split_x0(rs1(instr)), i_imm(instr)) {
                (X0, _) | (_, 0) => Hint { instr },
                (rs1, imm) => Addi(instruction::SplitITypeArgs { rd: X0, rs1, imm }),
            },
            (F3_0, rd) => Addi(instruction::SplitITypeArgs {
                rd,
                rs1: split_x0(rs1(instr)),
                imm: i_imm(instr),
            }),
            (F3_4, X0) => Hint { instr },
            (F3_4, NonZero(rd)) => i_instr!(Xori, instr, rd),
            (F3_6, X0) => Hint { instr },
            (F3_6, NonZero(rd)) => i_instr!(Ori, instr, rd),
            (F3_7, X0) => Hint { instr },
            (F3_7, NonZero(rd)) => i_instr!(Andi, instr, rd),
            (F3_1, rd) => match (imm_11_6(instr), rd) {
                // imm[0:5] -> shift amount
                (F7_0, X0) => Hint { instr },
                (F7_0, NonZero(rd)) => i_instr!(Slli, instr, rd),
                _ => Unknown { instr },
            },
            (F3_5, rd) => match (imm_11_6(instr), rd) {
                // imm[6:11] -> type of shift, imm[0:5] -> shift amount
                (F7_0, X0) => Hint { instr },
                (F7_0, NonZero(rd)) => i_instr!(Srli, instr, rd),
                (F7_20, X0) => Hint { instr },
                (F7_20, NonZero(rd)) => InstrCacheable::Srai(NonZeroRdITypeArgs {
                    rd,
                    rs1: rs1(instr),
                    imm: shift_imm(instr),
                }),
                _ => Unknown { instr },
            },
            (F3_2, X0) => Hint { instr },
            (F3_2, NonZero(rd)) => i_instr!(Slti, instr, rd),
            (F3_3, X0) => Hint { instr },
            (F3_3, NonZero(rd)) => i_instr!(Sltiu, instr, rd),
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
        OP_ARITH_IW => match (funct3(instr), split_x0(rd(instr))) {
            (F3_0, X0) => Hint { instr },
            (F3_0, NonZero(rd)) => i_instr!(Addiw, instr, rd),
            (F3_1, rd) => match (imm_11_6(instr), rd) {
                // imm[0:4] -> shift amount
                (F7_0, X0) => Hint { instr },
                (F7_0, NonZero(rd)) => i_instr!(Slliw, instr, rd),
                _ => Unknown { instr },
            },
            (F3_5, rd) => match (imm_11_6(instr), rd) {
                // imm[6:11] -> type of shift, imm[0:4] -> shift amount
                (F7_0, X0) => Hint { instr },
                (F7_0, NonZero(rd)) => i_instr!(Srliw, instr, rd),
                (F7_20, X0) => Hint { instr },
                (F7_20, NonZero(rd)) => i_instr!(Sraiw, instr, rd),
                _ => Unknown { instr },
            },
            _ => Unknown { instr },
        },
        OP_SYNCH => match funct3(instr) {
            F3_0 => match fm(instr) {
                FM_0 => match (bits(instr, 20, 4), bits(instr, 24, 4)) {
                    (0, _) => Hint { instr },
                    (_, 0) => Hint { instr },
                    (_, _) => return Instr::Uncacheable(fence_instr!(Fence, instr)),
                },
                FM_8 => return Instr::Uncacheable(fence_instr!(FenceTso, instr)),
                _ => Unknown { instr },
            },
            F3_1 => return Instr::Uncacheable(InstrUncacheable::FenceI),
            _ => Unknown { instr },
        },
        OP_SYS => match funct3(instr) {
            F3_0 => match funct7(instr) {
                F7_0 => match (rs1_bits(instr), rs2_bits(instr)) {
                    (RS1_0, RS2_0) => Ecall,
                    (RS1_0, RS2_1) => return Instr::Uncacheable(Ebreak),
                    _ => Unknown { instr },
                },
                F7_9 => {
                    return Instr::Uncacheable(SFenceVma {
                        vaddr: rs1(instr),
                        asid: rs2(instr),
                    });
                }
                F7_8 => match (rs1_bits(instr), rs2_bits(instr)) {
                    (RS1_0, RS2_2) => return Instr::Uncacheable(Sret),
                    (RS1_0, RS2_5) => return Instr::Cacheable(Wfi),
                    _ => Unknown { instr },
                },
                F7_24 => match (rs1_bits(instr), rs2_bits(instr)) {
                    (RS1_0, RS2_2) => return Instr::Uncacheable(Mret),
                    _ => Unknown { instr },
                },
                F7_56 => match (rs1_bits(instr), rs2_bits(instr)) {
                    (RS1_0, RS2_2) => return Instr::Uncacheable(Mnret),
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
                F5_2 => match rs2_bits(instr) {
                    RS2_0 => amo_instr!(Lrw, instr),
                    _ => Unknown { instr },
                },
                F5_3 => amo_instr!(Scw, instr),
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
            F3_3 => match funct5(instr) {
                F5_2 => match rs2_bits(instr) {
                    RS2_0 => amo_instr!(Lrd, instr),
                    _ => Unknown { instr },
                },
                F5_3 => amo_instr!(Scd, instr),
                F5_1 => amo_instr!(Amoswapd, instr),
                F5_0 => amo_instr!(Amoaddd, instr),
                F5_4 => amo_instr!(Amoxord, instr),
                F5_12 => amo_instr!(Amoandd, instr),
                F5_8 => amo_instr!(Amoord, instr),
                F5_16 => amo_instr!(Amomind, instr),
                F5_20 => amo_instr!(Amomaxd, instr),
                F5_24 => amo_instr!(Amominud, instr),
                F5_28 => amo_instr!(Amomaxud, instr),
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
        OP_LUI => match split_x0(rd(instr)) {
            X0 => Hint { instr },
            NonZero(rd) => u_instr!(Lui, instr, rd),
        },
        OP_AUIPC => match split_x0(rd(instr)) {
            X0 => Hint { instr },
            NonZero(rd) => u_instr!(Auipc, instr, rd),
        },

        // F/D-type instructions
        OP_FP => match fmt(instr) {
            FMT_S => match (funct5(instr), funct3(instr), rs2_bits_u5(instr)) {
                (F5_0, rounding, rs2_bits) => {
                    f_r_rm_2_instr!(Fadds, instr, rs2_bits, rounding)
                }
                (F5_1, rounding, rs2_bits) => {
                    f_r_rm_2_instr!(Fsubs, instr, rs2_bits, rounding)
                }
                (F5_2, rounding, rs2_bits) => {
                    f_r_rm_2_instr!(Fmuls, instr, rs2_bits, rounding)
                }
                (F5_3, rounding, rs2_bits) => {
                    f_r_rm_2_instr!(Fdivs, instr, rs2_bits, rounding)
                }
                (F5_4, RM_0, rs2_bits) => f_r_instr!(Fsgnjs, instr, rs2_bits),
                (F5_4, RM_1, rs2_bits) => f_r_instr!(Fsgnjns, instr, rs2_bits),
                (F5_4, RM_2, rs2_bits) => f_r_instr!(Fsgnjxs, instr, rs2_bits),
                (F5_5, RM_MIN, rs2_bits) => f_r_instr!(Fmins, instr, rs2_bits),
                (F5_5, RM_MAX, rs2_bits) => f_r_instr!(Fmaxs, instr, rs2_bits),
                (F5_8, rounding, RS2_1_U5) => f_r_rm_1_instr!(Fcvtsd, instr, rounding),
                (F5_11, rounding, RS2_0_U5) => f_r_rm_1_instr!(Fsqrts, instr, rounding),
                (F5_20, RM_EQ, rs2_bits) => f_cmp_instr!(Feqs, instr, rs2_bits),
                (F5_20, RM_LE, rs2_bits) => f_cmp_instr!(Fles, instr, rs2_bits),
                (F5_20, RM_LT, rs2_bits) => f_cmp_instr!(Flts, instr, rs2_bits),
                (F5_24, rounding, RS2_0_U5) => f_r_int_fmt_instr!(Fcvtws, instr, rounding),
                (F5_24, rounding, RS2_1_U5) => f_r_int_fmt_instr!(Fcvtwus, instr, rounding),
                (F5_24, rounding, RS2_2_U5) => f_r_int_fmt_instr!(Fcvtls, instr, rounding),
                (F5_24, rounding, RS2_3_U5) => f_r_int_fmt_instr!(Fcvtlus, instr, rounding),
                (F5_26, rounding, RS2_0_U5) => f_r_fmt_int_instr!(Fcvtsw, instr, rounding),
                (F5_26, rounding, RS2_1_U5) => f_r_fmt_int_instr!(Fcvtswu, instr, rounding),
                (F5_26, rounding, RS2_2_U5) => f_r_fmt_int_instr!(Fcvtsl, instr, rounding),
                (F5_26, rounding, RS2_3_U5) => f_r_fmt_int_instr!(Fcvtslu, instr, rounding),
                (F5_28, RM_0, RS2_0_U5) => FmvXW(FRegToXRegArgs {
                    rd: rd(instr),
                    rs1: rs1_f(instr),
                }),
                (F5_28, RM_1, RS2_0_U5) => FclassS(FRegToXRegArgs {
                    rd: rd(instr),
                    rs1: rs1_f(instr),
                }),
                (F5_30, RM_0, RS2_0_U5) => FmvWX(XRegToFRegArgs {
                    rd: rd_f(instr),
                    rs1: rs1(instr),
                }),
                _ => Unknown { instr },
            },
            FMT_D => match (funct5(instr), funct3(instr), rs2_bits_u5(instr)) {
                (F5_0, rounding, rs2_bits) => {
                    f_r_rm_2_instr!(Faddd, instr, rs2_bits, rounding)
                }
                (F5_1, rounding, rs2_bits) => {
                    f_r_rm_2_instr!(Fsubd, instr, rs2_bits, rounding)
                }
                (F5_2, rounding, rs2_bits) => {
                    f_r_rm_2_instr!(Fmuld, instr, rs2_bits, rounding)
                }
                (F5_3, rounding, rs2_bits) => {
                    f_r_rm_2_instr!(Fdivd, instr, rs2_bits, rounding)
                }
                (F5_4, RM_0, rs2_bits) => f_r_instr!(Fsgnjd, instr, rs2_bits),
                (F5_4, RM_1, rs2_bits) => f_r_instr!(Fsgnjnd, instr, rs2_bits),
                (F5_4, RM_2, rs2_bits) => f_r_instr!(Fsgnjxd, instr, rs2_bits),
                (F5_5, RM_MIN, rs2_bits) => f_r_instr!(Fmind, instr, rs2_bits),
                (F5_5, RM_MAX, rs2_bits) => f_r_instr!(Fmaxd, instr, rs2_bits),
                (F5_8, rounding, RS2_0_U5) => f_r_rm_1_instr!(Fcvtds, instr, rounding),
                (F5_11, rounding, RS2_0_U5) => f_r_rm_1_instr!(Fsqrtd, instr, rounding),
                (F5_20, RM_EQ, rs2_bits) => f_cmp_instr!(Feqd, instr, rs2_bits),
                (F5_20, RM_LE, rs2_bits) => f_cmp_instr!(Fled, instr, rs2_bits),
                (F5_20, RM_LT, rs2_bits) => f_cmp_instr!(Fltd, instr, rs2_bits),
                (F5_24, rounding, RS2_0_U5) => f_r_int_fmt_instr!(Fcvtwd, instr, rounding),
                (F5_24, rounding, RS2_1_U5) => f_r_int_fmt_instr!(Fcvtwud, instr, rounding),
                (F5_24, rounding, RS2_2_U5) => f_r_int_fmt_instr!(Fcvtld, instr, rounding),
                (F5_24, rounding, RS2_3_U5) => f_r_int_fmt_instr!(Fcvtlud, instr, rounding),
                (F5_26, rounding, RS2_0_U5) => f_r_fmt_int_instr!(Fcvtdw, instr, rounding),
                (F5_26, rounding, RS2_1_U5) => f_r_fmt_int_instr!(Fcvtdwu, instr, rounding),
                (F5_26, rounding, RS2_2_U5) => f_r_fmt_int_instr!(Fcvtdl, instr, rounding),
                (F5_26, rounding, RS2_3_U5) => f_r_fmt_int_instr!(Fcvtdlu, instr, rounding),
                (F5_28, RM_0, RS2_0_U5) => FmvXD(FRegToXRegArgs {
                    rd: rd(instr),
                    rs1: rs1_f(instr),
                }),
                (F5_28, RM_1, RS2_0_U5) => FclassD(FRegToXRegArgs {
                    rd: rd(instr),
                    rs1: rs1_f(instr),
                }),
                (F5_30, RM_0, RS2_0_U5) => FmvDX(XRegToFRegArgs {
                    rd: rd_f(instr),
                    rs1: rs1(instr),
                }),
                _ => Unknown { instr },
            },
            _ => Unknown { instr },
        },
        // F/D fused multiply add instructions
        OP_FMADD => match fmt(instr) {
            FMT_S => f_r_rm_3_instr!(Fmadds, instr),
            FMT_D => f_r_rm_3_instr!(Fmaddd, instr),
            _ => Unknown { instr },
        },
        OP_FMSUB => match fmt(instr) {
            FMT_S => f_r_rm_3_instr!(Fmsubs, instr),
            FMT_D => f_r_rm_3_instr!(Fmsubd, instr),
            _ => Unknown { instr },
        },
        OP_FNMSUB => match fmt(instr) {
            FMT_S => f_r_rm_3_instr!(Fnmsubs, instr),
            FMT_D => f_r_rm_3_instr!(Fnmsubd, instr),
            _ => Unknown { instr },
        },
        OP_FNMADD => match fmt(instr) {
            FMT_S => f_r_rm_3_instr!(Fnmadds, instr),
            FMT_D => f_r_rm_3_instr!(Fnmaddd, instr),
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
    };

    Instr::Cacheable(i)
}

const NUM_COMPRESSED_INSTRUCTIONS: usize = (u16::MAX as usize) + 1;

static COMPRESSED_JUMP_TABLE: [Instr; NUM_COMPRESSED_INSTRUCTIONS] = {
    let mut table =
        [Instr::Cacheable(InstrCacheable::Unknown { instr: 0 }); NUM_COMPRESSED_INSTRUCTIONS];
    let mut i = 0;

    while i < u16::MAX {
        table[i as usize] = parse_compressed_instruction_inner(i);
        i += 1;
    }
    table[i as usize] = parse_compressed_instruction_inner(i);

    table
};

macro_rules! cr_instr {
    ($enum_variant:ident, $instr:expr) => {
        $enum_variant(CRTypeArgs {
            rd_rs1: c_rs1p($instr),
            rs2: c_rdp_rs2p($instr),
        })
    };
}

macro_rules! cc_instr {
    ($enum_variant:ident, $imm:expr, $instr:expr) => {
        $enum_variant(CIBTypeArgs {
            rd_rs1: c_rs1p($instr),
            imm: $imm,
        })
    };
}

#[inline(always)]
const fn c_bits(bytes: u16, pos: usize, n: usize) -> u16 {
    (bytes >> pos) & (!0 >> (16 - n))
}

#[inline(always)]
const fn c_rd_rs1(instr: u16) -> XRegister {
    parse_xregister(u5::extract_u16(instr, 7))
}

#[inline(always)]
const fn c_f_rd_rs1(instr: u16) -> FRegister {
    parse_fregister(u5::extract_u16(instr, 7))
}

#[inline(always)]
const fn c_rs2(instr: u16) -> XRegister {
    parse_xregister(u5::extract_u16(instr, 2))
}

#[inline(always)]
const fn c_f_rs2(instr: u16) -> FRegister {
    parse_fregister(u5::extract_u16(instr, 2))
}

/// Encodings for the most used registers for certain compressed instructions
/// See U:C-16.2
#[inline(always)]
const fn c_reg_prime(instr: u16, pos: usize) -> u5 {
    const EIGHT: u5 = u5::new(8);

    u3::extract_u16(instr, pos).widen::<5>().wrapping_add(EIGHT)
}

#[inline(always)]
const fn c_rs1p(instr: u16) -> XRegister {
    parse_xregister(c_reg_prime(instr, 7))
}

#[inline(always)]
const fn c_rs1p_nz(instr: u16) -> NonZeroXRegister {
    // This cannot panic as parsing can only return a register from x8 to x15,
    // which are all non-zero.
    NonZeroXRegister::assert_from(parse_xregister(c_reg_prime(instr, 7)))
}

#[inline(always)]
const fn c_rdp_rs2p(instr: u16) -> XRegister {
    parse_xregister(c_reg_prime(instr, 2))
}

#[inline(always)]
const fn c_rdp_rs2p_nz(instr: u16) -> NonZeroXRegister {
    // This cannot panic as parsing can only return a register from x8 to x15,
    // which are all non-zero.
    NonZeroXRegister::assert_from(parse_xregister(c_reg_prime(instr, 2)))
}

#[inline(always)]
const fn c_f_rdp_rs2p(instr: u16) -> FRegister {
    parse_fregister(c_reg_prime(instr, 2))
}

#[inline(always)]
const fn c_opcode(instr: u16) -> u16 {
    c_bits(instr, 0, 2)
}

#[inline(always)]
const fn c_funct3(instr: u16) -> u16 {
    c_bits(instr, 13, 3)
}

#[inline(always)]
const fn c_q1_10(instr: u16) -> u16 {
    c_bits(instr, 10, 2)
}

#[inline(always)]
const fn c_q1_5(instr: u16) -> u16 {
    c_bits(instr, 5, 2)
}

const fn sign_extend_u16(value: u16, size: usize) -> i64 {
    assert!(size < 16);
    let shift = 16 - size;
    (((value as i16) << shift) >> shift) as i64
}

const fn clw_imm(instr: u16) -> i64 {
    // instr[5] | instr[12:10] | instr[6] | 00
    let res = (u16::bits_subset(instr, 5, 5) << 6)
        | (u16::bits_subset(instr, 12, 10) << 3)
        | (u16::bits_subset(instr, 6, 6) << 2);
    res as i64
}

const fn cld_imm(instr: u16) -> i64 {
    // instr[6:5] | instr[12:10] | 000
    let res = (u16::bits_subset(instr, 6, 5) << 6) | (u16::bits_subset(instr, 12, 10) << 3);
    res as i64
}

const fn ci_imm(instr: u16) -> i64 {
    // instr[12] | instr[6:2]
    let res = (u16::bits_subset(instr, 12, 12) << 5) | u16::bits_subset(instr, 6, 2);
    sign_extend_u16(res, 6)
}

const fn cslli_imm(instr: u16) -> i64 {
    // instr[12] | instr[6:2]
    let res = (u16::bits_subset(instr, 12, 12) << 5) | u16::bits_subset(instr, 6, 2);
    res as i64
}

const fn ci_addi16sp_imm(instr: u16) -> i64 {
    // instr[12] | instr[4:3] | instr[5] | instr[2] | instr[6] | 0000
    let res = (u16::bits_subset(instr, 12, 12) << 9)
        | (u16::bits_subset(instr, 4, 3) << 7)
        | (u16::bits_subset(instr, 5, 5) << 6)
        | (u16::bits_subset(instr, 2, 2) << 5)
        | (u16::bits_subset(instr, 6, 6) << 4);
    sign_extend_u16(res, 10)
}

const fn ci_lwsp_imm(instr: u16) -> i64 {
    // instr[3:2] | instr[12] | instr[6:4] | 00
    let res = (u16::bits_subset(instr, 3, 2) << 6)
        | (u16::bits_subset(instr, 12, 12) << 5)
        | (u16::bits_subset(instr, 6, 4) << 2);
    res as i64
}

const fn ci_ldsp_imm(instr: u16) -> i64 {
    // instr[4:2] | instr[12] | instr[6:5] | 000
    let res = (u16::bits_subset(instr, 4, 2) << 6)
        | (u16::bits_subset(instr, 12, 12) << 5)
        | (u16::bits_subset(instr, 6, 5) << 3);
    res as i64
}

const fn css_swsp_imm(instr: u16) -> i64 {
    // instr[8:7] | instr[12:9] | 00
    let res = (u16::bits_subset(instr, 8, 7) << 6) | (u16::bits_subset(instr, 12, 9) << 2);
    res as i64
}

const fn css_sdsp_imm(instr: u16) -> i64 {
    // instr[9:7] | instr[12:10] | 000
    let res = (u16::bits_subset(instr, 9, 7) << 6) | (u16::bits_subset(instr, 12, 10) << 3);
    res as i64
}

const fn ciw_imm(instr: u16) -> i64 {
    // instr[10:7] | instr[12:11] | instr[5] | instr[6] | 00
    let res = (u16::bits_subset(instr, 10, 7) << 6)
        | (u16::bits_subset(instr, 12, 11) << 4)
        | (u16::bits_subset(instr, 5, 5) << 3)
        | (u16::bits_subset(instr, 6, 6) << 2);
    res as i64
}

const fn cb_imm(instr: u16) -> i64 {
    // instr[12] | instr[6:5] | instr[2] | instr[11:10] | instr[4:3] | 0
    let res = (u16::bits_subset(instr, 12, 12) << 8)
        | (u16::bits_subset(instr, 6, 5) << 6)
        | (u16::bits_subset(instr, 2, 2) << 5)
        | (u16::bits_subset(instr, 11, 10) << 3)
        | (u16::bits_subset(instr, 4, 3) << 1);
    sign_extend_u16(res, 9)
}

const fn cb_shamt_imm(instr: u16) -> i64 {
    // instr[12] | instr[6:2]
    let res = (u16::bits_subset(instr, 12, 12) << 5) | u16::bits_subset(instr, 6, 2);
    res as i64
}

const fn cb_andi_imm(instr: u16) -> i64 {
    // instr[12] | instr[6:2]
    let res = (u16::bits_subset(instr, 12, 12) << 5) | u16::bits_subset(instr, 6, 2);
    sign_extend_u16(res, 6)
}

const fn cj_imm(instr: u16) -> i64 {
    // instr[12] | instr[8] | instr[10:9] | instr[6] | instr[7] | instr[2] | instr[11] | instr[5:3] | 0
    let res = (u16::bits_subset(instr, 12, 12) << 11)
        | (u16::bits_subset(instr, 8, 8) << 10)
        | (u16::bits_subset(instr, 10, 9) << 8)
        | (u16::bits_subset(instr, 6, 6) << 7)
        | (u16::bits_subset(instr, 7, 7) << 6)
        | (u16::bits_subset(instr, 2, 2) << 5)
        | (u16::bits_subset(instr, 11, 11) << 4)
        | (u16::bits_subset(instr, 5, 3) << 1);
    sign_extend_u16(res, 12)
}

const OP_C0: u16 = 0b00;
const OP_C1: u16 = 0b01;
const OP_C2: u16 = 0b10;

const C_F3_0: u16 = 0b000;
const C_F3_1: u16 = 0b001;
const C_F3_2: u16 = 0b010;
const C_F3_3: u16 = 0b011;
const C_F3_4: u16 = 0b100;
const C_F3_5: u16 = 0b101;
const C_F3_6: u16 = 0b110;
const C_F3_7: u16 = 0b111;

const C_Q1_0: u16 = 0b00;
const C_Q1_1: u16 = 0b01;
const C_Q1_2: u16 = 0b10;
const C_Q1_3: u16 = 0b11;

/// Wrapper for splitting x0 from other XRegisters that can
/// be represented by [`NonZeroXRegister`].
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum XRegisterParsed {
    X0,
    NonZero(NonZeroXRegister),
}

/// Convert [`XRegister`] to [`NonZeroXRegister`] when `r != x0`.
pub(crate) const fn split_x0(r: XRegister) -> XRegisterParsed {
    match r {
        x0 => XRegisterParsed::X0,
        r => XRegisterParsed::NonZero(NonZeroXRegister::assert_from(r)),
    }
}

impl Display for XRegisterParsed {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            XRegisterParsed::X0 => write!(f, "zero"),
            XRegisterParsed::NonZero(r) => write!(f, "{}", r),
        }
    }
}

#[inline]
const fn parse_compressed_instruction_inner(instr: u16) -> Instr {
    use InstrCacheable::*;
    use XRegisterParsed::*;
    let i = match c_opcode(instr) {
        OP_C0 => match c_funct3(instr) {
            C_F3_1 => CFld(FLoadArgs {
                rd: c_f_rdp_rs2p(instr),
                rs1: c_rs1p(instr),
                imm: cld_imm(instr),
            }),
            C_F3_0 => match ciw_imm(instr) {
                0 => UnknownCompressed { instr },
                imm => CAddi4spn(CIBTypeArgs {
                    rd_rs1: c_rdp_rs2p(instr),
                    imm,
                }),
            },
            C_F3_2 => CLw(NonZeroITypeArgs {
                rd: c_rdp_rs2p_nz(instr),
                rs1: c_rs1p_nz(instr),
                imm: clw_imm(instr),
            }),
            C_F3_3 => CLd(NonZeroITypeArgs {
                rd: c_rdp_rs2p_nz(instr),
                rs1: c_rs1p_nz(instr),
                imm: cld_imm(instr),
            }),
            C_F3_5 => CFsd(FStoreArgs {
                rs1: c_rs1p(instr),
                rs2: c_f_rdp_rs2p(instr),
                imm: cld_imm(instr),
            }),
            C_F3_6 => CSw(NonZeroSBTypeArgs {
                rs1: c_rs1p_nz(instr),
                rs2: c_rdp_rs2p_nz(instr),
                imm: clw_imm(instr),
            }),
            C_F3_7 => CSd(NonZeroSBTypeArgs {
                rs1: c_rs1p_nz(instr),
                rs2: c_rdp_rs2p_nz(instr),
                imm: cld_imm(instr),
            }),
            _ => UnknownCompressed { instr },
        },
        OP_C1 => match c_funct3(instr) {
            C_F3_0 => match (ci_imm(instr), split_x0(c_rd_rs1(instr))) {
                // "C.ADDI is only valid when rd != x0 and nzimm != 0.
                // The code points with rd == x0 and nzimm == 0 encode the C.NOP instruction;
                // the remaining code points with nzimm != 0 encode HINTs."
                (0, X0) => CNop,
                (0, _) | (_, X0) => HintCompressed { instr },
                (imm, NonZero(rd_rs1)) => CAddi(CIBNZTypeArgs { rd_rs1, imm }),
            },
            C_F3_1 => match (ci_imm(instr), split_x0(c_rd_rs1(instr))) {
                (_, X0) => UnknownCompressed { instr },
                (imm, NonZero(rd_rs1)) => CAddiw(CIBNZTypeArgs { rd_rs1, imm }),
            },
            C_F3_2 => match split_x0(c_rd_rs1(instr)) {
                X0 => HintCompressed { instr },
                NonZero(rd_rs1) => CLi(CIBNZTypeArgs {
                    rd_rs1,
                    imm: ci_imm(instr),
                }),
            },
            C_F3_3 => {
                if u16::bits_subset(instr, 6, 2) == 0 && !u16::bit(instr, 12) {
                    return Instr::Cacheable(UnknownCompressed { instr });
                };
                match (split_x0(c_rd_rs1(instr)), ci_imm(instr)) {
                    (NonZero(NonZeroXRegister::x2), _) => CAddi16sp(CJTypeArgs {
                        imm: ci_addi16sp_imm(instr),
                    }),
                    (_, 0) => UnknownCompressed { instr },
                    (X0, _) => HintCompressed { instr },
                    (NonZero(rd_rs1), _) => CLui(CIBNZTypeArgs {
                        rd_rs1,
                        imm: ci_imm(instr) << 12,
                    }),
                }
            }
            // RV64C declares that `shamt == 0` here is for HINTS.
            C_F3_4 => match (c_q1_10(instr), cb_shamt_imm(instr)) {
                (C_Q1_0, 0) => HintCompressed { instr },
                (C_Q1_0, imm) => cc_instr!(CSrli, imm, instr),
                (C_Q1_1, 0) => HintCompressed { instr },
                (C_Q1_1, imm) => cc_instr!(CSrai, imm, instr),
                (C_Q1_2, _) => cc_instr!(CAndi, cb_andi_imm(instr), instr),
                (C_Q1_3, _) => match (u16::bit(instr, 12), c_q1_5(instr)) {
                    (false, C_Q1_0) => cr_instr!(CSub, instr),
                    (false, C_Q1_1) => cr_instr!(CXor, instr),
                    (false, C_Q1_2) => cr_instr!(COr, instr),
                    (false, C_Q1_3) => cr_instr!(CAnd, instr),
                    (true, C_Q1_0) => cr_instr!(CSubw, instr),
                    (true, C_Q1_1) => cr_instr!(CAddw, instr),
                    _ => UnknownCompressed { instr },
                },
                _ => UnknownCompressed { instr },
            },
            C_F3_5 => CJ(CJTypeArgs { imm: cj_imm(instr) }),

            C_F3_6 => cc_instr!(CBeqz, cb_imm(instr), instr),
            C_F3_7 => cc_instr!(CBnez, cb_imm(instr), instr),
            _ => UnknownCompressed { instr },
        },
        OP_C2 => match c_funct3(instr) {
            C_F3_0 => match (cslli_imm(instr), split_x0(c_rd_rs1(instr))) {
                (_, X0) | (0, _) => HintCompressed { instr },
                (imm, NonZero(rd_rs1)) => CSlli(CIBNZTypeArgs { rd_rs1, imm }),
            },
            C_F3_1 => CFldsp(CIBDTypeArgs {
                rd_rs1: c_f_rd_rs1(instr),
                imm: ci_ldsp_imm(instr),
            }),
            C_F3_2 => match split_x0(c_rd_rs1(instr)) {
                X0 => UnknownCompressed { instr },
                NonZero(rd_rs1) => CLwsp(CIBNZTypeArgs {
                    rd_rs1,
                    imm: ci_lwsp_imm(instr),
                }),
            },
            C_F3_3 => match split_x0(c_rd_rs1(instr)) {
                X0 => UnknownCompressed { instr },
                NonZero(rd_rs1) => CLdsp(CIBNZTypeArgs {
                    rd_rs1,
                    imm: ci_ldsp_imm(instr),
                }),
            },

            C_F3_4 => match (
                u16::bit(instr, 12),
                split_x0(c_rd_rs1(instr)),
                split_x0(c_rs2(instr)),
            ) {
                (true, X0, X0) => return Instr::Uncacheable(InstrUncacheable::CEbreak),
                (_, X0, X0) => UnknownCompressed { instr },
                (_, X0, NonZero(_)) => HintCompressed { instr },
                (true, NonZero(rs1), X0) => CJalr(CRJTypeArgs { rs1 }),
                (true, NonZero(rd_rs1), NonZero(rs2)) => CAdd(CNZRTypeArgs { rd_rs1, rs2 }),
                (false, NonZero(rs1), X0) => CJr(CRJTypeArgs { rs1 }),
                (false, NonZero(rd_rs1), NonZero(rs2)) => CMv(CNZRTypeArgs { rd_rs1, rs2 }),
            },
            C_F3_5 => CFsdsp(CSSDTypeArgs {
                rs2: c_f_rs2(instr),
                imm: css_sdsp_imm(instr),
            }),
            C_F3_6 => CSwsp(CSSTypeArgs {
                rs2: c_rs2(instr),
                imm: css_swsp_imm(instr),
            }),
            C_F3_7 => CSdsp(CSSTypeArgs {
                rs2: c_rs2(instr),
                imm: css_sdsp_imm(instr),
            }),
            _ => UnknownCompressed { instr },
        },
        _ => UnknownCompressed { instr },
    };

    Instr::Cacheable(i)
}

/// Parse a compressed instruction from a u16.
#[inline(always)]
pub fn parse_compressed_instruction(bytes: u16) -> Instr {
    COMPRESSED_JUMP_TABLE[bytes as usize]
}

/// Attempt to parse `bytes` into an instruction. If `bytes` encodes a 2-byte
/// compressed instruction, parse it immediately. If it encodes a 4-byte
/// uncompressed instruction, request 2 extra bytes via `more`.
#[inline(always)]
pub fn parse<E>(bytes: u16, more: impl FnOnce() -> Result<u16, E>) -> Result<Instr, E> {
    if bytes & 0b11 != 0b11 {
        Ok(parse_compressed_instruction(bytes))
    } else {
        let upper = more()?;
        let combined = ((upper as u32) << 16) | (bytes as u32);
        Ok(parse_uncompressed_instruction(combined))
    }
}

/// Whether the given bytes correspond to a compressed instruction.
#[inline]
pub const fn is_compressed(bytes: u16) -> bool {
    bytes & 0b11 != 0b11
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
    use std::collections::HashMap;

    use super::XRegisterParsed::*;
    use super::instruction::CsrArgs;
    use super::instruction::Instr;
    use super::instruction::InstrCacheable::*;
    use super::instruction::NonZeroRdITypeArgs;
    use super::instruction::SBTypeArgs;
    use super::instruction::UJTypeArgs;
    use super::parse_block;
    use crate::machine_state::csregisters::CSRegister::mcause;
    use crate::machine_state::registers::NonZeroXRegister;
    use crate::machine_state::registers::XRegister::*;
    use crate::parser::NonZeroRdUJTypeArgs;
    use crate::parser::SplitITypeArgs;
    use crate::parser::instruction::CIBNZTypeArgs;
    use crate::parser::instruction::CIBTypeArgs;
    use crate::parser::instruction::InstrUncacheable;
    use crate::parser::parse_compressed_instruction;
    use crate::parser::parse_compressed_instruction_inner;

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
            Instr::Cacheable(Jal(UJTypeArgs { rd: x0, imm: 0x50 })),
            Instr::Cacheable(Csrrs(CsrArgs {
                rd: x30,
                rs1: x0,
                csr: mcause,
            })),
            Instr::Cacheable(Addi(SplitITypeArgs {
                rd: NonZero(NonZeroXRegister::x31),
                rs1: X0,
                imm: 0x8,
            })),
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
            Instr::Cacheable(Addi(SplitITypeArgs {
                rd: NonZero(NonZeroXRegister::x3),
                rs1: X0,
                imm: 21,
            })),
            Instr::Cacheable(CLui(CIBNZTypeArgs {
                rd_rs1: NonZeroXRegister::x8,
                imm: 0x1 << 12,
            })),
            Instr::Cacheable(Addiw(NonZeroRdITypeArgs {
                rd: NonZeroXRegister::x8,
                rs1: x8,
                imm: 564,
            })),
            Instr::Cacheable(CSlli(CIBNZTypeArgs {
                rd_rs1: NonZeroXRegister::x8,
                imm: 4,
            })),
            Instr::Cacheable(Lui(NonZeroRdUJTypeArgs {
                rd: NonZeroXRegister::x7,
                imm: 0x12 << 12,
            })),
            Instr::Cacheable(Addiw(NonZeroRdITypeArgs {
                rd: NonZeroXRegister::x7,
                rs1: x7,
                imm: 832,
            })),
            Instr::Cacheable(Bne(SBTypeArgs {
                rs1: x8,
                rs2: x7,
                imm: 288,
            })),
        ];
        let instructions = parse_block(&bytes);
        assert_eq!(instructions, expected)
    }

    // Misaligned buffer, last byte ignored
    #[test]
    fn test_3() {
        let bytes: [u8; 5] = [0x1, 0x5, 0x64, 0x1b, 0x4];
        let expected = [
            Instr::Cacheable(HintCompressed {
                instr: u16::from_le_bytes([0x1, 0x5]),
            }),
            Instr::Cacheable(CAddi4spn(CIBTypeArgs {
                rd_rs1: x9,
                imm: 444,
            })),
        ];
        let instructions = parse_block(&bytes);
        assert_eq!(instructions, expected)
    }

    // Expected uncompressed instruction, only 2 bytes left
    #[test]
    fn test_4() {
        let bytes: [u8; 6] = [0x6f, 0x0, 0x0, 0x5, 0x73, 0x2f];
        let expected = [Instr::Cacheable(Jal(UJTypeArgs { rd: x0, imm: 0x50 }))];
        let instructions = parse_block(&bytes);
        assert_eq!(instructions, expected)
    }

    // A valid `slli	x10,x10,0x1f` instruction, followed by an invalid one,
    // in which one of the upper 6 bits (bit 29) has been set to 1.
    #[test]
    fn test_5() {
        let bytes: [u8; 8] = [0x13, 0x15, 0xf5, 0x01, 0x13, 0x15, 0xf5, 0x21];
        let expected = [
            Instr::Cacheable(Slli(NonZeroRdITypeArgs {
                rd: NonZeroXRegister::x10,
                rs1: x10,
                imm: 31,
            })),
            Instr::Cacheable(Unknown {
                instr: u32::from_le_bytes([0x13, 0x15, 0xf5, 0x21]),
            }),
        ];
        let instructions = parse_block(&bytes);
        assert_eq!(instructions, expected)
    }

    // Instructions not covered in integration tests
    #[test]
    fn test_6() {
        let bytes: [u8; 4] = [0x73, 0x00, 0x20, 0x70];
        let expected = [Instr::Uncacheable(InstrUncacheable::Mnret)];
        let instructions = parse_block(&bytes);
        assert_eq!(instructions, expected)
    }

    // Ensure the u16 jump table is initialised correctly.
    #[test]
    fn parser_compressed() {
        for i in 0..=u16::MAX {
            assert_eq!(
                parse_compressed_instruction(i),
                parse_compressed_instruction_inner(i)
            );
        }
    }

    // Ensure parsing correctly is 1:2:1.
    #[test]
    fn test_unparse_compressed() {
        let mut reverse = HashMap::new();

        for i in 0..=u16::MAX {
            reverse.insert(parse_compressed_instruction(i), i);
        }

        for i in 0..=u16::MAX {
            assert_eq!(
                Some(&i),
                reverse.get(&parse_compressed_instruction(i)),
                "Failed {i} {}",
                parse_compressed_instruction(i)
            );
        }
    }

    // Check a HINT encoding for each uncompressed opcode.
    #[test]
    fn test_uncompressed_hints() {
        let bytes: [u8; 120] = [
            0x37, 0x80, 0x33, 0x33, // LUI X0, 3355520
            0x17, 0x80, 0x33, 0x33, // AUIPC X0, 3355520
            0x13, 0x00, 0xA0, 0x36, // ADDI, X0, X0, 874
            0x13, 0xB0, 0xAC, 0x36, // ORI, X0, X25, 874
            0x13, 0xC0, 0xAC, 0x36, // XORI, X0, X25, 874
            0x1B, 0x80, 0x19, 0x00, // ADDIW X0, X11, 1
            0x33, 0x80, 0x31, 0x00, // ADD X0, X3, X3
            0x33, 0x80, 0x5A, 0x41, // SUB X0, X21, X21
            0x33, 0xB0, 0x31, 0x00, // AND X0, X3, X3
            0x33, 0xC0, 0x31, 0x00, // OR X0, X3, X3
            0x33, 0xD0, 0x31, 0x00, // XOR X0, X3, X3
            0x33, 0xE0, 0x31, 0x00, // SLL X0, X3, X3
            0x33, 0xF0, 0x31, 0x00, // SRL X0, X3, X3
            0x33, 0xD0, 0x5A, 0x41, // SRA X0, X21, X21
            0x37, 0x00, 0xC6, 0x00, // ADDW X0, X6, X6
            0x37, 0x00, 0xC6, 0x40, // SUBW X0, X6, X6
            0x37, 0x10, 0xC6, 0x00, // SLLW X0, X6, X6
            0x37, 0x20, 0xC6, 0x00, // SRLW X0, X6, X6
            0x37, 0x10, 0xC6, 0x40, // SRAW X0, X6, X6
            0x0F, 0x00, 0x00, 0x0F, // FENCE F, 0
            0x13, 0x20, 0xA0, 0x36, // SLTI, X0, X0, 874
            0x13, 0x30, 0xA0, 0x36, // SLTIU, X0, X0, 874
            0x13, 0x10, 0x44, 0x01, // SLLI X0, X8, 333
            0x13, 0x50, 0x44, 0x01, // SRLI X0, X8, 333
            0x13, 0x50, 0x44, 0x41, // SRAI X0, X8, 333
            0x1B, 0x90, 0x19, 0x00, // SLLIW X0, X11, 1
            0x1B, 0xD0, 0x19, 0x00, // SRLIW X0, X11, 1
            0x1B, 0xD0, 0x19, 0x40, // SRAIW X0, X11, 1
            0x33, 0x90, 0x31, 0x00, // SLT X0, X3, X3
            0x33, 0xA0, 0x31, 0x00, // SLTU X0, X3, X3
        ];
        let expected = [
            Instr::Cacheable(Hint { instr: 0x33338037 }),
            Instr::Cacheable(Hint { instr: 0x33338017 }),
            Instr::Cacheable(Hint { instr: 0x36A00013 }),
            Instr::Cacheable(Hint { instr: 0x36ACB013 }),
            Instr::Cacheable(Hint { instr: 0x36ACC013 }),
            Instr::Cacheable(Hint { instr: 0x0019801B }),
            Instr::Cacheable(Hint { instr: 0x00318033 }),
            Instr::Cacheable(Hint { instr: 0x415A8033 }),
            Instr::Cacheable(Hint { instr: 0x0031B033 }),
            Instr::Cacheable(Hint { instr: 0x0031C033 }),
            Instr::Cacheable(Hint { instr: 0x0031D033 }),
            Instr::Cacheable(Hint { instr: 0x0031E033 }),
            Instr::Cacheable(Hint { instr: 0x0031F033 }),
            Instr::Cacheable(Hint { instr: 0x415AD033 }),
            Instr::Cacheable(Hint { instr: 0x00C60037 }),
            Instr::Cacheable(Hint { instr: 0x40C60037 }),
            Instr::Cacheable(Hint { instr: 0x00C61037 }),
            Instr::Cacheable(Hint { instr: 0x00C62037 }),
            Instr::Cacheable(Hint { instr: 0x40C61037 }),
            Instr::Cacheable(Hint { instr: 0x0F00000F }),
            Instr::Cacheable(Hint { instr: 0x36A02013 }),
            Instr::Cacheable(Hint { instr: 0x36A03013 }),
            Instr::Cacheable(Hint { instr: 0x01441013 }),
            Instr::Cacheable(Hint { instr: 0x01445013 }),
            Instr::Cacheable(Hint { instr: 0x41445013 }),
            Instr::Cacheable(Hint { instr: 0x0019901B }),
            Instr::Cacheable(Hint { instr: 0x0019D01B }),
            Instr::Cacheable(Hint { instr: 0x4019D01B }),
            Instr::Cacheable(Hint { instr: 0x00319033 }),
            Instr::Cacheable(Hint { instr: 0x0031A033 }),
        ];
        let instructions = parse_block(&bytes);
        assert_eq!(instructions, expected);
    }

    // Check a HINT encoding for each compressed opcode.
    #[test]
    fn test_compressed_hints() {
        let bytes: [u8; 16] = [
            0x01, 0x02, // C.ADDI x4, 0
            0x01, 0x40, // C.LI x0, 0
            0x51, 0x60, // C.LUI X0, 20
            0x06, 0x80, // C.MV x0, x1
            0x06, 0x90, // C.ADD x0, x1
            0x02, 0x00, // C.SLLI x0 0
            0x01, 0x81, // C.SRLI x6 0
            0x01, 0x87, // C.SRAI x6 0
        ];
        let expected = [
            Instr::Cacheable(HintCompressed { instr: 0x0201 }),
            Instr::Cacheable(HintCompressed { instr: 0x4001 }),
            Instr::Cacheable(HintCompressed { instr: 0x6051 }),
            Instr::Cacheable(HintCompressed { instr: 0x8006 }),
            Instr::Cacheable(HintCompressed { instr: 0x9006 }),
            Instr::Cacheable(HintCompressed { instr: 0x0002 }),
            Instr::Cacheable(HintCompressed { instr: 0x8101 }),
            Instr::Cacheable(HintCompressed { instr: 0x8701 }),
        ];
        let instructions = parse_block(&bytes);
        assert_eq!(instructions, expected);
    }
}
