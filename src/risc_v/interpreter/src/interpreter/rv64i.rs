// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_64_I extension for RISC-V
//!
//! Chapter 5 - Unprivileged spec

use crate::machine_state::registers::{XRegister, XRegisters};
use crate::state_backend as backend;

impl<M> XRegisters<M>
where
    M: backend::Manager,
{
    /// `ADDIW` I-type instruction
    pub fn run_addiw(&mut self, imm: i64, rs1: XRegister, rd: XRegister) {
        // Perform addition only on the lower 32 bits, ignoring the upper 32 bits.
        // We do not need to explicitly truncate for the lower bits since wrapping_add
        // has the same semantics & result on the lower 32 bits irrespective of bit width
        let rval = self.read(rs1);
        let result = rval.wrapping_add(imm as u64);
        // Truncate result to use only the lower 32 bits, then sign-extend to 64 bits.
        let result = result as i32 as u64;
        self.write(rd, result);
    }

    /// `SLLI` I-type instruction
    ///
    /// Shift left logically
    /// (zeros are shifted in the lower bits)
    ///
    /// NOTE: RV64I makes the shift amount (shamt) be 6 bits wide for SLLI
    pub fn run_slli(&mut self, imm: i64, rs1: XRegister, rd: XRegister) {
        // SLLI encoding allows to consider the whole immediate as the shift amount
        self.write(rd, self.read(rs1) << imm)
    }

    /// `SRLI` I-type instruction
    ///
    /// Shift right logically
    /// (zeros are shifted in the upper bits)
    ///
    /// NOTE: RV64I makes the shift amount (shamt) be 6 bits wide for SRLI
    pub fn run_srli(&mut self, imm: i64, rs1: XRegister, rd: XRegister) {
        // SLLI encoding allows to consider the whole immediate as the shift amount
        self.write(rd, self.read(rs1) >> imm)
    }

    /// `SRAI` I-type instruction
    ///
    /// Shift right arithmetically
    /// (sign-bits are shifted in the upper bits)
    ///
    /// NOTE: RV64I makes the shift amount (shamt) be 6 bits wide for SRAI
    pub fn run_srai(&mut self, imm: i64, rs1: XRegister, rd: XRegister) {
        // SRAI encoding has bit imm[10] set, so need to mask the shift amount
        let sh_amt = imm & 0b11_1111;

        // Right shift on i64 is an arithmetic shift
        let result = (self.read(rs1) as i64) >> sh_amt;
        // i64 as u64 is a no-op
        self.write(rd, result as u64)
    }

    /// `SLLIW` I-type instruction
    ///
    /// Shift left logically only on lower 32 bits
    /// (zeros are shifted in the lower bits)
    pub fn run_slliw(&mut self, imm: i64, rs1: XRegister, rd: XRegister) {
        // SLLIW encoding allows to consider the whole immediate as the shift amount
        // Since we are shifting left, we can operate directly on u64
        let result = self.read(rs1) << imm;

        // Even though SLLIW operates only on lowest 32 bits, RISC-V convention
        // mandates for register values to be saved in a sign-extended manner
        // Note: u64 as i32 as u64 will sign-extend the lowest 32 bits
        self.write(rd, result as i32 as u64)
    }

    /// `SRLIW` I-type instruction
    ///
    /// Shift right logically only on lower 32 bits
    /// (zeros are shifted in the upper bits)
    pub fn run_srliw(&mut self, imm: i64, rs1: XRegister, rd: XRegister) {
        // SRLIW encoding allows to consider the whole immediate as the shift amount
        let result = (self.read(rs1) as u32) >> imm;

        // Even though SRLIW operates only on lowest 32 bits, RISC-V convention
        // mandates for register values to be saved in a sign-extended manner
        // Note: u32 as i32 as u64 will sign-extend the lowest 32 bits
        self.write(rd, result as i32 as u64)
    }

    /// `SRAIW` I-type instruction
    ///
    /// Shift right arithmetically only on lower 32 bits
    /// (sign-bits are shifted in the upper bits)
    pub fn run_sraiw(&mut self, imm: i64, rs1: XRegister, rd: XRegister) {
        // SRAIW encoding has bit imm[10] set, so need to mask the shift amount
        let sh_amt = imm & 0b1_1111;
        // Right shift on i32 is an arithmetic shift
        let result = (self.read(rs1) as i32) >> sh_amt;

        // Even though SRAIW operates only on lowest 32 bits, RISC-V convention
        // mandates for register values to be saved in a sign-extended manner
        // Note: i32 as u64 will sign-extend the lowest 32 bits
        self.write(rd, result as u64)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        backend_test,
        machine_state::{
            registers::{a0, a1, t0},
            HartState, HartStateLayout,
        },
    };
    use crate::{create_backend, create_state};
    use proptest::{arbitrary::any, prop_assert_eq, proptest};

    backend_test!(test_addiw, F, {
        proptest!(|(
            imm in any::<i64>(),
            reg_val in any::<i64>())|
        {
            let mut backend = create_backend!(HartStateLayout, F);
            let mut state = create_state!(HartState, F, backend);

            state.xregisters.write(a0, reg_val as u64);
            state.xregisters.run_addiw(imm, a0, a1);
            // check against wrapping addition performed on the lowest 32 bits
            let r_val = reg_val as u32;
            let i_val = imm as u32;
            prop_assert_eq!(
                state.xregisters.read(a1),
                r_val.wrapping_add(i_val) as i32 as i64 as u64
            )
        });
    });

    macro_rules! test_shift_instr {
        ($state:ident, $shift_fn:tt, $imm:expr,
            $rs1:ident, $r1_val:expr,
            $rd:ident, $expected_val:expr
        ) => {
            $state.xregisters.write($rs1, $r1_val);
            $state.xregisters.$shift_fn($imm, $rs1, $rd);
            let new_val = $state.xregisters.read($rd);
            assert_eq!(new_val, $expected_val);
        };
    }

    backend_test!(test_shift, F, {
        let mut backend = create_backend!(HartStateLayout, F);
        let mut state = create_state!(HartState, F, backend);

        // imm = 0
        test_shift_instr!(state, run_slli, 0, a0, 0x1234_ABEF, a1, 0x1234_ABEF);
        test_shift_instr!(state, run_srli, 0, a0, 0x1234_ABEF, a0, 0x1234_ABEF);
        test_shift_instr!(
            state,
            run_srai,
            0,
            a0,
            0xFFFF_DEAD_1234_ABEF,
            a1,
            0xFFFF_DEAD_1234_ABEF
        );

        // small imm (< 32))
        test_shift_instr!(state, run_slli, 20, a0, 0x1234_ABEF, a1, 0x1_234A_BEF0_0000);
        test_shift_instr!(state, run_srli, 10, a0, 0x44_1234_ABEF, a1, 0x1104_8D2A);
        test_shift_instr!(
            state,
            run_srli,
            14,
            t0,
            -1_i64 as u64,
            a0,
            0x0003_FFFF_FFFF_FFFF
        );
        test_shift_instr!(
            state,
            run_srai,
            10,
            a0,
            0xFFFF_F0FF_FFF0_FF00,
            a0,
            0xFFFF_FFFC_3FFF_FC3F
        );

        // big imm (>= 32))
        test_shift_instr!(
            state,
            run_slli,
            40,
            a0,
            0x1234_ABEF,
            a0,
            0x34AB_EF00_0000_0000
        );
        test_shift_instr!(state, run_srli, 40, a0, 0x1234_ABEF, a0, 0x0);
        test_shift_instr!(
            state,
            run_srai,
            40,
            a0,
            0x8000_FAFF_1234_ABEF,
            a1,
            0xFFFF_FFFF_FF80_00FA
        );
    });

    backend_test!(test_shift_w, F, {
        let mut backend = create_backend!(HartStateLayout, F);
        let mut state = create_state!(HartState, F, backend);

        // imm = 0
        test_shift_instr!(state, run_slliw, 0, a0, 0xEDDD_1234_ABEF, a1, 0x1234_ABEF);
        test_shift_instr!(state, run_srliw, 0, a0, 0x1234_ABEF, a0, 0x1234_ABEF);
        test_shift_instr!(
            state,
            run_sraiw,
            0,
            a0,
            0xFFFF_DEAD_1234_ABEF,
            a1,
            0x1234_ABEF
        );

        // small imm (< 32))
        test_shift_instr!(
            state,
            run_slliw,
            20,
            a0,
            0x1F0B_FFFF,
            a0,
            0xFFFF_FFFF_FFF0_0000
        );
        test_shift_instr!(state, run_srliw, 10, a0, 0x44_1234_ABEF, a1, 0x4_8D2A);
        test_shift_instr!(state, run_srliw, 16, t0, -1_i64 as u64, a0, 0xFFFF);
        test_shift_instr!(
            state,
            run_sraiw,
            10,
            a0,
            0xFFFF_F0FF_FFF0_FF00,
            a0,
            0xFFFF_FFFF_FFFF_FC3F
        );

        // big imm (>= 32) are not allowed for <shift>w operations
        test_shift_instr!(
            state,
            run_slliw,
            31,
            a0,
            0x1234_ABEF,
            a0,
            0xFFFF_FFFF_8000_0000
        );
        test_shift_instr!(state, run_srliw, 31, a0, 0x8234_ABEF, a1, 0x1);
        test_shift_instr!(
            state,
            run_sraiw,
            31,
            a0,
            0x8234_ABEF,
            a1,
            0xFFFF_FFFF_FFFF_FFFF
        );
    });
}
