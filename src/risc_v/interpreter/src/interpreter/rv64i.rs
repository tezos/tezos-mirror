// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_64_I extension for RISC-V
//!
//! Chapter 5 - Unprivileged spec

use crate::{
    machine_state::{
        bus::{main_memory::MainMemoryLayout, Addressable, OutOfBounds},
        registers::{XRegister, XRegisters},
        MachineState,
    },
    state_backend as backend,
    traps::Exception,
};

impl<M> XRegisters<M>
where
    M: backend::Manager,
{
    /// `ADDIW` I-type instruction
    ///
    /// Add `imm` to val(rs1) only on lowest 32 bits
    /// and store the sign-extended result in `rd`
    pub fn run_addiw(&mut self, imm: i64, rs1: XRegister, rd: XRegister) {
        // We do not need to explicitly truncate for the lower bits since wrapping_add
        // has the same semantics & result on the lower 32 bits irrespective of bit width
        let rval = self.read(rs1);
        let result = rval.wrapping_add(imm as u64);
        // Truncate result to use only the lower 32 bits, then sign-extend to 64 bits.
        let result = result as i32 as u64;
        self.write(rd, result);
    }

    /// `ADDW` R-type instruction
    ///
    /// Perform val(rs1) + val(rs2) but only on lowest 32 bits
    /// and store the sign-extended result in `rd`
    pub fn run_addw(&mut self, rs1: XRegister, rs2: XRegister, rd: XRegister) {
        // We do not need to explicitly truncate for the lower bits since wrapping_add
        // has the same semantics & result on the lower 32 bits irrespective of bit width
        let lhs = self.read(rs1);
        let rhs = self.read(rs2);
        // Truncate result to use only the lower 32 bits, then sign-extend to 64 bits.
        let result = lhs.wrapping_add(rhs) as i32 as u64;
        self.write(rd, result)
    }

    /// `SUBW` R-type instruction
    ///
    /// Perform val(rs1) - val(rs2) but only on lowest 32 bits
    /// and store the sign-extended result in `rd`
    pub fn run_subw(&mut self, rs1: XRegister, rs2: XRegister, rd: XRegister) {
        // We do not need to explicitly truncate for the lower bits since wrapping_sub
        // has the same semantics & result on the lower 32 bits irrespective of bit width
        let lhs = self.read(rs1);
        let rhs = self.read(rs2);
        // Truncate result to use only the lower 32 bits, then sign-extend to 64 bits.
        let result = lhs.wrapping_sub(rhs) as i32 as u64;
        self.write(rd, result)
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

    /// `SLL` R-type instruction
    ///
    /// Shift left logically bits in rs1 by shift_amount = val(rs2)\[5:0\]
    /// saving the result in rd
    /// (zeros are shifted in the lower bits)
    pub fn run_sll(&mut self, rs1: XRegister, rs2: XRegister, rd: XRegister) {
        // Get last 6 bits of rs2
        let sh_amt = self.read(rs2) & 0b11_1111;
        let result = self.read(rs1) << sh_amt;
        self.write(rd, result)
    }

    /// `SRL` R-type instruction
    ///
    /// Shift right logically bits in rs1 by shift_amount = val(rs2)\[5:0\]
    /// saving the result in rd
    /// (zeros are shifted in the upper bits)
    pub fn run_srl(&mut self, rs1: XRegister, rs2: XRegister, rd: XRegister) {
        // Get last 6 bits of rs2
        let sh_amt = self.read(rs2) & 0b11_1111;
        let result = self.read(rs1) >> sh_amt;
        self.write(rd, result)
    }

    /// `SRA` R-type instruction
    ///
    /// Shift right arithmeticallly bits in rs1 by shift_amount = val(rs2)\[5:0\]
    /// saving the result in rd
    /// (sign-bits are shifted in the upper bits)
    pub fn run_sra(&mut self, rs1: XRegister, rs2: XRegister, rd: XRegister) {
        // Get last 6 bits of rs2
        let sh_amt = self.read(rs2) & 0b11_1111;
        // Right shift on i64 is an arithmetic shift
        let result = (self.read(rs1) as i64) >> sh_amt;
        // i64 as u64 is a no-op
        self.write(rd, result as u64)
    }

    /// `SLLW` R-type instruction
    ///
    /// Shift left logically only lowest 32 bits in rs1
    /// by shift_amount = val(rs2)\[4:0\] saving the result in rd
    /// (zeros are shifted in the lower bits)
    pub fn run_sllw(&mut self, rs1: XRegister, rs2: XRegister, rd: XRegister) {
        // Get last 5 bits of rs2
        let sh_amt = self.read(rs2) & 0b1_1111;
        // Since we are shifting left, we can operate directly on u64
        let result = self.read(rs1) << sh_amt;
        // Even though SLLW operates only on lowest 32 bits, RISC-V convention
        // mandates for register values to be saved in a sign-extended manner
        // Note: u64 as i32 as u64 will sign-extend the lowest 32 bits
        self.write(rd, result as i32 as u64)
    }

    /// `SRLW` R-type instruction
    ///
    /// Shift right logically only the lowest 32 bits in rs1
    /// by shift_amount = val(rs2)\[4:0\] saving the result in rd
    /// (zeros are shifted in the upper bits)
    pub fn run_srlw(&mut self, rs1: XRegister, rs2: XRegister, rd: XRegister) {
        // Get last 5 bits of rs2
        let sh_amt = self.read(rs2) & 0b1_1111;
        let result = (self.read(rs1) as u32) >> sh_amt;
        // Even though SRLW operates only on lowest 32 bits, RISC-V convention
        // mandates for register values to be saved in a sign-extended manner
        // Note: u32 as i32 as u64 will sign-extend the lowest 32 bits
        self.write(rd, result as i32 as u64)
    }

    /// `SRAW` R-type instruction
    ///
    /// Shift right arithmeticallly only the lowest 32 bits bits in rs1
    /// by shift_amount = val(rs1)\[4:0\] saving the result in rd
    /// (sign-bits are shifted in the upper bits)
    pub fn run_sraw(&mut self, rs1: XRegister, rs2: XRegister, rd: XRegister) {
        // Get last 5 bits of rs2
        let sh_amt = self.read(rs2) & 0b1_1111;
        // Right shift on i32 is an arithmetic shift
        let result = (self.read(rs1) as i32) >> sh_amt;
        // Even though SRAIW operates only on lowest 32 bits, RISC-V convention
        // mandates for register values to be saved in a sign-extended manner
        // Note: i32 as u64 will sign-extend the lowest 32 bits
        self.write(rd, result as u64)
    }
}

impl<ML, M> MachineState<ML, M>
where
    ML: MainMemoryLayout,
    M: backend::Manager,
{
    /// Generic read function for loading `mem::size_of<T>` bytes from address val(rs1) + imm
    fn read_from_bus<T: backend::Elem>(&self, imm: i64, rs1: XRegister) -> Result<T, Exception> {
        let address = self.hart.xregisters.read(rs1).wrapping_add(imm as u64);
        self.bus
            .read(address)
            .map_err(|_: OutOfBounds| Exception::LoadAccessFault)
    }

    /// `LD` I-type instruction
    ///
    /// Loads a double-word (8 bytes) starting from address given by: val(rs1) + imm
    pub fn run_ld(&mut self, imm: i64, rs1: XRegister, rd: XRegister) -> Result<(), Exception> {
        let value: i64 = self.read_from_bus(imm, rs1)?;
        // i64 as u64 is a no-op
        self.hart.xregisters.write(rd, value as u64);
        Ok(())
    }

    /// `LW` I-type instruction
    ///
    /// Loads a word (4 bytes) starting from address given by: val(rs1) + imm
    /// NOTE: For RV64I the value is sign-extended to 64 bits
    pub fn run_lw(&mut self, imm: i64, rs1: XRegister, rd: XRegister) -> Result<(), Exception> {
        let value: i32 = self.read_from_bus(imm, rs1)?;
        // i32 as u64 sign-extends to 64 bits
        self.hart.xregisters.write(rd, value as u64);
        Ok(())
    }

    /// `LH` I-type instruction
    ///
    /// Loads a half-word (2 bytes) starting from address given by: val(rs1) + imm
    /// sign-extending the result
    pub fn run_lh(&mut self, imm: i64, rs1: XRegister, rd: XRegister) -> Result<(), Exception> {
        let value: i16 = self.read_from_bus(imm, rs1)?;
        // i16 as u64 sign-extends to 64 bits
        self.hart.xregisters.write(rd, value as u64);
        Ok(())
    }

    /// `LB` I-type instruction
    ///
    /// Loads a single byte from the address given by: val(rs1) + imm
    /// sign-extending the result
    pub fn run_lb(&mut self, imm: i64, rs1: XRegister, rd: XRegister) -> Result<(), Exception> {
        let value: i8 = self.read_from_bus(imm, rs1)?;
        // i8 as u64 sign-extends to 64 bits
        self.hart.xregisters.write(rd, value as u64);
        Ok(())
    }

    /// `LWU` I-type instruction
    ///
    /// Loads a word (4 bytes) starting from address given by: val(rs1) + imm
    /// zero-extending the result
    pub fn run_lwu(&mut self, imm: i64, rs1: XRegister, rd: XRegister) -> Result<(), Exception> {
        let value: u32 = self.read_from_bus(imm, rs1)?;
        // u32 as u64 zero-extends to 64 bits
        self.hart.xregisters.write(rd, value as u64);
        Ok(())
    }

    /// `LHU` I-type instruction
    ///
    /// Loads a half-word (2 bytes) starting from address given by: val(rs1) + imm
    /// zero-extending the result
    pub fn run_lhu(&mut self, imm: i64, rs1: XRegister, rs2: XRegister) -> Result<(), Exception> {
        let value: u16 = self.read_from_bus(imm, rs1)?;
        // u16 as u64 zero-extends to 64 bits
        self.hart.xregisters.write(rs2, value as u64);
        Ok(())
    }

    /// `LBU` I-type instruction
    ///
    /// Loads a single byte from the address given by: val(rs1) + imm
    /// zero-extending the result
    pub fn run_lbu(&mut self, imm: i64, rs1: XRegister, rs2: XRegister) -> Result<(), Exception> {
        let value: u8 = self.read_from_bus(imm, rs1)?;
        // u8 as u64 zero-extends to 64 bits
        self.hart.xregisters.write(rs2, value as u64);
        Ok(())
    }

    /// Generic store-operation for writing `mem::size_of<T>` bytes starting at address val(rs1) + imm
    fn write_to_bus<T: backend::Elem>(
        &mut self,
        imm: i64,
        rs1: XRegister,
        value: T,
    ) -> Result<(), Exception> {
        let address = self.hart.xregisters.read(rs1).wrapping_add(imm as u64);
        self.bus
            .write(address, value)
            .map_err(|_: OutOfBounds| Exception::StoreAccessFault)
    }

    /// `SD` S-type instruction
    ///
    /// Stores a double-word (8 bytes from rs2) to the address starting at: val(rs1) + imm
    pub fn run_sd(&mut self, imm: i64, rs1: XRegister, rs2: XRegister) -> Result<(), Exception> {
        let value: u64 = self.hart.xregisters.read(rs2);
        self.write_to_bus(imm, rs1, value)
    }

    /// `SW` S-type instruction
    ///
    /// Stores a word (lowest 4 bytes from rs2) to the address starting at: val(rs1) + imm
    pub fn run_sw(&mut self, imm: i64, rs1: XRegister, rs2: XRegister) -> Result<(), Exception> {
        let value: u64 = self.hart.xregisters.read(rs2);
        // u64 as u32 is truncated, getting the lowest 32 bits
        self.write_to_bus(imm, rs1, value as u32)
    }

    /// `SH` S-type instruction
    ///
    /// Stores a half-word (lowest 2 bytes from rs2) to the address starting at: val(rs1) + imm
    pub fn run_sh(&mut self, imm: i64, rs1: XRegister, rs2: XRegister) -> Result<(), Exception> {
        let value: u64 = self.hart.xregisters.read(rs2);
        // u64 as u16 is truncated, getting the lowest 16 bits
        self.write_to_bus(imm, rs1, value as u16)
    }

    /// `SB` S-type instruction
    ///
    /// Stores a byte (lowest 1 byte from rs2) to the address starting at: val(rs1) + imm
    pub fn run_sb(&mut self, imm: i64, rs1: XRegister, rs2: XRegister) -> Result<(), Exception> {
        let value: u64 = self.hart.xregisters.read(rs2);
        // u64 as u8 is truncated, getting the lowest 8 bits
        self.write_to_bus(imm, rs1, value as u8)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        backend_test, create_backend, create_state,
        machine_state::{
            bus::{devices::DEVICES_ADDRESS_SPACE_LENGTH, main_memory::tests::T1K},
            hart_state::{HartState, HartStateLayout},
            registers::{a0, a1, a2, a3, a4, t0, t1, t2, t3, t4},
            MachineState, MachineStateLayout,
        },
        traps::Exception,
    };
    use proptest::{arbitrary::any, prop_assert, prop_assert_eq, proptest};

    backend_test!(test_add_w, F, {
        proptest!(|(
            imm in any::<i64>(),
            reg_val in any::<i64>())|
        {
            let mut backend = create_backend!(HartStateLayout, F);
            let mut state = create_state!(HartState, F, backend);

            state.xregisters.write(a0, reg_val as u64);
            state.xregisters.write(t0, imm as u64);
            state.xregisters.run_addiw(imm, a0, a1);
            // check against wrapping addition performed on the lowest 32 bits
            let r_val = reg_val as u32;
            let i_val = imm as u32;
            prop_assert_eq!(
                state.xregisters.read(a1),
                r_val.wrapping_add(i_val) as i32 as i64 as u64
            );
            state.xregisters.run_addw(a0, t0, a2);
            prop_assert_eq!(
                state.xregisters.read(a2),
                r_val.wrapping_add(i_val) as i32 as i64 as u64
            );
        });
    });

    backend_test!(test_sub_w, F, {
        proptest!(|(
            v1 in any::<i64>(),
            v2 in any::<i64>())|
        {
            let mut backend = create_backend!(HartStateLayout, F);
            let mut state = create_state!(HartState, F, backend);

            state.xregisters.write(t0, v1 as u64);
            state.xregisters.write(a0, v2 as u64);
            state.xregisters.run_subw(t0, a0, a1);
            // check against wrapping subtraction performed on the lowest 32 bits
            let v1_u32 = v1 as u32;
            let v2_u32 = v2 as u32;
            prop_assert_eq!(
                state.xregisters.read(a1),
                v1_u32.wrapping_sub(v2_u32) as i32 as i64 as u64
            );
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

    macro_rules! test_shift_reg_instr {
        ($state:ident, $shift_fn:tt,
            $rs2:ident, $r2_val:expr,
            $rs1:ident, $r1_val:expr,
            $rd:ident, $expected_val:expr
        ) => {
            $state.xregisters.write($rs2, $r2_val);
            $state.xregisters.write($rs1, $r1_val);
            $state.xregisters.$shift_fn($rs1, $rs2, $rd);
            let new_val = $state.xregisters.read($rd);
            assert_eq!(new_val, $expected_val);
        };
    }

    macro_rules! test_both_shift_instr {
        ($state:ident, $shift_fn_imm:tt, $shift_fn_reg:tt,
            $rs2:ident, $r2_val:expr,
            $rs1:ident, $r1_val:expr,
            $rd:ident, $expected_val:expr
        ) => {
            test_shift_instr!(
                $state,
                $shift_fn_imm,
                $r2_val,
                $rs1,
                $r1_val,
                $rd,
                $expected_val
            );
            test_shift_reg_instr!(
                $state,
                $shift_fn_reg,
                $rs2,
                $r2_val,
                $rs1,
                $r1_val,
                $rd,
                $expected_val
            );
        };
    }

    backend_test!(test_shift, F, {
        let mut backend = create_backend!(HartStateLayout, F);
        let mut state = create_state!(HartState, F, backend);

        // imm = 0
        test_both_shift_instr!(
            state,
            run_slli,
            run_sll,
            t0,
            0,
            a0,
            0x1234_ABEF,
            a1,
            0x1234_ABEF
        );
        test_both_shift_instr!(
            state,
            run_srli,
            run_srl,
            t1,
            0,
            a0,
            0x1234_ABEF,
            a0,
            0x1234_ABEF
        );
        test_both_shift_instr!(
            state,
            run_srai,
            run_sra,
            t3,
            0,
            a0,
            0xFFFF_DEAD_1234_ABEF,
            a1,
            0xFFFF_DEAD_1234_ABEF
        );

        // small imm (< 32))
        test_both_shift_instr!(
            state,
            run_slli,
            run_sll,
            a2,
            20,
            a0,
            0x1234_ABEF,
            a1,
            0x1_234A_BEF0_0000
        );
        test_both_shift_instr!(
            state,
            run_srli,
            run_srl,
            a2,
            10,
            a0,
            0x44_1234_ABEF,
            a1,
            0x1104_8D2A
        );
        test_both_shift_instr!(
            state,
            run_srli,
            run_srl,
            a2,
            14,
            t0,
            -1_i64 as u64,
            a0,
            0x0003_FFFF_FFFF_FFFF
        );
        test_both_shift_instr!(
            state,
            run_srai,
            run_sra,
            t0,
            10,
            a0,
            0xFFFF_F0FF_FFF0_FF00,
            a0,
            0xFFFF_FFFC_3FFF_FC3F
        );

        // big imm (>= 32))
        test_both_shift_instr!(
            state,
            run_slli,
            run_sll,
            t0,
            40,
            a0,
            0x1234_ABEF,
            a0,
            0x34AB_EF00_0000_0000
        );
        test_both_shift_instr!(state, run_srli, run_srl, a1, 40, a0, 0x1234_ABEF, a0, 0x0);
        test_both_shift_instr!(
            state,
            run_srai,
            run_sra,
            a2,
            40,
            a0,
            0x8000_FAFF_1234_ABEF,
            a1,
            0xFFFF_FFFF_FF80_00FA
        );

        // Use same register for shift and source
        test_shift_reg_instr!(
            state,
            run_sll,
            a1,
            0b1001_0101,
            a1,
            0b1001_0101,
            a2,
            0x12A0_0000
        );
        // Use same register for shift and destination
        test_shift_reg_instr!(
            state,
            run_sll,
            a1,
            0b1001_0101,
            a2,
            0b1101_0101,
            a1,
            0x1AA0_0000
        );
        // Use same register for shift, source and destination
        test_shift_reg_instr!(
            state,
            run_sll,
            a1,
            0b1101_0101,
            a1,
            0b1101_0101,
            a1,
            0x1AA0_0000
        );
    });

    backend_test!(test_shift_w, F, {
        let mut backend = create_backend!(HartStateLayout, F);
        let mut state = create_state!(HartState, F, backend);

        // imm = 0
        test_both_shift_instr!(
            state,
            run_slliw,
            run_sllw,
            t0,
            0,
            a0,
            0xEDDD_1234_ABEF,
            a1,
            0x1234_ABEF
        );
        test_both_shift_instr!(
            state,
            run_srliw,
            run_srlw,
            t0,
            0,
            a0,
            0x1234_ABEF,
            a0,
            0x1234_ABEF
        );
        test_both_shift_instr!(
            state,
            run_sraiw,
            run_sraw,
            a2,
            0,
            a0,
            0xFFFF_DEAD_1234_ABEF,
            a1,
            0x1234_ABEF
        );

        // small imm (< 32))
        test_both_shift_instr!(
            state,
            run_slliw,
            run_sllw,
            a3,
            20,
            a0,
            0x1F0B_FFFF,
            a0,
            0xFFFF_FFFF_FFF0_0000
        );
        test_both_shift_instr!(
            state,
            run_srliw,
            run_srlw,
            t0,
            10,
            a0,
            0x44_1234_ABEF,
            a1,
            0x4_8D2A
        );
        test_both_shift_instr!(
            state,
            run_srliw,
            run_srlw,
            a1,
            16,
            t0,
            -1_i64 as u64,
            a0,
            0xFFFF
        );
        test_both_shift_instr!(
            state,
            run_sraiw,
            run_sraw,
            a1,
            10,
            a0,
            0xFFFF_F0FF_FFF0_FF00,
            a0,
            0xFFFF_FFFF_FFFF_FC3F
        );

        // big imm (>= 32) are not allowed for <shift>w operations
        test_both_shift_instr!(
            state,
            run_slliw,
            run_sllw,
            t0,
            31,
            a0,
            0x1234_ABEF,
            a0,
            0xFFFF_FFFF_8000_0000
        );
        test_both_shift_instr!(state, run_srliw, run_srlw, t0, 31, a0, 0x8234_ABEF, a1, 0x1);
        test_both_shift_instr!(
            state,
            run_sraiw,
            run_sraw,
            t2,
            31,
            a0,
            0x8234_ABEF,
            a1,
            0xFFFF_FFFF_FFFF_FFFF
        );

        // Use same register for shift and source
        test_shift_reg_instr!(
            state,
            run_sllw,
            a1,
            0b1001_0101,
            a1,
            0b1001_0101,
            a2,
            0x12A0_0000
        );
        // Use same register for shift and destination
        test_shift_reg_instr!(
            state,
            run_sllw,
            a1,
            0b1001_0101,
            a2,
            0b1101_0101,
            a1,
            0x1AA0_0000
        );
        // Use same register for shift, source and destination
        // sign-extend with ones to 64 bits
        test_shift_reg_instr!(
            state,
            run_sllw,
            a1,
            0b0100_1101_0101,
            a1,
            0b0100_1101_0101,
            a1,
            0xFFFF_FFFF_9AA0_0000
        );
    });

    backend_test!(test_load_store, F, {
        proptest!(|(
            v_1 in any::<u8>(),
            v_2 in any::<u16>(),
            v_3 in any::<u32>(),
            v_4 in any::<u64>(),
        )|
        {
            let mut backend = create_backend!(MachineStateLayout<T1K>, F);
            let mut state = create_state!(MachineState, MachineStateLayout<T1K>, F, backend, T1K);

            let mut perform_test = |offset: u64, signed: bool| -> Result<(), Exception> {
                // Save test values v_i in registers ai
                state.hart.xregisters.write(a4, v_4);
                state.hart.xregisters.write(a3, v_3 as u64);
                state.hart.xregisters.write(a2, v_2 as u64);
                state.hart.xregisters.write(a1, v_1 as u64);

                // t0 will hold the "global" offset of all loads / stores we are going to make
                state.hart.xregisters.write(t0, offset);

                // Perform the stores
                state.run_sb(14, t0, a1)?;
                state.run_sw(8, t0, a3)?;
                state.run_sh(12, t0, a2)?;
                state.run_sd(0, t0, a4)?;

                match signed {
                    true => {
                        state.run_ld(0, t0, t4)?;
                        state.run_lw(8, t0, t3)?;
                        state.run_lh(12, t0, t2)?;
                        state.run_lb(14, t0, t1)?;
                        assert_eq!(state.hart.xregisters.read(t4), v_4);
                        // Converting the expected result we are also checking the sign-extension behaviour
                        assert_eq!(state.hart.xregisters.read(t3), v_3 as i32 as u64);
                        assert_eq!(state.hart.xregisters.read(t2), v_2 as i16 as u64);
                        assert_eq!(state.hart.xregisters.read(t1), v_1 as i8 as u64);
                        Ok(())
                    },
                    false => {
                        state.run_ld(0, t0, t4)?;
                        state.run_lwu(8, t0, t3)?;
                        state.run_lhu(12, t0, t2)?;
                        state.run_lbu(14, t0, t1)?;
                        assert_eq!(state.hart.xregisters.read(t4), v_4);
                        // Converting the expected result we are also checking the sign-extension behaviour
                        assert_eq!(state.hart.xregisters.read(t3), v_3 as u64);
                        assert_eq!(state.hart.xregisters.read(t2), v_2 as u64);
                        assert_eq!(state.hart.xregisters.read(t1), v_1 as u64);
                        Ok(())
                    },
                }?;
                Ok(())
            };

            let invalid_offset = DEVICES_ADDRESS_SPACE_LENGTH - 1024;
            let aligned_offset = DEVICES_ADDRESS_SPACE_LENGTH + 512;
            let misaligned_offset = DEVICES_ADDRESS_SPACE_LENGTH + 513;

            // Out of bounds loads / stores
            prop_assert!(perform_test(invalid_offset, true).is_err_and(|e|
                e == Exception::StoreAccessFault
            ));
            // Aligned loads / stores
            prop_assert!(perform_test(aligned_offset, true).is_ok());
            // Unaligned loads / stores
            prop_assert!(perform_test(misaligned_offset, true).is_ok());

            // Out of bounds loads / stores
            prop_assert!(perform_test(invalid_offset, false).is_err_and(|e|
                e == Exception::StoreAccessFault
            ));
            // Aligned loads / stores
            prop_assert!(perform_test(aligned_offset, false).is_ok());
            // Unaligned loads / stores
            prop_assert!(perform_test(misaligned_offset, false).is_ok());
        });
    });
}
