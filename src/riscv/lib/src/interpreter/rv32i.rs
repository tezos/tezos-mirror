// SPDX-FileCopyrightText: 2023-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_32_I extension for RISC-V
//!
//! Chapter 2 - Unprivileged spec

use crate::{
    machine_state::{
        MachineCoreState, ProgramCounterUpdate,
        hart_state::HartState,
        main_memory::{Address, MainMemoryLayout},
        mode::Mode,
        registers::{NonZeroXRegister, XRegister, XRegisters},
    },
    parser::instruction::{FenceSet, InstrWidth},
    state_backend::{self as backend},
    traps::Exception,
};

impl<M> XRegisters<M>
where
    M: backend::ManagerReadWrite,
{
    /// Add `imm` to val(rs1) and store the result in `rd`
    ///
    /// Relevant RISC-V opcodes:
    /// - `ADDI`
    /// - `C.ADDI`
    /// - `C.ADDI4SPN`
    /// - `C.ADDI16SP`
    pub fn run_addi(&mut self, imm: i64, rs1: NonZeroXRegister, rd: NonZeroXRegister) {
        // Return the lower XLEN (64 bits in our case) bits of the addition
        // Irrespective of sign, the result is the same, casting to u64 for addition
        let rval = self.read_nz(rs1);
        let result = rval.wrapping_add(imm as u64);
        self.write_nz(rd, result)
    }

    /// Perform [`val(rs1) - val(rs2)`] and store the result in `rd`
    ///
    /// Relevant RISC-V opcodes:
    /// - SUB
    /// - C.SUB
    pub fn run_sub(&mut self, rs1: NonZeroXRegister, rs2: NonZeroXRegister, rd: NonZeroXRegister) {
        let lhs = self.read_nz(rs1);
        let rhs = self.read_nz(rs2);
        // Wrapped subtraction in two's complement behaves the same for signed and unsigned
        let result = lhs.wrapping_sub(rhs);
        self.write_nz(rd, result)
    }

    /// `LUI` U-type instruction
    ///
    /// Set the upper 20 bits of the `rd` register with the `U-type` formatted immediate `imm`
    pub fn run_lui(&mut self, imm: i64, rd: NonZeroXRegister) {
        // Being a `U-type` operation, the immediate is correctly formatted
        // (lower 12 bits cleared and the value is sign-extended)
        self.write_nz(rd, imm as u64);
    }

    /// Saves in `rd` the bitwise AND between the value in `rs1` and `imm`
    ///
    /// Relevant RISC-V opcodes:
    /// - `ANDI`
    /// - `C.ANDI`
    pub fn run_andi(&mut self, imm: i64, rs1: NonZeroXRegister, rd: NonZeroXRegister) {
        let result = self.read_nz(rs1) & (imm as u64);
        self.write_nz(rd, result)
    }

    /// Saves in `rd` the bitwise OR between the value in `rs1` and `imm`
    ///
    /// Relevant RISC-V opcodes:
    /// - `ORI`
    pub fn run_ori(&mut self, imm: i64, rs1: NonZeroXRegister, rd: NonZeroXRegister) {
        let result = self.read_nz(rs1) | (imm as u64);
        self.write_nz(rd, result)
    }

    /// Saves in `rd` the bitwise XOR between the value in `rs1` and `imm`
    ///
    /// Relevant RISC-V opcodes:
    /// - `XORI`
    pub fn run_xori(&mut self, imm: i64, rs1: NonZeroXRegister, rd: NonZeroXRegister) {
        let result = self.read_nz(rs1) ^ (imm as u64);
        self.write_nz(rd, result)
    }

    /// Saves in `rd` the bitwise AND between the value in `rs1` and `rs2`
    ///
    /// Relevant RISC-V opcodes:
    /// - `AND`
    /// - `C.AND`
    pub fn run_and(&mut self, rs1: NonZeroXRegister, rs2: NonZeroXRegister, rd: NonZeroXRegister) {
        let result = self.read_nz(rs1) & self.read_nz(rs2);
        self.write_nz(rd, result)
    }

    /// Saves in `rd` the bitwise OR between the value in `rs1` and `rs2`
    ///
    /// Relevant RISC-V opcodes:
    /// - `OR`
    /// - `C.OR`
    pub fn run_or(&mut self, rs1: NonZeroXRegister, rs2: NonZeroXRegister, rd: NonZeroXRegister) {
        let result = self.read_nz(rs1) | self.read_nz(rs2);
        self.write_nz(rd, result)
    }

    /// Saves in `rd` the bitwise XOR between the value in `rs1` and `imm`
    ///
    /// Relevant RISC-V opcodes:
    /// - `XOR`
    /// - `C.XOR`
    pub fn run_xor(&mut self, rs1: NonZeroXRegister, rs2: NonZeroXRegister, rd: NonZeroXRegister) {
        let result = self.read_nz(rs1) ^ self.read_nz(rs2);
        self.write_nz(rd, result)
    }

    /// `SLTI` I-type instruction
    ///
    /// Places the value 1 in `rd` if val(rs1) is less than the immediate
    /// when treated as signed integers, 0 otherwise
    pub fn run_slti(&mut self, imm: i64, rs1: XRegister, rd: NonZeroXRegister) {
        let result = if (self.read(rs1) as i64) < imm { 1 } else { 0 };
        self.write_nz(rd, result)
    }

    /// `SLTIU` I-type instruction
    ///
    /// Places the value 1 in `rd` if val(rs1) is less than the immediate
    /// when treated as unsigned integers, 0 otherwise
    pub fn run_sltiu(&mut self, imm: i64, rs1: XRegister, rd: NonZeroXRegister) {
        let result = if self.read(rs1) < (imm as u64) { 1 } else { 0 };
        self.write_nz(rd, result)
    }

    /// `SLT` R-type instruction
    ///
    /// Places the value 1 in `rd` if val(rs1) < val(rs2)
    /// when treated as signed integers, 0 otherwise
    pub fn run_slt(&mut self, rs1: XRegister, rs2: XRegister, rd: NonZeroXRegister) {
        let result = if (self.read(rs1) as i64) < (self.read(rs2) as i64) {
            1
        } else {
            0
        };
        self.write_nz(rd, result)
    }

    /// `SLTU` R-type instruction
    ///
    /// Places the value 1 in `rd` if val(rs1) < val(rs2)
    /// when treated as unsigned integers, 0 otherwise
    pub fn run_sltu(&mut self, rs1: XRegister, rs2: XRegister, rd: NonZeroXRegister) {
        let result = if self.read(rs1) < self.read(rs2) {
            1
        } else {
            0
        };
        self.write_nz(rd, result)
    }
}

impl<M> HartState<M>
where
    M: backend::ManagerReadWrite,
{
    /// `AUIPC` U-type instruction
    pub fn run_auipc(&mut self, imm: i64, rd: NonZeroXRegister) {
        // U-type immediates have bits [31:12] set and the lower 12 bits zeroed.
        let rval = self.pc.read().wrapping_add(imm as u64);
        self.xregisters.write_nz(rd, rval);
    }

    /// `EBREAK` instruction
    pub fn run_ebreak(&self) -> Exception {
        Exception::Breakpoint
    }

    /// `ECALL` instruction
    pub fn run_ecall(&self) -> Exception {
        match self.mode.read() {
            Mode::User => Exception::EnvCallFromUMode,
            Mode::Supervisor => Exception::EnvCallFromSMode,
            Mode::Machine => Exception::EnvCallFromMMode,
        }
    }

    /// `JALR` I-type instruction (note: uncompressed variant)
    ///
    /// Instruction mis-aligned will never be thrown because we allow C extension
    ///
    /// Always returns the target address (val(rs1) + imm)
    pub fn run_jalr(&mut self, imm: i64, rs1: XRegister, rd: XRegister) -> Address {
        // The return address to be saved in rd
        let return_address = self.pc.read().wrapping_add(InstrWidth::Uncompressed as u64);

        // The target address is obtained by adding the sign-extended
        // 12-bit I-immediate to the register rs1, then setting
        // the least-significant bit of the result to zero
        let target_address = self.xregisters.read(rs1).wrapping_add(imm as u64) & !1;

        self.xregisters.write(rd, return_address);

        target_address
    }

    /// Store the next instruction address in `rd` and jump to the target address.
    /// Always returns the target address (current program counter + imm)
    pub fn run_jal(&mut self, imm: i64, rd: NonZeroXRegister, width: InstrWidth) -> Address {
        let current_pc = self.pc.read();

        // Save the address after jump instruction into rd
        let return_address = current_pc.wrapping_add(width as u64);
        self.xregisters.write_nz(rd, return_address);

        current_pc.wrapping_add(imm as u64)
    }

    /// Performs a conditional ( `val(rs1) == val(rs2)` ) control transfer.
    /// If condition met, the offset is sign-extended and added to the pc to form the branch
    /// target address that is then set, otherwise indicates to proceed to the next instruction.
    ///
    /// Relevant RISC-V opcodes:
    /// - `BEQ`
    /// - `C.BEQZ`
    pub fn run_beq(
        &mut self,
        imm: i64,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> ProgramCounterUpdate {
        let current_pc = self.pc.read();

        if self.xregisters.read_nz(rs1) == self.xregisters.read_nz(rs2) {
            ProgramCounterUpdate::Set(current_pc.wrapping_add(imm as u64))
        } else {
            ProgramCounterUpdate::Next(width)
        }
    }

    /// Performs a conditional ( `val(rs1) != val(rs2)` ) control transfer.
    /// If condition met, the offset is sign-extended and added to the pc to form the branch
    /// target address that is then set, otherwise indicates to proceed to the next instruction.
    ///
    /// Relevant RISC-V opcodes:
    /// - `BNE`
    /// - `C.BNEZ`
    pub fn run_bne(
        &mut self,
        imm: i64,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> ProgramCounterUpdate {
        let current_pc = self.pc.read();

        if self.xregisters.read_nz(rs1) != self.xregisters.read_nz(rs2) {
            ProgramCounterUpdate::Set(current_pc.wrapping_add(imm as u64))
        } else {
            ProgramCounterUpdate::Next(width)
        }
    }

    /// `BGE` B-type instruction
    ///
    /// Sets branching address if rs1 is greater than or equal to rs2 in signed comparison,
    /// otherwise proceeds to the next instruction address
    pub fn run_bge(
        &mut self,
        imm: i64,
        rs1: XRegister,
        rs2: XRegister,
        width: InstrWidth,
    ) -> ProgramCounterUpdate {
        let current_pc = self.pc.read();

        let lhs = self.xregisters.read(rs1) as i64;
        let rhs = self.xregisters.read(rs2) as i64;

        if lhs >= rhs {
            ProgramCounterUpdate::Set(current_pc.wrapping_add(imm as u64))
        } else {
            ProgramCounterUpdate::Next(width)
        }
    }

    /// `BGEU` B-type instruction
    ///
    /// Sets branching address if rs1 is greater than or equal to rs2 in unsigned comparison,
    /// otherwise proceeds to the next instruction address
    pub fn run_bgeu(
        &mut self,
        imm: i64,
        rs1: XRegister,
        rs2: XRegister,
        width: InstrWidth,
    ) -> ProgramCounterUpdate {
        let current_pc = self.pc.read();

        let lhs = self.xregisters.read(rs1);
        let rhs = self.xregisters.read(rs2);

        if lhs >= rhs {
            ProgramCounterUpdate::Set(current_pc.wrapping_add(imm as u64))
        } else {
            ProgramCounterUpdate::Next(width)
        }
    }

    /// `BLT` B-type instruction
    ///
    /// Sets branching address if rs1 is lower than rs2 in signed comparison,
    /// otherwise proceeds to the next instruction address
    pub fn run_blt(
        &mut self,
        imm: i64,
        rs1: XRegister,
        rs2: XRegister,
        width: InstrWidth,
    ) -> ProgramCounterUpdate {
        let current_pc = self.pc.read();

        let lhs = self.xregisters.read(rs1) as i64;
        let rhs = self.xregisters.read(rs2) as i64;

        if lhs < rhs {
            ProgramCounterUpdate::Set(current_pc.wrapping_add(imm as u64))
        } else {
            ProgramCounterUpdate::Next(width)
        }
    }

    /// `BLTU` B-type instruction
    ///
    /// Sets branching address if rs1 is lower than rs2 in unsigned comparison,
    /// otherwise proceeds to the next instruction address
    pub fn run_bltu(
        &mut self,
        imm: i64,
        rs1: XRegister,
        rs2: XRegister,
        width: InstrWidth,
    ) -> ProgramCounterUpdate {
        let current_pc = self.pc.read();

        let lhs = self.xregisters.read(rs1);
        let rhs = self.xregisters.read(rs2);

        if lhs < rhs {
            ProgramCounterUpdate::Set(current_pc.wrapping_add(imm as u64))
        } else {
            ProgramCounterUpdate::Next(width)
        }
    }
}

impl<ML, M> MachineCoreState<ML, M>
where
    ML: MainMemoryLayout,
    M: backend::ManagerBase,
{
    /// `FENCE` I-Type instruction
    ///
    /// Orders Device I/O, Memory R/W operations. For all harts, for all instructions in the successor sets, instructions in the predecessor sets are visible.
    /// NOTE: Since our interpreter is single-threaded (only one hart), the `FENCE` instruction is a no-op
    #[inline(always)]
    pub fn run_fence(&self, _pred: FenceSet, _succ: FenceSet) {
        // no-op
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        backend_test, create_state,
        interpreter::i::run_add,
        machine_state::{
            MachineCoreState, MachineCoreStateLayout, ProgramCounterUpdate,
            csregisters::{
                CSRRepr, CSRegister,
                xstatus::{MPPValue, MStatus, SPPValue},
            },
            hart_state::{HartState, HartStateLayout},
            main_memory::{Address, tests::T1K},
            mode::Mode,
            registers::{
                XRegisters, XRegistersLayout, a0, a1, a2, a3, a4, fa0, nz, t0, t1, t2, t3,
            },
        },
        parser::instruction::{FenceSet, InstrWidth},
        traps::Exception,
    };
    use proptest::{
        prelude::{any, prop},
        prop_assert_eq, prop_assume, proptest,
    };

    backend_test!(test_add_sub, F, {
        let imm_rs1_rd_res = [
            (0_i64, 0_u64, nz::t3, 0_u64),
            (0, 0xFFF0_0420, nz::t2, 0xFFF0_0420),
            (-1, 0, nz::t4, 0xFFFF_FFFF_FFFF_FFFF),
            (
                1_000_000,
                -123_000_987_i64 as u64,
                nz::a2,
                -122_000_987_i64 as u64,
            ),
            (1_000_000, 123_000_987, nz::a2, 124_000_987),
            (
                -1,
                -321_000_000_000_i64 as u64,
                nz::a1,
                -321_000_000_001_i64 as u64,
            ),
        ];

        for (imm, rs1, rd, res) in imm_rs1_rd_res {
            let mut state = create_state!(MachineCoreState, MachineCoreStateLayout<T1K>, F, T1K);

            state.hart.xregisters.write(a0, rs1);
            state.hart.xregisters.write(t0, imm as u64);
            state.hart.xregisters.run_addi(imm, nz::a0, rd);
            assert_eq!(state.hart.xregisters.read_nz(rd), res);
            run_add(&mut state, nz::a0, nz::t0, nz::a0);
            assert_eq!(state.hart.xregisters.read(a0), res);
            // test sub with: res - imm = rs1 and res - rs1 = imm
            state.hart.xregisters.write(a0, res);
            state.hart.xregisters.write(t0, imm as u64);
            state.hart.xregisters.run_sub(nz::a0, nz::t0, nz::a1);
            assert_eq!(state.hart.xregisters.read(a1), rs1);
            // now rs1 is in register a1
            state.hart.xregisters.run_sub(nz::a0, nz::a1, nz::a1);
            assert_eq!(state.hart.xregisters.read(a1), imm as u64);
        }
    });

    backend_test!(test_auipc, F, {
        let pc_imm_res_rd = [
            (0, 0, 0, nz::a2),
            (0, 0xFF_FFF0_0000, 0xFF_FFF0_0000, nz::a0),
            (0x000A_AAAA, 0xFF_FFF0_0000, 0xFF_FFFA_AAAA, nz::a1),
            (0xABCD_AAAA_FBC0_D3FE, 0, 0xABCD_AAAA_FBC0_D3FE, nz::t5),
            (0xFFFF_FFFF_FFF0_0000, 0x10_0000, 0, nz::t6),
        ];

        for (init_pc, imm, res, rd) in pc_imm_res_rd {
            let mut state = create_state!(HartState, F);

            // U-type immediate only has upper 20 bits set, the lower 12 being set to 0
            assert_eq!(imm, ((imm >> 20) & 0xF_FFFF) << 20);

            state.pc.write(init_pc);
            state.run_auipc(imm, rd);

            let read_pc = state.xregisters.read_nz(rd);

            assert_eq!(read_pc, res);
        }
    });

    macro_rules! test_branch_instr {
        ($state:ident, $branch_fn:tt, $imm:expr,
         $rs1:ident, $r1_val:expr,
         $rs2:ident, $r2_val:expr, $width:expr,
         $init_pc:ident, $expected_pc:expr
        ) => {
            $state.pc.write($init_pc);
            $state.xregisters.write($rs1, $r1_val);
            $state.xregisters.write($rs2, $r2_val);

            let new_pc = $state.$branch_fn($imm, $rs1, $rs2, $width);
            prop_assert_eq!(&new_pc, $expected_pc);
        };
    }

    macro_rules! test_nz_branch_instr {
        ($state:ident, $branch_fn:tt, $imm:expr,
            $rs1:ident, $r1_val:expr,
            $rs2:ident, $r2_val:expr, $width:expr,
            $init_pc:ident, $expected_pc:expr
           ) => {
            $state.pc.write($init_pc);
            $state.xregisters.write($rs1, $r1_val);
            $state.xregisters.write($rs2, $r2_val);

            let new_pc = $state.$branch_fn($imm, nz::$rs1, nz::$rs2, $width);
            prop_assert_eq!(&new_pc, $expected_pc);
        };
    }

    backend_test!(test_beq_bne, F, {
        proptest!(|(
            init_pc in any::<u64>(),
            imm in any::<i64>(),
            r1_val in any::<u64>(),
            r2_val in any::<u64>(),
        )| {
            // to ensure different behaviour for tests
            prop_assume!(r1_val != r2_val);
            // to ensure branch_pc, init_pc, next_pc are different
            prop_assume!(imm > 10);
            let branch_pcu = ProgramCounterUpdate::Set(init_pc.wrapping_add(imm as u64));
            let width = InstrWidth::Uncompressed;
            let next_pcu = ProgramCounterUpdate::Next(InstrWidth::Uncompressed);
            let init_pcu = ProgramCounterUpdate::Set(init_pc);

            let mut state = create_state!(HartState, F);

            // BEQ - different
            test_nz_branch_instr!(state, run_beq, imm, t1, r1_val, t2, r2_val, width, init_pc, &next_pcu);
            // BEQ - equal
            test_nz_branch_instr!(state, run_beq, imm, t1, r1_val, t2, r1_val, width, init_pc, &branch_pcu);

            // BNE - different
            test_nz_branch_instr!(state, run_bne, imm, t1, r1_val, t2, r2_val, width, init_pc, &branch_pcu);
            // BNE - equal
            test_nz_branch_instr!(state, run_bne, imm, t1, r1_val, t2, r1_val, width, init_pc, &next_pcu);

            // BEQ - different - imm = 0
            test_nz_branch_instr!(state, run_beq, 0, t1, r1_val, t2, r2_val, width, init_pc, &next_pcu);
            // BEQ - equal - imm = 0
            test_nz_branch_instr!(state, run_beq, 0, t1, r1_val, t2, r1_val, width, init_pc, &init_pcu);

            // BNE - different - imm = 0
            test_nz_branch_instr!(state, run_bne, 0, t1, r1_val, t2, r2_val, width, init_pc, &init_pcu);
            // BNE - equal - imm = 0
            test_nz_branch_instr!(state, run_bne, 0, t1, r1_val, t2, r1_val, width, init_pc, &next_pcu);

            // BEQ - same register - imm = 0
            test_nz_branch_instr!(state, run_beq, 0, t1, r1_val, t1, r2_val, width, init_pc, &init_pcu);
            // BEQ - same register
            test_nz_branch_instr!(state, run_beq, imm, t1, r1_val, t1, r2_val, width, init_pc, &branch_pcu);

            // BNE - same register - imm = 0
            test_nz_branch_instr!(state, run_bne, 0, t1, r1_val, t1, r2_val, width, init_pc, &next_pcu);
            // BNE - same register
            test_nz_branch_instr!(state, run_bne, imm, t1, r1_val, t1, r2_val, width, init_pc, &next_pcu);
        });
    });

    backend_test!(test_bge_blt, F, {
        proptest!(|(
            init_pc in any::<u64>(),
            imm in any::<i64>(),
        )| {
            // to ensure branch_pc and init_pc are different
            prop_assume!(imm > 10);
            let branch_pcu = ProgramCounterUpdate::Set(init_pc.wrapping_add(imm as u64));
            let width = InstrWidth::Uncompressed;
            let next_pcu = ProgramCounterUpdate::Next(InstrWidth::Uncompressed);
            let init_pcu = ProgramCounterUpdate::Set(init_pc);

            let mut state = create_state!(HartState, F);

            // lhs < rhs
            test_branch_instr!(state, run_blt, imm, t1, 0, t2, 1, width, init_pc, &branch_pcu);
            test_branch_instr!(state, run_bge, imm, t1, i64::MIN as u64, t2, i64::MAX as u64, width, init_pc, &next_pcu);

            // lhs > rhs
            test_branch_instr!(state, run_blt, imm, t1, -1_i64 as u64, t2, i64::MAX as u64, width, init_pc, &branch_pcu);
            test_branch_instr!(state, run_bge, imm, t1, 0, t2, -123_123i64 as u64, width, init_pc, &branch_pcu);

            // lhs = rhs
            test_branch_instr!(state, run_blt, imm, t1, 0, t2, 0, width, init_pc, &next_pcu);
            test_branch_instr!(state, run_bge, imm, t1, i64::MAX as u64, t2, i64::MAX as u64, width, init_pc, &branch_pcu);

            // same register
            test_branch_instr!(state, run_blt, imm, t1, -1_i64 as u64, t1, -1_i64 as u64, width, init_pc, &next_pcu);
            test_branch_instr!(state, run_bge, imm, t2, 0, t2, 0, width, init_pc, &branch_pcu);

            // imm 0
            // lhs < rhs
            test_branch_instr!(state, run_blt, 0, t1, 100, t2, i64::MAX as u64, width, init_pc, &init_pcu);
            test_branch_instr!(state, run_bge, 0, t1, -1_i64 as u64, t2, i64::MIN as u64, width, init_pc, &init_pcu);

            // same register
            test_branch_instr!(state, run_blt, 0, t1, 123_123_123, t1, 123_123_123, width, init_pc, &next_pcu);
            test_branch_instr!(state, run_bge, 0, t2, -1_i64 as u64, t2, -1_i64 as u64, width, init_pc, &init_pcu);
        });
    });

    backend_test!(test_bitwise, F, {
        proptest!(|(val in any::<u64>(), imm in any::<u64>())| {
            let mut state = create_state!(XRegisters, F);

            // The sign-extension of an immediate on 12 bits has bits 31:11 equal the sign-bit
            let prefix_mask = 0xFFFF_FFFF_FFFF_F800;
            let negative_imm = imm | prefix_mask;
            let positive_imm = imm & !prefix_mask;

            state.write(a0, val);
            state.run_andi(negative_imm as i64, nz::a0, nz::a1);
            prop_assert_eq!(state.read(a1), val & negative_imm);

            state.write(a1, val);
            state.run_andi(positive_imm as i64, nz::a1, nz::a2);
            prop_assert_eq!(state.read(a2), val & positive_imm);

            state.write(a0, val);
            state.run_ori(negative_imm as i64, nz::a0, nz::a0);
            prop_assert_eq!(state.read(a0), val | negative_imm);

            state.write(a0, val);
            state.run_ori(positive_imm as i64, nz::a0, nz::a1);
            prop_assert_eq!(state.read(a1), val | positive_imm);

            state.write(t2, val);
            state.run_xori(negative_imm as i64, nz::t2, nz::t2);
            prop_assert_eq!(state.read(t2), val ^ negative_imm);

            state.write(t2, val);
            state.run_xori(positive_imm as i64, nz::t2, nz::t1);
            prop_assert_eq!(state.read(t1), val ^ positive_imm);
        })
    });

    backend_test!(test_bitwise_reg, F, {
        proptest!(|(v1 in any::<u64>(), v2 in any::<u64>())| {
            let mut state = create_state!(XRegisters, F);

            state.write(a0, v1);
            state.write(t3, v2);
            state.run_and(nz::t3, nz::a0, nz::a1);
            prop_assert_eq!(state.read(a1), v1 & v2);

            state.write(a0, v1);
            state.write(t3, v2);
            state.run_or(nz::t3, nz::a0, nz::a0);
            prop_assert_eq!(state.read(a0), v1 | v2);

            state.write(t2, v1);
            state.write(t3, v2);
            state.run_xor(nz::t3, nz::t2, nz::t1);
            prop_assert_eq!(state.read(t1), v1 ^ v2);

            // Same register
            state.write(a0, v1);
            state.run_and(nz::a0, nz::a0, nz::a1);
            prop_assert_eq!(state.read(a1), v1);
            state.run_or(nz::a0, nz::a0, nz::a1);
            prop_assert_eq!(state.read(a1), v1);
            state.run_xor(nz::a0, nz::a0, nz::a0);
            prop_assert_eq!(state.read(a0), 0);
        });
    });

    backend_test!(test_bge_ble_u, F, {
        proptest!(|(
            init_pc in any::<u64>(),
            imm in any::<i64>(),
            r1_val in any::<u64>(),
            r2_val in any::<u64>(),
        )| {
            prop_assume!(r1_val < r2_val);
            // to ensure branch_pc and init_pc are different
            prop_assume!(imm > 10);
            let branch_pcu = ProgramCounterUpdate::Set(init_pc.wrapping_add(imm as u64));
            let width = InstrWidth::Uncompressed;
            let next_pcu = ProgramCounterUpdate::Next(InstrWidth::Uncompressed);
            let pc_update_init_pcu = ProgramCounterUpdate::Set(init_pc);

            let mut state = create_state!(HartState, F);

            // lhs < rhs
            test_branch_instr!(state, run_bltu, imm, t1, r1_val, t2, r2_val, width, init_pc, &branch_pcu);
            test_branch_instr!(state, run_bgeu, imm, t1, r1_val, t2, r2_val, width, init_pc, &next_pcu);

            // lhs > rhs
            test_branch_instr!(state, run_bltu, imm, t1, r2_val, t2, r1_val, width, init_pc, &next_pcu);
            test_branch_instr!(state, run_bgeu, imm, t1, r2_val, t2, r1_val, width, init_pc, &branch_pcu);

            // lhs = rhs
            test_branch_instr!(state, run_bltu, imm, t1, r1_val, t2, r1_val, width, init_pc, &next_pcu);
            test_branch_instr!(state, run_bgeu, imm, t1, r2_val, t2, r2_val, width, init_pc, &branch_pcu);

            // same register
            test_branch_instr!(state, run_bltu, imm, t1, r1_val, t1, r1_val, width, init_pc, &next_pcu);
            test_branch_instr!(state, run_bgeu, imm, t2, r1_val, t2, r1_val, width, init_pc, &branch_pcu);

            // imm 0
            // lhs < rhs
            test_branch_instr!(state, run_bltu, 0, t1, r1_val, t2, r2_val, width, init_pc, &pc_update_init_pcu);
            test_branch_instr!(state, run_bgeu, 0, t1, r1_val, t2, r2_val, width, init_pc, &next_pcu);

            // lhs > rhs
            test_branch_instr!(state, run_bltu, 0, t1, r2_val, t2, r1_val, width, init_pc, &next_pcu);
            test_branch_instr!(state, run_bgeu, 0, t1, r2_val, t2, r1_val, width, init_pc, &pc_update_init_pcu);

            // lhs = rhs
            test_branch_instr!(state, run_bltu, 0, t1, r1_val, t2, r1_val, width, init_pc, &next_pcu);
            test_branch_instr!(state, run_bgeu, 0, t2, r2_val, t1, r2_val, width, init_pc, &pc_update_init_pcu);

            // same register
            test_branch_instr!(state, run_bltu, 0, t1, r1_val, t1, r1_val, width, init_pc, &next_pcu);
            test_branch_instr!(state, run_bgeu, 0, t2, r1_val, t2, r1_val, width, init_pc, &pc_update_init_pcu);

        });
    });

    backend_test!(test_ebreak, F, {
        let state = create_state!(MachineCoreState, MachineCoreStateLayout<T1K>, F, T1K);

        let ret_val = state.hart.run_ebreak();
        assert_eq!(ret_val, Exception::Breakpoint);
    });

    backend_test!(test_fence, F, {
        let state = create_state!(MachineCoreState, MachineCoreStateLayout<T1K>, F, T1K);
        let state_cell = std::cell::RefCell::new(state);

        proptest!(|(
            pred in prop::array::uniform4(any::<bool>()),
            succ in prop::array::uniform4(any::<bool>())
        )| {
            let mut state = state_cell.borrow_mut();
            state.reset();

            let pred = FenceSet { i: pred[0], o: pred[1], r: pred[2], w: pred[3] };
            let succ = FenceSet { i: succ[0], o: succ[1], r: succ[2], w: succ[3] };

            state.hart.xregisters.write(t1, 123);
            state.hart.fregisters.write(fa0, 0.1_f64.to_bits().into());
            state.run_fence(pred, succ);
            assert_eq!(state.hart.xregisters.read(t1), 123);
            assert_eq!(state.hart.fregisters.read(fa0), 0.1_f64.to_bits().into());
        });
    });

    backend_test!(test_ecall, F, {
        let mut state = create_state!(HartState, F);

        let mode_exc = [
            (Mode::User, Exception::EnvCallFromUMode),
            (Mode::Supervisor, Exception::EnvCallFromSMode),
            (Mode::Machine, Exception::EnvCallFromMMode),
        ];

        for (mode, expected_e) in mode_exc {
            state.mode.write(mode);
            let instr_res = state.run_ecall();
            assert!(instr_res == expected_e);
        }
    });

    backend_test!(test_jalr, F, {
        let ipc_imm_irs1_rs1_rd_fpc_frd = [
            (42, 42, 4, a2, t1, 46, 46),
            (0, 1001, 100, a1, t1, 1100, 4),
            (
                u64::MAX - 1,
                100,
                -200_i64 as u64,
                a2,
                a2,
                -100_i64 as u64,
                2,
            ),
            (
                1_000_000_000_000,
                1_000_000_000_000,
                u64::MAX - 1_000_000_000_000 + 3,
                a2,
                t2,
                2,
                1_000_000_000_004,
            ),
        ];
        for (init_pc, imm, init_rs1, rs1, rd, res_pc, res_rd) in ipc_imm_irs1_rs1_rd_fpc_frd {
            let mut state = create_state!(HartState, F);

            state.pc.write(init_pc);
            state.xregisters.write(rs1, init_rs1);
            let new_pc = state.run_jalr(imm, rs1, rd);

            assert_eq!(state.pc.read(), init_pc);
            assert_eq!(new_pc, res_pc);
            assert_eq!(state.xregisters.read(rd), res_rd);
        }
    });

    backend_test!(test_jal, F, {
        let ipc_imm_rd_fpc_frd = [
            (42, 42, nz::t1, 84, 46),
            (0, 1000, nz::t1, 1000, 4),
            (50, -100, nz::t1, -50_i64 as u64, 54),
            (u64::MAX - 1, 100, nz::t1, 98_i64 as u64, 2),
            (
                1_000_000_000_000,
                (u64::MAX - 1_000_000_000_000 + 1) as i64,
                nz::t2,
                0,
                1_000_000_000_004,
            ),
        ];
        for (init_pc, imm, rd, res_pc, res_rd) in ipc_imm_rd_fpc_frd {
            let mut state = create_state!(HartState, F);

            state.pc.write(init_pc);
            let new_pc = state.run_jal(imm, rd, InstrWidth::Uncompressed);

            assert_eq!(state.pc.read(), init_pc);
            assert_eq!(new_pc, res_pc);
            assert_eq!(state.xregisters.read_nz(rd), res_rd);
        }
    });

    backend_test!(test_lui, F, {
        proptest!(|(imm in any::<i64>())| {
            let mut xregs = create_state!(XRegisters, F);
            xregs.write(a2, 0);
            xregs.write(a4, 0);

            // U-type immediate sets imm[31:20]
            let imm = imm & 0xFFFF_F000;
            xregs.run_lui(imm, nz::a3);
            // read value is the expected one
            prop_assert_eq!(xregs.read(a3), imm as u64);
            // it doesn't modify other registers
            prop_assert_eq!(xregs.read(a2), 0);
            prop_assert_eq!(xregs.read(a4), 0);
        });
    });

    backend_test!(test_slt, F, {
        let mut xregs = create_state!(XRegisters, F);

        let v1_v2_exp_expu = [
            (0, 0, 0, 0),
            (-1_i64 as u64, 0, 1, 0),
            (123123123, -1_i64 as u64, 0, 1),
            (123, 123123, 1, 1),
        ];

        for (v1, v2, exp, expu) in v1_v2_exp_expu {
            xregs.write(a1, v1);
            xregs.write(a2, v2);
            xregs.run_slt(a1, a2, nz::t0);
            assert_eq!(xregs.read(t0), exp);
            xregs.run_sltu(a1, a2, nz::t1);
            assert_eq!(xregs.read(t1), expu);
            xregs.run_slti(v2 as i64, a1, nz::t0);
            assert_eq!(xregs.read(t0), exp);
            xregs.run_sltiu(v2 as i64, a1, nz::t0);
            assert_eq!(xregs.read(t0), expu);
        }
    });

    backend_test!(test_xret, F, {
        proptest!(|(
            curr_pc in any::<Address>(),
            mepc in any::<Address>(),
            sepc in any::<Address>(),
        )| {
            let mut state = create_state!(HartState, F);

            // 4-byte align
            let mepc = mepc & !0b11;
            let sepc = sepc & !0b11;

            // TEST: TSR trapping
            state.reset(curr_pc);
            state.csregisters.write(CSRegister::mepc, mepc);
            state.csregisters.write(CSRegister::sepc, sepc);

            assert_eq!(state.csregisters.read::<CSRRepr>(CSRegister::sepc), sepc);
            assert_eq!(state.csregisters.read::<CSRRepr>(CSRegister::mepc), mepc);

            let mstatus: MStatus = state.csregisters.read(CSRegister::mstatus);
            let mstatus = mstatus.with_tsr(true);
            state.csregisters.write(CSRegister::mstatus, mstatus);
            assert_eq!(state.run_sret(), Err(Exception::IllegalInstruction));

            // set TSR back to 0
            let mstatus = mstatus.with_tsr(false);
            state.csregisters.write(CSRegister::mstatus, mstatus);

            // TEST: insuficient privilege mode
            state.mode.write(Mode::User);
            assert_eq!(state.run_sret(), Err(Exception::IllegalInstruction));

            // TEST: Use SRET from M-mode, check SPP, SIE, SPIE, MPRV
            state.mode.write(Mode::Machine);
            let mstatus = state.csregisters.read::<MStatus>(CSRegister::mstatus).with_sie(true);
            let mstatus = mstatus.with_spp(SPPValue::User);
            state.csregisters.write(CSRegister::mstatus, mstatus);

            // check pc address
            assert_eq!(state.run_sret(), Ok(sepc));
            // check fields
            let mstatus: MStatus = state.csregisters.read(CSRegister::mstatus);
            assert!(mstatus.spie());
            assert!(!mstatus.sie());
            assert!(!mstatus.mprv());
            assert_eq!(mstatus.spp(), SPPValue::User);
            assert_eq!(state.mode.read(), Mode::User);

            // TEST: Call MRET from M-mode, with MPRV true, and MPP Machine to see if MPRV stays the same.
            let mstatus = mstatus.with_mpie(true);
            let mstatus = mstatus.with_mpp(MPPValue::Machine);
            let mstatus = mstatus.with_mprv(true);
            state.csregisters.write(CSRegister::mstatus, mstatus);
            state.mode.write(Mode::Machine);
            // check pc address
            assert_eq!(state.run_mret(), Ok(mepc));
            // check fields
            let mstatus: MStatus = state.csregisters.read(CSRegister::mstatus);
            assert!(mstatus.mpie());
            assert!(mstatus.mie());
            assert!(mstatus.mprv());
            assert_eq!(mstatus.mpp(), MPPValue::User);
            assert_eq!(state.mode.read(), Mode::Machine);
        });
    });
}
