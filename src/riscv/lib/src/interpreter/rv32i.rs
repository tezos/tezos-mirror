// SPDX-FileCopyrightText: 2023-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_32_I extension for RISC-V
//!
//! Chapter 2 - Unprivileged spec

use crate::machine_state::MachineCoreState;
use crate::machine_state::ProgramCounterUpdate;
use crate::machine_state::hart_state::HartState;
use crate::machine_state::memory;
use crate::machine_state::memory::Address;
use crate::machine_state::mode::Mode;
use crate::machine_state::registers::NonZeroXRegister;
use crate::machine_state::registers::XRegisters;
use crate::parser::instruction::FenceSet;
use crate::parser::instruction::InstrWidth;
use crate::state_backend as backend;
use crate::traps::Exception;

impl<M> XRegisters<M>
where
    M: backend::ManagerReadWrite,
{
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

    /// Saves in `rd` the bitwise XOR between the value in `rs1` and `imm`
    ///
    /// Relevant RISC-V opcodes:
    /// - `XOR`
    /// - `C.XOR`
    pub fn run_xor(&mut self, rs1: NonZeroXRegister, rs2: NonZeroXRegister, rd: NonZeroXRegister) {
        let result = self.read_nz(rs1) ^ self.read_nz(rs2);
        self.write_nz(rd, result)
    }
}

impl<M> HartState<M>
where
    M: backend::ManagerReadWrite,
{
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
    ) -> ProgramCounterUpdate<Address> {
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
    ) -> ProgramCounterUpdate<Address> {
        if self.xregisters.read_nz(rs1) != self.xregisters.read_nz(rs2) {
            let current_pc = self.pc.read();
            ProgramCounterUpdate::Set(current_pc.wrapping_add(imm as u64))
        } else {
            ProgramCounterUpdate::Next(width)
        }
    }

    /// Sets branching address if `val(rs1) >= val(rs2)` in signed comparison,
    /// otherwise proceeds to the next instruction address.
    ///
    /// Relevant RISC-V opcodes:
    /// - `BGE`
    pub fn run_bge(
        &mut self,
        imm: i64,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> ProgramCounterUpdate<Address> {
        let lhs = self.xregisters.read_nz(rs1) as i64;
        let rhs = self.xregisters.read_nz(rs2) as i64;

        if lhs >= rhs {
            let current_pc = self.pc.read();
            ProgramCounterUpdate::Set(current_pc.wrapping_add(imm as u64))
        } else {
            ProgramCounterUpdate::Next(width)
        }
    }

    /// Sets branching address if `val(rs1) >= 0` in signed comparison,
    /// otherwise proceeds to the next instruction address.
    ///
    /// Relevant RISC-V opcodes:
    /// - `BLT`
    pub fn run_bgez(
        &mut self,
        imm: i64,
        rs1: NonZeroXRegister,
        width: InstrWidth,
    ) -> ProgramCounterUpdate<Address> {
        let lhs = self.xregisters.read_nz(rs1) as i64;

        if lhs >= 0 {
            let current_pc = self.pc.read();
            ProgramCounterUpdate::Set(current_pc.wrapping_add(imm as u64))
        } else {
            ProgramCounterUpdate::Next(width)
        }
    }

    /// Sets branching address if `val(rs1) > 0` in signed comparison,
    /// otherwise proceeds to the next instruction address.
    ///
    /// Relevant RISC-V opcodes:
    /// - `BLT`
    pub fn run_bgz(
        &mut self,
        imm: i64,
        rs1: NonZeroXRegister,
        width: InstrWidth,
    ) -> ProgramCounterUpdate<Address> {
        let lhs = self.xregisters.read_nz(rs1) as i64;

        if lhs > 0 {
            let current_pc = self.pc.read();
            ProgramCounterUpdate::Set(current_pc.wrapping_add(imm as u64))
        } else {
            ProgramCounterUpdate::Next(width)
        }
    }

    /// Sets branching address if `val(rs1) >= val(rs2)` in unsigned comparison,
    /// otherwise proceeds to the next instruction address.
    ///
    /// Relevant RISC-V opcodes:
    /// - `BGEU`
    pub fn run_bgeu(
        &mut self,
        imm: i64,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> ProgramCounterUpdate<Address> {
        let lhs = self.xregisters.read_nz(rs1);
        let rhs = self.xregisters.read_nz(rs2);

        if lhs >= rhs {
            let current_pc = self.pc.read();
            ProgramCounterUpdate::Set(current_pc.wrapping_add(imm as u64))
        } else {
            ProgramCounterUpdate::Next(width)
        }
    }

    /// Sets branching address if `val(rs1) < val(rs2)` in signed comparison,
    /// otherwise proceeds to the next instruction address.
    ///
    /// Relevant RISC-V opcodes:
    /// - `BLT`
    pub fn run_blt(
        &mut self,
        imm: i64,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> ProgramCounterUpdate<Address> {
        let lhs = self.xregisters.read_nz(rs1) as i64;
        let rhs = self.xregisters.read_nz(rs2) as i64;

        if lhs < rhs {
            let current_pc = self.pc.read();
            ProgramCounterUpdate::Set(current_pc.wrapping_add(imm as u64))
        } else {
            ProgramCounterUpdate::Next(width)
        }
    }

    /// Sets branching address if `val(rs1) < 0` in signed comparison,
    /// otherwise proceeds to the next instruction address.
    ///
    /// Relevant RISC-V opcodes:
    /// - `BLT`
    /// - `BGE`
    pub fn run_bltz(
        &mut self,
        imm: i64,
        rs1: NonZeroXRegister,
        width: InstrWidth,
    ) -> ProgramCounterUpdate<Address> {
        let lhs = self.xregisters.read_nz(rs1) as i64;

        if lhs < 0 {
            let current_pc = self.pc.read();
            ProgramCounterUpdate::Set(current_pc.wrapping_add(imm as u64))
        } else {
            ProgramCounterUpdate::Next(width)
        }
    }

    /// Sets branching address if `val(rs1) <= 0` in signed comparison,
    /// otherwise proceeds to the next instruction address.
    ///
    /// Relevant RISC-V opcodes:
    /// - `BGE`
    pub fn run_bltez(
        &mut self,
        imm: i64,
        rs1: NonZeroXRegister,
        width: InstrWidth,
    ) -> ProgramCounterUpdate<Address> {
        let lhs = self.xregisters.read_nz(rs1) as i64;

        if lhs <= 0 {
            let current_pc = self.pc.read();
            ProgramCounterUpdate::Set(current_pc.wrapping_add(imm as u64))
        } else {
            ProgramCounterUpdate::Next(width)
        }
    }

    /// `BLTU` B-type instruction
    ///
    /// Sets branching address if `val(rs1) < val(rs2)` in unsigned comparison,
    /// otherwise proceeds to the next instruction address.
    ///
    /// Relevant RISC-V opcodes:
    /// - `BLTU`
    pub fn run_bltu(
        &mut self,
        imm: i64,
        rs1: NonZeroXRegister,
        rs2: NonZeroXRegister,
        width: InstrWidth,
    ) -> ProgramCounterUpdate<Address> {
        let lhs = self.xregisters.read_nz(rs1);
        let rhs = self.xregisters.read_nz(rs2);

        if lhs < rhs {
            let current_pc = self.pc.read();
            ProgramCounterUpdate::Set(current_pc.wrapping_add(imm as u64))
        } else {
            ProgramCounterUpdate::Next(width)
        }
    }
}

impl<MC, M> MachineCoreState<MC, M>
where
    MC: memory::MemoryConfig,
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
    use proptest::prelude::any;
    use proptest::prelude::prop;
    use proptest::prop_assert_eq;
    use proptest::prop_assume;
    use proptest::proptest;

    use crate::backend_test;
    use crate::create_state;
    use crate::interpreter::integer::run_and;
    use crate::interpreter::integer::run_andi;
    use crate::interpreter::integer::run_or;
    use crate::machine_state::MachineCoreState;
    use crate::machine_state::MachineCoreStateLayout;
    use crate::machine_state::ProgramCounterUpdate;
    use crate::machine_state::csregisters::CSRRepr;
    use crate::machine_state::csregisters::CSRegister;
    use crate::machine_state::csregisters::xstatus::MPPValue;
    use crate::machine_state::csregisters::xstatus::MStatus;
    use crate::machine_state::csregisters::xstatus::SPPValue;
    use crate::machine_state::hart_state::HartState;
    use crate::machine_state::hart_state::HartStateLayout;
    use crate::machine_state::memory::Address;
    use crate::machine_state::memory::M4K;
    use crate::machine_state::mode::Mode;
    use crate::machine_state::registers::a0;
    use crate::machine_state::registers::a1;
    use crate::machine_state::registers::a2;
    use crate::machine_state::registers::fa0;
    use crate::machine_state::registers::nz;
    use crate::machine_state::registers::t1;
    use crate::machine_state::registers::t2;
    use crate::machine_state::registers::t3;
    use crate::parser::instruction::FenceSet;
    use crate::parser::instruction::InstrWidth;
    use crate::traps::Exception;

    macro_rules! test_b_instr {
        ($state:ident, $branch_fn:tt, $imm:expr,
         $rs1:ident, $r1_val:expr, $width:expr,
         $init_pc:ident, $expected_pc:expr
        ) => {
            $state.pc.write($init_pc);
            $state.xregisters.write_nz(nz::$rs1, $r1_val);

            let new_pc = $state.$branch_fn($imm, nz::$rs1, $width);
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
            test_nz_branch_instr!(state, run_blt, imm, t1, 0, t2, 1, width, init_pc, &branch_pcu);
            test_nz_branch_instr!(state, run_bge, imm, t1, i64::MIN as u64, t2, i64::MAX as u64, width, init_pc, &next_pcu);

            // lhs > rhs
            test_nz_branch_instr!(state, run_blt, imm, t1, -1_i64 as u64, t2, i64::MAX as u64, width, init_pc, &branch_pcu);
            test_nz_branch_instr!(state, run_bge, imm, t1, 0, t2, -123_123i64 as u64, width, init_pc, &branch_pcu);

            // lhs = rhs
            test_nz_branch_instr!(state, run_blt, imm, t1, 0, t2, 0, width, init_pc, &next_pcu);
            test_nz_branch_instr!(state, run_bge, imm, t1, i64::MAX as u64, t2, i64::MAX as u64, width, init_pc, &branch_pcu);

            // same register
            test_nz_branch_instr!(state, run_blt, imm, t1, -1_i64 as u64, t1, -1_i64 as u64, width, init_pc, &next_pcu);
            test_nz_branch_instr!(state, run_bge, imm, t2, 0, t2, 0, width, init_pc, &branch_pcu);

            // imm 0
            // lhs < rhs
            test_nz_branch_instr!(state, run_blt, 0, t1, 100, t2, i64::MAX as u64, width, init_pc, &init_pcu);
            test_nz_branch_instr!(state, run_bge, 0, t1, -1_i64 as u64, t2, i64::MIN as u64, width, init_pc, &init_pcu);

            // same register
            test_nz_branch_instr!(state, run_blt, 0, t1, 123_123_123, t1, 123_123_123, width, init_pc, &next_pcu);
            test_nz_branch_instr!(state, run_bge, 0, t2, -1_i64 as u64, t2, -1_i64 as u64, width, init_pc, &init_pcu);
        });
    });

    backend_test!(test_b, F, {
        proptest!(|(
            init_pc in any::<u64>(),
            imm in any::<i64>(),
        )| {
            // to ensure branch_pc and init_pc are different
            prop_assume!(imm > 10);
            let branch_pcu = ProgramCounterUpdate::Set(init_pc.wrapping_add(imm as u64));
            let width = InstrWidth::Uncompressed;
            let next_pcu = ProgramCounterUpdate::Next(InstrWidth::Uncompressed);

            let mut state = create_state!(HartState, F);

            // lhs < 0
            test_b_instr!(state, run_bltz, imm, t1, -1_i64 as u64, width, init_pc, &branch_pcu);
            test_b_instr!(state, run_bgez, imm, t1, -1_i64 as u64, width, init_pc, &next_pcu);
            test_b_instr!(state, run_bltez, imm, t1, -1_i64 as u64, width, init_pc, &branch_pcu);
            test_b_instr!(state, run_bgz, imm, t1, -1_i64 as u64, width, init_pc, &next_pcu);

            // lhs > 0
            test_b_instr!(state, run_bltz, imm, t1, 1, width, init_pc, &next_pcu);
            test_b_instr!(state, run_bgez, imm, t1, 1, width, init_pc, &branch_pcu);
            test_b_instr!(state, run_bltez, imm, t1, 1, width, init_pc, &next_pcu);
            test_b_instr!(state, run_bgz, imm, t1, 1, width, init_pc, &branch_pcu);

            // lhs = 0
            test_b_instr!(state, run_bltz, imm, t1, 0, width, init_pc, &next_pcu);
            test_b_instr!(state, run_bgez, imm, t1, 0, width, init_pc, &branch_pcu);
            test_b_instr!(state, run_bltez, imm, t1, 0, width, init_pc, &branch_pcu);
            test_b_instr!(state, run_bgz, imm, t1, 0, width, init_pc, &next_pcu);
        })
    });

    backend_test!(test_bitwise, F, {
        proptest!(|(val in any::<u64>(), imm in any::<u64>())| {
            let mut state = create_state!(MachineCoreState, MachineCoreStateLayout<M4K>, F, M4K);

            // The sign-extension of an immediate on 12 bits has bits 31:11 equal the sign-bit
            let prefix_mask = 0xFFFF_FFFF_FFFF_F800;
            let negative_imm = imm | prefix_mask;
            let positive_imm = imm & !prefix_mask;

            state.hart.xregisters.write(a0, val);
            run_andi(&mut state, negative_imm as i64, nz::a0, nz::a1);
            prop_assert_eq!(state.hart.xregisters.read(a1), val & negative_imm);

            state.hart.xregisters.write(a1, val);
            run_andi(&mut state, positive_imm as i64, nz::a1, nz::a2);
            prop_assert_eq!(state.hart.xregisters.read(a2), val & positive_imm);

            state.hart.xregisters.write(a0, val);
            state.hart.xregisters.run_ori(negative_imm as i64, nz::a0, nz::a0);
            prop_assert_eq!(state.hart.xregisters.read(a0), val | negative_imm);

            state.hart.xregisters.write(a0, val);
            state.hart.xregisters.run_ori(positive_imm as i64, nz::a0, nz::a1);
            prop_assert_eq!(state.hart.xregisters.read(a1), val | positive_imm);

            state.hart.xregisters.write(t2, val);
            state.hart.xregisters.run_xori(negative_imm as i64, nz::t2, nz::t2);
            prop_assert_eq!(state.hart.xregisters.read(t2), val ^ negative_imm);

            state.hart.xregisters.write(t2, val);
            state.hart.xregisters.run_xori(positive_imm as i64, nz::t2, nz::t1);
            prop_assert_eq!(state.hart.xregisters.read(t1), val ^ positive_imm);
        })
    });

    backend_test!(test_bitwise_reg, F, {
        // TODO: RV-512: move to integer.rs once all are supported.
        proptest!(|(v1 in any::<u64>(), v2 in any::<u64>())| {
            let mut state = create_state!(MachineCoreState, MachineCoreStateLayout<M4K>, F, M4K);

            state.hart.xregisters.write(a0, v1);
            state.hart.xregisters.write(t3, v2);
            run_and(&mut state, nz::t3, nz::a0, nz::a1);
            prop_assert_eq!(state.hart.xregisters.read(a1), v1 & v2);

            state.hart.xregisters.write(a0, v1);
            state.hart.xregisters.write(t3, v2);
            run_or(&mut state, nz::t3, nz::a0, nz::a0);
            prop_assert_eq!(state.hart.xregisters.read(a0), v1 | v2);

            state.hart.xregisters.write(t2, v1);
            state.hart.xregisters.write(t3, v2);
            state.hart.xregisters.run_xor(nz::t3, nz::t2, nz::t1);
            prop_assert_eq!(state.hart.xregisters.read(t1), v1 ^ v2);

            // Same register
            state.hart.xregisters.write(a0, v1);
            run_and(&mut state, nz::a0, nz::a0, nz::a1);
            prop_assert_eq!(state.hart.xregisters.read(a1), v1);
            run_or(&mut state, nz::a0, nz::a0, nz::a1);
            prop_assert_eq!(state.hart.xregisters.read(a1), v1);
            state.hart.xregisters.run_xor(nz::a0, nz::a0, nz::a0);
            prop_assert_eq!(state.hart.xregisters.read(a0), 0);
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
            test_nz_branch_instr!(state, run_bltu, imm, t1, r1_val, t2, r2_val, width, init_pc, &branch_pcu);
            test_nz_branch_instr!(state, run_bgeu, imm, t1, r1_val, t2, r2_val, width, init_pc, &next_pcu);

            // lhs > rhs
            test_nz_branch_instr!(state, run_bltu, imm, t1, r2_val, t2, r1_val, width, init_pc, &next_pcu);
            test_nz_branch_instr!(state, run_bgeu, imm, t1, r2_val, t2, r1_val, width, init_pc, &branch_pcu);

            // lhs = rhs
            test_nz_branch_instr!(state, run_bltu, imm, t1, r1_val, t2, r1_val, width, init_pc, &next_pcu);
            test_nz_branch_instr!(state, run_bgeu, imm, t1, r2_val, t2, r2_val, width, init_pc, &branch_pcu);

            // same register
            test_nz_branch_instr!(state, run_bltu, imm, t1, r1_val, t1, r1_val, width, init_pc, &next_pcu);
            test_nz_branch_instr!(state, run_bgeu, imm, t2, r1_val, t2, r1_val, width, init_pc, &branch_pcu);

            // imm 0
            // lhs < rhs
            test_nz_branch_instr!(state, run_bltu, 0, t1, r1_val, t2, r2_val, width, init_pc, &pc_update_init_pcu);
            test_nz_branch_instr!(state, run_bgeu, 0, t1, r1_val, t2, r2_val, width, init_pc, &next_pcu);

            // lhs > rhs
            test_nz_branch_instr!(state, run_bltu, 0, t1, r2_val, t2, r1_val, width, init_pc, &next_pcu);
            test_nz_branch_instr!(state, run_bgeu, 0, t1, r2_val, t2, r1_val, width, init_pc, &pc_update_init_pcu);

            // lhs = rhs
            test_nz_branch_instr!(state, run_bltu, 0, t1, r1_val, t2, r1_val, width, init_pc, &next_pcu);
            test_nz_branch_instr!(state, run_bgeu, 0, t2, r2_val, t1, r2_val, width, init_pc, &pc_update_init_pcu);

            // same register
            test_nz_branch_instr!(state, run_bltu, 0, t1, r1_val, t1, r1_val, width, init_pc, &next_pcu);
            test_nz_branch_instr!(state, run_bgeu, 0, t2, r1_val, t2, r1_val, width, init_pc, &pc_update_init_pcu);

        });
    });

    backend_test!(test_ebreak, F, {
        let state = create_state!(MachineCoreState, MachineCoreStateLayout<M4K>, F, M4K);

        let ret_val = state.hart.run_ebreak();
        assert_eq!(ret_val, Exception::Breakpoint);
    });

    backend_test!(test_fence, F, {
        let state = create_state!(MachineCoreState, MachineCoreStateLayout<M4K>, F, M4K);
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
