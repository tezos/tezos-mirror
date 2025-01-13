// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_32_C extension for RISC-V
//!
//! U:C-16

use crate::{
    machine_state::{
        hart_state::HartState,
        main_memory::{Address, MainMemoryLayout},
        registers::{sp, x0, NonZeroXRegister, XRegister, XRegisters},
        MachineCoreState, ProgramCounterUpdate,
    },
    parser::instruction::InstrWidth,
    state_backend as backend,
    traps::Exception,
};

impl<M> HartState<M>
where
    M: backend::ManagerReadWrite,
{
    /// `C.J` CJ-type compressed instruction
    ///
    /// Performs an unconditional control transfer. The immediate is added to
    /// the pc to form the jump target address.
    pub fn run_cj(&mut self, imm: i64) -> Address {
        self.run_jal(imm, x0)
    }

    /// `C.JR` CR-type compressed instruction
    ///
    /// Performs an unconditional control transfer to the address in register `rs1`.
    pub fn run_cjr(&mut self, rs1: NonZeroXRegister) -> Address {
        // The target address is obtained by setting the
        // least-significant bit of the address in rs1 to zero
        self.xregisters.read_nz(rs1) & !1
    }

    /// `C.JALR` CR-type compressed instruction
    ///
    /// Performs the same operation as `C.JR`, but additionally writes the
    /// address of the instruction following the jump (pc+2) to the
    /// link register (`x1`).
    pub fn run_cjalr(&mut self, rs1: NonZeroXRegister) -> Address {
        // The return address to be saved in rd is the next instruction after this one.
        let return_address = self.pc.read().wrapping_add(InstrWidth::Compressed as u64);
        self.xregisters
            .write_nz(NonZeroXRegister::x1, return_address);

        // The target address is obtained by setting the
        // least-significant bit of the address in rs1 to zero
        self.xregisters.read_nz(rs1) & !1
    }

    /// `C.BEQZ` CB-type compressed instruction
    ///
    /// Performs a conditional ( val(`rs1`) == 0 ) control transfer.
    /// If condition met, the offset is sign-extended and added to the pc to form the branch
    /// target address that is then set, otherwise indicates to proceed to the next instruction.
    pub fn run_cbeqz(&mut self, imm: i64, rs1: XRegister) -> ProgramCounterUpdate {
        self.run_beq_impl(imm, rs1, x0, InstrWidth::Compressed)
    }

    /// `C.BNEZ` CB-type compressed instruction
    ///
    /// Performs a conditional ( val(`rs1`) != 0 ) control transfer.
    /// If condition met, the offset is sign-extended and added to the pc to form the branch
    /// target address that is then set, otherwise indicates to proceed to the next instruction.
    pub fn run_cbnez(&mut self, imm: i64, rs1: XRegister) -> ProgramCounterUpdate {
        self.run_bne_impl(imm, rs1, x0, InstrWidth::Compressed)
    }

    /// `C.EBREAK` compressed instruction
    ///
    /// Equivalent to `EBREAK`.
    pub fn run_cebreak(&self) -> Exception {
        Exception::Breakpoint
    }
}

impl<M> XRegisters<M>
where
    M: backend::ManagerReadWrite,
{
    /// `C.ADDI` CI-type compressed instruction
    ///
    /// Adds the non-zero sign-extended 6-bit `imm` to the value in `rd_rs1` then
    /// writes the result to `rd_rs1`.
    pub fn run_caddi(&mut self, imm: i64, rd_rs1: NonZeroXRegister) {
        // Return the lower XLEN (64 bits in our case) bits of the addition
        // Irrespective of sign, the result is the same, casting to u64 for addition
        let rval = self.read_nz(rd_rs1);
        let result = rval.wrapping_add(imm as u64);
        self.write_nz(rd_rs1, result)
    }

    /// `C.ADDI16SP` CI-type compressed instruction
    ///
    /// Adds the non-zero immediate to the value in the stack pointer.
    /// The immediate is obtained by sign-extending and scaling by 16 the value
    /// encoded in the instruction (see U:C-16.5).
    pub fn run_caddi16sp(&mut self, imm: i64) {
        debug_assert!(imm != 0 && imm % 16 == 0);
        self.run_addi(imm, sp, sp)
    }

    /// `C.ADDI4SPN` CIW-type compressed instruction
    ///
    /// Adds the non-zero immediate to the stack pointer and writes the result
    /// to `rd`.
    /// The immediate is obtained by zero-extending and scaling by 4 the value
    /// encoded in the instruction (see U:C-16.5).
    pub fn run_caddi4spn(&mut self, imm: i64, rd: XRegister) {
        debug_assert!(imm > 0 && imm % 4 == 0);
        self.run_addi(imm, sp, rd)
    }

    /// `C.SLLI` CI-type compressed instruction
    ///
    /// Performs a logical left shift of the value in register `rd_rs1`
    /// then writes the result back to `rd_rs1`.
    pub fn run_cslli(&mut self, imm: i64, rd_rs1: NonZeroXRegister) {
        // SLLI encoding allows to consider the whole immediate as the shift amount
        self.write_nz(rd_rs1, self.read_nz(rd_rs1) << imm)
    }

    /// `C.SRLI` CB-type compressed instruction
    ///
    /// Performs a logical right shift of the value in register `rd_rs1`
    /// then writes the result back to `rd_rs1`.
    pub fn run_csrli(&mut self, imm: i64, rd_rs1: XRegister) {
        self.run_srli(imm, rd_rs1, rd_rs1)
    }

    /// `C.SRAI` CB-type compressed instruction
    ///
    /// Performs an arithmetic right shift of the value in register `rd_rs1`
    /// then writes the result back to `rd_rs1`.
    pub fn run_csrai(&mut self, imm: i64, rd_rs1: XRegister) {
        self.run_srai(imm, rd_rs1, rd_rs1)
    }

    /// `C.ANDI` CB-format instruction
    ///
    /// Computes the bitwise AND of the value in register `rd_rs1` and
    /// the sign-extended 6-bit immediate, then writes the result back to `rd_rs1`.
    pub fn run_candi(&mut self, imm: i64, rd_rs1: XRegister) {
        self.run_andi(imm, rd_rs1, rd_rs1)
    }

    /// `C.AND` CA-type compressed instruction
    ///
    /// Computes the bitwise AND of the values in registers `rd_rs1` and `rs2`,
    /// then writes the result back to register rd `rd_rs1`.
    pub fn run_cand(&mut self, rd_rs1: XRegister, rs2: XRegister) {
        self.run_and(rd_rs1, rs2, rd_rs1)
    }

    /// `C.XOR` CA-type compressed instruction
    ///
    /// Computes the bitwise XOR of the values in registers `rd_rs1` and `rs2`,
    /// then writes the result back to register rd `rd_rs1`.
    pub fn run_cxor(&mut self, rd_rs1: XRegister, rs2: XRegister) {
        self.run_xor(rd_rs1, rs2, rd_rs1)
    }

    /// `C.OR` CA-type compressed instruction
    ///
    /// Computes the bitwise OR of the values in registers `rd_rs1` and `rs2`,
    /// then writes the result back to register rd `rd_rs1`.
    pub fn run_cor(&mut self, rd_rs1: XRegister, rs2: XRegister) {
        self.run_or(rd_rs1, rs2, rd_rs1)
    }

    /// `C.SUB` CA-type compressed instruction
    ///
    /// Subtracts the value in register `rs2` from the value in register `rd_rs1`,
    /// then writes the result to register `rd_rs1`.
    pub fn run_csub(&mut self, rd_rs1: XRegister, rs2: XRegister) {
        self.run_sub(rd_rs1, rs2, rd_rs1)
    }

    /// `C.LI` CI-type compressed instruction
    ///
    /// Loads the sign-extended 6-bit immediate into register `rd_rs1`.
    pub fn run_cli(&mut self, imm: i64, rd_rs1: NonZeroXRegister) {
        self.write_nz(rd_rs1, imm as u64)
    }

    /// `C.LUI` CI-type compressed instruction
    ///
    /// Loads the non-zero 6-bit immediate into bits 17–12 of the
    /// register `rd_rs1`, clears the bottom 12 bits, and sign-extends bit 17
    /// into all higher bits of `rd_rs1`.
    pub fn run_clui(&mut self, imm: i64, rd_rs1: NonZeroXRegister) {
        self.write_nz(rd_rs1, imm as u64)
    }
}

impl<ML, M> MachineCoreState<ML, M>
where
    ML: MainMemoryLayout,
    M: backend::ManagerReadWrite,
{
    /// `C.LW` CL-type compressed instruction
    ///
    /// Loads a 32-bit value from memory into register `rd`. It computes
    /// an effective address by adding the immediate to the base address
    /// in register `rs1`.
    /// The immediate is obtained by zero-extending and scaling by 4 the
    /// offset encoded in the instruction (see U:C-16.3).
    pub fn run_clw(&mut self, imm: i64, rs1: XRegister, rd: XRegister) -> Result<(), Exception> {
        debug_assert!(imm >= 0 && imm % 4 == 0);
        self.run_lw(imm, rs1, rd)
    }

    /// `C.LWSP` CI-type compressed instruction
    ///
    /// Loads a 32-bit value from memory into register `rd`. It computes
    /// an effective address by adding the immediate to the stack pointer.
    /// The immediate is obtained by zero-extending and scaling by 4 the
    /// offset encoded in the instruction (see U:C-16.3).
    pub fn run_clwsp(&mut self, imm: i64, rd_rs1: NonZeroXRegister) -> Result<(), Exception> {
        debug_assert!(imm >= 0 && imm % 4 == 0);
        let value: i32 = self.read_from_bus(imm, sp)?;
        // i32 as u64 sign-extends to 64 bits
        self.hart.xregisters.write_nz(rd_rs1, value as u64);
        Ok(())
    }

    /// `C.SW` CS-type compressed instruction
    ///
    /// Stores a 32-bit value in register `rs2` to memory. It computes
    /// an effective address by adding the immediate to the base address
    /// in register `rs1`.
    /// The immediate is obtained by zero-extending and scaling by 4 the
    /// offset encoded in the instruction (see U:C-16.3).
    pub fn run_csw(&mut self, imm: i64, rs1: XRegister, rs2: XRegister) -> Result<(), Exception> {
        debug_assert!(imm >= 0 && imm % 4 == 0);
        self.run_sw(imm, rs1, rs2)
    }

    /// `C.SWSP` CSS-type compressed instruction
    ///
    /// Stores a 32-bit value in register `rs2` to memory. It computes
    /// an effective address by adding the immediate to the stack pointer.
    /// The immediate is obtained by zero-extending and scaling by 4 the
    /// offset encoded in the instruction (see U:C-16.3).
    pub fn run_cswsp(&mut self, imm: i64, rs2: XRegister) -> Result<(), Exception> {
        debug_assert!(imm >= 0 && imm % 4 == 0);
        self.run_sw(imm, sp, rs2)
    }

    /// C.NOP CI-type compressed instruction
    ///
    /// Does not change any user-visible state, except for advancing the pc and
    /// incrementing any applicable performance counters. Equivalent to `NOP`.
    pub fn run_cnop(&self) {}
}

#[cfg(test)]
mod tests {
    use crate::{
        backend_test, create_state,
        machine_state::{
            hart_state::{HartState, HartStateLayout},
            main_memory::tests::T1K,
            registers::{nz, nz::a0, XRegisters, XRegistersLayout},
            MachineCoreState, MachineCoreStateLayout,
        },
    };
    use proptest::{prelude::*, prop_assert_eq, proptest};

    backend_test!(run_cj, F, {
        let test_case = [
            (42, 42, 84),
            (0, 1000, 1000),
            (100, -50, 50),
            (50, -100, -50_i64 as u64),
            (u64::MAX - 1, 100, 98_i64 as u64),
        ];
        for (init_pc, imm, res_pc) in test_case {
            let mut state = create_state!(MachineCoreState, MachineCoreStateLayout<T1K>, F, T1K);

            state.hart.pc.write(init_pc);
            let new_pc = state.hart.run_cj(imm);

            assert_eq!(state.hart.pc.read(), init_pc);
            assert_eq!(new_pc, res_pc);
        }
    });

    backend_test!(test_cjr_cjalr, F, {
        let scenarios = [
            (42, 2, nz::a2, 2, 44),
            (u64::MAX - 1, -200_i64 as u64, nz::a2, -200_i64 as u64, 0),
            (
                1_000_000_000_000,
                u64::MAX - 1_000_000_000_000 + 3,
                nz::a2,
                u64::MAX - 1_000_000_000_000 + 3,
                1_000_000_000_002,
            ),
        ];
        for (init_pc, init_rs1, rs1, res_pc, res_rd) in scenarios {
            let mut state = create_state!(HartState, F);

            // Test C.JALR
            // save program counter and value for rs1.
            state.pc.write(init_pc);
            state.xregisters.write_nz(rs1, init_rs1);
            let new_pc = state.run_cjalr(rs1);

            // check the program counter hasn't changed, the returned
            // value for the program counter is correct, and that the link
            // register has been updated.
            assert_eq!(state.pc.read(), init_pc);
            assert_eq!(new_pc, res_pc);
            assert_eq!(state.xregisters.read_nz(nz::ra), res_rd);

            // Test C.JR
            // save program counter and value for rs1.
            state.pc.write(init_pc);
            state.xregisters.write_nz(rs1, init_rs1);
            let new_pc = state.run_cjr(rs1);

            // check the program counter hasn't changed and the returned
            // value for the program counter is correct.
            assert_eq!(state.pc.read(), init_pc);
            assert_eq!(new_pc, res_pc);
        }
    });

    backend_test!(run_caddi, F, {
        let state = create_state!(MachineCoreState, MachineCoreStateLayout<T1K>, F, T1K);
        let state_cell = std::cell::RefCell::new(state);

        proptest!(|(
            rd_val in any::<u64>(),
            imm in any::<i64>(),
        )| {
            let mut state = state_cell.borrow_mut();
            state.reset();

            state.hart.xregisters.write_nz(nz::a1, rd_val);
            state.hart.xregisters.run_caddi(imm, nz::a1);
            let res = state.hart.xregisters.read_nz(nz::a1);
            prop_assert_eq!(res, rd_val.wrapping_add(imm as u64));
        });
    });

    backend_test!(test_run_cli, F, {
        let imm_rdrs1_res = [
            (0_i64, nz::t3, 0_u64),
            (0xFFF0_0420, nz::t2, 0xFFF0_0420),
            (-1, nz::t4, 0xFFFF_FFFF_FFFF_FFFF),
        ];

        for (imm, rd_rs1, res) in imm_rdrs1_res {
            let mut state = create_state!(HartState, F);
            state.xregisters.run_cli(imm, rd_rs1);
            assert_eq!(state.xregisters.read_nz(rd_rs1), res);
        }
    });

    backend_test!(test_run_clui, F, {
        proptest!(|(imm in any::<i64>())| {
            let mut xregs = create_state!(XRegisters, F);
            xregs.write_nz(nz::a2, 0);
            xregs.write_nz(nz::a4, 0);

            // U-type immediate sets imm[31:20]
            let imm = imm & 0xFFFF_F000;
            xregs.run_clui(imm, nz::a3);
            // read value is the expected one
            prop_assert_eq!(xregs.read_nz(nz::a3), imm as u64);
            // it doesn't modify other registers
            prop_assert_eq!(xregs.read_nz(nz::a2), 0);
            prop_assert_eq!(xregs.read_nz(nz::a4), 0);
        });
    });

    macro_rules! test_shift_instr {
        ($state:ident, $shift_fn:tt, $imm:expr,
            $rd_rs1:ident, $r1_val:expr, $expected_val:expr
        ) => {
            $state.xregisters.write_nz($rd_rs1, $r1_val);
            $state.xregisters.$shift_fn($imm, $rd_rs1);
            let new_val = $state.xregisters.read_nz($rd_rs1);
            assert_eq!(new_val, $expected_val);
        };
    }

    backend_test!(test_shift, F, {
        let mut state = create_state!(HartState, F);

        // imm = 0
        test_shift_instr!(state, run_cslli, 0, a0, 0x1234_ABEF, 0x1234_ABEF);

        // small imm (< 32))
        test_shift_instr!(state, run_cslli, 20, a0, 0x1234_ABEF, 0x1_234A_BEF0_0000);
        // big imm (>= 32))
        test_shift_instr!(state, run_cslli, 40, a0, 0x1234_ABEF, 0x34AB_EF00_0000_0000);
    });
}
