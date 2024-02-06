//! Implementation of RV_32_I extension for RISC-V
//!
//! Chapter 2 - Unprivileged spec

use crate::machine_state::{
    backend,
    bus::Address,
    registers::{XRegister, XRegisters},
    HartState,
};

impl<M> XRegisters<M>
where
    M: backend::Manager,
{
    /// `ADDI` I-type instruction
    pub fn run_addi(&mut self, imm: i64, rs1: XRegister, rd: XRegister) {
        // Return the lower XLEN (64 bits in our case) bits of the multiplication
        // Irrespective of sign, the result is the same, casting to u64 for addition
        let rval = self.read(rs1);
        let result = rval.wrapping_add(imm as u64);
        self.write(rd, result);
    }

    /// `LUI` U-type instruction
    ///
    /// Set the upper 20 bits of the `rd` register with the `U-type` formatted immediate `imm`
    pub fn run_lui(&mut self, imm: i64, rd: XRegister) {
        // Being a `U-type` operation, the immediate is correctly formatted
        // (lower 12 bits cleared and the value is sign-extended)
        self.write(rd, imm as u64);
    }
}

impl<M> HartState<M>
where
    M: backend::Manager,
{
    /// `AUIPC` U-type instruction
    pub fn run_auipc(&mut self, imm: i64, rd: XRegister) {
        // U-type immediates have bits [31:12] set and the lower 12 bits zeroed.
        let rval = self.pc.read().wrapping_add(imm as u64);
        self.xregisters.write(rd, rval);
    }

    /// Generic `JALR` w.r.t instruction width
    fn run_jalr_impl<const INSTR_WIDTH: u64>(
        &mut self,
        imm: i64,
        rs1: XRegister,
        rd: XRegister,
    ) -> Address {
        // The return address to be saved in rd
        let return_address = self.pc.read().wrapping_add(INSTR_WIDTH);

        // The target address is obtained by adding the sign-extended
        // 12-bit I-immediate to the register rs1, then setting
        // the least-significant bit of the result to zero
        let target_address = self.xregisters.read(rs1).wrapping_add(imm as u64) & !1;

        self.xregisters.write(rd, return_address);

        target_address
    }

    /// `JALR` I-type instruction (note: uncompressed variant)
    ///
    /// Instruction mis-aligned will never be thrown because we allow C extension
    ///
    /// Always returns the target address (val(rs1) + imm)
    pub fn run_jalr(&mut self, imm: i64, rs1: XRegister, rd: XRegister) -> Address {
        self.run_jalr_impl::<4>(imm, rs1, rd)
    }

    /// Generic `JAL` w.r.t. instruction width
    fn run_jal_impl<const INSTR_WIDTH: u64>(&mut self, imm: i64, rd: XRegister) -> Address {
        let current_pc = self.pc.read();

        // Save the address after jump instruction into rd
        let return_address = current_pc.wrapping_add(INSTR_WIDTH);
        self.xregisters.write(rd, return_address);

        current_pc.wrapping_add(imm as u64)
    }

    /// `JAL` J-type instruction (note: uncompressed variant)
    ///
    /// Instruction mis-aligned will never be thrown because we allow C extension
    ///
    /// Always returns the target address (current program counter + imm)
    pub fn run_jal(&mut self, imm: i64, rd: XRegister) -> Address {
        self.run_jal_impl::<4>(imm, rd)
    }

    /// Generic `BEQ` w.r.t. instruction width
    fn run_beq_impl<const INSTR_WIDTH: u64>(
        &mut self,
        imm: i64,
        rs1: XRegister,
        rs2: XRegister,
    ) -> Address {
        let current_pc = self.pc.read();

        // Branch if `val(rs1) == val(rs2)`, jumping `imm` bytes ahead.
        // Otherwise, jump the width of current instruction
        if self.xregisters.read(rs1) == self.xregisters.read(rs2) {
            current_pc.wrapping_add(imm as u64)
        } else {
            current_pc.wrapping_add(INSTR_WIDTH)
        }
    }

    /// `BEQ` B-type instruction
    ///
    /// Returns the target address if registers contain the same value,
    /// otherwise the next instruction address
    pub fn run_beq(&mut self, imm: i64, rs1: XRegister, rs2: XRegister) -> Address {
        self.run_beq_impl::<4>(imm, rs1, rs2)
    }

    /// Generic `BNE` w.r.t. instruction width
    fn run_bne_impl<const INSTR_WIDTH: u64>(
        &mut self,
        imm: i64,
        rs1: XRegister,
        rs2: XRegister,
    ) -> Address {
        let current_pc = self.pc.read();

        // Branch if `val(rs1) != val(rs2)`, jumping `imm` bytes ahead.
        // Otherwise, jump the width of current instruction
        if self.xregisters.read(rs1) != self.xregisters.read(rs2) {
            current_pc.wrapping_add(imm as u64)
        } else {
            current_pc.wrapping_add(INSTR_WIDTH)
        }
    }

    /// `BNE` B-type instruction
    ///
    /// Returns the target address if registers contain different values,
    /// otherwise the next instruction address
    pub fn run_bne(&mut self, imm: i64, rs1: XRegister, rs2: XRegister) -> Address {
        self.run_bne_impl::<4>(imm, rs1, rs2)
    }
}

#[cfg(test)]
pub mod tests {
    use crate::machine_state::{
        backend::tests::TestBackendFactory,
        registers::{a0, a1, a2, a3, a4, t1, t2, t3, t4, t5, t6, XRegisters, XRegistersLayout},
        HartState, HartStateLayout,
    };
    use crate::{create_backend, create_state};
    use proptest::{prelude::any, prop_assert_eq, prop_assume, proptest};

    pub fn test<F: TestBackendFactory>() {
        test_addi::<F>();
        test_auipc::<F>();
        test_beq_bne::<F>();
        test_jal::<F>();
        test_jalr::<F>();
        test_lui::<F>();
    }

    fn test_addi<F: TestBackendFactory>() {
        let imm_rs1_rd_res = [
            (0_i64, 0_u64, t3, 0_u64),
            (0, 0xFFF0_0420, t2, 0xFFF0_0420),
            (-1, 0, t4, 0xFFFF_FFFF_FFFF_FFFF),
            (
                1_000_000,
                -123_000_987_i64 as u64,
                a2,
                -122_000_987_i64 as u64,
            ),
            (1_000_000, 123_000_987, a2, 124_000_987),
            (
                -1,
                -321_000_000_000_i64 as u64,
                a1,
                -321_000_000_001_i64 as u64,
            ),
        ];

        for (imm, rs1, rd, res) in imm_rs1_rd_res {
            let mut backend = create_backend!(HartStateLayout, F);
            let mut state = create_state!(HartState, F, backend);

            state.xregisters.write(a1, rs1);
            state.xregisters.run_addi(imm, a1, rd);
            // check against wrapping addition performed on the lowest 32 bits
            assert_eq!(state.xregisters.read(rd), res)
        }
    }

    fn test_auipc<F: TestBackendFactory>() {
        let pc_imm_res_rd = [
            (0, 0, 0, a2),
            (0, 0xFF_FFF0_0000, 0xFF_FFF0_0000, a0),
            (0x000A_AAAA, 0xFF_FFF0_0000, 0xFF_FFFA_AAAA, a1),
            (0xABCD_AAAA_FBC0_D3FE, 0, 0xABCD_AAAA_FBC0_D3FE, t5),
            (0xFFFF_FFFF_FFF0_0000, 0x10_0000, 0, t6),
        ];

        for (init_pc, imm, res, rd) in pc_imm_res_rd {
            let mut backend = create_backend!(HartStateLayout, F);
            let mut state = create_state!(HartState, F, backend);

            // U-type immediate only has upper 20 bits set, the lower 12 being set to 0
            assert_eq!(imm, ((imm >> 20) & 0xF_FFFF) << 20);

            state.pc.write(init_pc);
            state.run_auipc(imm, rd);

            let read_pc = state.xregisters.read(rd);

            assert_eq!(read_pc, res);
        }
    }

    macro_rules! test_branch_instr {
        ($state:ident, $branch_fn:tt, $imm:expr,
         $rs1:ident, $r1_val:expr,
         $rs2:ident, $r2_val:expr,
         $init_pc:ident, $expected_pc:expr
        ) => {
            $state.pc.write($init_pc);
            $state.xregisters.write($rs1, $r1_val);
            $state.xregisters.write($rs2, $r2_val);

            let new_pc = $state.$branch_fn($imm, $rs1, $rs2);
            prop_assert_eq!(new_pc, $expected_pc);
        };
    }

    fn test_beq_bne<F: TestBackendFactory>() {
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
            let branch_pc = init_pc.wrapping_add(imm as u64);
            let next_pc = init_pc.wrapping_add(4);

            let mut backend = create_backend!(HartStateLayout, F);
            let mut state = create_state!(HartState, F, backend);

            // BEQ - different
            test_branch_instr!(state, run_beq, imm, t1, r1_val, t2, r2_val, init_pc, next_pc);
            // BEQ - equal
            test_branch_instr!(state, run_beq, imm, t1, r1_val, t2, r1_val, init_pc, branch_pc);

            // BNE - different
            test_branch_instr!(state, run_bne, imm, t1, r1_val, t2, r2_val, init_pc, branch_pc);
            // BNE - equal
            test_branch_instr!(state, run_bne, imm, t1, r1_val, t2, r1_val, init_pc, next_pc);

            // BEQ - different - imm = 0
            test_branch_instr!(state, run_beq, 0, t1, r1_val, t2, r2_val, init_pc, next_pc);
            // BEQ - equal - imm = 0
            test_branch_instr!(state, run_beq, 0, t1, r1_val, t2, r1_val, init_pc, init_pc);

            // BNE - different - imm = 0
            test_branch_instr!(state, run_bne, 0, t1, r1_val, t2, r2_val, init_pc, init_pc);
            // BNE - equal - imm = 0
            test_branch_instr!(state, run_bne, 0, t1, r1_val, t2, r1_val, init_pc, next_pc);

            // BEQ - same register - imm = 0
            test_branch_instr!(state, run_beq, 0, t1, r1_val, t1, r2_val, init_pc, init_pc);
            // BEQ - same register
            test_branch_instr!(state, run_beq, imm, t1, r1_val, t1, r2_val, init_pc, branch_pc);

            // BNE - same register - imm = 0
            test_branch_instr!(state, run_bne, 0, t1, r1_val, t1, r2_val, init_pc, next_pc);
            // BNE - same register
            test_branch_instr!(state, run_bne, imm, t1, r1_val, t1, r2_val, init_pc, next_pc);
        });
    }

    fn test_jalr<F: TestBackendFactory>() {
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
            let mut backend = create_backend!(HartStateLayout, F);
            let mut state = create_state!(HartState, F, backend);

            state.pc.write(init_pc);
            state.xregisters.write(rs1, init_rs1);
            let new_pc = state.run_jalr(imm, rs1, rd);

            assert_eq!(state.pc.read(), init_pc);
            assert_eq!(new_pc, res_pc);
            assert_eq!(state.xregisters.read(rd), res_rd);
        }
    }

    fn test_jal<F: TestBackendFactory>() {
        let ipc_imm_rd_fpc_frd = [
            (42, 42, t1, 84, 46),
            (0, 1000, t1, 1000, 4),
            (50, -100, t1, -50_i64 as u64, 54),
            (u64::MAX - 1, 100, t1, 98_i64 as u64, 2),
            (
                1_000_000_000_000,
                (u64::MAX - 1_000_000_000_000 + 1) as i64,
                t2,
                0,
                1_000_000_000_004,
            ),
        ];
        for (init_pc, imm, rd, res_pc, res_rd) in ipc_imm_rd_fpc_frd {
            let mut backend = create_backend!(HartStateLayout, F);
            let mut state = create_state!(HartState, F, backend);

            state.pc.write(init_pc);
            let new_pc = state.run_jal(imm, rd);

            assert_eq!(state.pc.read(), init_pc);
            assert_eq!(new_pc, res_pc);
            assert_eq!(state.xregisters.read(rd), res_rd);
        }
    }

    fn test_lui<F: TestBackendFactory>() {
        proptest!(|(imm in any::<i64>())| {
            let mut backend = create_backend!(XRegistersLayout, F);
            let mut xregs = create_state!(XRegisters, F, backend);
            xregs.write(a2, 0);
            xregs.write(a4, 0);

            // U-type immediate sets imm[31:20]
            let imm = imm & 0xFFFF_F000;
            xregs.run_lui(imm, a3);
            // read value is the expected one
            prop_assert_eq!(xregs.read(a3), imm as u64);
            // it doesn't modify other registers
            prop_assert_eq!(xregs.read(a2), 0);
            prop_assert_eq!(xregs.read(a4), 0);
        });
    }
}
