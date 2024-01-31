//! Implementation of RV_32_I extension for RISC-V
//!
//! Chapter 2 - Unprivileged spec

use crate::{
    backend,
    registers::{XRegister, XRegisters},
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

#[cfg(test)]
pub mod tests {
    use crate::{
        backend::tests::TestBackendFactory,
        create_backend, create_state,
        registers::{a1, a2, a3, a4, t2, t3, t4, XRegisters, XRegistersLayout},
        HartState, HartStateLayout,
    };
    use proptest::{prelude::any, prop_assert_eq, proptest};

    pub fn test<F: TestBackendFactory>() {
        test_addi::<F>();
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
