//! Implementation of RV_64_I extension for RISC-V
//!
//! Chapter 5 - Unprivileged spec

use crate::machine_state::{
    backend,
    registers::{XRegister, XRegisters},
};

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
}

#[cfg(test)]
pub mod tests {
    use crate::machine_state::{
        backend::tests::TestBackendFactory,
        registers::{a0, a1},
        HartState, HartStateLayout,
    };
    use crate::{create_backend, create_state};
    use proptest::{arbitrary::any, prop_assert_eq, proptest};

    pub fn test<F: TestBackendFactory>() {
        test_addiw::<F>();
    }

    fn test_addiw<F: TestBackendFactory>() {
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
    }
}
