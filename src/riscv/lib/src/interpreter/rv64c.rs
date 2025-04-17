// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_64_C extension for RISC-V
//!
//! U:C-16

use crate::machine_state::registers::NonZeroXRegister;
use crate::machine_state::registers::XRegisters;
use crate::state_backend as backend;

impl<M> XRegisters<M>
where
    M: backend::ManagerReadWrite,
{
    /// `C.ADDIW` CI-type compressed instruction
    ///
    /// Adds the non-zero sign-extended 6-bit `imm` to the value in `rd_rs1`,
    /// producing a 32-bit result which is then sign-extended to 64 bits and
    /// written back to `rd_rs1`.
    pub fn run_caddiw(&mut self, imm: i64, rd_rs1: NonZeroXRegister) {
        // We do not need to explicitly truncate for the lower bits since wrapping_add
        // has the same semantics & result on the lower 32 bits irrespective of bit width
        let rval = self.read_nz(rd_rs1);
        let result = rval.wrapping_add(imm as u64);
        // Truncate result to use only the lower 32 bits, then sign-extend to 64 bits.
        let result = result as i32 as u64;
        self.write_nz(rd_rs1, result);
    }
}

#[cfg(test)]
mod tests {
    use proptest::arbitrary::any;
    use proptest::prop_assert;
    use proptest::prop_assert_eq;
    use proptest::proptest;

    use crate::backend_test;
    use crate::machine_state::MachineCoreState;
    use crate::machine_state::hart_state::HartState;
    use crate::machine_state::memory::M4K;
    use crate::machine_state::registers::a3;
    use crate::machine_state::registers::a4;
    use crate::machine_state::registers::nz;
    use crate::machine_state::registers::t0;
    use crate::state::NewState;
    use crate::traps::Exception;

    backend_test!(test_caddiw, F, {
        proptest!(|(
            imm in any::<i64>(),
            reg_val in any::<i64>())|
        {
            let mut state = HartState::new(&mut F::manager());

            state.xregisters.write_nz(nz::a0, reg_val as u64);
            state.xregisters.run_caddiw(imm, nz::a0);
            // check against wrapping addition performed on the lowest 32 bits
            let r_val = reg_val as u32;
            let i_val = imm as u32;
            prop_assert_eq!(
                state.xregisters.read_nz(nz::a0),
                r_val.wrapping_add(i_val) as i32 as i64 as u64
            );
        });
    });

    backend_test!(test_run_cldsp_clwsp, F, {
        let state = MachineCoreState::<M4K, _>::new(&mut F::manager());
        let state_cell = std::cell::RefCell::new(state);

        proptest!(|(
            v_d in any::<u64>(),
            v_w in any::<u32>(),
        )|
        {
            let mut state = state_cell.borrow_mut();
            state.reset();
            state.main_memory.set_all_readable_writeable();

            let mut perform_test = |offset: u64| -> Result<(), Exception> {

                state.hart.xregisters.write(a4, v_d);
                state.hart.xregisters.write(a3, v_w as u64);

                // t0 will hold the "global" offset of all loads / stores we are going to make
                state.hart.xregisters.write(t0, offset);

                state.run_sd(0, t0, a4)?;
                state.run_sw(8, t0, a3)?;

                state.run_ldnz(offset as i64, nz::sp, nz::t4)?;
                state.run_lwnz((offset + 8) as i64, nz::sp, nz::t3)?;
                assert_eq!(state.hart.xregisters.read_nz(nz::t4), v_d);
                assert_eq!(state.hart.xregisters.read_nz(nz::t3), v_w as i32 as u64);

                Ok(())
            };

            let invalid_offset = 0u64.wrapping_sub(1024);
            let aligned_offset = 512;

            // Out of bounds loads / stores
            prop_assert!(perform_test(invalid_offset).is_err_and(|e|
                matches!(e, Exception::StoreAMOAccessFault(_))
            ));

            // Aligned loads / stores
            prop_assert!(perform_test(aligned_offset).is_ok());
        });
    });
}
