// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_32_C extension for RISC-V
//!
//! U:C-16

use crate::machine_state::hart_state::HartState;
use crate::state_backend as backend;
use crate::traps::Exception;

impl<M> HartState<M>
where
    M: backend::ManagerReadWrite,
{
    /// `C.EBREAK` compressed instruction
    ///
    /// Equivalent to `EBREAK`.
    pub fn run_cebreak(&self) -> Exception {
        Exception::Breakpoint
    }
}

#[cfg(test)]
mod tests {
    use crate::backend_test;
    use crate::interpreter::branching::run_j;
    use crate::interpreter::branching::run_jr;
    use crate::machine_state::MachineCoreState;
    use crate::machine_state::memory::M4K;
    use crate::machine_state::registers::nz;
    use crate::state::NewState;

    backend_test!(test_run_j, F, {
        let test_case = [
            (42, 42, 84),
            (0, 1000, 1000),
            (100, -50, 50),
            (50, -100, -50_i64 as u64),
            (u64::MAX - 1, 100, 98_i64 as u64),
        ];
        for (init_pc, imm, res_pc) in test_case {
            let mut state = MachineCoreState::<M4K, _>::new(&mut F::manager());

            state.hart.pc.write(init_pc);
            let new_pc = run_j(&mut state, imm);

            assert_eq!(state.hart.pc.read(), init_pc);
            assert_eq!(new_pc, res_pc);
        }
    });

    backend_test!(test_cjr, F, {
        let scenarios = [
            (42, 2, nz::a2, 2),
            (u64::MAX - 1, -200_i64 as u64, nz::a2, -200_i64 as u64),
            (
                1_000_000_000_000,
                u64::MAX - 1_000_000_000_000 + 3,
                nz::a2,
                u64::MAX - 1_000_000_000_000 + 3,
            ),
        ];
        for (init_pc, init_rs1, rs1, res_pc) in scenarios {
            let mut state = MachineCoreState::<M4K, _>::new(&mut F::manager());

            // Test C.JR
            // save program counter and value for rs1.
            state.hart.pc.write(init_pc);
            state.hart.xregisters.write_nz(rs1, init_rs1);
            let new_pc = run_jr(&mut state, rs1);

            // check the program counter hasn't changed and the returned
            // value for the program counter is correct.
            assert_eq!(state.hart.pc.read(), init_pc);
            assert_eq!(new_pc, res_pc);
        }
    });
}
