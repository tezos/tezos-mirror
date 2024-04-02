// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_32_C extension for RISC-V
//!
//! Chapter 16 - Unprivileged spec

use crate::{
    machine_state::{
        bus::{main_memory::MainMemoryLayout, Address},
        hart_state::HartState,
        registers::{XRegister, XRegisters},
        MachineState,
    },
    state_backend as backend,
};

impl<M> HartState<M>
where
    M: backend::Manager,
{
    /// `C.J` CJ-type compressed instruction
    ///
    /// Equivalent to `JAL x0,imm`
    /// Performs an unconditional control transfer. The immediate is added to
    /// the pc to form the jump target address.
    pub fn run_cj(&mut self, imm: i64) -> Address {
        self.run_jal(imm, XRegister::x0)
    }
}

impl<M> XRegisters<M>
where
    M: backend::Manager,
{
    /// `C.ADDI` CI-type compressed instruction
    ///
    /// Equivalent to `ADDI rd,rd,imm`
    /// Adds the non-zero sign-extended 6-bit `imm` to the value in `rd_rs1` then
    /// writes the result to `rd_rs1`.
    pub fn run_caddi(&mut self, imm: i64, rd_rs1: XRegister) {
        self.run_addi(imm, rd_rs1, rd_rs1)
    }
}

impl<ML, M> MachineState<ML, M>
where
    ML: MainMemoryLayout,
    M: backend::Manager,
{
    pub fn run_cnop(&self) {}
}

#[cfg(test)]
mod tests {
    use crate::{
        backend_test, create_backend, create_state,
        machine_state::{
            bus::main_memory::tests::T1K, registers::a4, MachineState, MachineStateLayout,
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
            let mut backend = create_backend!(MachineStateLayout<T1K>, F);
            let mut state = create_state!(MachineState, MachineStateLayout<T1K>, F, backend, T1K);

            state.hart.pc.write(init_pc);
            let new_pc = state.hart.run_cj(imm);

            assert_eq!(state.hart.pc.read(), init_pc);
            assert_eq!(new_pc, res_pc);
        }
    });

    backend_test!(run_caddi, F, {
        proptest!(|(
            rd_val in any::<u64>(),
            imm in any::<i64>(),
        )| {
            let mut backend = create_backend!(MachineStateLayout<T1K>, F);
            let mut state = create_state!(MachineState, MachineStateLayout<T1K>, F, backend, T1K);

            state.hart.xregisters.write(a4, rd_val);
            state.hart.xregisters.run_caddi(imm, a4);
            let res = state.hart.xregisters.read(a4);
            prop_assert_eq!(res, rd_val.wrapping_add(imm as u64));
        });
    });
}
