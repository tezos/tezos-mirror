// SPDX-FileCopyrightText: 2023-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_32_I extension for RISC-V
//!
//! Chapter 2 - Unprivileged spec

use crate::machine_state::MachineCoreState;
use crate::machine_state::hart_state::HartState;
use crate::machine_state::memory;
use crate::parser::instruction::FenceSet;
use crate::state_backend as backend;
use crate::traps::Exception;

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
        Exception::EnvCall
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
    use proptest::proptest;

    use crate::backend_test;
    use crate::interpreter::integer::run_and;
    use crate::interpreter::integer::run_andi;
    use crate::interpreter::integer::run_or;
    use crate::machine_state::MachineCoreState;
    use crate::machine_state::hart_state::HartState;
    use crate::machine_state::memory::M4K;
    use crate::machine_state::registers::a0;
    use crate::machine_state::registers::a1;
    use crate::machine_state::registers::a2;
    use crate::machine_state::registers::fa0;
    use crate::machine_state::registers::nz;
    use crate::machine_state::registers::t1;
    use crate::machine_state::registers::t3;
    use crate::parser::instruction::FenceSet;
    use crate::state::NewState;
    use crate::traps::Exception;

    backend_test!(test_bitwise, F, {
        proptest!(|(val in any::<u64>(), imm in any::<u64>())| {
            let mut state = MachineCoreState::<M4K, _>::new(&mut F::manager());

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
        })
    });

    backend_test!(test_bitwise_reg, F, {
        // TODO: RV-512: move to integer.rs once all are supported.
        proptest!(|(v1 in any::<u64>(), v2 in any::<u64>())| {
            let mut state = MachineCoreState::<M4K, _>::new(&mut F::manager());

            state.hart.xregisters.write(a0, v1);
            state.hart.xregisters.write(t3, v2);
            run_and(&mut state, nz::t3, nz::a0, nz::a1);
            prop_assert_eq!(state.hart.xregisters.read(a1), v1 & v2);

            state.hart.xregisters.write(a0, v1);
            state.hart.xregisters.write(t3, v2);
            run_or(&mut state, nz::t3, nz::a0, nz::a0);
            prop_assert_eq!(state.hart.xregisters.read(a0), v1 | v2);

            // Same register
            state.hart.xregisters.write(a0, v1);
            run_and(&mut state, nz::a0, nz::a0, nz::a1);
            prop_assert_eq!(state.hart.xregisters.read(a1), v1);
            run_or(&mut state, nz::a0, nz::a0, nz::a1);
            prop_assert_eq!(state.hart.xregisters.read(a1), v1);
        });
    });

    backend_test!(test_ebreak, F, {
        let state = MachineCoreState::<M4K, _>::new(&mut F::manager());

        let ret_val = state.hart.run_ebreak();
        assert_eq!(ret_val, Exception::Breakpoint);
    });

    backend_test!(test_fence, F, {
        let state = MachineCoreState::<M4K, _>::new(&mut F::manager());
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
        let state = HartState::new(&mut F::manager());

        let instr_res = state.run_ecall();
        assert!(instr_res == Exception::EnvCall);
    });
}
