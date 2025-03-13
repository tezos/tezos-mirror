// SPDX-FileCopyrightText: 2023-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_32_I extension for RISC-V
//!
//! Chapter 2 - Unprivileged spec

use crate::machine_state::MachineCoreState;
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
    use crate::create_state;
    use crate::interpreter::integer::run_and;
    use crate::interpreter::integer::run_andi;
    use crate::interpreter::integer::run_or;
    use crate::machine_state::MachineCoreState;
    use crate::machine_state::MachineCoreStateLayout;
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
