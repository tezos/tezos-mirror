// SPDX-FileCopyrightText: 2023-2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_64_I extension for RISC-V
//!
//! Chapter 5 - Unprivileged spec

use crate::machine_state::MachineCoreState;
use crate::machine_state::memory;
use crate::machine_state::registers::XRegister;
use crate::state_backend as backend;
use crate::traps::Exception;

impl<MC, M> MachineCoreState<MC, M>
where
    MC: memory::MemoryConfig,
    M: backend::ManagerReadWrite,
{
    /// `LWU` I-type instruction
    ///
    /// Loads a word (4 bytes) starting from address given by: val(rs1) + imm
    /// zero-extending the result
    pub fn run_lwu(&mut self, imm: i64, rs1: XRegister, rd: XRegister) -> Result<(), Exception> {
        let value: u32 = self.read_from_bus(imm, rs1)?;
        // u32 as u64 zero-extends to 64 bits
        self.hart.xregisters.write(rd, value as u64);
        Ok(())
    }

    /// `LHU` I-type instruction
    ///
    /// Loads a half-word (2 bytes) starting from address given by: val(rs1) + imm
    /// zero-extending the result
    pub fn run_lhu(&mut self, imm: i64, rs1: XRegister, rs2: XRegister) -> Result<(), Exception> {
        let value: u16 = self.read_from_bus(imm, rs1)?;
        // u16 as u64 zero-extends to 64 bits
        self.hart.xregisters.write(rs2, value as u64);
        Ok(())
    }

    /// `LBU` I-type instruction
    ///
    /// Loads a single byte from the address given by: val(rs1) + imm
    /// zero-extending the result
    pub fn run_lbu(&mut self, imm: i64, rs1: XRegister, rs2: XRegister) -> Result<(), Exception> {
        let value: u8 = self.read_from_bus(imm, rs1)?;
        // u8 as u64 zero-extends to 64 bits
        self.hart.xregisters.write(rs2, value as u64);
        Ok(())
    }

    /// Stores a double-word (8 bytes from rs2) to the address starting at: `val(rs1) + imm`
    ///
    /// Relevant opcodes:
    /// - `SD`
    /// - `C.SDSP`
    pub fn run_sd(&mut self, imm: i64, rs1: XRegister, rs2: XRegister) -> Result<(), Exception> {
        let value: u64 = self.hart.xregisters.read(rs2);
        self.write_to_bus(imm, rs1, value)
    }

    /// Stores a word (lowest 4 bytes from rs2) to the address starting at: `val(rs1) + imm`
    ///
    /// Relevant opcodes:
    /// - `SW`
    /// - `C.SWSP`
    pub fn run_sw(&mut self, imm: i64, rs1: XRegister, rs2: XRegister) -> Result<(), Exception> {
        let value: u64 = self.hart.xregisters.read(rs2);
        // u64 as u32 is truncated, getting the lowest 32 bits
        self.write_to_bus(imm, rs1, value as u32)
    }

    /// Stores a half-word (lowest 2 bytes from rs2) to the address starting at: `val(rs1) + imm`
    ///
    /// Relevant opcodes:
    /// - `SH`
    pub fn run_sh(&mut self, imm: i64, rs1: XRegister, rs2: XRegister) -> Result<(), Exception> {
        let value: u64 = self.hart.xregisters.read(rs2);
        // u64 as u16 is truncated, getting the lowest 16 bits
        self.write_to_bus(imm, rs1, value as u16)
    }

    /// Stores a byte (lowest 1 byte from rs2) to the address starting at: `val(rs1) + imm`
    ///
    /// Relevant opcodes:
    /// - `SB`
    pub fn run_sb(&mut self, imm: i64, rs1: XRegister, rs2: XRegister) -> Result<(), Exception> {
        let value: u64 = self.hart.xregisters.read(rs2);
        // u64 as u8 is truncated, getting the lowest 8 bits
        self.write_to_bus(imm, rs1, value as u8)
    }
}

#[cfg(test)]
mod tests {
    use proptest::arbitrary::any;
    use proptest::prop_assert;
    use proptest::proptest;

    use crate::backend_test;
    use crate::instruction_context::LoadStoreWidth;
    use crate::interpreter::load_store::run_load;
    use crate::machine_state::MachineCoreState;
    use crate::machine_state::memory::M4K;
    use crate::machine_state::registers::a1;
    use crate::machine_state::registers::a2;
    use crate::machine_state::registers::a3;
    use crate::machine_state::registers::a4;
    use crate::machine_state::registers::t0;
    use crate::machine_state::registers::t1;
    use crate::machine_state::registers::t2;
    use crate::machine_state::registers::t3;
    use crate::machine_state::registers::t4;
    use crate::state::NewState;
    use crate::traps::Exception;

    backend_test!(test_load_store, F, {
        let state = MachineCoreState::<M4K, _>::new(&mut F::manager());
        let state_cell = std::cell::RefCell::new(state);

        proptest!(|(
            v_1 in any::<u8>(),
            v_2 in any::<u16>(),
            v_3 in any::<u32>(),
            v_4 in any::<u64>(),
        )|
        {
            let mut state = state_cell.borrow_mut();
            state.reset();
            state.main_memory.set_all_readable_writeable();

            let mut perform_test = |offset: u64, signed: bool| -> Result<(), Exception> {
                // Save test values v_i in registers ai
                state.hart.xregisters.write(a4, v_4);
                state.hart.xregisters.write(a3, v_3 as u64);
                state.hart.xregisters.write(a2, v_2 as u64);
                state.hart.xregisters.write(a1, v_1 as u64);

                // t0 will hold the "global" offset of all loads / stores we are going to make
                state.hart.xregisters.write(t0, offset);

                // Perform the stores
                state.run_sb(14, t0, a1)?;
                state.run_sw(8, t0, a3)?;
                state.run_sh(12, t0, a2)?;
                state.run_sd(0, t0, a4)?;

                match signed {
                    true => {
                        run_load(&mut *state, 0, t0, t4, true, LoadStoreWidth::Double)?;
                        run_load(&mut *state, 8, t0, t3, true, LoadStoreWidth::Word)?;
                        run_load(&mut *state, 12, t0, t2, true, LoadStoreWidth::Half)?;
                        run_load(&mut *state, 14, t0, t1, true, LoadStoreWidth::Byte)?;
                        assert_eq!(state.hart.xregisters.read(t4), v_4);
                        // Converting the expected result we are also checking the sign-extension behaviour
                        assert_eq!(state.hart.xregisters.read(t3), v_3 as i32 as u64);
                        assert_eq!(state.hart.xregisters.read(t2), v_2 as i16 as u64);
                        assert_eq!(state.hart.xregisters.read(t1), v_1 as i8 as u64);
                        Ok(())
                    },
                    false => {
                        run_load(&mut *state, 0, t0, t4, true, LoadStoreWidth::Double)?;
                        run_load(&mut *state, 8, t0, t3, false, LoadStoreWidth::Word)?;
                        run_load(&mut *state, 12, t0, t2, false, LoadStoreWidth::Half)?;
                        run_load(&mut *state, 14, t0, t1, false, LoadStoreWidth::Byte)?;
                        assert_eq!(state.hart.xregisters.read(t4), v_4);
                        // Converting the expected result we are also checking the sign-extension behaviour
                        assert_eq!(state.hart.xregisters.read(t3), v_3 as u64);
                        assert_eq!(state.hart.xregisters.read(t2), v_2 as u64);
                        assert_eq!(state.hart.xregisters.read(t1), v_1 as u64);
                        Ok(())
                    },
                }?;
                Ok(())
            };

            let invalid_offset = 0u64.wrapping_sub(1024);
            let aligned_offset = 512;
            let misaligned_offset = 513;

            // Out of bounds loads / stores
            prop_assert!(perform_test(invalid_offset, true).is_err_and(|e|
                matches!(e, Exception::StoreAMOAccessFault(_))
            ));
            // Aligned loads / stores
            prop_assert!(perform_test(aligned_offset, true).is_ok());
            // Unaligned loads / stores
            prop_assert!(perform_test(misaligned_offset, true).is_ok());

            // Out of bounds loads / stores
            prop_assert!(perform_test(invalid_offset, false).is_err_and(|e|
                matches!(e, Exception::StoreAMOAccessFault(_))
            ));
            // Aligned loads / stores
            prop_assert!(perform_test(aligned_offset, false).is_ok());
            // Unaligned loads / stores
            prop_assert!(perform_test(misaligned_offset, false).is_ok());
        });
    });
}
