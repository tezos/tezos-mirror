// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_64_DC extension for RISC-V
//!
//! U:C-16

use crate::{
    machine_state::{
        bus::main_memory::MainMemoryLayout,
        registers::{sp, FRegister, XRegister},
        MachineState,
    },
    state_backend as backend,
    traps::Exception,
};

impl<ML, M> MachineState<ML, M>
where
    ML: MainMemoryLayout,
    M: backend::Manager,
{
    /// `C.FLD` CL-type compressed instruction
    ///
    /// Loads a double-precision floating-point value from memory into
    /// floating-point register `rd`. It computes an effective address by
    /// adding the immediate to the base address in register `rs1`.
    /// The immediate is obtained by zero-extending and scaling by 8 the
    /// offset encoded in the instruction (see U:C-16.3).
    pub fn run_cfld(&mut self, imm: i64, rs1: XRegister, rd: FRegister) -> Result<(), Exception> {
        debug_assert!(imm >= 0 && imm % 8 == 0);
        self.run_fld(imm, rs1, rd)
    }

    /// `C.FLDSP` CI-type compressed instruction
    ///
    /// Loads a double-precision floating-point value from memory into
    /// floating-point register `rd`. It computes an effective address by
    /// adding the immediate to the stack pointer.
    /// The immediate is obtained by zero-extending and scaling by 8 the
    /// offset encoded in the instruction (see U:C-16.3).
    pub fn run_cfldsp(&mut self, imm: i64, rd_rs1: FRegister) -> Result<(), Exception> {
        debug_assert!(imm >= 0 && imm % 8 == 0);
        self.run_fld(imm, sp, rd_rs1)
    }

    /// `C.FSD` CS-type compressed instruction
    ///
    /// Stores a double-precision floating-point value in floating-point
    /// register `rs2` to memory. It computes an effective address by adding
    /// the immediate to the base address in register `rs1`.
    /// The immediate is obtained by zero-extending and scaling by 8 the
    /// offset encoded in the instruction (see U:C-16.3).
    pub fn run_cfsd(&mut self, imm: i64, rs1: XRegister, rs2: FRegister) -> Result<(), Exception> {
        debug_assert!(imm >= 0 && imm % 8 == 0);
        self.run_fsd(imm, rs1, rs2)
    }

    /// `C.FSDSP` CSS-type compressed instruction
    ///
    /// Stores a double-precision floating-point value in floating-point
    /// register `rs2` to memory. It computes an effective address by adding
    /// the immediate to the stack pointer.
    /// The immediate is obtained by zero-extending and scaling by 8 the
    /// offset encoded in the instruction (see U:C-16.3).
    pub fn run_cfsdsp(&mut self, imm: i64, rs2: FRegister) -> Result<(), Exception> {
        debug_assert!(imm >= 0 && imm % 8 == 0);
        self.run_fsd(imm, sp, rs2)
    }
}

#[cfg(test)]
mod test {
    use crate::{
        backend_test,
        bits::Bits64,
        create_backend, create_state,
        machine_state::{
            bus::{devices::DEVICES_ADDRESS_SPACE_LENGTH, main_memory::tests::T1K},
            csregisters::{
                xstatus::{ExtensionValue, MStatus},
                CSRegister,
            },
            registers::{fa2, fa3, parse_xregister, sp},
            MachineState, MachineStateLayout,
        },
        traps::Exception,
    };
    use arbitrary_int::u5;
    use proptest::prelude::*;

    const ZERO_OFFSET: i64 = 0;
    const OUT_OF_BOUNDS_OFFSET: i64 = 1024;

    backend_test!(test_cfsd_cfld, F, {
        proptest!(|(
            base_addr in (DEVICES_ADDRESS_SPACE_LENGTH..(DEVICES_ADDRESS_SPACE_LENGTH+504_u64)),
            base_imm in (0..=64i64).prop_map(|x| x * 8), // multiples of 8 in the 0..512 range
            val in any::<f64>().prop_map(f64::to_bits),
            rs1 in (1_u8..31).prop_map(u5::new).prop_map(parse_xregister),
        )|
        {
            let mut backend = create_backend!(MachineStateLayout<T1K>, F);
            let mut state = create_state!(MachineState, MachineStateLayout<T1K>, F, backend, T1K);

            // Turn fs on
            let mstatus = MStatus::from_bits(0u64).with_fs(ExtensionValue::Dirty);
            state.hart.csregisters.write(CSRegister::mstatus, mstatus);

            let mut perform_test = |offset: i64| -> Result<(), Exception> {
                state.hart.fregisters.write(fa2, val.into());
                state.hart.xregisters.write(rs1, base_addr);

                let imm = base_imm + offset;
                state.run_cfsd(imm, rs1, fa2)?;
                state.run_cfld(imm, rs1, fa3)?;

                assert_eq!(state.hart.fregisters.read(fa3), val.into());
                Ok(())
            };

            // Aligned and unaligned loads / stores
            prop_assert!(perform_test(ZERO_OFFSET).is_ok());

            // Out of bounds loads / stores
            prop_assert!(perform_test(OUT_OF_BOUNDS_OFFSET).is_err_and(|e|
                matches!(e, Exception::StoreAMOAccessFault(_))
            ));
        });
    });

    backend_test!(test_cfsdsp_cfldsp, F, {
        proptest!(|(
            base_addr in (DEVICES_ADDRESS_SPACE_LENGTH..(DEVICES_ADDRESS_SPACE_LENGTH+504_u64)),
            base_imm in (0..=64i64).prop_map(|x| x * 8), // multiples of 8 in the 0..512 range
            val in any::<f64>().prop_map(f64::to_bits),
        )|
        {
            let mut backend = create_backend!(MachineStateLayout<T1K>, F);
            let mut state = create_state!(MachineState, MachineStateLayout<T1K>, F, backend, T1K);

            // Turn fs on
            let mstatus = MStatus::from_bits(0u64).with_fs(ExtensionValue::Dirty);
            state.hart.csregisters.write(CSRegister::mstatus, mstatus);

            let mut perform_test = |offset: i64| -> Result<(), Exception> {
                state.hart.fregisters.write(fa2, val.into());
                state.hart.xregisters.write(sp, base_addr);

                let imm = base_imm + offset;
                state.run_cfsdsp(imm, fa2)?;
                state.run_cfldsp(imm, fa3)?;

                assert_eq!(state.hart.fregisters.read(fa3), val.into());
                Ok(())
            };

            // Aligned and unaligned loads / stores
            prop_assert!(perform_test(ZERO_OFFSET).is_ok());

            // Out of bounds loads / stores
            prop_assert!(perform_test(OUT_OF_BOUNDS_OFFSET).is_err_and(|e|
                matches!(e, Exception::StoreAMOAccessFault(_))
            ));
        });
    });
}
