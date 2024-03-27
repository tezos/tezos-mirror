// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_32_A extension for RISC-V
//!
//! Chapter 8 - Unprivileged spec

use crate::{
    machine_state::{bus::main_memory::MainMemoryLayout, registers::XRegister, MachineState},
    state_backend as backend,
    traps::Exception,
};
use std::ops::{BitAnd, BitOr, BitXor};

impl<ML, M> MachineState<ML, M>
where
    ML: MainMemoryLayout,
    M: backend::Manager,
{
    /// Generic implementation of any atomic memory operation which works on
    /// 32-bit values, implementing read-modify-write operations for multi-
    /// processor synchronisation (Section 8.4)
    fn run_amo_w(
        &mut self,
        rs1: XRegister,
        rs2: XRegister,
        rd: XRegister,
        f: fn(i32, i32) -> i32,
    ) -> Result<(), Exception> {
        // Load the value from address in rs1
        let address_rs1 = self.hart.xregisters.read(rs1);
        let value_rs1: i32 = self.read_from_address(address_rs1)?;

        // Apply the binary operation to the loaded value and the value in rs2
        let value_rs2 = self.hart.xregisters.read(rs2) as i32;
        let value = f(value_rs1, value_rs2) as u64;

        // Write the value read fom the address in rs1 in rd
        self.hart.xregisters.write(rd, value_rs1 as u64);

        // Store the resulting value to the address in rs1
        self.write_to_address(address_rs1, value)
    }

    /// `AMOSWAP.W` R-type instruction
    ///
    /// Loads in rd the value from the address in rs1 and writes val(rs2)
    /// back to the address in rs1.
    /// The `aq` and `rl` bits specify additional memory constraints in
    /// multi-hart environments so they are currently ignored.
    pub fn run_amoswapw(
        &mut self,
        rs1: XRegister,
        rs2: XRegister,
        rd: XRegister,
        _rl: bool,
        _aq: bool,
    ) -> Result<(), Exception> {
        self.run_amo_w(rs1, rs2, rd, |_, value_rs2| value_rs2)
    }

    /// `AMOADD.W` R-type instruction
    ///
    /// Loads in rd the value from the address in rs1 and stores the result of
    /// adding it to val(rs2) back to the address in rs1.
    /// The `aq` and `rl` bits specify additional memory constraints in
    /// multi-hart environments so they are currently ignored.
    pub fn run_amoaddw(
        &mut self,
        rs1: XRegister,
        rs2: XRegister,
        rd: XRegister,
        _rl: bool,
        _aq: bool,
    ) -> Result<(), Exception> {
        self.run_amo_w(rs1, rs2, rd, |value_rs1, value_rs2| {
            i32::wrapping_add(value_rs1, value_rs2)
        })
    }

    /// `AMOXOR.W` R-type instruction
    ///
    /// Loads in rd the value from the address in rs1 and stores the result of
    /// XORing it to val(rs2) back to the address in rs1.
    /// The `aq` and `rl` bits specify additional memory constraints in
    /// multi-hart environments so they are currently ignored.
    pub fn run_amoxorw(
        &mut self,
        rs1: XRegister,
        rs2: XRegister,
        rd: XRegister,
        _rl: bool,
        _aq: bool,
    ) -> Result<(), Exception> {
        self.run_amo_w(rs1, rs2, rd, |value_rs1, value_rs2| {
            i32::bitxor(value_rs1, value_rs2)
        })
    }

    /// `AMOAND.W` R-type instruction
    ///
    /// Loads in rd the value from the address in rs1 and stores the result of
    /// ANDing it to val(rs2) back to the address in rs1.
    /// The `aq` and `rl` bits specify additional memory constraints in
    /// multi-hart environments so they are currently ignored.
    pub fn run_amoandw(
        &mut self,
        rs1: XRegister,
        rs2: XRegister,
        rd: XRegister,
        _rl: bool,
        _aq: bool,
    ) -> Result<(), Exception> {
        self.run_amo_w(rs1, rs2, rd, |value_rs1, value_rs2| {
            i32::bitand(value_rs1, value_rs2)
        })
    }

    /// `AMOOR.W` R-type instruction
    ///
    /// Loads in rd the value from the address in rs1 and stores the result of
    /// ORing it to val(rs2) back to the address in rs1.
    /// The `aq` and `rl` bits specify additional memory constraints in
    /// multi-hart environments so they are currently ignored.
    pub fn run_amoorw(
        &mut self,
        rs1: XRegister,
        rs2: XRegister,
        rd: XRegister,
        _rl: bool,
        _aq: bool,
    ) -> Result<(), Exception> {
        self.run_amo_w(rs1, rs2, rd, |value_rs1, value_rs2| {
            i32::bitor(value_rs1, value_rs2)
        })
    }

    /// `AMOMIN.W` R-type instruction
    ///
    /// Loads in rd the value from the address in rs1 and stores the minimum
    /// between it and val(rs2) back to the address in rs1.
    /// The `aq` and `rl` bits specify additional memory constraints in
    /// multi-hart environments so they are currently ignored.
    pub fn run_amominw(
        &mut self,
        rs1: XRegister,
        rs2: XRegister,
        rd: XRegister,
        _rl: bool,
        _aq: bool,
    ) -> Result<(), Exception> {
        self.run_amo_w(rs1, rs2, rd, |value_rs1, value_rs2| {
            i32::min(value_rs1, value_rs2)
        })
    }

    /// `AMOMAX.W` R-type instruction
    ///
    /// Loads in rd the value from the address in rs1 and stores the maximum
    /// between it and val(rs2) back to the address in rs1.
    /// The `aq` and `rl` bits specify additional memory constraints in
    /// multi-hart environments so they are currently ignored.
    pub fn run_amomaxw(
        &mut self,
        rs1: XRegister,
        rs2: XRegister,
        rd: XRegister,
        _rl: bool,
        _aq: bool,
    ) -> Result<(), Exception> {
        self.run_amo_w(rs1, rs2, rd, |value_rs1, value_rs2| {
            i32::max(value_rs1, value_rs2)
        })
    }

    /// `AMOMINU.W` R-type instruction
    ///
    /// Loads in rd the value from the address in rs1 and stores the minimum
    /// between it and val(rs2) back to the address in rs1, treating both as
    /// unsigned values.
    /// The `aq` and `rl` bits specify additional memory constraints in
    /// multi-hart environments so they are currently ignored.
    pub fn run_amominuw(
        &mut self,
        rs1: XRegister,
        rs2: XRegister,
        rd: XRegister,
        _rl: bool,
        _aq: bool,
    ) -> Result<(), Exception> {
        self.run_amo_w(rs1, rs2, rd, |value_rs1, value_rs2| {
            (value_rs1 as u32).min(value_rs2 as u32) as i32
        })
    }

    /// `AMOMAXU.W` R-type instruction
    ///
    /// Loads in rd the value from the address in rs1 and stores the maximum
    /// between it and val(rs2) back to the address in rs1, treating both as
    /// unsigned values.
    /// The `aq` and `rl` bits specify additional memory constraints in
    /// multi-hart environments so they are currently ignored.
    pub fn run_amomaxuw(
        &mut self,
        rs1: XRegister,
        rs2: XRegister,
        rd: XRegister,
        _rl: bool,
        _aq: bool,
    ) -> Result<(), Exception> {
        self.run_amo_w(rs1, rs2, rd, |value_rs1, value_rs2| {
            (value_rs1 as u32).max(value_rs2 as u32) as i32
        })
    }
}

#[cfg(test)]
mod test {
    use crate::{
        backend_test, create_backend, create_state,
        machine_state::{
            bus::{devices::DEVICES_ADDRESS_SPACE_LENGTH, main_memory::tests::T1K},
            registers::{a0, a1, a2},
            MachineState, MachineStateLayout,
        },
    };
    use proptest::prelude::*;
    use std::ops::{BitAnd, BitOr, BitXor};

    macro_rules! test_amo_w {
        ($name:ident, $instr: ident, $f: expr) => {
            backend_test!($name, F, {
                proptest!(|(
                    r1_addr in (DEVICES_ADDRESS_SPACE_LENGTH/4..(DEVICES_ADDRESS_SPACE_LENGTH+1023_u64)/4).prop_map(|x| x * 4),
                    r1_val in any::<u64>(),
                    r2_val in any::<u64>(),
                )| {
                    let mut backend = create_backend!(MachineStateLayout<T1K>, F);
                    let mut state = create_state!(MachineState, MachineStateLayout<T1K>, F, backend, T1K);

                    state.hart.xregisters.write(a0, r1_addr);
                    state.write_to_bus(0, a0, r1_val)?;
                    state.hart.xregisters.write(a1, r2_val);
                    state.$instr(a0, a1, a2, false, false)?;
                    let res: i32 = state.read_from_address(r1_addr)?;

                    prop_assert_eq!(
                        state.hart.xregisters.read(a2) as i32, r1_val as i32);
                    // avoids redundant_closure_call warnings
                    let f = $f;
                    prop_assert_eq!(res, f(r1_val, r2_val))
                })
            });

        }
    }

    test_amo_w!(test_amoswapw, run_amoswapw, |_, r2_val| r2_val as i32);

    test_amo_w!(test_amoaddw, run_amoaddw, |r1_val, r2_val| (r1_val as i32)
        .wrapping_add(r2_val as i32));

    test_amo_w!(test_amoxorw, run_amoxorw, |r1_val, r2_val| (r1_val as i32)
        .bitxor(r2_val as i32));

    test_amo_w!(test_amoandw, run_amoandw, |r1_val, r2_val| (r1_val as i32)
        .bitand(r2_val as i32));

    test_amo_w!(test_amoorw, run_amoorw, |r1_val, r2_val| (r1_val as i32)
        .bitor(r2_val as i32));

    test_amo_w!(test_amominw, run_amominw, |r1_val, r2_val| (r1_val as i32)
        .min(r2_val as i32));

    test_amo_w!(test_amomaxw, run_amomaxw, |r1_val, r2_val| (r1_val as i32)
        .max(r2_val as i32));

    test_amo_w!(
        test_amominuw,
        run_amominuw,
        |r1_val, r2_val| (r1_val as u32).min(r2_val as u32) as i32
    );

    test_amo_w!(
        test_amomaxuw,
        run_amomaxuw,
        |r1_val, r2_val| (r1_val as u32).max(r2_val as u32) as i32
    );
}
