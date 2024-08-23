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
    /// `LR.W` R-type instruction
    ///
    /// See [Self::run_lr].
    /// The value in `rs2` is always 0 so it is ignored.
    /// The `aq` and `rl` bits specify additional memory constraints in
    /// multi-hart environments so they are currently ignored.
    pub fn run_lrw(
        &mut self,
        rs1: XRegister,
        _rs2: XRegister,
        rd: XRegister,
        _rl: bool,
        _aq: bool,
    ) -> Result<(), Exception> {
        self.run_lr::<i32>(rs1, rd, |x| x as u64)
    }

    /// `SC.W` R-type instruction
    ///
    /// Conditionally writes a word in `rs2` to the address in `rs1`.
    /// SC.W succeeds only if the reservation is still valid and
    /// the reservation set contains the bytes being written.
    /// In case of success, write 0 in `rd`, otherwise write 1.
    /// See also [crate::machine_state::reservation_set].
    /// The `aq` and `rl` bits specify additional memory constraints in
    /// multi-hart environments so they are currently ignored.
    pub fn run_scw(
        &mut self,
        rs1: XRegister,
        rs2: XRegister,
        rd: XRegister,
        _rl: bool,
        _aq: bool,
    ) -> Result<(), Exception> {
        self.run_sc::<i32>(rs1, rs2, rd, |x| x as i32)
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
        self.run_amo_w(rs1, rs2, rd, i32::wrapping_add)
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
        self.run_amo_w(rs1, rs2, rd, i32::bitxor)
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
        self.run_amo_w(rs1, rs2, rd, i32::bitand)
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
        self.run_amo_w(rs1, rs2, rd, i32::bitor)
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
        self.run_amo_w(rs1, rs2, rd, i32::min)
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
        self.run_amo_w(rs1, rs2, rd, i32::max)
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
        interpreter::atomics::{SC_FAILURE, SC_SUCCESS},
        machine_state::{
            bus::{devices::DEVICES_ADDRESS_SPACE_LENGTH, main_memory::tests::T1K},
            registers::{a0, a1, a2},
            MachineState, MachineStateLayout,
        },
    };
    use proptest::prelude::*;
    use std::ops::{BitAnd, BitOr, BitXor};

    #[macro_export]
    macro_rules! test_lrsc {
        ($name:ident, $lr: ident, $sc: ident, $align: expr, $t: ident) => {
            backend_test!($name, F, {
                let mut backend = create_backend!(MachineStateLayout<T1K>, F);
                let state = std::cell::RefCell::new(create_state!(MachineState, MachineStateLayout<T1K>, F, backend, T1K));

                proptest!(|(
                    r1_addr in (DEVICES_ADDRESS_SPACE_LENGTH/$align..(DEVICES_ADDRESS_SPACE_LENGTH+1023_u64)/$align).prop_map(|x| x * $align),
                    r1_val in any::<u64>(),
                    imm in any::<i64>(),
                )| {
                    let mut state = state.borrow_mut();
                    state.reset();

                    state.hart.xregisters.write(a0, r1_addr);
                    state.write_to_bus(0, a0, r1_val)?;

                    // SC.x fails when no reservation is set on the hart
                    state.$sc(a0, a1, a2, false, false)?;
                    let res = state.hart.xregisters.read(a2);
                    prop_assert_eq!(res, SC_FAILURE);

                    // Correct sequence of LR.x / SC.y instructions
                    // SC.x succeeds and stores the expected value
                    state.$lr(a0, a1, a2, false, false)?;
                    state.hart.xregisters.run_addi(imm, a2, a1);
                    state.$sc(a0, a1, a2, false, false)?;
                    let res = state.hart.xregisters.read(a2);
                    let val: $t = state.read_from_address(r1_addr)?;
                    prop_assert_eq!(res, SC_SUCCESS);
                    prop_assert_eq!(val, r1_val.wrapping_add(imm as u64) as $t);

                    // SC.x fails when a previous SC.x has been executed
                    state.$sc(a0, a1, a2, false, false)?;
                    let res = state.hart.xregisters.read(a2);
                    prop_assert_eq!(res, SC_FAILURE);
                })
            });
        }
    }

    #[macro_export]
    macro_rules! test_amo {
        ($instr: ident, $f: expr, $align: expr, $t: ident) => {
            backend_test!($instr, F, {
                let mut backend = create_backend!(MachineStateLayout<T1K>, F);
                let state = std::cell::RefCell::new(create_state!(MachineState, MachineStateLayout<T1K>, F, backend, T1K));

                proptest!(|(
                    r1_addr in (DEVICES_ADDRESS_SPACE_LENGTH/$align..(DEVICES_ADDRESS_SPACE_LENGTH+1023_u64)/$align).prop_map(|x| x * $align),
                    r1_val in any::<u64>(),
                    r2_val in any::<u64>(),
                )| {
                    let mut state = state.borrow_mut();
                    state.reset();

                    state.hart.xregisters.write(a0, r1_addr);
                    state.write_to_bus(0, a0, r1_val)?;
                    state.hart.xregisters.write(a1, r2_val);
                    state.$instr(a0, a1, a2, false, false)?;
                    let res: $t = state.read_from_address(r1_addr)?;

                    prop_assert_eq!(
                        state.hart.xregisters.read(a2) as $t, r1_val as $t);
                    // avoids redundant_closure_call warnings
                    let f = $f;
                    prop_assert_eq!(res, f(r1_val, r2_val))
                })
            });

        }
    }

    test_lrsc!(test_lrw_scw, run_lrw, run_scw, 4, u32);

    test_amo!(run_amoswapw, |_, r2_val| r2_val as i32, 4, i32);

    test_amo!(
        run_amoaddw,
        |r1_val, r2_val| (r1_val as i32).wrapping_add(r2_val as i32),
        4,
        i32
    );

    test_amo!(
        run_amoxorw,
        |r1_val, r2_val| (r1_val as i32).bitxor(r2_val as i32),
        4,
        i32
    );

    test_amo!(
        run_amoandw,
        |r1_val, r2_val| (r1_val as i32).bitand(r2_val as i32),
        4,
        i32
    );

    test_amo!(
        run_amoorw,
        |r1_val, r2_val| (r1_val as i32).bitor(r2_val as i32),
        4,
        i32
    );

    test_amo!(
        run_amominw,
        |r1_val, r2_val| (r1_val as i32).min(r2_val as i32),
        4,
        i32
    );

    test_amo!(
        run_amomaxw,
        |r1_val, r2_val| (r1_val as i32).max(r2_val as i32),
        4,
        i32
    );

    test_amo!(
        run_amominuw,
        |r1_val, r2_val| (r1_val as u32).min(r2_val as u32) as i32,
        4,
        i32
    );

    test_amo!(
        run_amomaxuw,
        |r1_val, r2_val| (r1_val as u32).max(r2_val as u32) as i32,
        4,
        i32
    );
}
