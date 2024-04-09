// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Implementation of RV_64_A extension for RISC-V
//!
//! Chapter 8 - Unprivileged spec

use crate::{
    machine_state::{bus::main_memory::MainMemoryLayout, registers::XRegister, MachineState},
    state_backend as backend,
    traps::Exception,
};

impl<ML, M> MachineState<ML, M>
where
    ML: MainMemoryLayout,
    M: backend::Manager,
{
    /// `LR.D` R-type instruction
    ///
    /// See [Self::run_lr].
    /// The value in `rs2` is always 0 so it is ignored.
    /// The `aq` and `rl` bits specify additional memory constraints in
    /// multi-hart environments so they are currently ignored.
    pub fn run_lrd(
        &mut self,
        rs1: XRegister,
        _rs2: XRegister,
        rd: XRegister,
        _rl: bool,
        _aq: bool,
    ) -> Result<(), Exception> {
        self.run_lr::<u64>(rs1, rd, |x| x)
    }

    /// `SC.D` R-type instruction
    ///
    /// Conditionally writes a double in `rs2` to the address in `rs1`.
    /// SC.D succeeds only if the reservation is still valid and
    /// the reservation set contains the bytes being written.
    /// In case of success, write 0 in `rd`, otherwise write 1.
    /// See also [crate::machine_state::reservation_set].
    /// The `aq` and `rl` bits specify additional memory constraints in
    /// multi-hart environments so they are currently ignored.
    pub fn run_scd(
        &mut self,
        rs1: XRegister,
        rs2: XRegister,
        rd: XRegister,
        _rl: bool,
        _aq: bool,
    ) -> Result<(), Exception> {
        self.run_sc::<u64>(rs1, rs2, rd, |x| x)
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
        test_lrsc,
    };
    use proptest::prelude::*;

    test_lrsc!(test_lrd_scd, run_lrd, run_scd, 8, u64);

    test_lrsc!(test_lrd_scw, run_lrd, run_scw, 8, u32);

    test_lrsc!(test_lrw_scd, run_lrw, run_scd, 8, u32);
}
