// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::{
    machine_state::{bus::main_memory::MainMemoryLayout, MachineState},
    state_backend::Manager,
};

pub mod test;

/// Interface for a debuggable stepper
pub trait Stepper {
    /// Memory layout of the underlying machine state
    type MainMemoryLayout: MainMemoryLayout;

    /// State backend with which the stepper was instantiated
    type Manager: Manager;

    /// Obtain a reference to the underlying machine state.
    fn machine_state(&self) -> &MachineState<Self::MainMemoryLayout, Self::Manager>;
}
