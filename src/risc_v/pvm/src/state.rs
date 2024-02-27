// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

// Allow dead code while this module contains stubs.
#![allow(dead_code)]

use risc_v_interpreter::{machine_state, state_backend};

/// Memory configuration
type MemorySize = machine_state::bus::main_memory::M1G;

/// PVM state layout
type PvmLayout = (
    state_backend::Atom<u64>,
    machine_state::MachineStateLayout<MemorySize>,
);

/// Value for the initial version
const INITIAL_VERSION: u64 = 0;

/// Proof-generating virtual machine
pub struct Pvm<M: state_backend::Manager> {
    version: state_backend::Cell<u64, M>,
    machine_state: machine_state::MachineState<MemorySize, M>,
}

impl<M: state_backend::Manager> Pvm<M> {
    /// Bind the PVM to the given allocated region.
    pub fn bind(space: state_backend::AllocatedOf<PvmLayout, M>) -> Self {
        // Ensure we're binding a version we can deal with
        assert_eq!(space.0.read(), INITIAL_VERSION);

        Self {
            version: space.0,
            machine_state: machine_state::MachineState::bind(space.1),
        }
    }

    /// Reset the PVM state.
    pub fn reset(&mut self) {
        self.version.write(INITIAL_VERSION);
        self.machine_state.reset();
    }

    /// Provide input. Returns `false` if the machine state is not in
    /// `Status::Input` status.
    pub fn provide_input(&mut self, _level: u64, _counter: u64, _payload: &[u8]) -> bool {
        // TODO: https://gitlab.com/tezos/tezos/-/issues/6945
        // Implement input provider
        todo!("Input function is not implemented")
    }

    /// Get the current machine status.
    pub fn status(&self) -> Status {
        Status::Eval
    }

    /// Perform one step. Returns `false` if the PVM is not in `Status::Eval` status.
    pub fn step(&mut self) -> bool {
        self.machine_state.step();
        true
    }

    /// Perform at most `max_steps` steps. Returns the actual number of steps
    /// performed.
    pub fn step_many(&mut self, max_steps: usize) -> usize {
        self.machine_state.step_many(max_steps, |_| true)
    }
}

/// Machine status
pub enum Status {
    /// Evaluating normally
    Eval,

    /// Input has been requested by the PVM
    Input,
}
