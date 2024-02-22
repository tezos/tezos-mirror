// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

// Allow dead code while this module contains stubs.
#![allow(dead_code)]

use risc_v_interpreter::{
    machine_state::{self, MachineState, StepManyResult},
    state_backend,
    traps::EnvironException,
};

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

    fn execution_environment_trap_handler(
        _machine_state: &MachineState<MemorySize, M>,
        _exception: EnvironException,
    ) {
        // TODO: https://app.asana.com/0/1206655199123740/1206682246825814/f
        todo!("PVM Trap handler for execution environment traps not implemented")
    }

    /// Perform one step. Returns `false` if the PVM is not in [`Status::Eval`] status.
    pub fn step(&mut self) -> bool {
        if let Err(exc) = self.machine_state.step() {
            Pvm::execution_environment_trap_handler(&self.machine_state, exc)
        };
        true
    }

    /// Perform at most `max_steps` steps. Returns the actual number of steps
    /// performed (retired instructions)
    ///
    /// If an environment trap is raised, handle it and
    /// return the number of retired instructions until the raised trap
    ///
    /// NOTE: instructions which raise exceptions / are interrupted are NOT retired
    ///       See section 3.3.1 for context on retired instructions.
    /// e.g: a load instruction raises an exception but the first instruction
    /// of the trap handler will be executed and retired,
    /// so in the end the load instruction which does not bubble it's exception up to
    /// the execution environment will still retire an instruction, just not itself.
    /// (a possible case: the privilege mode access violation is treated in EE,
    /// but a page fault is not)
    pub fn step_many(&mut self, max_steps: usize) -> usize {
        let StepManyResult { steps, exception } = self.machine_state.step_many(max_steps, |_| true);

        if let Some(exc) = exception {
            Pvm::execution_environment_trap_handler(&self.machine_state, exc)
        }

        steps
    }
}

/// Machine status
pub enum Status {
    /// Evaluating normally
    Eval,

    /// Input has been requested by the PVM
    Input,
}
