// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

pub mod devicetree;
mod interpreter;
pub mod machine_state;
pub mod parser;
pub mod program;
pub mod state_backend;
pub mod traps;

use crate::machine_state::{
    bus::main_memory::M1G,
    mode,
    registers::{a0, a7},
    MachineError, MachineState, MachineStateLayout, StepManyResult,
};
use crate::program::Program;
use crate::state_backend::{
    memory_backend::{InMemoryBackend, SliceManager},
    Backend, Layout,
};
use crate::traps::EnvironException;
use derive_more::{Error, From};
use InterpreterResult::*;

type StateLayout = MachineStateLayout<M1G>;

pub struct Interpreter<'a> {
    machine_state: MachineState<M1G, SliceManager<'a>>,
    steps: usize,
}

pub enum InterpreterResult {
    /// Execution has not finished. Returns the number of steps executed.
    Running(usize),
    /// Program exited. Returns exit code and number of steps executed.
    Exit { code: usize, steps: usize },
    /// Execution finished because an unhandled environment exception has been thrown.
    /// Returns exception and number of steps executed.
    Exception(EnvironException, usize),
}

#[derive(Debug, From, Error, derive_more::Display)]
pub enum InterpreterError {
    KernelLoadingError(kernel_loader::Error),
    MachineError(MachineError),
}

impl<'a> Interpreter<'a> {
    /// In order to create an [Interpreter], a memory backend must first be generated.
    /// Currently, the size of the main memory to be allocated is fixed at 1GB.
    pub fn create_backend() -> InMemoryBackend<StateLayout> {
        InMemoryBackend::<StateLayout>::new().0
    }

    /// Initialise an interpreter with a given [program], starting execution in [mode].
    /// An initial ramdisk can also optionally be passed.
    pub fn new(
        backend: &'a mut InMemoryBackend<StateLayout>,
        program: &[u8],
        initrd: Option<&[u8]>,
        mode: mode::Mode,
    ) -> Result<Self, InterpreterError> {
        let alloc = backend.allocate(StateLayout::placed().into_location());
        let mut machine_state = MachineState::<M1G, SliceManager<'a>>::bind(alloc);
        let elf_program = Program::<M1G>::from_elf(program)?;
        machine_state.setup_boot(&elf_program, initrd, mode)?;
        Ok(Self {
            machine_state,
            steps: 0,
        })
    }

    fn handle_step_result(&mut self, result: StepManyResult) -> InterpreterResult {
        self.steps += result.steps;

        // The only exception currently handled is exit
        if let Some(EnvironException::EnvCallFromUMode) = result.exception {
            if self.machine_state.hart.xregisters.read(a7) == 93 {
                return Exit {
                    code: self.machine_state.hart.xregisters.read(a0) as usize,
                    steps: result.steps,
                };
            }
        }

        match result.exception {
            Some(exception) => Exception(exception, result.steps),
            None => Running(result.steps),
        }
    }

    pub fn run(&mut self, max: usize) -> InterpreterResult {
        let result = self.machine_state.step_many(max, |_| true);
        self.handle_step_result(result)
    }
}

// TODO: remove after updating OCaml bindings
pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
