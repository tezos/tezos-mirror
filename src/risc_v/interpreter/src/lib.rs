// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

pub mod devicetree;
pub mod exec_env;
mod interpreter;
pub mod machine_state;
pub mod parser;
pub mod program;
pub mod state_backend;
pub mod traps;

use crate::{
    machine_state::{
        bus::main_memory::M1G, mode, registers::XRegister, MachineError, MachineState,
        MachineStateLayout, StepManyResult,
    },
    program::Program,
    state_backend::{
        memory_backend::{InMemoryBackend, SliceManager},
        Backend, Layout,
    },
    traps::EnvironException,
};
use derive_more::{Error, From};
use exec_env::{
    posix::{Posix, PosixState},
    ExecutionEnvironment, ExecutionEnvironmentState,
};
use std::collections::BTreeMap;
use InterpreterResult::*;

type StateLayout = (
    <Posix as ExecutionEnvironment>::Layout,
    MachineStateLayout<M1G>,
);

pub struct Interpreter<'a> {
    posix_state: PosixState<SliceManager<'a>>,
    machine_state: MachineState<M1G, SliceManager<'a>>,
}

#[derive(Debug)]
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

    fn init(
        backend: &'a mut InMemoryBackend<StateLayout>,
        mode: mode::Mode,
    ) -> (
        PosixState<SliceManager<'a>>,
        MachineState<M1G, SliceManager<'a>>,
    ) {
        let alloc = backend.allocate(StateLayout::placed().into_location());
        let mut posix_state = PosixState::bind(alloc.0);
        posix_state.set_exit_mode(mode);
        let machine_state = MachineState::<M1G, SliceManager<'a>>::bind(alloc.1);
        (posix_state, machine_state)
    }

    /// Initialise an interpreter with a given [program], starting execution in [mode].
    /// An initial ramdisk can also optionally be passed.
    pub fn new(
        backend: &'a mut InMemoryBackend<StateLayout>,
        program: &[u8],
        initrd: Option<&[u8]>,
        mode: mode::Mode,
    ) -> Result<Self, InterpreterError> {
        let (posix_state, mut machine_state) = Self::init(backend, mode);
        let elf_program = Program::<M1G>::from_elf(program)?;
        machine_state.setup_boot(&elf_program, initrd, mode::Mode::Machine)?;
        Ok(Self {
            posix_state,
            machine_state,
        })
    }

    fn handle_step_result(&mut self, mut result: StepManyResult, max: usize) -> InterpreterResult {
        match result.exception {
            Some(exc) => match self.posix_state.handle_call(&mut self.machine_state, exc) {
                exec_env::EcallOutcome::Fatal => Exception(exc, result.steps),
                exec_env::EcallOutcome::Handled { continue_eval } => {
                    // Handling the ECall marks the completion of a step
                    result.steps = result.steps.saturating_add(1);

                    let steps_left = max.saturating_sub(result.steps);
                    if let Some(code) = self.posix_state.exit_code() {
                        Exit {
                            code: code as usize,
                            steps: result.steps,
                        }
                    } else if continue_eval && steps_left > 0 {
                        self.run_accum(result.steps, steps_left)
                    } else {
                        Running(result.steps)
                    }
                }
            },

            None => Running(result.steps),
        }
    }

    pub fn run(&mut self, max: usize) -> InterpreterResult {
        self.run_accum(0, max)
    }

    /// This function only exists to make the funneling of [steps_done]
    /// tail-recursive.
    fn run_accum(&mut self, steps_done: usize, max: usize) -> InterpreterResult {
        let mut result = self.machine_state.step_many(max, |_| true);
        result.steps = result.steps.saturating_add(steps_done);
        self.handle_step_result(result, max)
    }

    pub fn step_many<F>(&mut self, max: usize, should_continue: F) -> InterpreterResult
    where
        F: FnMut(&MachineState<M1G, SliceManager<'a>>) -> bool,
    {
        let result = self.machine_state.step_many(max, should_continue);
        self.handle_step_result(result, max)
    }

    pub fn read_register(&self, reg: XRegister) -> u64 {
        self.machine_state.hart.xregisters.read(reg)
    }

    pub fn read_pc(&self) -> u64 {
        self.machine_state.hart.pc.read()
    }
}

/// Debugger-specific functions
impl<'a> Interpreter<'a> {
    /// Initialise an interpreter with a given [program], starting execution in [mode].
    /// An initial ramdisk can also optionally be passed. Returns both the interpreter
    /// and the fully parsed program.
    pub fn new_with_parsed_program(
        backend: &'a mut InMemoryBackend<StateLayout>,
        program: &[u8],
        initrd: Option<&[u8]>,
        mode: mode::Mode,
    ) -> Result<(Self, BTreeMap<u64, String>), InterpreterError> {
        let (posix_state, mut machine_state) = Self::init(backend, mode);
        let elf_program = Program::<M1G>::from_elf(program)?;
        machine_state.setup_boot(&elf_program, initrd, mode::Mode::Machine)?;
        Ok((
            Self {
                posix_state,
                machine_state,
            },
            elf_program.parsed(),
        ))
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
