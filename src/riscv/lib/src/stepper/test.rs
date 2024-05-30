// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::Stepper;
use crate::{
    exec_env::{
        posix::{Posix, PosixState},
        EcallOutcome, ExecutionEnvironment, ExecutionEnvironmentState,
    },
    kernel_loader,
    machine_state::bus::main_memory::{MainMemoryLayout, M1G},
    machine_state::{mode, MachineError, MachineState, MachineStateLayout, StepManyResult},
    program::Program,
    state_backend::{
        memory_backend::{InMemoryBackend, SliceManager},
        Backend, Layout,
    },
    traps::EnvironException,
};
use derive_more::{Error, From};
use std::{collections::BTreeMap, ops::RangeBounds};

#[derive(Clone, Debug)]
pub enum TestStepperResult {
    /// Execution has not finished. Returns the number of steps executed.
    Running(usize),
    /// Program exited. Returns exit code and number of steps executed.
    Exit { code: usize, steps: usize },
    /// Execution finished because an unhandled environment exception has been thrown.
    /// Returns exception and number of steps executed.
    Exception {
        cause: EnvironException,
        steps: usize,
        message: Option<String>,
    },
}

use TestStepperResult::*;

#[derive(Debug, From, Error, derive_more::Display)]
pub enum TestStepperError {
    KernelLoadingError(kernel_loader::Error),
    MachineError(MachineError),
}

type TestStepperLayout<ML = M1G> = (
    <Posix as ExecutionEnvironment>::Layout,
    MachineStateLayout<ML>,
);

pub struct TestStepper<'a, ML: MainMemoryLayout = M1G> {
    machine_state: MachineState<ML, SliceManager<'a>>,
    exec_env_state: PosixState<SliceManager<'a>>,
}

impl<'a, ML: MainMemoryLayout> TestStepper<'a, ML> {
    /// In order to create an [Interpreter], a memory backend must first be generated.
    /// Currently, the size of the main memory to be allocated is fixed at 1GB.
    pub fn create_backend() -> InMemoryBackend<TestStepperLayout<ML>> {
        InMemoryBackend::<TestStepperLayout<ML>>::new().0
    }

    fn bind_states(backend: &'a mut InMemoryBackend<TestStepperLayout<ML>>) -> Self {
        let placed = TestStepperLayout::<ML>::placed().into_location();
        let (exec_env_space, machine_state_space) = backend.allocate(placed);
        let exec_env_state = PosixState::bind(exec_env_space);
        let machine_state = MachineState::bind(machine_state_space);
        Self {
            exec_env_state,
            machine_state,
        }
    }

    /// Initialise an interpreter with a given [program], starting execution in [mode].
    /// An initial ramdisk can also optionally be passed.
    #[inline]
    pub fn new(
        backend: &'a mut InMemoryBackend<TestStepperLayout<ML>>,
        program: &[u8],
        initrd: Option<&[u8]>,
        mode: mode::Mode,
    ) -> Result<Self, TestStepperError> {
        Ok(Self::new_with_parsed_program(backend, program, initrd, mode)?.0)
    }

    /// Initialise an interpreter with a given [program], starting execution in [mode].
    /// An initial ramdisk can also optionally be passed. Returns both the interpreter
    /// and the fully parsed program.
    #[inline]
    pub fn new_with_parsed_program(
        backend: &'a mut InMemoryBackend<TestStepperLayout<ML>>,
        program: &[u8],
        initrd: Option<&[u8]>,
        mode: mode::Mode,
    ) -> Result<(Self, BTreeMap<u64, String>), TestStepperError> {
        let mut stepper = Self::bind_states(backend);

        // By default the Posix EE expects to exit in a specific privilege mode.
        stepper.exec_env_state.set_exit_mode(mode);

        // The interpreter needs a program to run.
        let elf_program = Program::<ML>::from_elf(program)?;
        stepper
            .machine_state
            .setup_boot(&elf_program, initrd, mode::Mode::Machine)?;

        Ok((stepper, elf_program.parsed()))
    }

    fn handle_step_result(
        &mut self,
        result: StepManyResult<(EnvironException, String)>,
    ) -> TestStepperResult {
        match result.error {
            // An error was encountered in the evaluation function.
            Some((cause, error)) => TestStepperResult::Exception {
                cause,
                steps: result.steps,
                message: Some(error),
            },

            // Evaluation function returned without error.
            None => {
                // Check if the machine has exited.
                if let Some(code) = self.exec_env_state.exit_code() {
                    Exit {
                        code: code as usize,
                        steps: result.steps,
                    }
                } else {
                    Running(result.steps)
                }
            }
        }
    }

    /// This function only exists to make the funneling of [steps_done]
    /// tail-recursive.
    fn run_accum<F>(
        &mut self,
        steps_done: usize,
        step_bounds: &impl RangeBounds<usize>,
        mut should_continue: F,
    ) -> TestStepperResult
    where
        F: FnMut(&MachineState<ML, SliceManager<'a>>) -> bool,
    {
        let mut result = self.machine_state.step_range_handle(
            step_bounds,
            &mut should_continue,
            |machine_state, exc| match self.exec_env_state.handle_call(
                machine_state,
                &mut Default::default(),
                exc,
            ) {
                EcallOutcome::Fatal { message } => Err((exc, message)),
                EcallOutcome::Handled { continue_eval } => Ok(continue_eval),
            },
        );
        result.steps = result.steps.saturating_add(steps_done);
        self.handle_step_result(result)
    }

    /// Run at most `max` steps.
    pub fn run(&mut self, max: usize) -> TestStepperResult {
        self.run_accum(0, &..=max, |_| true)
    }

    /// Run as many steps such that they statisfy the given range bound.
    /// The `should_predicate` lets you control when to stop within that range.
    pub fn run_range_while<F>(
        &mut self,
        steps: impl RangeBounds<usize>,
        should_continue: F,
    ) -> TestStepperResult
    where
        F: FnMut(&MachineState<ML, SliceManager<'a>>) -> bool,
    {
        self.run_accum(0, &steps, should_continue)
    }
}

impl<'a, ML: MainMemoryLayout> Stepper for TestStepper<'a, ML> {
    type MainMemoryLayout = ML;

    type Manager = SliceManager<'a>;

    #[inline(always)]
    fn machine_state(&self) -> &MachineState<Self::MainMemoryLayout, Self::Manager> {
        &self.machine_state
    }
}
