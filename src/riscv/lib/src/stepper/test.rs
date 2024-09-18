// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

mod posix;

use super::{StepResult, Stepper, StepperStatus};
use crate::{
    kernel_loader,
    machine_state::{
        bus::main_memory::{MainMemoryLayout, M1G},
        instruction_cache::{InstructionCacheLayout, TestInstructionCacheLayout},
    },
    machine_state::{mode, MachineError, MachineState, MachineStateLayout, StepManyResult},
    program::Program,
    state_backend::owned_backend::Owned,
    traps::EnvironException,
};
use derive_more::{Error, From};
use posix::{PosixState, PosixStateLayout};
use std::{collections::BTreeMap, ops::Bound};

#[derive(Clone, Debug)]
pub enum TestStepperResult {
    /// Execution has not finished. Returns the number of steps executed.
    Running { steps: usize },
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

impl Default for TestStepperResult {
    fn default() -> Self {
        Self::Running { steps: 0 }
    }
}

impl StepResult for TestStepperResult {
    fn to_stepper_status(&self) -> StepperStatus {
        match self {
            Running { steps } => StepperStatus::Running { steps: *steps },
            Exit { code, steps } => StepperStatus::Exited {
                steps: *steps,
                success: *code == 0,
                status: format!("code {code}"),
            },
            Exception {
                cause,
                steps,
                message,
            } => StepperStatus::Errored {
                steps: *steps,
                cause: format!("{cause:?}"),
                message: message.as_deref().unwrap_or("<no message>").to_owned(),
            },
        }
    }
}

use TestStepperResult::*;

#[derive(Debug, From, Error, derive_more::Display)]
pub enum TestStepperError {
    KernelLoadingError(kernel_loader::Error),
    MachineError(MachineError),
}

pub type TestStepperLayout<ML = M1G, ICL = TestInstructionCacheLayout> =
    (PosixStateLayout, MachineStateLayout<ML, ICL>);

pub struct TestStepper<
    ML: MainMemoryLayout = M1G,
    ICL: InstructionCacheLayout = TestInstructionCacheLayout,
> {
    machine_state: MachineState<ML, ICL, Owned>,
    posix_state: PosixState<Owned>,
}

impl<ML: MainMemoryLayout> TestStepper<ML> {
    /// Initialise an interpreter with a given [program], starting execution in [mode].
    /// An initial ramdisk can also optionally be passed.
    #[inline]
    pub fn new(
        program: &[u8],
        initrd: Option<&[u8]>,
        mode: mode::Mode,
    ) -> Result<Self, TestStepperError> {
        Ok(Self::new_with_parsed_program(program, initrd, mode)?.0)
    }

    /// Initialise an interpreter with a given [program], starting execution in [mode].
    /// An initial ramdisk can also optionally be passed. Returns both the interpreter
    /// and the fully parsed program.
    #[inline]
    pub fn new_with_parsed_program(
        program: &[u8],
        initrd: Option<&[u8]>,
        mode: mode::Mode,
    ) -> Result<(Self, BTreeMap<u64, String>), TestStepperError> {
        let (posix_space, machine_state_space) = Owned::allocate::<TestStepperLayout<ML>>();
        let posix_state = PosixState::bind(posix_space);
        let machine_state = MachineState::bind(machine_state_space);
        let mut stepper = Self {
            posix_state,
            machine_state,
        };

        // By default the Posix EE expects to exit in a specific privilege mode.
        stepper.posix_state.set_exit_mode(mode);

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
                if let Some(code) = self.posix_state.exit_code() {
                    Exit {
                        code: code as usize,
                        steps: result.steps,
                    }
                } else {
                    Running {
                        steps: result.steps,
                    }
                }
            }
        }
    }
}

impl<ML: MainMemoryLayout> Stepper for TestStepper<ML, TestInstructionCacheLayout> {
    type MainMemoryLayout = ML;

    type InstructionCacheLayout = TestInstructionCacheLayout;

    type Manager = Owned;

    #[inline(always)]
    fn machine_state(
        &self,
    ) -> &MachineState<Self::MainMemoryLayout, Self::InstructionCacheLayout, Self::Manager> {
        &self.machine_state
    }

    type StepResult = TestStepperResult;

    fn step_max(&mut self, steps: Bound<usize>) -> Self::StepResult {
        let result = self
            .machine_state
            .step_max_handle(steps, |machine_state, exc| {
                self.posix_state
                    .handle_call(machine_state, exc)
                    .map_err(|message| (exc, message))
            });
        self.handle_step_result(result)
    }
}
