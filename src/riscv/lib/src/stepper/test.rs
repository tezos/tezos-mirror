// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

mod posix;

use std::{collections::BTreeMap, ops::Bound};

use derive_more::{Error, From};
use posix::{PosixState, PosixStateLayout};

use super::{StepResult, Stepper, StepperStatus};
use crate::{
    kernel_loader,
    machine_state::{
        CacheLayouts, MachineCoreState, MachineError, MachineState, MachineStateLayout,
        StepManyResult, TestCacheLayouts,
        block_cache::bcall::{Block, Interpreted},
        memory::{M1G, MemoryConfig},
        mode,
    },
    program::Program,
    state_backend::owned_backend::Owned,
    traps::EnvironException,
};

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

pub type TestStepperLayout<MC = M1G, CL = TestCacheLayouts> =
    (PosixStateLayout, MachineStateLayout<MC, CL>);

pub struct TestStepper<
    MC: MemoryConfig = M1G,
    CL: CacheLayouts = TestCacheLayouts,
    B: Block<MC, Owned> = Interpreted<MC, Owned>,
> {
    machine_state: MachineState<MC, CL, B, Owned>,
    posix_state: PosixState<Owned>,
}

impl<MC: MemoryConfig, B: Block<MC, Owned>> TestStepper<MC, TestCacheLayouts, B> {
    /// Initialise an interpreter with a given `program`, starting execution in [mode].
    /// An initial ramdisk can also optionally be passed.
    #[inline]
    pub fn new(
        program: &[u8],
        initrd: Option<&[u8]>,
        mode: mode::Mode,
        block_builder: B::BlockBuilder,
    ) -> Result<Self, TestStepperError> {
        Ok(Self::new_with_parsed_program(program, initrd, mode, block_builder)?.0)
    }

    /// Initialise an interpreter with a given `program`, starting execution in [mode::Mode].
    /// An initial ramdisk can also optionally be passed. Returns both the interpreter
    /// and the fully parsed program.
    #[inline]
    pub fn new_with_parsed_program(
        program: &[u8],
        initrd: Option<&[u8]>,
        mode: mode::Mode,
        block_builder: B::BlockBuilder,
    ) -> Result<(Self, BTreeMap<u64, String>), TestStepperError> {
        let (posix_space, machine_state_space) = Owned::allocate::<TestStepperLayout<MC>>();
        let posix_state = PosixState::bind(posix_space);
        let machine_state = MachineState::bind(machine_state_space, block_builder);
        let mut stepper = Self {
            posix_state,
            machine_state,
        };

        // By default the Posix EE expects to exit in a specific privilege mode.
        stepper.posix_state.set_exit_mode(mode);

        // The interpreter needs a program to run.
        let elf_program = Program::<MC>::from_elf(program)?;
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

impl<MC: MemoryConfig, B: Block<MC, Owned>> Stepper for TestStepper<MC, TestCacheLayouts, B> {
    type MemoryConfig = MC;

    type CacheLayouts = TestCacheLayouts;

    type Manager = Owned;

    #[inline(always)]
    fn machine_state(&self) -> &MachineCoreState<Self::MemoryConfig, Self::Manager> {
        &self.machine_state.core
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
