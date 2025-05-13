// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

mod interpreter;
mod posix;

use std::collections::BTreeMap;
use std::ops::Bound;

use derive_more::Error;
use derive_more::From;
use posix::PosixState;

use super::StepResult;
use super::Stepper;
use super::StepperStatus;
use crate::kernel_loader;
use crate::machine_state::CacheLayouts;
use crate::machine_state::MachineCoreState;
use crate::machine_state::MachineError;
use crate::machine_state::MachineState;
use crate::machine_state::StepManyResult;
use crate::machine_state::TestCacheLayouts;
use crate::machine_state::block_cache::block::Block;
use crate::machine_state::block_cache::block::Interpreted;
use crate::machine_state::memory::M1G;
use crate::machine_state::memory::Memory;
use crate::machine_state::memory::MemoryConfig;
use crate::machine_state::memory::Permissions;
use crate::program::Program;
use crate::state::NewState;
use crate::state_backend::owned_backend::Owned;
use crate::traps::EnvironException;

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

pub struct TestStepper<
    MC: MemoryConfig = M1G,
    CL: CacheLayouts = TestCacheLayouts,
    B: Block<MC, Owned> = Interpreted<MC, Owned>,
> {
    machine_state: MachineState<MC, CL, B, Owned>,
    posix_state: PosixState<Owned>,
}

impl<MC: MemoryConfig, B: Block<MC, Owned>> TestStepper<MC, TestCacheLayouts, B> {
    /// Initialise an interpreter with a given `program`.
    /// An initial ramdisk can also optionally be passed.
    #[inline]
    pub fn new(
        program: &[u8],
        initrd: Option<&[u8]>,
        block_builder: B::BlockBuilder,
    ) -> Result<Self, TestStepperError> {
        Ok(Self::new_with_parsed_program(program, initrd, block_builder)?.0)
    }

    /// Consumes the stepper, returning the [`BlockBuilder`] used internally.
    ///
    /// This allows the block builder to be re-used with a second stepper.
    ///
    /// [`BlockBuilder`]: Block::BlockBuilder
    pub fn recover_builder(self) -> B::BlockBuilder {
        self.machine_state.block_cache.block_builder
    }

    /// Initialise an interpreter with a given `program`.
    /// An initial ramdisk can also optionally be passed. Returns both the interpreter
    /// and the fully parsed program.
    #[inline]
    pub fn new_with_parsed_program(
        program: &[u8],
        initrd: Option<&[u8]>,
        block_builder: B::BlockBuilder,
    ) -> Result<(Self, BTreeMap<u64, String>), TestStepperError> {
        let mut stepper = Self {
            posix_state: PosixState::new(&mut Owned),
            machine_state: MachineState::new(&mut Owned, block_builder),
        };

        // The interpreter needs a program to run.
        let elf_program = Program::<MC>::from_elf(program)?;

        stepper
            .machine_state
            .core
            .main_memory
            .protect_pages(0, MC::TOTAL_BYTES, Permissions::ReadWriteExec)
            .unwrap();

        stepper.machine_state.setup_boot(&elf_program, initrd)?;

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
                    .handle_call(machine_state)
                    .map_err(|message| (exc, message))
            });
        self.handle_step_result(result)
    }
}
