// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{Stepper, StepperStatus};
use crate::{
    kernel_loader,
    machine_state::{
        bus::main_memory::{MainMemoryLayout, M1G},
        mode::Mode,
        CacheLayouts, DefaultCacheLayouts, MachineCoreState, MachineError,
    },
    program::Program,
    pvm::{Pvm, PvmHooks, PvmLayout, PvmStatus},
    range_utils::bound_saturating_sub,
    state_backend::owned_backend::Owned,
};
use std::ops::Bound;
use tezos_smart_rollup_utils::inbox::Inbox;

/// Error during PVM stepping
#[derive(Debug, derive_more::From, thiserror::Error, derive_more::Display)]
pub enum PvmStepperError {
    /// Errors related to the machine state
    MachineError(MachineError),

    /// Errors arising from loading the kernel
    KernelError(kernel_loader::Error),
}

/// Wrapper over a PVM that lets you step through it
pub struct PvmStepper<'hooks, ML: MainMemoryLayout = M1G, CL: CacheLayouts = DefaultCacheLayouts> {
    pvm: Pvm<ML, CL, Owned>,
    hooks: PvmHooks<'hooks>,
    inbox: Inbox,
    rollup_address: [u8; 20],
    origination_level: u32,
}

impl<'hooks, ML: MainMemoryLayout, CL: CacheLayouts> PvmStepper<'hooks, ML, CL> {
    /// Create a new PVM stepper.
    pub fn new(
        program: &[u8],
        initrd: Option<&[u8]>,
        inbox: Inbox,
        hooks: PvmHooks<'hooks>,
        rollup_address: [u8; 20],
        origination_level: u32,
    ) -> Result<Self, PvmStepperError> {
        let space = Owned::allocate::<PvmLayout<ML, CL>>();
        let mut pvm = Pvm::bind(space);

        let program = Program::<ML>::from_elf(program)?;
        pvm.machine_state
            .setup_boot(&program, initrd, Mode::Supervisor)?;

        Ok(Self {
            pvm,
            hooks,
            inbox,
            rollup_address,
            origination_level,
        })
    }

    /// Non-continuing variant of [`Stepper::step_max`]
    fn step_max_once(&mut self, steps: Bound<usize>) -> StepperStatus {
        match self.pvm.status() {
            PvmStatus::Evaluating => {
                let steps = self.pvm.eval_max(&mut self.hooks, steps);
                StepperStatus::Running { steps }
            }

            PvmStatus::WaitingForInput => match self.inbox.next() {
                Some((level, counter, payload)) => {
                    let success = self.pvm.provide_input(level, counter, payload.as_slice());

                    if success {
                        StepperStatus::Running { steps: 1 }
                    } else {
                        StepperStatus::Errored {
                            steps: 0,
                            cause: "PVM was waiting for input".to_owned(),
                            message: "Providing input did not succeed".to_owned(),
                        }
                    }
                }

                None => {
                    if self.inbox.none_count() < 2 {
                        self.pvm.provide_no_input();
                        StepperStatus::Running { steps: 1 }
                    } else {
                        StepperStatus::Exited {
                            steps: 0,
                            success: true,
                            status: "Inbox has been drained".to_owned(),
                        }
                    }
                }
            },

            PvmStatus::WaitingForMetadata => {
                let success = self
                    .pvm
                    .provide_metadata(&self.rollup_address, self.origination_level);

                if success {
                    StepperStatus::Running { steps: 1 }
                } else {
                    StepperStatus::Errored {
                        steps: 0,
                        cause: "PVM was waiting for metadata".to_owned(),
                        message: "Providing metadata did not succeed".to_owned(),
                    }
                }
            }
        }
    }
}

impl<'hooks, ML: MainMemoryLayout, CL: CacheLayouts> Stepper for PvmStepper<'hooks, ML, CL> {
    type MainMemoryLayout = ML;

    type CacheLayouts = CL;

    type Manager = Owned;

    fn machine_state(&self) -> &MachineCoreState<Self::MainMemoryLayout, Self::Manager> {
        &self.pvm.machine_state.core
    }

    type StepResult = StepperStatus;

    fn step_max(&mut self, mut step_bounds: Bound<usize>) -> Self::StepResult {
        let mut total_steps = 0usize;

        loop {
            match self.step_max_once(step_bounds) {
                StepperStatus::Running { steps } => {
                    total_steps = total_steps.saturating_add(steps);
                    step_bounds = bound_saturating_sub(step_bounds, steps);

                    if steps < 1 {
                        // Break if no progress has been made.
                        break StepperStatus::Running { steps: total_steps };
                    }
                }

                StepperStatus::Exited {
                    steps,
                    success,
                    status,
                } => {
                    break StepperStatus::Exited {
                        steps: total_steps.saturating_add(steps),
                        success,
                        status,
                    }
                }

                StepperStatus::Errored {
                    steps,
                    cause,
                    message,
                } => {
                    break StepperStatus::Errored {
                        steps: total_steps.saturating_add(steps),
                        cause,
                        message,
                    };
                }
            }
        }
    }
}
