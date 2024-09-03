// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::{
    machine_state::{bus::main_memory::MainMemoryLayout, MachineState},
    state_backend::ManagerBase,
};
use std::ops::{AddAssign, Bound};

pub mod pvm;
pub mod test;

/// Status of a stepper
#[derive(Clone, Debug)]
pub enum StepperStatus {
    /// Stepper is still running.
    Running { steps: usize },

    /// Stepper exited.
    Exited {
        steps: usize,
        success: bool,
        status: String,
    },

    /// Stepper errored.
    Errored {
        steps: usize,
        cause: String,
        message: String,
    },
}

impl Default for StepperStatus {
    fn default() -> Self {
        Self::Running { steps: 0 }
    }
}

impl AddAssign for StepperStatus {
    fn add_assign(&mut self, mut rhs: Self) {
        match self {
            StepperStatus::Running { steps: prev_steps } => match &mut rhs {
                StepperStatus::Running { steps }
                | StepperStatus::Exited { steps, .. }
                | StepperStatus::Errored { steps, .. } => {
                    *steps = steps.saturating_add(*prev_steps);
                }
            },

            StepperStatus::Exited { .. } | StepperStatus::Errored { .. } => return,
        }

        *self = rhs;
    }
}

/// Result after performing a number of steps
pub trait StepResult: Default {
    /// Retrieve the status of the stepper
    fn to_stepper_status(&self) -> StepperStatus;
}

impl StepResult for StepperStatus {
    #[inline(always)]
    fn to_stepper_status(&self) -> StepperStatus {
        self.clone()
    }
}

/// Interface for a debuggable stepper
pub trait Stepper {
    /// Memory layout of the underlying machine state
    type MainMemoryLayout: MainMemoryLayout;

    /// State backend with which the stepper was instantiated
    type Manager: ManagerBase;

    /// Obtain a reference to the underlying machine state.
    fn machine_state(&self) -> &MachineState<Self::MainMemoryLayout, Self::Manager>;

    /// Result of one or more steps
    type StepResult: StepResult;

    /// Run as many steps as possible but not more than `max`.
    fn step_max(&mut self, max: Bound<usize>) -> Self::StepResult;
}
