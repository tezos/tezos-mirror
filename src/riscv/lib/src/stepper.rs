// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::ops::AddAssign;
use std::ops::Bound;

use crate::machine_state::CacheLayouts;
use crate::machine_state::MachineCoreState;
use crate::machine_state::memory::MemoryConfig;
use crate::state_backend::ManagerBase;
use crate::state_backend::ManagerRead;

pub mod pvm;
#[cfg(test)]
mod test;

/// Status of a stepper
#[derive(Clone, Debug, PartialEq, Eq)]
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

impl StepperStatus {
    /// Get the number of steps taken so far.
    pub fn steps(&self) -> usize {
        *match self {
            StepperStatus::Running { steps } => steps,
            StepperStatus::Exited { steps, .. } => steps,
            StepperStatus::Errored { steps, .. } => steps,
        }
    }
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
    /// Memory config of the underlying machine state
    type MemoryConfig: MemoryConfig;

    /// Layout of the instruction cache
    type CacheLayouts: CacheLayouts;

    /// State backend with which the stepper was instantiated
    type Manager: ManagerBase + ManagerRead;

    /// Obtain a reference to the underlying machine state.
    fn machine_state(&self) -> &MachineCoreState<Self::MemoryConfig, Self::Manager>;

    /// Result of one or more steps
    type StepResult: StepResult;

    /// Run as many steps as possible but not more than `max`.
    fn step_max(&mut self, max: Bound<usize>) -> Self::StepResult;
}
