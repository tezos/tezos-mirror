// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

pub mod posix;

use self::posix::PosixState;
use crate::{
    machine_state::{bus::main_memory::MainMemoryLayout, MachineState},
    state_backend::{self, AllocatedOf, Manager},
    traps::EnvironException,
};

pub trait PosixHandler {
    fn handle_call<ML, M, PH>(
        posix_state: &mut PosixState<M>,
        machine: &mut MachineState<ML, M>,
        env_exception: EnvironException,
    ) -> EcallOutcome
    where
        ML: MainMemoryLayout,
        M: state_backend::Manager,
        PH: PosixHandler;
}

/// An execution environment
pub trait ExecutionEnvironment {
    /// Layout of the EE state
    type Layout: state_backend::Layout;

    /// EE state type
    type State<M: Manager>: ExecutionEnvironmentState<M, ExecutionEnvironment = Self>;
}

/// Outcome of handling an ECALL
pub enum EcallOutcome {
    /// Handling the ECALL ended in a fatal error
    Fatal,

    /// ECALL was handled successfully
    Handled { continue_eval: bool },
}

/// State for an execution environment
pub trait ExecutionEnvironmentState<M: Manager> {
    /// Matching execution environment
    type ExecutionEnvironment: ExecutionEnvironment;

    /// Bind the EE state to the allocated regions.
    fn bind(
        space: AllocatedOf<<Self::ExecutionEnvironment as ExecutionEnvironment>::Layout, M>,
    ) -> Self;

    /// Reset the EE state.
    fn reset(&mut self);

    /// Handle a call to the execution environment.
    fn handle_call<ML: MainMemoryLayout>(
        &mut self,
        machine: &mut MachineState<ML, M>,
        exception: EnvironException,
    ) -> EcallOutcome;
}
