//! Traps doc
//! There are 4 types of traps, depending on where they are handled and visibility to the hart.
//! ### Contained:
//! A trap which is handled by the normal procedure of
//! trap handling without interacting with the execution environment.
//! (Software knows a trap is taken e.g. U -> M/S, S -> M/S, M -> M)
//!
//! ### Requested:
//! A trap requested by the software to the execution environment.
//! so the software is aware of traps like U/S/M -> EE -> M/S
//!
//! ### Invisible:
//! A trap is handled by the execution environment without software being aware of this.
//!
//! ### Fatal:
//! A trap which causes the execution environment to halt the machine.
//!

/// RISC-V Exceptions (also known as synchronous exceptions)
#[derive(PartialEq, Eq, thiserror::Error, strum::Display, Debug)]
pub enum EnvironException {
    EnvCallFromUMode,
    EnvCallFromSMode,
    EnvCallFromMMode,
}

/// RISC-V Exceptions (also known as synchronous exceptions)
#[derive(PartialEq, Eq, thiserror::Error, strum::Display, Debug)]
pub enum Exception {
    InstructionAccessFault,
    IllegalInstruction,
    Breakpoint,
    LoadAccessFault,
    StoreAccessFault,
    EnvCallFromUMode,
    EnvCallFromSMode,
    EnvCallFromMMode,
}

/// RISC-V Interrupts (also known as asynchronous exceptions)
#[derive(PartialEq, Eq, thiserror::Error, strum::Display, Debug)]
pub enum Interrupt {
    SupervisorSoftware,
    MachineSoftware,
    SupervisorTimer,
    MachineTimer,
    SupervisorExternal,
}

impl TryFrom<Exception> for EnvironException {
    type Error = &'static str;

    fn try_from(value: Exception) -> Result<Self, Self::Error> {
        match value {
            Exception::EnvCallFromUMode => Ok(EnvironException::EnvCallFromUMode),
            Exception::EnvCallFromSMode => Ok(EnvironException::EnvCallFromSMode),
            Exception::EnvCallFromMMode => Ok(EnvironException::EnvCallFromMMode),
            Exception::Breakpoint
            | Exception::IllegalInstruction
            | Exception::InstructionAccessFault
            | Exception::LoadAccessFault
            | Exception::StoreAccessFault => {
                Err("Execution environment supports only ecall exceptions")
            }
        }
    }
}
