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

use crate::machine_state::{bus::Address, csregisters::CSRValue};

/// RISC-V Exceptions (also known as synchronous exceptions)
#[derive(PartialEq, Eq, thiserror::Error, strum::Display, Debug)]
pub enum EnvironException {
    EnvCallFromUMode,
    EnvCallFromSMode,
    EnvCallFromMMode,
    TrapFromDMode,
}

impl TryFrom<&Exception> for EnvironException {
    type Error = &'static str;

    fn try_from(value: &Exception) -> Result<Self, Self::Error> {
        match value {
            Exception::EnvCallFromUMode => Ok(EnvironException::EnvCallFromUMode),
            Exception::EnvCallFromSMode => Ok(EnvironException::EnvCallFromSMode),
            Exception::EnvCallFromMMode => Ok(EnvironException::EnvCallFromMMode),
            Exception::Breakpoint
            | Exception::IllegalInstruction
            | Exception::InstructionAccessFault(_)
            | Exception::LoadAccessFault(_)
            | Exception::StoreAccessFault(_) => {
                Err("Execution environment supports only ecall exceptions")
            }
        }
    }
}

/// RISC-V Exceptions (also known as synchronous exceptions)
#[derive(PartialEq, Eq, thiserror::Error, strum::Display, Debug)]
pub enum Exception {
    /// `InstructionAccessFault(addr)` where `addr` is the faulting instruction address
    InstructionAccessFault(Address),
    IllegalInstruction,
    Breakpoint,
    /// `InstructionAccessFault(addr)` where `addr` is the faulting load address
    LoadAccessFault(Address),
    /// `InstructionAccessFault(addr)` where `addr` is the faulting store address
    StoreAccessFault(Address),
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
    MachineExternal,
}

impl Interrupt {
    /// Bitmask of all supervisor interrupts
    pub const SUPERVISOR_BIT_MASK: CSRValue = 1
        << Interrupt::SupervisorSoftware.exception_code_const()
        | 1 << Interrupt::SupervisorTimer.exception_code_const()
        | 1 << Interrupt::SupervisorExternal.exception_code_const();

    /// Bitmask of all machine interrupts
    pub const MACHINE_BIT_MASK: CSRValue = 1 << Interrupt::MachineSoftware.exception_code_const()
        | 1 << Interrupt::MachineTimer.exception_code_const()
        | 1 << Interrupt::MachineExternal.exception_code_const();

    /// Exception code of interrupts
    pub const fn exception_code_const(&self) -> CSRValue {
        match self {
            Interrupt::SupervisorSoftware => 1,
            Interrupt::MachineSoftware => 3,
            Interrupt::SupervisorTimer => 5,
            Interrupt::MachineTimer => 7,
            Interrupt::SupervisorExternal => 9,
            Interrupt::MachineExternal => 11,
        }
    }
}

/// Common trait for [`Exception`] & [`Interrupt`] traps used in the context of trap handling
pub trait TrapContext {
    /// Trap values to be stored in `xtval` registers when taking a trap.
    /// See sections 3.1.16 & 5.1.9
    fn xtval(&self) -> CSRValue;

    /// Code of trap (exception / interrupt) also known as cause, given by tables 3.6 & 5.2
    /// NOTE: This value does NOT include the interrupt bit
    fn exception_code(&self) -> CSRValue;

    /// xcause value a.k.a. what should be written to `xcause` register.
    /// NOTE: this values DOES include the interrupt bit
    fn xcause(&self) -> CSRValue;

    /// Computes the address pc should be set when entering the trap
    fn trap_handler_address(&self, xtvec_val: CSRValue) -> Address;
}

impl TrapContext for Exception {
    fn exception_code(&self) -> CSRValue {
        match self {
            Exception::InstructionAccessFault(_) => 1,
            Exception::IllegalInstruction => 2,
            Exception::Breakpoint => 3,
            Exception::LoadAccessFault(_) => 5,
            Exception::StoreAccessFault(_) => 7,
            Exception::EnvCallFromUMode => 8,
            Exception::EnvCallFromSMode => 9,
            Exception::EnvCallFromMMode => 11,
        }
    }

    fn xcause(&self) -> CSRValue {
        self.exception_code()
    }

    fn xtval(&self) -> CSRValue {
        match self {
            Exception::IllegalInstruction
            | Exception::Breakpoint
            | Exception::EnvCallFromUMode
            | Exception::EnvCallFromSMode
            | Exception::EnvCallFromMMode => 0,
            Exception::InstructionAccessFault(addr) => *addr,
            Exception::LoadAccessFault(addr) => *addr,
            Exception::StoreAccessFault(addr) => *addr,
        }
    }

    fn trap_handler_address(&self, xtvec_val: CSRValue) -> Address {
        // MODE = xtvec[1:0]
        // BASE[xLEN-1:2] = xtvec[xLEN-1:2]
        xtvec_val & !0b11
    }
}

impl TrapContext for Interrupt {
    fn xtval(&self) -> CSRValue {
        0
    }

    fn exception_code(&self) -> CSRValue {
        self.exception_code_const()
    }

    fn xcause(&self) -> CSRValue {
        let interrupt_bit = 1 << (CSRValue::BITS - 1);
        interrupt_bit | self.exception_code()
    }

    fn trap_handler_address(&self, xtvec_val: CSRValue) -> Address {
        // MODE = xtvec[1:0]
        // BASE[xLEN-1:2] = xtvec[xLEN-1:2]
        let xtvec_mode = xtvec_val & 0b11;
        let xtvec_base = xtvec_val & !0b11;
        let handler_offset = match xtvec_mode {
            // Vectored mode
            1 => 4 * self.exception_code(),
            // Direct or Reserved mode
            _ => 0,
        };

        println!(
            "Trap address: {:x}, {}",
            xtvec_base + handler_offset,
            self.exception_code()
        );

        xtvec_base + handler_offset
    }
}
