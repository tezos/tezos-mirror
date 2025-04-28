// SPDX-FileCopyrightText: 2023-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

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

use std::fmt::Formatter;

use tezos_smart_rollup_constants::riscv::SbiError;

use crate::machine_state::csregisters::CSRRepr;
use crate::machine_state::memory::Address;

/// RISC-V Exceptions (also known as synchronous exceptions)
#[derive(PartialEq, Eq, thiserror::Error, strum::Display, Debug, Clone, Copy)]
pub enum EnvironException {
    EnvCall,
}

impl EnvironException {
    /// Convert from environment exception.
    pub fn as_exception(&self) -> Exception {
        // This is not implemeted as a `From<_>` implementation to not make `?`
        // syntax harder which unfortunately tries to convert between `Exception` and
        // `EnvironException` when "circular" implementations exist. These cycles
        // can only be broken with explicit type annotations which is annoying.

        match self {
            Self::EnvCall => Exception::EnvCall,
        }
    }
}

impl TryFrom<&Exception> for EnvironException {
    type Error = &'static str;

    fn try_from(value: &Exception) -> Result<Self, Self::Error> {
        match value {
            Exception::EnvCall => Ok(EnvironException::EnvCall),
            Exception::Breakpoint
            | Exception::IllegalInstruction
            | Exception::InstructionAccessFault(_)
            | Exception::LoadAccessFault(_)
            | Exception::StoreAMOAccessFault(_)
            | Exception::InstructionPageFault(_)
            | Exception::LoadPageFault(_)
            | Exception::StoreAMOPageFault(_) => {
                Err("Execution environment supports only ecall exceptions")
            }
        }
    }
}

/// RISC-V Exceptions (also known as synchronous exceptions)
#[derive(PartialEq, Eq, thiserror::Error, strum::Display, Clone, Copy)]
pub enum Exception {
    /// `InstructionAccessFault(addr)` where `addr` is the faulting instruction address
    InstructionAccessFault(Address),
    IllegalInstruction,
    Breakpoint,
    /// `LoadAccessFault(addr)` where `addr` is the faulting load address
    LoadAccessFault(Address),
    /// `StoreAccessFault(addr)` where `addr` is the faulting store address
    StoreAMOAccessFault(Address),
    EnvCall,
    InstructionPageFault(Address),
    LoadPageFault(Address),
    StoreAMOPageFault(Address),
}

impl core::fmt::Debug for Exception {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::InstructionPageFault(adr) => write!(f, "InstructionPageFault({adr:#X})"),
            Self::LoadPageFault(adr) => write!(f, "LoadPageFault({adr:#X})"),
            Self::StoreAMOPageFault(adr) => write!(f, "StoreAMOPageFault({adr:#X})"),
            Self::LoadAccessFault(adr) => write!(f, "LoadAccessFault({adr:#X})"),
            other => write!(f, "{other}"),
        }
    }
}

impl From<Exception> for SbiError {
    fn from(value: Exception) -> Self {
        match value {
            Exception::InstructionAccessFault(_)
            | Exception::InstructionPageFault(_)
            | Exception::LoadAccessFault(_)
            | Exception::LoadPageFault(_)
            | Exception::StoreAMOAccessFault(_)
            | Exception::StoreAMOPageFault(_) => SbiError::InvalidAddress,
            Exception::IllegalInstruction | Exception::Breakpoint | Exception::EnvCall => {
                SbiError::Failed
            }
        }
    }
}

/// RISC-V Interrupts (also known as asynchronous exceptions)
#[derive(PartialEq, Eq, thiserror::Error, strum::Display, Debug, Copy, Clone)]
pub enum Interrupt {
    SupervisorSoftware,
    MachineSoftware,
    SupervisorTimer,
    MachineTimer,
    SupervisorExternal,
    MachineExternal,
}

impl Interrupt {
    pub const SUPERVISOR_SOFTWARE_EXCEPTION_CODE: CSRRepr = 1;
    pub const MACHINE_SOFTWARE_EXCEPTION_CODE: CSRRepr = 3;
    pub const SUPERVISOR_TIMER_EXCEPTION_CODE: CSRRepr = 5;
    pub const MACHINE_TIMER_EXCEPTION_CODE: CSRRepr = 7;
    pub const SUPERVISOR_EXTERNAL_EXCEPTION_CODE: CSRRepr = 9;
    pub const MACHINE_EXTERNAL_EXCEPTION_CODE: CSRRepr = 11;

    /// Bitmask of all supervisor interrupts
    pub const SUPERVISOR_BIT_MASK: CSRRepr = (1
        << Interrupt::SupervisorSoftware.exception_code_const())
        | (1 << Interrupt::SupervisorTimer.exception_code_const())
        | (1 << Interrupt::SupervisorExternal.exception_code_const());

    /// Bitmask of all machine interrupts
    pub const MACHINE_BIT_MASK: CSRRepr = (1 << Interrupt::MachineSoftware.exception_code_const())
        | (1 << Interrupt::MachineTimer.exception_code_const())
        | (1 << Interrupt::MachineExternal.exception_code_const());

    /// Exception code of interrupts
    pub const fn exception_code_const(&self) -> CSRRepr {
        match self {
            Interrupt::SupervisorSoftware => Self::SUPERVISOR_SOFTWARE_EXCEPTION_CODE,
            Interrupt::MachineSoftware => Self::MACHINE_SOFTWARE_EXCEPTION_CODE,
            Interrupt::SupervisorTimer => Self::SUPERVISOR_TIMER_EXCEPTION_CODE,
            Interrupt::MachineTimer => Self::MACHINE_TIMER_EXCEPTION_CODE,
            Interrupt::SupervisorExternal => Self::SUPERVISOR_EXTERNAL_EXCEPTION_CODE,
            Interrupt::MachineExternal => Self::MACHINE_EXTERNAL_EXCEPTION_CODE,
        }
    }
}

/// Flavour of the trap cause
pub enum TrapKind {
    Interrupt,
    Exception,
}

/// Common trait for [`Exception`] & [`Interrupt`] traps used in the context of trap handling
pub trait TrapContext {
    /// Trap values to be stored in `xtval` registers when taking a trap.
    /// See sections 3.1.16 & 5.1.9
    fn xtval(&self) -> CSRRepr;

    /// Code of trap (exception / interrupt) also known as cause, given by tables 3.6 & 5.2
    /// NOTE: This value does NOT include the interrupt bit
    fn exception_code(&self) -> CSRRepr;

    /// xcause value a.k.a. what should be written to `xcause` register.
    /// NOTE: this values DOES include the interrupt bit
    fn xcause(&self) -> CSRRepr;

    /// Computes the address pc should be set when entering the trap
    fn trap_handler_address(&self, xtvec_val: CSRRepr) -> Address;

    /// Obtain the kind that would cause this trap.
    fn kind() -> TrapKind;
}

impl TrapContext for Exception {
    fn exception_code(&self) -> CSRRepr {
        match self {
            Exception::InstructionAccessFault(_) => 1,
            Exception::IllegalInstruction => 2,
            Exception::Breakpoint => 3,
            Exception::LoadAccessFault(_) => 5,
            Exception::StoreAMOAccessFault(_) => 7,
            Exception::EnvCall => 8,
            Exception::InstructionPageFault(_) => 12,
            Exception::LoadPageFault(_) => 13,
            Exception::StoreAMOPageFault(_) => 15,
        }
    }

    fn xcause(&self) -> CSRRepr {
        self.exception_code()
    }

    fn xtval(&self) -> CSRRepr {
        match self {
            Exception::IllegalInstruction | Exception::Breakpoint | Exception::EnvCall => 0,
            Exception::InstructionAccessFault(addr) => *addr,
            Exception::LoadAccessFault(addr) => *addr,
            Exception::StoreAMOAccessFault(addr) => *addr,
            Exception::InstructionPageFault(addr) => *addr,
            Exception::LoadPageFault(addr) => *addr,
            Exception::StoreAMOPageFault(addr) => *addr,
        }
    }

    fn trap_handler_address(&self, xtvec_val: CSRRepr) -> Address {
        // MODE = xtvec[1:0]
        // BASE[xLEN-1:2] = xtvec[xLEN-1:2]
        xtvec_val & !0b11
    }

    fn kind() -> TrapKind {
        TrapKind::Exception
    }
}

impl TrapContext for Interrupt {
    fn xtval(&self) -> CSRRepr {
        0
    }

    fn exception_code(&self) -> CSRRepr {
        self.exception_code_const()
    }

    fn xcause(&self) -> CSRRepr {
        let interrupt_bit = 1 << (CSRRepr::BITS - 1);
        interrupt_bit | self.exception_code()
    }

    fn trap_handler_address(&self, xtvec_val: CSRRepr) -> Address {
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

        xtvec_base + handler_offset
    }

    fn kind() -> TrapKind {
        TrapKind::Interrupt
    }
}
