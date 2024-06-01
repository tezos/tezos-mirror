// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{EcallOutcome, ExecutionEnvironment, ExecutionEnvironmentState};
use crate::{
    machine_state::{
        bus::{main_memory::MainMemoryLayout, Addressable},
        registers::{a0, a1, a2, a6, a7},
        MachineState,
    },
    parser::instruction::Instr,
    state_backend::{AllocatedOf, EnumCell, EnumCellLayout, Manager},
    traps::EnvironException,
};
use std::{
    cmp,
    io::{stdout, Write},
};
use tezos_smart_rollup_constants::riscv::{
    SBI_CONSOLE_PUTCHAR, SBI_FIRMWARE_TEZOS, SBI_SHUTDOWN, SBI_TEZOS_BLAKE2B_HASH256,
    SBI_TEZOS_ED25519_SIGN, SBI_TEZOS_ED25519_VERIFY, SBI_TEZOS_INBOX_NEXT, SBI_TEZOS_META_ADDRESS,
    SBI_TEZOS_META_ORIGINATION_LEVEL,
};

/// PVM status
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum PvmStatus {
    Evaluating,
    WaitingForInput,
}

impl Default for PvmStatus {
    fn default() -> Self {
        Self::Evaluating
    }
}

impl TryFrom<u8> for PvmStatus {
    type Error = u8;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        const EVALUATING: u8 = PvmStatus::Evaluating as u8;
        const WAITING_FOR_INPUT: u8 = PvmStatus::WaitingForInput as u8;

        match value {
            EVALUATING => Ok(Self::Evaluating),
            WAITING_FOR_INPUT => Ok(Self::WaitingForInput),
            _ => Err(value),
        }
    }
}

impl From<PvmStatus> for u8 {
    fn from(value: PvmStatus) -> Self {
        value as u8
    }
}

/// PVM configuration
pub struct PvmSbiConfig<'a> {
    pub putchar_hook: Box<dyn FnMut(u8) + 'a>,
}

impl<'a> PvmSbiConfig<'a> {
    /// Create a new configuration.
    pub fn new<F: FnMut(u8) + 'a>(putchar: F) -> Self {
        Self {
            putchar_hook: Box::new(putchar),
        }
    }
}

/// The default PVM configuration prints all debug information from the kernel
/// to the standard output.
impl<'a> Default for PvmSbiConfig<'a> {
    fn default() -> Self {
        fn putchar(char: u8) {
            stdout().lock().write_all(&[char]).unwrap();
        }
        Self::new(putchar)
    }
}

/// PVM execution environment
pub enum PvmSbi {}

impl ExecutionEnvironment for PvmSbi {
    type Layout = EnumCellLayout<u8>;

    type State<M: Manager> = PvmSbiState<M>;

    type Config<'a> = PvmSbiConfig<'a>;
}

/// PVM execution environment state
pub struct PvmSbiState<M: Manager> {
    status: EnumCell<PvmStatus, u8, M>,
}

impl<M: Manager> PvmSbiState<M> {
    /// Get the current PVM status.
    pub fn status(&self) -> PvmStatus {
        self.status.read_default()
    }

    /// Respond to a request for input with no input. Returns `false` in case the
    /// machine wasn't expecting any input, otherwise returns `true`.
    pub fn provide_no_input<ML: MainMemoryLayout>(
        &mut self,
        machine: &mut MachineState<ML, M>,
    ) -> bool {
        // This method should only do something when we're waiting for input.
        match self.status() {
            PvmStatus::WaitingForInput => {}
            _ => return false,
        }

        // We're evaluating again after this.
        self.status.write(PvmStatus::Evaluating);

        // Zeros in all these registers is equivalent to 'None'.
        machine.hart.xregisters.write(a0, 0);
        machine.hart.xregisters.write(a1, 0);
        machine.hart.xregisters.write(a2, 0);

        true
    }

    /// Provide input information to the machine. Returns `false` in case the
    /// machine wasn't expecting any input, otherwise returns `true`.
    pub fn provide_input<ML: MainMemoryLayout>(
        &mut self,
        machine: &mut MachineState<ML, M>,
        level: u64,
        counter: u64,
        payload: &[u8],
    ) -> bool {
        // This method should only do something when we're waiting for input.
        match self.status() {
            PvmStatus::WaitingForInput => {}
            _ => return false,
        }

        // We're evaluating again after this.
        self.status.write(PvmStatus::Evaluating);

        // These arguments should have been set by the previous SBI call.
        let arg_buffer_addr = machine.hart.xregisters.read(a0);
        let arg_buffer_size = machine.hart.xregisters.read(a1);

        // The argument address is a virtual address. We need to translate it to
        // a physical address.
        let phys_dest_addr =
            match machine.translate(arg_buffer_addr, crate::machine_state::AccessType::Store) {
                Ok(phys_addr) => phys_addr,
                Err(_exc) => {
                    // We back out on failure.
                    machine.hart.xregisters.write(a0, 0);
                    machine.hart.xregisters.write(a1, 0);
                    machine.hart.xregisters.write(a2, 0);
                    return true;
                }
            };

        // The SBI caller expects the payload to be returned at [phys_dest_addr]
        // with at maximum [max_buffer_size] bytes written.
        let max_buffer_size = cmp::min(arg_buffer_size as usize, payload.len());
        let write_res = machine
            .bus
            .write_all(phys_dest_addr, &payload[..max_buffer_size]);

        if write_res.is_err() {
            // We back out on failure.
            machine.hart.xregisters.write(a0, 0);
            machine.hart.xregisters.write(a1, 0);
            machine.hart.xregisters.write(a2, 0);
        } else {
            // Write meta information as return data.
            machine.hart.xregisters.write(a0, level);
            machine.hart.xregisters.write(a1, counter);
            machine.hart.xregisters.write(a2, max_buffer_size as u64);
        }

        true
    }

    /// Handle a [SBI_TEZOS_INBOX_NEXT] call.
    fn handle_tezos_inbox_next(&mut self) -> EcallOutcome {
        // This method only makes sense when evaluating.
        match self.status() {
            PvmStatus::Evaluating => {}
            status => {
                return EcallOutcome::Fatal {
                    message: format!(
                        "Called SBI_TEZOS_INBOX_NEXT while in non-Evaluating status {status:?}"
                    ),
                }
            }
        }

        // Prepare the EE state for an input tick.
        self.status.write(PvmStatus::WaitingForInput);

        EcallOutcome::Handled {
            // We can't evaluate after this. The next step is an input step.
            continue_eval: false,
        }
    }

    /// Handle a [SBI_SHUTDOWN] call.
    fn handle_shutdown(&self) -> EcallOutcome {
        // Shutting down in the PVM does nothing at the moment.
        EcallOutcome::Handled {
            continue_eval: true,
        }
    }

    /// Handle a [SBI_CONSOLE_PUTCHAR] call.
    fn handle_console_putchar<ML: MainMemoryLayout>(
        &self,
        machine: &mut MachineState<ML, M>,
        config: &mut PvmSbiConfig,
    ) -> EcallOutcome {
        let char = machine.hart.xregisters.read(a0) as u8;
        (config.putchar_hook)(char);

        // This call always succeeds.
        machine.hart.xregisters.write(a0, 0);

        EcallOutcome::Handled {
            continue_eval: true,
        }
    }
}

/// TODO: Implement a PVM execution environment.
impl<M> ExecutionEnvironmentState<M> for PvmSbiState<M>
where
    M: Manager,
{
    type ExecutionEnvironment = PvmSbi;

    fn bind(space: AllocatedOf<<PvmSbi as ExecutionEnvironment>::Layout, M>) -> Self {
        Self {
            status: EnumCell::bind(space),
        }
    }

    fn reset(&mut self) {
        self.status.reset();
    }

    fn handle_call<ML: MainMemoryLayout>(
        &mut self,
        machine: &mut MachineState<ML, M>,
        config: &mut PvmSbiConfig,
        env_exception: EnvironException,
    ) -> EcallOutcome {
        if let EnvironException::EnvCallFromMMode = env_exception {
            return EcallOutcome::Fatal {
                message: "ECALLs from M-mode are not supported".to_owned(),
            };
        }

        // No matter the outcome, we need to bump the
        // program counter because ECALL's don't update it
        // to the following instructions.
        let pc = machine.hart.pc.read() + Instr::Ecall.width();
        machine.hart.pc.write(pc);

        // SBI extension is contained in a7.
        let sbi_extension = machine.hart.xregisters.read(a7);

        match sbi_extension {
            SBI_CONSOLE_PUTCHAR => self.handle_console_putchar(machine, config),
            SBI_SHUTDOWN => self.handle_shutdown(),
            SBI_FIRMWARE_TEZOS => {
                let sbi_function = machine.hart.xregisters.read(a6);

                match sbi_function {
                    SBI_TEZOS_INBOX_NEXT => self.handle_tezos_inbox_next(),
                    SBI_TEZOS_META_ORIGINATION_LEVEL => todo!(),
                    SBI_TEZOS_META_ADDRESS => todo!(),
                    SBI_TEZOS_ED25519_SIGN => todo!(),
                    SBI_TEZOS_ED25519_VERIFY => todo!(),
                    SBI_TEZOS_BLAKE2B_HASH256 => todo!(),

                    // Unimplemented
                    _ => EcallOutcome::Fatal {
                        message: format!("Unsupported Tezos SBI extension function {sbi_function}"),
                    },
                }
            }
            // Unimplemented
            _ => EcallOutcome::Fatal {
                message: format!("Unsupported SBI extension {sbi_extension}"),
            },
        }
    }
}
