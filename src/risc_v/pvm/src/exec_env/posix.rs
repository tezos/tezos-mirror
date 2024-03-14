// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{EcallOutcome, ExecutionEnvironment, ExecutionEnvironmentState};
use risc_v_interpreter::{
    machine_state::{
        bus::main_memory::MainMemoryLayout,
        registers::{a0, a7},
        MachineState,
    },
    state_backend::{AllocatedOf, Atom, Cell, Manager},
};

/// Posix execution environment
pub enum Posix {}

impl ExecutionEnvironment for Posix {
    type Layout = (Atom<u8>, Atom<u64>);

    type State<M: Manager> = PosixState<M>;
}

/// Posix execution environment state
pub struct PosixState<M: Manager> {
    exited: Cell<u8, M>,
    code: Cell<u64, M>,
}

impl<M: Manager> PosixState<M> {
    /// If an exit has been requested, return the exit code.
    #[allow(dead_code)]
    pub fn exit_code(&self) -> Option<u64> {
        self.exited().then(|| self.code.read())
    }

    /// Has an exit been requested?
    pub fn exited(&self) -> bool {
        self.exited.read() > 0
    }
}

impl<M: Manager> ExecutionEnvironmentState<M> for PosixState<M> {
    type ExecutionEnvironment = Posix;

    fn bind(space: AllocatedOf<<Posix as ExecutionEnvironment>::Layout, M>) -> Self {
        Self {
            exited: space.0,
            code: space.1,
        }
    }

    fn reset(&mut self) {
        self.exited.write(0);
        self.code.write(0);
    }

    fn handle_call<ML: MainMemoryLayout>(
        &mut self,
        machine: &mut MachineState<ML, M>,
    ) -> EcallOutcome {
        if self.exited() {
            // Can't exit twice
            return EcallOutcome::Fatal;
        }

        let syscall_number = machine.hart.xregisters.read(a7);
        match syscall_number {
            // Exit
            93 => {
                let exited = self.exited.read();
                self.exited.write(exited.saturating_add(1));

                let code = machine.hart.xregisters.read(a0);
                self.code.write(code);

                EcallOutcome::Handled {
                    continue_eval: false,
                }
            }

            // Unimplemented
            _ => EcallOutcome::Fatal,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::state::{Pvm, PvmLayout};
    use risc_v_interpreter::{
        backend_test,
        machine_state::{
            bus::main_memory::M1K,
            mode::Mode,
            registers::{a0, a7, zero},
        },
        parser::{
            instruction::{ITypeArgs, Instr},
            parse_block,
        },
        program::Program,
        state_backend::{Backend, Layout},
    };

    backend_test!(test_exit, F, {
        type L = PvmLayout<Posix, M1K>;

        let mut backend = F::new::<L>();

        let placed = <L as Layout>::placed().into_location();
        let space = backend.allocate(placed);

        let mut state: Pvm<Posix, M1K, _> = Pvm::bind(space);
        state.reset();

        // The following program invokes the POSIX syscall 93 (exit) with 1337
        // as the first parameter.
        let code = [
            // 53900513 => li a0, 1337
            u32::from_le(0x53900513),
            // 05d00893 => li a7, 93
            u32::from_le(0x05d00893),
            // 00000073 => ecall
            u32::from_le(0x00000073),
        ];
        let code: Vec<u8> = code.iter().flat_map(|instr| instr.to_le_bytes()).collect();

        // Verify that the instructions we crafted are what we expect
        assert_eq!(
            parse_block(code.as_slice()),
            [
                Instr::Addi(ITypeArgs {
                    rd: a0,
                    rs1: zero,
                    imm: 1337
                }),
                Instr::Addi(ITypeArgs {
                    rd: a7,
                    rs1: zero,
                    imm: 93
                }),
                Instr::Ecall
            ]
        );

        // Load the program into the PVM
        let program = Program::from_raw(code.as_slice());
        state
            .machine_state
            .setup_boot(&program, None, Mode::Machine)
            .unwrap();

        // 3 instructions is enough, but we want to uncover other types of
        // failures so we set the maximum a little higher
        let steps = state.step_many(1000);
        assert_eq!(steps, 3);

        // After calling exit, the EE should indicate the right exit code
        assert_eq!(state.syscall_state.exit_code(), Some(1337));
    });
}
