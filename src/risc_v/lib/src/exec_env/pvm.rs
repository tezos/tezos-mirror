// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{EcallOutcome, ExecutionEnvironment, ExecutionEnvironmentState};
use crate::{
    machine_state::{
        bus::main_memory::MainMemoryLayout,
        registers::{a6, a7},
        MachineState,
    },
    state_backend::{AllocatedOf, Atom, Cell, Manager},
    traps::EnvironException,
};
use std::marker::PhantomData;
use tezos_smart_rollup_constants::riscv::{
    SBI_CONSOLE_PUTCHAR, SBI_FIRMWARE_TEZOS, SBI_SHUTDOWN, SBI_TEZOS_BLAKE2B_HASH256,
    SBI_TEZOS_ED25519_SIGN, SBI_TEZOS_ED25519_VERIFY, SBI_TEZOS_INBOX_NEXT, SBI_TEZOS_META_ADDRESS,
    SBI_TEZOS_META_ORIGINATION_LEVEL,
};

/// PVM execution environment
pub enum PvmSbi {}

impl ExecutionEnvironment for PvmSbi {
    type Layout = Atom<u8>; // Placeholder
    type State<M: Manager> = PvmSbiState<M>;
}

/// PVM execution environment state
pub struct PvmSbiState<M: Manager> {
    code: PhantomData<Cell<u8, M>>, //Placeholder
}

/// TODO: Implement a PVM execution environment for the RISC-V test suite.
impl<M> ExecutionEnvironmentState<M> for PvmSbiState<M>
where
    M: Manager,
{
    type ExecutionEnvironment = PvmSbi;

    fn bind(_space: AllocatedOf<<PvmSbi as ExecutionEnvironment>::Layout, M>) -> Self {
        todo!()
    }

    fn reset(&mut self) {}

    fn handle_call<ML: MainMemoryLayout>(
        &mut self,
        machine: &mut MachineState<ML, M>,
        env_exception: EnvironException,
    ) -> EcallOutcome {
        // All calls from machine mode are fatal (according to rvemu sbi)
        if let EnvironException::EnvCallFromMMode = env_exception {
            return EcallOutcome::Handled {
                continue_eval: false,
            };
        }

        // SBI extension is contained in a7.
        let sbi_extension = machine.hart.xregisters.read(a7);
        match sbi_extension {
            SBI_CONSOLE_PUTCHAR => todo!(),
            SBI_SHUTDOWN => todo!(),
            SBI_FIRMWARE_TEZOS => {
                let sbi_function = machine.hart.xregisters.read(a6);

                match sbi_function {
                    SBI_TEZOS_INBOX_NEXT => todo!(),
                    SBI_TEZOS_META_ORIGINATION_LEVEL => todo!(),
                    SBI_TEZOS_META_ADDRESS => todo!(),
                    SBI_TEZOS_ED25519_SIGN => todo!(),
                    SBI_TEZOS_ED25519_VERIFY => todo!(),
                    SBI_TEZOS_BLAKE2B_HASH256 => todo!(),

                    // Unimplemented
                    _ => EcallOutcome::Fatal,
                }
            }
            // Unimplemented
            _ => EcallOutcome::Fatal,
        }
    }
}
