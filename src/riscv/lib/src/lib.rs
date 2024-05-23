// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

pub mod bits;
pub mod devicetree;
pub mod exec_env;
mod interpreter;
pub mod kernel_loader;
pub mod machine_state;
pub mod parser;
pub mod program;
pub mod pvm;
pub mod range_utils;
pub mod state_backend;
pub mod storage;
pub mod traps;

#[cfg(feature = "ocaml-api")]
pub mod ocaml_api;

use crate::{
    machine_state::{
        bus::main_memory::M1G, mode, registers::XRegister, MachineError, MachineState,
    },
    program::Program,
    state_backend::{
        memory_backend::{InMemoryBackend, SliceManager},
        Backend, Layout,
    },
    traps::EnvironException,
};
use bits::Bits64;
use derive_more::{Error, From};
use exec_env::posix::Posix;
use machine_state::{
    bus::{Address, Addressable, OutOfBounds},
    csregisters::{satp::TranslationAlgorithm, CSRegister},
    registers::{FRegister, FValue},
    AccessType,
};
use pvm::{EvalError, EvalManyResult, Pvm, PvmLayout};
use state_backend::Elem;
use std::{collections::BTreeMap, ops::RangeBounds};
use traps::Exception;
use InterpreterResult::*;

type PvmStateLayout = PvmLayout<Posix, M1G>;

pub struct Interpreter<'a> {
    pvm: Pvm<Posix, M1G, SliceManager<'a>>,
}

#[derive(Debug)]
pub enum InterpreterResult {
    /// Execution has not finished. Returns the number of steps executed.
    Running(usize),
    /// Program exited. Returns exit code and number of steps executed.
    Exit { code: usize, steps: usize },
    /// Execution finished because an unhandled environment exception has been thrown.
    /// Returns exception and number of steps executed.
    Exception {
        cause: EnvironException,
        steps: usize,
        message: Option<String>,
    },
}

#[derive(Debug, From, Error, derive_more::Display)]
pub enum InterpreterError {
    KernelLoadingError(kernel_loader::Error),
    MachineError(MachineError),
}

impl<'a> Interpreter<'a> {
    /// In order to create an [Interpreter], a memory backend must first be generated.
    /// Currently, the size of the main memory to be allocated is fixed at 1GB.
    pub fn create_backend() -> InMemoryBackend<PvmStateLayout> {
        InMemoryBackend::<PvmStateLayout>::new().0
    }

    fn bind_states(
        backend: &'a mut InMemoryBackend<PvmStateLayout>,
    ) -> Pvm<Posix, M1G, SliceManager<'a>> {
        Pvm::bind(backend.allocate(PvmStateLayout::placed().into_location()))
    }

    /// Initialise an interpreter with a given [program], starting execution in [mode].
    /// An initial ramdisk can also optionally be passed.
    pub fn new(
        backend: &'a mut InMemoryBackend<PvmStateLayout>,
        program: &[u8],
        initrd: Option<&[u8]>,
        mode: mode::Mode,
    ) -> Result<Self, InterpreterError> {
        let mut pvm = Self::bind_states(backend);
        pvm.exec_env_state.set_exit_mode(mode);
        let elf_program = Program::<M1G>::from_elf(program)?;
        pvm.machine_state
            .setup_boot(&elf_program, initrd, mode::Mode::Machine)?;
        Ok(Self { pvm })
    }

    fn handle_step_result(&mut self, result: EvalManyResult) -> InterpreterResult {
        match result.error {
            // An error was encountered in the evaluation function.
            Some(EvalError { cause, message }) => InterpreterResult::Exception {
                cause,
                steps: result.steps,
                message: Some(message),
            },

            // Evaluation function returned without error.
            None => {
                // Check if the machine has exited.
                if let Some(code) = self.pvm.exec_env_state.exit_code() {
                    Exit {
                        code: code as usize,
                        steps: result.steps,
                    }
                } else {
                    Running(result.steps)
                }
            }
        }
    }

    pub fn run(&mut self, max: usize) -> InterpreterResult {
        self.run_accum(0, &..=max, |_| true)
    }

    /// This function only exists to make the funneling of [steps_done]
    /// tail-recursive.
    fn run_accum<F>(
        &mut self,
        steps_done: usize,
        step_bounds: &impl RangeBounds<usize>,
        mut should_continue: F,
    ) -> InterpreterResult
    where
        F: FnMut(&MachineState<M1G, SliceManager<'a>>) -> bool,
    {
        let mut result =
            self.pvm
                .eval_range(&mut Default::default(), step_bounds, &mut should_continue);
        result.steps = result.steps.saturating_add(steps_done);
        self.handle_step_result(result)
    }

    pub fn step_range<F>(
        &mut self,
        steps: impl RangeBounds<usize>,
        should_continue: F,
    ) -> InterpreterResult
    where
        F: FnMut(&MachineState<M1G, SliceManager<'a>>) -> bool,
    {
        self.run_accum(0, &steps, should_continue)
    }

    pub fn effective_translation_alg(
        &self,
        access_type: &AccessType,
    ) -> Option<TranslationAlgorithm> {
        self.pvm
            .machine_state
            .effective_translation_alg(access_type)
    }

    pub fn read_bus<E: Elem>(&self, address: Address) -> Result<E, OutOfBounds> {
        self.pvm.machine_state.bus.read(address)
    }

    pub fn read_xregister(&self, reg: XRegister) -> u64 {
        self.pvm.machine_state.hart.xregisters.read(reg)
    }

    pub fn read_fregister(&self, reg: FRegister) -> FValue {
        self.pvm.machine_state.hart.fregisters.read(reg)
    }

    pub fn read_csregister<V: Bits64>(&self, reg: CSRegister) -> V {
        self.pvm.machine_state.hart.csregisters.read::<V>(reg)
    }

    pub fn read_pc(&self) -> u64 {
        self.pvm.machine_state.hart.pc.read()
    }

    pub fn read_mode(&self) -> mode::Mode {
        self.pvm.machine_state.hart.mode.read_default()
    }
}

/// Debugger-specific functions
impl<'a> Interpreter<'a> {
    /// Initialise an interpreter with a given [program], starting execution in [mode].
    /// An initial ramdisk can also optionally be passed. Returns both the interpreter
    /// and the fully parsed program.
    pub fn new_with_parsed_program(
        backend: &'a mut InMemoryBackend<PvmStateLayout>,
        program: &[u8],
        initrd: Option<&[u8]>,
        mode: mode::Mode,
    ) -> Result<(Self, BTreeMap<u64, String>), InterpreterError> {
        let mut pvm = Self::bind_states(backend);
        pvm.exec_env_state.set_exit_mode(mode);
        let elf_program = Program::<M1G>::from_elf(program)?;
        pvm.machine_state
            .setup_boot(&elf_program, initrd, mode::Mode::Machine)?;
        Ok((Self { pvm }, elf_program.parsed()))
    }

    /// Obtain the translated address of pc.
    pub fn translate_instruction_address(&self, pc: Address) -> Result<Address, Exception> {
        self.pvm
            .machine_state
            .translate(pc, AccessType::Instruction)
    }
}
