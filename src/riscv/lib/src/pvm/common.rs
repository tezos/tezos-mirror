// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::{
    machine_state::{self, bus::main_memory},
    pvm::sbi,
    state_backend::{self, EnumCell, EnumCellLayout},
    traps::EnvironException,
};
use std::{
    convert::Infallible,
    fmt,
    io::{stdout, Write},
    ops::RangeBounds,
};

/// PVM configuration
pub struct PvmHooks<'a> {
    pub putchar_hook: Box<dyn FnMut(u8) + 'a>,
}

impl<'a> PvmHooks<'a> {
    /// Create a new configuration.
    pub fn new<F: FnMut(u8) + 'a>(putchar: F) -> Self {
        Self {
            putchar_hook: Box::new(putchar),
        }
    }
}

/// The default PVM configuration prints all debug information from the kernel
/// to the standard output.
impl<'a> Default for PvmHooks<'a> {
    fn default() -> Self {
        fn putchar(char: u8) {
            stdout().lock().write_all(&[char]).unwrap();
        }

        Self::new(putchar)
    }
}

/// PVM state layout
pub type PvmLayout<ML> = (
    state_backend::Atom<u64>,
    machine_state::MachineStateLayout<ML>,
    EnumCellLayout<u8>,
);

/// PVM status
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum PvmStatus {
    Evaluating,
    WaitingForInput,
    WaitingForMetadata,
}

impl Default for PvmStatus {
    #[inline(always)]
    fn default() -> Self {
        Self::Evaluating
    }
}

impl fmt::Display for PvmStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let status = match self {
            PvmStatus::Evaluating => "Evaluating",
            PvmStatus::WaitingForInput => "Waiting for input message",
            PvmStatus::WaitingForMetadata => "Waiting for metadata",
        };
        f.write_str(status)
    }
}

impl From<u8> for PvmStatus {
    #[inline(always)]
    fn from(value: u8) -> Self {
        const EVALUATING: u8 = PvmStatus::Evaluating as u8;
        const WAITING_FOR_INPUT: u8 = PvmStatus::WaitingForInput as u8;
        const WAITING_FOR_METADATA: u8 = PvmStatus::WaitingForMetadata as u8;

        match value {
            EVALUATING => Self::Evaluating,
            WAITING_FOR_INPUT => Self::WaitingForInput,
            WAITING_FOR_METADATA => Self::WaitingForMetadata,
            _ => Self::default(),
        }
    }
}

impl From<PvmStatus> for u8 {
    #[inline(always)]
    fn from(value: PvmStatus) -> Self {
        value as u8
    }
}

/// Value for the initial version
const INITIAL_VERSION: u64 = 0;

/// Proof-generating virtual machine
pub struct Pvm<ML: main_memory::MainMemoryLayout, M: state_backend::ManagerBase> {
    version: state_backend::Cell<u64, M>,
    pub(crate) machine_state: machine_state::MachineState<ML, M>,
    status: EnumCell<PvmStatus, u8, M>,
}

impl<ML: main_memory::MainMemoryLayout, M: state_backend::Manager> Pvm<ML, M> {
    /// Bind the PVM to the given allocated region.
    pub fn bind(space: state_backend::AllocatedOf<PvmLayout<ML>, M>) -> Self {
        // Ensure we're binding a version we can deal with
        assert_eq!(space.0.read(), INITIAL_VERSION);

        Self {
            version: space.0,
            machine_state: machine_state::MachineState::bind(space.1),
            status: EnumCell::bind(space.2),
        }
    }

    /// Reset the PVM state.
    pub fn reset(&mut self) {
        self.version.write(INITIAL_VERSION);
        self.machine_state.reset();
        self.status.reset();
    }

    /// Handle an exception using the defined Execution Environment.
    pub fn handle_exception(
        &mut self,
        hooks: &mut PvmHooks<'_>,
        exception: EnvironException,
    ) -> bool {
        sbi::handle_call(&mut self.status, &mut self.machine_state, hooks, exception)
    }

    /// Perform one evaluation step.
    pub fn eval_one(&mut self, hooks: &mut PvmHooks<'_>) {
        if let Err(exc) = self.machine_state.step() {
            self.handle_exception(hooks, exc);
        }
    }

    /// Perform a range of evaluation steps. Returns the actual number of steps
    /// performed.
    ///
    /// If an environment trap is raised, handle it and
    /// return the number of retired instructions until the raised trap
    ///
    /// NOTE: instructions which raise exceptions / are interrupted are NOT retired
    ///       See section 3.3.1 for context on retired instructions.
    /// e.g: a load instruction raises an exception but the first instruction
    /// of the trap handler will be executed and retired,
    /// so in the end the load instruction which does not bubble it's exception up to
    /// the execution environment will still retire an instruction, just not itself.
    /// (a possible case: the privilege mode access violation is treated in EE,
    /// but a page fault is not)

    // Trampoline style function for [eval_range]
    pub fn eval_range_while<F>(
        &mut self,
        hooks: &mut PvmHooks<'_>,
        step_bounds: &impl RangeBounds<usize>,
        should_continue: F,
    ) -> usize
    where
        F: FnMut(&machine_state::MachineState<ML, M>) -> bool,
    {
        self.machine_state
            .step_range_handle::<Infallible>(step_bounds, should_continue, |machine_state, exc| {
                Ok(sbi::handle_call(
                    &mut self.status,
                    machine_state,
                    hooks,
                    exc,
                ))
            })
            .steps
    }

    /// Respond to a request for input with no input. Returns `false` in case the
    /// machine wasn't expecting any input, otherwise returns `true`.
    pub fn provide_no_input(&mut self) -> bool {
        sbi::provide_no_input(&mut self.status, &mut self.machine_state)
    }

    /// Provide input. Returns `false` if the machine state is not expecting
    /// input.
    pub fn provide_input(&mut self, level: u32, counter: u32, payload: &[u8]) -> bool {
        sbi::provide_input(
            &mut self.status,
            &mut self.machine_state,
            level,
            counter,
            payload,
        )
    }

    /// Provide metadata in response to a metadata request. Returns `false`
    /// if the machine is not expecting metadata.
    pub fn provide_metadata(&mut self, rollup_address: &[u8; 20], origination_level: u32) -> bool {
        sbi::provide_metadata(
            &mut self.status,
            &mut self.machine_state,
            rollup_address,
            origination_level,
        )
    }

    /// Get the current machine status.
    pub fn status(&self) -> PvmStatus {
        self.status.read()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        backend_test,
        machine_state::{
            bus::{
                main_memory::{M1K, M1M},
                start_of_main_memory, Addressable,
            },
            registers::{a0, a1, a2, a3, a6, a7},
        },
        state_backend::{
            memory_backend::InMemoryBackend,
            tests::{test_determinism, ManagerFor},
            Backend,
        },
    };
    use rand::{thread_rng, Fill};
    use std::mem;
    use tezos_smart_rollup_constants::riscv::{
        SBI_CONSOLE_PUTCHAR, SBI_FIRMWARE_TEZOS, SBI_TEZOS_INBOX_NEXT,
    };

    #[test]
    fn test_read_input() {
        type ML = M1M;
        type L = PvmLayout<ML>;

        // Setup PVM
        let (mut backend, placed) = InMemoryBackend::<L>::new();
        let space = backend.allocate(placed);
        let mut pvm = Pvm::<ML, _>::bind(space);
        pvm.reset();

        let level_addr = start_of_main_memory::<ML>();
        let counter_addr = level_addr + 4;
        let buffer_addr = counter_addr + 4;

        const BUFFER_LEN: usize = 1024;

        // Configure machine for 'sbi_tezos_inbox_next'
        pvm.machine_state.hart.xregisters.write(a0, buffer_addr);
        pvm.machine_state
            .hart
            .xregisters
            .write(a1, BUFFER_LEN as u64);
        pvm.machine_state.hart.xregisters.write(a2, level_addr);
        pvm.machine_state.hart.xregisters.write(a3, counter_addr);
        pvm.machine_state
            .hart
            .xregisters
            .write(a7, SBI_FIRMWARE_TEZOS);
        pvm.machine_state
            .hart
            .xregisters
            .write(a6, SBI_TEZOS_INBOX_NEXT);

        // Should be in evaluating mode
        assert_eq!(pvm.status(), PvmStatus::Evaluating);

        // Handle the ECALL successfully
        let outcome =
            pvm.handle_exception(&mut Default::default(), EnvironException::EnvCallFromUMode);
        assert!(matches!(outcome, false));

        // After the ECALL we should be waiting for input
        assert_eq!(pvm.status(), PvmStatus::WaitingForInput);

        // Respond to the request for input
        let level = rand::random();
        let counter = rand::random();
        let mut payload = [0u8; BUFFER_LEN + 10];
        payload.try_fill(&mut thread_rng()).unwrap();
        assert!(pvm.provide_input(level, counter, &payload));

        // The status should switch from WaitingForInput to Evaluating
        assert_eq!(pvm.status(), PvmStatus::Evaluating);

        // Returned data is as expected
        assert_eq!(
            pvm.machine_state.hart.xregisters.read(a0) as usize,
            BUFFER_LEN
        );
        assert_eq!(pvm.machine_state.bus.read(level_addr), Ok(level));
        assert_eq!(pvm.machine_state.bus.read(counter_addr), Ok(counter));

        // Payload in memory should be as expected
        for (offset, &byte) in payload[..BUFFER_LEN].iter().enumerate() {
            let addr = buffer_addr + offset as u64;
            let byte_written: u8 = pvm.machine_state.bus.read(addr).unwrap();
            assert_eq!(
                byte, byte_written,
                "Byte at {addr:x} (offset {offset}) is not the same"
            );
        }

        // Data after the buffer should be untouched
        assert!((BUFFER_LEN..4096)
            .map(|offset| {
                let addr = buffer_addr + offset as u64;
                pvm.machine_state.bus.read(addr).unwrap()
            })
            .all(|b: u8| b == 0));
    }

    #[test]
    fn test_write_debug() {
        type ML = M1M;
        type L = PvmLayout<ML>;

        let mut buffer = Vec::new();
        let mut hooks = PvmHooks::new(|c| buffer.push(c));

        // Setup PVM
        let (mut backend, placed) = InMemoryBackend::<L>::new();
        let space = backend.allocate(placed);
        let mut pvm = Pvm::<ML, _>::bind(space);
        pvm.reset();

        // Prepare subsequent ECALLs to use the SBI_CONSOLE_PUTCHAR extension
        pvm.machine_state
            .hart
            .xregisters
            .write(a7, SBI_CONSOLE_PUTCHAR);

        // Write characters
        let mut written = Vec::new();
        for _ in 0..10 {
            let char: u8 = rand::random();
            pvm.machine_state.hart.xregisters.write(a0, char as u64);
            written.push(char);

            let outcome = pvm.handle_exception(&mut hooks, EnvironException::EnvCallFromUMode);
            assert!(outcome, "Unexpected outcome");
        }

        // Drop `hooks` to regain access to the mutable references it kept
        mem::drop(hooks);

        // Compare what characters have been passed to the hook verrsus what we
        // intended to write
        assert_eq!(written, buffer);
    }

    backend_test!(test_reset, F, {
        test_determinism::<F, PvmLayout<M1K>, _>(|mut space| {
            // The [Pvm] type won't bind unless the version cell is set to its initial value.
            // TODO: RV-46 might change this constraint in the future.
            space.0.write(INITIAL_VERSION);

            let mut machine_state: Pvm<M1K, ManagerFor<'_, F, PvmLayout<M1K>>> = Pvm::bind(space);
            machine_state.reset();
        });
    });
}
