// SPDX-FileCopyrightText: 2023-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::reveals::{RevealRequest, RevealRequestLayout};
use crate::{
    default::ConstDefault,
    machine_state::{
        self,
        block_cache::bcall,
        main_memory::{self},
    },
    pvm::sbi,
    state_backend::{
        self, Atom, Cell,
        proof_backend::{ProofDynRegion, ProofEnrichedCell, ProofGen, ProofRegion},
    },
    traps::EnvironException,
};
use std::{
    convert::Infallible,
    fmt,
    io::{Write, stdout},
    ops::Bound,
};

#[cfg(feature = "supervisor")]
use super::linux;

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

impl PvmHooks<'static> {
    /// Hook that does nothing.
    pub fn none() -> Self {
        Self {
            putchar_hook: Box::new(|_| {}),
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
#[cfg(feature = "supervisor")]
pub type PvmLayout<ML, CL> = (
    state_backend::Atom<u64>,
    machine_state::MachineStateLayout<ML, CL>,
    Atom<PvmStatus>,
    RevealRequestLayout,
    linux::SupervisorStateLayout,
);

/// PVM state layout
#[cfg(not(feature = "supervisor"))]
pub type PvmLayout<ML, CL> = (
    state_backend::Atom<u64>,
    machine_state::MachineStateLayout<ML, CL>,
    Atom<PvmStatus>,
    RevealRequestLayout,
);

/// PVM status
#[derive(
    Clone,
    Copy,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    serde::Serialize,
    serde::Deserialize,
    strum::EnumCount,
)]
#[repr(u8)]
pub enum PvmStatus {
    Evaluating,
    WaitingForInput,
    WaitingForMetadata,
    WaitingForReveal,
}

impl ConstDefault for PvmStatus {
    const DEFAULT: Self = Self::Evaluating;
}

impl fmt::Display for PvmStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let status = match self {
            PvmStatus::Evaluating => "Evaluating",
            PvmStatus::WaitingForInput => "Waiting for input message",
            PvmStatus::WaitingForMetadata => "Waiting for metadata",
            PvmStatus::WaitingForReveal => "Waiting for reveal",
        };
        f.write_str(status)
    }
}

/// Value for the initial version
const INITIAL_VERSION: u64 = 0;

/// Proof generator for the PVM.
///
/// Uses the interpreted block backend by default.
type PvmProofGen<'a, ML, CL, M> = Pvm<
    ML,
    CL,
    bcall::Interpreted<ML, ProofGen<state_backend::Ref<'a, M>>>,
    ProofGen<state_backend::Ref<'a, M>>,
>;

/// Proof-generating virtual machine
pub struct Pvm<
    ML: main_memory::MainMemoryLayout,
    CL: machine_state::CacheLayouts,
    B: bcall::Block<ML, M>,
    M: state_backend::ManagerBase,
> {
    pub(crate) machine_state: machine_state::MachineState<ML, CL, B, M>,
    reveal_request: RevealRequest<M>,
    #[cfg(feature = "supervisor")]
    pub(super) system_state: linux::SupervisorState<M>,
    version: state_backend::Cell<u64, M>,
    status: Cell<PvmStatus, M>,
}

impl<
    ML: main_memory::MainMemoryLayout,
    CL: machine_state::CacheLayouts,
    B: bcall::Block<ML, M>,
    M: state_backend::ManagerBase,
> Pvm<ML, CL, B, M>
{
    /// Bind the block cache to the given allocated state and the given [block builder].
    ///
    /// [block builder]: bcall::Block::BlockBuilder
    pub fn bind(
        space: state_backend::AllocatedOf<PvmLayout<ML, CL>, M>,
        block_builder: B::BlockBuilder,
    ) -> Self {
        Self {
            version: space.0,
            machine_state: machine_state::MachineState::bind(space.1, block_builder),
            status: space.2,
            reveal_request: RevealRequest::bind(space.3),
            #[cfg(feature = "supervisor")]
            system_state: linux::SupervisorState::bind(space.4),
        }
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub fn struct_ref<'a, F: state_backend::FnManager<state_backend::Ref<'a, M>>>(
        &'a self,
    ) -> state_backend::AllocatedOf<PvmLayout<ML, CL>, F::Output> {
        (
            self.version.struct_ref::<F>(),
            self.machine_state.struct_ref::<F>(),
            self.status.struct_ref::<F>(),
            self.reveal_request.struct_ref::<F>(),
            #[cfg(feature = "supervisor")]
            self.system_state.struct_ref::<F>(),
        )
    }

    /// Generate a proof-generating version of this PVM.
    pub fn start_proof(&self) -> PvmProofGen<'_, ML, CL, M> {
        enum ProofWrapper {}

        impl<M: state_backend::ManagerBase> state_backend::FnManager<M> for ProofWrapper {
            type Output = ProofGen<M>;

            fn map_region<E: 'static, const LEN: usize>(
                input: <M as state_backend::ManagerBase>::Region<E, LEN>,
            ) -> <ProofGen<M> as state_backend::ManagerBase>::Region<E, LEN> {
                ProofRegion::bind(input)
            }

            fn map_dyn_region<const LEN: usize>(
                input: <M as state_backend::ManagerBase>::DynRegion<LEN>,
            ) -> <ProofGen<M> as state_backend::ManagerBase>::DynRegion<LEN> {
                ProofDynRegion::bind(input)
            }

            fn map_enriched_cell<V: state_backend::EnrichedValue>(
                input: <M as state_backend::ManagerBase>::EnrichedCell<V>,
            ) -> <ProofGen<M> as state_backend::ManagerBase>::EnrichedCell<V> {
                ProofEnrichedCell::bind(input)
            }
        }

        Pvm::bind(
            self.struct_ref::<ProofWrapper>(),
            bcall::InterpretedBlockBuilder,
        )
    }

    /// Reset the PVM state.
    pub fn reset(&mut self)
    where
        M: state_backend::ManagerReadWrite,
    {
        self.version.write(INITIAL_VERSION);
        self.machine_state.reset();
        self.status.write(PvmStatus::DEFAULT);
    }

    /// Handle an exception using the defined Execution Environment.
    // The conditional compilation below causes some warnings.
    #[cfg_attr(feature = "supervisor", allow(unused_variables, unreachable_code))]
    pub fn handle_exception(
        &mut self,
        hooks: &mut PvmHooks<'_>,
        exception: EnvironException,
    ) -> bool
    where
        M: state_backend::ManagerReadWrite,
    {
        #[cfg(feature = "supervisor")]
        return self
            .system_state
            .handle_system_call(&mut self.machine_state.core);

        sbi::handle_call(
            &mut self.status,
            &mut self.reveal_request,
            &mut self.machine_state,
            hooks,
            exception,
        )
    }

    /// Perform one evaluation step.
    pub fn eval_one(&mut self, hooks: &mut PvmHooks<'_>)
    where
        M: state_backend::ManagerReadWrite,
    {
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
    // The conditional compilation below causes some warnings.
    #[cfg_attr(feature = "supervisor", allow(unused_variables, unreachable_code))]
    pub fn eval_max(&mut self, hooks: &mut PvmHooks<'_>, step_bounds: Bound<usize>) -> usize
    where
        M: state_backend::ManagerReadWrite,
    {
        self.machine_state
            .step_max_handle::<Infallible>(step_bounds, |machine_state, exception| {
                #[cfg(feature = "supervisor")]
                return Ok(self
                    .system_state
                    .handle_system_call(&mut machine_state.core));

                Ok(sbi::handle_call(
                    &mut self.status,
                    &mut self.reveal_request,
                    machine_state,
                    hooks,
                    exception,
                ))
            })
            .steps
    }

    /// Respond to a request for input with no input. Returns `false` in case the
    /// machine wasn't expecting any input, otherwise returns `true`.
    pub fn provide_no_input(&mut self) -> bool
    where
        M: state_backend::ManagerReadWrite,
    {
        sbi::provide_no_input(&mut self.status, &mut self.machine_state)
    }

    /// Provide input. Returns `false` if the machine state is not expecting
    /// input.
    pub fn provide_input(&mut self, level: u32, counter: u32, payload: &[u8]) -> bool
    where
        M: state_backend::ManagerReadWrite,
    {
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
    pub fn provide_metadata(&mut self, rollup_address: &[u8; 20], origination_level: u32) -> bool
    where
        M: state_backend::ManagerReadWrite,
    {
        sbi::provide_metadata(
            &mut self.status,
            &mut self.machine_state,
            rollup_address,
            origination_level,
        )
    }

    /// Provide reveal data in response to a reveal request.
    /// Returns `false` if the machine is not expecting a reveal.
    pub fn provide_reveal_response(&mut self, reveal_data: &[u8]) -> bool
    where
        M: state_backend::ManagerReadWrite,
    {
        sbi::provide_reveal_response(&mut self.status, &mut self.machine_state, reveal_data)
    }

    /// Get the current machine status.
    pub fn status(&self) -> PvmStatus
    where
        M: state_backend::ManagerRead,
    {
        self.status.read()
    }

    /// Get the reveal request in the machine state
    pub fn reveal_request(&self) -> Vec<u8>
    where
        M: state_backend::ManagerRead,
    {
        self.reveal_request.to_vec()
    }
}

impl<
    ML: main_memory::MainMemoryLayout,
    CL: machine_state::CacheLayouts,
    B: bcall::Block<ML, M> + Clone,
    M: state_backend::ManagerClone,
> Clone for Pvm<ML, CL, B, M>
{
    fn clone(&self) -> Self {
        Self {
            version: self.version.clone(),
            machine_state: self.machine_state.clone(),
            status: self.status.clone(),
            reveal_request: self.reveal_request.clone(),
            #[cfg(feature = "supervisor")]
            system_state: self.system_state.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::state_backend::test_helpers::TestBackendFactory;
    use crate::{
        backend_test, create_state,
        machine_state::{
            TestCacheLayouts,
            block_cache::bcall::InterpretedBlockBuilder,
            main_memory::M1M,
            registers::{a0, a1, a2, a3, a6, a7},
        },
        state_backend::owned_backend::Owned,
    };
    use rand::{Fill, thread_rng};
    use std::mem;
    use tezos_smart_rollup_constants::riscv::{
        REVEAL_REQUEST_MAX_SIZE, SBI_CONSOLE_PUTCHAR, SBI_FIRMWARE_TEZOS, SBI_TEZOS_INBOX_NEXT,
        SBI_TEZOS_REVEAL,
    };

    #[test]
    fn test_read_input() {
        type ML = M1M;
        type L = PvmLayout<ML, TestCacheLayouts>;
        type B = bcall::Interpreted<ML, Owned>;

        // Setup PVM
        let space = Owned::allocate::<L>();
        let mut pvm = Pvm::<ML, TestCacheLayouts, B, _>::bind(space, InterpretedBlockBuilder);
        pvm.reset();

        let level_addr = main_memory::FIRST_ADDRESS;
        let counter_addr = level_addr + 4;
        let buffer_addr = counter_addr + 4;

        const BUFFER_LEN: usize = 1024;

        // Configure machine for 'sbi_tezos_inbox_next'
        pvm.machine_state
            .core
            .hart
            .xregisters
            .write(a0, buffer_addr);
        pvm.machine_state
            .core
            .hart
            .xregisters
            .write(a1, BUFFER_LEN as u64);
        pvm.machine_state.core.hart.xregisters.write(a2, level_addr);
        pvm.machine_state
            .core
            .hart
            .xregisters
            .write(a3, counter_addr);
        pvm.machine_state
            .core
            .hart
            .xregisters
            .write(a7, SBI_FIRMWARE_TEZOS);
        pvm.machine_state
            .core
            .hart
            .xregisters
            .write(a6, SBI_TEZOS_INBOX_NEXT);

        // Should be in evaluating mode
        assert_eq!(pvm.status(), PvmStatus::Evaluating);

        // Handle the ECALL successfully
        let outcome =
            pvm.handle_exception(&mut Default::default(), EnvironException::EnvCallFromUMode);
        assert!(!outcome);

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
            pvm.machine_state.core.hart.xregisters.read(a0) as usize,
            BUFFER_LEN
        );
        assert_eq!(
            pvm.machine_state.core.main_memory.read(level_addr),
            Ok(level)
        );
        assert_eq!(
            pvm.machine_state.core.main_memory.read(counter_addr),
            Ok(counter)
        );

        // Payload in memory should be as expected
        for (offset, &byte) in payload[..BUFFER_LEN].iter().enumerate() {
            let addr = buffer_addr + offset as u64;
            let byte_written: u8 = pvm.machine_state.core.main_memory.read(addr).unwrap();
            assert_eq!(
                byte, byte_written,
                "Byte at {addr:x} (offset {offset}) is not the same"
            );
        }

        // Data after the buffer should be untouched
        assert!(
            (BUFFER_LEN..4096)
                .map(|offset| {
                    let addr = buffer_addr + offset as u64;
                    pvm.machine_state.core.main_memory.read(addr).unwrap()
                })
                .all(|b: u8| b == 0)
        );
    }

    #[test]
    fn test_write_debug() {
        type ML = M1M;
        type L = PvmLayout<ML, TestCacheLayouts>;
        type B = bcall::Interpreted<ML, Owned>;

        let mut buffer = Vec::new();
        let mut hooks = PvmHooks::new(|c| buffer.push(c));

        // Setup PVM
        let space = Owned::allocate::<L>();
        let mut pvm = Pvm::<ML, TestCacheLayouts, B, _>::bind(space, InterpretedBlockBuilder);
        pvm.reset();

        // Prepare subsequent ECALLs to use the SBI_CONSOLE_PUTCHAR extension
        pvm.machine_state
            .core
            .hart
            .xregisters
            .write(a7, SBI_CONSOLE_PUTCHAR);

        // Write characters
        let mut written = Vec::new();
        for _ in 0..10 {
            let char: u8 = rand::random();
            pvm.machine_state
                .core
                .hart
                .xregisters
                .write(a0, char as u64);
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

    backend_test!(test_reveal, F, {
        type ML = M1M;
        type L = PvmLayout<ML, TestCacheLayouts>;
        type B<F> = bcall::Interpreted<ML, <F as TestBackendFactory>::Manager>;

        // Setup PVM
        let mut pvm = create_state!(Pvm, L, F, ML, TestCacheLayouts, B<F>, || {
            InterpretedBlockBuilder
        });
        pvm.reset();

        let input_address = main_memory::FIRST_ADDRESS;
        let buffer = [1u8, 2, 3, 4];
        let output_address = input_address + buffer.len() as u64;
        let xregisters = &mut pvm.machine_state.core.hart.xregisters;

        // Configure machine for 'sbi_tezos_reveal'
        xregisters.write(a7, SBI_FIRMWARE_TEZOS);

        xregisters.write(a6, SBI_TEZOS_REVEAL);

        xregisters.write(a0, input_address);

        xregisters.write(a1, buffer.len() as u64);

        xregisters.write(a2, output_address);

        xregisters.write(a3, REVEAL_REQUEST_MAX_SIZE as u64);

        pvm.machine_state
            .core
            .main_memory
            .write_all(input_address, &buffer)
            .unwrap();

        assert_eq!(pvm.status(), PvmStatus::Evaluating);

        // Handle the ECALL successfully
        let outcome =
            pvm.handle_exception(&mut Default::default(), EnvironException::EnvCallFromUMode);
        assert!(!outcome);

        // After the ECALL we should be waiting for reveal
        assert_eq!(pvm.status(), PvmStatus::WaitingForReveal);

        // After ECALL the reveal_request field should be set correctly
        assert_eq!(pvm.reveal_request.to_vec(), buffer);

        const REVEAL_DATA_SIZE: usize = 1000;
        let reveal_data = [1u8; REVEAL_DATA_SIZE];

        // Handle Reveal successfully
        let outcome = pvm.provide_reveal_response(&reveal_data);
        assert!(outcome, "Failed to provide reveal data to the PVM");

        // After the reveal the size of the data revealed should be written to a0
        assert_eq!(
            pvm.machine_state.core.hart.xregisters.read(a0) as usize,
            REVEAL_DATA_SIZE
        );

        let mut reveal_result_buffer = [0u8; REVEAL_DATA_SIZE];

        pvm.machine_state
            .core
            .main_memory
            .read_all(output_address, &mut reveal_result_buffer)
            .unwrap();

        // Reveal data returned correctly
        assert_eq!(reveal_result_buffer, reveal_data);
    });

    backend_test!(test_reveal_insufficient_buffer_size, F, {
        type ML = M1M;
        type L = PvmLayout<ML, TestCacheLayouts>;
        type B<F> = bcall::Interpreted<ML, <F as TestBackendFactory>::Manager>;

        // Setup PVM
        let mut pvm = create_state!(Pvm, L, F, ML, TestCacheLayouts, B<F>, || {
            InterpretedBlockBuilder
        });
        pvm.reset();

        const OUTPUT_BUFFER_SIZE: usize = 10;
        let input_address = main_memory::FIRST_ADDRESS;
        let buffer = [1u8, 2, 3, 4];
        let output_address = input_address + buffer.len() as u64;

        let xregisters = &mut pvm.machine_state.core.hart.xregisters;

        // Configure machine for 'sbi_tezos_reveal'
        xregisters.write(a7, SBI_FIRMWARE_TEZOS);

        xregisters.write(a6, SBI_TEZOS_REVEAL);

        xregisters.write(a0, input_address);

        xregisters.write(a1, buffer.len() as u64);

        xregisters.write(a2, output_address);

        xregisters.write(a3, OUTPUT_BUFFER_SIZE as u64);

        pvm.machine_state
            .core
            .main_memory
            .write_all(input_address, &buffer)
            .unwrap();

        assert_eq!(pvm.status(), PvmStatus::Evaluating);

        // Handle the ECALL successfully
        let outcome =
            pvm.handle_exception(&mut Default::default(), EnvironException::EnvCallFromUMode);
        assert!(!outcome);

        // After the ECALL we should be waiting for reveal
        assert_eq!(pvm.status(), PvmStatus::WaitingForReveal);

        // After ECALL the reveal_request field should be set correctly
        assert_eq!(pvm.reveal_request.to_vec(), buffer);

        const REVEAL_DATA_SIZE: usize = 1000;
        let reveal_data = [1u8; REVEAL_DATA_SIZE];

        // Handle Reveal successfully
        let outcome = pvm.provide_reveal_response(&reveal_data);
        assert!(outcome, "Failed to provide reveal data to the PVM");

        // After the reveal the size of the data revealed should be written to a0
        assert_eq!(
            pvm.machine_state.core.hart.xregisters.read(a0) as usize,
            OUTPUT_BUFFER_SIZE
        );

        let mut reveal_result_buffer = [0u8; OUTPUT_BUFFER_SIZE];

        pvm.machine_state
            .core
            .main_memory
            .read_all(output_address, &mut reveal_result_buffer)
            .unwrap();

        // Reveal data returned correctly
        assert_eq!(reveal_result_buffer, reveal_data[..OUTPUT_BUFFER_SIZE]);
    });
}
