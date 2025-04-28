// SPDX-FileCopyrightText: 2023-2025 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::convert::Infallible;
use std::fmt;
use std::io::Write;
use std::io::stdout;
use std::ops::Bound;

use tezos_smart_rollup_constants::riscv::SbiError;

use super::linux;
use super::reveals::RevealRequest;
use super::reveals::RevealRequestLayout;
use crate::default::ConstDefault;
use crate::instruction_context::ICB;
use crate::machine_state;
use crate::machine_state::CacheLayouts;
use crate::machine_state::block_cache::block;
use crate::machine_state::block_cache::block::Block;
use crate::machine_state::csregisters::CSRegister;
use crate::machine_state::memory::MemoryConfig;
use crate::machine_state::registers::a0;
use crate::pvm::sbi;
use crate::range_utils::less_than_bound;
use crate::state::NewState;
use crate::state_backend;
use crate::state_backend::Atom;
use crate::state_backend::Cell;
use crate::state_backend::CommitmentLayout;
use crate::state_backend::FnManagerIdent;
use crate::state_backend::ManagerBase;
use crate::state_backend::ProofLayout;
use crate::state_backend::ProofTree;
use crate::state_backend::Ref;
use crate::state_backend::owned_backend::Owned;
use crate::state_backend::proof_backend::ProofGen;
use crate::state_backend::proof_backend::ProofWrapper;
use crate::state_backend::proof_backend::proof::MerkleProof;
use crate::state_backend::proof_backend::proof::Proof;
use crate::state_backend::verify_backend::Verifier;
use crate::storage::Hash;
use crate::storage::HashError;
use crate::struct_layout;
use crate::traps::EnvironException;

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
impl Default for PvmHooks<'_> {
    fn default() -> Self {
        fn putchar(char: u8) {
            stdout().lock().write_all(&[char]).unwrap();
        }

        Self::new(putchar)
    }
}

/// Type of input that can be passed to the PVM
pub enum PvmInput<'a> {
    InboxMessage {
        inbox_level: u32,
        message_counter: u64,
        payload: &'a [u8],
    },
    Reveal(&'a [u8]),
}

struct_layout! {
    pub struct PvmLayout<MC, CL> {
        machine_state: machine_state::MachineStateLayout<MC, CL>,
        reveal_request: RevealRequestLayout,
        system_state: linux::SupervisorStateLayout,
        version: Atom<u64>,
        tick: Atom<u64>,
        message_counter: Atom<u64>,
        level: Atom<u32>,
        level_is_set: Atom<bool>,
        status: Atom<PvmStatus>,
    }
}

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
pub(crate) type PvmProofGen<'a, MC, CL, M> = Pvm<
    MC,
    CL,
    block::Interpreted<MC, ProofGen<state_backend::Ref<'a, M>>>,
    ProofGen<state_backend::Ref<'a, M>>,
>;

/// Proof-generating virtual machine
pub struct Pvm<MC: MemoryConfig, CL: CacheLayouts, B: block::Block<MC, M>, M: ManagerBase> {
    pub(crate) machine_state: machine_state::MachineState<MC, CL, B, M>,
    reveal_request: RevealRequest<M>,
    pub(super) system_state: linux::SupervisorState<M>,
    version: Cell<u64, M>,
    pub(crate) tick: Cell<u64, M>,
    pub(crate) message_counter: Cell<u64, M>,
    pub(crate) level: Cell<u32, M>,
    pub(crate) level_is_set: Cell<bool, M>,
    status: Cell<PvmStatus, M>,
}

impl<
    MC: MemoryConfig,
    CL: machine_state::CacheLayouts,
    B: block::Block<MC, M>,
    M: state_backend::ManagerBase,
> Pvm<MC, CL, B, M>
{
    /// Allocate a new PVM.
    pub fn new(manager: &mut M, block_builder: B::BlockBuilder) -> Self
    where
        M: state_backend::ManagerAlloc,
    {
        Self {
            machine_state: machine_state::MachineState::new(manager, block_builder),
            reveal_request: RevealRequest::new(manager),
            system_state: linux::SupervisorState::new(manager),
            version: Cell::new_with(manager, INITIAL_VERSION),
            status: Cell::new(manager),
            tick: Cell::new(manager),
            message_counter: Cell::new(manager),
            level: Cell::new(manager),
            level_is_set: Cell::new(manager),
        }
    }

    /// Bind the block cache to the given allocated state and the given [block builder].
    ///
    /// [block builder]: block::Block::BlockBuilder
    pub(crate) fn bind(
        space: state_backend::AllocatedOf<PvmLayout<MC, CL>, M>,
        block_builder: B::BlockBuilder,
    ) -> Self
    where
        M::ManagerRoot: state_backend::ManagerReadWrite,
    {
        Self {
            machine_state: machine_state::MachineState::bind(space.machine_state, block_builder),
            reveal_request: RevealRequest::bind(space.reveal_request),
            system_state: linux::SupervisorState::bind(space.system_state),
            version: space.version,
            tick: space.tick,
            message_counter: space.message_counter,
            level: space.level,
            level_is_set: space.level_is_set,
            status: space.status,
        }
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub(crate) fn struct_ref<'a, F: state_backend::FnManager<state_backend::Ref<'a, M>>>(
        &'a self,
    ) -> state_backend::AllocatedOf<PvmLayout<MC, CL>, F::Output> {
        PvmLayoutF {
            machine_state: self.machine_state.struct_ref::<F>(),
            reveal_request: self.reveal_request.struct_ref::<F>(),
            system_state: self.system_state.struct_ref::<F>(),
            version: self.version.struct_ref::<F>(),
            tick: self.tick.struct_ref::<F>(),
            message_counter: self.message_counter.struct_ref::<F>(),
            level: self.level.struct_ref::<F>(),
            level_is_set: self.level_is_set.struct_ref::<F>(),
            status: self.status.struct_ref::<F>(),
        }
    }

    /// Generate a proof-generating version of this PVM.
    pub(crate) fn start_proof(&self) -> PvmProofGen<'_, MC, CL, M>
    where
        M: state_backend::ManagerRead,
    {
        let space = self.struct_ref::<ProofWrapper>();
        Pvm::bind(space, block::InterpretedBlockBuilder)
    }

    /// Reset the PVM.
    pub fn reset(&mut self)
    where
        M: state_backend::ManagerReadWrite,
    {
        self.machine_state.reset();
        self.version.write(INITIAL_VERSION);
        self.tick.write(0);
        self.message_counter.write(0);
        self.level.write(0);
        self.level_is_set.write(false);
        self.status.write(PvmStatus::DEFAULT);
    }

    /// Used for testing, corrupt the state so the following proofs will be incorrect.
    pub fn insert_failure(&mut self)
    where
        M: state_backend::ManagerReadWrite,
    {
        // We want to just slightly modify the state without interfering with normal execution.
        // dscratch1 is a debug register, any reasonable (test) kernel shouldn't interact with it.
        let csregs = &mut self.machine_state.core.hart.csregisters;
        let dscratch1 = csregs.read::<u64>(CSRegister::dscratch1);
        csregs.write(CSRegister::dscratch1, dscratch1 + 1);
    }

    /// Handle an exception using the defined Execution Environment.
    // The conditional compilation below causes some warnings.
    fn handle_exception(&mut self, hooks: &mut PvmHooks<'_>, _exception: EnvironException) -> bool
    where
        M: state_backend::ManagerReadWrite,
    {
        handle_system_call(
            &mut self.machine_state.core,
            &mut self.system_state,
            &mut self.status,
            &mut self.reveal_request,
            hooks,
        )
    }

    /// Perform one evaluation step.
    pub(crate) fn eval_one(&mut self, hooks: &mut PvmHooks<'_>)
    where
        M: state_backend::ManagerReadWrite,
    {
        // When the status is WaitingForReveal during evaluation, we know that
        // nothing has been returned by the rollup node and the reveal request
        // is invalid.
        if let PvmStatus::WaitingForReveal = self.status.read() {
            return self.provide_reveal_error_response();
        }

        if let Err(exc) = self.machine_state.step() {
            self.handle_exception(hooks, exc);
        }
        self.tick.write(self.tick.read().wrapping_add(1u64));
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
    pub(crate) fn eval_max(&mut self, hooks: &mut PvmHooks<'_>, step_bounds: Bound<usize>) -> usize
    where
        M: state_backend::ManagerReadWrite,
    {
        // Do nothing if step_bounds is less than 1
        if !less_than_bound(0, step_bounds) {
            return 0;
        }

        // When the status is WaitingForReveal during evaluation, we know that
        // nothing has been returned by the rollup node and the reveal request
        // is invalid.
        if let PvmStatus::WaitingForReveal = self.status.read() {
            self.provide_reveal_error_response();
            return 1;
        }

        let steps = self
            .machine_state
            .step_max_handle::<Infallible>(step_bounds, |machine_state, _exception| {
                Ok(handle_system_call(
                    &mut machine_state.core,
                    &mut self.system_state,
                    &mut self.status,
                    &mut self.reveal_request,
                    hooks,
                ))
            })
            .steps;
        self.tick.write(self.tick.read().wrapping_add(steps as u64));
        steps
    }

    /// Provide input. Returns `false` if the machine state is not expecting input.
    pub(crate) fn provide_input(&mut self, input: PvmInput) -> bool
    where
        M: state_backend::ManagerReadWrite,
    {
        // TODO RV-615: Remove `as u32` conversion
        match input {
            PvmInput::InboxMessage {
                inbox_level,
                message_counter,
                payload,
            } => self.provide_inbox_message(inbox_level, message_counter as u32, payload),
            PvmInput::Reveal(reveal_data) => self.provide_reveal_response(reveal_data),
        }
    }

    /// Provide an inbox message. Returns `false` if the machine state is not
    /// expecting a message.
    pub(crate) fn provide_inbox_message(&mut self, level: u32, counter: u32, payload: &[u8]) -> bool
    where
        M: state_backend::ManagerReadWrite,
    {
        if !sbi::provide_input(
            &mut self.status,
            &mut self.machine_state.core,
            level,
            counter,
            payload,
        ) {
            return false;
        }
        self.tick.write(self.tick.read().wrapping_add(1u64));
        self.message_counter.write(counter as u64);
        self.level_is_set.write(true);
        self.level.write(level);
        true
    }

    /// Provide reveal data in response to a reveal request.
    /// Returns `false` if the machine is not expecting a reveal.
    pub(crate) fn provide_reveal_response(&mut self, reveal_data: &[u8]) -> bool
    where
        M: state_backend::ManagerReadWrite,
    {
        if !sbi::provide_reveal_response(
            &mut self.status,
            &mut self.machine_state.core,
            reveal_data,
        ) {
            return false;
        }
        self.tick.write(self.tick.read().wrapping_add(1u64));
        true
    }

    /// Get the reveal request in the machine state.
    pub(crate) fn reveal_request(&self) -> Vec<u8>
    where
        M: state_backend::ManagerRead,
    {
        self.reveal_request.to_vec()
    }

    /// Provide a reveal error response to the PVM
    pub fn provide_reveal_error_response(&mut self)
    where
        M: state_backend::ManagerReadWrite,
    {
        self.machine_state
            .core
            .xregister_write(a0, SbiError::InvalidParam as u64);
        self.tick.write(self.tick.read().wrapping_add(1u64));
        self.status.write(PvmStatus::Evaluating);
    }

    /// Get the current machine status.
    pub fn status(&self) -> PvmStatus
    where
        M: state_backend::ManagerRead,
    {
        self.status.read()
    }
}

impl<MC: MemoryConfig, CL: CacheLayouts, B: Block<MC, Owned>> Pvm<MC, CL, B, Owned> {
    pub(crate) fn empty(block_builder: B::BlockBuilder) -> Self {
        Self::new(&mut Owned, block_builder)
    }

    pub(crate) fn hash(&self) -> Result<Hash, HashError> {
        let refs = self.struct_ref::<FnManagerIdent>();
        PvmLayout::<MC, CL>::state_hash(refs)
    }
}

impl<'a, MC: MemoryConfig, CL: CacheLayouts, B: Block<MC, ProofGen<Ref<'a, Owned>>>>
    Pvm<MC, CL, B, ProofGen<Ref<'a, Owned>>>
{
    /// Produce a proof.
    pub(crate) fn to_proof(&self) -> Result<Proof, HashError> {
        let refs = self.struct_ref::<FnManagerIdent>();
        let merkle_proof = PvmLayout::<MC, CL>::to_merkle_tree(refs)?.to_merkle_proof()?;

        let refs = self.struct_ref::<FnManagerIdent>();
        let final_hash = PvmLayout::<MC, CL>::state_hash(refs)?;
        let proof = Proof::new(merkle_proof, final_hash);

        Ok(proof)
    }
}

impl<MC: MemoryConfig, CL: CacheLayouts, B: Block<MC, Verifier>> Pvm<MC, CL, B, Verifier> {
    /// Construct a PVM state from a Merkle proof.
    pub fn from_proof(proof: &MerkleProof, block_builder: B::BlockBuilder) -> Option<Self> {
        let space =
            <PvmLayout<MC, CL> as ProofLayout>::from_proof(ProofTree::Present(proof)).ok()?;
        Some(Self::bind(space, block_builder))
    }
}

impl<
    MC: MemoryConfig,
    CL: CacheLayouts,
    B: block::Block<MC, M> + Clone,
    M: state_backend::ManagerClone,
> Clone for Pvm<MC, CL, B, M>
{
    fn clone(&self) -> Self {
        Self {
            machine_state: self.machine_state.clone(),
            reveal_request: self.reveal_request.clone(),
            system_state: self.system_state.clone(),
            version: self.version.clone(),
            tick: self.tick.clone(),
            message_counter: self.message_counter.clone(),
            level: self.level.clone(),
            level_is_set: self.level_is_set.clone(),
            status: self.status.clone(),
        }
    }
}

fn handle_system_call<MC, M>(
    core: &mut machine_state::MachineCoreState<MC, M>,
    system_state: &mut linux::SupervisorState<M>,
    status: &mut Cell<PvmStatus, M>,
    reveal_request: &mut RevealRequest<M>,
    hooks: &mut PvmHooks,
) -> bool
where
    MC: MemoryConfig,
    M: state_backend::ManagerReadWrite,
{
    system_state.handle_system_call(core, hooks, |core| {
        sbi::handle_tezos(core, status, reveal_request);
        status.read() == PvmStatus::Evaluating
    })
}

#[cfg(test)]
mod tests {
    use std::mem;

    use proptest::proptest;
    use rand::Fill;
    use rand::thread_rng;
    use tezos_smart_rollup_constants::riscv::REVEAL_REQUEST_MAX_SIZE;
    use tezos_smart_rollup_constants::riscv::SBI_FIRMWARE_TEZOS;
    use tezos_smart_rollup_constants::riscv::SBI_TEZOS_INBOX_NEXT;
    use tezos_smart_rollup_constants::riscv::SBI_TEZOS_REVEAL;

    use super::*;
    use crate::backend_test;
    use crate::machine_state::TestCacheLayouts;
    use crate::machine_state::block_cache::block::InterpretedBlockBuilder;
    use crate::machine_state::memory;
    use crate::machine_state::memory::M1M;
    use crate::machine_state::memory::Memory;
    use crate::machine_state::registers::a0;
    use crate::machine_state::registers::a1;
    use crate::machine_state::registers::a2;
    use crate::machine_state::registers::a3;
    use crate::machine_state::registers::a6;
    use crate::machine_state::registers::a7;
    use crate::pvm::common::tests::memory::Address;
    use crate::pvm::linux;
    use crate::state_backend::owned_backend::Owned;
    use crate::state_backend::test_helpers::TestBackendFactory;

    #[test]
    fn test_read_input() {
        type MC = M1M;
        type B = block::Interpreted<MC, Owned>;

        // Setup PVM
        let mut pvm = Pvm::<MC, TestCacheLayouts, B, _>::new(&mut Owned, InterpretedBlockBuilder);
        pvm.reset();
        pvm.machine_state
            .core
            .main_memory
            .set_all_readable_writeable();

        let level_addr = memory::FIRST_ADDRESS;
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
        let outcome = pvm.handle_exception(&mut Default::default(), EnvironException::EnvCall);
        assert!(!outcome);

        // After the ECALL we should be waiting for input
        assert_eq!(pvm.status(), PvmStatus::WaitingForInput);

        // Respond to the request for input
        let level = rand::random();
        let counter = rand::random();
        let mut payload = [0u8; BUFFER_LEN + 10];
        payload.try_fill(&mut thread_rng()).unwrap();
        assert!(pvm.provide_inbox_message(level, counter, &payload));

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
        const WRITTEN_SIZE: usize = 100;
        proptest!(|(
            address in 0u64 as Address..(1024 * 1024 - WRITTEN_SIZE) as Address,
            written: [u8; WRITTEN_SIZE],
        )|{
            type MC = M1M;
            type B = block::Interpreted<MC, Owned>;

            let mut buffer = Vec::new();
            let mut hooks = PvmHooks::new(|c| buffer.push(c));

            // Setup PVM
            let mut pvm = Pvm::<MC, TestCacheLayouts, B, _>::new(&mut Owned, InterpretedBlockBuilder);
            pvm.reset();
            pvm.machine_state
                .core
                .main_memory
                .set_all_readable_writeable();

            // Write characters
            pvm.machine_state
                .core
                .main_memory
                .write_all(address, &written)
                .unwrap();

            // Write the `write` system call number for `ecall`
            pvm.machine_state.core.hart.xregisters.write(a7, linux::WRITE);

            // Write `stdout` as the file descriptor parameter
            pvm.machine_state.core.hart.xregisters.write(a0, 1);

            // Write the address for the string to be read from
            pvm.machine_state
                .core
                .hart
                .xregisters
                .write(a1, address);

            // Write the length of the string
            pvm.machine_state
                .core
                .hart
                .xregisters
                .write(a2, written.len() as u64);

            pvm.handle_exception(&mut hooks, EnvironException::EnvCall);

            // Drop `hooks` to regain access to the mutable references it kept
            mem::drop(hooks);

            // Compare what characters have been passed to the hook versus what we
            // intended to write
            assert_eq!(written.to_vec(), buffer);
        });
    }

    backend_test!(test_reveal, F, {
        type MC = M1M;
        type B<F> = block::Interpreted<MC, <F as TestBackendFactory>::Manager>;

        // Setup PVM
        let mut pvm =
            Pvm::<MC, TestCacheLayouts, B<F>, _>::new(&mut F::manager(), InterpretedBlockBuilder);
        pvm.reset();
        pvm.machine_state
            .core
            .main_memory
            .set_all_readable_writeable();

        let input_address = memory::FIRST_ADDRESS;
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
        let outcome = pvm.handle_exception(&mut Default::default(), EnvironException::EnvCall);
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
        type MC = M1M;
        type B<F> = block::Interpreted<MC, <F as TestBackendFactory>::Manager>;

        // Setup PVM
        let mut pvm =
            Pvm::<MC, TestCacheLayouts, B<F>, _>::new(&mut F::manager(), InterpretedBlockBuilder);
        pvm.reset();
        pvm.machine_state
            .core
            .main_memory
            .set_all_readable_writeable();

        const OUTPUT_BUFFER_SIZE: usize = 10;
        let input_address = memory::FIRST_ADDRESS;
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
        let outcome = pvm.handle_exception(&mut Default::default(), EnvironException::EnvCall);
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
