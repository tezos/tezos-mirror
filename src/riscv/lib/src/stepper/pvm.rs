// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024-2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use super::{Stepper, StepperStatus};
use crate::state_backend::{ManagerBase, ManagerReadWrite};
use crate::{
    kernel_loader,
    machine_state::{
        main_memory::{MainMemoryLayout, M1G},
        mode::Mode,
        CacheLayouts, DefaultCacheLayouts, MachineCoreState, MachineError,
    },
    program::Program,
    pvm::{Pvm, PvmHooks, PvmLayout, PvmStatus},
    range_utils::bound_saturating_sub,
    state_backend::{
        hash::Hash,
        owned_backend::Owned,
        proof_backend::{merkle::Merkleisable, proof::Proof, ProofGen},
        verify_backend::Verifier,
        AllocatedOf, CommitmentLayout, FnManagerIdent, ProofLayout, ProofTree, Ref, RefOwnedAlloc,
    },
    storage::binary,
};
use serde::{de::DeserializeOwned, Serialize};
use std::ops::Bound;
use tezos_smart_rollup_utils::inbox::Inbox;

/// Error during PVM stepping
#[derive(Debug, derive_more::From, thiserror::Error, derive_more::Display)]
pub enum PvmStepperError {
    /// Errors related to the machine state
    MachineError(MachineError),

    /// Errors arising from loading the kernel
    KernelError(kernel_loader::Error),
}

/// Wrapper over a PVM that lets you step through it
pub struct PvmStepper<
    'hooks,
    ML: MainMemoryLayout = M1G,
    CL: CacheLayouts = DefaultCacheLayouts,
    M: ManagerBase = Owned,
> {
    pvm: Pvm<ML, CL, M>,
    hooks: PvmHooks<'hooks>,
    inbox: Inbox,
    rollup_address: [u8; 20],
    origination_level: u32,
}

impl<'hooks, ML: MainMemoryLayout, CL: CacheLayouts> PvmStepper<'hooks, ML, CL> {
    /// Create a new PVM stepper.
    pub fn new(
        program: &[u8],
        initrd: Option<&[u8]>,
        inbox: Inbox,
        hooks: PvmHooks<'hooks>,
        rollup_address: [u8; 20],
        origination_level: u32,
    ) -> Result<Self, PvmStepperError> {
        let space = Owned::allocate::<PvmLayout<ML, CL>>();
        let mut pvm = Pvm::bind(space);

        let program = Program::<ML>::from_elf(program)?;
        pvm.machine_state
            .setup_boot(&program, initrd, Mode::Supervisor)?;

        Ok(Self {
            pvm,
            hooks,
            inbox,
            rollup_address,
            origination_level,
        })
    }

    /// Non-continuing variant of [`Stepper::step_max`]
    fn step_max_once(&mut self, steps: Bound<usize>) -> StepperStatus {
        match self.pvm.status() {
            PvmStatus::Evaluating => {
                let steps = self.pvm.eval_max(&mut self.hooks, steps);
                StepperStatus::Running { steps }
            }

            PvmStatus::WaitingForInput => match self.inbox.next() {
                Some((level, counter, payload)) => {
                    let success = self.pvm.provide_input(level, counter, payload.as_slice());

                    if success {
                        StepperStatus::Running { steps: 1 }
                    } else {
                        StepperStatus::Errored {
                            steps: 0,
                            cause: "PVM was waiting for input".to_owned(),
                            message: "Providing input did not succeed".to_owned(),
                        }
                    }
                }

                None => {
                    if self.inbox.none_count() < 2 {
                        self.pvm.provide_no_input();
                        StepperStatus::Running { steps: 1 }
                    } else {
                        StepperStatus::Exited {
                            steps: 0,
                            success: true,
                            status: "Inbox has been drained".to_owned(),
                        }
                    }
                }
            },

            PvmStatus::WaitingForMetadata => {
                let success = self
                    .pvm
                    .provide_metadata(&self.rollup_address, self.origination_level);

                if success {
                    StepperStatus::Running { steps: 1 }
                } else {
                    StepperStatus::Errored {
                        steps: 0,
                        cause: "PVM was waiting for metadata".to_owned(),
                        message: "Providing metadata did not succeed".to_owned(),
                    }
                }
            }
        }
    }

    /// Obtain the root hash for the PVM state.
    pub fn hash(&self) -> Hash {
        let refs = self.pvm.struct_ref::<FnManagerIdent>();
        PvmLayout::<ML, CL>::state_hash(refs).unwrap()
    }

    /// Create a new stepper in which the existing PVM is managed by
    /// the proof-generating backend.
    pub fn start_proof_mode<'a>(&'a self) -> PvmStepper<'hooks, ML, CL, ProofGen<Ref<'a, Owned>>> {
        PvmStepper::<'hooks, ML, CL, ProofGen<Ref<'a, Owned>>> {
            pvm: self.pvm.start_proof(),
            hooks: PvmHooks::none(),
            inbox: self.inbox.clone(),
            rollup_address: self.rollup_address,
            origination_level: self.origination_level,
        }
    }

    /// Verify a Merkle proof. The [`PvmStepper`] is used for inbox information.
    pub fn verify_proof(&self, proof: Proof) -> bool {
        // Allow some warnings while this method goes through iterations.
        #![allow(
            dead_code,
            unreachable_code,
            unused_variables,
            clippy::diverging_sub_expression
        )]

        let proof_tree = ProofTree::Present(proof.tree());
        let Some(space) = PvmLayout::<ML, CL>::from_proof(proof_tree).ok() else {
            return false;
        };

        let pvm = Pvm::<ML, CL, Verifier>::bind(space);

        // TODO: RV-361: Exercise the PVM
        todo!("Can't exercise the PVM during proof verification yet");

        // TODO: RV-362: Check the final state
        let refs = pvm.struct_ref::<FnManagerIdent>();
        let hash: Hash = todo!("Can't obtain the hash of a verification state yet");

        &hash == proof.final_state_hash()
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub fn struct_ref(&self) -> RefOwnedAlloc<PvmLayout<ML, CL>> {
        self.pvm.struct_ref::<FnManagerIdent>()
    }

    /// Re-bind the PVM type by serialising and deserialising its state in order to eliminate
    /// ephemeral state that doesn't make it into the serialised output.
    pub fn rebind_via_serde(&mut self)
    where
        for<'a> RefOwnedAlloc<'a, PvmLayout<ML, CL>>: Serialize,
        AllocatedOf<PvmLayout<ML, CL>, Owned>: DeserializeOwned,
    {
        let space = {
            let refs = self.pvm.struct_ref::<FnManagerIdent>();
            let data = binary::serialise(&refs).unwrap();
            binary::deserialise(&data).unwrap()
        };

        self.pvm = Pvm::bind(space);
    }
}

impl<'hooks, ML: MainMemoryLayout, CL: CacheLayouts, M: ManagerReadWrite>
    PvmStepper<'hooks, ML, CL, M>
{
    /// Perform one evaluation step.
    pub fn eval_one(&mut self) {
        self.pvm.eval_one(&mut self.hooks)
    }
}

impl<'hooks, ML: MainMemoryLayout, CL: CacheLayouts> PvmStepper<'hooks, ML, CL>
where
    for<'a, 'b> AllocatedOf<PvmLayout<ML, CL>, Ref<'a, ProofGen<Ref<'b, Owned>>>>: Merkleisable,
{
    /// Produce the Merkle proof for evaluating one step on the given PVM state.
    /// The given stepper takes one step.
    pub fn produce_proof(&mut self) -> Option<Proof> {
        // Step using the proof mode stepper in order to obtain the proof
        let mut proof_stepper = self.start_proof_mode();
        proof_stepper.eval_one();
        let merkle_proof = proof_stepper
            .pvm
            .struct_ref::<crate::state_backend::FnManagerIdent>()
            .to_merkle_tree()
            .expect("Obtaining the Merkle tree of a proof-gen state should not fail")
            .to_merkle_proof()
            .expect("Converting a Merkle tree to a compressed Merkle proof should not fail");

        // TODO RV-373 : Proof-generating backend should also compute final state hash
        // Currently, the proof and the initial state hash are valid,
        // but the final state hash is not.
        Some(Proof::new(merkle_proof, self.hash()))
    }
}

impl<'hooks, ML: MainMemoryLayout, CL: CacheLayouts> Stepper for PvmStepper<'hooks, ML, CL> {
    type MainMemoryLayout = ML;

    type CacheLayouts = CL;

    type Manager = Owned;

    fn machine_state(&self) -> &MachineCoreState<Self::MainMemoryLayout, Self::Manager> {
        &self.pvm.machine_state.core
    }

    type StepResult = StepperStatus;

    fn step_max(&mut self, mut step_bounds: Bound<usize>) -> Self::StepResult {
        let mut total_steps = 0usize;

        loop {
            match self.step_max_once(step_bounds) {
                StepperStatus::Running { steps } => {
                    total_steps = total_steps.saturating_add(steps);
                    step_bounds = bound_saturating_sub(step_bounds, steps);

                    if steps < 1 {
                        // Break if no progress has been made.
                        break StepperStatus::Running { steps: total_steps };
                    }
                }

                StepperStatus::Exited {
                    steps,
                    success,
                    status,
                } => {
                    break StepperStatus::Exited {
                        steps: total_steps.saturating_add(steps),
                        success,
                        status,
                    }
                }

                StepperStatus::Errored {
                    steps,
                    cause,
                    message,
                } => {
                    break StepperStatus::Errored {
                        steps: total_steps.saturating_add(steps),
                        cause,
                        message,
                    };
                }
            }
        }
    }
}
