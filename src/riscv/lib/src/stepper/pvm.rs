// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024-2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use super::{Stepper, StepperStatus};
use crate::machine_state::block_cache::bcall::{Block, Interpreted, InterpretedBlockBuilder};
use crate::state_backend::{ManagerBase, ManagerReadWrite};
use crate::{
    kernel_loader,
    machine_state::{
        CacheLayouts, DefaultCacheLayouts, MachineCoreState, MachineError,
        main_memory::{M1G, MainMemoryLayout},
    },
    program::Program,
    pvm::{Pvm, PvmHooks, PvmLayout, PvmStatus},
    range_utils::bound_saturating_sub,
    state_backend::{
        AllocatedOf, CommitmentLayout, FnManagerIdent, ProofLayout, ProofTree, Ref,
        hash::Hash,
        owned_backend::Owned,
        proof_backend::{ProofGen, proof::Proof},
        verify_backend::Verifier,
    },
    storage::binary,
};
use serde::{Serialize, de::DeserializeOwned};
use std::collections::HashMap;
use std::ops::Bound;
use std::sync::Arc;
use tezos_smart_rollup_utils::inbox::Inbox;

/// Error during PVM stepping
#[derive(Debug, derive_more::From, thiserror::Error, derive_more::Display)]
pub enum PvmStepperError {
    /// Errors related to the machine state
    MachineError(MachineError),

    /// Errors arising from loading the kernel
    KernelError(kernel_loader::Error),
}

type ResponseFn = Arc<dyn Fn() -> Result<Box<[u8]>, std::io::Error>>;

/// Wrapper over a PVM that lets you step through it
pub struct PvmStepper<
    'hooks,
    ML: MainMemoryLayout = M1G,
    CL: CacheLayouts = DefaultCacheLayouts,
    M: ManagerBase = Owned,
    B: Block<ML, M> = Interpreted<ML, M>,
> {
    pvm: Pvm<ML, CL, B, M>,
    hooks: PvmHooks<'hooks>,
    inbox: Inbox,
    rollup_address: [u8; 20],
    origination_level: u32,
    reveal_request_response_map: HashMap<Box<[u8]>, ResponseFn>,
}

impl<'hooks, ML: MainMemoryLayout, B: Block<ML, Owned>, CL: CacheLayouts>
    PvmStepper<'hooks, ML, CL, Owned, B>
{
    /// Create a new PVM stepper.
    pub fn new(
        program: &[u8],
        initrd: Option<&[u8]>,
        inbox: Inbox,
        hooks: PvmHooks<'hooks>,
        rollup_address: [u8; 20],
        origination_level: u32,
        block_builder: B::BlockBuilder,
    ) -> Result<Self, PvmStepperError> {
        let space = Owned::allocate::<PvmLayout<ML, CL>>();
        let mut pvm = Pvm::bind(space, block_builder);

        let program = Program::<ML>::from_elf(program)?;

        #[cfg(feature = "supervisor")]
        {
            pvm.machine_state.setup_linux_process(&program)?;
            assert!(initrd.is_none(), "initrd is not supported");
        }

        #[cfg(not(feature = "supervisor"))]
        {
            pvm.machine_state.setup_boot(
                &program,
                initrd,
                crate::machine_state::mode::Mode::Supervisor,
            )?;
        }

        // TODO RV-458: Sandbox can load pre-images and provide them when it receives reveal request
        // Currently, one sample record with dummy data is added to the map for testing purpose
        let mut reveal_request_response_map: HashMap<Box<[u8]>, ResponseFn> = HashMap::new();

        reveal_request_response_map
            .insert(Box::new([1u8; 100]), Arc::new(|| Ok(Box::new([2u8; 100]))));

        Ok(Self {
            pvm,
            hooks,
            inbox,
            rollup_address,
            origination_level,
            reveal_request_response_map,
        })
    }
}

impl<'hooks, ML: MainMemoryLayout, CL: CacheLayouts> PvmStepper<'hooks, ML, CL, Owned> {
    /// Obtain the root hash for the PVM state.
    pub fn hash(&self) -> Hash {
        let refs = self.pvm.struct_ref::<FnManagerIdent>();
        PvmLayout::<ML, CL>::state_hash(refs).unwrap()
    }

    /// Produce the Merkle proof for evaluating one step on the given PVM state.
    /// The given stepper takes one step.
    pub fn produce_proof(&mut self) -> Option<Proof> {
        // Step using the proof mode stepper in order to obtain the proof
        let mut proof_stepper = self.start_proof_mode();

        proof_stepper.try_step().then(|| {
            let refs = proof_stepper
                .pvm
                .struct_ref::<crate::state_backend::FnManagerIdent>();
            let merkle_proof = PvmLayout::<ML, CL>::to_merkle_tree(refs)
                .expect("Obtaining the Merkle tree of a proof-gen state should not fail")
                .to_merkle_proof()
                .expect("Converting a Merkle tree to a compressed Merkle proof should not fail");

            let refs = proof_stepper.struct_ref();
            let final_hash = PvmLayout::<ML, CL>::state_hash(refs)
                .expect("Obtaining the final hash of the proof-gen state should not fail");
            Proof::new(merkle_proof, final_hash)
        })
    }
}

impl<'hooks, ML: MainMemoryLayout, CL: CacheLayouts, B: Block<ML, M>, M: ManagerReadWrite>
    PvmStepper<'hooks, ML, CL, M, B>
{
    /// Non-continuing variant of [`Stepper::step_max`]
    fn step_max_once(&mut self, steps: Bound<usize>) -> StepperStatus {
        #[cfg(feature = "supervisor")]
        if let Some(exit_code) = self.pvm.has_exited() {
            return StepperStatus::Exited {
                steps: 0,
                success: exit_code == 0,
                status: format!("Exited with code {}", exit_code),
            };
        }

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

            PvmStatus::WaitingForReveal => {
                let reveal_request = self.pvm.reveal_request();

                let Some(reveal_response_getter) = self
                    .reveal_request_response_map
                    .get(reveal_request.as_slice())
                else {
                    return StepperStatus::Errored {
                        steps: 0,
                        cause: "PVM was waiting for reveal response".to_owned(),
                        message: format!(
                            "Unable to handle reveal request {:02X?}",
                            reveal_request.as_slice()
                        ),
                    };
                };

                let reveal_response = match reveal_response_getter() {
                    Ok(response) => response,
                    Err(error) => {
                        return StepperStatus::Errored {
                            steps: 0,
                            cause: "PVM was waiting for reveal response".to_owned(),
                            message: format!(
                                "Retrieving reveal response did not succeed with {:?}",
                                error
                            )
                            .to_owned(),
                        };
                    }
                };

                let success = self.pvm.provide_reveal_response(&reveal_response);
                if success {
                    StepperStatus::Running { steps: 1 }
                } else {
                    StepperStatus::Errored {
                        steps: 0,
                        cause: "PVM was waiting for reveal response".to_owned(),
                        message: "Providing reveal response did not succeed".to_owned(),
                    }
                }
            }
        }
    }

    /// Try to take one step and return true if the PVM is not in an errored state.
    fn try_step(&mut self) -> bool {
        match self.step_max_once(Bound::Included(1)) {
            // We don't include errors in this case because errors count as 0 steps. That means if
            // we receive a [`StepperStatus::Errored`] with `steps: 1` then it tried to run 2 steps
            // but failed at the second.
            StepperStatus::Running { steps: 1 } | StepperStatus::Exited { steps: 1, .. } => true,
            _ => false,
        }
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub fn struct_ref(&self) -> AllocatedOf<PvmLayout<ML, CL>, Ref<'_, M>> {
        self.pvm.struct_ref::<FnManagerIdent>()
    }

    /// Re-bind the PVM type by serialising and deserialising its state in order to eliminate
    /// ephemeral state that doesn't make it into the serialised output.
    ///
    /// We additionally pass a new instance of the [`BlockBuilder`], to be used in the deserialised
    /// pvm.
    ///
    /// [`BlockBuilder`]: Block::BlockBuilder
    pub fn rebind_via_serde(&mut self, block_builder: B::BlockBuilder)
    where
        for<'a> AllocatedOf<PvmLayout<ML, CL>, Ref<'a, M>>: Serialize,
        AllocatedOf<PvmLayout<ML, CL>, M>: DeserializeOwned,
    {
        let space = {
            let refs = self.pvm.struct_ref::<FnManagerIdent>();
            let data = binary::serialise(&refs).unwrap();
            binary::deserialise(&data).unwrap()
        };

        self.pvm = Pvm::bind(space, block_builder);
    }
}

impl<'hooks, ML: MainMemoryLayout, CL: CacheLayouts, M: ManagerReadWrite>
    PvmStepper<'hooks, ML, CL, M>
{
    /// Create a new stepper in which the existing PVM is managed by
    /// the proof-generating backend.
    pub fn start_proof_mode<'a>(&'a self) -> PvmStepper<'hooks, ML, CL, ProofGen<Ref<'a, M>>> {
        PvmStepper::<'hooks, ML, CL, ProofGen<Ref<'a, M>>> {
            pvm: self.pvm.start_proof(),
            rollup_address: self.rollup_address,
            origination_level: self.origination_level,

            // The inbox needs to be cloned because we should not mutate it through the new stepper
            // instance.
            inbox: self.inbox.clone(),

            // We don't want to re-use the same hooks to avoid polluting logs with refutation game
            // output. Instead we use hooks that don't do anything.
            hooks: PvmHooks::none(),

            // TODO: RV-462 Pvm stepper uses response map less expensive to clone
            reveal_request_response_map: self.reveal_request_response_map.clone(),
        }
    }

    /// Verify a Merkle proof. The [`PvmStepper`] is used for inbox information.
    pub fn verify_proof(&self, proof: Proof) -> bool {
        // Allow some warnings while this method goes through iterations.
        #![allow(unreachable_code, unused_variables, clippy::diverging_sub_expression)]

        let proof_tree = ProofTree::Present(proof.tree());
        let Some(space) = PvmLayout::<ML, CL>::from_proof(proof_tree).ok() else {
            return false;
        };

        let pvm = Pvm::<ML, CL, Interpreted<_, _>, Verifier>::bind(space, InterpretedBlockBuilder);
        let mut stepper = PvmStepper {
            pvm,
            rollup_address: self.rollup_address,
            origination_level: self.origination_level,

            // The inbox needs to be cloned because we should not mutate it through the new stepper
            // instance.
            inbox: self.inbox.clone(),

            // We don't want to re-use the same hooks to avoid polluting logs with refutation game
            // output. Instead we use hooks that don't do anything.
            hooks: PvmHooks::none(),

            // TODO: RV-462 Pvm stepper uses response map less expensive to clone
            reveal_request_response_map: self.reveal_request_response_map.clone(),
        };

        stepper.try_step()
        // TODO: RV-362: Check the final state
        // let _refs = stepper.pvm.struct_ref::<FnManagerIdent>();
        // let hash: Hash = todo!("Can't obtain the hash of a verification state yet");
        // &hash == proof.final_state_hash()
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

impl<'hooks, ML: MainMemoryLayout, B: Block<ML, Owned>, CL: CacheLayouts> Stepper
    for PvmStepper<'hooks, ML, CL, Owned, B>
{
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
                    };
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
