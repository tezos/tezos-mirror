// SPDX-FileCopyrightText: 2024-2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::fmt;
use std::ops::Bound;
use std::path::Path;

use thiserror::Error;

use super::Pvm;
use super::PvmLayout;
use crate::machine_state::TestCacheLayouts;
use crate::machine_state::block_cache::block::Interpreted;
use crate::machine_state::block_cache::block::InterpretedBlockBuilder;
use crate::program::Program;
use crate::pvm::common::PvmHooks;
use crate::pvm::common::PvmInput;
use crate::pvm::common::PvmStatus;
use crate::state::NewState;
use crate::state_backend;
use crate::state_backend::AllocatedOf;
use crate::state_backend::FnManagerIdent;
use crate::state_backend::ProofLayout;
use crate::state_backend::ProofTree;
use crate::state_backend::owned_backend::Owned;
use crate::state_backend::proof_backend::proof::Proof;
use crate::state_backend::verify_backend::Verifier;
use crate::storage;
use crate::storage::Hash;
use crate::storage::Repo;

#[derive(Error, Debug)]
pub enum PvmError {
    #[error("Serialization error: {0}")]
    SerializationError(String),
}

type NodePvmMemConfig = crate::machine_state::memory::M64M;

type NodePvmLayout = PvmLayout<NodePvmMemConfig, TestCacheLayouts>;

type NodePvmState<M> = Pvm<NodePvmMemConfig, TestCacheLayouts, Interpreted<NodePvmMemConfig, M>, M>;

pub struct NodePvm<M: state_backend::ManagerBase = Owned> {
    state: Box<NodePvmState<M>>,
}

impl<M: state_backend::ManagerBase> NodePvm<M> {
    pub fn bind(space: AllocatedOf<NodePvmLayout, M>) -> Self
    where
        M::ManagerRoot: state_backend::ManagerReadWrite,
    {
        let state = NodePvmState::<M>::bind(space, InterpretedBlockBuilder);
        Self {
            state: Box::new(state),
        }
    }

    fn with_backend_mut<T, F>(&mut self, f: F) -> T
    where
        F: FnOnce(&mut NodePvmState<M>) -> T,
    {
        f(&mut self.state)
    }

    fn with_backend<T, F>(&self, f: F) -> T
    where
        F: FnOnce(&NodePvmState<M>) -> T,
    {
        f(&self.state)
    }

    pub fn get_status(&self) -> PvmStatus
    where
        M: state_backend::ManagerRead,
    {
        self.with_backend(|pvm| pvm.status())
    }

    pub fn get_tick(&self) -> u64
    where
        M: state_backend::ManagerRead,
    {
        self.with_backend(|pvm| pvm.tick.read())
    }

    pub fn get_current_level(&self) -> Option<u32>
    where
        M: state_backend::ManagerRead,
    {
        self.with_backend(|pvm| {
            if pvm.level_is_set.read() {
                Some(pvm.level.read())
            } else {
                None
            }
        })
    }

    pub fn get_message_counter(&self) -> u64
    where
        M: state_backend::ManagerRead,
    {
        self.with_backend(|pvm| pvm.message_counter.read())
    }

    /// Get the reveal request from the PVM state
    pub fn get_reveal_request(&self) -> Vec<u8>
    where
        M: state_backend::ManagerRead,
    {
        self.with_backend(|pvm| pvm.reveal_request())
    }

    pub fn install_boot_sector(&mut self, kernel: &[u8])
    where
        M: state_backend::ManagerReadWrite,
    {
        self.with_backend_mut(|pvm| {
            let program = Program::from_elf(kernel).unwrap();
            pvm.setup_linux_process(&program).unwrap()
        })
    }

    pub fn compute_step(&mut self, pvm_hooks: &mut PvmHooks)
    where
        M: state_backend::ManagerReadWrite,
    {
        self.with_backend_mut(|pvm| pvm.eval_one(pvm_hooks))
    }

    pub fn compute_step_many(&mut self, pvm_hooks: &mut PvmHooks, max_steps: usize) -> i64
    where
        M: state_backend::ManagerReadWrite,
    {
        self.with_backend_mut(|pvm| pvm.eval_max(pvm_hooks, Bound::Included(max_steps))) as i64
    }

    pub fn set_input(&mut self, input: PvmInput) -> bool
    where
        M: state_backend::ManagerReadWrite,
    {
        self.with_backend_mut(|pvm| pvm.provide_input(input))
    }

    /// Only used by the rollup node in "loser mode" in order to test
    /// refutation games.
    ///
    /// Corrupt the state so commitments after this point will conflict with
    /// those of an honest operator.
    pub fn insert_failure(&mut self)
    where
        M: state_backend::ManagerReadWrite,
    {
        self.with_backend_mut(|pvm| {
            pvm.insert_failure();
        })
    }
}

impl NodePvm {
    /// Construct an empty PVM state.
    pub fn empty() -> Self {
        Self::new(&mut Owned)
    }

    /// Compute the root hash of the PVM state.
    pub fn hash(&self) -> Hash {
        self.with_backend(|pvm| pvm.hash().unwrap())
    }

    /// Produce the Merkle proof corresponding to the next step of the PVM.
    /// If the next step is an input request, provide the given input.
    pub fn produce_proof(
        &self,
        input: Option<PvmInput>,
        pvm_hooks: &mut PvmHooks,
    ) -> Option<Proof> {
        let mut proof_state = self.state.start_proof();

        match input {
            None => proof_state.eval_one(pvm_hooks),
            Some(input) => {
                if !proof_state.provide_input(input) {
                    return None;
                }
            }
        }

        let proof = proof_state.to_proof().ok()?;
        Some(proof)
    }
}

impl NodePvm<Verifier> {
    /// Verify the proof with the given input. Upon success, return the input
    /// request which corresponds to the initial state of the proof.
    pub fn verify_proof(
        proof: &Proof,
        input: Option<PvmInput>,
        pvm_hooks: &mut PvmHooks,
    ) -> Option<()> {
        let proof_tree = proof.tree();
        let mut pvm = Pvm::from_proof(proof_tree, InterpretedBlockBuilder).map(|state| Self {
            state: Box::new(state),
        })?;

        pvm.with_backend_mut(|pvm| {
            match input {
                None => pvm.eval_one(pvm_hooks),
                Some(input) => {
                    if !pvm.provide_input(input) {
                        return None;
                    }
                }
            };

            let refs = pvm.struct_ref::<FnManagerIdent>();
            let final_hash =
                NodePvmLayout::partial_state_hash(refs, ProofTree::Present(proof_tree)).ok()?;
            if final_hash != proof.final_state_hash() {
                return None;
            }

            // TODO: RV-556: Construct and return input request upon successful verification
            todo!()
        })
    }
}

impl<M: state_backend::ManagerSerialise> fmt::Debug for NodePvm<M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let refs = self.state.struct_ref::<state_backend::FnManagerIdent>();
        let rendered = if f.alternate() {
            serde_json::to_string_pretty(&refs)
        } else {
            serde_json::to_string(&refs)
        }
        .expect("Could not serialize PVM state");
        f.write_str(&rendered)
    }
}

impl<M: state_backend::ManagerClone> Clone for NodePvm<M> {
    fn clone(&self) -> Self {
        Self {
            state: self.state.clone(),
        }
    }
}

impl PartialEq for NodePvm {
    fn eq(&self, other: &Self) -> bool {
        self.state.struct_ref::<state_backend::FnManagerIdent>()
            == other.state.struct_ref::<state_backend::FnManagerIdent>()
    }
}

impl Eq for NodePvm {}

impl<M: state_backend::ManagerBase> NewState<M> for NodePvm<M> {
    fn new(manager: &mut M) -> Self
    where
        M: state_backend::ManagerAlloc,
    {
        Self {
            state: Box::new(NodePvmState::<M>::new(manager, InterpretedBlockBuilder)),
        }
    }
}

#[derive(Error, Debug)]
pub enum PvmStorageError {
    #[error("Storage error: {0}")]
    StorageError(#[from] storage::StorageError),

    #[error("Serialization error: {0}")]
    PvmError(#[from] PvmError),
}

pub struct PvmStorage {
    repo: Repo,
}

impl PvmStorage {
    /// Load or create new repo at `path`.
    pub fn load(path: impl AsRef<Path>) -> Result<PvmStorage, PvmStorageError> {
        let repo = Repo::load(path)?;
        Ok(PvmStorage { repo })
    }

    pub fn close(self) {
        self.repo.close()
    }

    /// Create a new commit for `state` and  return the commit id.
    pub fn commit(&mut self, state: &NodePvm) -> Result<Hash, PvmStorageError> {
        Ok(state.with_backend(|pvm| {
            let struct_ref = pvm.struct_ref::<state_backend::FnManagerIdent>();
            self.repo.commit_serialised(&struct_ref)
        })?)
    }

    /// Checkout the PVM state committed under `id`, if the commit exists.
    pub fn checkout(&self, id: &Hash) -> Result<NodePvm, PvmStorageError> {
        let allocated: AllocatedOf<NodePvmLayout, Owned> = self.repo.checkout_serialised(id)?;
        Ok(NodePvm::bind(allocated))
    }

    /// A snapshot is a new repo to which only `id` has been committed.
    pub fn export_snapshot(
        &self,
        id: &Hash,
        path: impl AsRef<Path>,
    ) -> Result<(), PvmStorageError> {
        Ok(self.repo.export_snapshot(id, path)?)
    }
}
