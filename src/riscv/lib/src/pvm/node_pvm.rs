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
use crate::machine_state::block_cache::bcall::Interpreted;
use crate::machine_state::block_cache::bcall::InterpretedBlockBuilder;
use crate::machine_state::mode::Mode;
use crate::program::Program;
use crate::pvm::common::PvmHooks;
use crate::pvm::common::PvmStatus;
use crate::state_backend;
use crate::state_backend::AllocatedOf;
use crate::state_backend::owned_backend::Owned;
use crate::state_backend::proof_backend::proof::MerkleProof;
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
        self.with_backend(|state| state.status())
    }

    pub fn get_tick(&self) -> u64
    where
        M: state_backend::ManagerRead,
    {
        self.with_backend(|state| state.tick.read())
    }

    pub fn get_current_level(&self) -> Option<u32>
    where
        M: state_backend::ManagerRead,
    {
        self.with_backend(|state| {
            if state.level_is_set.read() {
                Some(state.level.read())
            } else {
                None
            }
        })
    }

    pub fn get_message_counter(&self) -> u64
    where
        M: state_backend::ManagerRead,
    {
        self.with_backend(|state| state.message_counter.read())
    }

    /// Get the reveal request from the PVM state
    pub fn get_reveal_request(&self) -> Vec<u8>
    where
        M: state_backend::ManagerRead,
    {
        self.with_backend(|state| state.reveal_request())
    }

    pub fn install_boot_sector(&mut self, loader: &[u8], kernel: &[u8])
    where
        M: state_backend::ManagerReadWrite,
    {
        self.with_backend_mut(|state| {
            let program = Program::<NodePvmMemConfig>::from_elf(loader)
                .expect("Could not parse Hermit loader");
            state
                .machine_state
                .setup_boot(&program, Some(kernel), Mode::Supervisor)
                .unwrap()
        })
    }

    pub fn compute_step(&mut self, pvm_hooks: &mut PvmHooks)
    where
        M: state_backend::ManagerReadWrite,
    {
        self.with_backend_mut(|state| state.eval_one(pvm_hooks))
    }

    pub fn compute_step_many(&mut self, pvm_hooks: &mut PvmHooks, max_steps: usize) -> i64
    where
        M: state_backend::ManagerReadWrite,
    {
        self.with_backend_mut(|state| state.eval_max(pvm_hooks, Bound::Included(max_steps))) as i64
    }

    pub fn set_input_message(&mut self, level: u32, message_counter: u64, input: Vec<u8>)
    where
        M: state_backend::ManagerReadWrite,
    {
        self.with_backend_mut(|state| {
            assert!(
                state.provide_input(level, message_counter as u32, &input),
                "Cannot accept input in current state ({})",
                state.status()
            )
        })
    }

    /// Set reveal data reponse to pvm state
    pub fn set_reveal_response(&mut self, reveal_data: &[u8])
    where
        M: state_backend::ManagerReadWrite,
    {
        self.with_backend_mut(|state| {
            assert!(
                state.provide_reveal_response(reveal_data),
                "Cannot accept reveal in current state ({})",
                state.status()
            )
        })
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
        self.with_backend_mut(|state| {
            state.insert_failure();
        })
    }
}

impl NodePvm {
    /// Construct an empty PVM state.
    pub fn empty() -> Self {
        let space = Owned::allocate::<NodePvmLayout>();
        Self::bind(space)
    }

    /// Compute the root hash of the PVM state.
    pub fn hash(&self) -> Hash {
        self.with_backend(|state| state.hash().unwrap())
    }
}

impl NodePvm<Verifier> {
    /// Construct a PVM state from a Merkle proof.
    pub fn from_proof(proof: &MerkleProof) -> Option<Self> {
        Pvm::from_proof(proof, InterpretedBlockBuilder).map(|s| Self { state: Box::new(s) })
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
        Ok(state.with_backend(|state| {
            let struct_ref = state.struct_ref::<state_backend::FnManagerIdent>();
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
