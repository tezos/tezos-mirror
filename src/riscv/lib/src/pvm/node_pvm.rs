// SPDX-FileCopyrightText: 2024-2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::fmt;
use std::ops::Bound;
use std::path::Path;

use thiserror::Error;

use crate::machine_state::TestCacheLayouts;
use crate::machine_state::block_cache::bcall::Interpreted;
use crate::machine_state::block_cache::bcall::InterpretedBlockBuilder;
use crate::machine_state::memory::M64M;
use crate::machine_state::mode::Mode;
use crate::program::Program;
use crate::pvm::common::Pvm;
use crate::pvm::common::PvmHooks;
use crate::pvm::common::PvmLayout;
use crate::pvm::common::PvmStatus;
use crate::state_backend;
use crate::state_backend::AllocatedOf;
use crate::state_backend::CommitmentLayout;
use crate::state_backend::ProofLayout;
use crate::state_backend::ProofTree;
use crate::state_backend::Ref;
use crate::state_backend::owned_backend::Owned;
use crate::state_backend::proof_backend::ProofGen;
use crate::state_backend::proof_backend::ProofWrapper;
use crate::state_backend::proof_backend::proof::MerkleProof;
use crate::state_backend::verify_backend::Verifier;
use crate::storage;
use crate::storage::Hash;
use crate::storage::Repo;

pub type StateLayout = (
    PvmLayout<M64M, TestCacheLayouts>,
    state_backend::Atom<bool>,
    state_backend::Atom<u32>,
    state_backend::Atom<u64>,
    state_backend::Atom<u64>,
);

pub struct State<M: state_backend::ManagerBase> {
    pvm: Pvm<M64M, TestCacheLayouts, Interpreted<M64M, M>, M>,
    level_is_set: state_backend::Cell<bool, M>,
    level: state_backend::Cell<u32, M>,
    message_counter: state_backend::Cell<u64, M>,
    tick: state_backend::Cell<u64, M>,
}

impl<M: state_backend::ManagerBase> State<M> {
    pub fn bind(space: state_backend::AllocatedOf<StateLayout, M>) -> Self
    where
        M::ManagerRoot: state_backend::ManagerReadWrite,
    {
        Self {
            pvm: Pvm::<M64M, _, _, M>::bind(space.0, InterpretedBlockBuilder),
            level_is_set: space.1,
            level: space.2,
            message_counter: space.3,
            tick: space.4,
        }
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub fn struct_ref<'a, F: state_backend::FnManager<state_backend::Ref<'a, M>>>(
        &'a self,
    ) -> state_backend::AllocatedOf<StateLayout, F::Output> {
        (
            self.pvm.struct_ref::<F>(),
            self.level_is_set.struct_ref::<F>(),
            self.level.struct_ref::<F>(),
            self.message_counter.struct_ref::<F>(),
            self.tick.struct_ref::<F>(),
        )
    }

    /// Generate a proof-generating version of this state.
    pub fn start_proof(&self) -> State<ProofGen<Ref<'_, M>>>
    where
        M: state_backend::ManagerRead,
    {
        State::bind(self.struct_ref::<ProofWrapper>())
    }
}

impl<M: state_backend::ManagerClone> Clone for State<M> {
    fn clone(&self) -> Self {
        Self {
            pvm: self.pvm.clone(),
            level_is_set: self.level_is_set.clone(),
            level: self.level.clone(),
            message_counter: self.message_counter.clone(),
            tick: self.tick.clone(),
        }
    }
}

impl<M: state_backend::ManagerSerialise> fmt::Debug for State<M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let refs = self.struct_ref::<state_backend::FnManagerIdent>();
        let rendered = if f.alternate() {
            serde_json::to_string_pretty(&refs)
        } else {
            serde_json::to_string(&refs)
        }
        .expect("Could not serialize PVM state");
        f.write_str(&rendered)
    }
}

#[derive(Error, Debug)]
pub enum PvmError {
    #[error("Serialization error: {0}")]
    SerializationError(String),
}

pub struct NodePvm<M: state_backend::ManagerBase = Owned> {
    state: Box<State<M>>,
}

impl<M: state_backend::ManagerSerialise> fmt::Debug for NodePvm<M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.state.fmt(f)
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

impl NodePvm<Owned> {
    /// Construct an empty PVM state.
    pub fn empty() -> Self {
        let space = Owned::allocate::<StateLayout>();
        let state = State::bind(space);
        Self {
            state: Box::new(state),
        }
    }

    pub fn hash(&self) -> Hash {
        self.with_backend(|state| {
            let refs = state.struct_ref::<state_backend::FnManagerIdent>();
            <StateLayout as CommitmentLayout>::state_hash(refs).unwrap()
        })
    }

    /// Used for testing refutation games, corrupt the state so commitments
    /// after this point will conflict with those of an honest operator.
    pub fn insert_failure(&mut self) {
        self.with_backend_mut(|state| {
            state.pvm.insert_failure();
        })
    }
}

impl NodePvm<Verifier> {
    /// Construct a PVM state from a Merkle proof.
    pub fn from_proof(proof: &MerkleProof) -> Option<Self> {
        let space = <StateLayout as ProofLayout>::from_proof(ProofTree::Present(proof)).ok()?;
        let state = State::bind(space);
        let state = Self {
            state: Box::new(state),
        };
        Some(state)
    }
}

impl<M: state_backend::ManagerBase> NodePvm<M> {
    fn with_backend_mut<T, F>(&mut self, f: F) -> T
    where
        F: FnOnce(&mut State<M>) -> T,
    {
        f(&mut self.state)
    }

    fn with_backend<T, F>(&self, f: F) -> T
    where
        F: FnOnce(&State<M>) -> T,
    {
        f(&self.state)
    }

    pub fn get_status(&self) -> PvmStatus
    where
        M: state_backend::ManagerRead,
    {
        self.with_backend(|state| state.pvm.status())
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
        self.with_backend(|state| state.pvm.reveal_request())
    }

    pub fn install_boot_sector(&mut self, loader: &[u8], kernel: &[u8])
    where
        M: state_backend::ManagerReadWrite,
    {
        self.with_backend_mut(|state| {
            let program = Program::<M64M>::from_elf(loader).expect("Could not parse Hermit loader");
            state
                .pvm
                .machine_state
                .setup_boot(&program, Some(kernel), Mode::Supervisor)
                .unwrap()
        })
    }

    pub fn compute_step(&mut self, pvm_hooks: &mut PvmHooks)
    where
        M: state_backend::ManagerReadWrite,
    {
        self.with_backend_mut(|state| {
            state.pvm.eval_one(pvm_hooks);
            state.tick.write(state.tick.read() + 1);
        })
    }

    pub fn compute_step_many(&mut self, pvm_hooks: &mut PvmHooks, max_steps: usize) -> i64
    where
        M: state_backend::ManagerReadWrite,
    {
        self.with_backend_mut(|state| {
            let steps = state.pvm.eval_max(pvm_hooks, Bound::Included(max_steps));
            state.tick.write(state.tick.read() + steps as u64);
            steps as i64
        })
    }

    pub fn set_input_message(&mut self, level: u32, message_counter: u64, input: Vec<u8>)
    where
        M: state_backend::ManagerReadWrite,
    {
        self.with_backend_mut(|state| {
            assert!(
                state
                    .pvm
                    .provide_input(level, message_counter as u32, &input),
                "Cannot accept input in current state ({})",
                state.pvm.status()
            );
            state.tick.write(state.tick.read() + 1);
            state.message_counter.write(message_counter);
            state.level_is_set.write(true);
            state.level.write(level);
        })
    }

    /// Set reveal data reponse to pvm state
    pub fn set_reveal_response(&mut self, reveal_data: &[u8])
    where
        M: state_backend::ManagerReadWrite,
    {
        self.with_backend_mut(|state| {
            assert!(
                state.pvm.provide_reveal_response(reveal_data),
                "Cannot accept reveal in current state ({})",
                state.pvm.status()
            );
            state.tick.write(state.tick.read() + 1);
        })
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
        Ok(state.with_backend(|state| {
            let struct_ref = state.struct_ref::<state_backend::FnManagerIdent>();
            self.repo.commit_serialised(&struct_ref)
        })?)
    }

    /// Checkout the PVM state committed under `id`, if the commit exists.
    pub fn checkout(&self, id: &Hash) -> Result<NodePvm, PvmStorageError> {
        let allocated: AllocatedOf<StateLayout, Owned> = self.repo.checkout_serialised(id)?;
        let state = State::bind(allocated);
        Ok(NodePvm {
            state: Box::new(state),
        })
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
