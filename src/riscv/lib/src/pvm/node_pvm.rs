// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::{
    machine_state::{bus::main_memory::M100M, mode::Mode, DefaultCacheLayouts},
    program::Program,
    pvm::common::{Pvm, PvmHooks, PvmLayout, PvmStatus},
    state_backend::{
        self,
        hash::RootHashable,
        memory_backend::{InMemoryBackend, SliceManager, SliceManagerRO},
        Backend, Layout,
    },
    storage::{self, Hash, Repo},
};
use std::{ops::Bound, path::Path};
use thiserror::Error;

pub type StateLayout = (
    PvmLayout<M100M, DefaultCacheLayouts>,
    state_backend::Atom<bool>,
    state_backend::Atom<u32>,
    state_backend::Atom<u64>,
    state_backend::Atom<u64>,
);

pub struct State<M: state_backend::ManagerBase> {
    pvm: Pvm<M100M, DefaultCacheLayouts, M>,
    level_is_set: state_backend::Cell<bool, M>,
    level: state_backend::Cell<u32, M>,
    message_counter: state_backend::Cell<u64, M>,
    tick: state_backend::Cell<u64, M>,
}

impl<M: state_backend::ManagerBase> State<M> {
    pub fn bind(space: state_backend::AllocatedOf<StateLayout, M>) -> Self {
        Self {
            pvm: Pvm::<M100M, _, M>::bind(space.0),
            level_is_set: space.1,
            level: space.2,
            message_counter: space.3,
            tick: space.4,
        }
    }

    /// Obtain a structure with references to the bound regions of this type.
    pub fn struct_ref(&self) -> state_backend::AllocatedOf<StateLayout, state_backend::Ref<'_, M>> {
        (
            self.pvm.struct_ref(),
            self.level_is_set.struct_ref(),
            self.level.struct_ref(),
            self.message_counter.struct_ref(),
            self.tick.struct_ref(),
        )
    }

    pub fn reset(&mut self)
    where
        M: state_backend::ManagerWrite,
    {
        self.pvm.reset();
        self.level_is_set.write(false);
        self.level.write(0);
        self.message_counter.write(0);
        self.tick.write(0);
    }
}

#[derive(Error, Debug)]
pub enum PvmError {
    #[error("Serialization error: {0}")]
    SerializationError(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NodePvm {
    backend: InMemoryBackend<StateLayout>,
}

impl NodePvm {
    fn with_backend_mut<T, F>(&mut self, f: F) -> T
    where
        F: FnOnce(&mut State<SliceManager>) -> T,
    {
        let placed = <StateLayout as Layout>::placed().into_location();
        let space = self.backend.allocate(placed);
        let mut state = State::bind(space);
        f(&mut state)
    }

    fn with_backend<T, F>(&self, f: F) -> T
    where
        F: FnOnce(&State<SliceManagerRO>) -> T,
    {
        let placed = <StateLayout as Layout>::placed().into_location();
        let space = self.backend.allocate_ro(placed);
        let state = State::bind(space);
        f(&state)
    }

    pub fn empty() -> Self {
        let (mut backend, placed) = InMemoryBackend::<StateLayout>::new();
        let space = backend.allocate(placed);
        let mut state = State::bind(space);
        state.reset();
        Self { backend }
    }

    pub fn get_status(&self) -> PvmStatus {
        self.with_backend(|state| state.pvm.status())
    }

    pub fn get_tick(&self) -> u64 {
        self.with_backend(|state| state.tick.read())
    }

    pub fn get_current_level(&self) -> Option<u32> {
        self.with_backend(|state| {
            if state.level_is_set.read() {
                Some(state.level.read())
            } else {
                None
            }
        })
    }

    pub fn get_message_counter(&self) -> u64 {
        self.with_backend(|state| state.message_counter.read())
    }

    pub fn install_boot_sector(&mut self, loader: &[u8], kernel: &[u8]) {
        self.with_backend_mut(|state| {
            let program =
                Program::<M100M>::from_elf(loader).expect("Could not parse Hermit loader");
            state
                .pvm
                .machine_state
                .setup_boot(&program, Some(kernel), Mode::Supervisor)
                .unwrap()
        })
    }

    pub fn compute_step(&mut self, pvm_hooks: &mut PvmHooks) {
        self.with_backend_mut(|state| {
            state.pvm.eval_one(pvm_hooks);
            state.tick.write(state.tick.read() + 1);
        })
    }

    pub fn compute_step_many(&mut self, pvm_hooks: &mut PvmHooks, max_steps: usize) -> i64 {
        self.with_backend_mut(|state| {
            let steps = state.pvm.eval_max(pvm_hooks, Bound::Included(max_steps));
            state.tick.write(state.tick.read() + steps as u64);
            steps as i64
        })
    }

    pub fn hash(&self) -> Hash {
        self.with_backend(|state| state.struct_ref().hash().unwrap())
    }

    pub fn set_input_message(&mut self, level: u32, message_counter: u64, input: Vec<u8>) {
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

    pub fn set_metadata(&mut self, rollup_address: &[u8; 20], origination_level: u32) {
        self.with_backend_mut(|state| {
            assert!(
                state
                    .pvm
                    .provide_metadata(rollup_address, origination_level),
                "Cannot accept metadata in current state ({})",
                state.pvm.status()
            );
            state.tick.write(state.tick.read() + 1);
        })
    }

    pub fn to_bytes(&self) -> &[u8] {
        self.backend.borrow()
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<Self, PvmError> {
        if bytes.len() != StateLayout::placed().size() {
            Err(PvmError::SerializationError(format!(
                "Unexpected byte buffer size (expected {}, got {})",
                StateLayout::placed().size(),
                bytes.len()
            )))
        } else {
            let mut backend = InMemoryBackend::<StateLayout>::new().0;
            backend.borrow_mut().copy_from_slice(bytes);
            Ok(Self { backend })
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
        Ok(self.repo.commit(state.to_bytes())?)
    }

    /// Checkout the PVM state committed under `id`, if the commit exists.
    pub fn checkout(&self, id: &Hash) -> Result<NodePvm, PvmStorageError> {
        let bytes = self.repo.checkout(id)?;
        let state = NodePvm::from_bytes(&bytes)?;
        Ok(state)
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
