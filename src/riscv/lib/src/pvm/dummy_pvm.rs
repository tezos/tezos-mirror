// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::storage::{self, Hash, Repo};
use bincode::{deserialize, serialize};
use serde::{Deserialize, Serialize};
use std::{fmt, path::Path};
use thiserror::Error;

const DUMMY_STATUS: &str = "riscv_dummy_status";

#[derive(Debug, Serialize, Deserialize, Default, PartialEq)]
pub struct State {
    payload: Vec<u8>,
    level: Option<u32>,
    message_counter: u64,
    tick: u64,
}

impl State {
    fn empty() -> &'static Self {
        const EMPTY_STATE: &State = &State {
            payload: Vec::new(),
            level: None,
            message_counter: 0,
            tick: 0,
        };
        EMPTY_STATE
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum DummyPvm {
    // The purpose of this variant is for the encoding (hence, the hash)
    // of an empty PVM state to remain constant if the `State` type changes.
    // This hash is hardcored in the protocol implementation of the dummy PVM
    // in src/proto_alpha/lib_protocol/sc_rollup_riscv.ml.
    Empty,
    Pvm(State),
}

pub struct Status(String);

impl DummyPvm {
    pub fn empty() -> Self {
        Self::Empty
    }

    fn state(&self) -> &State {
        match self {
            Self::Empty => State::empty(),
            Self::Pvm(state) => state,
        }
    }

    pub fn get_status(&self) -> Status {
        Status(DUMMY_STATUS.to_string())
    }

    pub fn get_tick(&self) -> u64 {
        self.state().tick
    }

    pub fn get_current_level(&self) -> Option<u32> {
        self.state().level
    }

    pub fn get_message_counter(&self) -> u64 {
        self.state().message_counter
    }

    pub fn install_boot_sector(&self, boot_sector: Vec<u8>) -> Self {
        let state = self.state();
        Self::Pvm(State {
            payload: boot_sector,
            level: state.level,
            message_counter: state.message_counter,
            tick: state.tick,
        })
    }

    pub fn compute_step(&self) -> Self {
        let state = self.state();
        Self::Pvm(State {
            payload: state.payload.clone(),
            level: state.level,
            message_counter: state.message_counter,
            tick: state.tick + 1,
        })
    }

    pub fn compute_step_many(&self, _max_steps: usize) -> (Self, i64) {
        (Self::Empty, 0)
    }

    pub fn hash(&self) -> Hash {
        let bytes = serialize(&self).unwrap();
        tezos_crypto_rs::blake2b::digest_256(&bytes)
            .unwrap()
            .try_into()
            .unwrap()
    }

    pub fn set_input(&self, level: u32, message_counter: u64, input: Vec<u8>) -> Self {
        Self::Pvm(State {
            payload: input,
            level: Some(level),
            message_counter,
            tick: self.get_tick() + 1,
        })
    }
}

impl fmt::Display for Status {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Error, Debug)]
pub enum PvmStorageError {
    #[error("Storage error: {0}")]
    StorageError(#[from] storage::StorageError),

    #[error("Serialization error: {0}")]
    SerializationError(#[from] bincode::Error),
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
    pub fn commit(&mut self, state: &DummyPvm) -> Result<Hash, PvmStorageError> {
        let bytes = serialize(state)?;
        Ok(self.repo.commit(&bytes)?)
    }

    /// Checkout the PVM state committed under `id`, if the commit exists.
    pub fn checkout(&self, id: &Hash) -> Result<DummyPvm, PvmStorageError> {
        let bytes = self.repo.checkout(id)?;
        let state: DummyPvm = deserialize(&bytes)?;
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
