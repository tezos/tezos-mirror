// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::storage::Hash;
use bincode::serialize;
use serde::{Deserialize, Serialize};
use std::fmt;

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
