// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use bincode::serialize;
use serde::{Deserialize, Serialize};
use std::fmt;

const DUMMY_STATUS: &str = "riscv_dummy_status";

#[derive(Debug, Serialize, Deserialize, Default, PartialEq)]
pub struct DummyPvm {
    payload: Vec<u8>,
    level: Option<u32>,
    message_counter: u64,
    tick: u64,
}

pub struct Status(String);

impl DummyPvm {
    pub fn empty() -> Self {
        Self::default()
    }

    pub fn get_status(&self) -> Status {
        Status(DUMMY_STATUS.to_string())
    }

    pub fn get_tick(&self) -> u64 {
        self.tick
    }

    pub fn get_current_level(&self) -> Option<u32> {
        self.level
    }

    pub fn get_message_counter(&self) -> u64 {
        self.message_counter
    }

    pub fn install_boot_sector(&self, boot_sector: Vec<u8>) -> Self {
        DummyPvm {
            payload: boot_sector,
            level: self.level,
            message_counter: self.message_counter,
            tick: self.tick,
        }
    }

    pub fn compute_step(&self) -> Self {
        DummyPvm {
            payload: self.payload.clone(),
            level: self.level,
            message_counter: self.message_counter,
            tick: self.tick + 1,
        }
    }

    pub fn compute_step_many(&self, _max_steps: usize) -> (Self, i64) {
        (Self::empty(), 0)
    }

    pub fn hash(&self) -> Vec<u8> {
        let bytes = serialize(&self).unwrap();
        tezos_crypto_rs::blake2b::digest_256(&bytes).unwrap()
    }

    pub fn set_input(&self, level: u32, message_counter: u64, input: Vec<u8>) -> Self {
        DummyPvm {
            payload: input,
            level: Some(level),
            message_counter,
            tick: self.tick + 1,
        }
    }
}

impl fmt::Display for Status {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
