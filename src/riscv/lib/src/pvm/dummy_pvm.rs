// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, Default, PartialEq)]
pub struct DummyPvm {
    payload: Vec<u8>,
    level: Option<u64>,
    message_counter: u64,
    tick: u64,
}

impl DummyPvm {
    pub fn empty() -> Self {
        Self::default()
    }
}
