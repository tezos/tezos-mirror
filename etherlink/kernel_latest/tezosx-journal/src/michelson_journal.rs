// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

#[derive(Debug, Default, PartialEq, Eq)]
pub struct MichelsonJournal {
    pub depth_level: usize,
}

impl MichelsonJournal {
    pub fn new() -> Self {
        Self { depth_level: 0 }
    }
}
