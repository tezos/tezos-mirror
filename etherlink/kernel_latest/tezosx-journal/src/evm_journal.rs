// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm::{context::JournalInner, JournalEntry};

use crate::LayeredState;

#[derive(Debug, PartialEq, Eq)]
pub struct EvmJournal {
    pub layered_state: LayeredState,
    pub inner: JournalInner<JournalEntry>,
}

impl EvmJournal {
    pub fn new() -> Self {
        Self {
            layered_state: LayeredState::new(),
            inner: JournalInner::new(),
        }
    }
}

impl Default for EvmJournal {
    fn default() -> Self {
        Self::new()
    }
}
