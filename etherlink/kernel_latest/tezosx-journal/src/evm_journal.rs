// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use anyhow::anyhow;
use revm::{
    context::{transaction::AccessList, JournalInner},
    JournalEntry,
};

use crate::LayeredState;

#[derive(Debug, PartialEq, Eq)]
pub struct EvmJournal {
    pub layered_state: LayeredState,
    pub inner: JournalInner<JournalEntry>,
    access_list: Option<AccessList>,
}

impl EvmJournal {
    pub fn new() -> Self {
        Self {
            layered_state: LayeredState::new(),
            inner: JournalInner::new(),
            access_list: None,
        }
    }
}

impl EvmJournal {
    pub fn set_access_list(
        &mut self,
        input_list: AccessList,
    ) -> Result<(), anyhow::Error> {
        if self.access_list.is_none() {
            self.access_list = Some(input_list);
            Ok(())
        } else {
            Err(anyhow!("Access list should only be set once per context"))
        }
    }

    pub fn get_access_list_copy(&self) -> AccessList {
        self.access_list.clone().unwrap_or_default()
    }
}

impl Default for EvmJournal {
    fn default() -> Self {
        Self::new()
    }
}
