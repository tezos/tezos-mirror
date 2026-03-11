// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::{EvmJournal, MichelsonJournal};

#[derive(Debug, Default, PartialEq, Eq)]
pub struct TezosXJournal {
    pub evm: EvmJournal,
    pub michelson: MichelsonJournal,
}

impl TezosXJournal {
    pub fn new() -> Self {
        Self::default()
    }
}
