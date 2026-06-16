// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use alloy_primitives::map::{HashMap, HashSet};
use alloy_primitives::{Address, U256};
use michelson_types::Withdrawal;

use crate::{FaDepositWithProxy, SequencerKeyChange};

pub type TicketBalanceKey = (Address, U256);

#[derive(Debug, PartialEq, Eq, Default)]
pub struct PrecompileStateChanges {
    pub ticket_balances: HashMap<TicketBalanceKey, U256>,
    pub removed_deposits: HashSet<U256>,
    pub deposits: Vec<(U256, FaDepositWithProxy)>,
    pub withdrawals: Vec<Withdrawal>,
    pub global_counter: Option<U256>,
    pub sequencer_key_change: Option<SequencerKeyChange>,
}
