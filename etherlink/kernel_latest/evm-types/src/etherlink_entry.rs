// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm::primitives::{Address, U256};

use crate::SequencerKeyChange;

#[derive(Debug, PartialEq, Eq)]
pub enum EtherlinkEntry {
    TicketBalanceAdd {
        ticket_hash: U256,
        owner: Address,
        amount: U256,
    },
    TicketBalanceRemove {
        ticket_hash: U256,
        owner: Address,
        amount: U256,
    },
    RemoveDeposit {
        deposit_id: U256,
    },
    QueueDeposit,
    PushWithdrawal,
    IncrementGlobalCounter,
    StoreSequencerKeyChange {
        old_sequencer_key_change: Option<SequencerKeyChange>,
    },
}
