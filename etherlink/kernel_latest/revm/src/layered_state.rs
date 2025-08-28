// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::mem;

use revm::primitives::{Address, U256};

use crate::{
    database::DatabasePrecompileStateChanges, journal::PrecompileStateChanges,
    precompiles::send_outbox_message::Withdrawal, Error,
};

/// This state is created to manage one object because
/// everything that we used here, is stored in one address (Address::ZERO).
/// If we want to store in multiple addresses in the future,
/// we can adapt this to have a collection of this object.
#[derive(Debug, PartialEq, Eq)]
pub struct LayeredState {
    etherlink_data: PrecompileStateChanges,
    entries: Vec<EtherlinkEntry>,
    depths: Vec<usize>,
}

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
    PushWithdrawal,
}

impl Default for LayeredState {
    fn default() -> Self {
        Self::new()
    }
}

impl LayeredState {
    pub fn new() -> Self {
        Self {
            entries: vec![],
            depths: vec![],
            etherlink_data: Default::default(),
        }
    }

    pub fn ticket_balance_add<DB: DatabasePrecompileStateChanges>(
        &mut self,
        ticket_hash: &U256,
        owner: &Address,
        amount: U256,
        db: &DB,
    ) -> Result<(), Error> {
        let key = (*owner, *ticket_hash);
        let ticket_balance = match self.etherlink_data.ticket_balances.get(&key) {
            Some(balance) => *balance,
            None => db.ticket_balance(ticket_hash, owner)?,
        };
        let new_balance = ticket_balance.checked_add(amount).ok_or(Error::Custom(
            "Overflow in ticket balance addition".to_string(),
        ))?;
        self.etherlink_data.ticket_balances.insert(key, new_balance);
        self.entries.push(EtherlinkEntry::TicketBalanceAdd {
            ticket_hash: *ticket_hash,
            owner: *owner,
            amount,
        });
        Ok(())
    }

    pub fn ticket_balance_remove<DB: DatabasePrecompileStateChanges>(
        &mut self,
        ticket_hash: &U256,
        owner: &Address,
        amount: U256,
        db: &DB,
    ) -> Result<(), Error> {
        let key = (*owner, *ticket_hash);
        let ticket_balance = match self.etherlink_data.ticket_balances.get(&key) {
            Some(balance) => *balance,
            None => db.ticket_balance(ticket_hash, owner)?,
        };
        let new_balance = ticket_balance.checked_sub(amount).ok_or(Error::Custom(
            "Underflow in ticket balance removal".to_string(),
        ))?;
        self.etherlink_data.ticket_balances.insert(key, new_balance);
        self.entries.push(EtherlinkEntry::TicketBalanceRemove {
            ticket_hash: *ticket_hash,
            owner: *owner,
            amount,
        });
        Ok(())
    }

    pub fn remove_deposit(
        &mut self,
        deposit_id: &U256,
        db: &impl DatabasePrecompileStateChanges,
    ) -> Result<(), Error> {
        if self.etherlink_data.removed_deposits.contains(deposit_id) {
            return Err(Error::Custom("Deposit already removed".to_string()));
        }
        if db.deposit_in_queue(deposit_id)?.is_none() {
            return Err(Error::Custom("Deposit not found in queue".to_string()));
        }
        self.etherlink_data.removed_deposits.insert(*deposit_id);
        self.entries.push(EtherlinkEntry::RemoveDeposit {
            deposit_id: *deposit_id,
        });
        Ok(())
    }

    pub fn push_withdrawal(&mut self, withdrawal: Withdrawal) {
        self.etherlink_data.withdrawals.push(withdrawal);
        self.entries.push(EtherlinkEntry::PushWithdrawal);
    }

    pub fn is_deposit_removed(&self, deposit_id: &U256) -> bool {
        self.etherlink_data.removed_deposits.contains(deposit_id)
    }

    pub fn checkpoint(&mut self) {
        self.depths.push(self.entries.len());
    }

    pub fn checkpoint_commit(&mut self) {
        self.depths.pop();
    }

    /// Revert the data stored in `etherlink_data` by reverse-applying
    /// all the entries from the last one to the the latest checkpoint.
    pub fn checkpoint_revert(&mut self) {
        let entries = if let Some(depth) = self.depths.pop() {
            self.entries.drain(depth..).collect::<Vec<_>>()
        } else {
            mem::take(&mut self.entries)
        };
        for entry in entries.into_iter().rev() {
            match entry {
                EtherlinkEntry::TicketBalanceAdd {
                    ticket_hash,
                    owner,
                    amount,
                } => {
                    let key = (owner, ticket_hash);
                    // Safe: checked when inserting
                    let ticket_balance =
                        self.etherlink_data.ticket_balances.get(&key).unwrap();
                    // Safe: checked when inserting
                    let previous_balance = ticket_balance - amount;
                    self.etherlink_data
                        .ticket_balances
                        .insert(key, previous_balance);
                }
                EtherlinkEntry::TicketBalanceRemove {
                    ticket_hash,
                    owner,
                    amount,
                } => {
                    let key = (owner, ticket_hash);
                    // Safe: checked when inserting
                    let ticket_balance =
                        self.etherlink_data.ticket_balances.get(&key).unwrap();
                    // Safe: checked when inserting
                    let previous_balance = ticket_balance + amount;
                    self.etherlink_data
                        .ticket_balances
                        .insert(key, previous_balance);
                }
                EtherlinkEntry::RemoveDeposit { deposit_id } => {
                    self.etherlink_data.removed_deposits.remove(&deposit_id);
                }
                EtherlinkEntry::PushWithdrawal => {
                    self.etherlink_data.withdrawals.pop();
                }
            }
        }
    }

    pub fn finalize(&mut self) -> PrecompileStateChanges {
        // Get all withdrawals from the entries.
        self.depths.clear();
        self.entries.clear();
        mem::take(&mut self.etherlink_data)
    }
}
