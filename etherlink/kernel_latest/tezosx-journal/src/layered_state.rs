// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use std::mem;

use evm_types::{
    CustomPrecompileError, DatabasePrecompileStateChanges, EtherlinkEntry,
    FaDepositWithProxy, PrecompileStateChanges, SequencerKeyChange,
};
use michelson_types::Withdrawal;
use revm::interpreter::Gas;
use revm::primitives::{Address, U256};

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

    pub fn get_and_increment_global_counter<DB: DatabasePrecompileStateChanges>(
        &mut self,
        db: &DB,
    ) -> Result<U256, CustomPrecompileError> {
        let returned = self
            .etherlink_data
            .global_counter
            .map(Ok)
            .unwrap_or_else(|| db.global_counter())?;
        let counter = returned.checked_add(U256::ONE).ok_or_else(|| {
            CustomPrecompileError::Revert(
                "Global counter overflow".to_string(),
                Gas::new(0),
            )
        })?;
        self.etherlink_data.global_counter = Some(counter);
        self.entries.push(EtherlinkEntry::IncrementGlobalCounter);
        Ok(returned)
    }

    pub fn ticket_balance_add<DB: DatabasePrecompileStateChanges>(
        &mut self,
        ticket_hash: &U256,
        owner: &Address,
        amount: U256,
        db: &DB,
    ) -> Result<(), CustomPrecompileError> {
        let key = (*owner, *ticket_hash);
        let ticket_balance = match self.etherlink_data.ticket_balances.get(&key) {
            Some(balance) => *balance,
            None => db.ticket_balance(ticket_hash, owner)?,
        };
        let new_balance =
            ticket_balance
                .checked_add(amount)
                .ok_or(CustomPrecompileError::Revert(
                    format!(
                "Adding {amount} to {owner} balance failed, ticket hash is {ticket_hash}"
            ),
                    Gas::new(0),
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
    ) -> Result<(), CustomPrecompileError> {
        let key = (*owner, *ticket_hash);
        let ticket_balance = match self.etherlink_data.ticket_balances.get(&key) {
            Some(balance) => *balance,
            None => db.ticket_balance(ticket_hash, owner)?,
        };
        let new_balance =
            ticket_balance
                .checked_sub(amount)
                .ok_or(CustomPrecompileError::Revert(
                    format!(
            "Removing {amount} from {owner} balance failed, ticket hash is {ticket_hash}"
        ),
                    Gas::new(0),
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
    ) -> Result<(), CustomPrecompileError> {
        if self.etherlink_data.removed_deposits.contains(deposit_id) {
            return Err(CustomPrecompileError::Revert(
                "Deposit already removed".to_string(),
                Gas::new(0),
            ));
        }
        db.deposit_in_queue(deposit_id)?;
        self.etherlink_data.removed_deposits.insert(*deposit_id);
        self.entries.push(EtherlinkEntry::RemoveDeposit {
            deposit_id: *deposit_id,
        });
        Ok(())
    }

    // Queue entrypoint is called exclusively and directly by the system address after a transaction is deposited.
    // Because of this, find and remove operations can never interact with queued deposits within the same transaction.
    // This justfies the fact that layered state functions do not take `etherlink_data.deposits` into account.
    // If the first assumption was to change this behavior would need to be updated.
    pub fn queue_deposit(&mut self, deposit: FaDepositWithProxy, deposit_id: U256) {
        self.etherlink_data.deposits.push((deposit_id, deposit));
        self.entries.push(EtherlinkEntry::QueueDeposit);
    }

    pub fn push_withdrawal(&mut self, withdrawal: Withdrawal) {
        self.etherlink_data.withdrawals.push(withdrawal);
        self.entries.push(EtherlinkEntry::PushWithdrawal);
    }

    pub fn store_sequencer_key_change(&mut self, upgrade: SequencerKeyChange) {
        let old_sequencer_key_change = self.etherlink_data.sequencer_key_change.take();
        self.etherlink_data.sequencer_key_change = Some(upgrade);
        self.entries.push(EtherlinkEntry::StoreSequencerKeyChange {
            old_sequencer_key_change,
        });
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
        let start = self.depths.pop().unwrap_or(0);
        for entry in self.entries.drain(start..).rev() {
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
                EtherlinkEntry::QueueDeposit => {
                    self.etherlink_data.deposits.pop();
                }
                EtherlinkEntry::PushWithdrawal => {
                    self.etherlink_data.withdrawals.pop();
                }
                EtherlinkEntry::IncrementGlobalCounter => {
                    if let Some(counter) = self.etherlink_data.global_counter.as_mut() {
                        *counter -= U256::ONE;
                    }
                }
                EtherlinkEntry::StoreSequencerKeyChange {
                    old_sequencer_key_change,
                } => {
                    self.etherlink_data.sequencer_key_change = old_sequencer_key_change;
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

#[cfg(test)]
mod tests {
    use evm_types::{
        custom, CustomPrecompileError, DatabasePrecompileStateChanges,
        FaDepositWithProxy, IntoWithRemainder,
    };
    use revm::interpreter::Gas;
    use revm::primitives::{Address, U256};
    use tezos_crypto_rs::{hash::ContractKt1Hash, public_key::PublicKey};

    use super::LayeredState;

    struct DummyDB;

    impl DatabasePrecompileStateChanges for DummyDB {
        fn log_node_message(&mut self, _: tezos_evm_logging::Level, _: &str) {}

        fn global_counter(&self) -> Result<U256, CustomPrecompileError> {
            Ok(U256::ZERO)
        }

        fn ticket_balance(
            &self,
            _ticket_hash: &U256,
            _owner: &Address,
        ) -> Result<U256, CustomPrecompileError> {
            Ok(U256::ZERO)
        }

        fn deposit_in_queue(
            &self,
            _deposit_id: &U256,
        ) -> Result<FaDepositWithProxy, CustomPrecompileError> {
            Ok(FaDepositWithProxy::default())
        }

        fn ticketer(&self) -> Result<ContractKt1Hash, CustomPrecompileError> {
            ContractKt1Hash::from_base58_check("tz1fp5ncDmqYwYC568fREYz9iwQTgGQuKZqX")
                .map_err(|e| custom(e).into_with_remainder(Gas::new(0)))
        }

        fn sequencer(&self) -> Result<PublicKey, CustomPrecompileError> {
            PublicKey::from_b58check(
                "edpkv4NmL2YPe8eiVGXUDXmPQybD725ofKirTzGRxs1X9UmaG3voKw",
            )
            .map_err(|e| {
                CustomPrecompileError::Revert(
                    format!("Invalid sequencer address: {e}"),
                    Gas::new(0),
                )
            })
        }

        fn governance_sequencer_upgrade_exists(
            &self,
        ) -> Result<bool, CustomPrecompileError> {
            Ok(false)
        }
    }

    #[test]
    fn test_layered_state() {
        let mut layered_state = LayeredState::new();
        let dummy_db = DummyDB;
        let ticket_hash = U256::from(1);
        let owner = Address::ZERO;
        let amount = U256::from(3);
        layered_state.checkpoint();
        layered_state
            .ticket_balance_add(&ticket_hash, &owner, amount, &dummy_db)
            .unwrap();
        layered_state.checkpoint();
        layered_state
            .ticket_balance_add(&ticket_hash, &owner, U256::from(5), &dummy_db)
            .unwrap();
        layered_state.checkpoint_revert();
        let etherlink_data = layered_state.finalize();
        assert_eq!(
            etherlink_data.ticket_balances.get(&(owner, ticket_hash)),
            Some(&amount)
        );
    }
}
