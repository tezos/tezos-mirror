// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use revm::{
    bytecode::Bytecode,
    context::JournalInner,
    context_interface::{
        context::{SStoreResult, SelfDestructResult, StateLoad},
        journaled_state::{AccountLoad, JournalCheckpoint, JournalTr, TransferError},
        Database,
    },
    inspector::JournalExt,
    primitives::{
        hardfork::SpecId, Address, HashMap, HashSet, Log, StorageKey, StorageValue, B256,
        U256,
    },
    state::{Account, EvmState},
    JournalEntry,
};
use std::vec::Vec;

use crate::{
    database::{DatabaseCommitPrecompileStateChanges, DatabasePrecompileStateChanges},
    helpers::legacy::FaDepositWithProxy,
    layered_state::LayeredState,
    precompiles::{error::CustomPrecompileError, send_outbox_message::Withdrawal},
    storage::sequencer_key_change::SequencerKeyChange,
};

type TicketBalanceKey = (Address, U256);

#[derive(Debug, PartialEq, Eq, Default)]
pub struct PrecompileStateChanges {
    pub ticket_balances: HashMap<TicketBalanceKey, U256>,
    pub removed_deposits: HashSet<U256>,
    pub withdrawals: Vec<Withdrawal>,
    pub global_counter: Option<U256>,
    pub sequencer_key_change: Option<SequencerKeyChange>,
}

/// A journal of state changes internal to the EVM
///
/// On each additional call, the depth of the journaled state is increased (`depth`) and a new journal is added.
///
/// The journal contains every state change that happens within that call, making it possible to revert changes made in a specific call.
#[derive(Debug, PartialEq, Eq)]
pub struct Journal<DB> {
    /// Database
    pub database: DB,

    /// Layered state for state changes not managed by REVM
    /// (i.e. induced by Etherlink-specific precompiles)
    pub layered_state: LayeredState,

    /// Inner journal state.
    pub inner: JournalInner<JournalEntry>,
}

/// The implementation is only calling the underline REVM object which is the same as the REVM journal one.
/// The only changes are the invocation of `LayeredDB` methods in some functions.
impl<DB: Database + DatabaseCommitPrecompileStateChanges> JournalTr for Journal<DB> {
    type Database = DB;
    type State = EvmState;

    fn new(database: DB) -> Journal<DB> {
        Self {
            inner: JournalInner::new(),
            layered_state: LayeredState::new(),
            database,
        }
    }

    fn db(&self) -> &Self::Database {
        &self.database
    }

    fn db_mut(&mut self) -> &mut Self::Database {
        &mut self.database
    }

    fn sload(
        &mut self,
        address: Address,
        key: StorageKey,
    ) -> Result<StateLoad<StorageValue>, <Self::Database as Database>::Error> {
        self.inner.sload(&mut self.database, address, key)
    }

    fn sstore(
        &mut self,
        address: Address,
        key: StorageKey,
        value: StorageValue,
    ) -> Result<StateLoad<SStoreResult>, <Self::Database as Database>::Error> {
        self.inner.sstore(&mut self.database, address, key, value)
    }

    fn tload(&mut self, address: Address, key: StorageKey) -> StorageValue {
        self.inner.tload(address, key)
    }

    fn tstore(&mut self, address: Address, key: StorageKey, value: StorageValue) {
        self.inner.tstore(address, key, value)
    }

    fn log(&mut self, log: Log) {
        self.inner.log(log)
    }

    fn selfdestruct(
        &mut self,
        address: Address,
        target: Address,
    ) -> Result<StateLoad<SelfDestructResult>, DB::Error> {
        self.inner.selfdestruct(&mut self.database, address, target)
    }

    fn warm_coinbase_account(&mut self, address: Address) {
        self.inner.warm_addresses.set_coinbase(address);
    }

    fn warm_precompiles(&mut self, precompiles: HashSet<Address>) {
        self.inner
            .warm_addresses
            .set_precompile_addresses(precompiles);
    }

    #[inline]
    fn precompile_addresses(&self) -> &HashSet<Address> {
        self.inner.warm_addresses.precompiles()
    }

    /// Returns call depth.
    #[inline]
    fn depth(&self) -> usize {
        self.inner.depth
    }

    #[inline]
    fn warm_account_and_storage(
        &mut self,
        address: Address,
        storage_keys: impl IntoIterator<Item = StorageKey>,
    ) -> Result<(), <Self::Database as Database>::Error> {
        self.inner.load_account_optional(
            &mut self.database,
            address,
            false,
            storage_keys,
        )?;
        Ok(())
    }

    #[inline]
    fn set_spec_id(&mut self, spec_id: SpecId) {
        self.inner.spec = spec_id;
    }

    #[inline]
    fn transfer(
        &mut self,
        from: Address,
        to: Address,
        balance: U256,
    ) -> Result<Option<TransferError>, DB::Error> {
        self.inner.transfer(&mut self.database, from, to, balance)
    }

    #[inline]
    fn touch_account(&mut self, address: Address) {
        self.inner.touch(address);
    }

    #[inline]
    fn caller_accounting_journal_entry(
        &mut self,
        address: Address,
        old_balance: U256,
        bump_nonce: bool,
    ) {
        self.inner
            .caller_accounting_journal_entry(address, old_balance, bump_nonce);
    }

    /// Increments the balance of the account.
    #[inline]
    fn balance_incr(
        &mut self,
        address: Address,
        balance: U256,
    ) -> Result<(), <Self::Database as Database>::Error> {
        self.inner
            .balance_incr(&mut self.database, address, balance)
    }

    /// Increments the nonce of the account.
    #[inline]
    fn nonce_bump_journal_entry(&mut self, address: Address) {
        self.inner.nonce_bump_journal_entry(address)
    }

    #[inline]
    fn load_account(
        &mut self,
        address: Address,
    ) -> Result<StateLoad<&mut Account>, DB::Error> {
        self.inner.load_account(&mut self.database, address)
    }

    #[inline]
    fn load_account_code(
        &mut self,
        address: Address,
    ) -> Result<StateLoad<&mut Account>, DB::Error> {
        self.inner.load_code(&mut self.database, address)
    }

    #[inline]
    fn load_account_delegated(
        &mut self,
        address: Address,
    ) -> Result<StateLoad<AccountLoad>, DB::Error> {
        self.inner
            .load_account_delegated(&mut self.database, address)
    }

    #[inline]
    fn checkpoint(&mut self) -> JournalCheckpoint {
        self.layered_state.checkpoint();
        self.inner.checkpoint()
    }

    #[inline]
    fn checkpoint_commit(&mut self) {
        self.layered_state.checkpoint_commit();
        self.inner.checkpoint_commit()
    }

    #[inline]
    fn checkpoint_revert(&mut self, checkpoint: JournalCheckpoint) {
        // Following the doc of REVM it's safe to consider that `checkpoint` is always the latest created.
        // https://github.com/bluealloy/revm/blob/a8916288952ca65ead1b0fd7aae20341e396b1c6/crates/context/src/journal/inner.rs#L465
        self.layered_state.checkpoint_revert();
        self.inner.checkpoint_revert(checkpoint)
    }

    #[inline]
    fn set_code_with_hash(&mut self, address: Address, code: Bytecode, hash: B256) {
        self.inner.set_code_with_hash(address, code, hash);
    }

    #[inline]
    fn create_account_checkpoint(
        &mut self,
        caller: Address,
        address: Address,
        balance: U256,
        spec_id: SpecId,
    ) -> Result<JournalCheckpoint, TransferError> {
        self.inner
            .create_account_checkpoint(caller, address, balance, spec_id)
    }

    #[inline]
    fn take_logs(&mut self) -> Vec<Log> {
        self.inner.take_logs()
    }

    #[inline]
    fn commit_tx(&mut self) {
        self.inner.commit_tx()
    }

    #[inline]
    fn discard_tx(&mut self) {
        self.inner.discard_tx();
    }

    /// Clear current journal resetting it to initial state and return changes state.
    #[inline]
    fn finalize(&mut self) -> Self::State {
        self.database.commit(self.layered_state.finalize());
        self.inner.finalize()
    }
}

impl<DB> JournalExt for Journal<DB> {
    #[inline]
    fn logs(&self) -> &[Log] {
        &self.inner.logs
    }

    #[inline]
    fn journal(&self) -> &[JournalEntry] {
        &self.inner.journal
    }

    #[inline]
    fn evm_state(&self) -> &EvmState {
        &self.inner.state
    }

    #[inline]
    fn evm_state_mut(&mut self) -> &mut EvmState {
        &mut self.inner.state
    }
}

impl<DB: DatabasePrecompileStateChanges> Journal<DB> {
    pub fn get_and_increment_global_counter(
        &mut self,
    ) -> Result<U256, CustomPrecompileError> {
        self.layered_state
            .get_and_increment_global_counter(&self.database)
    }

    pub fn ticket_balance_add(
        &mut self,
        ticket_hash: U256,
        owner: Address,
        amount: U256,
    ) -> Result<(), CustomPrecompileError> {
        self.layered_state.ticket_balance_add(
            &ticket_hash,
            &owner,
            amount,
            &self.database,
        )
    }

    pub fn ticket_balance_remove(
        &mut self,
        ticket_hash: U256,
        owner: Address,
        amount: U256,
    ) -> Result<(), CustomPrecompileError> {
        self.layered_state.ticket_balance_remove(
            &ticket_hash,
            &owner,
            amount,
            &self.database,
        )
    }

    pub fn remove_deposit_from_queue(
        &mut self,
        deposit_id: U256,
    ) -> Result<(), CustomPrecompileError> {
        self.layered_state
            .remove_deposit(&deposit_id, &self.database)
    }

    pub fn push_withdrawal(&mut self, withdrawal: Withdrawal) {
        self.layered_state.push_withdrawal(withdrawal)
    }

    pub fn find_deposit_in_queue(
        &self,
        deposit_id: &U256,
    ) -> Result<FaDepositWithProxy, CustomPrecompileError> {
        if self.layered_state.is_deposit_removed(deposit_id) {
            return Err(CustomPrecompileError::Revert(
                "Deposit removed in layered state".to_string(),
            ));
        }
        self.database.deposit_in_queue(deposit_id)
    }

    pub fn store_sequencer_key_change(&mut self, upgrade: SequencerKeyChange) {
        self.layered_state.store_sequencer_key_change(upgrade)
    }

    pub fn log(&mut self, log: Log) {
        self.inner.log(log)
    }
}
