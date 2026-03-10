// SPDX-FileCopyrightText: 2025-2026 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use revm::{
    bytecode::Bytecode,
    context::journaled_state::{
        account::JournaledAccount, AccountInfoLoad, JournalLoadError,
    },
    context_interface::{
        context::{SStoreResult, SelfDestructResult, StateLoad},
        journaled_state::{AccountLoad, JournalCheckpoint, JournalTr, TransferError},
        Database,
    },
    inspector::JournalExt,
    primitives::{
        hardfork::SpecId, Address, AddressMap, AddressSet, HashSet, Log, StorageKey,
        StorageValue, B256, U256,
    },
    state::{Account, EvmState},
    JournalEntry,
};
use std::vec::Vec;

use tezosx_journal::EvmJournal;

use crate::database::EtherlinkVMDB;
use crate::helpers::legacy::alloy_to_u256;
use crate::tezosx::{get_alias, store_alias};
use evm_types::{
    CustomPrecompileError, DatabaseCommitPrecompileStateChanges,
    DatabasePrecompileStateChanges, FaDepositWithProxy, SequencerKeyChange,
};
use michelson_types::Withdrawal;
use tezos_ethereum::wei::mutez_from_wei;
use tezos_evm_logging::Logging;
use tezos_smart_rollup_host::storage::StorageV1;
use tezosx_interfaces::{CrossCallResult, CrossRuntimeContext, Registry, RuntimeId};

/// A journal of state changes internal to the EVM
///
/// On each additional call, the depth of the journaled state is increased (`depth`) and a new journal is added.
///
/// The journal contains every state change that happens within that call, making it possible to revert changes made in a specific call.
#[derive(Debug, PartialEq, Eq)]
pub struct Journal<DB> {
    /// Database
    pub database: DB,

    /// EVM journal combining layered state and inner journal.
    pub evm_journal: EvmJournal,
}

/// The implementation is only calling the underline REVM object which is the same as the REVM journal one.
/// The only changes are the invocation of `LayeredDB` methods in some functions.
impl<DB: Database + DatabaseCommitPrecompileStateChanges> JournalTr for Journal<DB> {
    type Database = DB;
    type State = EvmState;
    type JournaledAccount<'a>
        = JournaledAccount<'a, DB>
    where
        DB: 'a;

    fn new(database: DB) -> Journal<DB> {
        Self {
            evm_journal: EvmJournal::new(),
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
        self.sload_skip_cold_load(address, key, false)
            .map_err(JournalLoadError::unwrap_db_error)
    }

    #[inline]
    fn sload_skip_cold_load(
        &mut self,
        address: Address,
        key: StorageKey,
        skip_cold_load: bool,
    ) -> Result<
        StateLoad<StorageValue>,
        JournalLoadError<<Self::Database as Database>::Error>,
    > {
        self.evm_journal
            .inner
            .sload(&mut self.database, address, key, skip_cold_load)
    }

    fn sstore(
        &mut self,
        address: Address,
        key: StorageKey,
        value: StorageValue,
    ) -> Result<StateLoad<SStoreResult>, <Self::Database as Database>::Error> {
        self.sstore_skip_cold_load(address, key, value, false)
            .map_err(JournalLoadError::unwrap_db_error)
    }

    #[inline]
    fn sstore_skip_cold_load(
        &mut self,
        address: Address,
        key: StorageKey,
        value: StorageValue,
        skip_cold_load: bool,
    ) -> Result<
        StateLoad<SStoreResult>,
        JournalLoadError<<Self::Database as Database>::Error>,
    > {
        self.evm_journal.inner.sstore(
            &mut self.database,
            address,
            key,
            value,
            skip_cold_load,
        )
    }

    fn tload(&mut self, address: Address, key: StorageKey) -> StorageValue {
        self.evm_journal.inner.tload(address, key)
    }

    fn tstore(&mut self, address: Address, key: StorageKey, value: StorageValue) {
        self.evm_journal.inner.tstore(address, key, value)
    }

    fn log(&mut self, log: Log) {
        self.evm_journal.inner.log(log)
    }

    fn selfdestruct(
        &mut self,
        address: Address,
        target: Address,
        skip_cold_load: bool,
    ) -> Result<StateLoad<SelfDestructResult>, JournalLoadError<DB::Error>> {
        self.evm_journal.inner.selfdestruct(
            &mut self.database,
            address,
            target,
            skip_cold_load,
        )
    }

    fn warm_coinbase_account(&mut self, address: Address) {
        self.evm_journal.inner.warm_addresses.set_coinbase(address);
    }

    fn warm_precompiles(&mut self, precompiles: AddressSet) {
        self.evm_journal
            .inner
            .warm_addresses
            .set_precompile_addresses(precompiles);
    }

    #[inline]
    fn precompile_addresses(&self) -> &AddressSet {
        self.evm_journal.inner.warm_addresses.precompiles()
    }

    /// Returns call depth.
    #[inline]
    fn depth(&self) -> usize {
        self.evm_journal.inner.depth
    }

    #[inline]
    fn set_spec_id(&mut self, spec_id: SpecId) {
        self.evm_journal.inner.cfg.spec = spec_id;
    }

    #[inline]
    fn set_eip7708_config(&mut self, disabled: bool, delayed_burn_disabled: bool) {
        self.evm_journal.inner.cfg.eip7708_disabled = disabled;
        self.evm_journal.inner.cfg.eip7708_delayed_burn_disabled = delayed_burn_disabled;
    }

    #[inline]
    fn transfer(
        &mut self,
        from: Address,
        to: Address,
        balance: U256,
    ) -> Result<Option<TransferError>, DB::Error> {
        self.evm_journal
            .inner
            .transfer(&mut self.database, from, to, balance)
    }

    #[inline]
    fn transfer_loaded(
        &mut self,
        from: Address,
        to: Address,
        balance: U256,
    ) -> Option<TransferError> {
        self.evm_journal.inner.transfer_loaded(from, to, balance)
    }

    #[inline]
    fn touch_account(&mut self, address: Address) {
        self.evm_journal.inner.touch(address);
    }

    #[inline]
    fn caller_accounting_journal_entry(
        &mut self,
        address: Address,
        old_balance: U256,
        bump_nonce: bool,
    ) {
        #[allow(deprecated)]
        self.evm_journal.inner.caller_accounting_journal_entry(
            address,
            old_balance,
            bump_nonce,
        );
    }

    /// Increments the balance of the account.
    #[inline]
    fn balance_incr(
        &mut self,
        address: Address,
        balance: U256,
    ) -> Result<(), <Self::Database as Database>::Error> {
        self.evm_journal
            .inner
            .balance_incr(&mut self.database, address, balance)
    }

    /// Increments the nonce of the account.
    #[inline]
    fn nonce_bump_journal_entry(&mut self, address: Address) {
        #[allow(deprecated)]
        self.evm_journal.inner.nonce_bump_journal_entry(address)
    }

    #[inline]
    fn load_account(
        &mut self,
        address: Address,
    ) -> Result<StateLoad<&Account>, DB::Error> {
        self.evm_journal
            .inner
            .load_account(&mut self.database, address)
    }

    #[inline]
    fn load_account_code(
        &mut self,
        address: Address,
    ) -> Result<StateLoad<&Account>, DB::Error> {
        self.evm_journal
            .inner
            .load_code(&mut self.database, address)
    }

    #[inline]
    fn load_account_delegated(
        &mut self,
        address: Address,
    ) -> Result<StateLoad<AccountLoad>, DB::Error> {
        self.evm_journal
            .inner
            .load_account_delegated(&mut self.database, address)
    }

    #[inline]
    fn checkpoint(&mut self) -> JournalCheckpoint {
        self.evm_journal.layered_state.checkpoint();
        self.evm_journal.inner.checkpoint()
    }

    #[inline]
    fn checkpoint_commit(&mut self) {
        self.evm_journal.layered_state.checkpoint_commit();
        self.evm_journal.inner.checkpoint_commit()
    }

    #[inline]
    fn checkpoint_revert(&mut self, checkpoint: JournalCheckpoint) {
        // Following the doc of REVM it's safe to consider that `checkpoint` is always the latest created.
        // https://github.com/bluealloy/revm/blob/a8916288952ca65ead1b0fd7aae20341e396b1c6/crates/context/src/journal/inner.rs#L465
        self.evm_journal.layered_state.checkpoint_revert();
        self.evm_journal.inner.checkpoint_revert(checkpoint)
    }

    #[inline]
    fn set_code_with_hash(&mut self, address: Address, code: Bytecode, hash: B256) {
        self.evm_journal
            .inner
            .set_code_with_hash(address, code, hash);
    }

    #[inline]
    fn create_account_checkpoint(
        &mut self,
        caller: Address,
        address: Address,
        balance: U256,
        spec_id: SpecId,
    ) -> Result<JournalCheckpoint, TransferError> {
        self.evm_journal
            .inner
            .create_account_checkpoint(caller, address, balance, spec_id)
    }

    #[inline]
    fn take_logs(&mut self) -> Vec<Log> {
        self.evm_journal.inner.take_logs()
    }

    #[inline]
    fn commit_tx(&mut self) {
        self.evm_journal.inner.commit_tx()
    }

    #[inline]
    fn discard_tx(&mut self) {
        self.evm_journal.inner.discard_tx();
    }

    /// Clear current journal resetting it to initial state and return changes state.
    #[inline]
    fn finalize(&mut self) -> Self::State {
        self.database
            .commit(self.evm_journal.layered_state.finalize());
        self.evm_journal.inner.finalize()
    }

    fn load_account_info_skip_cold_load(
        &mut self,
        address: Address,
        load_code: bool,
        skip_cold_load: bool,
    ) -> Result<AccountInfoLoad<'_>, JournalLoadError<<Self::Database as Database>::Error>>
    {
        let spec = self.evm_journal.inner.cfg.spec;
        self.evm_journal
            .inner
            .load_account_optional(&mut self.database, address, load_code, skip_cold_load)
            .map(|a| {
                AccountInfoLoad::new(
                    &a.data.info,
                    a.is_cold,
                    a.state_clear_aware_is_empty(spec),
                )
            })
    }

    #[inline]
    fn logs(&self) -> &[Log] {
        &self.evm_journal.inner.logs
    }

    #[inline]
    fn warm_access_list(&mut self, access_list: AddressMap<HashSet<StorageKey>>) {
        self.evm_journal
            .inner
            .warm_addresses
            .set_access_list(access_list)
    }

    #[inline]
    fn load_account_with_code(
        &mut self,
        address: Address,
    ) -> Result<StateLoad<&Account>, <Self::Database as Database>::Error> {
        self.evm_journal
            .inner
            .load_code(&mut self.database, address)
    }

    #[inline]
    fn load_account_mut_skip_cold_load(
        &mut self,
        address: Address,
        skip_cold_load: bool,
    ) -> Result<StateLoad<Self::JournaledAccount<'_>>, <Self::Database as Database>::Error>
    {
        self.evm_journal
            .inner
            .load_account_mut_optional(&mut self.database, address, skip_cold_load)
            .map_err(JournalLoadError::unwrap_db_error)
    }

    #[inline]
    fn load_account_mut_optional_code(
        &mut self,
        address: Address,
        load_code: bool,
    ) -> Result<StateLoad<Self::JournaledAccount<'_>>, <Self::Database as Database>::Error>
    {
        self.evm_journal
            .inner
            .load_account_mut_optional_code(&mut self.database, address, load_code, false)
            .map_err(JournalLoadError::unwrap_db_error)
    }
}

impl<DB> JournalExt for Journal<DB> {
    #[inline]
    fn journal(&self) -> &[JournalEntry] {
        &self.evm_journal.inner.journal
    }

    #[inline]
    fn evm_state(&self) -> &EvmState {
        &self.evm_journal.inner.state
    }

    #[inline]
    fn evm_state_mut(&mut self) -> &mut EvmState {
        &mut self.evm_journal.inner.state
    }
}

impl<DB: DatabasePrecompileStateChanges> Journal<DB> {
    pub fn get_and_increment_global_counter(
        &mut self,
    ) -> Result<U256, CustomPrecompileError> {
        self.evm_journal
            .layered_state
            .get_and_increment_global_counter(&self.database)
    }

    pub fn ticket_balance_add(
        &mut self,
        ticket_hash: U256,
        owner: Address,
        amount: U256,
    ) -> Result<(), CustomPrecompileError> {
        self.evm_journal.layered_state.ticket_balance_add(
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
        self.evm_journal.layered_state.ticket_balance_remove(
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
        self.evm_journal
            .layered_state
            .remove_deposit(&deposit_id, &self.database)
    }

    pub fn queue_deposit(&mut self, deposit: FaDepositWithProxy, deposit_id: U256) {
        self.evm_journal
            .layered_state
            .queue_deposit(deposit, deposit_id)
    }

    pub fn push_withdrawal(&mut self, withdrawal: Withdrawal) {
        self.evm_journal.layered_state.push_withdrawal(withdrawal)
    }

    pub fn find_deposit_in_queue(
        &self,
        deposit_id: &U256,
    ) -> Result<FaDepositWithProxy, CustomPrecompileError> {
        if self
            .evm_journal
            .layered_state
            .is_deposit_removed(deposit_id)
        {
            return Err(CustomPrecompileError::Revert(
                "Deposit removed in layered state".to_string(),
            ));
        }
        self.database.deposit_in_queue(deposit_id)
    }

    pub fn store_sequencer_key_change(&mut self, upgrade: SequencerKeyChange) {
        self.evm_journal
            .layered_state
            .store_sequencer_key_change(upgrade)
    }

    pub fn log(&mut self, log: Log) {
        self.evm_journal.inner.log(log)
    }
}

pub trait CrossRuntimeCall {
    fn tezosx_resolve_source_alias(
        &mut self,
        source: Address,
    ) -> Result<Vec<u8>, CustomPrecompileError>;

    fn tezosx_call_michelson(
        &mut self,
        source: Address,
        destination: &str,
        amount: U256,
        data: &[u8],
    ) -> Result<(), CustomPrecompileError>;
}

impl<Host, R: Registry> CrossRuntimeCall for Journal<EtherlinkVMDB<'_, Host, R>>
where
    Host: StorageV1 + Logging,
{
    fn tezosx_resolve_source_alias(
        &mut self,
        source: Address,
    ) -> Result<Vec<u8>, CustomPrecompileError> {
        let context = CrossRuntimeContext {
            gas_limit: self.database.block.gas_limit,
            timestamp: self.database.block.timestamp,
            block_number: self.database.block.number,
        };
        match get_alias(self.database.host, &source, RuntimeId::Tezos)? {
            Some(alias) => Ok(alias),
            None => {
                let alias = self
                    .database
                    .registry
                    .generate_alias(
                        self.database.host,
                        &source.0 .0,
                        RuntimeId::Tezos,
                        context,
                    )
                    .map_err(|e| {
                        CustomPrecompileError::Revert(format!(
                            "Failed to generate alias for source address: {e:?}"
                        ))
                    })?;
                store_alias(self.database.host, &source, RuntimeId::Tezos, &alias)?;
                Ok(alias)
            }
        }
    }

    fn tezosx_call_michelson(
        &mut self,
        source: Address,
        destination: &str,
        amount: U256,
        data: &[u8],
    ) -> Result<(), CustomPrecompileError> {
        let alias = self.tezosx_resolve_source_alias(source)?;
        let context = CrossRuntimeContext {
            gas_limit: self.database.block.gas_limit,
            timestamp: self.database.block.timestamp,
            block_number: self.database.block.number,
        };
        let destination_contract = self
            .database
            .registry
            .address_from_string(destination, RuntimeId::Tezos)
            .map_err(|e| {
                CustomPrecompileError::Revert(format!(
                    "Failed to get destination contract address from string: {e:?}"
                ))
            })?;
        let result = self
            .database
            .registry
            .bridge(
                self.database.host,
                RuntimeId::Tezos,
                &destination_contract,
                &alias,
                primitive_types::U256::from(
                    mutez_from_wei(alloy_to_u256(&amount)).map_err(|e| {
                        CustomPrecompileError::Revert(format!(
                            "Failed to convert amount from wei to mutez: {e:?}"
                        ))
                    })?,
                ),
                data,
                context,
            )
            .map_err(|e| {
                CustomPrecompileError::Revert(format!("Cross-runtime call failed: {e:?}"))
            })?;
        match result {
            CrossCallResult::Success(_) => Ok(()),
            CrossCallResult::Revert(data) => Err(CustomPrecompileError::Revert(format!(
                "EVM Cross-runtime call reverted: {}",
                hex::encode(&data)
            ))),
            CrossCallResult::Halt(data) => Err(CustomPrecompileError::Revert(format!(
                "EVM Cross-runtime call halted: {}",
                hex::encode(&data)
            ))),
        }
    }
}
