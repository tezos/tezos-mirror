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
    interpreter::Gas,
    primitives::{
        hardfork::SpecId, Address, AddressMap, AddressSet, HashSet, Log, StorageKey,
        StorageValue, B256, U256,
    },
    state::{Account, EvmState},
    DatabaseCommit, JournalEntry,
};
use std::vec::Vec;

use tezosx_journal::{LayeredStateError, TezosXJournal};

use crate::database::EtherlinkVMDB;
use crate::storage::world_state_handler::StorageAccount;
use crate::tezosx::{get_alias, store_alias};
use evm_types::{
    CustomPrecompileError, DatabaseCommitPrecompileStateChanges,
    DatabasePrecompileStateChanges, Error, FaDepositWithProxy, IntoWithRemainder,
    SequencerKeyChange,
};
use michelson_types::Withdrawal;
use tezos_crypto_rs::{blake2b, hash::ContractKt1Hash};
use tezos_ethereum::block::BlockConstants;
use tezos_smart_rollup_host::runtime::RuntimeError;
use tezos_smart_rollup_host::storage::StorageV1;
use tezosx_interfaces::{
    resolve_routing, AliasInfo, CrossRuntimeContext, Registry, RoutingDecision, RuntimeId,
};

/// A journal of state changes internal to the EVM
///
/// On each additional call, the depth of the journaled state is increased (`depth`) and a new journal is added.
///
/// The journal contains every state change that happens within that call, making it possible to revert changes made in a specific call.
pub struct Journal<'a, Host: StorageV1, R: Registry> {
    /// Database
    pub database: EtherlinkVMDB<'a, Host, R>,

    /// TezosX journal combining EVM and Michelson journal state.
    pub journal: &'a mut TezosXJournal,

    /// Deferred error from michelson journal checkpoint operations.
    /// The `JournalTr` trait methods `checkpoint_commit` and `checkpoint_revert`
    /// cannot return errors, so we store them here for later retrieval.
    deferred_error: Option<RuntimeError>,
}

impl<'a, Host: StorageV1, R: Registry> Journal<'a, Host, R> {
    pub fn new_with_inner(
        database: EtherlinkVMDB<'a, Host, R>,
        journal: &'a mut TezosXJournal,
    ) -> Self {
        Self {
            database,
            journal,
            deferred_error: None,
        }
    }

    /// Take any deferred error from michelson journal checkpoint operations.
    pub fn take_deferred_error(&mut self) -> Option<RuntimeError> {
        self.deferred_error.take()
    }
}

/// The implementation is only calling the underline REVM object which is the same as the REVM journal one.
/// The only changes are the invocation of `LayeredDB` methods in some functions.
impl<'a, Host: StorageV1, R: Registry> JournalTr for Journal<'a, Host, R> {
    type Database = EtherlinkVMDB<'a, Host, R>;
    type State = EvmState;
    type JournaledAccount<'b>
        = JournaledAccount<'b, EtherlinkVMDB<'a, Host, R>>
    where
        EtherlinkVMDB<'a, Host, R>: 'b,
        'a: 'b;

    fn new(_database: EtherlinkVMDB<'a, Host, R>) -> Journal<'a, Host, R> {
        unimplemented!("Use Journal::new_with_inner instead")
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
        self.journal
            .evm
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
        self.journal.evm.inner.sstore(
            &mut self.database,
            address,
            key,
            value,
            skip_cold_load,
        )
    }

    fn tload(&mut self, address: Address, key: StorageKey) -> StorageValue {
        self.journal.evm.inner.tload(address, key)
    }

    fn tstore(&mut self, address: Address, key: StorageKey, value: StorageValue) {
        self.journal.evm.inner.tstore(address, key, value)
    }

    fn log(&mut self, log: Log) {
        self.journal.evm.inner.log(log)
    }

    fn selfdestruct(
        &mut self,
        address: Address,
        target: Address,
        skip_cold_load: bool,
    ) -> Result<
        StateLoad<SelfDestructResult>,
        JournalLoadError<<Self::Database as Database>::Error>,
    > {
        self.journal.evm.inner.selfdestruct(
            &mut self.database,
            address,
            target,
            skip_cold_load,
        )
    }

    fn warm_coinbase_account(&mut self, address: Address) {
        self.journal.evm.inner.warm_addresses.set_coinbase(address);
    }

    fn warm_precompiles(&mut self, precompiles: AddressSet) {
        self.journal
            .evm
            .inner
            .warm_addresses
            .set_precompile_addresses(precompiles);
    }

    #[inline]
    fn precompile_addresses(&self) -> &AddressSet {
        self.journal.evm.inner.warm_addresses.precompiles()
    }

    /// Returns call depth.
    #[inline]
    fn depth(&self) -> usize {
        self.journal.evm.inner.depth
    }

    #[inline]
    fn set_spec_id(&mut self, spec_id: SpecId) {
        self.journal.evm.inner.cfg.spec = spec_id;
    }

    #[inline]
    fn set_eip7708_config(&mut self, disabled: bool, delayed_burn_disabled: bool) {
        self.journal.evm.inner.cfg.eip7708_disabled = disabled;
        self.journal.evm.inner.cfg.eip7708_delayed_burn_disabled = delayed_burn_disabled;
    }

    #[inline]
    fn transfer(
        &mut self,
        from: Address,
        to: Address,
        balance: U256,
    ) -> Result<Option<TransferError>, <Self::Database as Database>::Error> {
        self.journal
            .evm
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
        self.journal.evm.inner.transfer_loaded(from, to, balance)
    }

    #[inline]
    fn touch_account(&mut self, address: Address) {
        self.journal.evm.inner.touch(address);
    }

    #[inline]
    fn caller_accounting_journal_entry(
        &mut self,
        address: Address,
        old_balance: U256,
        bump_nonce: bool,
    ) {
        #[allow(deprecated)]
        self.journal.evm.inner.caller_accounting_journal_entry(
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
        self.journal
            .evm
            .inner
            .balance_incr(&mut self.database, address, balance)
    }

    /// Increments the nonce of the account.
    #[inline]
    fn nonce_bump_journal_entry(&mut self, address: Address) {
        #[allow(deprecated)]
        self.journal.evm.inner.nonce_bump_journal_entry(address)
    }

    #[inline]
    fn load_account(
        &mut self,
        address: Address,
    ) -> Result<StateLoad<&Account>, <Self::Database as Database>::Error> {
        self.journal
            .evm
            .inner
            .load_account(&mut self.database, address)
    }

    #[inline]
    fn load_account_code(
        &mut self,
        address: Address,
    ) -> Result<StateLoad<&Account>, <Self::Database as Database>::Error> {
        self.journal
            .evm
            .inner
            .load_code(&mut self.database, address)
    }

    #[inline]
    fn load_account_delegated(
        &mut self,
        address: Address,
    ) -> Result<StateLoad<AccountLoad>, <Self::Database as Database>::Error> {
        self.journal
            .evm
            .inner
            .load_account_delegated(&mut self.database, address)
    }

    #[inline]
    fn checkpoint(&mut self) -> JournalCheckpoint {
        self.journal.michelson.push_external_checkpoint();
        self.journal.evm.layered_state.checkpoint();
        self.journal.evm.inner.checkpoint()
    }

    #[inline]
    fn checkpoint_commit(&mut self) {
        self.journal.evm.layered_state.checkpoint_commit();
        self.journal.evm.inner.checkpoint_commit();
        if let Err(e) = self.journal.michelson.commit_frame(self.database.host) {
            self.deferred_error.get_or_insert(e);
        }
    }

    #[inline]
    fn checkpoint_revert(&mut self, checkpoint: JournalCheckpoint) {
        self.journal.evm.layered_state.checkpoint_revert();
        self.journal.evm.inner.checkpoint_revert(checkpoint);
        // The Michelson journal recovers each snapshot's revert target
        // from the snapshot itself, so no path is supplied here. CRACs
        // may snapshot subtrees other than `/evm/world_state` (e.g.
        // `/tez/tez_accounts` for the Michelson), and hard-coding
        // `/evm/world_state` here would store_move a Tezlink snapshot onto
        // the EVM world state and wipe out kernel-managed paths like
        // `/evm/world_state/fees/da_fee_per_byte`.
        if let Err(e) = self.journal.michelson.revert_frame(self.database.host) {
            self.deferred_error.get_or_insert(e);
        }
    }

    #[inline]
    fn set_code_with_hash(&mut self, address: Address, code: Bytecode, hash: B256) {
        self.journal
            .evm
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
        self.journal
            .evm
            .inner
            .create_account_checkpoint(caller, address, balance, spec_id)
    }

    #[inline]
    fn take_logs(&mut self) -> Vec<Log> {
        // Clone instead of drain: inner CRAC EVM executions share the
        // JournalInner with the outer EVM transaction. Draining would
        // invalidate checkpoint.log_i indices held by outer CALL frames.
        // The logs are properly drained by finalize() at the top level.
        self.journal.evm.inner.logs.clone()
    }

    #[inline]
    fn commit_tx(&mut self) {
        // Noop: CRAC sub-calls reuse the outer EVM transaction JournalInner.
        // All the transaction state (logs, journal entries, depth, transient
        // storage, warm addresses, selfdestructed addresses, transaction_id)
        // belongs to the outer frame and must not be reset.
        // Full cleanup happens in finalize() for top-level UserInput transactions.
    }

    #[inline]
    fn discard_tx(&mut self) {
        self.journal.evm.inner.discard_tx();
    }

    /// Clear current journal resetting it to initial state and return changes state.
    #[inline]
    fn finalize(&mut self) -> Self::State {
        DatabaseCommitPrecompileStateChanges::commit(
            &mut self.database,
            self.journal.evm.layered_state.finalize(),
        );
        self.journal.evm.inner.finalize()
    }

    fn load_account_info_skip_cold_load(
        &mut self,
        address: Address,
        load_code: bool,
        skip_cold_load: bool,
    ) -> Result<AccountInfoLoad<'_>, JournalLoadError<<Self::Database as Database>::Error>>
    {
        let spec = self.journal.evm.inner.cfg.spec;
        self.journal
            .evm
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
        &self.journal.evm.inner.logs
    }

    #[inline]
    fn warm_access_list(&mut self, access_list: AddressMap<HashSet<StorageKey>>) {
        self.journal
            .evm
            .inner
            .warm_addresses
            .set_access_list(access_list)
    }

    #[inline]
    fn load_account_with_code(
        &mut self,
        address: Address,
    ) -> Result<StateLoad<&Account>, <Self::Database as Database>::Error> {
        self.journal
            .evm
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
        self.journal
            .evm
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
        self.journal
            .evm
            .inner
            .load_account_mut_optional_code(&mut self.database, address, load_code, false)
            .map_err(JournalLoadError::unwrap_db_error)
    }
}

impl<Host: StorageV1, R: Registry> JournalExt for Journal<'_, Host, R> {
    #[inline]
    fn journal(&self) -> &[JournalEntry] {
        &self.journal.evm.inner.journal
    }

    #[inline]
    fn evm_state(&self) -> &EvmState {
        &self.journal.evm.inner.state
    }

    #[inline]
    fn evm_state_mut(&mut self) -> &mut EvmState {
        &mut self.journal.evm.inner.state
    }
}

impl<Host: StorageV1, R: Registry> Journal<'_, Host, R> {
    pub fn get_and_increment_global_counter(
        &mut self,
    ) -> Result<U256, LayeredStateError> {
        self.journal
            .evm
            .layered_state
            .get_and_increment_global_counter(&self.database)
    }

    pub fn ticket_balance_add(
        &mut self,
        ticket_hash: U256,
        owner: Address,
        amount: U256,
    ) -> Result<(), LayeredStateError> {
        self.journal.evm.layered_state.ticket_balance_add(
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
    ) -> Result<(), LayeredStateError> {
        self.journal.evm.layered_state.ticket_balance_remove(
            &ticket_hash,
            &owner,
            amount,
            &self.database,
        )
    }

    pub fn remove_deposit_from_queue(
        &mut self,
        deposit_id: U256,
    ) -> Result<(), LayeredStateError> {
        self.journal
            .evm
            .layered_state
            .remove_deposit(&deposit_id, &self.database)
    }

    pub fn queue_deposit(&mut self, deposit: FaDepositWithProxy, deposit_id: U256) {
        self.journal
            .evm
            .layered_state
            .queue_deposit(deposit, deposit_id)
    }

    pub fn push_withdrawal(&mut self, withdrawal: Withdrawal) {
        self.journal.evm.layered_state.push_withdrawal(withdrawal)
    }

    pub fn find_deposit_in_queue(
        &self,
        deposit_id: &U256,
    ) -> Result<FaDepositWithProxy, LayeredStateError> {
        if self
            .journal
            .evm
            .layered_state
            .is_deposit_removed(deposit_id)
        {
            return Err(LayeredStateError::DepositAlreadyRemoved);
        }
        Ok(self.database.deposit_in_queue(deposit_id)?)
    }

    pub fn store_sequencer_key_change(&mut self, upgrade: SequencerKeyChange) {
        self.journal
            .evm
            .layered_state
            .store_sequencer_key_change(upgrade)
    }

    pub fn log(&mut self, log: Log) {
        self.journal.evm.inner.log(log)
    }
}

/// Trait for cross-runtime call support on the journal.
///
/// This is implemented for Journal backed by EtherlinkVMDB, which has access
/// to the host, registry, and block constants needed for cross-runtime calls.
pub trait CrossRuntimeCall {
    /// Resolve the alias for an EVM address in `target_runtime`.
    /// `remaining_evm_gas` is the caller's remaining gas budget (used to
    /// cap alias generation).
    ///
    /// Returns `(alias, generation_gas_consumed)` where `generation_gas_consumed`
    /// is in `target_runtime` units (milligas for Tezos, EVM gas for Ethereum),
    /// 0 on cache hit.
    fn tezosx_resolve_source_alias(
        &mut self,
        source: Address,
        target_runtime: RuntimeId,
        remaining_evm_gas: u64,
    ) -> Result<(String, u64), CustomPrecompileError>;

    /// Read-only variant of [`Self::tezosx_resolve_source_alias`]: on
    /// cache hit returns the persisted alias for `target_runtime`, on
    /// cache miss computes the deterministic alias from the EVM
    /// address without writing anything to storage.
    ///
    /// The deterministic alias is the same value
    /// [`Self::tezosx_resolve_source_alias`] would persist — by design
    /// of [`Registry::ensure_alias`]:
    /// - `RuntimeId::Tezos` → `blake2b-160(lowercase_hex(address))`
    ///   formatted as a `KT1...` base58check string;
    /// - `RuntimeId::Ethereum` → `keccak256(lowercase_hex(address))[..20]`
    ///   formatted as a `0x...` hex string.
    ///
    /// Callers can rely on the read-only path producing the canonical
    /// alias for the target runtime even when no state-mutating call
    /// has ever seen this address before.
    ///
    /// This variant is safe to call from a STATICCALL context, where
    /// any storage write would revert the enclosing frame. It is used
    /// by read-only precompile entries such as `callMichelsonView`.
    fn tezosx_resolve_source_alias_readonly(
        &self,
        source: Address,
        target_runtime: RuntimeId,
        remaining_evm_gas: u64,
    ) -> Result<String, CustomPrecompileError>;

    fn tezosx_call_http(
        &mut self,
        http_request: http::Request<Vec<u8>>,
    ) -> http::Response<Vec<u8>>;

    /// Get the CRAC-ID for the current transaction.
    fn crac_id(&self) -> String;

    /// Store the original EVM source address (E_0) on the first
    /// outgoing gateway call. Subsequent calls are no-ops.
    fn set_original_evm_source(&mut self, source: Address);

    /// Retrieve the original EVM source address, if previously stored.
    fn original_evm_source(&self) -> Option<Address>;
}

impl<'a, Host, R: Registry> CrossRuntimeCall for Journal<'a, Host, R>
where
    Host: StorageV1,
{
    fn tezosx_resolve_source_alias(
        &mut self,
        source: Address,
        target_runtime: RuntimeId,
        remaining_evm_gas: u64,
    ) -> Result<(String, u64), CustomPrecompileError> {
        let context = CrossRuntimeContext {
            gas_limit: self.database.block.gas_limit,
            timestamp: self.database.block.timestamp,
            block_number: self.database.block.number,
        };
        let remaining = Gas::new(remaining_evm_gas);
        let origin = StorageAccount::from_address(&source)
            .and_then(|a| a.get_origin(self.database.host))
            .map_err(|e| e.into_with_remainder(remaining))?;
        let alias_info = match resolve_routing(origin, target_runtime)
            .map_err(|e| Error::from(e).into_with_remainder(remaining))?
        {
            RoutingDecision::RoundTrip(target) => return Ok((target, 0)),
            RoutingDecision::Transitive(info) => info,
            RoutingDecision::Native => AliasInfo {
                runtime: RuntimeId::Ethereum,
                native_address: source.to_string().to_lowercase().into_bytes(),
            },
        };
        if let Some(alias) = get_alias(self.database.host, &source, target_runtime)
            .map_err(|e| e.into_with_remainder(remaining))?
        {
            return Ok((alias, 0));
        }
        // Convert remaining EVM gas to target runtime units to cap
        // the generation cost.
        let target_gas_budget = tezosx_interfaces::gas::convert(
            RuntimeId::Ethereum,
            target_runtime,
            remaining_evm_gas,
        )
        .ok_or_else(|| {
            CustomPrecompileError::Revert(
                "alias generation: EVM gas overflows target runtime units".into(),
                remaining,
            )
        })?;
        let (alias_str, target_gas_remaining) = self
            .database
            .registry
            .ensure_alias(
                self.database.host,
                self.journal,
                alias_info,
                None,
                target_runtime,
                context,
                target_gas_budget,
            )
            .map_err(|e| {
                CustomPrecompileError::Revert(
                    format!("Failed to generate alias for source address: {e:?}"),
                    remaining,
                )
            })?;
        store_alias(self.database.host, &source, target_runtime, &alias_str)
            .map_err(|e| e.into_with_remainder(remaining))?;
        let consumed_target = target_gas_budget - target_gas_remaining;
        Ok((alias_str, consumed_target))
    }

    fn tezosx_resolve_source_alias_readonly(
        &self,
        source: Address,
        target_runtime: RuntimeId,
        remaining_evm_gas: u64,
    ) -> Result<String, CustomPrecompileError> {
        let remaining = Gas::new(remaining_evm_gas);
        let origin = StorageAccount::from_address(&source)
            .and_then(|a| a.get_origin(self.database.host))
            .map_err(|e| e.into_with_remainder(remaining))?;
        let native_bytes = match resolve_routing(origin, target_runtime)
            .map_err(|e| Error::from(e).into_with_remainder(remaining))?
        {
            RoutingDecision::RoundTrip(target) => return Ok(target),
            RoutingDecision::Transitive(info) => info.native_address,
            RoutingDecision::Native => source.to_string().to_lowercase().into_bytes(),
        };
        if let Some(alias) = get_alias(self.database.host, &source, target_runtime)
            .map_err(|e| e.into_with_remainder(remaining))?
        {
            return Ok(alias);
        }
        // Deterministic fallback: reproduce the alias that the target
        // runtime's `ensure_alias` would compute (and persist on the
        // state-mutating path), but skip the storage writes.
        //
        // Each branch duplicates the deterministic name-derivation
        // step from the corresponding runtime's `ensure_alias` — the
        // canonical implementation — and must stay in sync with it.
        // If either formula changes, this read-only path must change
        // too.
        match target_runtime {
            RuntimeId::Tezos => {
                let kt1 = ContractKt1Hash::from(blake2b::digest_160(&native_bytes));
                Ok(kt1.to_base58_check())
            }
            RuntimeId::Ethereum => {
                let hash = revm::primitives::keccak256(&native_bytes);
                Ok(Address::from_slice(&hash.0[..20]).to_string())
            }
        }
    }

    fn tezosx_call_http(
        &mut self,
        http_request: http::Request<Vec<u8>>,
    ) -> http::Response<Vec<u8>> {
        self.database
            .registry
            .serve(self.database.host, self.journal, http_request)
    }

    fn crac_id(&self) -> String {
        self.journal.crac_id().to_string()
    }

    fn set_original_evm_source(&mut self, source: Address) {
        self.journal.evm.set_original_evm_source(source);
    }

    fn original_evm_source(&self) -> Option<Address> {
        self.journal.evm.original_evm_source()
    }
}

pub fn commit_evm_journal_from_external<Host>(
    host: &mut Host,
    registry: &impl Registry,
    block_constants: &BlockConstants,
    journal: &mut TezosXJournal,
) -> Result<Vec<Withdrawal>, Error>
where
    Host: StorageV1,
{
    let db = EtherlinkVMDB::new(host, registry, block_constants)?;
    let mut journal = Journal::new_with_inner(db, journal);
    let state = journal.finalize();
    DatabaseCommit::commit(journal.db_mut(), state);
    Ok(journal.db_mut().take_withdrawals())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezosx_interfaces::{AliasInfo, Origin};

    fn alias_origin(runtime: RuntimeId, native: &[u8]) -> Origin {
        Origin::Alias(AliasInfo {
            runtime,
            native_address: native.to_vec(),
        })
    }

    #[test]
    fn routing_returns_round_trip_for_matching_alias() {
        let mut host = MockKernelHost::default();
        let source = Address::from_slice(&[0xaa; 20]);
        let mut account = StorageAccount::from_address(&source).unwrap();
        account
            .set_origin(&mut host, &alias_origin(RuntimeId::Tezos, b"tz1abcdef"))
            .unwrap();

        let origin = StorageAccount::from_address(&source)
            .unwrap()
            .get_origin(&host)
            .unwrap();
        match resolve_routing(origin, RuntimeId::Tezos).unwrap() {
            RoutingDecision::RoundTrip(target) => assert_eq!(target, "tz1abcdef"),
            other => panic!(
                "expected RoundTrip, got {:?}",
                std::mem::discriminant(&other)
            ),
        }
    }

    #[test]
    fn routing_returns_native_for_native_origin() {
        let mut host = MockKernelHost::default();
        let source = Address::from_slice(&[0xcc; 20]);
        let mut account = StorageAccount::from_address(&source).unwrap();
        account.set_origin(&mut host, &Origin::Native).unwrap();

        let origin = account.get_origin(&host).unwrap();
        assert!(matches!(
            resolve_routing(origin, RuntimeId::Tezos).unwrap(),
            RoutingDecision::Native,
        ));
    }

    #[test]
    fn routing_returns_native_for_unrecorded_source() {
        let host = MockKernelHost::default();
        let source = Address::from_slice(&[0xdd; 20]);

        let origin = StorageAccount::from_address(&source)
            .unwrap()
            .get_origin(&host)
            .unwrap();
        assert!(matches!(
            resolve_routing(origin, RuntimeId::Tezos).unwrap(),
            RoutingDecision::Native,
        ));
    }

    #[test]
    fn routing_returns_transitive_for_mismatched_runtime() {
        // The recorded info is the basis for derivation toward a third target.
        // Unreachable in two runtime mode.
        let mut host = MockKernelHost::default();
        let source = Address::from_slice(&[0xbb; 20]);
        let mut account = StorageAccount::from_address(&source).unwrap();
        account
            .set_origin(&mut host, &alias_origin(RuntimeId::Tezos, b"tz1abcdef"))
            .unwrap();

        let origin = account.get_origin(&host).unwrap();
        match resolve_routing(origin, RuntimeId::Ethereum).unwrap() {
            RoutingDecision::Transitive(info) => {
                assert_eq!(info.runtime, RuntimeId::Tezos);
                assert_eq!(info.native_address, b"tz1abcdef".to_vec());
            }
            other => panic!(
                "expected Transitive, got {:?}",
                std::mem::discriminant(&other)
            ),
        }
    }
}
