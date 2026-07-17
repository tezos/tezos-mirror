// SPDX-FileCopyrightText: 2025-2026 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::error::EvmDbError;
use crate::helpers::storage::read_u256_le_default;
use crate::inspectors::HasHost;
use crate::storage::{
    block::{get_block_hash, BLOCKS_STORED},
    code::CodeStorage,
    sequencer_key_change::{store_sequencer_key_change, write_sequencer_change_counter},
    world_state_handler::{
        AccountInfo, AccountOrigin, StorageAccount, GOVERNANCE_SEQUENCER_UPGRADE_PATH,
        KT1_B58_SIZE, NATIVE_TOKEN_TICKETER_PATH, SEQUENCER_KEY_CHANGE_COUNTER_PATH,
        SEQUENCER_KEY_PATH,
    },
};
use evm_types::{
    DatabaseCommitPrecompileStateChanges, DatabasePrecompileStateChanges,
    FaDepositWithProxy, PrecompileStateChanges, PrecompileStateError,
};
use michelson_types::Withdrawal;
use revm::{
    primitives::{
        Address, AddressMap, HashMap, StorageKey, StorageValue, B256, KECCAK_EMPTY, U256,
    },
    state::{
        Account, AccountInfo as RevmAccountInfo, Bytecode, EvmStorage, EvmStorageSlot,
    },
    Database, DatabaseCommit,
};
use tezos_crypto_rs::{
    hash::{ContractKt1Hash, HashTrait},
    public_key::PublicKey,
};
use tezos_ethereum::block::BlockConstants;
use tezos_evm_logging::{log, tracing::instrument, Level};
use tezos_smart_rollup_host::{runtime::RuntimeError, storage::StorageV1};
use tezosx_interfaces::{AliasInfo, Origin, Registry};

pub struct EtherlinkVMDB<'a, Host, R> {
    pub registry: &'a R,
    /// Runtime host
    pub host: &'a mut Host,
    /// Constants for the current block
    pub(crate) block: &'a BlockConstants,
    /// Commit guard, the `DatabaseCommit` trait and in particular
    /// its `commit` function does NOT return errors.
    /// We need this guard to change if there's an unrecoverable
    /// error and we need to revert the changes made to the durable
    /// storage.
    commit_status: bool,
    /// Withdrawals accumulated by the current execution and consumed at the end of it
    withdrawals: Vec<Withdrawal>,
    /// Storage access to address zero aka the system address
    system: StorageAccount,
    /// Account info snapshot (origin included) taken when read, to
    /// avoid re-writing it if it hasn't changed and only the storage
    /// has been touched, and to thread the existing classification
    /// through the commit write.
    original_account_infos: HashMap<Address, AccountInfo>,
    /// Caller to classify as Native at commit when still unclassified.
    /// Set for user-input (natively signed) transactions; the caller's
    /// info record is rewritten anyway (nonce bump), so the
    /// classification write is free.
    classify_native: Option<Address>,
    /// Alias classifications staged by the journal and flushed by the
    /// precompile-state commit; the account commit consumes them when
    /// writing the info record, and the leftovers (alias staged for an
    /// account untouched by the EVM run) are written at the end of the
    /// commit.
    staged_alias_origins: HashMap<Address, AliasInfo>,
}

impl<'a, Host, R> HasHost for EtherlinkVMDB<'a, Host, R> {
    type H = Host;

    fn as_host_mut(&mut self) -> &mut Self::H {
        self.host
    }
}

enum AccountState {
    Touched((RevmAccountInfo, EvmStorage)),
    SelfDestructed,
}

impl<'a, Host, R> EtherlinkVMDB<'a, Host, R>
where
    Host: StorageV1,
{
    #[instrument(skip_all)]
    pub fn new(
        host: &'a mut Host,
        registry: &'a R,
        block: &'a BlockConstants,
        classify_native: Option<Address>,
    ) -> Result<Self, EvmDbError> {
        let system = StorageAccount::from_address(&Address::ZERO)?;
        Ok(EtherlinkVMDB {
            host,
            registry,
            block,
            commit_status: true,
            withdrawals: vec![],
            system,
            original_account_infos: HashMap::default(),
            classify_native,
            staged_alias_origins: HashMap::default(),
        })
    }
}

macro_rules! abort_on_error {
    ($obj:expr, $expr:expr, $msg:expr) => {
        if let Err(err) = $expr {
            $obj.abort();
            log!(Level::Error, "{} error: {err:?}", $msg);
            return;
        }
    };
}

impl<Host, R: Registry> EtherlinkVMDB<'_, Host, R>
where
    Host: StorageV1,
{
    #[instrument(skip_all)]
    pub fn commit_status(&self) -> bool {
        self.commit_status
    }

    pub fn take_withdrawals(&mut self) -> Vec<Withdrawal> {
        std::mem::take(&mut self.withdrawals)
    }

    fn abort(&mut self) {
        self.commit_status = false;
    }

    /// Origin classification currently persisted for `address`. Read
    /// from the snapshot taken in `basic`; accounts committed without
    /// a prior `basic` on this instance (external cross-runtime
    /// commits) fall back to one durable read — never on the
    /// user-input hot path.
    fn existing_origin(
        &mut self,
        address: &Address,
    ) -> Result<AccountOrigin, EvmDbError> {
        match self.original_account_infos.get(address) {
            Some(info) => Ok(info.origin.clone()),
            None => {
                let storage_account = StorageAccount::from_address(address)?;
                Ok(storage_account
                    .info_without_migration(self.host)?
                    .map(|info| info.origin)
                    .unwrap_or_default())
            }
        }
    }

    fn update_account(&mut self, address: Address, account_state: AccountState) {
        let existing_origin = match self.existing_origin(&address) {
            Ok(origin) => origin,
            Err(err) => {
                self.abort();
                log!(
                    Level::Error,
                    "DatabaseCommit `existing_origin` error: {err:?}"
                );
                return;
            }
        };
        match StorageAccount::from_address(&address) {
            Ok(mut storage_account) => match account_state {
                AccountState::Touched((mut info, storage)) => {
                    if let Some(code) = info.code.take() {
                        abort_on_error!(
                            self,
                            CodeStorage::add(
                                self.host,
                                code.original_byte_slice(),
                                Some(info.code_hash)
                            ),
                            "DatabaseCommit `CodeStorage::add`"
                        );
                    }
                    // Decide the classification persisted with this
                    // commit. Existing classifications always survive;
                    // unclassified accounts become Native when they
                    // signed this transaction (`classify_native`) or
                    // carry code — at zero host-call cost since the
                    // origin travels with the info write below. An
                    // alias staged by ensure_alias takes precedence:
                    // its forwarder account is being created in this
                    // very commit.
                    let staged_alias = self.staged_alias_origins.remove(&address);
                    let has_code = info.code_hash != KECCAK_EMPTY;
                    let final_origin = if let Some(alias_info) = staged_alias {
                        AccountOrigin::Alias(alias_info)
                    } else if existing_origin != AccountOrigin::Unclassified {
                        existing_origin
                    } else if self.classify_native == Some(address) || has_code {
                        AccountOrigin::Native
                    } else {
                        AccountOrigin::Unclassified
                    };
                    let info = AccountInfo::with_origin(info, final_origin);
                    // Avoid rewriting the account info if it hasn't changed
                    let unchanged = matches!(
                        self.original_account_infos.get(&address),
                        Some(original_info) if *original_info == info
                    );
                    if !unchanged {
                        abort_on_error!(
                            self,
                            storage_account.set_info_without_code(self.host, info),
                            "DatabaseCommit `set_info_without_code`"
                        );
                    }

                    for (
                        key,
                        EvmStorageSlot {
                            original_value,
                            present_value,
                            ..
                        },
                    ) in storage
                    {
                        if original_value != present_value {
                            abort_on_error!(
                                self,
                                storage_account.set_storage(
                                    self.host,
                                    &key,
                                    &present_value,
                                ),
                                "DatabaseCommit `set_storage`"
                            );
                        }
                    }
                }
                AccountState::SelfDestructed => {
                    // The classification survives a selfdestruct.
                    abort_on_error!(
                        self,
                        storage_account.set_info_without_code(
                            self.host,
                            AccountInfo {
                                origin: existing_origin,
                                ..AccountInfo::default()
                            },
                        ),
                        "DatabaseCommit `set_info_without_code`"
                    );
                }
            },
            error => {
                abort_on_error!(self, error, "DatabaseCommit `get_or_create_account`")
            }
        }
    }
}

// Precompile read functions care about the difference between a path not found and a runtime error
// as path not found is the only one that will produce a revert result
impl<Host, R: Registry> DatabasePrecompileStateChanges for EtherlinkVMDB<'_, Host, R>
where
    Host: StorageV1,
{
    fn log_node_message(&mut self, level: Level, message: &str) {
        log!(level, "{message:?}");
    }

    fn global_counter(&self) -> Result<U256, PrecompileStateError> {
        self.system.read_global_counter(self.host)
    }

    fn ticket_balance(
        &self,
        ticket_hash: &U256,
        owner: &Address,
    ) -> Result<U256, PrecompileStateError> {
        self.system
            .read_ticket_balance(self.host, ticket_hash, owner)
    }

    fn deposit_in_queue(
        &self,
        deposit_id: &U256,
    ) -> Result<FaDepositWithProxy, PrecompileStateError> {
        self.system.read_deposit_from_queue(self.host, deposit_id)
    }

    fn ticketer(&self) -> Result<ContractKt1Hash, PrecompileStateError> {
        let ticketer =
            self.host
                .store_read(&NATIVE_TOKEN_TICKETER_PATH, 0, KT1_B58_SIZE)?;
        let kt1_b58 = std::str::from_utf8(&ticketer)?;
        Ok(ContractKt1Hash::from_b58check(kt1_b58)?)
    }

    fn sequencer(&self) -> Result<PublicKey, PrecompileStateError> {
        let bytes = self.host.store_read_all(&SEQUENCER_KEY_PATH)?;
        let s = std::str::from_utf8(&bytes)?;
        Ok(PublicKey::from_b58check(s)?)
    }

    fn governance_sequencer_upgrade_exists(&self) -> Result<bool, PrecompileStateError> {
        match self.host.store_read_all(&GOVERNANCE_SEQUENCER_UPGRADE_PATH) {
            Ok(_) => Ok(true),
            Err(RuntimeError::PathNotFound) => Ok(false),
            Err(e) => Err(e.into()),
        }
    }

    fn sequencer_change_counter(&self) -> Result<U256, PrecompileStateError> {
        Ok(read_u256_le_default(
            self.host,
            &SEQUENCER_KEY_CHANGE_COUNTER_PATH,
            U256::ZERO,
        )?)
    }
}

impl<Host, R> Database for EtherlinkVMDB<'_, Host, R>
where
    Host: StorageV1,
{
    type Error = EvmDbError;

    fn basic(
        &mut self,
        address: Address,
    ) -> Result<Option<RevmAccountInfo>, Self::Error> {
        let storage_account = StorageAccount::from_address(&address)?;
        let account_info = storage_account.info(self.host)?;

        self.original_account_infos
            .insert(address, account_info.clone());

        Ok(Some(account_info.into()))
    }

    fn code_by_hash(&mut self, code_hash: B256) -> Result<Bytecode, Self::Error> {
        let code_storage = CodeStorage::new(&code_hash)?;
        let bytecode = code_storage.get_code(self.host)?;
        Ok(bytecode.unwrap_or_default())
    }

    fn storage(
        &mut self,
        address: Address,
        index: StorageKey,
    ) -> Result<StorageValue, Self::Error> {
        let storage_account = StorageAccount::from_address(&address)?;
        storage_account.get_storage(self.host, &index)
    }

    fn block_hash(&mut self, number: u64) -> Result<B256, Self::Error> {
        // return 0 when block number not in valid range
        // Ref. https://www.evm.codes/?fork=prague#40 (opcode 0x40)

        match self.block.number.checked_sub(number.into()) {
            Some(block_diff)
                if block_diff <= BLOCKS_STORED.into() && !block_diff.is_zero() =>
            {
                get_block_hash(self.host, number)
            }
            _ => Ok(B256::ZERO),
        }
    }
}
impl<Host, R: Registry> DatabaseCommitPrecompileStateChanges
    for EtherlinkVMDB<'_, Host, R>
where
    Host: StorageV1,
{
    fn commit(&mut self, etherlink_data: PrecompileStateChanges) {
        self.withdrawals = etherlink_data.withdrawals;
        if let Some(new_sequencer_key_change) = etherlink_data.sequencer_key_change {
            abort_on_error!(
                self,
                store_sequencer_key_change(self.host, new_sequencer_key_change),
                "DatabaseCommitPrecompileStateChanges `store_sequencer_key_change`"
            );
        }
        // Persist the replay counter finalized by the layered state. The bump
        // happens at store-time (see `LayeredState::store_sequencer_key_change`)
        // so the counter -- like the change itself -- is rolled back by a
        // reverting transaction, and `commit` only writes the finalized value
        // rather than doing a durable read-modify-write. This is the only bump
        // on the precompile path (application via `store_sequencer` does not bump
        // again), so a single change advances the counter by exactly one.
        if let Some(sequencer_change_counter) = etherlink_data.sequencer_change_counter {
            abort_on_error!(
                self,
                write_sequencer_change_counter(self.host, sequencer_change_counter),
                "DatabaseCommitPrecompileStateChanges `write_sequencer_change_counter`"
            );
        }
        if let Some(global_counter) = etherlink_data.global_counter {
            abort_on_error!(
                self,
                self.system.write_global_counter(self.host, global_counter),
                "DatabaseCommitPrecompileStateChanges `write_global_counter`"
            );
        }
        for ((owner, ticket_hash), amount) in etherlink_data.ticket_balances {
            if amount.is_zero() {
                abort_on_error!(
                    self,
                    self.system
                        .delete_ticket_balance(self.host, &ticket_hash, &owner),
                    "DatabaseCommitPrecompileStateChanges `delete_ticket_balance`"
                );
            } else {
                abort_on_error!(
                    self,
                    self.system.write_ticket_balance(
                        self.host,
                        &ticket_hash,
                        &owner,
                        amount
                    ),
                    "DatabaseCommitPrecompileStateChanges `write_ticket_balance`"
                );
            }
        }
        for (deposit_id, deposit) in etherlink_data.deposits.iter() {
            abort_on_error!(
                self,
                self.system.write_deposit(self.host, deposit_id, deposit),
                "DatabaseCommitPrecompileStateChanges `write_deposit`"
            );
        }
        for deposit_id in etherlink_data.removed_deposits {
            abort_on_error!(
                self,
                self.system
                    .remove_deposit_from_queue(self.host, &deposit_id),
                "DatabaseCommitPrecompileStateChanges `remove_deposit_from_queue`"
            );
        }
        // Hand the staged alias classifications over to the account
        // commit, which runs right after on the same instance: the
        // alias payload is written inside the forwarder's info record.
        for (address, origin) in etherlink_data.created_aliases {
            match origin {
                Origin::Alias(alias_info) => {
                    self.staged_alias_origins.insert(address, alias_info);
                }
                // The journal only stages Alias classifications.
                Origin::Native => {
                    self.abort();
                    log!(
                        Level::Error,
                        "DatabaseCommitPrecompileStateChanges: staged Native origin for {address}"
                    );
                    return;
                }
            }
        }
    }
}

impl<Host, R: Registry> DatabaseCommit for EtherlinkVMDB<'_, Host, R>
where
    Host: StorageV1,
{
    fn commit(&mut self, changes: AddressMap<Account>) {
        for (address, account) in changes {
            // The account isn't marked as touched, the changes are not commited
            // to the database.
            if !account.is_touched() {
                continue;
            }

            // The account is touched and marked as selfdestructed, we clear the
            // account.
            if account.is_selfdestructed() {
                self.update_account(address, AccountState::SelfDestructed);
                continue;
            }

            // The account is touched, the changes are naturally commited to the database.
            self.update_account(
                address,
                AccountState::Touched((account.info, account.storage)),
            );
        }
        // Aliases staged for addresses the EVM run never touched
        // (ensure_alias Branch 2: forwarder already deployed) still
        // need their classification written into the info record.
        for (address, alias_info) in std::mem::take(&mut self.staged_alias_origins) {
            abort_on_error!(
                self,
                write_alias_origin(self.host, &address, alias_info),
                "DatabaseCommit `write_alias_origin`"
            );
        }
        self.original_account_infos.clear();
    }
}

/// Read-modify-write of the info record setting the alias
/// classification, creating a default record if the account does not
/// exist yet.
fn write_alias_origin(
    host: &mut impl StorageV1,
    address: &Address,
    alias_info: AliasInfo,
) -> Result<(), EvmDbError> {
    let mut storage_account = StorageAccount::from_address(address)?;
    let mut info = storage_account
        .info_without_migration(host)?
        .unwrap_or_default();
    info.origin = AccountOrigin::Alias(alias_info);
    storage_account.set_info_without_code(host, info)
}
