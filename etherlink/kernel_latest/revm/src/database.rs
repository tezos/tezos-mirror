// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::mem;

use crate::{
    block_storage::{get_block_hash, BLOCKS_STORED},
    code_storage::CodeStorage,
    helpers::legacy::FaDepositWithProxy,
    send_outbox_message::Withdrawal,
    world_state_handler::{
        account_path, StorageAccount, WorldStateHandler, WITHDRAWALS_TICKETER_PATH,
    },
    Error,
};

use alloy_primitives::KECCAK256_EMPTY;
use revm::{
    primitives::{Address, HashMap, StorageKey, StorageValue, B256, U256},
    state::{Account, AccountInfo, Bytecode, EvmStorage, EvmStorageSlot},
    Database, DatabaseCommit,
};
use tezos_crypto_rs::hash::{ContractKt1Hash, HashTrait};
use tezos_ethereum::block::BlockConstants;
use tezos_evm_logging::{log, Level::Error as LogError};
use tezos_evm_runtime::runtime::Runtime;

pub struct EtherlinkVMDB<'a, Host: Runtime> {
    /// Runtime host
    host: &'a mut Host,
    /// EVM world state handler
    world_state_handler: &'a mut WorldStateHandler,
    /// Constants for the current block
    block: &'a BlockConstants,
    /// Commit guard, the `DatabaseCommit` trait and in particular
    /// its `commit` function does NOT return errors.
    /// We need this guard to change if there's an unrecoverable
    /// error and we need to revert the changes made to the durable
    /// storage.
    commit_status: &'a mut bool,
    /// Withdrawals accumulated by the current execution and consumed at the end of it
    withdrawals: Vec<Withdrawal>,
    /// HACK: [PRECOMPILE_ZERO_ADDRESS_AND_SIMULATION]
    /// This is used in order to avoid the problem of EIP-3607 for address
    /// zero which contains the forwarder code.
    caller: Address,
}

enum AccountState {
    Touched((AccountInfo, EvmStorage)),
    SelfDestructed(B256),
}

// See: https://github.com/rust-lang/rust-clippy/issues/5787
#[allow(clippy::needless_lifetimes)]
impl<'a, Host: Runtime> EtherlinkVMDB<'a, Host> {
    pub fn new(
        host: &'a mut Host,
        block: &'a BlockConstants,
        world_state_handler: &'a mut WorldStateHandler,
        commit_status: &'a mut bool,
        caller: Address,
    ) -> Self {
        EtherlinkVMDB {
            host,
            block,
            world_state_handler,
            commit_status,
            withdrawals: vec![],
            caller,
        }
    }
}

pub(crate) trait PrecompileDatabase: Database {
    fn get_or_create_account(&self, address: Address) -> Result<StorageAccount, Error>;
    fn ticketer(&self) -> Result<ContractKt1Hash, Error>;
    fn push_withdrawal(&mut self, withdrawal: Withdrawal);
    fn take_withdrawals(&mut self) -> Vec<Withdrawal>;
    fn ticket_balance_add(
        &mut self,
        ticket_hash: &U256,
        owner: &Address,
        amount: U256,
    ) -> Result<bool, Error>;
    fn ticket_balance_remove(
        &mut self,
        ticket_hash: &U256,
        owner: &Address,
        amount: U256,
    ) -> Result<bool, Error>;
    fn read_deposit_from_queue(
        &self,
        deposit_id: &U256,
    ) -> Result<FaDepositWithProxy, Error>;
    fn remove_deposit_from_queue(&mut self, deposit_id: &U256) -> Result<(), Error>;
}

impl<Host: Runtime> EtherlinkVMDB<'_, Host> {
    pub fn commit_status(&self) -> bool {
        *self.commit_status
    }

    pub fn initialize_storage(&mut self) -> Result<(), Error> {
        self.world_state_handler
            .begin_transaction(self.host)
            .map_err(|err| Error::Custom(err.to_string()))
    }

    pub fn commit_storage(&mut self) -> Result<(), Error> {
        self.world_state_handler
            .commit_transaction(self.host)
            .map_err(|err| Error::Custom(err.to_string()))
    }

    pub fn drop_storage(&mut self) -> Result<(), Error> {
        self.world_state_handler
            .rollback_transaction(self.host)
            .map_err(|err| Error::Custom(err.to_string()))
    }

    fn abort(&mut self) {
        *self.commit_status = false;
    }

    fn update_account(&mut self, address: Address, account_state: AccountState) {
        match self.get_or_create_account(address) {
            Ok(mut storage_account) => match account_state {
                AccountState::Touched((info, storage)) => {
                    if let Err(err) = storage_account.set_info(self.host, info) {
                        self.abort();
                        log!(
                            self.host,
                            LogError,
                            "DatabaseCommit `set_info` error: {err:?}"
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
                            if let Err(err) = storage_account.set_storage(
                                self.host,
                                &key,
                                &present_value,
                            ) {
                                self.abort();
                                log!(
                                    self.host,
                                    LogError,
                                    "DatabaseCommit `set_storage` error: {err:?}"
                                );
                            }
                        }
                    }
                }
                AccountState::SelfDestructed(code_hash) => {
                    if let Err(err) = storage_account.clear_info(self.host, &code_hash) {
                        self.abort();
                        log!(
                            self.host,
                            LogError,
                            "DatabaseCommit `clear_info` error: {err:?}"
                        );
                    }

                    if let Err(err) = storage_account.clear_storage(self.host) {
                        self.abort();
                        log!(
                            self.host,
                            LogError,
                            "DatabaseCommit `clear_storage` error: {err:?}"
                        );
                    }
                }
            },
            Err(err) => {
                self.abort();
                log!(
                    self.host,
                    LogError,
                    "DatabaseCommit `get_or_create_account` error: {err:?}"
                )
            }
        }
    }
}

impl<Host: Runtime> PrecompileDatabase for EtherlinkVMDB<'_, Host> {
    fn get_or_create_account(&self, address: Address) -> Result<StorageAccount, Error> {
        // TODO: get_account function should be implemented whenever errors are
        // reintroduced
        self.world_state_handler
            .get_or_create(self.host, &account_path(&address)?)
            .map_err(|err| Error::Custom(err.to_string()))
    }

    // This is only used by native withdrawals for backwards compatibility
    fn ticketer(&self) -> Result<ContractKt1Hash, Error> {
        let ticketer = self.host.store_read_all(&WITHDRAWALS_TICKETER_PATH)?;
        let kt1_b58 = String::from_utf8(ticketer.to_vec()).unwrap();
        Ok(ContractKt1Hash::from_b58check(&kt1_b58).unwrap())
    }

    fn push_withdrawal(&mut self, withdrawal: Withdrawal) {
        self.withdrawals.push(withdrawal);
    }

    fn take_withdrawals(&mut self) -> Vec<Withdrawal> {
        mem::take(&mut self.withdrawals)
    }

    fn ticket_balance_add(
        &mut self,
        ticket_hash: &U256,
        owner: &Address,
        amount: U256,
    ) -> Result<bool, Error> {
        let mut account_zero = self.get_or_create_account(Address::ZERO)?;
        account_zero.ticket_balance_add(self.host, ticket_hash, owner, amount)
    }

    fn ticket_balance_remove(
        &mut self,
        ticket_hash: &U256,
        owner: &Address,
        amount: U256,
    ) -> Result<bool, Error> {
        let mut account_zero = self.get_or_create_account(Address::ZERO)?;
        account_zero.ticket_balance_remove(self.host, ticket_hash, owner, amount)
    }

    fn read_deposit_from_queue(
        &self,
        deposit_id: &U256,
    ) -> Result<FaDepositWithProxy, Error> {
        let account_zero = self.get_or_create_account(Address::ZERO)?;
        account_zero.read_deposit_from_queue(self.host, deposit_id)
    }

    fn remove_deposit_from_queue(&mut self, deposit_id: &U256) -> Result<(), Error> {
        let account_zero = self.get_or_create_account(Address::ZERO)?;
        account_zero.remove_deposit_from_queue(self.host, deposit_id)
    }
}

impl<Host: Runtime> Database for EtherlinkVMDB<'_, Host> {
    type Error = Error;

    fn basic(&mut self, address: Address) -> Result<Option<AccountInfo>, Self::Error> {
        let storage_account = self.get_or_create_account(address)?;
        let account_info = storage_account.info(self.host)?;

        if self.caller == Address::ZERO && address == Address::ZERO {
            // HACK: [PRECOMPILE_ZERO_ADDRESS_AND_SIMULATION]
            // This can only happen in a simulation case only.
            // The zero address contains code for legacy reasons related to precompiles.
            // We must return empty code here otherwise the simulation will fail because
            // of EIP-3607.
            return Ok(Some(AccountInfo {
                code_hash: KECCAK256_EMPTY,
                code: None,
                ..account_info
            }));
        }

        Ok(Some(account_info))
    }

    fn code_by_hash(&mut self, code_hash: B256) -> Result<Bytecode, Self::Error> {
        let code_storage = CodeStorage::new(&code_hash)?;
        let bytecode = code_storage.get_code(self.host)?;

        Ok(bytecode)
    }

    fn storage(
        &mut self,
        address: Address,
        index: StorageKey,
    ) -> Result<StorageValue, Self::Error> {
        let storage_account = self.get_or_create_account(address)?;
        let storage_value = storage_account.get_storage(self.host, &index)?;

        Ok(storage_value)
    }

    fn block_hash(&mut self, number: u64) -> Result<B256, Self::Error> {
        // return 0 when block number not in valid range
        // Ref. https://www.evm.codes/?fork=cancun#40 (opcode 0x40)

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

impl<Host: Runtime> DatabaseCommit for EtherlinkVMDB<'_, Host> {
    fn commit(&mut self, changes: HashMap<Address, Account>) {
        for (address, account) in changes {
            // The account isn't marked as touched, the changes are not commited
            // to the database.
            if !account.is_touched() {
                continue;
            }

            // The account is touched and marked as selfdestructed, we clear the
            // account.
            if account.is_selfdestructed() {
                self.update_account(
                    address,
                    AccountState::SelfDestructed(account.info.code_hash),
                );
                continue;
            }

            // The account is touched, the changes are naturally commited to the database.
            self.update_account(
                address,
                AccountState::Touched((account.info, account.storage)),
            );
        }
    }
}
