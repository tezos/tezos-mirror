// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::mem;

use crate::{
    block_storage::{get_block_hash, BLOCKS_STORED},
    code_storage::CodeStorage,
    send_outbox_message::Withdrawal,
    world_state_handler::{account_path, StorageAccount, WorldStateHandler},
    Error,
};

use revm::{
    primitives::{Address, HashMap, StorageKey, StorageValue, B256},
    state::{Account, AccountInfo, AccountStatus, Bytecode, EvmStorageSlot},
    Database, DatabaseCommit,
};
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
}

// See: https://github.com/rust-lang/rust-clippy/issues/5787
#[allow(clippy::needless_lifetimes)]
impl<'a, Host: Runtime> EtherlinkVMDB<'a, Host> {
    pub fn new(
        host: &'a mut Host,
        block: &'a BlockConstants,
        world_state_handler: &'a mut WorldStateHandler,
        commit_status: &'a mut bool,
    ) -> Self {
        EtherlinkVMDB {
            host,
            block,
            world_state_handler,
            commit_status,
            withdrawals: vec![],
        }
    }
}

pub trait PrecompileDatabase: Database {
    fn get_or_create_account(&self, address: Address) -> Result<StorageAccount, Error>;
    fn push_withdrawal(&mut self, withdrawal: Withdrawal);
    fn take_withdrawals(&mut self) -> Vec<Withdrawal>;
}

impl<Host: Runtime> EtherlinkVMDB<'_, Host> {
    pub fn abort(&mut self) {
        *self.commit_status = false;
    }

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
}

impl<Host: Runtime> PrecompileDatabase for EtherlinkVMDB<'_, Host> {
    fn get_or_create_account(&self, address: Address) -> Result<StorageAccount, Error> {
        // TODO: get_account function should be implemented whenever errors are
        // reintroduced
        self.world_state_handler
            .get_or_create(self.host, &account_path(&address))
            .map_err(|err| Error::Custom(err.to_string()))
    }

    fn push_withdrawal(&mut self, withdrawal: Withdrawal) {
        self.withdrawals.push(withdrawal);
    }

    fn take_withdrawals(&mut self) -> Vec<Withdrawal> {
        mem::take(&mut self.withdrawals)
    }
}

impl<Host: Runtime> Database for EtherlinkVMDB<'_, Host> {
    type Error = Error;

    fn basic(&mut self, address: Address) -> Result<Option<AccountInfo>, Self::Error> {
        let storage_account = self.get_or_create_account(address)?;
        let account_info = storage_account.info(self.host)?;

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
        for (
            address,
            Account {
                info,
                storage,
                status,
                ..
            },
        ) in changes
        {
            // The account is marked as touched, the changes should be commited
            // to the database.
            if status.contains(AccountStatus::Touched) {
                match self.get_or_create_account(address) {
                    Ok(mut storage_account) => {
                        if let Err(err) = storage_account.set_info(self.host, info) {
                            self.abort();
                            log!(
                                self.host,
                                LogError,
                                "DatabaseCommit `set_info` error: {err:?}"
                            );
                        }

                        for (key, EvmStorageSlot { present_value, .. }) in storage {
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
    }
}
