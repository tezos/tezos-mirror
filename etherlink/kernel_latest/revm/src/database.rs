// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    block_storage::{get_block_hash, BLOCKS_STORED},
    code_storage::CodeStorage,
    world_state_handler::{account_path, StorageAccount, WorldStateHandler},
    Error,
};

use revm::{
    primitives::{Address, HashMap, StorageKey, StorageValue, B256},
    state::{Account, AccountInfo, AccountStatus, Bytecode, EvmStorageSlot},
    Database, DatabaseCommit,
};
use tezos_ethereum::block::BlockConstants;
use tezos_evm_logging::{
    log,
    Level::{Debug, Error as LogError},
};
use tezos_evm_runtime::runtime::Runtime;

pub struct EtherlinkVMDB<'a, Host: Runtime> {
    /// Runtime host
    host: &'a mut Host,
    /// EVM world state handler
    world_state_handler: &'a mut WorldStateHandler,
    /// Constants for the current block
    block: &'a BlockConstants,
}

// See: https://github.com/rust-lang/rust-clippy/issues/5787
#[allow(clippy::needless_lifetimes)]
impl<'a, Host: Runtime> EtherlinkVMDB<'a, Host> {
    pub fn new(
        host: &'a mut Host,
        block: &'a BlockConstants,
        world_state_handler: &'a mut WorldStateHandler,
    ) -> Self {
        EtherlinkVMDB {
            host,
            block,
            world_state_handler,
        }
    }
}

pub trait AccountDatabase: Database {
    fn get_or_create_account(&self, address: Address) -> Result<StorageAccount, Error>;
}

impl<Host: Runtime> EtherlinkVMDB<'_, Host> {
    #[cfg(test)]
    pub fn insert_account_info(&mut self, address: Address, info: AccountInfo) {
        let mut storage_account = self.get_or_create_account(address).unwrap();
        storage_account.set_info(self.host, info).unwrap()
    }

    #[cfg(test)]
    pub fn storage_slot(
        &self,
        address: Address,
        storage_key: StorageKey,
    ) -> StorageValue {
        let storage_account = self.get_or_create_account(address).unwrap();
        storage_account
            .get_storage(self.host, &storage_key)
            .unwrap()
    }
}

impl<Host: Runtime> AccountDatabase for EtherlinkVMDB<'_, Host> {
    fn get_or_create_account(&self, address: Address) -> Result<StorageAccount, Error> {
        // TODO: get_account function should be implemented whenever errors are
        // reintroduced
        self.world_state_handler
            .get_or_create(self.host, &account_path(&address))
            .map_err(|err| Error::Custom(err.to_string()))
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
            match status {
                // The account is marked as touched, the changes should be commited
                // to the database.
                AccountStatus::Touched => match self.get_or_create_account(address) {
                    Ok(mut storage_account) => {
                        if let Err(err) = storage_account.set_info(self.host, info) {
                            log!(self.host, LogError, "DatabaseCommit `set_info` error: {err:?}");
                        }

                        for (key, EvmStorageSlot { present_value, .. }) in storage {
                            if let Err(err) = storage_account.set_storage(
                                self.host,
                                &key,
                                &present_value,
                            ) {
                                log!(self.host, LogError, "DatabaseCommit `set_storage` error: {err:?}");
                            }
                        }
                    }
                    Err(err) => log!(self.host, LogError, "DatabaseCommit `get_or_create_account` error: {err:?}"),
                },
                AccountStatus::Created
                | AccountStatus::CreatedLocal
                | AccountStatus::SelfDestructed
                | AccountStatus::SelfDestructedLocal
                | AccountStatus::LoadedAsNotExisting
                | AccountStatus::Cold => {
                    // Local changes only, nothing is commited
                    // TODO: Double check for [Created] case.
                }
                undefined_account_status => log!(
                    self.host,
                    Debug,
                    "DatabaseCommit debug: undefined account status {undefined_account_status:?}"
                ),
            }
        }
    }
}
