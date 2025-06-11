// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::world_state_handler::{account_path, StorageAccount, WorldStateHandler};

use revm::{
    primitives::{Address, HashMap, StorageKey, StorageValue, B256},
    state::{Account, AccountInfo, Bytecode},
    Database, DatabaseCommit,
};
use std::convert::Infallible;
use tezos_smart_rollup_host::runtime::Runtime;

pub struct EtherlinkVMDB<'a, Host: Runtime> {
    /// Runtime host
    host: &'a mut Host,
    /// EVM world state handler
    world_state_handler: &'a mut WorldStateHandler,
}

// See: https://github.com/rust-lang/rust-clippy/issues/5787
#[allow(clippy::needless_lifetimes)]
impl<'a, Host: Runtime> EtherlinkVMDB<'a, Host> {
    #[cfg(test)]
    pub fn new(
        host: &'a mut Host,
        world_state_handler: &'a mut WorldStateHandler,
    ) -> Self {
        EtherlinkVMDB {
            host,
            world_state_handler,
        }
    }
}

impl<Host: Runtime> EtherlinkVMDB<'_, Host> {
    pub(crate) fn get_or_create_account(&self, address: Address) -> StorageAccount {
        // TODO: get_account function should be implemented whenever errors are
        // reintroduced
        self.world_state_handler
            .get_or_create(self.host, &account_path(&address))
            .unwrap()
    }
}

impl<Host: Runtime> Database for EtherlinkVMDB<'_, Host> {
    type Error = Infallible;

    fn basic(&mut self, address: Address) -> Result<Option<AccountInfo>, Self::Error> {
        let storage_account = self.get_or_create_account(address);
        let account_info = storage_account.info(self.host);

        Ok(Some(account_info))
    }

    fn code_by_hash(&mut self, _code_hash: B256) -> Result<Bytecode, Self::Error> {
        // TODO: use code storage when implemented
        unimplemented!()
    }

    fn storage(
        &mut self,
        address: Address,
        index: StorageKey,
    ) -> Result<StorageValue, Self::Error> {
        let storage_account = self.get_or_create_account(address);
        let storage_value = storage_account.get_storage(self.host, &index);

        Ok(storage_value)
    }

    fn block_hash(&mut self, _number: u64) -> Result<B256, Self::Error> {
        // TODO: use block constants and block storage when implemented
        unimplemented!()
    }
}

impl<Host: Runtime> DatabaseCommit for EtherlinkVMDB<'_, Host> {
    fn commit(&mut self, _changes: HashMap<Address, Account>) {
        unimplemented!()
    }
}
