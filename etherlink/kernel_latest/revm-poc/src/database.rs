// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use std::{convert::Infallible, marker::PhantomData};

use revm::{
    primitives::{Address, HashMap, StorageKey, StorageValue, B256},
    state::{Account, AccountInfo, Bytecode},
    Database, DatabaseCommit,
};

pub struct EtherlinkVMDB {
    empty: PhantomData<()>,
}

impl EtherlinkVMDB {
    #[cfg(test)]
    pub fn new() -> Self {
        EtherlinkVMDB { empty: PhantomData }
    }
}

impl Database for EtherlinkVMDB {
    type Error = Infallible;

    fn basic(&mut self, _address: Address) -> Result<Option<AccountInfo>, Self::Error> {
        unimplemented!()
    }

    fn code_by_hash(&mut self, _code_hash: B256) -> Result<Bytecode, Self::Error> {
        unimplemented!()
    }

    fn storage(
        &mut self,
        _address: Address,
        _index: StorageKey,
    ) -> Result<StorageValue, Self::Error> {
        unimplemented!()
    }

    fn block_hash(&mut self, _number: u64) -> Result<B256, Self::Error> {
        unimplemented!()
    }
}

impl DatabaseCommit for EtherlinkVMDB {
    fn commit(&mut self, _changes: HashMap<Address, Account>) {
        unimplemented!()
    }
}
