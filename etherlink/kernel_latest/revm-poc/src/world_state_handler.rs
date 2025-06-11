// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023,2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm::{
    primitives::{Address, Bytes, B256, KECCAK_EMPTY, U256},
    state::{AccountInfo, Bytecode},
};
use tezos_smart_rollup_host::{
    path::{concat, OwnedPath, RefPath},
    runtime::{Runtime, RuntimeError},
};
use tezos_smart_rollup_storage::storage::Storage;

use crate::storage_helpers::{
    read_b256_be_default, read_u256_le_default, read_u64_le_default,
};

#[cfg(test)]
/// Path where EVM accounts are stored.
pub const EVM_ACCOUNTS_PATH: RefPath =
    RefPath::assert_from(b"/evm/world_state/eth_accounts");

/// Path where an account nonce is stored. This should be prefixed with the path to
/// where the account is stored for the world state or for the current transaction.
const NONCE_PATH: RefPath = RefPath::assert_from(b"/nonce");

/// Path where an account balance, ether held, is stored. This should be prefixed with the path to
/// where the account is stored for the world state or for the current transaction.
const BALANCE_PATH: RefPath = RefPath::assert_from(b"/balance");

/// "Internal" accounts - accounts with contract code have a contract code hash.
/// This value is computed when the code is stored and kept for future queries. This
/// path should be prefixed with the path to where the account is stored for the world
/// state or for the current transaction.
const CODE_HASH_PATH: RefPath = RefPath::assert_from(b"/code.hash");

/// "Internal" accounts - accounts with contract code, have their code stored here.
/// This path should be prefixed with the path to where the account is stored for the
/// world state or for the current transaction.
const CODE_PATH: RefPath = RefPath::assert_from(b"/code");

/// The contracts of "internal" accounts have their own storage area. The account
/// location prefixed to this path gives the root path (prefix) to where such storage
/// values are kept. Each index in durable storage gives one complete path to one
/// such 256 bit integer value in storage.
const STORAGE_ROOT_PATH: RefPath = RefPath::assert_from(b"/storage");

/// If a contract tries to read a value from storage and it has previously not written
/// anything to this location or if it wrote the default value, then it gets this
/// value back.
const STORAGE_DEFAULT_VALUE: U256 = U256::ZERO;

/// Default balance value for an account.
const BALANCE_DEFAULT_VALUE: U256 = U256::ZERO;

/// Default nonce value for an account.
const NONCE_DEFAULT_VALUE: u64 = 0;

pub fn account_path(address: &Address) -> OwnedPath {
    let path_string = format!("/{:?}", address);
    OwnedPath::try_from(path_string).unwrap()
}

pub fn path_from_u256(index: &U256) -> OwnedPath {
    let path_string = format!("/{}", index);
    OwnedPath::try_from(path_string).unwrap()
}

pub struct StorageAccount {
    path: OwnedPath,
}

impl StorageAccount {
    #[cfg(test)]
    pub fn _from_address(address: &Address) -> Self {
        let path = concat(&EVM_ACCOUNTS_PATH, &account_path(address)).unwrap();
        path.into()
    }

    pub fn balance(&self, host: &impl Runtime) -> U256 {
        let path = concat(&self.path, &BALANCE_PATH).unwrap();
        read_u256_le_default(host, &path, BALANCE_DEFAULT_VALUE)
    }

    pub fn nonce(&self, host: &impl Runtime) -> u64 {
        let path = concat(&self.path, &NONCE_PATH).unwrap();
        read_u64_le_default(host, &path, NONCE_DEFAULT_VALUE)
    }

    pub fn code_hash(&self, host: &impl Runtime) -> B256 {
        let path = concat(&self.path, &CODE_HASH_PATH).unwrap();
        read_b256_be_default(host, &path, KECCAK_EMPTY)
    }

    pub fn code(&self, host: &impl Runtime) -> Option<Bytecode> {
        let path = concat(&self.path, &CODE_PATH).unwrap();

        match host.store_read_all(&path) {
            Ok(bytes) =>
            // TODO: we don't support EIP-7702 nor EOF related code.
            {
                Some(Bytecode::new_legacy(Bytes::from(bytes)))
            }
            Err(RuntimeError::PathNotFound) => {
                // TODO: add code storage when implemented
                panic!("Hashconsing not implemented yet")
            }
            Err(err) => panic!("{err:?}"),
        }
    }

    pub fn info(&self, host: &impl Runtime) -> AccountInfo {
        AccountInfo {
            balance: self.balance(host),
            nonce: self.nonce(host),
            code_hash: self.code_hash(host),
            code: self.code(host),
        }
    }

    pub fn storage_path(&self, index: &U256) -> OwnedPath {
        let storage_path = concat(&self.path, &STORAGE_ROOT_PATH).unwrap();
        let index_path = path_from_u256(index);
        concat(&storage_path, &index_path).unwrap()
    }

    pub fn get_storage(&self, host: &impl Runtime, index: &U256) -> U256 {
        let path = self.storage_path(index);
        read_u256_le_default(host, &path, STORAGE_DEFAULT_VALUE)
    }
}

impl From<OwnedPath> for StorageAccount {
    fn from(path: OwnedPath) -> Self {
        Self { path }
    }
}

pub type WorldStateHandler = Storage<StorageAccount>;

#[cfg(test)]
pub fn new_world_state_handler() -> WorldStateHandler {
    Storage::<StorageAccount>::init(&EVM_ACCOUNTS_PATH).unwrap()
}
