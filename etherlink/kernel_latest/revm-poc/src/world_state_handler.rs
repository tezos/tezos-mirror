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
    runtime::{Runtime, RuntimeError, ValueType},
};
use tezos_smart_rollup_storage::storage::Storage;

use crate::{
    code_storage::CodeStorage,
    storage_helpers::{read_b256_be_default, read_u256_le_default, read_u64_le_default},
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
    let path_string = format!("/{:x}", address);
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
    pub fn balance(&self, host: &impl Runtime) -> U256 {
        let path = concat(&self.path, &BALANCE_PATH).unwrap();
        read_u256_le_default(host, &path, BALANCE_DEFAULT_VALUE)
    }

    pub fn set_balance(&mut self, host: &mut impl Runtime, new_balance: U256) {
        let path = concat(&self.path, &BALANCE_PATH).unwrap();
        host.store_write_all(&path, &new_balance.to_le_bytes::<{ U256::BYTES }>())
            .unwrap()
    }

    pub fn nonce(&self, host: &impl Runtime) -> u64 {
        let path = concat(&self.path, &NONCE_PATH).unwrap();
        read_u64_le_default(host, &path, NONCE_DEFAULT_VALUE)
    }

    pub fn set_nonce(&mut self, host: &mut impl Runtime, nonce: u64) {
        let path = concat(&self.path, &NONCE_PATH).unwrap();

        let value_bytes: [u8; 8] = nonce.to_le_bytes();

        host.store_write_all(&path, &value_bytes).unwrap()
    }

    pub fn code_hash(&self, host: &impl Runtime) -> B256 {
        let path = concat(&self.path, &CODE_HASH_PATH).unwrap();
        read_b256_be_default(host, &path, KECCAK_EMPTY)
    }

    pub fn code_exists(&self, host: &impl Runtime) -> bool {
        let path = concat(&self.path, &CODE_HASH_PATH).unwrap();

        match host.store_has(&path) {
            Ok(Some(ValueType::Value | ValueType::ValueWithSubtree)) => true,
            Ok(Some(ValueType::Subtree) | None) => false,
            Err(err) => panic!("{err:?}"),
        }
    }

    pub fn code(&self, host: &impl Runtime) -> Option<Bytecode> {
        let path = concat(&self.path, &CODE_PATH).unwrap();

        match host.store_read_all(&path) {
            Ok(bytes) => {
                // NB: [new_raw_checked] here is great as it's backward-compatible
                // but also future proof. It will check if the decoded bytes is
                // a legacy code, an EIP-7702 EOA's code or an EOF code.
                Some(Bytecode::new_raw_checked(Bytes::from(bytes)).unwrap())
            }
            Err(RuntimeError::PathNotFound) => {
                let code_hash = self.code_hash(host);
                let code_storage = CodeStorage::new(&code_hash);
                Some(code_storage.get_code(host))
            }
            Err(err) => panic!("{err:?}"),
        }
    }

    pub fn set_code(&mut self, host: &mut impl Runtime, code: Option<Bytecode>) {
        if self.code_exists(host) {
            // TODO: return a proper error here and handle it when it does
            // make sense.
        } else {
            let code_hash = match code {
                None => KECCAK_EMPTY,
                Some(code) => CodeStorage::add(host, code.bytes_slice()),
            };
            let code_hash_bytes: [u8; 32] = code_hash.into();
            let code_hash_path = concat(&self.path, &CODE_HASH_PATH).unwrap();
            host.store_write_all(&code_hash_path, &code_hash_bytes)
                .unwrap()
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

    pub fn set_info(&mut self, host: &mut impl Runtime, new_infos: AccountInfo) {
        let AccountInfo {
            balance,
            nonce,
            code,
            ..
        } = new_infos;

        self.set_balance(host, balance);
        self.set_nonce(host, nonce);
        // TODO: setting an already existing code will cause issues, catch
        // the error here when it's implemented.
        self.set_code(host, code);
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

    pub fn set_storage(&mut self, host: &mut impl Runtime, index: &U256, value: &U256) {
        let path = self.storage_path(index);
        let value_bytes = value.to_le_bytes::<{ U256::BYTES }>();

        host.store_write_all(&path, &value_bytes).unwrap()
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
