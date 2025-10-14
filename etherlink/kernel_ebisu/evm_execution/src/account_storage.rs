// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Ethereum account state and storage

use const_decoder::Decoder;
use host::path::{concat, OwnedPath, Path, RefPath};
use host::runtime::{RuntimeError, ValueType};
use primitive_types::{H160, H256, U256};
use revm::primitives::{B256, KECCAK_EMPTY};
use revm::state::AccountInfo;
use revm_etherlink::storage::world_state_handler::StorageAccount;
use rlp::DecoderError;
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_storage::storage::Storage;
use tezos_storage::error::Error as GenStorageError;
use tezos_storage::{
    path_from_h256, read_h256_be_default, read_h256_be_opt, read_u256_le_default,
    WORD_SIZE,
};
use thiserror::Error;

use crate::utilities::alloy::alloy_to_u256;
use crate::{code_storage, DurableStorageError};

/// All errors that may happen as result of using the Ethereum account
/// interface.
#[derive(Error, Eq, PartialEq, Clone, Debug)]
pub enum AccountStorageError {
    /// Some error happened while using durable storage, either from an invalid
    /// path or a runtime error.
    #[error("Durable storage error: {0:?}")]
    DurableStorageError(#[from] DurableStorageError),
    /// Some error occurred while using the transaction storage
    /// API.
    #[error("Transaction storage API error: {0:?}")]
    StorageError(tezos_smart_rollup_storage::StorageError),
    #[error("Failed to decode: {0}")]
    RlpDecoderError(DecoderError),
    #[error("Storage error: error while reading a value (incorrect size). Expected {expected} but got {actual}")]
    InvalidLoadValue { expected: usize, actual: usize },
    #[error("Storage error: Failed to encode a value with BinWriter: {0}")]
    BinWriteError(String),
    #[error("Storage error: Failed to decode a value with NomReader: {0}")]
    NomReadError(String),
    /// Some account balance became greater than what can be
    /// stored in an unsigned 256 bit integer.
    #[error("Account balance overflow")]
    BalanceOverflow,
    /// Technically, the Ethereum account nonce can overflow if
    /// an account does an incredible number of transactions.
    #[error("Nonce overflow")]
    NonceOverflow,
    #[error("Account code already initialised")]
    AccountCodeAlreadySet,
    #[error("Tried casting an Implicit account into an Originated account")]
    ImplicitToOriginated,
    #[error("Tried casting an Originated account into an Implicit account")]
    OriginatedToImplicit,
    #[error("REVM Storage error: {0}")]
    REVMStorageError(revm_etherlink::Error),
}

impl From<revm_etherlink::Error> for AccountStorageError {
    fn from(error: revm_etherlink::Error) -> Self {
        AccountStorageError::REVMStorageError(error)
    }
}

impl From<tezos_smart_rollup_storage::StorageError> for AccountStorageError {
    fn from(error: tezos_smart_rollup_storage::StorageError) -> Self {
        AccountStorageError::StorageError(error)
    }
}

impl From<host::path::PathError> for AccountStorageError {
    fn from(error: host::path::PathError) -> Self {
        AccountStorageError::DurableStorageError(DurableStorageError::from(error))
    }
}

impl From<host::runtime::RuntimeError> for AccountStorageError {
    fn from(error: host::runtime::RuntimeError) -> Self {
        AccountStorageError::DurableStorageError(DurableStorageError::from(error))
    }
}

impl From<GenStorageError> for AccountStorageError {
    fn from(e: GenStorageError) -> Self {
        match e {
            GenStorageError::Path(e) => {
                AccountStorageError::DurableStorageError(DurableStorageError::from(e))
            }
            GenStorageError::Runtime(e) => {
                AccountStorageError::DurableStorageError(DurableStorageError::from(e))
            }
            GenStorageError::BinWriteError(msg) => {
                AccountStorageError::BinWriteError(msg)
            }
            GenStorageError::NomReadError(msg) => AccountStorageError::NomReadError(msg),
            GenStorageError::Storage(e) => AccountStorageError::StorageError(e),
            GenStorageError::RlpDecoderError(e) => {
                AccountStorageError::RlpDecoderError(e)
            }
            GenStorageError::InvalidLoadValue { expected, actual } => {
                AccountStorageError::InvalidLoadValue { expected, actual }
            }
            GenStorageError::ImplicitToOriginated => {
                AccountStorageError::ImplicitToOriginated
            }
            GenStorageError::OriginatedToImplicit => {
                AccountStorageError::OriginatedToImplicit
            }
        }
    }
}

/// When an Ethereum contract acts on storage, it spends gas. The gas cost of
/// operations that affect storage depends both on what was in storage already,
/// and the new value written to storage.
#[derive(Eq, PartialEq, Debug)]
pub struct StorageEffect {
    /// Indicates whether the original value before a storage update
    /// was the default value.
    pub from_default: bool,
    /// Indicates whether the new value after storage update is the
    /// default value.
    pub to_default: bool,
}

/// An Ethereum account
///
/// This struct defines the storage interface for interacting with Ethereum accounts
/// in durable storage. The values kept in storage correspond to the values in section
/// 4.1 of the Ethereum Yellow Paper. Also, contract code and contract permanent data
/// storage are accessed through this API. The durable storage for an account includes:
/// - The **nonce** of the account. A scalar value equal to the number of transactions
///   send from this address or (in the case of contract accounts) the number of contract
///   creations done by this contract.
/// - The **balance** of the account. A scalar value equal to the number of Wei held by
///   the account.
/// - The **code hash** of any contract code associated with the account.
/// - The **code**, ie, the opcodes, of any contract associated with the account.
///
/// An account is considered _empty_ (according to EIP-161) iff
/// `balance == nonce == code == 0x`.
///
/// The Ethereum Yellow Paper also lists the **storageRoot** as a field associated with
/// an account. We don't currently require it, and so it's omitted.
#[derive(Debug, PartialEq)]
pub struct EthereumAccount {
    pub path: OwnedPath,
}

impl From<OwnedPath> for EthereumAccount {
    fn from(path: OwnedPath) -> Self {
        Self { path }
    }
}

/// Path where Ethereum accounts are stored
pub const EVM_ACCOUNTS_PATH: RefPath =
    RefPath::assert_from(b"/evm/world_state/eth_accounts");

/// Path where an account balance, ether held, is stored. This should be prefixed with the path to
/// where the account is stored for the world state or for the current transaction.
const BALANCE_PATH: RefPath = RefPath::assert_from(b"/balance");

/// "Internal" accounts - accounts with contract code, have their code stored here.
/// This
/// path should be prefixed with the path to
/// where the account is stored for the world state or for the current transaction.
const CODE_PATH: RefPath = RefPath::assert_from(b"/code");

/// The contracts of "internal" accounts have their own storage area. The account
/// location prefixed to this path gives the root path (prefix) to where such storage
/// values are kept. Each index in durable storage gives one complete path to one
/// such 256 bit integer value in storage.
const STORAGE_ROOT_PATH: RefPath = RefPath::assert_from(b"/storage");

/// If a contract tries to read a value from storage and it has previously not written
/// anything to this location or if it wrote the default value, then it gets this
/// value back.
const STORAGE_DEFAULT_VALUE: H256 = H256::zero();

/// An account with no code - an "external" account, or an unused account has the zero
/// hash as code hash.
const CODE_HASH_BYTES: [u8; WORD_SIZE] = Decoder::Hex
    .decode(b"c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470");

/// The default hash for when there is no code - the hash of the empty string.
pub const CODE_HASH_DEFAULT: H256 = H256(CODE_HASH_BYTES);

/// Turn an Ethereum address - a H160 - into a valid path
pub fn account_path(address: &H160) -> Result<OwnedPath, DurableStorageError> {
    let path_string = alloc::format!("/{}", hex::encode(address.to_fixed_bytes()));
    OwnedPath::try_from(path_string).map_err(DurableStorageError::from)
}

#[derive(Clone, Copy)]
pub enum StorageValue {
    Hit(H256),
    Default,
}

impl StorageValue {
    pub fn h256(self) -> H256 {
        match self {
            StorageValue::Hit(v) => v,
            StorageValue::Default => STORAGE_DEFAULT_VALUE,
        }
    }
}

pub fn u256_to_alloy(value: &U256) -> alloy_primitives::U256 {
    let mut bytes = [0u8; 32];
    value.to_little_endian(&mut bytes);
    alloy_primitives::U256::from_le_bytes::<32>(bytes)
}

impl EthereumAccount {
    pub fn from_address(address: &H160) -> Result<Self, DurableStorageError> {
        let path = concat(&EVM_ACCOUNTS_PATH, &account_path(address)?)?;
        Ok(path.into())
    }

    /// Get the **nonce** for the Ethereum account. Default value is zero, so an account will
    /// _always_ have this **nonce**.
    pub fn nonce(&self, host: &mut impl Runtime) -> Result<u64, AccountStorageError> {
        let new_format_account = StorageAccount::from_path(self.path.clone());
        Ok(new_format_account.info(host)?.nonce)
    }

    pub fn info(
        &self,
        host: &mut impl Runtime,
    ) -> Result<AccountInfo, AccountStorageError> {
        let new_format_account = StorageAccount::from_path(self.path.clone());
        new_format_account
            .info(host)
            .map_err(AccountStorageError::from)
    }

    pub fn set_info(
        &mut self,
        host: &mut impl Runtime,
        info: AccountInfo,
    ) -> Result<(), AccountStorageError> {
        let mut new_format_account = StorageAccount::from_path(self.path.clone());
        new_format_account
            .set_info(host, info)
            .map_err(AccountStorageError::from)
    }

    pub fn set_info_without_code(
        &mut self,
        host: &mut impl Runtime,
        info: AccountInfo,
    ) -> Result<(), AccountStorageError> {
        let mut new_format_account = StorageAccount::from_path(self.path.clone());
        new_format_account
            .set_info_without_code(host, info)
            .map_err(AccountStorageError::from)
    }

    /// Increment the **nonce** by one. It is technically possible for this operation to overflow,
    /// but in practice this will not happen for a very long time. The nonce is a 256 bit unsigned
    /// integer.
    pub fn increment_nonce(
        &mut self,
        host: &mut impl Runtime,
    ) -> Result<(), AccountStorageError> {
        let mut old_info = self.info(host)?;

        let new_value = old_info
            .nonce
            .checked_add(1)
            .ok_or(AccountStorageError::NonceOverflow)?;
        old_info.nonce = new_value;
        self.set_info_without_code(host, old_info)
    }

    pub fn set_nonce(
        &mut self,
        host: &mut impl Runtime,
        nonce: u64,
    ) -> Result<(), AccountStorageError> {
        let mut old_info = self.info(host)?;
        old_info.nonce = nonce;
        self.set_info_without_code(host, old_info)
    }

    /// Get the **balance** of an account in Wei held by the account.
    pub fn balance(&self, host: &mut impl Runtime) -> Result<U256, AccountStorageError> {
        let new_format_account = StorageAccount::from_path(self.path.clone());
        Ok(alloy_to_u256(&new_format_account.info(host)?.balance))
    }

    /// LEGACY: like `balance` but doesn't lazy migrate. Used for sputnikvm
    pub fn read_only_balance(
        &self,
        host: &impl Runtime,
    ) -> Result<U256, AccountStorageError> {
        let account = StorageAccount::from_path(self.path.clone());
        match account.info_without_migration(host) {
            Ok(Some(info)) => Ok(alloy_to_u256(&info.balance)),
            Ok(None) => {
                let balance_path = concat(&self.path, &BALANCE_PATH)?;
                read_u256_le_default(host, &balance_path, U256::zero())
                    .map_err(AccountStorageError::from)
            }
            Err(err) => Err(AccountStorageError::from(err)),
        }
    }

    /// Add an amount in Wei to the balance of an account. In theory, this can overflow if the
    /// final amount exceeds the range of a a 256 bit unsigned integer.
    pub fn balance_add(
        &mut self,
        host: &mut impl Runtime,
        amount: U256,
    ) -> Result<(), AccountStorageError> {
        let mut old_info = self.info(host)?;
        let new_value = old_info
            .balance
            .checked_add(u256_to_alloy(&amount))
            .ok_or(AccountStorageError::BalanceOverflow)?;
        old_info.balance = new_value;
        self.set_info_without_code(host, old_info)
    }

    /// Remove an amount in Wei from the balance of an account. If the account doesn't hold
    /// enough funds, this will underflow, in which case the account is unaffected, but the
    /// function call will return `Ok(false)`. In case the removal went without underflow,
    /// ie the account held enough funds, the function returns `Ok(true)`.
    pub fn balance_remove(
        &mut self,
        host: &mut impl Runtime,
        amount: U256,
    ) -> Result<bool, AccountStorageError> {
        let mut old_info = self.info(host)?;
        if let Some(new_value) = old_info.balance.checked_sub(u256_to_alloy(&amount)) {
            old_info.balance = new_value;
            self.set_info_without_code(host, old_info)?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// Set the balance of an account to an amount in Wei
    pub fn set_balance(
        &mut self,
        host: &mut impl Runtime,
        new_balance: U256,
    ) -> Result<(), AccountStorageError> {
        let mut old_info = self.info(host)?;
        old_info.balance = u256_to_alloy(&new_balance);
        self.set_info_without_code(host, old_info)
    }

    /// Get the path to a custom account state section, can be used by precompiles
    pub fn custom_path(
        &self,
        suffix: &impl Path,
    ) -> Result<OwnedPath, AccountStorageError> {
        concat(&self.path, suffix).map_err(AccountStorageError::from)
    }

    /// Get the path to an index in durable storage for an account.
    fn storage_path(&self, index: &H256) -> Result<OwnedPath, AccountStorageError> {
        let storage_path = concat(&self.path, &STORAGE_ROOT_PATH)?;
        let index_path = path_from_h256(index)?;
        concat(&storage_path, &index_path).map_err(AccountStorageError::from)
    }

    /// Clear the entire storage in durable storage for an account.
    pub fn clear_storage(
        &self,
        host: &mut impl Runtime,
    ) -> Result<(), AccountStorageError> {
        let storage_path = concat(&self.path, &STORAGE_ROOT_PATH)?;
        if host.store_has(&storage_path)?.is_some() {
            host.store_delete(&storage_path)
                .map_err(AccountStorageError::from)?
        };
        Ok(())
    }

    /// Get the value stored in contract permanent storage at a given index for an account.
    pub fn get_storage(
        &self,
        host: &impl Runtime,
        index: &H256,
    ) -> Result<H256, AccountStorageError> {
        let path = self.storage_path(index)?;
        read_h256_be_default(host, &path, STORAGE_DEFAULT_VALUE)
            .map_err(AccountStorageError::from)
    }

    pub fn read_storage(
        &self,
        host: &impl Runtime,
        index: &H256,
    ) -> Result<StorageValue, AccountStorageError> {
        let path = self.storage_path(index)?;
        let res = read_h256_be_opt(host, &path).map_err(AccountStorageError::from)?;

        match res {
            Some(v) => Ok(StorageValue::Hit(v)),
            None => Ok(StorageValue::Default),
        }
    }

    /// Set the value associated with an index in durable storage. The result depends on the
    /// values being stored. It tracks whether the update went from default to non-default,
    /// non-default to default, et.c. This is for the purpose of calculating gas cost.
    pub fn set_storage_checked(
        &mut self,
        host: &mut impl Runtime,
        index: &H256,
        value: &H256,
    ) -> Result<StorageEffect, AccountStorageError> {
        let path = self.storage_path(index)?;

        let old_value = self.get_storage(host, index)?;

        let from_default = old_value == STORAGE_DEFAULT_VALUE;
        let to_default = *value == STORAGE_DEFAULT_VALUE;

        if !from_default && to_default {
            host.store_delete(&path)?;
        }

        if !to_default {
            let value_bytes = value.to_fixed_bytes();

            host.store_write_all(&path, &value_bytes)?;
        }

        Ok(StorageEffect {
            from_default,
            to_default,
        })
    }

    /// Set the value associated with an index in durable storage. The result depends on the
    /// values being stored. This function does no tracking for the purpose of gas cost.
    pub fn set_storage(
        &mut self,
        host: &mut impl Runtime,
        index: &H256,
        value: &H256,
    ) -> Result<(), AccountStorageError> {
        let path = self.storage_path(index)?;

        let value_bytes = value.to_fixed_bytes();

        host.store_write_all(&path, &value_bytes)
            .map_err(AccountStorageError::from)
    }

    /// Find whether the account has any code associated with it.
    pub fn code_exists(
        &self,
        host: &mut impl Runtime,
    ) -> Result<bool, AccountStorageError> {
        Ok(self.info(host)?.code_hash != KECCAK_EMPTY)
    }

    /// Get the contract code associated with a contract. A contract can have zero length
    /// contract code associated with it - this is the same for "external" and un-used
    /// accounts. First check if code is located in this storage, if that fails try to
    /// retrieve it within the code_storage module.
    // There is a possibility here to migrate lazily all code in order
    // to have all legacy contract uses the new code storage.
    pub fn code(&self, host: &mut impl Runtime) -> Result<Vec<u8>, AccountStorageError> {
        let path = concat(&self.path, &CODE_PATH)?;

        // If we ever do the lazy migration of code for account
        // (i.e. moving code from account to code_storage) then this
        // would be dead code.
        match host.store_read_all(&path) {
            Ok(bytes) => Ok(bytes),
            Err(RuntimeError::PathNotFound) => {
                let code_hash = self.code_hash(host)?;
                code_storage::CodeStorage::get_code(host, &code_hash).map_err(Into::into)
            }
            Err(err) => Err(AccountStorageError::from(err)),
        }
    }

    /// Get the hash of the code associated with an account. This value is computed and
    /// stored when the code of a contract is set.
    pub fn code_hash(
        &self,
        host: &mut impl Runtime,
    ) -> Result<H256, AccountStorageError> {
        let info = self.info(host)?;
        Ok(H256::from_slice(&info.code_hash.0))
    }

    /// Get the size of a contract in number of bytes used for opcodes. This value is
    /// computed and stored when the code of a contract is set.
    pub fn code_size(
        &self,
        host: &mut impl Runtime,
    ) -> Result<U256, AccountStorageError> {
        let path = concat(&self.path, &CODE_PATH)?;
        // If we ever do the lazy migration of code for account
        // (i.e. moving code from account to code_storage) then this
        // would be dead code.
        if host.store_has(&path)? == Some(ValueType::Value) {
            let size = host.store_value_size(&path)?;
            Ok(size.into())
        } else {
            let code_hash = self.code_hash(host)?;
            code_storage::CodeStorage::code_size(host, &code_hash).map_err(Into::into)
        }
    }

    /// Set the code associated with an account. This stores the code and also computes
    /// hash and size and stores those values as well. No check for validity of contract
    /// code is done. Contract code is validated through execution (contract calls), and
    /// not before.
    pub fn set_code(
        &mut self,
        host: &mut impl Runtime,
        code: &[u8],
    ) -> Result<(), AccountStorageError> {
        if self.code_exists(host)? {
            Err(AccountStorageError::AccountCodeAlreadySet)
        } else {
            let code_hash = code_storage::CodeStorage::add(host, code)?;
            let mut old_info = self.info(host)?;
            old_info.code_hash = B256::from_slice(code_hash.as_bytes());
            self.set_info(host, old_info)?;
            Ok(())
        }
    }

    /// Delete all code associated with a contract. Also sets code length and size accordingly
    pub fn delete_code(
        &mut self,
        host: &mut impl Runtime,
    ) -> Result<(), AccountStorageError> {
        let mut info = self.info(host)?;
        if info.code_hash == KECCAK_EMPTY {
            // No code to delete
            return Ok(());
        }
        code_storage::CodeStorage::delete(host, &H256::from_slice(&info.code_hash.0))?;
        info.code_hash = KECCAK_EMPTY;
        self.set_info(host, info)?;
        Ok(())
    }
}

/// The type of the storage API for accessing the Ethereum World State.
pub type EthereumAccountStorage = Storage<EthereumAccount>;

/// Get the storage API for accessing the Ethereum World State and do transactions
/// on it.
pub fn init_account_storage() -> Result<EthereumAccountStorage, AccountStorageError> {
    Storage::<EthereumAccount>::init(&EVM_ACCOUNTS_PATH)
        .map_err(AccountStorageError::from)
}

#[cfg(test)]
mod test {
    use super::*;
    use host::path::RefPath;
    use primitive_types::U256;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_host::runtime::Runtime as SdkRuntime; // Used for
    use tezos_storage::helpers::bytes_hash;
    use tezos_storage::{read_u256_le_default, write_u256_le};

    #[test]
    fn test_account_nonce_update() {
        let mut host = MockKernelHost::default();
        let mut storage =
            init_account_storage().expect("Could not create EVM accounts storage API");

        let a1_path = RefPath::assert_from(b"/asdf");

        // Act
        storage
            .begin_transaction(&mut host)
            .expect("Could not begin transaction");

        let mut a1 = storage
            .create_new(&mut host, &a1_path)
            .expect("Could not create new account")
            .expect("Account already exists in storage");

        assert_eq!(
            a1.nonce(&mut host)
                .expect("Could not get nonce for account"),
            0
        );

        a1.increment_nonce(&mut host)
            .expect("Could not increment nonce");

        storage
            .commit_transaction(&mut host)
            .expect("Could not commit transaction");

        // Assert
        let a1 = storage
            .get(&host, &a1_path)
            .expect("Could not get account")
            .expect("Account does not exist");

        assert_eq!(
            a1.nonce(&mut host)
                .expect("Could not get nonce for account"),
            1
        );
    }

    #[test]
    fn test_zero_account_balance_for_new_accounts() {
        let mut host = MockKernelHost::default();
        let mut storage =
            init_account_storage().expect("Could not create EVM accounts storage API");

        let a1_path = RefPath::assert_from(b"/dfkjd");

        // Act - create an account with no funds
        storage
            .begin_transaction(&mut host)
            .expect("Could not begin transaction");

        let mut a1 = storage
            .create_new(&mut host, &a1_path)
            .expect("Could not create new account")
            .expect("Account already exists");

        a1.increment_nonce(&mut host)
            .expect("Could not increment nonce");

        storage
            .commit_transaction(&mut host)
            .expect("Could not commit transaction");

        // Assert
        let a1 = storage
            .get(&host, &a1_path)
            .expect("Could not get account from storage")
            .expect("Account does not exist");

        assert_eq!(
            a1.balance(&mut host)
                .expect("Could not get balance for account"),
            U256::zero()
        );
    }

    #[test]
    fn test_account_balance_add() {
        let mut host = MockKernelHost::default();
        let mut storage =
            init_account_storage().expect("Could not create EVM accounts storage API");

        let a1_path = RefPath::assert_from(b"/dfkjd");

        let v1: U256 = 17_u32.into();
        let v2: U256 = 119_u32.into();
        let v3: U256 = v1 + v2;

        // Act - create an account with no funds
        storage
            .begin_transaction(&mut host)
            .expect("Could not begin transaction");

        let mut a1 = storage
            .create_new(&mut host, &a1_path)
            .expect("Could not create new account")
            .expect("Account already exists");

        a1.balance_add(&mut host, v1)
            .expect("Could not add first value to balance");
        a1.balance_add(&mut host, v2)
            .expect("Could not add second value to balance");

        storage
            .commit_transaction(&mut host)
            .expect("Could not commit transaction");

        // Assert
        let a1 = storage
            .get(&host, &a1_path)
            .expect("Could not get account from storage")
            .expect("Account does not exist");

        assert_eq!(
            a1.balance(&mut host)
                .expect("Could not get balance for account"),
            v3
        );
    }

    #[test]
    fn test_account_balance_sub() {
        let mut host = MockKernelHost::default();
        let mut storage =
            init_account_storage().expect("Could not create EVM accounts storage API");

        let a1_path = RefPath::assert_from(b"/dfkjd");

        let v1: U256 = 170_u32.into();
        let v2: U256 = 19_u32.into();
        let v3: U256 = v1 - v2;

        // Act - create an account with no funds
        storage
            .begin_transaction(&mut host)
            .expect("Could not begin transaction");

        let mut a1 = storage
            .create_new(&mut host, &a1_path)
            .expect("Could not create new account")
            .expect("Account already exists");

        a1.balance_add(&mut host, v1)
            .expect("Could not add first value to balance");
        a1.balance_remove(&mut host, v2)
            .expect("Could not add second value to balance");

        storage
            .commit_transaction(&mut host)
            .expect("Could not commit transaction");

        // Assert
        let a1 = storage
            .get(&host, &a1_path)
            .expect("Could not get account from storage")
            .expect("Account does not exist");

        assert_eq!(
            a1.balance(&mut host)
                .expect("Could not get balance for account"),
            v3
        );
    }

    #[test]
    fn test_account_balance_underflow() {
        let mut host = MockKernelHost::default();
        let mut storage =
            init_account_storage().expect("Could not create EVM accounts storage API");

        let a1_path = RefPath::assert_from(b"/dfkjd");

        let v1: U256 = 17_u32.into();
        let v2: U256 = 190_u32.into();

        // Act - create an account with no funds
        storage
            .begin_transaction(&mut host)
            .expect("Could not begin transaction");

        let mut a1 = storage
            .create_new(&mut host, &a1_path)
            .expect("Could not create new account")
            .expect("Account already exists");

        a1.balance_add(&mut host, v1)
            .expect("Could not add first value to balance");
        assert_eq!(a1.balance_remove(&mut host, v2), Ok(false),);

        storage
            .commit_transaction(&mut host)
            .expect("Could not commit transaction");

        // Assert
        let a1 = storage
            .get(&host, &a1_path)
            .expect("Could not get account from storage")
            .expect("Account does not exist");

        assert_eq!(
            a1.balance(&mut host)
                .expect("Could not get balance for account"),
            v1
        );
    }

    #[test]
    fn test_account_storage_zero_default() {
        let mut host = MockKernelHost::default();
        let mut storage =
            init_account_storage().expect("Could not create EVM accounts storage API");

        let a1_path = RefPath::assert_from(b"/dfkjd");

        let addr: H256 = H256::from_low_u64_be(17_u64);

        // Act - create an account with no funds
        storage
            .begin_transaction(&mut host)
            .expect("Could not begin transaction");

        let a1 = storage
            .create_new(&mut host, &a1_path)
            .expect("Could not create new account")
            .expect("Account already exists");

        assert_eq!(
            a1.get_storage(&host, &addr)
                .expect("Could not read storage for account"),
            H256::zero()
        );
    }

    #[test]
    fn test_account_storage_update() {
        let mut host = MockKernelHost::default();
        let mut storage =
            init_account_storage().expect("Could not create EVM accounts storage API");

        let a1_path = RefPath::assert_from(b"/dfkjd");

        let addr: H256 = H256::from_low_u64_be(17_u64);
        let v: H256 = H256::from_low_u64_be(190_u64);

        // Act - create an account with no funds
        storage
            .begin_transaction(&mut host)
            .expect("Could not begin transaction");

        let mut a1 = storage
            .create_new(&mut host, &a1_path)
            .expect("Could not create new account")
            .expect("Account already exists");

        a1.set_storage(&mut host, &addr, &v)
            .expect("Could not update account storage");

        storage
            .commit_transaction(&mut host)
            .expect("Could not commit transaction");

        // Assert
        let a1 = storage
            .get(&host, &a1_path)
            .expect("Could not get account from storage")
            .expect("Account does not exist");

        assert_eq!(
            a1.get_storage(&host, &addr)
                .expect("Could not read storage for account"),
            v
        );
    }

    #[test]
    fn test_account_storage_update_checked() {
        let mut host = MockKernelHost::default();
        let mut storage =
            init_account_storage().expect("Could not create EVM accounts storage API");

        let a1_path = RefPath::assert_from(b"/dfkjd");

        let addr: H256 = H256::from_low_u64_be(17_u64);
        let v1: H256 = H256::from_low_u64_be(191_u64);
        let v2: H256 = H256::from_low_u64_be(192_u64);
        let v3: H256 = H256::zero();

        // Act - create an account with no funds
        storage
            .begin_transaction(&mut host)
            .expect("Could not begin transaction");

        let mut a1 = storage
            .create_new(&mut host, &a1_path)
            .expect("Could not create new account")
            .expect("Account already exists");

        assert_eq!(
            a1.set_storage_checked(&mut host, &addr, &v1)
                .expect("Could not update account storage"),
            StorageEffect {
                from_default: true,
                to_default: false
            }
        );
        assert_eq!(
            a1.set_storage_checked(&mut host, &addr, &v2)
                .expect("Could not update account storage"),
            StorageEffect {
                from_default: false,
                to_default: false
            }
        );
        assert_eq!(
            a1.set_storage_checked(&mut host, &addr, &v3)
                .expect("Could not update account storage"),
            StorageEffect {
                from_default: false,
                to_default: true
            }
        );
        assert_eq!(
            a1.set_storage_checked(&mut host, &addr, &v3)
                .expect("Could not update account storage"),
            StorageEffect {
                from_default: true,
                to_default: true
            }
        );
        assert_eq!(
            a1.set_storage_checked(&mut host, &addr, &v1)
                .expect("Could not update account storage"),
            StorageEffect {
                from_default: true,
                to_default: false
            }
        );

        storage
            .commit_transaction(&mut host)
            .expect("Could not commit transaction");

        // Assert
        let a1 = storage
            .get(&host, &a1_path)
            .expect("Could not get account from storage")
            .expect("Account does not exist");

        assert_eq!(
            a1.get_storage(&host, &addr)
                .expect("Could not read storage for account"),
            v1
        );
    }

    #[test]
    fn test_account_code_storage_initial_code_is_zero() {
        let mut host = MockKernelHost::default();
        let mut storage =
            init_account_storage().expect("Could not create EVM accounts storage API");

        let a1_path = RefPath::assert_from(b"/asdf");

        // Act - make sure there is an account
        storage
            .begin_transaction(&mut host)
            .expect("Could not begin transaction");

        let mut a1 = storage
            .create_new(&mut host, &a1_path)
            .expect("Could not create new account")
            .expect("Account already exists in storage");

        assert_eq!(
            a1.nonce(&mut host)
                .expect("Could not get nonce for account"),
            0
        );

        a1.increment_nonce(&mut host)
            .expect("Could not increment nonce");

        storage
            .commit_transaction(&mut host)
            .expect("Could not commit transaction");

        // Assert
        let a1 = storage
            .get(&host, &a1_path)
            .expect("Could not get account")
            .expect("Account does not exist");

        assert_eq!(
            a1.code(&mut host).expect("Could not get code for account"),
            Vec::<u8>::new()
        );
        assert_eq!(
            a1.code_size(&mut host)
                .expect("Could not get code size for account"),
            U256::zero()
        );
        assert_eq!(
            a1.code_hash(&mut host)
                .expect("Could not get code hash for account"),
            CODE_HASH_DEFAULT
        );
    }

    fn test_account_code_storage_write_code_aux(sample_code: Vec<u8>) {
        let mut host = MockKernelHost::default();
        let mut storage =
            init_account_storage().expect("Could not create EVM accounts storage API");

        let a1_path = RefPath::assert_from(b"/asdf");
        let sample_code_hash: H256 = bytes_hash(&sample_code);

        // Act
        storage
            .begin_transaction(&mut host)
            .expect("Could not begin transaction");

        let mut a1 = storage
            .create_new(&mut host, &a1_path)
            .expect("Could not create new account")
            .expect("Account already exists in storage");

        a1.set_code(&mut host, &sample_code)
            .expect("Could not write code to account");

        storage
            .commit_transaction(&mut host)
            .expect("Could not commit transaction");

        // Assert
        let a1 = storage
            .get(&host, &a1_path)
            .expect("Could not get account")
            .expect("Account does not exist");

        assert_eq!(
            a1.code(&mut host).expect("Could not get code for account"),
            sample_code
        );
        assert_eq!(
            a1.code_size(&mut host)
                .expect("Could not get code size for account"),
            sample_code.len().into()
        );
        assert_eq!(
            a1.code_hash(&mut host)
                .expect("Could not get code hash for account"),
            sample_code_hash
        );
    }

    #[test]
    fn test_account_code_storage_write_code() {
        let sample_code: Vec<u8> = (0..100).collect();
        test_account_code_storage_write_code_aux(sample_code)
    }

    #[test]
    fn test_account_code_storage_write_big_code() {
        let sample_code: Vec<u8> = vec![1; 10000];
        test_account_code_storage_write_code_aux(sample_code)
    }

    #[test]
    fn test_account_code_storage_cant_be_overwritten() {
        let mut host = MockKernelHost::default();
        let mut storage =
            init_account_storage().expect("Could not create EVM accounts storage API");

        let a1_path = RefPath::assert_from(b"/asdf");
        let sample_code1: Vec<u8> = (0..100).collect();
        let sample_code1_hash: H256 = bytes_hash(&sample_code1);
        let sample_code2: Vec<u8> = (0..50).map(|x| 50 - x).collect();

        // Act
        storage
            .begin_transaction(&mut host)
            .expect("Could not begin transaction");

        let mut a1 = storage
            .create_new(&mut host, &a1_path)
            .expect("Could not create new account")
            .expect("Account already exists in storage");

        a1.set_code(&mut host, &sample_code1)
            .expect("Could not write code to account");
        a1.set_code(&mut host, &sample_code2)
            .expect_err("Account storage code can't be overwritten");

        storage
            .commit_transaction(&mut host)
            .expect("Could not commit transaction");

        // Assert
        let a1 = storage
            .get(&host, &a1_path)
            .expect("Could not get account")
            .expect("Account does not exist");

        assert_eq!(
            a1.code(&mut host).expect("Could not get code for account"),
            sample_code1
        );
        assert_eq!(
            a1.code_size(&mut host)
                .expect("Could not get code size for account"),
            sample_code1.len().into()
        );
        assert_eq!(
            a1.code_hash(&mut host)
                .expect("Could not get code hash for account"),
            sample_code1_hash
        );
    }

    #[test]
    fn test_account_code_storage_delete_code() {
        let mut host = MockKernelHost::default();
        let mut storage =
            init_account_storage().expect("Could not create EVM accounts storage API");

        let a1_path = RefPath::assert_from(b"/asdf");
        let sample_code: Vec<u8> = (0..100).collect();

        // Act
        storage
            .begin_transaction(&mut host)
            .expect("Could not begin transaction");

        let mut a1 = storage
            .create_new(&mut host, &a1_path)
            .expect("Could not create new account")
            .expect("Account already exists in storage");

        a1.increment_nonce(&mut host)
            .expect("Could not increment nonce");

        a1.set_code(&mut host, &sample_code)
            .expect("Could not write code to account");

        a1.delete_code(&mut host)
            .expect("Could not delete code for contract");

        storage
            .commit_transaction(&mut host)
            .expect("Could not commit transaction");

        // Assert
        let a1 = storage
            .get(&host, &a1_path)
            .expect("Could not get account")
            .expect("Account does not exist");

        assert_eq!(
            a1.code_hash(&mut host)
                .expect("Could not get code hash for account"),
            CODE_HASH_DEFAULT
        );
        assert_eq!(
            a1.code(&mut host).expect("Could not get code for account"),
            Vec::<u8>::new()
        );
        assert_eq!(
            a1.code_size(&mut host)
                .expect("Could not get code size for account"),
            U256::zero()
        );
    }

    #[test]
    fn test_empty_contract_hash_matches_default() {
        let mut host = MockKernelHost::default();
        let mut storage =
            init_account_storage().expect("Could not create EVM accounts storage API");

        let a1_path = RefPath::assert_from(b"/asdf");
        let sample_code: Vec<u8> = vec![];
        let sample_code_hash: H256 = CODE_HASH_DEFAULT;

        // Act
        storage
            .begin_transaction(&mut host)
            .expect("Could not begin transaction");

        let mut a1 = storage
            .create_new(&mut host, &a1_path)
            .expect("Could not create new account")
            .expect("Account already exists in storage");

        a1.set_code(&mut host, &sample_code)
            .expect("Could not write code to account");

        storage
            .commit_transaction(&mut host)
            .expect("Could not commit transaction");

        // Assert
        let a1 = storage
            .get(&host, &a1_path)
            .expect("Could not get account")
            .expect("Account does not exist");

        assert_eq!(
            a1.code(&mut host).expect("Could not get code for account"),
            sample_code
        );
        assert_eq!(
            a1.code_size(&mut host)
                .expect("Could not get code size for account"),
            sample_code.len().into()
        );
        assert_eq!(
            a1.code_hash(&mut host)
                .expect("Could not get code hash for account"),
            sample_code_hash
        );
    }

    #[test]
    fn test_read_u256_le_default_le() {
        let mut host = MockKernelHost::default();
        let path = RefPath::assert_from(b"/value");
        assert_eq!(
            read_u256_le_default(&host, &path, U256::from(128)).unwrap(),
            U256::from(128)
        );

        host.store_write_all(&path, &[1u8; 20]).unwrap();
        assert_eq!(
            read_u256_le_default(&host, &path, U256::zero()).unwrap(),
            U256::zero()
        );

        host.store_write_all(
            &path,
            &hex::decode(
                "ff00000000000000000000000000000000000000000000000000000000000000",
            )
            .unwrap(),
        )
        .unwrap();
        assert_eq!(
            read_u256_le_default(&host, &path, U256::zero()).unwrap(),
            U256::from(255)
        );
    }

    #[test]
    fn test_write_u256_le_le() {
        let mut host = MockKernelHost::default();
        let path = RefPath::assert_from(b"/value");

        write_u256_le(&mut host, &path, U256::from(255)).unwrap();
        assert_eq!(
            hex::encode(host.store_read(&path, 0, 32).unwrap()),
            "ff00000000000000000000000000000000000000000000000000000000000000"
        );
    }

    #[test]
    fn test_account_code_storage_can_still_be_addressed_if_exists() {
        const CODE_HASH_PATH: RefPath = RefPath::assert_from(b"/code.hash");
        let sample_code: Vec<u8> = (0..100).collect();
        let sample_code_hash: H256 = bytes_hash(&sample_code);

        let mut host = MockKernelHost::default();
        let mut storage =
            init_account_storage().expect("Could not create EVM accounts storage API");

        let a1_path = RefPath::assert_from(b"/asdf");

        // Act
        storage
            .begin_transaction(&mut host)
            .expect("Could not begin transaction");

        let a1 = storage
            .create_new(&mut host, &a1_path)
            .expect("Could not create new account")
            .expect("Account already exists in storage");

        let code_hash_bytes: [u8; WORD_SIZE] = sample_code_hash.into();
        let code_hash_path =
            concat(&a1.path, &CODE_HASH_PATH).expect("Could not get code hash path");

        host.store_write_all(&code_hash_path, &code_hash_bytes)
            .expect("Could not write code hash into code hash path");

        let code_path = concat(&a1.path, &CODE_PATH).expect("Could not get code path");

        host.store_write_all(&code_path, &sample_code)
            .expect("Could not write code into code path");

        storage
            .commit_transaction(&mut host)
            .expect("Could not commit transaction");

        // Assert
        let a1 = storage
            .get(&host, &a1_path)
            .expect("Could not get account")
            .expect("Account does not exist");

        let code_path = concat(&a1.path, &CODE_PATH).expect("Could not get code path");

        let code = host
            .store_read_all(&code_path)
            .expect("Could not read code from code path");

        assert_eq!(code, sample_code);

        assert_eq!(
            a1.code(&mut host).expect("Could not get code for account"),
            sample_code
        );
        assert_eq!(
            a1.code_size(&mut host)
                .expect("Could not get code size for account"),
            sample_code.len().into()
        );
        assert_eq!(
            a1.code_hash(&mut host)
                .expect("Could not get code hash for account"),
            sample_code_hash
        );
    }

    #[test]
    fn test_code_is_deleted() {
        let sample_code: Vec<u8> = (0..100).collect();
        let sample_code_hash: H256 = bytes_hash(&sample_code);

        let mut host = MockKernelHost::default();
        let mut storage =
            init_account_storage().expect("Could not create EVM accounts storage API");

        let a1_path = RefPath::assert_from(b"/account1");

        storage
            .begin_transaction(&mut host)
            .expect("Could not begin transaction");

        let mut a1 = storage
            .create_new(&mut host, &a1_path)
            .expect("Could not create new account")
            .expect("Account already exists in storage");

        a1.set_code(&mut host, &sample_code)
            .expect("Could not write code to account");

        storage
            .commit_transaction(&mut host)
            .expect("Could not commit transaction");

        let a2_path = RefPath::assert_from(b"/account2");

        storage
            .begin_transaction(&mut host)
            .expect("Could not begin transaction");

        let mut a2 = storage
            .create_new(&mut host, &a2_path)
            .expect("Could not create new account")
            .expect("Account already exists in storage");

        a2.set_code(&mut host, &sample_code)
            .expect("Could not write code to account");

        a2.delete_code(&mut host)
            .expect("Could not delete code for contract");

        assert_eq!(
            a2.code_hash(&mut host)
                .expect("Could not get code hash for account"),
            CODE_HASH_DEFAULT
        );

        assert_eq!(
            a2.code(&mut host).expect("Could not get code for account"),
            Vec::<u8>::new()
        );
        assert_eq!(
            a2.code_size(&mut host)
                .expect("Could not get code size for account"),
            U256::zero()
        );

        assert_eq!(
            a1.code(&mut host).expect("Could not get code for account"),
            sample_code
        );

        assert_eq!(
            a1.code_size(&mut host)
                .expect("Could not get code size for account"),
            sample_code.len().into()
        );

        assert_eq!(
            a1.code_hash(&mut host)
                .expect("Could not get code hash for account"),
            sample_code_hash
        );
    }
}
