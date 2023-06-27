// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Ethereum account state and storage

use const_decoder::Decoder;
use host::path::{concat, OwnedPath, Path, RefPath};
use host::runtime::{Runtime, RuntimeError, ValueType};
use primitive_types::{H160, H256, U256};
use sha3::{Digest, Keccak256};
use tezos_smart_rollup_storage::storage::Storage;
use thiserror::Error;

/// The size of one 256 bit word. Size in bytes
pub const WORD_SIZE: usize = 32_usize;

/// All errors that may happen as result of using the Ethereum account
/// interface.
#[derive(Error, Eq, PartialEq, Clone, Debug)]
pub enum AccountStorageError {
    /// Some runtime error happened while using durable storage
    #[error("Runtime error: {0:?}")]
    RuntimeError(host::runtime::RuntimeError),
    /// Some error happened while constructing the path to some
    /// resource.
    #[error("Path error: {0:?}")]
    PathError(host::path::PathError),
    /// Some error occurred while using the transaction storage
    /// API.
    #[error("Transaction storage API error: {0:?}")]
    StorageError(tezos_smart_rollup_storage::StorageError),
    /// Some account balance became greater than what can be
    /// stored in an unsigned 256 bit integer.
    #[error("Account balance overflow")]
    BalanceOverflow,
    /// Some account balance became less than zero. Only positive
    /// integers in 256 bit range can be stored. Error parameter is
    /// the string representation of the path to the account where
    /// underflow happened.
    #[error("Account balance underflow")]
    BalanceUnderflow(String),
    /// Technically, the Ethereum account nonce can overflow if
    /// an account does an incredible number of transactions.
    #[error("Nonce overflow")]
    NonceOverflow,
}

impl From<tezos_smart_rollup_storage::StorageError> for AccountStorageError {
    fn from(error: tezos_smart_rollup_storage::StorageError) -> Self {
        AccountStorageError::StorageError(error)
    }
}

impl From<host::path::PathError> for AccountStorageError {
    fn from(error: host::path::PathError) -> Self {
        AccountStorageError::PathError(error)
    }
}

impl From<host::runtime::RuntimeError> for AccountStorageError {
    fn from(error: host::runtime::RuntimeError) -> Self {
        AccountStorageError::RuntimeError(error)
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
    path: OwnedPath,
}

impl From<OwnedPath> for EthereumAccount {
    fn from(path: OwnedPath) -> Self {
        Self { path }
    }
}

/// Path where Ethereum accounts are stored
const EVM_ACCOUNTS_PATH: RefPath = RefPath::assert_from(b"/eth_accounts");

/// Path where an account nonce is stored. This should be prefixed with the path to
/// where the account is stored for the world state or for the current transaction.
const NONCE_PATH: RefPath = RefPath::assert_from(b"/nonce");

/// Path where an account balance, ether held, is stored. This should be prefixed with the path to
/// where the account is stored for the world state or for the current transaction.
const BALANCE_PATH: RefPath = RefPath::assert_from(b"/balance");

/// "Internal" accounts - accounts with contract code have a contract code hash.
/// This value is computed when the code is stored and kept for future queries. This
/// path should be prefixed with the path to
/// where the account is stored for the world state or for the current transaction.
const CODE_HASH_PATH: RefPath = RefPath::assert_from(b"/code.hash");

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

/// Read a single unsigned 256 bit value from storage at the path given.
fn read_u256(
    host: &impl Runtime,
    path: &impl Path,
) -> Result<Option<U256>, AccountStorageError> {
    let bytes = host.store_read(path, 0, WORD_SIZE)?;

    if bytes.len() != WORD_SIZE {
        Ok(None)
    } else {
        Ok(Some(U256::from_little_endian(&bytes)))
    }
}

/// Read a single 256 bit hash from storage at the path given.
fn read_h256(
    host: &impl Runtime,
    path: &impl Path,
) -> Result<Option<H256>, AccountStorageError> {
    let bytes = host.store_read(path, 0, WORD_SIZE)?;

    if bytes.len() != WORD_SIZE {
        Ok(None)
    } else {
        Ok(Some(H256::from_slice(&bytes)))
    }
}

/// Get the path corresponding to an index of H256. This is used to
/// find the path to a value a contract stores in durable storage.
fn path_from_h256(index: &H256) -> Result<OwnedPath, AccountStorageError> {
    let path_string = alloc::format!("/{}", hex::encode(index.to_fixed_bytes()));
    OwnedPath::try_from(path_string).map_err(AccountStorageError::from)
}

/// Compute Keccak 256 for some bytes
fn bytes_hash(bytes: &[u8]) -> H256 {
    H256(Keccak256::digest(bytes).into())
}

/// Turn an Ethereum address - a H160 - into a valid path
pub fn account_path(address: &H160) -> Result<OwnedPath, AccountStorageError> {
    let path_string = alloc::format!("/{}", hex::encode(address.to_fixed_bytes()));
    OwnedPath::try_from(path_string).map_err(AccountStorageError::from)
}
impl EthereumAccount {
    /// Get the **nonce** for the Ethereum account. Default value is zero, so an account will
    /// _always_ have this **nonce**.
    pub fn nonce(&self, host: &impl Runtime) -> Result<U256, AccountStorageError> {
        let path = concat(&self.path, &NONCE_PATH)?;

        match host.store_has(&path) {
            Ok(Some(ValueType::Value | ValueType::ValueWithSubtree)) => {
                let value = read_u256(host, &path)?;
                Ok(value.unwrap_or(U256::zero()))
            }
            Ok(_) => Ok(U256::zero()),
            Err(err) => Err(AccountStorageError::from(err)),
        }
    }

    /// Increment the **nonce** by one. It is technically possible for this operation to overflow,
    /// but in practice this will not happen for a very long time. The nonce is a 256 bit unsigned
    /// integer.
    pub fn increment_nonce(
        &mut self,
        host: &mut impl Runtime,
    ) -> Result<(), AccountStorageError> {
        let path = concat(&self.path, &NONCE_PATH)?;

        let old_value = self.nonce(host)?;

        let new_value = old_value
            .checked_add(U256::one())
            .ok_or(AccountStorageError::NonceOverflow)?;

        let mut new_value_bytes: [u8; WORD_SIZE] = [0; WORD_SIZE];
        new_value.to_little_endian(&mut new_value_bytes);

        host.store_write(&path, &new_value_bytes, 0)
            .map_err(AccountStorageError::from)
    }

    /// Get the **balance** of an account in Wei held by the account.
    pub fn balance(&self, host: &impl Runtime) -> Result<U256, AccountStorageError> {
        let path = concat(&self.path, &BALANCE_PATH)?;

        match host.store_has(&path) {
            Ok(Some(ValueType::Value | ValueType::ValueWithSubtree)) => {
                let value = read_u256(host, &path)?;
                Ok(value.unwrap_or(U256::zero()))
            }
            Ok(_) => Ok(U256::zero()),
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
        let path = concat(&self.path, &BALANCE_PATH)?;

        let value = self.balance(host)?;

        if let Some(new_value) = value.checked_add(amount) {
            let mut new_value_bytes: [u8; WORD_SIZE] = [0; WORD_SIZE];
            new_value.to_little_endian(&mut new_value_bytes);

            host.store_write(&path, &new_value_bytes, 0)
                .map_err(AccountStorageError::from)
        } else {
            Err(AccountStorageError::BalanceOverflow)
        }
    }

    /// Remove an amount in Wei from the balance of an account. If the account doesn't hold
    /// enough funds, this will underflow, in which case the account is unaffected, but the
    /// function call will return an error.
    pub fn balance_remove(
        &mut self,
        host: &mut impl Runtime,
        amount: U256,
    ) -> Result<(), AccountStorageError> {
        let path = concat(&self.path, &BALANCE_PATH)?;

        let value = self.balance(host)?;

        if let Some(new_value) = value.checked_sub(amount) {
            let mut new_value_bytes: [u8; WORD_SIZE] = [0; WORD_SIZE];
            new_value.to_little_endian(&mut new_value_bytes);

            host.store_write(&path, &new_value_bytes, 0)
                .map_err(AccountStorageError::from)
        } else {
            Err(AccountStorageError::BalanceUnderflow(self.path.to_string()))
        }
    }

    /// Get the path to an index in durable storage for an account.
    fn storage_path(&self, index: &H256) -> Result<OwnedPath, AccountStorageError> {
        let storage_path = concat(&self.path, &STORAGE_ROOT_PATH)?;
        let index_path = path_from_h256(index)?;
        concat(&storage_path, &index_path).map_err(AccountStorageError::from)
    }

    /// Get the value stored in contract permanent storage at a given index for an account.
    pub fn get_storage(
        &self,
        host: &impl Runtime,
        index: &H256,
    ) -> Result<H256, AccountStorageError> {
        let path = self.storage_path(index)?;

        match host.store_has(&path)? {
            Some(ValueType::Value | ValueType::ValueWithSubtree) => {
                let value = read_h256(host, &path)?;
                Ok(value.unwrap_or(STORAGE_DEFAULT_VALUE))
            }
            _ => Ok(STORAGE_DEFAULT_VALUE),
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

            host.store_write(&path, &value_bytes, 0)?;
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

        host.store_write(&path, &value_bytes, 0)
            .map_err(AccountStorageError::from)
    }

    /// Get the contract code associated with a contract. A contract can have zero length
    /// contract code associated with it - this is the same for "external" and un-used
    /// accounts.
    pub fn code(&self, host: &impl Runtime) -> Result<Vec<u8>, AccountStorageError> {
        let path = concat(&self.path, &CODE_PATH)?;

        match host.store_has(&path) {
            Ok(Some(ValueType::Value | ValueType::ValueWithSubtree)) => host
                .store_read_all(&path)
                .map_err(AccountStorageError::from),
            Ok(_) => Ok(vec![]),
            Err(err) => Err(AccountStorageError::from(err)),
        }
    }

    /// Get the hash of the code associated with an account. This value is computed and
    /// stored when the code of a contract is set.
    pub fn code_hash(&self, host: &impl Runtime) -> Result<H256, AccountStorageError> {
        let path = concat(&self.path, &CODE_HASH_PATH)?;

        match host.store_has(&path) {
            Ok(Some(ValueType::Value | ValueType::ValueWithSubtree)) => {
                let value = read_h256(host, &path)?;
                Ok(value.unwrap_or(CODE_HASH_DEFAULT))
            }
            Ok(_) => Ok(CODE_HASH_DEFAULT),
            Err(err) => Err(AccountStorageError::from(err)),
        }
    }

    /// Get the size of a contract in number of bytes used for opcodes. This value is
    /// computed and stored when the code of a contract is set.
    pub fn code_size(&self, host: &impl Runtime) -> Result<U256, AccountStorageError> {
        let path = concat(&self.path, &CODE_PATH)?;

        match host.store_value_size(&path) {
            Ok(size) => Ok(U256::from(size)),
            Err(RuntimeError::PathNotFound) => Ok(U256::zero()),
            Err(err) => Err(AccountStorageError::from(err)),
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
        let code_hash: H256 = bytes_hash(code);
        let code_hash_bytes: [u8; WORD_SIZE] = code_hash.into();
        let code_hash_path = concat(&self.path, &CODE_HASH_PATH)?;

        host.store_write(&code_hash_path, &code_hash_bytes, 0)?;

        let code_path = concat(&self.path, &CODE_PATH)?;

        let store_has_program = host.store_has(&code_path)?;

        if store_has_program.is_some() {
            host.store_delete(&code_path)?;
        }
        host.store_write_all(&code_path, code)
            .map_err(AccountStorageError::from)
    }

    /// Delete all code associated with a contract. Also sets code length and size accordingly
    pub fn delete_code(
        &mut self,
        host: &mut impl Runtime,
    ) -> Result<(), AccountStorageError> {
        let code_hash_path = concat(&self.path, &CODE_HASH_PATH)?;

        if let Some(ValueType::Value | ValueType::ValueWithSubtree) =
            host.store_has(&code_hash_path)?
        {
            host.store_delete(&code_hash_path)?
        }

        let code_path = concat(&self.path, &CODE_PATH)?;

        if let Some(ValueType::Value | ValueType::ValueWithSubtree) =
            host.store_has(&code_path)?
        {
            host.store_delete(&code_path)?
        }

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
    use tezos_smart_rollup_mock::MockHost;

    #[test]
    fn test_account_nonce_update() {
        let mut host = MockHost::default();
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
            a1.nonce(&host).expect("Could not get nonce for account"),
            U256::zero()
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
            a1.nonce(&host).expect("Could nnt get nonce for account"),
            U256::one()
        );
    }

    #[test]
    fn test_zero_account_balance_for_new_accounts() {
        let mut host = MockHost::default();
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
            a1.balance(&host)
                .expect("Could not get balance for account"),
            U256::zero()
        );
    }

    #[test]
    fn test_account_balance_add() {
        let mut host = MockHost::default();
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
            a1.balance(&host)
                .expect("Could not get balance for account"),
            v3
        );
    }

    #[test]
    fn test_account_balance_sub() {
        let mut host = MockHost::default();
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
            a1.balance(&host)
                .expect("Could not get balance for account"),
            v3
        );
    }

    #[test]
    fn test_account_balance_underflow() {
        let mut host = MockHost::default();
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
        assert_eq!(
            a1.balance_remove(&mut host, v2).unwrap_err(),
            AccountStorageError::BalanceUnderflow("/eth_accounts.2/dfkjd".to_string())
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
            a1.balance(&host)
                .expect("Could not get balance for account"),
            v1
        );
    }

    #[test]
    fn test_account_storage_zero_default() {
        let mut host = MockHost::default();
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
        let mut host = MockHost::default();
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
        let mut host = MockHost::default();
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
        let mut host = MockHost::default();
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
            a1.nonce(&host).expect("Could not get nonce for account"),
            U256::zero()
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
            a1.code(&host).expect("Could not get code for account"),
            Vec::<u8>::new()
        );
        assert_eq!(
            a1.code_size(&host)
                .expect("Could not get code size for account"),
            U256::zero()
        );
        assert_eq!(
            a1.code_hash(&host)
                .expect("Could not get code hash for account"),
            CODE_HASH_DEFAULT
        );
    }

    fn test_account_code_storage_write_code_aux(sample_code: Vec<u8>) {
        let mut host = MockHost::default();
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
            a1.code(&host).expect("Could not get code for account"),
            sample_code
        );
        assert_eq!(
            a1.code_size(&host)
                .expect("Could not get code size for account"),
            sample_code.len().into()
        );
        assert_eq!(
            a1.code_hash(&host)
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
    fn test_account_code_storage_overwrite_code() {
        let mut host = MockHost::default();
        let mut storage =
            init_account_storage().expect("Could not create EVM accounts storage API");

        let a1_path = RefPath::assert_from(b"/asdf");
        let sample_code1: Vec<u8> = (0..100).collect();
        let sample_code2: Vec<u8> = (0..50).map(|x| 50 - x).collect();
        let sample_code2_hash: H256 = bytes_hash(&sample_code2);

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
            a1.code(&host).expect("Could not get code for account"),
            sample_code2
        );
        assert_eq!(
            a1.code_size(&host)
                .expect("Could not get code size for account"),
            sample_code2.len().into()
        );
        assert_eq!(
            a1.code_hash(&host)
                .expect("Could not get code hash for account"),
            sample_code2_hash
        );
    }

    #[test]
    fn test_account_code_storage_delete_code() {
        let mut host = MockHost::default();
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
            a1.code_hash(&host)
                .expect("Could not get code hash for account"),
            CODE_HASH_DEFAULT
        );
        assert_eq!(
            a1.code(&host).expect("Could not get code for account"),
            Vec::<u8>::new()
        );
        assert_eq!(
            a1.code_size(&host)
                .expect("Could not get code size for account"),
            U256::zero()
        );
    }

    #[test]
    fn test_empty_contract_hash_matches_default() {
        let mut host = MockHost::default();
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
            a1.code(&host).expect("Could not get code for account"),
            sample_code
        );
        assert_eq!(
            a1.code_size(&host)
                .expect("Could not get code size for account"),
            sample_code.len().into()
        );
        assert_eq!(
            a1.code_hash(&host)
                .expect("Could not get code hash for account"),
            sample_code_hash
        );
    }
}
