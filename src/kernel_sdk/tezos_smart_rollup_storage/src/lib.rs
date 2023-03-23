// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Transactions storage
//!
//! This crate supports dealing with accounts and transactions for updating
//! said accounts storage. All accounts are stored in durable storage.
//!
//! To use this crate, provide a definition of an account. The account structure
//! should follow these guidelines:
//!
//! - it can be created from an [OwnedPath], ie, it implements `From<OwnedPath>`
//! - it has getters and setters that operate directly on durable storage, each
//!   getter and setter should take a [Runtime] as argument to do so (`mut` in
//!   case of setters).
//!
//! To use this crate, create account struct and storage object like so:
//!
//! ```
//! use tezos_smart_rollup_host::runtime::Runtime;
//! use tezos_smart_rollup_host::path::{concat, RefPath, OwnedPath};
//! use tezos_smart_rollup_storage::storage::Storage;
//! use tezos_smart_rollup_mock::MockHost;
//!
//! struct MyAccount {
//!   path: OwnedPath,
//! }
//!
//! const VALUE_PATH: RefPath = RefPath::assert_from(b"/value");
//!
//! impl MyAccount {
//!   pub fn setter(&mut self, host: &mut impl Runtime, v: &str) {
//!     let value_path = concat(&self.path, &VALUE_PATH)
//!         .expect("Could not get path for account value");
//!     host.store_write(&value_path, v.as_bytes(), 0)
//!         .expect("Could not set value for account");
//!   }
//!
//!   pub fn getter(
//!       &mut self,
//!       host: &impl Runtime,
//!   ) -> Vec<u8> {
//!     let value_path = concat(&self.path, &VALUE_PATH)
//!         .expect("Could not get path for account value");
//!     host.store_read(&value_path, 0, 1024)
//!         .expect("Could not read account value")
//!   }
//! }
//!
//! impl From<OwnedPath> for MyAccount {
//!   fn from(path: OwnedPath) -> Self {
//!     Self { path }
//!   }
//! }
//!
//! const ACCOUNT_PATH: RefPath = RefPath::assert_from(b"/accounts");
//!
//! let mut host = MockHost::default();
//!
//! let mut storage = Storage::<MyAccount>::init(&ACCOUNT_PATH)
//!     .expect("Could not create storage interface");
//!
//! storage.begin_transaction(&mut host)
//!     .expect("Could not begin new transaction");
//!
//! let account_id = RefPath::assert_from(b"/my.account.id");
//!
//! let mut account = storage.new_account(&mut host, &account_id)
//!     .expect("Could not create new account")
//!     .expect("Account already exists");
//!
//! account.setter(&mut host, "some value");
//!
//! storage.commit(&mut host)
//!     .expect("Could not commit transaction");
//! ```
//!
//! [OwnedPath]: host::path::OwnedPath
//! [Runtime]: host::runtime::Runtime

extern crate tezos_smart_rollup_debug as debug;
extern crate tezos_smart_rollup_host as host;

use thiserror::Error;

/// Account transaction errors
///
/// All errors that may occur when using the accoun transaction API. When
/// the error variant encapsulates some other error type, this is where the
/// error originates from the durable storage interface on the host runtime.
#[derive(Error, Copy, Eq, PartialEq, Clone, Debug)]
pub enum StorageError {
    /// Invalid accounts path. Could not get the string representation of
    /// the accounts storage path.
    #[error("Invalid accounts storage path")]
    InvalidAccountsPath,
    /// Some path in durable storage was already in use when the storage
    /// interface needed to use it for holding a transaction.
    #[error("Transaction storage is already in use")]
    StorageInUse,
    /// Some storage operation was tried, but the storage doesn't even have
    /// the base (original) layer of accounts available.
    #[error("No storage")]
    NoStorage,
    /// Some transaction operation, commit or rollback, was attempted while
    /// no transaction was in progress.
    #[error("No current transaction to commit or roll back")]
    NoCurrentTransaction,
    /// Tried to create an invalid path to accounts. Check that the path to
    /// the storage for original account values (the base layer) starts with
    /// a '/' character (it should).
    #[error("Path error")]
    PathError(host::path::PathError),
    /// Some value (tx counter) was kept in a malformed format in
    /// durable storage.
    #[error("Malformed 32 or 64 bit integer in storage")]
    MalformedValue,
    /// A 32 bit transaction counter overflowed.
    #[error("Transaction counter overflow")]
    TxCounterOverflow,
    /// Some error was encountered while using the durable storage. This may
    /// happen when doing some transaction operation.
    #[error("Runtrime error")]
    RuntimeError(host::runtime::RuntimeError),
}

impl From<host::path::PathError> for StorageError {
    fn from(path_error: host::path::PathError) -> Self {
        StorageError::PathError(path_error)
    }
}

impl From<host::runtime::RuntimeError> for StorageError {
    fn from(runtime_error: host::runtime::RuntimeError) -> Self {
        StorageError::RuntimeError(runtime_error)
    }
}

mod layer;
pub mod storage;
