// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

#![doc = include_str!("../README.md")]

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
