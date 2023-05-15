// SPDX-FileCopyrightText: 2022 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Ethereum transaction data

use crate::account_storage::EthereumAccountStorage;
use crate::EthereumError;
use debug::debug_msg;
use evm::Context;
use host::runtime::Runtime;
use primitive_types::{H160, U256};

/// One transaction level
///
/// Calling an EVM contract initiates a new transaction unless it is a
/// delegate call. This is a wrapper for the Context object of SputnikVM,
/// but will in time contain additional data related to the rollup node,
/// including a vector of withdrawals that may be generated using
/// a precompiled contract.
#[derive(Clone)]
pub struct TransactionContext {
    /// The context for the transaction - caller/callee, et.c.
    pub context: Context,
}

#[allow(unused_variables)]
impl TransactionContext {
    /// Create a new transaction context
    pub fn new(caller_address: H160, callee_address: H160, apparent_value: U256) -> Self {
        Self {
            context: Context {
                address: callee_address,
                caller: caller_address,
                apparent_value,
            },
        }
    }

    /// Create a transaction context from a SputnikVm context
    pub fn from_context(context: Context) -> Self {
        Self { context }
    }
}

/// Begin a transaction for both ticket transactions _and_
/// Ethereum transactions (when we have an implementation for
/// Ethereum transactions).
pub fn begin_transaction(
    host: &mut impl Runtime,
    evm_account_storage: &mut EthereumAccountStorage,
) -> Result<(), EthereumError> {
    debug_msg!(host, "Begin transaction");
    evm_account_storage
        .begin_transaction(host)
        .map_err(EthereumError::from)
}

/// Commit a transaction in durable storage.
pub fn commit_transaction(
    host: &mut impl Runtime,
    evm_account_storage: &mut EthereumAccountStorage,
) -> Result<(), EthereumError> {
    debug_msg!(host, "Commit transaction");
    evm_account_storage
        .commit_transaction(host)
        .map_err(EthereumError::from)
}

/// Rollback a transaction in durable storage.
pub fn rollback_transaction(
    host: &mut impl Runtime,
    evm_account_storage: &mut EthereumAccountStorage,
) -> Result<(), EthereumError> {
    debug_msg!(host, "Rollback transaction");
    evm_account_storage
        .rollback_transaction(host)
        .map_err(EthereumError::from)
}
