// SPDX-FileCopyrightText: 2022 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Ethereum transaction data

use evm::Context;
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
