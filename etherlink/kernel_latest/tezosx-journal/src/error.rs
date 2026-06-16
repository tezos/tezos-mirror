// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use evm_types::{CustomPrecompileError, IntoWithRemainder, PrecompileStateError};
use revm::interpreter::Gas;
use revm::primitives::{Address, U256};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum LayeredStateError {
    #[error(transparent)]
    State(#[from] PrecompileStateError),
    #[error("Adding {amount} to {owner} balance failed, ticket hash is {ticket_hash}")]
    BalanceOverflow {
        ticket_hash: U256,
        owner: Address,
        amount: U256,
    },
    #[error(
        "Removing {amount} from {owner} balance failed, ticket hash is {ticket_hash}"
    )]
    BalanceUnderflow {
        ticket_hash: U256,
        owner: Address,
        amount: U256,
    },
    #[error("Global counter overflow")]
    GlobalCounterOverflow,
    #[error("Deposit already removed")]
    DepositAlreadyRemoved,
}

impl IntoWithRemainder for LayeredStateError {
    fn into_with_remainder(self, gas: Gas) -> CustomPrecompileError {
        match self {
            LayeredStateError::State(e) => e.into_with_remainder(gas),
            other => CustomPrecompileError::Revert(other.to_string(), gas),
        }
    }
}
