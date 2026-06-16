// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use alloy_primitives::{Address, U256};
use revm_interpreter::Gas;
use tezos_crypto_rs::{
    base58::FromBase58CheckError, hash::ContractKt1Hash, public_key::PublicKey,
};
use tezos_evm_logging::Level;
use tezos_smart_rollup_host::{path::PathError, runtime::RuntimeError};
use thiserror::Error;

use crate::{
    CustomPrecompileError, FaDepositWithProxy, IntoWithRemainder, PrecompileStateChanges,
};

#[derive(Debug, Error)]
pub enum PrecompileStateError {
    #[error(transparent)]
    Runtime(#[from] RuntimeError),
    #[error(transparent)]
    Path(#[from] PathError),
    #[error("UTF-8 decode: {0}")]
    Utf8(#[from] std::str::Utf8Error),
    #[error("Base58Check decode: {0}")]
    Base58(#[from] FromBase58CheckError),
    #[error("RLP decode: {0}")]
    Rlp(#[from] rlp::DecoderError),
}

impl IntoWithRemainder for PrecompileStateError {
    fn into_with_remainder(self, gas: Gas) -> CustomPrecompileError {
        match self {
            PrecompileStateError::Runtime(e) => e.into_with_remainder(gas),
            other => CustomPrecompileError::Revert(other.to_string(), gas),
        }
    }
}

pub trait DatabasePrecompileStateChanges {
    fn log_node_message(&mut self, level: Level, message: &str);
    fn global_counter(&self) -> Result<U256, PrecompileStateError>;
    fn ticket_balance(
        &self,
        ticket_hash: &U256,
        owner: &Address,
    ) -> Result<U256, PrecompileStateError>;
    fn sequencer(&self) -> Result<PublicKey, PrecompileStateError>;
    fn governance_sequencer_upgrade_exists(&self) -> Result<bool, PrecompileStateError>;
    fn deposit_in_queue(
        &self,
        deposit_id: &U256,
    ) -> Result<FaDepositWithProxy, PrecompileStateError>;
    fn ticketer(&self) -> Result<ContractKt1Hash, PrecompileStateError>;
}

pub trait DatabaseCommitPrecompileStateChanges {
    fn commit(&mut self, etherlink_data: PrecompileStateChanges);
}
