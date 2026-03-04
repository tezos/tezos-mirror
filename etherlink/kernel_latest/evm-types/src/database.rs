// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm::primitives::{Address, U256};
use tezos_crypto_rs::{hash::ContractKt1Hash, public_key::PublicKey};
use tezos_evm_logging::Level;

use crate::{CustomPrecompileError, FaDepositWithProxy, PrecompileStateChanges};

pub trait DatabasePrecompileStateChanges {
    fn log_node_message(&mut self, level: Level, message: &str);
    fn global_counter(&self) -> Result<U256, CustomPrecompileError>;
    fn ticket_balance(
        &self,
        ticket_hash: &U256,
        owner: &Address,
    ) -> Result<U256, CustomPrecompileError>;
    fn sequencer(&self) -> Result<PublicKey, CustomPrecompileError>;
    fn governance_sequencer_upgrade_exists(&self) -> Result<bool, CustomPrecompileError>;
    fn deposit_in_queue(
        &self,
        deposit_id: &U256,
    ) -> Result<FaDepositWithProxy, CustomPrecompileError>;
    fn ticketer(&self) -> Result<ContractKt1Hash, CustomPrecompileError>;
}

pub trait DatabaseCommitPrecompileStateChanges {
    fn commit(&mut self, etherlink_data: PrecompileStateChanges);
}
