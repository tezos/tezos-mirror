// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Tezos account state and storage

use primitive_types::U256;
use tezos_data_encoding::enc::BinWriter;
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup::types::Contract;
use tezos_smart_rollup_host::path::{concat, OwnedPath, RefPath};
use tezos_storage::{
    read_u256_le_default, read_u64_le_default, write_u256_le, write_u64_le,
};

use crate::context;

#[derive(Debug, PartialEq)]
pub struct TezlinkImplicitAccount {
    path: OwnedPath,
}

impl From<OwnedPath> for TezlinkImplicitAccount {
    fn from(path: OwnedPath) -> Self {
        Self { path }
    }
}

const BALANCE_PATH: RefPath = RefPath::assert_from(b"/balance");

const COUNTER_PATH: RefPath = RefPath::assert_from(b"/counter");

fn account_path(contract: &Contract) -> Result<OwnedPath, tezos_storage::error::Error> {
    // uses the same encoding as in the octez node's representation of the context
    // see `octez-codec describe alpha.contract binary schema`
    let mut contract_encoded = Vec::new();
    contract
        .bin_write(&mut contract_encoded)
        .map_err(|_| tezos_smart_rollup::host::RuntimeError::DecodingError)?;

    let path_string = alloc::format!("/{}", hex::encode(&contract_encoded));
    Ok(OwnedPath::try_from(path_string)?)
}

impl TezlinkImplicitAccount {
    // We must provide the context object to get the full path in the durable storage
    #[allow(dead_code)]
    pub fn from_contract(
        context: &context::Context,
        contract: &Contract,
    ) -> Result<Self, tezos_storage::error::Error> {
        let index = context::contracts::index(context)?;
        let path = concat(&index, &account_path(contract)?)?;
        Ok(path.into())
    }

    /// Get the **counter** for the Tezlink account.
    #[allow(dead_code)]
    pub fn counter(
        &self,
        host: &impl Runtime,
    ) -> Result<u64, tezos_storage::error::Error> {
        let path = concat(&self.path, &COUNTER_PATH)?;
        read_u64_le_default(host, &path, 0u64)
    }

    /// Set the **counter** for the Tezlink account.
    #[allow(dead_code)]
    pub fn set_counter(
        &mut self,
        host: &mut impl Runtime,
        counter: u64,
    ) -> Result<(), tezos_storage::error::Error> {
        let path = concat(&self.path, &COUNTER_PATH)?;
        write_u64_le(host, &path, counter)
    }

    /// Get the **balance** of an account in Mutez held by the account.
    #[allow(dead_code)]
    pub fn balance(
        &self,
        host: &impl Runtime,
    ) -> Result<U256, tezos_storage::error::Error> {
        let path = concat(&self.path, &BALANCE_PATH)?;
        read_u256_le_default(host, &path, U256::zero())
    }

    /// Set the **balance** of an account in Mutez held by the account.
    #[allow(dead_code)]
    pub fn set_balance(
        &mut self,
        host: &mut impl Runtime,
        balance: U256,
    ) -> Result<(), tezos_storage::error::Error> {
        let path = concat(&self.path, &BALANCE_PATH)?;
        write_u256_le(host, &path, balance)
    }
}
