// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::U256;
use tezos_crypto_rs::{
    blake2b,
    hash::{ContractKt1Hash, HashTrait},
};
use tezos_data_encoding::{enc::BinWriter, nom::NomReader};
use tezos_evm_runtime::runtime::Runtime;
use tezos_execution::{account_storage::TezlinkAccount, context::Context};
use tezos_protocol::contract::Contract;
use tezos_smart_rollup::types::PublicKeyHash;
use tezos_smart_rollup_host::{path::PathError, runtime::RuntimeError};
use thiserror::Error;

use crate::{
    account::{
        get_tezos_account_info_or_init, narith_to_u256, set_tezos_account_info,
        TEZOS_ACCOUNTS_PATH,
    },
    context::TezosRuntimeContext,
};

pub struct TezosRuntime;

#[derive(Debug, Error)]
pub enum TezosRuntimeError {
    // Define specific errors as needed
    #[error("Unimplemented feature")]
    Unimplemented,
    #[error("Conversion error: {0}")]
    ConversionError(String),
    #[error("Custom error: {0}")]
    Custom(String),
    #[error("Path error: {0}")]
    Path(#[from] PathError),
    #[error("Runtime error: {0}")]
    Runtime(#[from] RuntimeError),
    #[error("Storage error: {0}")]
    Storage(#[from] tezos_storage::error::Error),
}

pub mod account;
pub mod context;

impl tezosx_interfaces::RuntimeInterface for TezosRuntime {
    type AddressType = Contract;
    type Error = TezosRuntimeError;

    fn generate_alias(
        &self,
        _host: &mut impl Runtime,
        native_address: &[u8],
    ) -> Result<Self::AddressType, Self::Error> {
        let digest = blake2b::digest(native_address, ContractKt1Hash::hash_size())
            .map_err(|err| {
                TezosRuntimeError::Custom(format!(
                    "Failed to compute Blake2b digest: {err}"
                ))
            })?;
        // TODO: Add code in this contract.
        Ok(Contract::Originated(
            HashTrait::try_from_bytes(digest.as_slice())
                .map_err(|e| TezosRuntimeError::ConversionError(e.to_string()))?,
        ))
    }

    // For now only implement transfers with implicit accounts.
    fn call(
        &self,
        host: &mut impl Runtime,
        _from: &Self::AddressType,
        to: &Self::AddressType,
        amount: U256,
        _data: &[u8],
    ) -> Result<Vec<u8>, Self::Error> {
        match to {
            Contract::Implicit(pkh) => {
                let mut account = account::get_tezos_account_info_or_init(host, pkh)?;
                account.balance =
                    account.balance.checked_add(amount).ok_or_else(|| {
                        TezosRuntimeError::ConversionError("Balance overflow".to_string())
                    })?;
                account::set_tezos_account_info(host, pkh, account)?;
                Ok(vec![])
            }
            Contract::Originated(kt1) => {
                // TODO: Have our own implementation of originated contracts.
                let context = TezosRuntimeContext::from_root(&TEZOS_ACCOUNTS_PATH)?;
                let originated_account = context.originated_from_kt1(kt1)?;
                originated_account.add_balance(host, amount.as_u64())?;
                Ok(vec![])
            }
        }
    }

    fn encode_address(
        &self,
        address: &Self::AddressType,
    ) -> Result<Vec<u8>, Self::Error> {
        address.to_bytes().map_err(|e| {
            TezosRuntimeError::ConversionError(format!(
                "Failed to encode address to bytes: {e}"
            ))
        })
    }

    fn decode_address(&self, data: &[u8]) -> Result<Self::AddressType, Self::Error> {
        Contract::nom_read_exact(data).map_err(|e| {
            TezosRuntimeError::ConversionError(format!(
                "Failed to decode address from bytes: {e:?}"
            ))
        })
    }
}

impl TezosRuntime {
    pub fn add_balance(
        host: &mut impl Runtime,
        pub_key_hash: &PublicKeyHash,
        amount: U256,
    ) -> Result<(), TezosRuntimeError> {
        let mut info = get_tezos_account_info_or_init(host, pub_key_hash)?;
        info.balance = info
            .balance
            .checked_add(amount)
            .ok_or(TezosRuntimeError::Custom("Balance overflow".to_string()))?;
        set_tezos_account_info(host, pub_key_hash, info)
    }

    // Used for debug while we don't have our own originated account implementation.
    pub fn get_originated_account_balance(
        host: &mut impl Runtime,
        kt1: &ContractKt1Hash,
    ) -> Result<U256, TezosRuntimeError> {
        let context = TezosRuntimeContext::from_root(&TEZOS_ACCOUNTS_PATH)?;
        let originated_account = context.originated_from_kt1(kt1)?;
        let balance = originated_account.balance(host)?;
        narith_to_u256(&balance)
    }
}
