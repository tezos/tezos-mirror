// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::U256;
use tezos_crypto_rs::{blake2b, hash::ContractKt1Hash};
use tezos_data_encoding::{enc::BinWriter, nom::NomReader};
use tezos_evm_runtime::runtime::Runtime;
use tezos_execution::{account_storage::TezlinkAccount, context::Context};
use tezos_protocol::contract::Contract;
use tezos_smart_rollup::types::PublicKeyHash;
use tezosx_interfaces::{CrossCallResult, CrossRuntimeContext, TezosXRuntimeError};

use crate::{
    account::{
        get_tezos_account_info_or_init, narith_to_u256, set_tezos_account_info,
        TEZOS_ACCOUNTS_PATH,
    },
    context::TezosRuntimeContext,
};

pub struct TezosRuntime;

pub mod account;
pub mod context;

impl tezosx_interfaces::RuntimeInterface for TezosRuntime {
    fn generate_alias<Host: Runtime>(
        &self,
        _registry: &impl tezosx_interfaces::Registry,
        _host: &mut Host,
        native_address: &[u8],
        _context: CrossRuntimeContext,
    ) -> Result<Vec<u8>, TezosXRuntimeError> {
        // TODO: Add code in this contract.
        let contract = Contract::Originated(ContractKt1Hash::from(blake2b::digest_160(
            native_address,
        )));
        contract.to_bytes().map_err(|e| {
            TezosXRuntimeError::ConversionError(format!(
                "Failed to encode address to bytes: {e}"
            ))
        })
    }

    // For now only implement transfers with implicit accounts.
    fn call<Host: Runtime>(
        &self,
        _registry: &impl tezosx_interfaces::Registry,
        host: &mut Host,
        _from: &[u8],
        to: &[u8],
        amount: U256,
        _data: &[u8],
    ) -> Result<CrossCallResult, TezosXRuntimeError> {
        let to = Contract::nom_read_exact(to).map_err(|e| {
            TezosXRuntimeError::ConversionError(format!(
                "Failed to decode address from bytes: {e:?}"
            ))
        })?;
        match to {
            Contract::Implicit(pkh) => {
                let mut account = account::get_tezos_account_info_or_init(host, &pkh)?;
                account.balance =
                    account.balance.checked_add(amount).ok_or_else(|| {
                        TezosXRuntimeError::ConversionError(
                            "Balance overflow".to_string(),
                        )
                    })?;
                account::set_tezos_account_info(host, &pkh, account)?;
                Ok(CrossCallResult::Success(vec![]))
            }
            Contract::Originated(kt1) => {
                // TODO: Have our own implementation of originated contracts.
                let context = TezosRuntimeContext::from_root(&TEZOS_ACCOUNTS_PATH)?;
                let originated_account = context.originated_from_kt1(&kt1)?;
                originated_account.add_balance(host, amount.as_u64())?;
                Ok(CrossCallResult::Success(vec![]))
            }
        }
    }

    fn address_from_string(
        &self,
        address_str: &str,
    ) -> Result<Vec<u8>, TezosXRuntimeError> {
        let contract = Contract::from_b58check(address_str).map_err(|e| {
            TezosXRuntimeError::ConversionError(format!(
                "Failed to parse address from string: {e}"
            ))
        })?;
        contract.to_bytes().map_err(|e| {
            TezosXRuntimeError::ConversionError(format!(
                "Failed to encode address to bytes: {e}"
            ))
        })
    }

    // Need to implement this only for IDE. Not needed in compilation or tests.
    #[cfg(feature = "testing")]
    fn get_balance<Host: Runtime>(
        &self,
        _host: &mut Host,
        _address: &[u8],
    ) -> Result<U256, TezosXRuntimeError> {
        unimplemented!("Use mocks if you are in tests")
    }

    // Need to implement this only for IDE. Not needed in compilation or tests.
    #[cfg(feature = "testing")]
    fn string_from_address(&self, _address: &[u8]) -> Result<String, TezosXRuntimeError> {
        unimplemented!("Use mocks if you are in tests")
    }
}

impl TezosRuntime {
    pub fn add_balance(
        host: &mut impl Runtime,
        pub_key_hash: &PublicKeyHash,
        amount: U256,
    ) -> Result<(), TezosXRuntimeError> {
        let mut info = get_tezos_account_info_or_init(host, pub_key_hash)?;
        info.balance = info
            .balance
            .checked_add(amount)
            .ok_or(TezosXRuntimeError::Custom("Balance overflow".to_string()))?;
        set_tezos_account_info(host, pub_key_hash, info)
    }

    // Used for debug while we don't have our own originated account implementation.
    pub fn get_originated_account_balance(
        host: &impl Runtime,
        kt1: &ContractKt1Hash,
    ) -> Result<U256, TezosXRuntimeError> {
        let context = TezosRuntimeContext::from_root(&TEZOS_ACCOUNTS_PATH)?;
        let originated_account = context.originated_from_kt1(kt1)?;
        let balance = originated_account.balance(host)?;
        narith_to_u256(&balance)
    }
}
