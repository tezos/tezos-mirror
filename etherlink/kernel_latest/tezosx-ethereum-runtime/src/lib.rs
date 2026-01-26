// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use alloy_primitives::{Address, Bytes, Keccak256};
use revm_etherlink::{
    helpers::legacy::u256_to_alloy,
    precompiles::constants::ALWAYS_REVERT_SOL_CONTRACT,
    storage::{code::CodeStorage, world_state_handler::StorageAccount},
};
use tezos_evm_runtime::runtime::Runtime;
use tezosx_interfaces::RuntimeInterface;
use thiserror::Error;

pub struct EthereumRuntime;

#[derive(Debug, Error)]
pub enum EthereumRuntimeError {
    // Define specific errors as needed
    #[error("Unimplemented feature")]
    Unimplemented,
    #[error("Custom error: {0}")]
    Custom(String),
    #[error("Internal error: {0}")]
    InternalError(#[from] revm_etherlink::Error),
    #[error("Runtime error: {0}")]
    RuntimeError(#[from] tezos_smart_rollup_host::runtime::RuntimeError),
    #[error("Path error: {0}")]
    PathError(#[from] tezos_smart_rollup_host::path::PathError),
}

impl RuntimeInterface for EthereumRuntime {
    type AddressType = Address;
    type Error = EthereumRuntimeError;

    fn generate_alias(
        &self,
        host: &mut impl Runtime,
        native_address: &[u8],
    ) -> Result<Self::AddressType, Self::Error> {
        let mut hasher = Keccak256::new();
        hasher.update(native_address);
        let hash = hasher.finalize();
        let alias = alloy_primitives::Address::from_slice(&hash[0..20]);
        // TODO: Launch a real EVM invocation to set up the contract properly.
        let mut account = StorageAccount::from_address(&alias)?;
        let mut info = account.info(host)?;
        info.code_hash = ALWAYS_REVERT_SOL_CONTRACT.code_hash;
        account.set_info(host, info)?;
        CodeStorage::add(
            host,
            &Bytes::from_static(ALWAYS_REVERT_SOL_CONTRACT.code),
            Some(ALWAYS_REVERT_SOL_CONTRACT.code_hash),
        )
        .map_err(|e| {
            EthereumRuntimeError::Custom(format!("Failed to store code: {e}"))
        })?;
        Ok(alias)
    }

    fn call(
        &self,
        host: &mut impl Runtime,
        _from: &Self::AddressType,
        to: &Self::AddressType,
        amount: primitive_types::U256,
        _data: &[u8],
    ) -> Result<Vec<u8>, Self::Error> {
        // TODO: Implement a real EVM call here.
        // For now it only implements a transfer of amount.
        let amount = u256_to_alloy(&amount);
        let mut to_account = StorageAccount::from_address(to)?;
        let mut to_info = to_account.info(host)?;
        to_info.balance = to_info.balance.checked_add(amount).ok_or_else(|| {
            EthereumRuntimeError::Custom("Balance overflow".to_string())
        })?;
        to_account.set_info(host, to_info)?;
        Ok(vec![])
    }

    fn encode_address(
        &self,
        address: &Self::AddressType,
    ) -> Result<Vec<u8>, Self::Error> {
        Ok(address.0.to_vec())
    }

    fn decode_address(&self, data: &[u8]) -> Result<Self::AddressType, Self::Error> {
        if data.len() != 20 {
            return Err(EthereumRuntimeError::Custom(
                "Invalid address length".to_string(),
            ));
        }
        let mut addr = [0u8; 20];
        addr.copy_from_slice(data);
        Ok(Address::from(addr))
    }
}
