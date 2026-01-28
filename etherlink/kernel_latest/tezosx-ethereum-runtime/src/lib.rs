// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use alloy_primitives::{hex::FromHex, Address, Bytes, Keccak256};
use revm_etherlink::{
    helpers::legacy::u256_to_alloy,
    precompiles::constants::ALWAYS_REVERT_SOL_CONTRACT,
    storage::{code::CodeStorage, world_state_handler::StorageAccount},
};
use tezos_evm_runtime::runtime::Runtime;
use tezosx_interfaces::{RuntimeInterface, TezosXRuntimeError};

pub struct EthereumRuntime;

impl RuntimeInterface for EthereumRuntime {
    fn generate_alias<Host: Runtime>(
        &self,
        host: &mut Host,
        native_address: &[u8],
    ) -> Result<Vec<u8>, TezosXRuntimeError> {
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
        .map_err(|e| TezosXRuntimeError::Custom(format!("Failed to store code: {e}")))?;
        Ok(alias.0.to_vec())
    }

    fn call<Host: Runtime>(
        &self,
        _registry: &impl tezosx_interfaces::Registry,
        host: &mut Host,
        _from: &[u8],
        to: &[u8],
        amount: primitive_types::U256,
        _data: &[u8],
    ) -> Result<Vec<u8>, TezosXRuntimeError> {
        if to.len() != 20 {
            return Err(TezosXRuntimeError::Custom(
                "Invalid address length".to_string(),
            ));
        }
        let mut addr = [0u8; 20];
        addr.copy_from_slice(to);
        let to = Address::from(addr);
        // TODO: Implement a real EVM call here.
        // For now it only implements a transfer of amount.
        let amount = u256_to_alloy(&amount);
        let mut to_account = StorageAccount::from_address(&to)?;
        let mut to_info = to_account.info(host)?;
        to_info.balance = to_info
            .balance
            .checked_add(amount)
            .ok_or_else(|| TezosXRuntimeError::Custom("Balance overflow".to_string()))?;
        to_account.set_info(host, to_info)?;
        Ok(vec![])
    }

    fn address_from_string(
        &self,
        address_str: &str,
    ) -> Result<Vec<u8>, TezosXRuntimeError> {
        let address = Address::from_hex(address_str).map_err(|e| {
            TezosXRuntimeError::Custom(format!("Invalid address string: {e}"))
        })?;
        Ok(address.0.to_vec())
    }

    // Need to implement this only for IDE. Not needed in compilation or tests.
    #[cfg(feature = "testing")]
    fn get_balance<Host: Runtime>(
        &self,
        _host: &mut Host,
        _address: &[u8],
    ) -> Result<primitive_types::U256, TezosXRuntimeError> {
        unimplemented!("Use mocks if you are in tests")
    }

    // Need to implement this only for IDE. Not needed in compilation or tests.
    #[cfg(feature = "testing")]
    fn string_from_address(&self, _address: &[u8]) -> Result<String, TezosXRuntimeError> {
        unimplemented!("Use mocks if you are in tests")
    }
}
