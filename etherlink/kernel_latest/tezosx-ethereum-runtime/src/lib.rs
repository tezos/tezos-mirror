// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use alloy_primitives::{hex::FromHex, Address, Bytes, Keccak256, U256 as AlloyU256};
use alloy_sol_types::SolCall;
use revm::context::result::ExecutionResult;
use revm::state::Bytecode;
use revm_etherlink::{
    helpers::legacy::u256_to_alloy,
    precompiles::constants::{
        ALIAS_FORWARDER_PRECOMPILE_ADDRESS, ALIAS_FORWARDER_SOL_CONTRACT,
        TEZOSX_CALLER_ADDRESS,
    },
    run_transaction,
    storage::{
        code::CodeStorage, version::read_evm_version, world_state_handler::StorageAccount,
    },
    GasData,
};
use tezos_ethereum::block::{BlockConstants, BlockFees};
use tezos_evm_runtime::runtime::Runtime;
use tezosx_interfaces::{
    AliasCreationContext, Registry, RuntimeInterface, TezosXRuntimeError,
};

/// Solidity interface for init_tezosx_alias function.
alloy_sol_types::sol! {
    function init_tezosx_alias(string nativeAddress) external payable;
}

pub struct EthereumRuntime;

/// Read the sequencer pool address (coinbase) from storage.
fn read_sequencer_pool_address(host: &impl Runtime) -> Option<primitive_types::H160> {
    use tezos_smart_rollup_host::path::RefPath;
    const SEQUENCER_POOL_PATH: RefPath =
        RefPath::assert_from(b"/evm/sequencer_pool_address");
    let mut bytes = [0u8; 20];
    match host.store_read_slice(&SEQUENCER_POOL_PATH, 0, bytes.as_mut_slice()) {
        Ok(20) => Some(bytes.into()),
        _ => None,
    }
}

impl RuntimeInterface for EthereumRuntime {
    fn generate_alias<Host: Runtime>(
        &self,
        registry: &impl Registry,
        host: &mut Host,
        native_address: &[u8],
        context: AliasCreationContext,
    ) -> Result<Vec<u8>, TezosXRuntimeError> {
        // Step 1: Compute the alias address deterministically from the native address
        let mut hasher = Keccak256::new();
        hasher.update(native_address);
        let hash = hasher.finalize();
        let alias = Address::from_slice(&hash[0..20]);

        // Step 2: Set up EIP-7702 delegation bytecode at the alias address
        // The delegation bytecode points to the AliasForwarder precompile
        let delegation_bytecode =
            Bytecode::new_eip7702(ALIAS_FORWARDER_PRECOMPILE_ADDRESS);

        // Store the delegation bytecode at the alias address
        let mut alias_account = StorageAccount::from_address(&alias)?;
        let mut alias_info = alias_account.info(host)?;

        // Store the delegation code
        let code_bytes = delegation_bytecode.original_byte_slice();
        let code_hash = revm_etherlink::helpers::storage::bytes_hash(code_bytes);

        alias_info.code_hash = code_hash;
        alias_account.set_info(host, alias_info)?;

        CodeStorage::add(host, code_bytes, Some(code_hash)).map_err(|e| {
            TezosXRuntimeError::Custom(format!("Failed to store delegation code: {e}"))
        })?;

        // Ensure the AliasForwarder precompile code is available
        // (it should be initialized at kernel startup, but verify it exists)
        let precompile_account =
            StorageAccount::from_address(&ALIAS_FORWARDER_PRECOMPILE_ADDRESS)?;
        let precompile_info = precompile_account.info(host)?;
        if precompile_info.code_hash != ALIAS_FORWARDER_SOL_CONTRACT.code_hash {
            // Initialize the precompile if not already done
            CodeStorage::add(
                host,
                ALIAS_FORWARDER_SOL_CONTRACT.code,
                Some(ALIAS_FORWARDER_SOL_CONTRACT.code_hash),
            )
            .map_err(|e| {
                TezosXRuntimeError::Custom(format!(
                    "Failed to store AliasForwarder precompile code: {e}"
                ))
            })?;
        }

        // Step 3: Call init_tezosx_alias on the alias address to set up storage
        // The native address is passed as a string (e.g., "tz1...")
        let native_address_str =
            String::from_utf8(native_address.to_vec()).map_err(|e| {
                TezosXRuntimeError::Custom(format!(
                    "Invalid native address encoding: {e}"
                ))
            })?;

        // Encode the init_tezosx_alias call
        let call_data = init_tezosx_aliasCall {
            nativeAddress: native_address_str,
        }
        .abi_encode();

        // Set up block constants for EVM execution
        let evm_version = read_evm_version(host);
        let coinbase = read_sequencer_pool_address(host).unwrap_or_default();

        let block_constants = BlockConstants {
            number: context.block_number,
            coinbase,
            timestamp: context.timestamp,
            gas_limit: context.gas_limit,
            block_fees: BlockFees::new(
                primitive_types::U256::zero(),
                primitive_types::U256::zero(),
                primitive_types::U256::zero(),
            ),
            chain_id: primitive_types::U256::from(context.chain_id),
            tezos_experimental_features: true,
            prevrandao: None,
        };

        // Set up gas data (zero gas price since this is an internal transaction)
        let gas_data = GasData::new(context.gas_limit, 0, context.gas_limit);

        // Ensure the TezosX caller account has balance for gas
        let mut caller_account = StorageAccount::from_address(&TEZOSX_CALLER_ADDRESS)?;
        let mut caller_info = caller_account.info(host)?;
        if caller_info.balance < AlloyU256::from(context.gas_limit) {
            caller_info.balance = AlloyU256::MAX;
            caller_account.set_info_without_code(host, caller_info)?;
        }

        // Run the EVM transaction to call init_tezosx_alias
        let outcome = run_transaction(
            host,
            registry,
            evm_version.into(),
            &block_constants,
            None, // no transaction hash for internal transactions
            TEZOSX_CALLER_ADDRESS,
            Some(alias), // call to the alias address
            Bytes::from(call_data),
            gas_data,
            AlloyU256::ZERO, // no value transfer
            revm::context::transaction::AccessList(vec![]),
            None,  // no authorization list
            None,  // no tracer
            false, // not a simulation
        )
        .map_err(|e| {
            TezosXRuntimeError::Custom(format!("EVM execution failed: {e:?}"))
        })?;

        // Check that the call succeeded
        match outcome.result {
            ExecutionResult::Success { .. } => Ok(alias.0.to_vec()),
            ExecutionResult::Revert { output, .. } => Err(TezosXRuntimeError::Custom(
                format!("init_tezosx_alias reverted: {output:?}"),
            )),
            ExecutionResult::Halt { reason, .. } => Err(TezosXRuntimeError::Custom(
                format!("init_tezosx_alias halted: {reason:?}"),
            )),
        }
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

#[cfg(test)]
mod tests {
    use super::*;
    use revm::state::AccountInfo;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezosx_interfaces::{Registry as RegistryTrait, RuntimeId, TezosXRuntimeError};

    const GAS_LIMIT: u64 = 30_000_000;
    const CHAIN_ID: u64 = 42793;

    /// Mock Registry for testing that delegates to EthereumRuntime
    struct MockRegistry;

    impl RegistryTrait for MockRegistry {
        fn bridge<Host: Runtime>(
            &self,
            _host: &mut Host,
            _destination_runtime: RuntimeId,
            _destination_address: &[u8],
            _source_address: &[u8],
            _amount: primitive_types::U256,
            _data: &[u8],
        ) -> Result<Vec<u8>, TezosXRuntimeError> {
            unimplemented!("bridge not needed for these tests")
        }

        fn generate_alias<Host: Runtime>(
            &self,
            host: &mut Host,
            native_address: &[u8],
            runtime_id: RuntimeId,
            context: AliasCreationContext,
        ) -> Result<Vec<u8>, TezosXRuntimeError> {
            match runtime_id {
                RuntimeId::Ethereum => {
                    EthereumRuntime.generate_alias(self, host, native_address, context)
                }
                _ => Err(TezosXRuntimeError::RuntimeNotFound(runtime_id)),
            }
        }

        fn address_from_string(
            &self,
            _address_str: &str,
            _runtime_id: RuntimeId,
        ) -> Result<Vec<u8>, TezosXRuntimeError> {
            unimplemented!("address_from_string not needed for these tests")
        }
    }

    fn create_test_context() -> AliasCreationContext {
        AliasCreationContext {
            gas_limit: GAS_LIMIT,
            chain_id: CHAIN_ID,
            timestamp: primitive_types::U256::from(1),
            block_number: primitive_types::U256::from(1),
        }
    }

    /// Test that alias addresses are computed deterministically from native addresses.
    #[test]
    fn test_alias_address_is_deterministic() {
        let native_address = b"tz1abc123";

        // Compute alias address using the same algorithm as generate_alias
        let mut hasher = Keccak256::new();
        hasher.update(native_address);
        let hash = hasher.finalize();
        let alias = &hash[0..20];

        // Verify it's 20 bytes
        assert_eq!(alias.len(), 20);

        // Compute again to verify determinism
        let mut hasher2 = Keccak256::new();
        hasher2.update(native_address);
        let hash2 = hasher2.finalize();
        let alias2 = &hash2[0..20];

        assert_eq!(alias, alias2);
    }

    #[test]
    fn test_different_native_addresses_produce_different_aliases() {
        let compute_alias = |native: &[u8]| {
            let mut hasher = Keccak256::new();
            hasher.update(native);
            let hash = hasher.finalize();
            hash[0..20].to_vec()
        };

        let alias1 = compute_alias(b"tz1abc");
        let alias2 = compute_alias(b"tz1xyz");

        assert_ne!(alias1, alias2);
    }

    #[test]
    fn test_eip7702_delegation_bytecode_format() {
        // Verify the EIP-7702 delegation bytecode format
        let delegation = Bytecode::new_eip7702(ALIAS_FORWARDER_PRECOMPILE_ADDRESS);
        let raw = delegation.original_byte_slice();

        // EIP-7702 format: 0xEF01 + 0x00 (version) + 20 bytes address = 23 bytes
        assert_eq!(raw.len(), 23);
        assert_eq!(raw[0], 0xEF);
        assert_eq!(raw[1], 0x01);
        assert_eq!(raw[2], 0x00); // version

        // The remaining 20 bytes should be the precompile address
        assert_eq!(&raw[3..], ALIAS_FORWARDER_PRECOMPILE_ADDRESS.as_slice());
    }
}
