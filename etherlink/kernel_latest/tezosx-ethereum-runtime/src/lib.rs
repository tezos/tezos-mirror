// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use alloy_primitives::{hex::FromHex, Address, Bytes, Keccak256, U256 as AlloyU256};
use alloy_sol_types::SolCall;
use primitive_types::U256;
use revm::context::result::{ExecutionResult, Output};
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
    ExecutionOutcome, GasData,
};
use tezos_ethereum::block::{BlockConstants, BlockFees};
use tezos_evm_runtime::runtime::Runtime;
use tezosx_interfaces::{
    CrossCallResult, CrossRuntimeContext, Registry, RuntimeInterface, TezosXRuntimeError,
};

alloy_sol_types::sol! {
    function init_tezosx_alias(string nativeAddress) external payable;
}

pub struct EthereumRuntime {
    chain_id: primitive_types::U256,
}

impl Default for EthereumRuntime {
    fn default() -> Self {
        Self::new(U256::from(1337))
    }
}

impl EthereumRuntime {
    pub fn new(chain_id: primitive_types::U256) -> Self {
        Self { chain_id }
    }

    fn create_block_constants<Host: Runtime>(
        &self,
        host: &Host,
        context: &CrossRuntimeContext,
    ) -> BlockConstants {
        let coinbase = read_sequencer_pool_address(host).unwrap_or_default();

        BlockConstants {
            number: context.block_number,
            coinbase,
            timestamp: context.timestamp,
            gas_limit: context.gas_limit,
            block_fees: BlockFees::new(
                primitive_types::U256::zero(),
                primitive_types::U256::zero(),
                primitive_types::U256::zero(),
            ),
            chain_id: self.chain_id,
            tezos_experimental_features: true,
            prevrandao: None,
        }
    }
}

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
        context: CrossRuntimeContext,
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
        let block_constants = self.create_block_constants(host, &context);

        // Set up gas data (zero gas price since this is an internal transaction)
        let gas_data = GasData::new(context.gas_limit, 0, context.gas_limit);

        // Ensure the TezosX caller account has balance for gas
        let mut caller_account = StorageAccount::from_address(&TEZOSX_CALLER_ADDRESS)?;
        let mut caller_info = caller_account.info(host)?;
        if caller_info.balance < AlloyU256::MAX.div_ceil(AlloyU256::from(2)) {
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
        registry: &impl tezosx_interfaces::Registry,
        host: &mut Host,
        from: &[u8],
        to: &[u8],
        amount: primitive_types::U256,
        data: &[u8],
        context: CrossRuntimeContext,
    ) -> Result<CrossCallResult, TezosXRuntimeError> {
        if from.len() != 20 {
            return Err(TezosXRuntimeError::Custom(
                "Invalid 'from' address length".to_string(),
            ));
        }
        if to.len() != 20 {
            return Err(TezosXRuntimeError::Custom(
                "Invalid 'to' address length".to_string(),
            ));
        }

        let caller = Address::from_slice(from);
        let destination = Address::from_slice(to);

        let evm_version = read_evm_version(host);
        let block_constants = self.create_block_constants(host, &context);

        // Set up gas data (zero gas price since this is a cross-runtime transaction)
        // unlimited gas for now (ie current block gas limit, to make sure the transaction will
        // fit and not be rejected)
        // TODO: L2-869
        let gas_data = GasData::new(context.gas_limit, 0, context.gas_limit);

        let value = u256_to_alloy(&amount);
        let call_data = Bytes::from(data.to_vec());

        // TODO: L2-885 this should be done using the revm journal.
        let outcome = with_temporary_credit(caller, host, value, |host| {
            run_transaction(
                host,
                registry,
                evm_version.into(),
                &block_constants,
                None, // no transaction hash for cross-runtime transactions
                caller,
                Some(destination),
                call_data,
                gas_data,
                value,
                revm::context::transaction::AccessList(vec![]),
                None,  // no authorization list
                None,  // no tracer
                false, // not a simulation
            )
            .map_err(|e| {
                TezosXRuntimeError::Custom(format!("EVM execution failed: {e:?}"))
            })
        })?;

        match outcome.result {
            ExecutionResult::Success { output, .. } => match output {
                Output::Call(bytes) => Ok(CrossCallResult::Success(bytes.to_vec())),
                Output::Create(bytes, _) => Ok(CrossCallResult::Success(bytes.to_vec())),
            },
            ExecutionResult::Revert { output, .. } => {
                Ok(CrossCallResult::Revert(output.to_vec()))
            }
            ExecutionResult::Halt { reason, .. } => Ok(CrossCallResult::Halt(
                format!("EVM call halted: {reason:?}").into_bytes(),
            )),
        }
    }

    fn serve<Host: Runtime>(
        &self,
        _registry: &impl Registry,
        _host: &mut Host,
        _request: http::Request<Vec<u8>>,
    ) -> Result<http::Response<Vec<u8>>, TezosXRuntimeError> {
        todo!("EthereumRuntime::serve — will be implemented in a future issue")
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

/// Credits the caller alias with `value` so revm can debit it normally during
/// execution (the real debit already happened on the source runtime), then
/// runs `f`.
///
/// - On success: verifies that revm fully debited the alias — if not,
///   resets the balance to zero and returns an error.
/// - On revert/halt: revm reverted the debit so the temporary credit is
///   still present — reset the balance to zero and return the outcome.
/// - On error: resets the alias balance to zero.
///
/// TODO: L2-885 this should be done using the revm journal.
fn with_temporary_credit<Host: Runtime>(
    caller: Address,
    host: &mut Host,
    value: AlloyU256,
    f: impl FnOnce(&mut Host) -> Result<ExecutionOutcome, TezosXRuntimeError>,
) -> Result<ExecutionOutcome, TezosXRuntimeError> {
    if value == AlloyU256::ZERO {
        return f(host);
    }

    // value > zero
    let mut caller_account = StorageAccount::from_address(&caller)?;
    let mut caller_info = caller_account.info(host)?;
    caller_info.balance = caller_info
        .balance
        .checked_add(value)
        .ok_or_else(|| TezosXRuntimeError::Custom("Balance overflow".to_string()))?;
    caller_account.set_info(host, caller_info)?;

    let outcome = f(host);

    if matches!(
        &outcome,
        Ok(ExecutionOutcome {
            result: ExecutionResult::Success { .. },
            ..
        })
    ) {
        // On success revm debited the alias — verify it's fully spent.
        let caller_account = StorageAccount::from_address(&caller)?;
        let caller_info = caller_account.info(host)?;
        if caller_info.balance != AlloyU256::ZERO {
            let remaining = caller_info.balance;
            reset_balance(host, &caller)?;
            return Err(TezosXRuntimeError::Custom(format!(
                "Caller alias balance not zero after EVM execution: {remaining}"
            )));
        }
    } else {
        // On revert/halt revm reverted the debit, on error we just clean up.
        reset_balance(host, &caller)?;
    }

    outcome
}

fn reset_balance<Host: Runtime>(
    host: &mut Host,
    address: &Address,
) -> Result<(), TezosXRuntimeError> {
    let mut account = StorageAccount::from_address(address)?;
    let mut info = account.info(host)?;
    info.balance = AlloyU256::ZERO;
    account.set_info(host, info)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use alloy_primitives::{hex::FromHex, Bytes, Keccak256};
    use primitive_types::U256;
    use revm::primitives::Address;
    use revm::state::{AccountInfo, Bytecode};
    use revm_etherlink::{
        helpers::storage::bytes_hash,
        storage::{code::CodeStorage, world_state_handler::StorageAccount},
    };
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezosx_interfaces::{
        CrossCallResult, CrossRuntimeContext, Registry, RuntimeId, RuntimeInterface,
        TezosXRuntimeError,
    };

    use crate::EthereumRuntime;

    /// Minimal Registry stub for testing EthereumRuntime in isolation.
    struct StubRegistry;

    impl Registry for StubRegistry {
        fn bridge<Host: tezos_evm_runtime::runtime::Runtime>(
            &self,
            _host: &mut Host,
            _destination_runtime: RuntimeId,
            _destination_address: &[u8],
            _source_address: &[u8],
            _amount: U256,
            _data: &[u8],
            _context: CrossRuntimeContext,
        ) -> Result<CrossCallResult, TezosXRuntimeError> {
            unimplemented!("not needed for this test")
        }

        fn generate_alias<Host: tezos_evm_runtime::runtime::Runtime>(
            &self,
            _host: &mut Host,
            _native_address: &[u8],
            _runtime_id: RuntimeId,
            _context: CrossRuntimeContext,
        ) -> Result<Vec<u8>, TezosXRuntimeError> {
            unimplemented!("not needed for this test")
        }

        fn address_from_string(
            &self,
            _address_str: &str,
            _runtime_id: RuntimeId,
        ) -> Result<Vec<u8>, TezosXRuntimeError> {
            unimplemented!("not needed for this test")
        }
    }

    /// Adapted from `test_simple_transfer` in `revm/src/lib.rs`.
    #[test]
    fn test_call_simple_transfer() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = StubRegistry;

        let caller = Address::from_slice(&[0x11; 20]);
        let destination = Address::from_slice(&[0x22; 20]);
        let value = U256::from(5);

        // The caller alias has no pre-existing balance: the calling runtime
        // already debited the real account, and `call` handles the temporary
        // alias funding.

        let context = CrossRuntimeContext {
            gas_limit: u64::MAX,
            timestamp: U256::from(1),
            block_number: U256::from(1),
        };

        let result = runtime.call(
            &registry,
            &mut host,
            &caller.0 .0,
            &destination.0 .0,
            value,
            &[],
            context,
        );
        assert!(result.is_ok(), "EVM call should succeed: {result:?}");

        // Verify destination received the transfer
        let destination_account = StorageAccount::from_address(&destination).unwrap();
        let info = destination_account.info(&mut host).unwrap();
        assert_eq!(info.balance, revm::primitives::U256::from(5));
    }

    /// Adapted from `test_contract_call_sload_sstore` in `revm/src/lib.rs`.
    #[test]
    fn test_call_executes_contract_bytecode() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = StubRegistry;

        let caller = Address::from_slice(&[0x11; 20]);
        let contract = Address::from_slice(&[0x22; 20]);

        // The caller alias has no pre-existing balance: the calling runtime
        // already debited the real account, and `call` handles the temporary
        // alias funding.

        // Deploy a tiny contract:
        //   PUSH1 0x42   (value to store)
        //   PUSH1 0x01   (storage slot)
        //   SSTORE       (store 0x42 at slot 1)
        //   PUSH1 0x01
        //   SLOAD        (load from slot 1)
        let bytecode_raw = Bytes::from_hex("6042600155600154").unwrap();
        let code_hash = bytes_hash(&bytecode_raw);
        let mut contract_account = StorageAccount::from_address(&contract).unwrap();
        contract_account
            .set_info(
                &mut host,
                AccountInfo {
                    balance: revm::primitives::U256::ZERO,
                    nonce: 0,
                    code_hash,
                    account_id: None,
                    code: Some(Bytecode::new_raw(bytecode_raw.clone())),
                },
            )
            .unwrap();
        CodeStorage::add(&mut host, &bytecode_raw, Some(code_hash)).unwrap();

        let context = CrossRuntimeContext {
            gas_limit: u64::MAX,
            timestamp: U256::from(1),
            block_number: U256::from(1),
        };

        let result = runtime.call(
            &registry,
            &mut host,
            &caller.0 .0,
            &contract.0 .0,
            U256::zero(),
            &[],
            context,
        );
        assert!(result.is_ok(), "EVM call should succeed: {result:?}");

        // Verify the contract wrote 0x42 to storage slot 1
        let slot_value = contract_account
            .get_storage(&host, &revm::primitives::U256::from(1))
            .unwrap();
        assert_eq!(slot_value, revm::primitives::U256::from(0x42));
    }

    /// Test the cross-runtime scenario: caller alias starts with zero balance,
    /// sends value to a contract that reads CALLVALUE and stores it. Verifies:
    /// - the call succeeds despite zero initial caller balance
    /// - the contract sees the correct msg.value (CALLVALUE)
    /// - the caller alias balance is reset to 0 after execution
    #[test]
    fn test_call_with_zero_balance_caller_sets_correct_msg_value() {
        let mut host = MockKernelHost::default();
        let runtime = EthereumRuntime::default();
        let registry = StubRegistry;

        let caller = Address::from_slice(&[0x11; 20]);
        let contract = Address::from_slice(&[0x22; 20]);
        let transfer_value = U256::from(42);

        // Caller starts with zero balance — this is the cross-runtime alias scenario.
        // No set_info call: the alias has no EVM funds.

        // Deploy a contract that stores CALLVALUE to slot 0:
        //   CALLVALUE    (0x34)
        //   PUSH1 0x00   (0x6000)
        //   SSTORE       (0x55)
        let bytecode_raw = Bytes::from_hex("34600055").unwrap();
        let code_hash = bytes_hash(&bytecode_raw);
        let mut contract_account = StorageAccount::from_address(&contract).unwrap();
        contract_account
            .set_info(
                &mut host,
                AccountInfo {
                    balance: revm::primitives::U256::ZERO,
                    nonce: 0,
                    code_hash,
                    account_id: None,
                    code: Some(Bytecode::new_raw(bytecode_raw.clone())),
                },
            )
            .unwrap();
        CodeStorage::add(&mut host, &bytecode_raw, Some(code_hash)).unwrap();

        let context = CrossRuntimeContext {
            gas_limit: u64::MAX,
            timestamp: U256::from(1),
            block_number: U256::from(1),
        };

        let result = runtime.call(
            &registry,
            &mut host,
            &caller.0 .0,
            &contract.0 .0,
            transfer_value,
            &[],
            context,
        );
        assert!(
            matches!(result, Ok(CrossCallResult::Success(_))),
            "EVM call should succeed: {result:?}"
        );

        // Contract stored CALLVALUE at slot 0 — verify it saw the real value
        let stored_value = contract_account
            .get_storage(&host, &revm::primitives::U256::ZERO)
            .unwrap();
        assert_eq!(
            stored_value,
            revm::primitives::U256::from(42),
            "Contract should see msg.value = 42"
        );

        // Contract received the transfer
        let contract_info = contract_account.info(&mut host).unwrap();
        assert_eq!(
            contract_info.balance,
            revm::primitives::U256::from(42),
            "Contract should hold the transferred value"
        );

        // Caller alias balance is reset to 0
        let caller_account = StorageAccount::from_address(&caller).unwrap();
        let caller_info = caller_account.info(&mut host).unwrap();
        assert_eq!(
            caller_info.balance,
            revm::primitives::U256::ZERO,
            "Caller alias balance should be 0 after execution"
        );
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
}
