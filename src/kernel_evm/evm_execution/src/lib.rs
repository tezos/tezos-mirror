// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Types and functions for Ethereum compatibility
//!
//! We need to read and write Ethereum specific values such
//! as addresses and values.
use account_storage::{AccountStorageError, EthereumAccountStorage};
use debug::debug_msg;
use evm::executor::stack::PrecompileFailure;
use evm::{Config, ExitError, ExitFatal};
use host::runtime::Runtime;
use primitive_types::{H160, U256};
use tezos_smart_rollup_storage::StorageError;
use thiserror::Error;

pub mod account_storage;
pub mod block;
pub mod handler;
pub mod precompiles;
pub mod storage;
pub mod transaction;

#[cfg(feature = "testing")]
pub mod testing;

extern crate alloc;
extern crate tezos_crypto_rs as crypto;
extern crate tezos_smart_rollup_debug as debug;
extern crate tezos_smart_rollup_host as host;

use precompiles::PrecompileSet;

/// Errors when processing Ethereum transactions
///
/// What could possibly go wrong? Some of these are place holders for now.
/// When we call an address without code, this should be treated as a simple
/// transfer for instance.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum EthereumError {
    /// EVM returned with a machine error
    #[error("Internal machine error when running contract")]
    MachineExitError(ExitError),
    /// A fatal error from executing EVM.
    #[error("Fatal machine error when running contract")]
    FatalMachineError(ExitFatal),
    /// Calling a precompiled failed (implies there was a precompiled contract
    /// at the call address.
    #[error("Precompile call failed")]
    PrecompileFailed(PrecompileFailure),
    /// Contract did revert
    #[error("Contract call reverted")]
    CallRevert,
    /// The SputnikVM runtime returned a Trap. This should be impossible.
    #[error("Internal SputnikVM trap")]
    InternalTrapError,
    /// Something went wrong when using the durable storage for transactions
    #[error("Error when using durable storage for transactions: {0}")]
    EthereumStorageError(#[from] StorageError),
    /// Something went wrong with an account in durable storage during a transaction
    #[error("Error with an account in durable storage: {0}")]
    EthereumAccountError(#[from] AccountStorageError),
    /// A contract call transferred too much gas to sub-context or contract
    /// call itself got too much gas
    #[error("Gas limit overflow: {0}")]
    GasLimitOverflow(tezos_ethereum::basic::U256),
}

impl From<ExitError> for EthereumError {
    fn from(err: ExitError) -> Self {
        EthereumError::MachineExitError(err)
    }
}

/// Execute an Ethereum Transaction
#[allow(clippy::too_many_arguments)]
pub fn run_transaction<'a, Host>(
    host: &'a mut Host,
    block: &'a block::BlockConstants,
    evm_account_storage: &'a mut EthereumAccountStorage,
    precompiles: &'a precompiles::PrecompileBTreeMap<Host>,
    address: H160,
    caller: H160,
    call_data: Vec<u8>,
    gas_limit: Option<u64>,
    value: Option<U256>,
) -> Result<handler::ExecutionOutcome, EthereumError>
where
    Host: Runtime,
{
    debug_msg!(host, "Going to run an Ethereum transaction");
    debug_msg!(host, " - from address: {}", caller);
    debug_msg!(host, " - to address: {}", address);

    let config = Config::london();
    let mut handler = handler::EvmHandler::<'_, Host>::new(
        host,
        evm_account_storage,
        caller,
        block,
        &config,
        precompiles,
        gas_limit.unwrap_or(0_u64),
    );

    // TODO: check gas_limit vs caller balance. Make sure caller has
    // enough funds to pay for gas.

    if call_data.is_empty() {
        // This is a transfer transaction
        handler.transfer(caller, address, value.unwrap_or(U256::zero()), gas_limit)
    } else if address.is_zero() {
        // This is a create-contract transaction
        handler.create_contract(caller, value, call_data, gas_limit)
    } else {
        // This must be a contract-call transaction
        handler.call_contract(caller, address, value, call_data, gas_limit, false)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use account_storage::{
        account_path, init_account_storage as init_evm_account_storage,
        EthereumAccountStorage,
    };
    use evm::Opcode;
    use handler::ExecutionOutcome;
    use host::runtime::Runtime;
    use primitive_types::H160;
    use std::str::FromStr;
    use tezos_ethereum::address::EthereumAddress;
    use tezos_ethereum::signatures::EthereumTransactionCommon;
    use tezos_smart_rollup_mock::MockHost;

    // The compiled initialization code for the Ethereum demo contract given
    // as an example with <https://remix.ethereum.org>. It is the `Storage.sol`
    // contract.
    const STORAGE_CONTRACT_INITIALIZATION: &str = "608060405234801561001057600080fd5b50610150806100206000396000f3fe608060405234801561001057600080fd5b50600436106100365760003560e01c80632e64cec11461003b5780636057361d14610059575b600080fd5b610043610075565b60405161005091906100a1565b60405180910390f35b610073600480360381019061006e91906100ed565b61007e565b005b60008054905090565b8060008190555050565b6000819050919050565b61009b81610088565b82525050565b60006020820190506100b66000830184610092565b92915050565b600080fd5b6100ca81610088565b81146100d557600080fd5b50565b6000813590506100e7816100c1565b92915050565b600060208284031215610103576101026100bc565b5b6000610111848285016100d8565b9150509291505056fea26469706673582212208f6a6e5a1a593ae1ba29bd21e9d6e9092ae1df1986f8e8de139149a0e99dce1564736f6c63430008120033";

    fn set_balance(
        host: &mut MockHost,
        evm_account_storage: &mut EthereumAccountStorage,
        address: &H160,
        balance: U256,
    ) {
        let mut account = evm_account_storage
            .get_or_create_account(host, &account_path(address).unwrap())
            .unwrap();
        let current_balance = account.balance(host).unwrap();
        if current_balance > balance {
            account
                .balance_remove(host, current_balance - balance)
                .unwrap();
        } else {
            account
                .balance_add(host, balance - current_balance)
                .unwrap();
        }
    }

    fn get_balance(
        host: &mut MockHost,
        evm_account_storage: &mut EthereumAccountStorage,
        address: &H160,
    ) -> U256 {
        let account = evm_account_storage
            .get_or_create_account(host, &account_path(address).unwrap())
            .unwrap();
        account.balance(host).unwrap()
    }

    // Simple utility function to set some code for an account
    fn set_account_code(
        host: &mut impl Runtime,
        evm_account_storage: &mut EthereumAccountStorage,
        address: &H160,
        code: &[u8],
    ) {
        let path = account_path(address).unwrap();
        let mut account = evm_account_storage
            .get_or_create_account(host, &path)
            .unwrap();
        account.set_code(host, code).unwrap();
    }

    #[test]
    fn transfer_without_sufficient_funds_fails() {
        let mut mock_runtime = MockHost::default();
        let block = block::BlockConstants::first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();

        let callee = H160::from_low_u64_be(234213);
        let caller = H160::from_low_u64_be(985493);
        let call_data: Vec<u8> = vec![];
        let transaction_value = U256::from(100_u32);
        let config = Config::london();

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            U256::from(99),
        );
        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &callee,
            U256::from(2),
        );

        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            callee,
            caller,
            call_data,
            None,
            Some(transaction_value),
        );

        let expected_result = Ok(ExecutionOutcome {
            gas_used: config.gas_transaction_call,
            is_success: false,
            new_address: None,
            logs: vec![],
        });

        assert_eq!(expected_result, result);
        assert_eq!(
            get_balance(&mut mock_runtime, &mut evm_account_storage, &caller),
            U256::from(99)
        );
        assert_eq!(
            get_balance(&mut mock_runtime, &mut evm_account_storage, &callee),
            U256::from(2)
        );
    }

    #[test]
    fn transfer_funds_with_sufficient_balance() {
        let mut mock_runtime = MockHost::default();
        let block = block::BlockConstants::first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();

        let callee = H160::from_low_u64_be(82193);
        let caller = H160::from_low_u64_be(1234);
        let call_data: Vec<u8> = vec![];
        let transaction_value = U256::from(100_u32);
        let config = Config::london();

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &callee,
            U256::from(3),
        );
        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            U256::from(101),
        );

        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            callee,
            caller,
            call_data,
            None,
            Some(transaction_value),
        );

        let expected_result = Ok(ExecutionOutcome {
            gas_used: config.gas_transaction_call,
            is_success: true,
            new_address: None,
            logs: vec![],
        });

        assert_eq!(expected_result, result);

        assert_eq!(
            get_balance(&mut mock_runtime, &mut evm_account_storage, &callee),
            U256::from(103)
        );
        assert_eq!(
            get_balance(&mut mock_runtime, &mut evm_account_storage, &caller),
            U256::from(1)
        );
    }

    #[test]
    fn create_contract_fails_with_insufficient_funds() {
        let mut mock_runtime = MockHost::default();
        let block = block::BlockConstants::first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();

        let callee = H160::zero();
        let caller = H160::from_low_u64_be(328794);
        let transaction_value = U256::from(100_u32);
        let call_data: Vec<u8> = hex::decode(STORAGE_CONTRACT_INITIALIZATION).unwrap();

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            U256::from(10),
        );

        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            callee,
            caller,
            call_data,
            None,
            Some(transaction_value),
        );

        let expected_result = Ok(ExecutionOutcome {
            gas_used: 0,
            is_success: false,
            new_address: None,
            logs: vec![],
        });

        assert_eq!(expected_result, result);
        assert_eq!(
            get_balance(&mut mock_runtime, &mut evm_account_storage, &caller),
            U256::from(10)
        );
    }

    #[test]
    fn create_contract_succeeds_with_valid_initialization() {
        let mut mock_runtime = MockHost::default();
        let block = block::BlockConstants::first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();

        let callee = H160::zero();
        let caller = H160::from_low_u64_be(117);
        let transaction_value = U256::from(0);
        let call_data: Vec<u8> = hex::decode(STORAGE_CONTRACT_INITIALIZATION).unwrap();

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            U256::from(0),
        );

        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            callee,
            caller,
            call_data,
            Some(10000),
            Some(transaction_value),
        );

        let expected_result = Ok(ExecutionOutcome {
            gas_used: 117,
            is_success: true,
            new_address: Some(
                H160::from_str("907823e0a92f94355968feb2cbf0fbb594fe3214").unwrap(),
            ),
            logs: vec![],
        });

        assert_eq!(expected_result, result);
    }

    #[test]
    fn create_contract_fails_when_initialization_fails() {
        let mut mock_runtime = MockHost::default();
        let block = block::BlockConstants::first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();

        let callee = H160::zero();
        let caller = H160::from_low_u64_be(2893);
        let transaction_value = U256::from(100_u32);
        // Some EVM instructions. They are all valid, but the last one is opcode
        // 0xFE, which is the designated INVALID opcode, so running this code
        // snippet must fail.
        let call_data: Vec<u8> = hex::decode("602e600055600054600154fe").unwrap();

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            U256::from(100),
        );

        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            callee,
            caller,
            call_data,
            None,
            Some(transaction_value),
        );

        let expected_result = Ok(ExecutionOutcome {
            gas_used: 0,
            is_success: false,
            new_address: None,
            logs: vec![],
        });

        assert_eq!(expected_result, result);
        assert_eq!(
            get_balance(&mut mock_runtime, &mut evm_account_storage, &caller),
            U256::from(100)
        );
    }

    #[test]
    fn call_non_existing_contract() {
        // Arange
        let mut mock_runtime = MockHost::default();
        let block = block::BlockConstants::first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let target = H160::from_low_u64_be(117u64);
        let caller = H160::from_low_u64_be(118u64);
        let data = [0u8; 32]; // Need some data to make it a call

        // Act
        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            target,
            caller,
            data.to_vec(),
            None,
            None,
        );

        // Assert
        let expected_result = Ok(ExecutionOutcome {
            gas_used: 0,
            is_success: true,
            new_address: None,
            logs: vec![],
        });

        assert_eq!(expected_result, result);
    }

    #[test]
    //this is based on https://eips.ethereum.org/EIPS/eip-155
    fn test_signatures() {
        let (sk, _address) = tezos_ethereum::signatures::string_to_sk_and_address(
            "4646464646464646464646464646464646464646464646464646464646464646"
                .to_string(),
        );
        let m: [u8; 32] = hex::decode(
            "daf5a779ae972f972197303d7b574746c7ef83eadac0f2791ad23db92e4c8e53",
        )
        .unwrap()
        .try_into()
        .unwrap();
        let mes = libsecp256k1::Message::parse(&m);
        let (s, _ri) = libsecp256k1::sign(&mes, &sk);
        assert_eq!(
            hex::encode(s.r.b32()),
            "28ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276"
                .to_string()
        );
        assert_eq!(
            hex::encode(s.s.b32()),
            "67cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83"
        )
    }

    #[test]
    fn test_signature_to_address() {
        let test_list = vec![
            (
                "4f3edf983ac636a65a842ce7c78d9aa706d3b113bce9c46f30d7d21715b23b1d",
                "90F8bf6A479f320ead074411a4B0e7944Ea8c9C1",
            ),
            (
                "DC38EE117CAE37750EB1ECC5CFD3DE8E85963B481B93E732C5D0CB66EE6B0C9D",
                "c5ed5d9b9c957be2baa01c16310aa4d1f8bc8e6f",
            ),
            (
                "80b28170e7c2cb2145c052d622ced9de477abcb287e0d23f07263cc30a260534",
                "D0a2dBb5e6F757fd2066a7664f413CAAC504BC95",
            ),
        ];
        test_list.iter().fold((), |_, (s, ea)| {
            let (_, a) =
                tezos_ethereum::signatures::string_to_sk_and_address(s.to_string());
            let value: [u8; 20] = hex::decode(ea).unwrap().try_into().unwrap();
            let ea = EthereumAddress::from(value);
            assert_eq!(a, ea);
        })
    }

    #[test]
    fn test_caller_classic() {
        let (_sk, address_from_sk) = tezos_ethereum::signatures::string_to_sk_and_address(
            "4646464646464646464646464646464646464646464646464646464646464646"
                .to_string(),
        );
        let encoded =
        "f86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83".to_string();
        let transaction = EthereumTransactionCommon::from_rlp(encoded).unwrap();
        let address = transaction.caller();
        let expected_address_string: [u8; 20] =
            hex::decode("9d8A62f656a8d1615C1294fd71e9CFb3E4855A4F")
                .unwrap()
                .try_into()
                .unwrap();
        let expected_address = EthereumAddress::from(expected_address_string);
        assert_eq!(expected_address, address);
        assert_eq!(expected_address, address_from_sk)
    }

    #[test]
    fn test_signed_classic_transaction() {
        let string_sk =
            "4646464646464646464646464646464646464646464646464646464646464646"
                .to_string();
        let encoded =
        "f86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83".to_string();
        let expected_transaction = EthereumTransactionCommon::from_rlp(encoded).unwrap();

        let transaction = expected_transaction.sign_transaction(string_sk);
        assert_eq!(expected_transaction, transaction)
    }

    #[test]
    fn call_simple_return_contract() {
        // Arrange
        let mut mock_runtime = MockHost::default();
        let block = block::BlockConstants::first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let target = H160::from_low_u64_be(117u64);
        let caller = H160::from_low_u64_be(118u64);
        let data = [0u8; 32]; // Need some data to make it a call
        let code = vec![
            Opcode::PUSH1.as_u8(),
            0u8,
            Opcode::PUSH1.as_u8(),
            0u8,
            Opcode::RETURN.as_u8(),
        ];

        set_account_code(&mut mock_runtime, &mut evm_account_storage, &target, &code);

        // Act
        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            target,
            caller,
            data.to_vec(),
            Some(6),
            None,
        );

        // Assert
        let expected_result = Ok(ExecutionOutcome {
            gas_used: 6,
            is_success: true,
            new_address: None,
            logs: vec![],
        });

        assert_eq!(expected_result, result);
    }

    #[test]
    fn call_simple_revert_contract() {
        // Arrange
        let mut mock_runtime = MockHost::default();
        let block = block::BlockConstants::first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let target = H160::from_low_u64_be(117u64);
        let caller = H160::from_low_u64_be(118u64);
        let data = [0u8; 32]; // Need some data to make it a call
        let code = vec![
            Opcode::PUSH1.as_u8(),
            0u8,
            Opcode::PUSH1.as_u8(),
            0u8,
            Opcode::REVERT.as_u8(),
        ];

        set_account_code(&mut mock_runtime, &mut evm_account_storage, &target, &code);

        // Act
        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            target,
            caller,
            data.to_vec(),
            Some(6),
            None,
        );

        // Assert
        let expected_result = Ok(ExecutionOutcome {
            gas_used: 6,
            is_success: false,
            new_address: None,
            logs: vec![],
        });

        assert_eq!(expected_result, result);
    }

    #[test]
    fn call_contract_with_invalid_opcode() {
        // Arrange
        let mut mock_runtime = MockHost::default();
        let block = block::BlockConstants::first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let target = H160::from_low_u64_be(117u64);
        let caller = H160::from_low_u64_be(118u64);
        let data = [0u8; 32]; // Need some data to make it a call
        let code = vec![
            Opcode::INVALID.as_u8(),
            Opcode::PUSH1.as_u8(),
            0u8,
            Opcode::PUSH1.as_u8(),
            0u8,
            Opcode::REVERT.as_u8(),
        ];

        set_account_code(&mut mock_runtime, &mut evm_account_storage, &target, &code);

        // Act
        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            target,
            caller,
            data.to_vec(),
            None,
            None,
        );

        // Assert
        let expected_result = Ok(ExecutionOutcome {
            gas_used: 0,
            is_success: false,
            new_address: None,
            logs: vec![],
        });

        assert_eq!(expected_result, result);
    }

    #[test]
    fn call_precompiled_contract() {
        // Arrange
        let mut mock_runtime = MockHost::default();
        let block = block::BlockConstants::first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let target = H160::from_low_u64_be(4u64);
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let caller = H160::from_low_u64_be(118u64);
        let data = [0u8; 32]; // Need some data to make it a contract call

        // Act
        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            target,
            caller,
            data.to_vec(),
            Some(1),
            None,
        );

        // Assert
        let expected_result = Ok(ExecutionOutcome {
            gas_used: 0,
            is_success: true,
            new_address: None,
            logs: vec![],
        });

        assert_eq!(expected_result, result);
    }

    #[test]
    fn create_and_call_contract() {
        // Arrange
        let mut mock_runtime = MockHost::default();
        let block = block::BlockConstants::first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let target = H160::from_low_u64_be(117u64);
        let caller = H160::from_low_u64_be(118u64);
        let data = [0u8; 32]; // Need some data to make it a contract call

        let code = vec![
            // Create a contract that creates an exception if first word of calldata is 0
            Opcode::PUSH17.as_u8(),
            0x67,
            0x60,
            0x00,
            0x35,
            0x60,
            0x07,
            0x57,
            0xFE,
            0x5B,
            0x60,
            0x00,
            0x52,
            0x60,
            0x08,
            0x60,
            0x18,
            0xF3,
            Opcode::PUSH1.as_u8(),
            0x0,
            Opcode::MSTORE.as_u8(),
            Opcode::PUSH1.as_u8(),
            0x17,
            Opcode::PUSH1.as_u8(),
            0x15,
            Opcode::PUSH1.as_u8(),
            0x0,
            Opcode::CREATE.as_u8(),
            // Call with no parameters, return 0
            Opcode::PUSH1.as_u8(),
            0x0,
            Opcode::PUSH1.as_u8(),
            0x0,
            Opcode::PUSH1.as_u8(),
            0x0,
            Opcode::PUSH1.as_u8(),
            0x0,
            Opcode::PUSH1.as_u8(),
            0x0,
            Opcode::DUP6.as_u8(),
            Opcode::PUSH2.as_u8(),
            0xFF,
            0xFF,
            Opcode::CALL.as_u8(),
            // Call with non 0 calldata, returns success
            Opcode::PUSH1.as_u8(),
            0x0,
            Opcode::PUSH1.as_u8(),
            0x0,
            Opcode::PUSH1.as_u8(),
            0x32,
            Opcode::PUSH1.as_u8(),
            0x0,
            Opcode::PUSH1.as_u8(),
            0x0,
            Opcode::DUP7.as_u8(),
            Opcode::PUSH2.as_u8(),
            0xFF,
            0xFF,
            Opcode::CALL.as_u8(),
        ];

        set_account_code(&mut mock_runtime, &mut evm_account_storage, &target, &code);

        // Act
        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            target,
            caller,
            data.to_vec(),
            None,
            None,
        );

        // Assert
        // let expected_result = Ok(());
        // assert_eq!(result, expected_result);
        assert!(result.is_ok());
    }
}
