// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
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
use tezos_ethereum::block::BlockConstants;
use tezos_smart_rollup_storage::StorageError;
use thiserror::Error;

pub mod account_storage;
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
    GasLimitOverflow(U256),
    /// The transaction stack has an unexpected size.
    ///  - First element tells the stack size at time of error.
    ///  - Second element telss if the error happened when we were expecting to deal
    ///    beginning or end of the initial transaction.
    ///  - The last element tells if the error happended at the beginning of a transaction
    ///    (if false, this happened at the commit or rollback of the transaction).
    #[error(
        "Inconsistent transaction stack: depth is {0}, is_initial is {1}, is_begin is {2}"
    )]
    InconsistentTransactionStack(usize, bool, bool),
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
    block: &'a BlockConstants,
    evm_account_storage: &'a mut EthereumAccountStorage,
    precompiles: &'a precompiles::PrecompileBTreeMap<Host>,
    address: Option<H160>,
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
    debug_msg!(host, " - to address: {:?}", address);

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

    if let Some(address) = address {
        if call_data.is_empty() {
            // This is a transfer transaction
            handler.transfer(caller, address, value.unwrap_or(U256::zero()), gas_limit)
        } else {
            // This must be a contract-call transaction
            handler.call_contract(caller, address, value, call_data, gas_limit, false)
        }
    } else {
        // This is a create-contract transaction
        handler.create_contract(caller, value, call_data, gas_limit)
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
    use tezos_ethereum::signatures::EthereumTransactionCommon;
    use tezos_smart_rollup_mock::MockHost;

    // The compiled initialization code for the Ethereum demo contract given
    // as an example with <https://remix.ethereum.org>. It is the `Storage.sol`
    // contract.
    const STORAGE_CONTRACT_INITIALIZATION: &str = "608060405234801561001057600080fd5b50610150806100206000396000f3fe608060405234801561001057600080fd5b50600436106100365760003560e01c80632e64cec11461003b5780636057361d14610059575b600080fd5b610043610075565b60405161005091906100a1565b60405180910390f35b610073600480360381019061006e91906100ed565b61007e565b005b60008054905090565b8060008190555050565b6000819050919050565b61009b81610088565b82525050565b60006020820190506100b66000830184610092565b92915050565b600080fd5b6100ca81610088565b81146100d557600080fd5b50565b6000813590506100e7816100c1565b92915050565b600060208284031215610103576101026100bc565b5b6000610111848285016100d8565b9150509291505056fea26469706673582212208f6a6e5a1a593ae1ba29bd21e9d6e9092ae1df1986f8e8de139149a0e99dce1564736f6c63430008120033";

    // erc20 contract
    const ERC20_CONTRACT_INITIALISATION: &str = "60806040526040518060400160405280601381526020017f536f6c6964697479206279204578616d706c6500000000000000000000000000815250600390816200004a91906200033c565b506040518060400160405280600781526020017f534f4c4259455800000000000000000000000000000000000000000000000000815250600490816200009191906200033c565b506012600560006101000a81548160ff021916908360ff160217905550348015620000bb57600080fd5b5062000423565b600081519050919050565b7f4e487b7100000000000000000000000000000000000000000000000000000000600052604160045260246000fd5b7f4e487b7100000000000000000000000000000000000000000000000000000000600052602260045260246000fd5b600060028204905060018216806200014457607f821691505b6020821081036200015a5762000159620000fc565b5b50919050565b60008190508160005260206000209050919050565b60006020601f8301049050919050565b600082821b905092915050565b600060088302620001c47fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8262000185565b620001d0868362000185565b95508019841693508086168417925050509392505050565b6000819050919050565b6000819050919050565b60006200021d620002176200021184620001e8565b620001f2565b620001e8565b9050919050565b6000819050919050565b6200023983620001fc565b62000251620002488262000224565b84845462000192565b825550505050565b600090565b6200026862000259565b620002758184846200022e565b505050565b5b818110156200029d57620002916000826200025e565b6001810190506200027b565b5050565b601f821115620002ec57620002b68162000160565b620002c18462000175565b81016020851015620002d1578190505b620002e9620002e08562000175565b8301826200027a565b50505b505050565b600082821c905092915050565b60006200031160001984600802620002f1565b1980831691505092915050565b60006200032c8383620002fe565b9150826002028217905092915050565b6200034782620000c2565b67ffffffffffffffff811115620003635762000362620000cd565b5b6200036f82546200012b565b6200037c828285620002a1565b600060209050601f831160018114620003b457600084156200039f578287015190505b620003ab85826200031e565b8655506200041b565b601f198416620003c48662000160565b60005b82811015620003ee57848901518255600182019150602085019450602081019050620003c7565b868310156200040e57848901516200040a601f891682620002fe565b8355505b6001600288020188555050505b505050505050565b610d6a80620004336000396000f3fe608060405234801561001057600080fd5b50600436106100a95760003560e01c806342966c681161007157806342966c681461016857806370a082311461018457806395d89b41146101b4578063a0712d68146101d2578063a9059cbb146101ee578063dd62ed3e1461021e576100a9565b806306fdde03146100ae578063095ea7b3146100cc57806318160ddd146100fc57806323b872dd1461011a578063313ce5671461014a575b600080fd5b6100b661024e565b6040516100c391906109be565b60405180910390f35b6100e660048036038101906100e19190610a79565b6102dc565b6040516100f39190610ad4565b60405180910390f35b6101046103ce565b6040516101119190610afe565b60405180910390f35b610134600480360381019061012f9190610b19565b6103d4565b6040516101419190610ad4565b60405180910390f35b610152610585565b60405161015f9190610b88565b60405180910390f35b610182600480360381019061017d9190610ba3565b610598565b005b61019e60048036038101906101999190610bd0565b61066f565b6040516101ab9190610afe565b60405180910390f35b6101bc610687565b6040516101c991906109be565b60405180910390f35b6101ec60048036038101906101e79190610ba3565b610715565b005b61020860048036038101906102039190610a79565b6107ec565b6040516102159190610ad4565b60405180910390f35b61023860048036038101906102339190610bfd565b610909565b6040516102459190610afe565b60405180910390f35b6003805461025b90610c6c565b80601f016020809104026020016040519081016040528092919081815260200182805461028790610c6c565b80156102d45780601f106102a9576101008083540402835291602001916102d4565b820191906000526020600020905b8154815290600101906020018083116102b757829003601f168201915b505050505081565b600081600260003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055508273ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff167f8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925846040516103bc9190610afe565b60405180910390a36001905092915050565b60005481565b600081600260008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008282546104629190610ccc565b9250508190555081600160008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008282546104b89190610ccc565b9250508190555081600160008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020600082825461050e9190610d00565b925050819055508273ffffffffffffffffffffffffffffffffffffffff168473ffffffffffffffffffffffffffffffffffffffff167fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef846040516105729190610afe565b60405180910390a3600190509392505050565b600560009054906101000a900460ff1681565b80600160003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008282546105e79190610ccc565b92505081905550806000808282546105ff9190610ccc565b92505081905550600073ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff167fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef836040516106649190610afe565b60405180910390a350565b60016020528060005260406000206000915090505481565b6004805461069490610c6c565b80601f01602080910402602001604051908101604052809291908181526020018280546106c090610c6c565b801561070d5780601f106106e25761010080835404028352916020019161070d565b820191906000526020600020905b8154815290600101906020018083116106f057829003601f168201915b505050505081565b80600160003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008282546107649190610d00565b925050819055508060008082825461077c9190610d00565b925050819055503373ffffffffffffffffffffffffffffffffffffffff16600073ffffffffffffffffffffffffffffffffffffffff167fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef836040516107e19190610afe565b60405180910390a350565b600081600160003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020600082825461083d9190610ccc565b9250508190555081600160008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008282546108939190610d00565b925050819055508273ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff167fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef846040516108f79190610afe565b60405180910390a36001905092915050565b6002602052816000526040600020602052806000526040600020600091509150505481565b600081519050919050565b600082825260208201905092915050565b60005b8381101561096857808201518184015260208101905061094d565b60008484015250505050565b6000601f19601f8301169050919050565b60006109908261092e565b61099a8185610939565b93506109aa81856020860161094a565b6109b381610974565b840191505092915050565b600060208201905081810360008301526109d88184610985565b905092915050565b600080fd5b600073ffffffffffffffffffffffffffffffffffffffff82169050919050565b6000610a10826109e5565b9050919050565b610a2081610a05565b8114610a2b57600080fd5b50565b600081359050610a3d81610a17565b92915050565b6000819050919050565b610a5681610a43565b8114610a6157600080fd5b50565b600081359050610a7381610a4d565b92915050565b60008060408385031215610a9057610a8f6109e0565b5b6000610a9e85828601610a2e565b9250506020610aaf85828601610a64565b9150509250929050565b60008115159050919050565b610ace81610ab9565b82525050565b6000602082019050610ae96000830184610ac5565b92915050565b610af881610a43565b82525050565b6000602082019050610b136000830184610aef565b92915050565b600080600060608486031215610b3257610b316109e0565b5b6000610b4086828701610a2e565b9350506020610b5186828701610a2e565b9250506040610b6286828701610a64565b9150509250925092565b600060ff82169050919050565b610b8281610b6c565b82525050565b6000602082019050610b9d6000830184610b79565b92915050565b600060208284031215610bb957610bb86109e0565b5b6000610bc784828501610a64565b91505092915050565b600060208284031215610be657610be56109e0565b5b6000610bf484828501610a2e565b91505092915050565b60008060408385031215610c1457610c136109e0565b5b6000610c2285828601610a2e565b9250506020610c3385828601610a2e565b9150509250929050565b7f4e487b7100000000000000000000000000000000000000000000000000000000600052602260045260246000fd5b60006002820490506001821680610c8457607f821691505b602082108103610c9757610c96610c3d565b5b50919050565b7f4e487b7100000000000000000000000000000000000000000000000000000000600052601160045260246000fd5b6000610cd782610a43565b9150610ce283610a43565b9250828203905081811115610cfa57610cf9610c9d565b5b92915050565b6000610d0b82610a43565b9150610d1683610a43565b9250828201905080821115610d2e57610d2d610c9d565b5b9291505056fea26469706673582212207b919b45bc1fe90bc3f638a6d1e91b799fb6d2f27c0f6ebaa634fc5258371a1a64736f6c63430008120033";

    fn set_balance(
        host: &mut MockHost,
        evm_account_storage: &mut EthereumAccountStorage,
        address: &H160,
        balance: U256,
    ) {
        let mut account = evm_account_storage
            .get_or_create(host, &account_path(address).unwrap())
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
            .get_or_create(host, &account_path(address).unwrap())
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
        let mut account = evm_account_storage.get_or_create(host, &path).unwrap();
        account.set_code(host, code).unwrap();
    }

    #[test]
    fn transfer_without_sufficient_funds_fails() {
        let mut mock_runtime = MockHost::default();
        let block = BlockConstants::first_block();
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
            Some(callee),
            caller,
            call_data,
            Some(22000),
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
        let block = BlockConstants::first_block();
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
            Some(callee),
            caller,
            call_data,
            Some(21000),
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
        let block = BlockConstants::first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();

        let callee = None;
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
        let block = BlockConstants::first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();

        let callee = None;
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
    fn create_contract_erc20_succeeds() {
        let mut mock_runtime = MockHost::default();
        let block = BlockConstants::first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();

        let callee = None;
        let caller = H160::from_low_u64_be(117);
        let transaction_value = U256::from(0);
        let call_data: Vec<u8> = hex::decode(ERC20_CONTRACT_INITIALISATION).unwrap();

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
            Some(100000),
            Some(transaction_value),
        );

        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(result.is_success);
        assert_eq!(
            Some(H160::from_str("907823e0a92f94355968feb2cbf0fbb594fe3214").unwrap()),
            result.new_address
        );
    }

    #[test]
    fn create_contract_fails_when_initialization_fails() {
        let mut mock_runtime = MockHost::default();
        let block = BlockConstants::first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();

        let callee = None;
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
        let block = BlockConstants::first_block();
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
            Some(target),
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
        )
        .unwrap();
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
                tezos_ethereum::signatures::string_to_sk_and_address(s.to_string())
                    .unwrap();
            let value: [u8; 20] = hex::decode(ea).unwrap().try_into().unwrap();
            let ea = value.into();
            assert_eq!(a, ea);
        })
    }

    #[test]
    fn test_caller_classic() {
        let (_sk, address_from_sk) =
            tezos_ethereum::signatures::string_to_sk_and_address(
                "4646464646464646464646464646464646464646464646464646464646464646"
                    .to_string(),
            )
            .unwrap();
        let encoded =
        "f86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83".to_string();
        let transaction = EthereumTransactionCommon::from_rlp(encoded).unwrap();
        let address = transaction.caller().unwrap();
        let expected_address_string: [u8; 20] =
            hex::decode("9d8A62f656a8d1615C1294fd71e9CFb3E4855A4F")
                .unwrap()
                .try_into()
                .unwrap();
        let expected_address: H160 = expected_address_string.into();
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

        let transaction = expected_transaction.sign_transaction(string_sk).unwrap();
        assert_eq!(expected_transaction, transaction)
    }

    #[test]
    fn call_simple_return_contract() {
        // Arrange
        let mut mock_runtime = MockHost::default();
        let block = BlockConstants::first_block();
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
            Some(target),
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
        let block = BlockConstants::first_block();
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
            Some(target),
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
        let block = BlockConstants::first_block();
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
            Some(target),
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
    fn no_transfer_when_contract_call_fails() {
        let mut mock_runtime = MockHost::default();
        let block = BlockConstants::first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let caller = H160::from_low_u64_be(118_u64);

        let address = H160::from_low_u64_be(117_u64);
        let input = vec![0_u8];
        let code: Vec<u8> = vec![
            Opcode::PUSH1.as_u8(),
            0u8,
            Opcode::PUSH1.as_u8(),
            0u8,
            Opcode::INVALID.as_u8(),
        ];

        set_account_code(&mut mock_runtime, &mut evm_account_storage, &address, &code);
        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            U256::from(101_u32),
        );

        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            Some(address),
            caller,
            input,
            None,
            Some(U256::from(100)),
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
            U256::from(101_u32)
        );
        assert_eq!(
            get_balance(&mut mock_runtime, &mut evm_account_storage, &address),
            U256::zero()
        );
    }

    #[test]
    fn call_precompiled_contract() {
        // Arrange
        let mut mock_runtime = MockHost::default();
        let block = BlockConstants::first_block();
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
            Some(target),
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
        let block = BlockConstants::first_block();
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
            Some(target),
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
