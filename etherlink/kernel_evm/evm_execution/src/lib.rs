// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Types and functions for Ethereum compatibility
//!
//! We need to read and write Ethereum specific values such
//! as addresses and values.
use account_storage::{AccountStorageError, EthereumAccountStorage};
use alloc::borrow::Cow;
use alloc::collections::TryReserveError;
use evm::executor::stack::PrecompileFailure;
use evm::ExitReason;
use host::runtime::Runtime;
use primitive_types::{H160, U256};
use tezos_ethereum::block::BlockConstants;
use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup_storage::StorageError;
use thiserror::Error;

pub mod abi;
pub mod account_storage;
pub mod handler;
pub mod modexp;
pub mod precompiles;
pub mod storage;
pub mod tick_model_opcodes;
pub mod transaction;
pub mod utilities;
pub mod zk_precompiled;

pub use evm::Config;

extern crate alloc;
extern crate tezos_crypto_rs as crypto;
extern crate tezos_smart_rollup_debug as debug;
extern crate tezos_smart_rollup_host as host;

use precompiles::PrecompileSet;

use crate::handler::ExtendedExitReason;

#[derive(Error, Clone, Copy, Debug, Eq, PartialEq)]
pub enum DurableStorageError {
    /// Some runtime error happened while using durable storage
    #[error("Runtime error: {0:?}")]
    RuntimeError(#[from] host::runtime::RuntimeError),
    /// Some error happened while constructing the path to some
    /// resource.
    #[error("Path error: {0:?}")]
    PathError(#[from] host::path::PathError),
}

/// Errors when processing Ethereum transactions
///
/// What could possibly go wrong? Some of these are place holders for now.
/// When we call an address without code, this should be treated as a simple
/// transfer for instance.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum EthereumError {
    /// An ethereum error happened inside a callback and we had to print it to
    /// a string so we could wrap it in a `ExitFatal` error. We have lost the
    /// exact variant, but we can retain the message.
    #[error("Wrapped Ethereum error: {0}")]
    WrappedError(Cow<'static, str>),
    /// Calling a precompiled failed (implies there was a precompiled contract
    /// at the call address.
    #[error("Precompile call failed")]
    PrecompileFailed(PrecompileFailure),
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
    /// The transaction data stack has an unexpected size. It should be the same size as
    /// the transaction stack, but it isn't.
    ///  - first argument is transaction depth,
    ///  - second argument is the transaction data size.
    #[error(
        "Inconsistent transaction data size: transaction depth is {0}, transaction info depth {1}"
    )]
    InconsistentTransactionData(usize, usize),
    /// Memory allocation error. Could not expand the capacity of a vector.
    #[error("Vector expand error: {0}")]
    VectorExpandError(#[from] TryReserveError),
    /// The execution state is inconsistent in some way. This can only happen as a
    /// result of a bug in the EvmHandler.
    #[error("Inconsistent EvmHandler state: {0}")]
    InconsistentState(Cow<'static, str>),
    /// The execution failed because it spent more ticks than the one currently
    /// available for the current run.
    #[error("The transaction took more ticks than expected")]
    OutOfTicks,
    /// gas_limit * gas_price > u64::max
    #[error("Gas payment overflowed u64::max")]
    GasPaymentOverflow,
    /// Converting non-execution fees to gas overflowed u64::max
    #[error("Gas for fees overflowed u64::max in conversion")]
    FeesToGasOverflow,
    /// Underflow of gas limit when subtracting gas for fees
    #[error("Insufficient gas to cover the non-execution fees")]
    GasToFeesUnderflow,
}

/// Execute an Ethereum Transaction
///
/// The function returns `Err` only if something is wrong with the kernel and/or the
/// rollup node. If the transaction ends by executing STOP, RETURN or SUICIDE, then this is
/// a _success_ (by Ethereum definition). Note that a REVERT instruction _can_ return
/// data even though it will mean rollback of the transaction effect. This is also true
/// for sub-transactions, ie, REVERT can _always_ return data.
///
/// If the gas limit is given as `None` (there is no gas limit), then there will be no
/// accounting for gas usage at all. So the gas usage in the return value will be zero.
#[allow(clippy::too_many_arguments)]
pub fn run_transaction<'a, Host>(
    host: &'a mut Host,
    block: &'a BlockConstants,
    evm_account_storage: &'a mut EthereumAccountStorage,
    precompiles: &'a precompiles::PrecompileBTreeMap<Host>,
    config: Config,
    address: Option<H160>,
    caller: H160,
    call_data: Vec<u8>,
    gas_limit: Option<u64>,
    effective_gas_price: U256,
    value: Option<U256>,
    pay_for_gas: bool,
    allocated_ticks: u64,
    retriable: bool,
) -> Result<Option<handler::ExecutionOutcome>, EthereumError>
where
    Host: Runtime,
{
    fn do_refund(outcome: &handler::ExecutionOutcome, pay_for_gas: bool) -> bool {
        match outcome.reason {
            ExtendedExitReason::Exit(ExitReason::Revert(_))
            | ExtendedExitReason::OutOfTicks => pay_for_gas,
            _ => pay_for_gas && outcome.is_success,
        }
    }

    log!(host, Info, "Going to run an Ethereum transaction\n  - from address: {}\n  - to address: {:?}", caller, address);

    let mut handler = handler::EvmHandler::<'_, Host>::new(
        host,
        evm_account_storage,
        caller,
        block,
        &config,
        precompiles,
        allocated_ticks,
        effective_gas_price,
    );

    if (!pay_for_gas)
        || handler.pre_pay_transactions(caller, gas_limit, effective_gas_price)?
    {
        let result = if let Some(address) = address {
            handler.call_contract(caller, address, value, call_data, gas_limit, false)
        } else {
            // This is a create-contract transaction
            handler.create_contract(caller, value, call_data, gas_limit)
        };

        match result {
            Ok(result) => {
                if !(result.reason == ExtendedExitReason::OutOfTicks && retriable) {
                    handler.increment_nonce(caller)?;
                }

                if do_refund(&result, pay_for_gas) {
                    // In case of `OutOfTicks` and the transaction can be
                    // retried, the gas is entirely refunded as it will be
                    // repaid in the next attempt
                    if result.reason == ExtendedExitReason::OutOfTicks && retriable {
                        log!(
                            handler.borrow_host(),
                            Debug,
                            "The transaction exhausted the ticks of the \
                             current reboot and is retriable: refunding all the gas."
                        );
                        handler.repay_gas(caller, gas_limit, effective_gas_price)?;
                    } else {
                        let unused_gas = gas_limit.map(|gl| gl - result.gas_used);
                        handler.repay_gas(caller, unused_gas, effective_gas_price)?;
                    }
                }

                Ok(Some(result))
            }
            Err(e) => Err(e),
        }
    } else {
        // caller was unable to pay for the gas limit
        if pay_for_gas {
            log!(host, Info, "Caller was unable to pre-pay the transaction")
        };
        Ok(None)
    }
}

#[cfg(test)]
mod test {
    use crate::account_storage::EthereumAccount;

    use super::*;
    use account_storage::{
        account_path, init_account_storage as init_evm_account_storage,
        EthereumAccountStorage,
    };
    use evm::executor::stack::Log;
    use evm::{ExitError, ExitReason, ExitRevert, ExitSucceed, Opcode};
    use handler::ExecutionOutcome;
    use host::runtime::Runtime;
    use primitive_types::{H160, H256};
    use std::str::FromStr;
    use std::vec;
    use tezos_ethereum::block::BlockFees;
    use tezos_ethereum::tx_common::EthereumTransactionCommon;
    use tezos_smart_rollup_mock::MockHost;

    // The compiled initialization code for the Ethereum demo contract given
    // as an example in kernel_evm/solidity_examples/storage.sol
    const STORAGE_CONTRACT_INITIALIZATION: &str = "608060405234801561001057600080fd5b5061017f806100206000396000f3fe608060405234801561001057600080fd5b50600436106100415760003560e01c80634e70b1dc1461004657806360fe47b1146100645780636d4ce63c14610080575b600080fd5b61004e61009e565b60405161005b91906100d0565b60405180910390f35b61007e6004803603810190610079919061011c565b6100a4565b005b6100886100ae565b60405161009591906100d0565b60405180910390f35b60005481565b8060008190555050565b60008054905090565b6000819050919050565b6100ca816100b7565b82525050565b60006020820190506100e560008301846100c1565b92915050565b600080fd5b6100f9816100b7565b811461010457600080fd5b50565b600081359050610116816100f0565b92915050565b600060208284031215610132576101316100eb565b5b600061014084828501610107565b9150509291505056fea2646970667358221220ec57e49a647342208a1f5c9b1f2049bf1a27f02e19940819f38929bf67670a5964736f6c63430008120033";
    // call: num
    const STORAGE_CONTRACT_CALL_NUM: &str = "4e70b1dc";
    // call: set(42)
    const STORAGE_CONTRACT_CALL_SET42: &str =
        "60fe47b1000000000000000000000000000000000000000000000000000000000000002a";

    const CONFIG: Config = Config::shanghai();

    // The compiled initialization code for the Ethereum demo contract given
    // as an example in kernel_evm/solidity_examples/erc20tok.sol
    const ERC20_CONTRACT_INITIALISATION: &str = "60806040526040518060400160405280601381526020017f536f6c6964697479206279204578616d706c6500000000000000000000000000815250600390816200004a91906200033c565b506040518060400160405280600781526020017f534f4c4259455800000000000000000000000000000000000000000000000000815250600490816200009191906200033c565b506012600560006101000a81548160ff021916908360ff160217905550348015620000bb57600080fd5b5062000423565b600081519050919050565b7f4e487b7100000000000000000000000000000000000000000000000000000000600052604160045260246000fd5b7f4e487b7100000000000000000000000000000000000000000000000000000000600052602260045260246000fd5b600060028204905060018216806200014457607f821691505b6020821081036200015a5762000159620000fc565b5b50919050565b60008190508160005260206000209050919050565b60006020601f8301049050919050565b600082821b905092915050565b600060088302620001c47fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8262000185565b620001d0868362000185565b95508019841693508086168417925050509392505050565b6000819050919050565b6000819050919050565b60006200021d620002176200021184620001e8565b620001f2565b620001e8565b9050919050565b6000819050919050565b6200023983620001fc565b62000251620002488262000224565b84845462000192565b825550505050565b600090565b6200026862000259565b620002758184846200022e565b505050565b5b818110156200029d57620002916000826200025e565b6001810190506200027b565b5050565b601f821115620002ec57620002b68162000160565b620002c18462000175565b81016020851015620002d1578190505b620002e9620002e08562000175565b8301826200027a565b50505b505050565b600082821c905092915050565b60006200031160001984600802620002f1565b1980831691505092915050565b60006200032c8383620002fe565b9150826002028217905092915050565b6200034782620000c2565b67ffffffffffffffff811115620003635762000362620000cd565b5b6200036f82546200012b565b6200037c828285620002a1565b600060209050601f831160018114620003b457600084156200039f578287015190505b620003ab85826200031e565b8655506200041b565b601f198416620003c48662000160565b60005b82811015620003ee57848901518255600182019150602085019450602081019050620003c7565b868310156200040e57848901516200040a601f891682620002fe565b8355505b6001600288020188555050505b505050505050565b610d6a80620004336000396000f3fe608060405234801561001057600080fd5b50600436106100a95760003560e01c806342966c681161007157806342966c681461016857806370a082311461018457806395d89b41146101b4578063a0712d68146101d2578063a9059cbb146101ee578063dd62ed3e1461021e576100a9565b806306fdde03146100ae578063095ea7b3146100cc57806318160ddd146100fc57806323b872dd1461011a578063313ce5671461014a575b600080fd5b6100b661024e565b6040516100c391906109be565b60405180910390f35b6100e660048036038101906100e19190610a79565b6102dc565b6040516100f39190610ad4565b60405180910390f35b6101046103ce565b6040516101119190610afe565b60405180910390f35b610134600480360381019061012f9190610b19565b6103d4565b6040516101419190610ad4565b60405180910390f35b610152610585565b60405161015f9190610b88565b60405180910390f35b610182600480360381019061017d9190610ba3565b610598565b005b61019e60048036038101906101999190610bd0565b61066f565b6040516101ab9190610afe565b60405180910390f35b6101bc610687565b6040516101c991906109be565b60405180910390f35b6101ec60048036038101906101e79190610ba3565b610715565b005b61020860048036038101906102039190610a79565b6107ec565b6040516102159190610ad4565b60405180910390f35b61023860048036038101906102339190610bfd565b610909565b6040516102459190610afe565b60405180910390f35b6003805461025b90610c6c565b80601f016020809104026020016040519081016040528092919081815260200182805461028790610c6c565b80156102d45780601f106102a9576101008083540402835291602001916102d4565b820191906000526020600020905b8154815290600101906020018083116102b757829003601f168201915b505050505081565b600081600260003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055508273ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff167f8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925846040516103bc9190610afe565b60405180910390a36001905092915050565b60005481565b600081600260008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008282546104629190610ccc565b9250508190555081600160008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008282546104b89190610ccc565b9250508190555081600160008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020600082825461050e9190610d00565b925050819055508273ffffffffffffffffffffffffffffffffffffffff168473ffffffffffffffffffffffffffffffffffffffff167fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef846040516105729190610afe565b60405180910390a3600190509392505050565b600560009054906101000a900460ff1681565b80600160003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008282546105e79190610ccc565b92505081905550806000808282546105ff9190610ccc565b92505081905550600073ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff167fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef836040516106649190610afe565b60405180910390a350565b60016020528060005260406000206000915090505481565b6004805461069490610c6c565b80601f01602080910402602001604051908101604052809291908181526020018280546106c090610c6c565b801561070d5780601f106106e25761010080835404028352916020019161070d565b820191906000526020600020905b8154815290600101906020018083116106f057829003601f168201915b505050505081565b80600160003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008282546107649190610d00565b925050819055508060008082825461077c9190610d00565b925050819055503373ffffffffffffffffffffffffffffffffffffffff16600073ffffffffffffffffffffffffffffffffffffffff167fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef836040516107e19190610afe565b60405180910390a350565b600081600160003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020600082825461083d9190610ccc565b9250508190555081600160008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008282546108939190610d00565b925050819055508273ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff167fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef846040516108f79190610afe565b60405180910390a36001905092915050565b6002602052816000526040600020602052806000526040600020600091509150505481565b600081519050919050565b600082825260208201905092915050565b60005b8381101561096857808201518184015260208101905061094d565b60008484015250505050565b6000601f19601f8301169050919050565b60006109908261092e565b61099a8185610939565b93506109aa81856020860161094a565b6109b381610974565b840191505092915050565b600060208201905081810360008301526109d88184610985565b905092915050565b600080fd5b600073ffffffffffffffffffffffffffffffffffffffff82169050919050565b6000610a10826109e5565b9050919050565b610a2081610a05565b8114610a2b57600080fd5b50565b600081359050610a3d81610a17565b92915050565b6000819050919050565b610a5681610a43565b8114610a6157600080fd5b50565b600081359050610a7381610a4d565b92915050565b60008060408385031215610a9057610a8f6109e0565b5b6000610a9e85828601610a2e565b9250506020610aaf85828601610a64565b9150509250929050565b60008115159050919050565b610ace81610ab9565b82525050565b6000602082019050610ae96000830184610ac5565b92915050565b610af881610a43565b82525050565b6000602082019050610b136000830184610aef565b92915050565b600080600060608486031215610b3257610b316109e0565b5b6000610b4086828701610a2e565b9350506020610b5186828701610a2e565b9250506040610b6286828701610a64565b9150509250925092565b600060ff82169050919050565b610b8281610b6c565b82525050565b6000602082019050610b9d6000830184610b79565b92915050565b600060208284031215610bb957610bb86109e0565b5b6000610bc784828501610a64565b91505092915050565b600060208284031215610be657610be56109e0565b5b6000610bf484828501610a2e565b91505092915050565b60008060408385031215610c1457610c136109e0565b5b6000610c2285828601610a2e565b9250506020610c3385828601610a2e565b9150509250929050565b7f4e487b7100000000000000000000000000000000000000000000000000000000600052602260045260246000fd5b60006002820490506001821680610c8457607f821691505b602082108103610c9757610c96610c3d565b5b50919050565b7f4e487b7100000000000000000000000000000000000000000000000000000000600052601160045260246000fd5b6000610cd782610a43565b9150610ce283610a43565b9250828203905081811115610cfa57610cf9610c9d565b5b92915050565b6000610d0b82610a43565b9150610d1683610a43565b9250828201905080821115610d2e57610d2d610c9d565b5b9291505056fea26469706673582212207b919b45bc1fe90bc3f638a6d1e91b799fb6d2f27c0f6ebaa634fc5258371a1a64736f6c63430008120033";

    const DUMMY_ALLOCATED_TICKS: u64 = 1_000_000_000;

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

    fn bump_nonce(
        host: &mut impl Runtime,
        evm_account_storage: &mut EthereumAccountStorage,
        address: &H160,
    ) {
        let path = account_path(address).unwrap();
        let mut account = evm_account_storage.get_or_create(host, &path).unwrap();
        account.increment_nonce(host).unwrap();
    }

    fn get_nonce(
        host: &mut MockHost,
        evm_account_storage: &mut EthereumAccountStorage,
        address: &H160,
    ) -> U256 {
        let account = evm_account_storage
            .get_or_create(host, &account_path(address).unwrap())
            .unwrap();
        account.nonce(host).unwrap()
    }

    fn dummy_first_block() -> BlockConstants {
        let block_fees = BlockFees::new(U256::from(12345), U256::from(500_000));
        BlockConstants::first_block(U256::zero(), U256::one(), block_fees)
    }

    #[test]
    fn transfer_without_sufficient_funds_fails() {
        let mut mock_runtime = MockHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();

        let callee = H160::from_low_u64_be(234213);
        let caller = H160::from_low_u64_be(985493);
        let call_data: Vec<u8> = vec![];
        let transaction_value = U256::from(100_u32);
        let config = Config::shanghai();
        let gas_price = U256::from(1);

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            U256::from(22099),
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
            CONFIG,
            Some(callee),
            caller,
            call_data,
            Some(22000),
            gas_price,
            Some(transaction_value),
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        let expected_result = Ok(Some(ExecutionOutcome {
            gas_used: config.gas_transaction_call,
            is_success: false,
            reason: ExitReason::Error(ExitError::OutOfFund).into(),
            new_address: None,
            logs: vec![],
            result: None,
            withdrawals: vec![],
            estimated_ticks_used: 0,
        }));

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
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();

        let callee = H160::from_low_u64_be(82193);
        let caller = H160::from_low_u64_be(1234);
        let call_data: Vec<u8> = vec![];
        let transaction_value = U256::from(100_u32);
        let config = Config::shanghai();
        let gas_price = U256::from(1);

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
            U256::from(21101),
        );

        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            Some(callee),
            caller,
            call_data,
            Some(21000),
            gas_price,
            Some(transaction_value),
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        let expected_result = Ok(Some(ExecutionOutcome {
            gas_used: config.gas_transaction_call,
            is_success: true,
            reason: ExitReason::Succeed(ExitSucceed::Stopped).into(),
            new_address: None,
            logs: vec![],
            result: Some(vec![]),
            withdrawals: vec![],
            estimated_ticks_used: 0,
        }));

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
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();

        let callee = None;
        let caller = H160::from_low_u64_be(328794);
        let transaction_value = U256::from(100_u32);
        let call_data: Vec<u8> = hex::decode(STORAGE_CONTRACT_INITIALIZATION).unwrap();
        let gas_price = U256::from(21000);

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
            CONFIG,
            callee,
            caller,
            call_data,
            None,
            gas_price,
            Some(transaction_value),
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        let expected_result = Ok(Some(ExecutionOutcome {
            gas_used: 0,
            is_success: false,
            reason: ExitReason::Error(ExitError::OutOfFund).into(),
            new_address: None,
            logs: vec![],
            result: None,
            withdrawals: vec![],
            estimated_ticks_used: 0,
        }));

        assert_eq!(expected_result, result);
        assert_eq!(
            get_balance(&mut mock_runtime, &mut evm_account_storage, &caller),
            U256::from(10)
        );
    }

    #[test]
    fn create_contract_succeeds_with_valid_initialization() {
        let mut mock_runtime = MockHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();

        let callee = None;
        let caller = H160::from_low_u64_be(117);
        let transaction_value = U256::from(0);
        let call_data: Vec<u8> = hex::decode(STORAGE_CONTRACT_INITIALIZATION).unwrap();
        let gas_price = U256::from(1);

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            U256::from(1_000_000),
        );
        // gas limit was estimated using Remix on Shanghai network (256,842)
        // plus a safety margin for gas accounting discrepancies
        let gas_limit = 300_000;
        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            callee,
            caller,
            call_data,
            Some(gas_limit),
            gas_price,
            Some(transaction_value),
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        let new_address =
            Some(H160::from_str("907823e0a92f94355968feb2cbf0fbb594fe3214").unwrap());

        assert!(result.is_ok(), "execution should have succeeded");
        let result = result.unwrap();
        assert!(
            result.is_some(),
            "execution should have produced some outcome"
        );
        let result = result.unwrap();
        assert!(result.is_success, "transaction should have succeeded");
        assert_eq!(
            new_address, result.new_address,
            "Contract addess not its expected value"
        );

        // test of a call
        let call_data2 = hex::decode(STORAGE_CONTRACT_CALL_NUM).unwrap();
        let result2 = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            new_address,
            caller,
            call_data2,
            Some(31000),
            gas_price,
            Some(U256::zero()),
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );
        assert!(result2.is_ok(), "execution should have succeeded");
        let result = result2.unwrap();
        assert!(
            result.is_some(),
            "execution should have produced some outcome"
        );
        let result = result.unwrap();
        assert!(result.is_success, "transaction should have succeeded");
        assert!(result.result.is_some(), "Call should have returned a value");
        let value = U256::from_little_endian(result.result.unwrap().as_slice());
        assert_eq!(U256::zero(), value, "unexpected result value");

        let call_data_set = hex::decode(STORAGE_CONTRACT_CALL_SET42).unwrap();
        let result3 = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            new_address,
            caller,
            call_data_set,
            Some(100000),
            gas_price,
            Some(U256::zero()),
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );
        assert!(result3.is_ok(), "execution should have succeeded");
        let result = result3.unwrap();
        assert!(
            result.is_some(),
            "execution should have produced some outcome"
        );
        let result = result.unwrap();
        assert!(result.is_success, "transaction should have succeeded");
        assert!(result.result.is_some(), "Call should have returned a value");
        let value = U256::from_big_endian(result.result.unwrap().as_slice());
        assert_eq!(U256::zero(), value, "unexpected result value");

        let result2 = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            new_address,
            caller,
            hex::decode(STORAGE_CONTRACT_CALL_NUM).unwrap(),
            Some(31000),
            gas_price,
            Some(U256::zero()),
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );
        assert!(result2.is_ok(), "execution should have succeeded");
        let result = result2.unwrap();
        assert!(
            result.is_some(),
            "execution should have produced some outcome"
        );
        let result = result.unwrap();
        assert!(result.is_success, "transaction should have succeeded");
        assert!(result.result.is_some(), "Call should have returned a value");
        let value = U256::from_big_endian(result.result.unwrap().as_slice());
        assert_eq!(U256::from(42), value, "unexpected result value");
    }

    #[test]
    fn create_contract_erc20_succeeds() {
        let mut mock_runtime = MockHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();

        let callee = None;
        let caller = H160::from_low_u64_be(117);
        let transaction_value = U256::from(0);
        let call_data: Vec<u8> = hex::decode(ERC20_CONTRACT_INITIALISATION).unwrap();

        // gas_limit estimated using remix on shanghai network (1,631,430)
        // plus a 50% margin for gas accounting discrepancies
        let gas_limit = 2_400_000;
        let gas_price = U256::from(21000);

        // the test is not to check that account can prepay,
        // so we can choose the balance depending on set gas limit
        let balance = gas_price.saturating_mul(gas_limit.into());
        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            balance,
        );
        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            callee,
            caller,
            call_data,
            Some(gas_limit),
            gas_price,
            Some(transaction_value),
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.is_some());
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
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();

        let callee = None;
        let caller = H160::from_low_u64_be(2893);
        let transaction_value = U256::from(100_u32);
        // Some EVM instructions. They are all valid, but the last one is opcode
        // 0xFE, which is the designated INVALID opcode, so running this code
        // snippet must fail.
        let call_data: Vec<u8> = hex::decode("602e600055600054600154fe").unwrap();
        let gas_price = U256::from(21000);

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
            CONFIG,
            callee,
            caller,
            call_data,
            None,
            gas_price,
            Some(transaction_value),
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        let expected_result = Ok(Some(ExecutionOutcome {
            gas_used: 0,
            is_success: false,
            reason: ExitReason::Error(ExitError::DesignatedInvalid).into(),
            new_address: None,
            logs: vec![],
            result: None,
            withdrawals: vec![],
            estimated_ticks_used: 620000,
        }));

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
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let target = H160::from_low_u64_be(117u64);
        let caller = H160::from_low_u64_be(118u64);
        let gas_price = U256::from(1);

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            U256::from(100000),
        );

        // Act
        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            Some(target),
            caller,
            vec![],
            Some(22000),
            gas_price,
            None,
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        let expected_gas = 21000; // base cost

        // Assert
        let expected_result = Ok(Some(ExecutionOutcome {
            gas_used: expected_gas,
            is_success: true,
            reason: ExitReason::Succeed(ExitSucceed::Stopped).into(),
            new_address: None,
            logs: vec![],
            result: Some(vec![]),
            withdrawals: vec![],
            estimated_ticks_used: 0,
        }));

        assert_eq!(expected_result, result);
    }

    #[test]
    //this is based on https://eips.ethereum.org/EIPS/eip-155
    fn test_signatures() {
        let (sk, _address) = tezos_ethereum::tx_common::string_to_sk_and_address_unsafe(
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
                tezos_ethereum::tx_common::string_to_sk_and_address_unsafe(s.to_string());
            let value: [u8; 20] = hex::decode(ea).unwrap().try_into().unwrap();
            let ea = value.into();
            assert_eq!(a, ea);
        })
    }

    #[test]
    fn test_caller_classic() {
        let (_sk, address_from_sk) =
            tezos_ethereum::tx_common::string_to_sk_and_address_unsafe(
                "4646464646464646464646464646464646464646464646464646464646464646"
                    .to_string(),
            );
        let encoded =
        "f86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83".to_string();
        let transaction = EthereumTransactionCommon::from_hex(encoded).unwrap();
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
        let expected_transaction = EthereumTransactionCommon::from_hex(encoded).unwrap();

        let transaction = expected_transaction.sign_transaction(string_sk).unwrap();
        assert_eq!(expected_transaction, transaction)
    }

    #[test]
    fn call_simple_return_contract() {
        // Arrange
        let mut mock_runtime = MockHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let target = H160::from_low_u64_be(117u64);
        let caller = H160::from_low_u64_be(118u64);
        let code = vec![
            Opcode::PUSH1.as_u8(),
            0u8,
            Opcode::PUSH1.as_u8(),
            0u8,
            Opcode::RETURN.as_u8(),
        ];

        set_account_code(&mut mock_runtime, &mut evm_account_storage, &target, &code);
        let all_the_gas = 25_000;
        let gas_price = U256::from(1);
        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            all_the_gas.into(),
        );

        // Act
        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            Some(target),
            caller,
            vec![],
            Some(all_the_gas),
            gas_price,
            None,
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        let expected_gas = 21000 // base cost
        + 6; // execution cost

        // Assert
        let expected_result = Ok(Some(ExecutionOutcome {
            gas_used: expected_gas,
            is_success: true,
            reason: ExitReason::Succeed(ExitSucceed::Returned).into(),
            new_address: None,
            logs: vec![],
            result: Some(vec![]),
            withdrawals: vec![],
            estimated_ticks_used: 59271,
        }));

        assert_eq!(expected_result, result);
    }

    #[test]
    fn call_simple_revert_contract() {
        // Arrange
        let mut mock_runtime = MockHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let target = H160::from_low_u64_be(117u64);
        let caller = H160::from_low_u64_be(118u64);
        let gas_price = U256::from(1);
        let code = vec![
            Opcode::PUSH1.as_u8(),
            0u8,
            Opcode::PUSH1.as_u8(),
            0u8,
            Opcode::REVERT.as_u8(),
        ];
        let init_balance = 22_000;

        set_account_code(&mut mock_runtime, &mut evm_account_storage, &target, &code);

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            init_balance.into(),
        );

        // Act
        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            Some(target),
            caller,
            vec![],
            Some(init_balance),
            gas_price,
            None,
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        let expected_gas = 21000 // base cost
        + 2 * 3; // execution cost (only push)

        // Assert
        let expected_result = Ok(Some(ExecutionOutcome {
            gas_used: expected_gas,
            is_success: false,
            reason: ExitReason::Revert(ExitRevert::Reverted).into(),
            new_address: None,
            logs: vec![],
            result: Some(vec![]),
            withdrawals: vec![],
            estimated_ticks_used: 65039,
        }));

        assert_eq!(expected_result, result);

        // Some gas is returned to the send after the transaction is reverted
        assert_eq!(
            get_balance(&mut mock_runtime, &mut evm_account_storage, &caller),
            gas_price.saturating_mul((init_balance - expected_gas).into())
        )
    }

    #[test]
    fn call_contract_with_invalid_opcode() {
        // Arrange
        let mut mock_runtime = MockHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let target = H160::from_low_u64_be(117u64);
        let caller = H160::from_low_u64_be(118u64);
        let gas_price = U256::from(21000);
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
            CONFIG,
            Some(target),
            caller,
            vec![],
            None,
            gas_price,
            None,
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        // Assert
        let expected_result = Ok(Some(ExecutionOutcome {
            gas_used: 0,
            is_success: false,
            reason: ExitReason::Error(ExitError::DesignatedInvalid).into(),
            new_address: None,
            logs: vec![],
            result: None,
            withdrawals: vec![],
            estimated_ticks_used: 0,
        }));

        assert_eq!(expected_result, result);
    }

    #[test]
    fn no_transfer_when_contract_call_fails() {
        let mut mock_runtime = MockHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let caller = H160::from_low_u64_be(118_u64);
        let gas_price = U256::from(21000);

        let address = H160::from_low_u64_be(117_u64);
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
            CONFIG,
            Some(address),
            caller,
            vec![],
            None,
            gas_price,
            Some(U256::from(100)),
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        let expected_result = Ok(Some(ExecutionOutcome {
            gas_used: 0,
            is_success: false,
            reason: ExitReason::Error(ExitError::DesignatedInvalid).into(),
            new_address: None,
            logs: vec![],
            result: None,
            withdrawals: vec![],
            estimated_ticks_used: 0,
        }));

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
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let target = H160::from_low_u64_be(4u64); // identity contract
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let caller = H160::from_low_u64_be(118u64);
        let data = [1u8; 32]; // Need some data to make it a contract call
        let gas_price = U256::from(1);

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            22006.into(),
        );

        // Act
        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            Some(target),
            caller,
            data.to_vec(),
            Some(22001),
            gas_price,
            None,
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        let expected_gas = 21000 // base cost
            + 18 // execution cost
            + 32 * CONFIG.gas_transaction_non_zero_data; // transaction data cost

        // Assert
        let expected_result = Ok(Some(ExecutionOutcome {
            gas_used: expected_gas,
            is_success: true,
            reason: ExitReason::Succeed(ExitSucceed::Returned).into(),
            new_address: None,
            logs: vec![],
            result: Some(vec![1u8; 32]),
            withdrawals: vec![],
            estimated_ticks_used: 42_000 + 35 * 32,
        }));

        assert_eq!(expected_result, result);
    }

    #[test]
    fn call_ecrecover() {
        // Arrange
        let mut mock_runtime = MockHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        // example from https://www.evm.codes/precompiled?fork=shanghai
        let data_str = "456e9aea5e197a1f1af7a3e85a3212fa4049a3ba34c2289b4c860fc0b0c64ef3000000000000000000000000000000000000000000000000000000000000001c9242685bf161793cc25603c231bc2f568eb630ea16aa137d2664ac80388256084f8ae3bd7535248d0bd448298cc2e2071e56992d0774dc340c368ae950852ada";
        let data = hex::decode(data_str)
            .expect("Data should have been decoded from hex to bytes");
        // targes precompiled 0x01: ecrecover
        let target = H160::from_low_u64_be(1u64);
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let caller = H160::from_low_u64_be(118u64);
        let gas_price = U256::from(21000);
        let gas_limit = 35000;

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            gas_price * gas_limit + U256::one(),
        );

        // Act
        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            Some(target),
            caller,
            data.to_vec(),
            Some(gas_limit),
            gas_price,
            None,
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        // Assert
        // ecrecover pad address with 0 to be encoded over 32 bytes
        let expected_address =
            "0000000000000000000000007156526fbd7a3c72969b54f64e42c10fbb768c8a";
        let expected_result = Ok(Some(ExecutionOutcome {
            gas_used: 25676,
            is_success: true,
            reason: ExitReason::Succeed(ExitSucceed::Returned).into(),
            new_address: None,
            logs: vec![],
            result: Some(hex::decode(expected_address).unwrap()),
            withdrawals: vec![],
            estimated_ticks_used: 30_000_000,
        }));

        assert_eq!(expected_result, result);
    }

    #[test]
    fn create_and_call_contract() {
        // Arrange
        let mut mock_runtime = MockHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let target = H160::from_low_u64_be(117u64);
        let caller = H160::from_low_u64_be(118u64);
        let gas_price = U256::from(21000);

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
            CONFIG,
            Some(target),
            caller,
            vec![],
            None,
            gas_price,
            None,
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        // Assert
        // let expected_result = Ok(());
        // assert_eq!(result, expected_result);
        assert!(result.is_ok());
    }

    #[test]
    fn static_calls_cannot_update_storage() {
        let mut mock_runtime = MockHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let target = H160::from_low_u64_be(117_u64);
        let caller = H160::from_low_u64_be(118_u64);
        let static_call_target = H160::from_low_u64_be(200_u64);
        let all_the_gas = 2_000_000_u64;
        let gas_price = U256::from(1);

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            all_the_gas.into(),
        );

        // contract that stores something in durable storage
        let static_call_code = vec![
            Opcode::PUSH2.as_u8(),
            0xFF,
            0xFF,
            Opcode::PUSH1.as_u8(),
            0,
            Opcode::SSTORE.as_u8(),
        ];

        set_account_code(
            &mut mock_runtime,
            &mut evm_account_storage,
            &static_call_target,
            &static_call_code,
        );

        // contract that does static call to contract above
        let code = vec![
            Opcode::PUSH1.as_u8(), // push return data size
            0,
            Opcode::PUSH1.as_u8(), // push return data offset
            0,
            Opcode::PUSH1.as_u8(), // push arg size
            0,
            Opcode::PUSH1.as_u8(), // push arg offset
            0,
            Opcode::PUSH1.as_u8(), // push address
            200,
            Opcode::PUSH2.as_u8(), // push gas
            0xFF,
            0xFF,
            Opcode::STATICCALL.as_u8(),
        ];

        set_account_code(&mut mock_runtime, &mut evm_account_storage, &target, &code);

        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            Some(target),
            caller,
            vec![],
            Some(all_the_gas),
            gas_price,
            None,
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        let expected_gas = 21000 // base cost
        + 65535 // staticcall allocated gas
        + 6 * 3 // cost for push
        + 100; // cost for staticcall

        // Since we execute an invalid instruction (for a static call that is) we spend
        // _all_ the gas allocated to the call (so 0xFFFF or 65535)
        let expected_result = Ok(Some(ExecutionOutcome {
            gas_used: expected_gas,
            is_success: true,
            reason: ExitReason::Succeed(ExitSucceed::Stopped).into(),
            new_address: None,
            logs: vec![],
            result: Some(vec![]),
            withdrawals: vec![],
            estimated_ticks_used: 428861740,
        }));

        // assert that call succeeds
        assert_eq!(result, expected_result);
    }

    #[test]
    fn static_calls_fail_when_logging() {
        let mut mock_runtime = MockHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let target = H160::from_low_u64_be(117_u64);
        let caller = H160::from_low_u64_be(118_u64);
        let static_call_target = H160::from_low_u64_be(200_u64);
        let all_the_gas = 2_000_000_u64;
        let gas_price = U256::from(1);

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            all_the_gas.into(),
        );

        // contract that does logging
        let static_call_code = vec![
            Opcode::PUSH1.as_u8(), // push size
            1,
            Opcode::PUSH1.as_u8(), // push address
            0x1,
            Opcode::LOG0.as_u8(), // write a zero to log
        ];

        set_account_code(
            &mut mock_runtime,
            &mut evm_account_storage,
            &static_call_target,
            &static_call_code,
        );

        // contract that does static call to contract above
        let code = vec![
            Opcode::PUSH1.as_u8(), // push return data size
            0,
            Opcode::PUSH1.as_u8(), // push return data offset
            0,
            Opcode::PUSH1.as_u8(), // push arg size
            0,
            Opcode::PUSH1.as_u8(), // push arg offset
            0,
            Opcode::PUSH1.as_u8(), // push address
            200,
            Opcode::PUSH2.as_u8(), // push gas
            0xFF,
            0xFF,
            Opcode::STATICCALL.as_u8(),
        ];

        set_account_code(&mut mock_runtime, &mut evm_account_storage, &target, &code);

        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            Some(target),
            caller,
            vec![],
            Some(all_the_gas),
            gas_price,
            None,
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        let expected_gas = 21000 // base cost
        + 65535 // staticcall allocated gas
        + 6 * 3 // cost for push
        + 100; // cost for staticcall

        // Since we execute an invalid instruction (for a static call that is), we
        // expect to spend _all_ the gas.
        let expected_result = Ok(Some(ExecutionOutcome {
            gas_used: expected_gas,
            is_success: true,
            reason: ExitReason::Succeed(ExitSucceed::Stopped).into(),
            new_address: None,
            // No logs were produced
            logs: vec![],
            result: Some(vec![]),
            withdrawals: vec![],
            estimated_ticks_used: 441347264,
        }));

        // assert that call succeeds
        assert_eq!(result, expected_result);
    }

    #[test]
    fn logs_get_written_to_output() {
        let mut mock_runtime = MockHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let target = H160::from_low_u64_be(117_u64);
        let caller = H160::from_low_u64_be(118_u64);
        let call_target = H160::from_low_u64_be(200_u64);
        let all_the_gas = 2_000_000_u64;
        let gas_price = U256::from(1);

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            all_the_gas.into(),
        );

        // contract that does logging
        let contract_that_logs = vec![
            Opcode::PUSH8.as_u8(), // push some value
            1,
            2,
            3,
            4,
            5,
            6,
            7,
            8,
            Opcode::PUSH1.as_u8(), // push memory address
            0,
            Opcode::MSTORE.as_u8(), // store value to memory address
            Opcode::PUSH1.as_u8(),  // push some topic
            42,
            Opcode::PUSH1.as_u8(), // push size
            8,
            Opcode::PUSH1.as_u8(), // push address
            24,
            Opcode::LOG1.as_u8(), // write a zero to log
        ];

        set_account_code(
            &mut mock_runtime,
            &mut evm_account_storage,
            &call_target,
            &contract_that_logs,
        );

        // contract that calls contract above
        let code = vec![
            Opcode::PUSH1.as_u8(), // push log record size
            1,
            Opcode::PUSH1.as_u8(), // push memory address of log data
            1,
            Opcode::LOG0.as_u8(),  // write something to the log
            Opcode::PUSH1.as_u8(), // push return data size
            0,
            Opcode::PUSH1.as_u8(), // push return data offset
            0,
            Opcode::PUSH1.as_u8(), // push arg size
            0,
            Opcode::PUSH1.as_u8(), // push arg offset
            0,
            Opcode::PUSH1.as_u8(), // push value
            0,
            Opcode::PUSH1.as_u8(), // push address
            200,
            Opcode::PUSH2.as_u8(), // push gas
            0xFF,
            0xFF,
            Opcode::CALL.as_u8(),
        ];

        set_account_code(&mut mock_runtime, &mut evm_account_storage, &target, &code);

        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            Some(target),
            caller,
            vec![],
            Some(all_the_gas),
            gas_price,
            None,
            true,
            1_000_000_000,
            false,
        );

        let log_record1 = Log {
            address: target,
            topics: vec![],
            data: vec![0],
        };

        let log_record2 = Log {
            address: call_target,
            topics: vec![H256::from_low_u64_be(42)],
            data: vec![1, 2, 3, 4, 5, 6, 7, 8],
        };

        let expected_gas = 21000 // base cost
        + 1348; // execution cost (taken at face value from tests)

        let expected_result = Ok(Some(ExecutionOutcome {
            gas_used: expected_gas,
            is_success: true,
            reason: ExitReason::Succeed(ExitSucceed::Stopped).into(),
            new_address: None,
            logs: vec![log_record1, log_record2],
            result: Some(vec![]),
            withdrawals: vec![],
            estimated_ticks_used: 1017247,
        }));

        assert_eq!(result, expected_result);
    }

    #[test]
    fn no_logs_when_contract_reverts() {
        let mut mock_runtime = MockHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let target = H160::from_low_u64_be(117_u64);
        let caller = H160::from_low_u64_be(118_u64);
        let static_call_target = H160::from_low_u64_be(200_u64);
        let all_the_gas = 2_000_000_u64;
        let gas_price = U256::from(1);

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            all_the_gas.into(),
        );

        // contract that does logging
        let static_call_code = vec![
            Opcode::PUSH1.as_u8(), // push size
            1,
            Opcode::PUSH1.as_u8(), // push address
            0x1,
            Opcode::LOG0.as_u8(),  // write a zero to log
            Opcode::PUSH1.as_u8(), // size of return data
            0,
            Opcode::PUSH1.as_u8(), // offset of return data
            0,
            Opcode::REVERT.as_u8(),
        ];

        set_account_code(
            &mut mock_runtime,
            &mut evm_account_storage,
            &static_call_target,
            &static_call_code,
        );

        // contract that does call to contract above
        let code = vec![
            Opcode::PUSH1.as_u8(), // push log record size
            1,
            Opcode::PUSH1.as_u8(), // push memory address of log data
            1,
            Opcode::LOG0.as_u8(),  // write something to the log
            Opcode::PUSH1.as_u8(), // push return data size
            0,
            Opcode::PUSH1.as_u8(), // push return data offset
            0,
            Opcode::PUSH1.as_u8(), // push arg size
            0,
            Opcode::PUSH1.as_u8(), // push arg offset
            0,
            Opcode::PUSH1.as_u8(), // push value
            0,
            Opcode::PUSH1.as_u8(), // push address
            200,
            Opcode::PUSH2.as_u8(), // push gas
            0xFF,
            0xFF,
            Opcode::CALL.as_u8(),
        ];

        set_account_code(&mut mock_runtime, &mut evm_account_storage, &target, &code);

        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            Some(target),
            caller,
            vec![],
            Some(all_the_gas),
            gas_price,
            None,
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        let log_record1 = Log {
            address: target,
            topics: vec![],
            data: vec![0],
        };

        let expected_gas = 21000 // base cost
        + 911; // execution cost (taken at face value from tests)

        let expected_result = Ok(Some(ExecutionOutcome {
            gas_used: expected_gas,
            is_success: true,
            reason: ExitReason::Succeed(ExitSucceed::Stopped).into(),
            new_address: None,
            logs: vec![log_record1],
            result: Some(vec![]),
            withdrawals: vec![],
            estimated_ticks_used: 614707,
        }));

        assert_eq!(result, expected_result);
    }

    #[test]
    fn contract_selfdestruct_deletes_contract() {
        let mut mock_runtime = MockHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let target = H160::from_low_u64_be(42_u64);
        let caller = H160::from_low_u64_be(115_u64);
        let selfdestructing_contract = H160::from_low_u64_be(100_u64);
        let all_the_gas = 1_000_000_u64;
        let gas_price = U256::from(1);

        // This contract selfdestructs and gives its funds to `caller`
        let selfdestructing_code = vec![
            Opcode::PUSH1.as_u8(), // push address of beneficiary
            115,
            Opcode::SUICIDE.as_u8(), // this also stops execution
        ];

        set_account_code(
            &mut mock_runtime,
            &mut evm_account_storage,
            &selfdestructing_contract,
            &selfdestructing_code,
        );

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            all_the_gas.into(),
        );

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &selfdestructing_contract,
            1_000_000.into(),
        );

        // contract that does call to contract above
        let code = vec![
            Opcode::PUSH1.as_u8(), // push return data size
            0,
            Opcode::PUSH1.as_u8(), // push return data offset
            0,
            Opcode::PUSH1.as_u8(), // push arg size
            0,
            Opcode::PUSH1.as_u8(), // push arg offset
            0,
            Opcode::PUSH1.as_u8(), // push value
            0,
            Opcode::PUSH1.as_u8(), // push address
            100,
            Opcode::PUSH2.as_u8(), // push gas
            0xFF,
            0xFF,
            Opcode::CALL.as_u8(),
        ];

        set_account_code(&mut mock_runtime, &mut evm_account_storage, &target, &code);

        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            Some(target),
            caller,
            vec![],
            Some(all_the_gas),
            gas_price,
            None,
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );
        let expected_gas = 21000 // base cost
        + 30124; // execution gas cost (taken at face value from tests)
        let expected_result = Ok(Some(ExecutionOutcome {
            gas_used: expected_gas,
            is_success: true,
            reason: ExitReason::Succeed(ExitSucceed::Stopped).into(),
            new_address: None,
            logs: vec![],
            result: Some(vec![]),
            withdrawals: vec![],
            estimated_ticks_used: 23749485,
        }));

        assert_eq!(result, expected_result);

        assert_eq!(
            evm_account_storage
                .get(
                    &mock_runtime,
                    &account_path(&selfdestructing_contract).unwrap()
                )
                .unwrap(),
            None
        );

        let funds_total =
            1_000_000 + all_the_gas - expected_result.unwrap().unwrap().gas_used;

        assert_eq!(
            get_balance(&mut mock_runtime, &mut evm_account_storage, &caller),
            funds_total.into()
        );
    }

    #[test]
    fn selfdestruct_is_ignored_when_call_fails() {
        let mut mock_runtime = MockHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let target = H160::from_low_u64_be(42_u64);
        let caller = H160::from_low_u64_be(115_u64);
        let selfdestructing_contract = H160::from_low_u64_be(100_u64);
        let all_the_gas = 1_000_000_u64;
        let gas_price = U256::from(1);

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            all_the_gas.into(),
        );

        // This contract selfdestructs and gives its funds to `caller`
        let selfdestructing_code = vec![
            Opcode::PUSH1.as_u8(), // push address of beneficiary
            115,
            Opcode::SUICIDE.as_u8(), // this also stops execution
        ];

        set_account_code(
            &mut mock_runtime,
            &mut evm_account_storage,
            &selfdestructing_contract,
            &selfdestructing_code,
        );

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &selfdestructing_contract,
            1_000_000.into(),
        );

        bump_nonce(
            &mut mock_runtime,
            &mut evm_account_storage,
            &selfdestructing_contract,
        );

        // contract that does call to contract above
        let code = vec![
            Opcode::PUSH1.as_u8(), // push return data size
            0,
            Opcode::PUSH1.as_u8(), // push return data offset
            0,
            Opcode::PUSH1.as_u8(), // push arg size
            0,
            Opcode::PUSH1.as_u8(), // push arg offset
            0,
            Opcode::PUSH1.as_u8(), // push value
            0,
            Opcode::PUSH1.as_u8(), // push address
            100,
            Opcode::PUSH2.as_u8(), // push gas
            0xFF,
            0xFF,
            Opcode::CALL.as_u8(), // call the contract that selfdestructs
            Opcode::INVALID.as_u8(), // fail the entire transaction
        ];

        set_account_code(&mut mock_runtime, &mut evm_account_storage, &target, &code);

        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            Some(target),
            caller,
            vec![],
            Some(all_the_gas),
            gas_price,
            None,
            true,
            10_000_000_000,
            false,
        );

        let expected_result = Ok(Some(ExecutionOutcome {
            gas_used: all_the_gas,
            is_success: false,
            reason: ExitReason::Error(ExitError::InvalidCode(Opcode::INVALID)).into(),
            new_address: None,
            logs: vec![],
            result: None,
            withdrawals: vec![],
            estimated_ticks_used: 9512509485,
        }));

        assert_eq!(result, expected_result);

        let account = evm_account_storage
            .get_or_create(
                &mock_runtime,
                &account_path(&selfdestructing_contract).unwrap(),
            )
            .unwrap();

        assert_eq!(
            account.balance(&mock_runtime).unwrap(),
            U256::from(1_000_000)
        );
        assert_eq!(account.code(&mock_runtime).unwrap(), selfdestructing_code);
        assert_eq!(account.nonce(&mock_runtime).unwrap(), U256::one());

        assert_eq!(
            get_balance(&mut mock_runtime, &mut evm_account_storage, &caller),
            0.into()
        );
    }

    #[test]
    fn test_chain_id() {
        // Arrange
        let mut mock_runtime = MockHost::default();
        let chain_id = U256::from(42);
        let mut chain_id_bytes = [0u8; 32];
        chain_id.to_big_endian(&mut chain_id_bytes);
        let block_fees = BlockFees::new(U256::from(54321), U256::from(1000));
        let block = BlockConstants::first_block(U256::zero(), chain_id, block_fees);
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let target = H160::from_low_u64_be(117u64);
        let caller = H160::from_low_u64_be(118u64);
        let gas_price = U256::from(12345);
        let code = vec![
            Opcode::CHAINID.as_u8(), // cost 2
            Opcode::PUSH1.as_u8(),   // push ost, cost 3
            0,
            Opcode::MSTORE.as_u8(), // cost 3, memory expansion cost 3
            Opcode::PUSH1.as_u8(),  // push len, cost 3
            32,
            Opcode::PUSH1.as_u8(), // push ost, cost 3
            0,
            Opcode::RETURN.as_u8(), // cost 0
        ];

        // value not relevant to test, must be big enough
        let all_the_gas = 25_000_u64;

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            gas_price * all_the_gas,
        );

        set_account_code(&mut mock_runtime, &mut evm_account_storage, &target, &code);

        // Act
        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            Some(target),
            caller,
            vec![],
            Some(all_the_gas),
            gas_price,
            None,
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        let expected_gas = 21000 // base cost
        + 17; // execution cost

        // Assert
        let expected_result = Ok(Some(ExecutionOutcome {
            gas_used: expected_gas,
            is_success: true,
            reason: ExitReason::Succeed(ExitSucceed::Returned).into(),
            new_address: None,
            logs: vec![],
            result: Some(chain_id_bytes.into()),
            withdrawals: vec![],
            estimated_ticks_used: 224101,
        }));
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_base_fee_per_gas() {
        // Arrange
        let mut mock_runtime = MockHost::default();
        let base_fee_per_gas = U256::from(23000);
        let mut base_fee_per_gas_bytes = [0u8; 32];
        base_fee_per_gas.to_big_endian(&mut base_fee_per_gas_bytes);
        let block_fees = BlockFees::new(base_fee_per_gas, U256::zero());
        let block = BlockConstants::first_block(U256::zero(), U256::one(), block_fees);
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let target = H160::from_low_u64_be(117u64);
        let caller = H160::from_low_u64_be(118u64);
        let code = vec![
            Opcode::BASEFEE.as_u8(), // cost 2
            Opcode::PUSH1.as_u8(),   // push ost, cost 3
            0,
            Opcode::MSTORE.as_u8(), // cost 3, memory expansion cost 3
            Opcode::PUSH1.as_u8(),  // push len, cost 3
            32,
            Opcode::PUSH1.as_u8(), // push ost, cost 3
            0,
            Opcode::RETURN.as_u8(),
        ];

        // value not relevant to test, just needs to be big enough
        let all_the_gas = 25_000_u64;
        let gas_price = U256::from(21000);

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            gas_price * all_the_gas,
        );

        set_account_code(&mut mock_runtime, &mut evm_account_storage, &target, &code);

        // Act
        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            Some(target),
            caller,
            vec![],
            Some(all_the_gas),
            gas_price,
            None,
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        let expected_gas = 21000 // base cost
        + 17; // execution cost

        // Assert
        let expected_result = Ok(Some(ExecutionOutcome {
            gas_used: expected_gas,
            reason: ExitReason::Succeed(ExitSucceed::Returned).into(),
            is_success: true,
            new_address: None,
            logs: vec![],
            result: Some(base_fee_per_gas_bytes.into()),
            withdrawals: vec![],
            estimated_ticks_used: 224101,
        }));
        assert_eq!(result, expected_result);
    }

    /// [unwrap_outcome!(result, expect_success)] tries to unwrap a value of type
    /// `Result<Option<ExecutionOutome>, ...>` and check the outcome status
    /// according to optional argument [expect_success] (default value true)
    macro_rules! unwrap_outcome {
        ($result:expr, $expect_success:expr) => {{
            assert!($result.is_ok(), "Couldn't unwrap, Result was Err");
            let tmp = $result.as_ref().unwrap();
            assert!(tmp.is_some(), "Couldn't unwrap, Option was None");
            let tmp = tmp.as_ref().unwrap();
            assert_eq!(
                tmp.is_success, $expect_success,
                "outcome field 'is_success' should be {}",
                $expect_success
            );
            tmp
        }};
        ($result:expr) => {{
            unwrap_outcome!($result, true)
        }};
    }

    #[test]
    fn evm_should_fail_gracefully_when_balance_overflow_occurs() {
        let mut mock_runtime = MockHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let caller = H160::from_low_u64_be(523);
        let target = H160::from_low_u64_be(210);
        let gas_price = U256::from(21000);

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            U256::from(200),
        );
        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &target,
            U256::max_value(),
        );

        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            Some(target),
            caller,
            vec![],
            None,
            gas_price,
            Some(U256::from(100)),
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        let expected_result = Err(EthereumError::EthereumAccountError(
            AccountStorageError::BalanceOverflow,
        ));
        assert_eq!(expected_result, result);
    }

    #[test]
    fn create_contract_gas_cost() {
        let mut mock_runtime = MockHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();

        let callee = None;
        let caller = H160::from_low_u64_be(117);
        let transaction_value = U256::from(0);
        // example stolen from https://www.rareskills.io/post/smart-contract-creation-cost
        let data_str = "6080604052603f8060116000396000f3fe6080604052600080fdfea2646970667358221220c5cad0aa1e64e2ca6a6cdf28a25255a8ebbf3cdd5ea0b8e4129a3c83c4fbb72a64736f6c63430008070033";
        let call_data: Vec<u8> = hex::decode(data_str).unwrap();

        // not testing gas_limit, should be big enough
        let gas_limit = 2_400_000;
        let gas_price = U256::from(21000);

        // the test is not to check that account can prepay,
        // so we can choose the balance depending on set gas limit
        let balance = gas_price.saturating_mul(gas_limit.into());
        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            balance,
        );
        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            callee,
            caller,
            call_data,
            Some(gas_limit),
            gas_price,
            Some(transaction_value),
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        let result = unwrap_outcome!(result);

        // gas calculation
        let expected_gas = 21000 // base cost
        + 32000 // create base cost
        + 1220 // transaction data cost
        + 12600 // code deposit cost
        + 42; // init cost

        assert_eq!(expected_gas, result.gas_used);
    }

    #[test]
    fn create_contract_fail_gas_cost() {
        let mut mock_runtime = MockHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();

        let callee = None;
        let caller = H160::from_low_u64_be(117);
        let transaction_value = U256::from(0);
        // data should result in failed contract creation
        let create_data: Vec<u8> = vec![0x01; 32];

        // not testing gas_limit, should be big enough
        let gas_limit = 2_400_000;
        let gas_price = U256::from(21000);

        // the test is not to check that account can prepay,
        // so we can choose the balance depending on set gas limit
        let balance = gas_price.saturating_mul(gas_limit.into());
        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            balance,
        );
        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            callee,
            caller,
            create_data,
            Some(gas_limit),
            gas_price,
            Some(transaction_value),
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        let result = unwrap_outcome!(&result, false);

        // gas calculation
        let expected_gas = 21000 // base cost
        + 32000 // create cost
        + 32 * CONFIG.gas_transaction_non_zero_data // transaction data cost
        + 3; // init cost

        assert_eq!(expected_gas, result.gas_used);
    }

    #[test]
    fn test_transaction_data_cost() {
        // Arrange
        let mut mock_runtime = MockHost::default();
        let base_fee_per_gas = U256::from(23000);
        let block_fees = BlockFees::new(base_fee_per_gas, U256::zero());
        let block = BlockConstants::first_block(U256::zero(), U256::one(), block_fees);
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let target = H160::from_low_u64_be(117u64);
        let caller = H160::from_low_u64_be(118u64);

        // zero byte data
        let data = [0u8; 32];

        // zero cost contract
        let code = vec![Opcode::STOP.as_u8()];

        // value not relevant to test, just needs to be big enough
        let all_the_gas = 25_000_u64;
        let gas_price = U256::from(21000);

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            gas_price * all_the_gas,
        );

        set_account_code(&mut mock_runtime, &mut evm_account_storage, &target, &code);

        // Act
        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            Some(target),
            caller,
            data.to_vec(),
            Some(all_the_gas),
            gas_price,
            None,
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        // Assert
        let result = unwrap_outcome!(&result);

        let expected_gas = 21000 // base cost
        + 32 * CONFIG.gas_transaction_zero_data; // transaction data cost

        assert_eq!(expected_gas, result.gas_used);
    }

    #[test]
    fn test_transaction_data_cost_non_zero() {
        // Arrange
        let mut mock_runtime = MockHost::default();
        let base_fee_per_gas = U256::from(23000);
        let block_fees = BlockFees::new(base_fee_per_gas, U256::zero());
        let block = BlockConstants::first_block(U256::zero(), U256::one(), block_fees);
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let target = H160::from_low_u64_be(117u64);
        let caller = H160::from_low_u64_be(118u64);

        // no zero byte data
        let data = [127u8; 32];

        // zero cost contract
        let code = vec![Opcode::STOP.as_u8()];

        // value not relevant to test, just needs to be big enough
        let all_the_gas = 25_000_u64;
        let gas_price = U256::from(21000);

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            gas_price * all_the_gas,
        );

        set_account_code(&mut mock_runtime, &mut evm_account_storage, &target, &code);

        // Act
        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            Some(target),
            caller,
            data.to_vec(),
            Some(all_the_gas),
            gas_price,
            None,
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        // Assert
        let result = unwrap_outcome!(&result);

        let expected_gas = 21000 // base cost
        + 32 * CONFIG.gas_transaction_non_zero_data; // transaction data cost: should be zero_byte cost * size

        assert_eq!(expected_gas, result.gas_used);
    }

    fn first_block() -> BlockConstants {
        let base_fee_per_gas = U256::from(23000);
        let block_fees = BlockFees::new(base_fee_per_gas, U256::zero());
        BlockConstants::first_block(U256::zero(), U256::one(), block_fees)
    }

    fn deploy(
        data: Vec<u8>,
        all_the_gas: u64,
    ) -> Result<Option<ExecutionOutcome>, EthereumError> {
        // Arrange
        let mut mock_runtime = MockHost::default();
        let block = first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let caller = H160::from_low_u64_be(118u64);
        let gas_price = U256::from(1356);

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            gas_price * all_the_gas,
        );

        // Act

        run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            None,
            caller,
            data.to_vec(),
            Some(all_the_gas),
            gas_price,
            None,
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        )
    }

    fn test_eip_3541(data: Vec<u8>) {
        // value not relevant to test, just needs to be big enough
        let all_the_gas = 65_000_u64;

        // Act
        let result = deploy(data, all_the_gas);

        // Assert
        let result = unwrap_outcome!(&result, false);
        assert_eq!(
            ExtendedExitReason::Exit(ExitReason::Error(ExitError::InvalidCode(Opcode(
                0xef
            )))),
            result.reason
        );
    }

    #[test]
    fn test_eip_3541_errors() {
        // Arrange: see test cases
        // in https://github.com/ethereum/EIPs/blob/master/EIPS/eip-3541.md
        test_eip_3541(hex::decode("60ef60005360016000f3").unwrap());
        test_eip_3541(hex::decode("60ef60005360026000f3").unwrap());
        test_eip_3541(hex::decode("60ef60005360036000f3").unwrap());
        test_eip_3541(hex::decode("60ef60005360206000f3").unwrap());
    }

    #[test]
    fn test_eip_3541_noerror() {
        // value not relevant to test, just needs to be big enough
        let all_the_gas = 65_000_u64;
        let data = hex::decode("60fe60005360016000f3").unwrap();
        // Act
        let result = deploy(data, all_the_gas);

        // Assert
        let result = unwrap_outcome!(&result);
        assert_eq!(
            ExtendedExitReason::Exit(ExitReason::Succeed(ExitSucceed::Returned)),
            result.reason
        );
    }

    // Test case from https://eips.ethereum.org/EIPS/eip-684
    #[test]
    fn test_create_to_address_with_code_returns_error() {
        let mut mock_runtime = MockHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();

        let caller =
            H160::from_str("0xd0bBEc6D2c628b7e2E6D5556daA14a5181b604C5").unwrap();

        let target_address =
            H160::from_str("0x7658771dc6Af74a3d2F8499D349FF9c1a0DF8826").unwrap();

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            10000000.into(),
        );

        set_account_code(
            &mut mock_runtime,
            &mut evm_account_storage,
            &target_address,
            hex::decode("B0B0FACE")
                .expect("Failed to decode contract code")
                .as_slice(),
        );

        // call_data is the input to deploy the following contract.
        // The content of the contract does not matter since address of created contract depends on address and nonce of caller
        // contract Empty {
        //     function run() public{
        //     }
        // }
        let call_data = hex::decode(
            "6080604052348015600e575f80fd5b50606a80601a5f395ff3fe6080604052348015600e575f80fd5b50600436106026575f3560e01c8063c040622614602a575b5f80fd5b60306032565b005b56fea264697066735822122033200c2933dd0930ac60a7727e0a3e56f8967f6f76afb4ecf2459651419983ab64736f6c63430008170033",
        )
        .expect("Failed to decode call data");

        let gas_limit = 300_000;
        let gas_price = U256::from(1);

        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            None,
            caller,
            call_data,
            Some(gas_limit),
            gas_price,
            None,
            true,
            10_000_000_000,
            false,
        );

        let result = unwrap_outcome!(&result, false);
        match &result.reason {
            ExtendedExitReason::Exit(ExitReason::Error(ExitError::CreateCollision)) => (),
            exit_error => panic!(
                "ExitReason: {:?}. Expect ExitReason::Error(ExitError::CreateCollision)",
                exit_error
            ),
        }
    }

    // Test case from https://eips.ethereum.org/EIPS/eip-684 with modification
    #[test]
    fn test_create_to_address_with_non_zero_nonce_returns_error() {
        let mut mock_runtime = MockHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();

        let caller =
            H160::from_str("0xd0bBEc6D2c628b7e2E6D5556daA14a5181b604C5").unwrap();

        let target_address =
            H160::from_str("0x7658771dc6Af74a3d2F8499D349FF9c1a0DF8826").unwrap();

        set_balance(
            &mut mock_runtime,
            &mut evm_account_storage,
            &caller,
            10000000.into(),
        );

        bump_nonce(&mut mock_runtime, &mut evm_account_storage, &target_address);

        // call_data is the input to deploy the following contract.
        // The content of the contract does not matter since address of created contract depends on address and nonce of caller
        // contract Empty {
        //     function run() public{
        //     }
        // }
        let call_data = hex::decode(
            "6080604052348015600e575f80fd5b50606a80601a5f395ff3fe6080604052348015600e575f80fd5b50600436106026575f3560e01c8063c040622614602a575b5f80fd5b60306032565b005b56fea264697066735822122033200c2933dd0930ac60a7727e0a3e56f8967f6f76afb4ecf2459651419983ab64736f6c63430008170033",
        )
        .expect("Failed to decode call data");

        let gas_limit = 300_000;
        let gas_price = U256::from(1);

        let result = run_transaction(
            &mut mock_runtime,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            None,
            caller,
            call_data,
            Some(gas_limit),
            gas_price,
            None,
            true,
            10_000_000_000,
            false,
        );

        let result = unwrap_outcome!(&result, false);
        match &result.reason {
            ExtendedExitReason::Exit(ExitReason::Error(ExitError::CreateCollision)) => (),
            exit_error => panic!(
                "ExitReason: {:?}. Expect ExitReason::Error(ExitError::CreateCollision)",
                exit_error
            ),
        }
    }

    #[test]
    fn created_contract_start_at_nonce_one() {
        let mut host = MockHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();

        let callee = None;
        let caller = H160::from_low_u64_be(117);
        let transaction_value = U256::from(0);
        let data_str = "6101aa6064526000600060206000600073b000000000000000000000000000000000000000620493e0f1506000600060006000733000000000000000000000000000000000000000620493e0f450600060006020600073b000000000000000000000000000000000000000620493e0f45060006000600060006000733000000000000000000000000000000000000000620493e0f25060006000600060006000732000000000000000000000000000000000000000620927c0f100";
        let call_data: Vec<u8> = hex::decode(data_str).unwrap();

        let gas_limit = 2_400_000;
        let gas_price = U256::from(21000);
        let balance = gas_price.saturating_mul(gas_limit.into());

        set_balance(&mut host, &mut evm_account_storage, &caller, balance);

        let result = run_transaction(
            &mut host,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            callee,
            caller,
            call_data,
            Some(gas_limit),
            gas_price,
            Some(transaction_value),
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        let result = unwrap_outcome!(result);
        let ExecutionOutcome { new_address, .. } = result;
        let address = new_address.unwrap();
        let smart_contract = EthereumAccount::from_address(&address).unwrap();

        assert_eq!(smart_contract.nonce(&host).unwrap(), U256::one())
    }

    #[test]
    fn call_contract_create_contract_with_insufficient_funds() {
        let mut host = MockHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();

        let callee = H160::from_str("095e7baea6a6c7c4c2dfeb977efac326af552d87").unwrap();
        let caller = H160::from_str("a94f5374fce5edbc8e2a8697c15331677e6ebf0b").unwrap();

        set_balance(
            &mut host,
            &mut evm_account_storage,
            &caller,
            U256::from(1000000000),
        );

        set_balance(
            &mut host,
            &mut evm_account_storage,
            &callee,
            U256::from(10000),
        );

        let code = hex::decode(
            "74600c60005566602060406000f060205260076039f36000526015600b620186a0f060005500",
        )
        .unwrap();
        set_account_code(&mut host, &mut evm_account_storage, &callee, &code);

        let result = run_transaction(
            &mut host,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            Some(callee),
            caller,
            vec![],
            Some(20000000),
            U256::one(),
            Some(U256::zero()),
            true,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        let path = account_path(&caller).unwrap();
        let account = evm_account_storage.get_or_create(&host, &path).unwrap();
        let caller_nonce = account.nonce(&host).unwrap();

        let path = account_path(&callee).unwrap();
        let account = evm_account_storage.get_or_create(&host, &path).unwrap();
        let callee_nonce = account.nonce(&host).unwrap();

        assert_eq!(
            ExtendedExitReason::Exit(ExitReason::Succeed(ExitSucceed::Stopped)),
            result.unwrap().unwrap().reason,
        );
        assert_eq!(callee_nonce, U256::zero());
        assert_eq!(caller_nonce, U256::one());
    }

    fn out_of_tick_scenario(
        retriable: bool,
    ) -> (
        MockHost,
        Result<Option<ExecutionOutcome>, EthereumError>,
        EthereumAccount,
        U256,
        U256,
    ) {
        let mut host = MockHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();
        let callee = None;
        let caller = H160::from_low_u64_be(117);
        let transaction_value = U256::from(0);
        let call_data: Vec<u8> = hex::decode(ERC20_CONTRACT_INITIALISATION).unwrap();

        // gas_limit estimated using remix on shanghai network (1,631,430)
        // plus a 50% margin for gas accounting discrepancies
        let gas_limit = 2_400_000;
        let gas_price = U256::from(21000);

        // the test is not to check that account can prepay,
        // so we can choose the balance depending on set gas limit
        let initial_balance = gas_price
            .saturating_mul(gas_limit.into())
            .saturating_mul(U256::from(2));
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &caller,
            initial_balance,
        );

        let path = account_path(&caller).unwrap();
        let account = evm_account_storage.get_or_create(&host, &path).unwrap();
        let initial_caller_nonce = account.nonce(&host).unwrap();

        let result = run_transaction(
            &mut host,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            callee,
            caller,
            call_data,
            Some(gas_limit),
            gas_price,
            Some(transaction_value),
            true,
            10_000,
            retriable,
        );

        (host, result, account, initial_balance, initial_caller_nonce)
    }

    #[test]
    fn transaction_has_no_impact_if_retriable() {
        let (host, result, caller, initial_caller_balance, initial_caller_nonce) =
            out_of_tick_scenario(true);
        let caller_balance = caller.balance(&host).unwrap();
        let caller_nonce = caller.nonce(&host).unwrap();

        assert_eq!(
            ExtendedExitReason::OutOfTicks,
            result.unwrap().unwrap().reason,
            "Contract creation was expected to fail and run out of ticks: \n"
        );
        assert_eq!(
            initial_caller_balance, caller_balance,
            "Balance shouldn't have changed as gas should have been repaid"
        );
        assert_eq!(
            initial_caller_nonce, caller_nonce,
            "Nonce shouldn't have changed"
        )
    }

    #[test]
    fn non_retriable_transaction_pays_for_exhausted_ticks() {
        let (host, result, caller, initial_caller_balance, initial_caller_nonce) =
            out_of_tick_scenario(false);
        let caller_balance = caller.balance(&host).unwrap();
        let caller_nonce = caller.nonce(&host).unwrap();

        assert_eq!(
            ExtendedExitReason::OutOfTicks,
            result.unwrap().unwrap().reason,
            "Contract creation was expected to fail and run out of ticks: \n"
        );
        assert_ne!(
            initial_caller_balance, caller_balance,
            "Gas was not deducted from the caller account"
        );
        assert_eq!(
            initial_caller_nonce + 1,
            caller_nonce,
            "Nonce should have been incremented"
        )
    }

    // This test will fail because it blows the stack with the Rust default
    // stack size.
    // use RUST_MIN_STACK=<value> cargo test -p evm-kernel --features testing
    // with <value> set to 104857600 or something similar in size
    #[ignore]
    #[test]
    fn call_too_deep_not_revert() {
        let mut host = MockHost::default();
        let block = dummy_first_block();
        let precompiles = precompiles::precompile_set::<MockHost>();
        let mut evm_account_storage = init_evm_account_storage().unwrap();

        let caller = H160::from_str("a94f5374fce5edbc8e2a8697c15331677e6ebf0b").unwrap();

        let internal_address =
            H160::from_str("7335dfb20cdcd40881235a54d61cb1152d771f4d").unwrap();

        let code = hex::decode("3060025560206000600039602060006000f000").unwrap(); // Creates an infinity of contract

        let result = run_transaction(
            &mut host,
            &block,
            &mut evm_account_storage,
            &precompiles,
            CONFIG,
            None,
            caller,
            code,
            None,
            U256::one(),
            None,
            false,
            DUMMY_ALLOCATED_TICKS,
            false,
        );

        let internal_address_nonce =
            get_nonce(&mut host, &mut evm_account_storage, &internal_address);
        let caller_nonce = get_nonce(&mut host, &mut evm_account_storage, &caller);

        assert_eq!(
            ExtendedExitReason::Exit(ExitReason::Succeed(ExitSucceed::Stopped)),
            result.unwrap().unwrap().reason,
        );

        assert_eq!(caller_nonce, U256::one());
        assert_eq!(internal_address_nonce, U256::from(2));
    }
}
