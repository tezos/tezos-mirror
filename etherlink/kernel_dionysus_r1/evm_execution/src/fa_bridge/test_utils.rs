// SPDX-FileCopyrightText: 2023 PK Lab <contact@pklab.io>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

pub use alloy_sol_types::SolCall;
use alloy_sol_types::{sol, SolConstructor};
use crypto::hash::ContractKt1Hash;
use num_bigint::BigInt;
use primitive_types::{H160, H256, U256};
use tezos_data_encoding::enc::BinWriter;
use tezos_ethereum::{
    block::{BlockConstants, BlockFees},
    Log,
};
use tezos_evm_runtime::runtime::MockKernelHost;
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_encoding::{
    contract::Contract,
    michelson::{
        ticket::FA2_1Ticket, MichelsonBytes, MichelsonNat, MichelsonOption, MichelsonPair,
    },
};
use tezos_storage::read_u256_le_default;

use crate::{
    account_storage::{account_path, EthereumAccountStorage},
    configuration::EVMVersion,
    handler::{EvmHandler, ExecutionOutcome},
    precompiles::{
        self, precompile_set, FA_BRIDGE_PRECOMPILE_ADDRESS, SYSTEM_ACCOUNT_ADDRESS,
    },
    run_transaction,
    transaction_layer_data::CallContext,
    utilities::keccak256_hash,
    withdrawal_counter::WITHDRAWAL_COUNTER_PATH,
};

use super::{
    claim_fa_deposit,
    deposit::{ticket_hash, FaDeposit},
    execute_fa_withdrawal, queue_fa_deposit,
    ticket_table::{ticket_balance_path, TicketTable},
    withdrawal::FaWithdrawal,
};
use crate::fa_bridge::FaWithdrawalKind;

sol!(
    token_wrapper,
    "tests/contracts/artifacts/MockFaBridgeWrapper.abi"
);
sol!(
    kernel_wrapper,
    "tests/contracts/artifacts/MockFaBridgePrecompile.abi"
);
sol!(
    reentrancy_tester,
    "tests/contracts/artifacts/ReentrancyTester.abi"
);

const MOCK_WRAPPER_BYTECODE: &[u8] =
    include_bytes!("../../tests/contracts/artifacts/MockFaBridgeWrapper.bytecode");

const REENTRANCY_TESTER_BYTECODE: &[u8] =
    include_bytes!("../../tests/contracts/artifacts/ReentrancyTester.bytecode");

/// Create a smart contract in the storage with the mocked token code
pub fn deploy_mock_wrapper(
    host: &mut MockKernelHost,
    evm_account_storage: &mut EthereumAccountStorage,
    ticket: &FA2_1Ticket,
    caller: &H160,
    flag: u32,
) -> ExecutionOutcome {
    let code = MOCK_WRAPPER_BYTECODE.to_vec();
    let (ticketer, content) = ticket_id(ticket);
    let calldata = token_wrapper::constructorCall::new((
        ticketer.into(),
        content.into(),
        caller.0.into(),
        convert_u256(&U256::from(flag)),
    ));

    let block = dummy_block_constants();
    let precompiles = precompile_set::<MockKernelHost>(false);

    set_balance(host, evm_account_storage, caller, U256::from(1_000_000));
    run_transaction(
        host,
        &block,
        evm_account_storage,
        &precompiles,
        &EVMVersion::current_test_config(),
        None,
        *caller,
        [code, calldata.abi_encode()].concat(),
        Some(300_000),
        U256::one(),
        U256::zero(),
        false,
        None,
    )
    .expect("Failed to deploy")
    .unwrap()
}

/// Create a smart contract in the storage with the mocked token code
pub fn deploy_reentrancy_tester(
    host: &mut MockKernelHost,
    evm_account_storage: &mut EthereumAccountStorage,
    ticket: &FA2_1Ticket,
    caller: &H160,
    withdrawal_amount: U256,
    withdrawal_count: U256,
) -> ExecutionOutcome {
    let code = REENTRANCY_TESTER_BYTECODE.to_vec();
    let (ticketer, content) = ticket_id(ticket);
    let dummy_routing_info = [vec![0u8; 22], vec![1u8], vec![0u8; 21]].concat();
    let calldata = reentrancy_tester::constructorCall::new((
        convert_h160(&FA_BRIDGE_PRECOMPILE_ADDRESS),
        dummy_routing_info.into(),
        convert_u256(&withdrawal_amount),
        ticketer.into(),
        content.into(),
        convert_u256(&withdrawal_count),
    ));

    let block = dummy_block_constants();
    let precompiles = precompile_set::<MockKernelHost>(false);

    set_balance(host, evm_account_storage, caller, U256::from(1_000_000));
    run_transaction(
        host,
        &block,
        evm_account_storage,
        &precompiles,
        &EVMVersion::current_test_config(),
        None,
        *caller,
        [code, calldata.abi_encode()].concat(),
        Some(1_000_000),
        U256::one(),
        U256::zero(),
        false,
        None,
    )
    .expect("Failed to deploy")
    .unwrap()
}

/// Execute FA deposit
pub fn run_fa_deposit(
    host: &mut MockKernelHost,
    evm_account_storage: &mut EthereumAccountStorage,
    deposit: &FaDeposit,
    caller: &H160,
    gas_limit: u64,
    enable_fa_withdrawals: bool,
) -> ExecutionOutcome {
    let block = dummy_block_constants();
    let precompiles = precompile_set::<MockKernelHost>(enable_fa_withdrawals);
    let config = EVMVersion::current_test_config();

    let (outcome, nonce) = queue_fa_deposit(
        host,
        &block,
        evm_account_storage,
        &precompiles,
        &EVMVersion::current_test_config(),
        *caller,
        deposit,
        None,
    )
    .expect("Failed to queue deposit");

    if !outcome.is_success() {
        return outcome;
    }

    if let Some(nonce) = nonce {
        // We now need to claim it

        let mut handler = EvmHandler::new(
            host,
            evm_account_storage,
            *caller,
            &block,
            &config,
            &precompiles,
            U256::from(21000),
            None,
        );

        handler
            .begin_initial_transaction(
                CallContext {
                    is_static: false,
                    is_creation: false,
                },
                Some(gas_limit),
            )
            .unwrap();

        let res = claim_fa_deposit(&mut handler, nonce);

        let execution_result = match res {
            Ok(outcome) => Ok((outcome.exit_status, None, outcome.output)),
            Err(err) => Err(err),
        };

        handler.end_initial_transaction(execution_result).unwrap()
    } else {
        outcome
    }
}

/// Create FA deposit given ticket and proxy address (optional)
pub fn dummy_fa_deposit(ticket: FA2_1Ticket, proxy: Option<H160>) -> FaDeposit {
    FaDeposit {
        ticket_hash: ticket_hash(&ticket).expect("Failed to calc ticket hash"),
        proxy,
        amount: 42.into(),
        receiver: H160([4u8; 20]),
        inbox_level: 0,
        inbox_msg_id: 0,
    }
}

/// Get value of a specific slot in the proxy contract storage
/// It is used to determine if the said contract was called
///
/// See MockFaBridgeWrapper.sol where the flag is set:
///
/// function setFlag(uint256 value) internal {
///     bytes32 slot = keccak256(abi.encodePacked("FLAG_TAG"));
///     assembly {
///         sstore(slot, value)
///     }
/// }
pub fn get_storage_flag(
    host: &MockKernelHost,
    evm_account_storage: &EthereumAccountStorage,
    proxy: H160,
) -> u32 {
    let proxy_account = evm_account_storage
        .get(host, &account_path(&proxy).unwrap())
        .unwrap()
        .unwrap();

    let flag = proxy_account
        .get_storage(host, &keccak256_hash(b"FLAG_TAG"))
        .unwrap();
    U256::from_big_endian(&flag.0).as_u32()
}

/// Block constants for testing
pub fn dummy_block_constants() -> BlockConstants {
    let block_fees = BlockFees::new(
        U256::from(21000),
        U256::from(21000),
        U256::from(2_000_000_000_000u64),
    );
    let gas_limit = 1u64;
    BlockConstants::first_block(
        U256::zero(),
        U256::one(),
        block_fees,
        gas_limit,
        H160::zero(),
    )
}

/// Provision ticket balance for a specified account
pub fn ticket_balance_add(
    host: &mut impl Runtime,
    evm_account_storage: &mut EthereumAccountStorage,
    ticket_hash: &H256,
    address: &H160,
    balance: U256,
) -> bool {
    let mut system = evm_account_storage
        .get_or_create(host, &account_path(&SYSTEM_ACCOUNT_ADDRESS).unwrap())
        .unwrap();
    system
        .ticket_balance_add(host, ticket_hash, address, balance)
        .unwrap()
}

/// Get ticket balance for a specified account
pub fn ticket_balance_get(
    host: &impl Runtime,
    evm_account_storage: &EthereumAccountStorage,
    ticket_hash: &H256,
    address: &H160,
) -> U256 {
    let system = evm_account_storage
        .get(host, &account_path(&SYSTEM_ACCOUNT_ADDRESS).unwrap())
        .unwrap()
        .unwrap();

    let path = system
        .custom_path(&ticket_balance_path(ticket_hash, address).unwrap())
        .unwrap();
    read_u256_le_default(host, &path, U256::zero()).unwrap()
}

/// Get next withdrawal counter value
pub fn withdrawal_counter_next(
    host: &impl Runtime,
    evm_account_storage: &EthereumAccountStorage,
) -> Option<U256> {
    let system = evm_account_storage
        .get_or_create(host, &account_path(&SYSTEM_ACCOUNT_ADDRESS).unwrap())
        .unwrap();

    let path = system.custom_path(&WITHDRAWAL_COUNTER_PATH).unwrap();
    match host.store_read_all(&path) {
        Ok(bytes) => Some(U256::from_little_endian(&bytes)),
        _ => None,
    }
}

/// Provision TEZ balance for a specified account
pub fn set_balance(
    host: &mut impl Runtime,
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

/// Create ticket with dummy creator and content
pub fn dummy_ticket() -> FA2_1Ticket {
    use tezos_crypto_rs::hash::HashTrait;

    let ticketer = ContractKt1Hash::try_from_bytes(&[1u8; 20]).unwrap();
    FA2_1Ticket::new(
        Contract::from_b58check(&ticketer.to_base58_check()).unwrap(),
        MichelsonPair(0.into(), MichelsonOption(None)),
        1i32,
    )
    .expect("Failed to construct ticket")
}

/// Return ticket creator and content in forged form
pub fn ticket_id(ticket: &FA2_1Ticket) -> ([u8; 22], Vec<u8>) {
    let mut ticketer = Vec::new();
    ticket.creator().0.bin_write(&mut ticketer).unwrap();

    let mut content = Vec::new();
    ticket.contents().bin_write(&mut content).unwrap();

    (ticketer.try_into().unwrap(), content)
}

/// Convert U256 to the alloy primitive type
pub fn convert_u256(value: &U256) -> alloy_primitives::U256 {
    alloy_primitives::U256::from_limbs(value.0)
}

/// Convert H160 to the alloy primitive type
pub fn convert_h160(value: &H160) -> alloy_primitives::Address {
    alloy_primitives::Address::from_slice(&value.0)
}

/// Convert EVM Log to the alloy primitive type
pub fn convert_log(log: &Log) -> alloy_primitives::LogData {
    alloy_primitives::LogData::new_unchecked(
        log.topics.iter().map(|x| x.0.into()).collect(),
        log.data.clone().into(),
    )
}

/// Create block constants
pub fn dummy_first_block() -> BlockConstants {
    let block_fees = BlockFees::new(
        U256::from(21000),
        U256::from(21000),
        U256::from(2_000_000_000_000u64),
    );
    let gas_limit = 1u64;
    BlockConstants::first_block(
        U256::zero(),
        U256::one(),
        block_fees,
        gas_limit,
        H160::zero(),
    )
}

/// Create FA withdrawal given ticket and sender/owner addresses
pub fn dummy_fa_withdrawal(
    ticket: FA2_1Ticket,
    sender: H160,
    ticket_owner: H160,
) -> FaWithdrawal {
    FaWithdrawal {
        sender,
        receiver: Contract::from_b58check("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU")
            .unwrap(),
        proxy: Contract::from_b58check("KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT").unwrap(),
        amount: 42.into(),
        ticket_hash: ticket_hash(&ticket).expect("Failed to calc ticket hash"),
        ticket,
        ticket_owner,
    }
}

/// Execute FA withdrawal directly without going through the precompile
pub fn fa_bridge_precompile_call_withdraw(
    host: &mut MockKernelHost,
    evm_account_storage: &mut EthereumAccountStorage,
    withdrawal: FaWithdrawal,
    caller: H160,
) -> ExecutionOutcome {
    let block = dummy_first_block();
    let precompiles = precompiles::precompile_set::<MockKernelHost>(false);
    let config = EVMVersion::current_test_config();

    let mut handler = EvmHandler::new(
        host,
        evm_account_storage,
        caller,
        &block,
        &config,
        &precompiles,
        U256::from(21000),
        None,
    );

    handler
        .begin_initial_transaction(
            CallContext {
                is_static: false,
                is_creation: false,
            },
            Some(30_000_000),
        )
        .unwrap();

    let res = execute_fa_withdrawal(
        &mut handler,
        caller,
        FaWithdrawalKind::Standard(withdrawal),
    );

    let execution_result = match res {
        Ok(mut outcome) => {
            handler.add_withdrawals(&mut outcome.withdrawals).unwrap();
            Ok((outcome.exit_status, None, vec![]))
        }
        Err(err) => Err(err),
    };

    handler.end_initial_transaction(execution_result).unwrap()
}

pub fn create_fa_ticket(
    ticketer: &str,
    token_id: u64,
    metadata: &[u8],
    amount: BigInt,
) -> FA2_1Ticket {
    let creator =
        Contract::Originated(ContractKt1Hash::from_base58_check(ticketer).unwrap());
    let contents = MichelsonPair(
        MichelsonNat::new(BigInt::from(token_id).into()).unwrap(),
        MichelsonOption(Some(MichelsonBytes(metadata.to_vec()))),
    );
    FA2_1Ticket::new(creator, contents, amount).unwrap()
}
