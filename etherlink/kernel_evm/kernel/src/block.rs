// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::apply::{apply_transaction, ExecutionInfo, ExecutionResult};
use crate::blueprint_storage::{drop_head_blueprint, read_next_blueprint};
use crate::error::Error;
use crate::indexable_storage::IndexableStorage;
use crate::safe_storage::KernelRuntime;
use crate::storage;
use crate::storage::init_account_index;
use crate::upgrade::KernelUpgrade;
use crate::Configuration;
use crate::{block_in_progress, tick_model};
use crate::{current_timestamp, upgrade};
use anyhow::Context;
use block_in_progress::BlockInProgress;
use evm_execution::account_storage::{init_account_storage, EthereumAccountStorage};
use evm_execution::precompiles;
use evm_execution::precompiles::PrecompileBTreeMap;
use primitive_types::{H256, U256};
use tezos_ethereum::block::BlockFees;
use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup_host::runtime::Runtime;
use tick_model::estimate_remaining_ticks_for_transaction_execution;

use tezos_ethereum::block::BlockConstants;

/// Struct used to allow the compiler to check that the tick counter value is
/// correctly moved and updated. Copy and Clone should NOT be derived.
struct TickCounter {
    c: u64,
}

impl TickCounter {
    pub fn new(c: u64) -> Self {
        Self { c }
    }
    pub fn finalize(consumed_ticks: u64) -> Self {
        Self {
            c: consumed_ticks + tick_model::constants::FINALIZE_UPPER_BOUND,
        }
    }
}

pub enum ComputationResult {
    RebootNeeded,
    Finished,
}

fn compute<Host: Runtime>(
    host: &mut Host,
    block_in_progress: &mut BlockInProgress,
    block_constants: &BlockConstants,
    precompiles: &PrecompileBTreeMap<Host>,
    evm_account_storage: &mut EthereumAccountStorage,
    accounts_index: &mut IndexableStorage,
    is_first_block_of_reboot: bool,
) -> Result<ComputationResult, anyhow::Error> {
    log!(
        host,
        Debug,
        "Queue length {}.",
        block_in_progress.queue_length()
    );
    let mut is_first_transaction = true;
    // iteration over all remaining transaction in the block
    while block_in_progress.has_tx() {
        let transaction = block_in_progress.pop_tx().ok_or(Error::Reboot)?;
        let data_size: u64 = transaction.data_size();

        // The current number of ticks remaining for the current `kernel_run` is allocated for the transaction.
        let allocated_ticks = estimate_remaining_ticks_for_transaction_execution(
            block_in_progress.estimated_ticks,
            data_size,
        );

        let retriable = !is_first_transaction || !is_first_block_of_reboot;
        // If `apply_transaction` returns `None`, the transaction should be
        // ignored, i.e. invalid signature or nonce.
        match apply_transaction(
            host,
            block_constants,
            precompiles,
            &transaction,
            block_in_progress.index,
            evm_account_storage,
            accounts_index,
            allocated_ticks,
            retriable,
        )? {
            ExecutionResult::Valid(ExecutionInfo {
                receipt_info,
                object_info,
                estimated_ticks_used,
            }) => {
                block_in_progress.register_valid_transaction(
                    &transaction,
                    object_info,
                    receipt_info,
                    estimated_ticks_used,
                    host,
                )?;
                log!(
                    host,
                    Debug,
                    "Estimated ticks after tx: {}",
                    block_in_progress.estimated_ticks
                );
            }

            ExecutionResult::Retriable(_) => {
                // It is the first block processed in this reboot. Additionally,
                // it is the first transaction processed from this block in this
                // reboot. The tick limit cannot be larger.
                log!(
                    host,
                    Debug,
                    "The transaction exhausted the ticks of the \
                         current reboot but will be retried."
                );
                block_in_progress.repush_tx(transaction);
                return Ok(ComputationResult::RebootNeeded);
            }
            ExecutionResult::Invalid => {
                block_in_progress.account_for_invalid_transaction(data_size);
                log!(
                    host,
                    Debug,
                    "Estimated ticks after tx: {}",
                    block_in_progress.estimated_ticks
                );
            }
        };
        is_first_transaction = false;
    }
    Ok(ComputationResult::Finished)
}

fn next_bip_from_blueprints<Host: Runtime>(
    host: &mut Host,
    current_block_number: U256,
    current_block_parent_hash: H256,
    current_constants: &mut BlockConstants,
    tick_counter: &TickCounter,
    config: &mut Configuration,
    kernel_upgrade: &Option<KernelUpgrade>,
) -> Result<Option<BlockInProgress>, anyhow::Error> {
    match read_next_blueprint(host, config)? {
        Some(blueprint) => {
            if let Some(kernel_upgrade) = kernel_upgrade {
                if blueprint.timestamp >= kernel_upgrade.activation_timestamp {
                    upgrade::upgrade(host, kernel_upgrade.preimage_hash)?;
                    // We abort the call, as there is no blueprint to execute,
                    // the kernel will reboot.
                    return Ok(None);
                }
            }
            let gas_price = crate::gas_price::base_fee_per_gas(host, blueprint.timestamp);
            crate::gas_price::store_new_base_fee_per_gas(
                host,
                gas_price,
                &mut current_constants.block_fees,
            )?;

            let bip = block_in_progress::BlockInProgress::from_blueprint(
                blueprint,
                current_block_number,
                current_block_parent_hash,
                current_constants,
                tick_counter.c,
            );

            tezos_evm_logging::log!(
                host,
                tezos_evm_logging::Level::Debug,
                "bip: {bip:?}"
            );
            Ok(Some(bip))
        }
        None => Ok(None),
    }
}

#[allow(clippy::too_many_arguments)]
fn compute_bip<Host: KernelRuntime>(
    host: &mut Host,
    mut block_in_progress: BlockInProgress,
    current_constants: &mut BlockConstants,
    current_block_number: &mut U256,
    current_block_parent_hash: &mut H256,
    precompiles: &PrecompileBTreeMap<Host>,
    evm_account_storage: &mut EthereumAccountStorage,
    accounts_index: &mut IndexableStorage,
    tick_counter: &mut TickCounter,
    first_block_of_reboot: &mut bool,
) -> anyhow::Result<ComputationResult> {
    let result = compute(
        host,
        &mut block_in_progress,
        current_constants,
        precompiles,
        evm_account_storage,
        accounts_index,
        *first_block_of_reboot,
    )?;
    match result {
        ComputationResult::RebootNeeded => {
            log!(
                host,
                Info,
                "Ask for reboot. Estimated ticks: {}",
                &block_in_progress.estimated_ticks
            );
            storage::store_block_in_progress(host, &block_in_progress)?;
            host.mark_for_reboot()?
        }
        ComputationResult::Finished => {
            crate::gas_price::register_block(host, &block_in_progress)?;
            *tick_counter = TickCounter::finalize(block_in_progress.estimated_ticks);
            let new_block = block_in_progress
                .finalize_and_store(host)
                .context("Failed to finalize the block in progress")?;
            *current_block_number = new_block.number + 1;
            *current_block_parent_hash = new_block.hash;
            *current_constants = new_block
                .constants(current_constants.chain_id, current_constants.block_fees);
            // Drop the processed blueprint from the storage
            drop_head_blueprint(host)?;

            *first_block_of_reboot = false;
        }
    }
    Ok(result)
}

pub fn produce<Host: KernelRuntime>(
    host: &mut Host,
    chain_id: U256,
    block_fees: BlockFees,
    config: &mut Configuration,
) -> Result<ComputationResult, anyhow::Error> {
    let kernel_upgrade = upgrade::read_kernel_upgrade(host)?;

    let (mut current_constants, mut current_block_number, mut current_block_parent_hash) =
        match storage::read_current_block(host) {
            Ok(block) => (
                block.constants(chain_id, block_fees),
                block.number + 1,
                block.hash,
            ),
            Err(_) => {
                let timestamp = current_timestamp(host);
                let timestamp = U256::from(timestamp.as_u64());
                (
                    BlockConstants::first_block(timestamp, chain_id, block_fees),
                    U256::zero(),
                    H256::from_slice(&hex::decode("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff").unwrap()),
                )
            }
        };
    let mut evm_account_storage =
        init_account_storage().context("Failed to initialize EVM account storage")?;
    let mut accounts_index = init_account_index()?;
    let precompiles = precompiles::precompile_set::<Host>();
    let mut tick_counter = TickCounter::new(0u64);
    let mut first_block_of_reboot = true;

    // Check if there's a BIP in storage to resume its execution
    match storage::read_block_in_progress(host)? {
        None => (),
        Some(block_in_progress) => match compute_bip(
            host,
            block_in_progress,
            &mut current_constants,
            &mut current_block_number,
            &mut current_block_parent_hash,
            &precompiles,
            &mut evm_account_storage,
            &mut accounts_index,
            &mut tick_counter,
            &mut first_block_of_reboot,
        )? {
            ComputationResult::Finished => storage::delete_block_in_progress(host)?,
            ComputationResult::RebootNeeded => {
                return Ok(ComputationResult::RebootNeeded)
            }
        },
    }

    upgrade::possible_sequencer_upgrade(host)?;

    // Execute stored blueprints
    while let Some(block_in_progress) = next_bip_from_blueprints(
        host,
        current_block_number,
        current_block_parent_hash,
        &mut current_constants,
        &tick_counter,
        config,
        &kernel_upgrade,
    )? {
        match compute_bip(
            host,
            block_in_progress,
            &mut current_constants,
            &mut current_block_number,
            &mut current_block_parent_hash,
            &precompiles,
            &mut evm_account_storage,
            &mut accounts_index,
            &mut tick_counter,
            &mut first_block_of_reboot,
        )? {
            ComputationResult::Finished => (),
            ComputationResult::RebootNeeded => {
                return Ok(ComputationResult::RebootNeeded)
            }
        }
    }
    log!(host, Debug, "Estimated ticks: {}", tick_counter.c);
    Ok(ComputationResult::Finished)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::blueprint::Blueprint;
    use crate::blueprint_storage::store_inbox_blueprint;
    use crate::inbox::Transaction;
    use crate::inbox::TransactionContent;
    use crate::inbox::TransactionContent::Ethereum;
    use crate::mock_internal::MockInternal;
    use crate::safe_storage::SafeStorage;
    use crate::storage::read_block_in_progress;
    use crate::storage::read_current_block;
    use crate::storage::{init_blocks_index, init_transaction_hashes_index};
    use crate::storage::{read_transaction_receipt, read_transaction_receipt_status};
    use crate::tick_model;
    use crate::{retrieve_block_fees, retrieve_chain_id};
    use evm_execution::account_storage::{
        account_path, init_account_storage, EthereumAccountStorage,
    };
    use primitive_types::{H160, U256};
    use std::str::FromStr;
    use tezos_ethereum::transaction::{
        TransactionHash, TransactionStatus, TransactionType, TRANSACTION_HASH_SIZE,
    };
    use tezos_ethereum::tx_common::EthereumTransactionCommon;
    use tezos_smart_rollup_core::SmartRollupCore;
    use tezos_smart_rollup_encoding::timestamp::Timestamp;
    use tezos_smart_rollup_host::path::RefPath;
    use tezos_smart_rollup_mock::MockHost;

    fn blueprint(transactions: Vec<Transaction>) -> Blueprint {
        Blueprint {
            transactions,
            timestamp: Timestamp::from(0i64),
        }
    }

    fn address_from_str(s: &str) -> Option<H160> {
        let data = &hex::decode(s).unwrap();
        Some(H160::from_slice(data))
    }

    fn set_balance<Host: KernelRuntime>(
        host: &mut Host,
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

    fn get_balance<Host: KernelRuntime>(
        host: &mut Host,
        evm_account_storage: &mut EthereumAccountStorage,
        address: &H160,
    ) -> U256 {
        let account = evm_account_storage
            .get_or_create(host, &account_path(address).unwrap())
            .unwrap();
        account.balance(host).unwrap()
    }

    const DUMMY_CHAIN_ID: U256 = U256::one();
    const DUMMY_BASE_FEE_PER_GAS: u64 = 21000u64;
    const DUMMY_DA_FEE: u64 = 2_000_000_000_000u64;

    fn dummy_block_fees() -> BlockFees {
        BlockFees::new(U256::from(DUMMY_BASE_FEE_PER_GAS), U256::from(DUMMY_DA_FEE))
    }

    fn dummy_eth_gen_transaction(
        nonce: u64,
        type_: TransactionType,
    ) -> EthereumTransactionCommon {
        let chain_id = Some(DUMMY_CHAIN_ID);
        let gas_price = U256::from(40000000u64);
        let gas_limit = 21000u64;

        let gas_for_fees =
            crate::fees::gas_for_fees(DUMMY_DA_FEE.into(), gas_price, &[], &[]).unwrap();
        let gas_limit = gas_limit + gas_for_fees;

        let to = address_from_str("423163e58aabec5daa3dd1130b759d24bef0f6ea");
        let value = U256::from(500000000u64);
        let data: Vec<u8> = vec![];
        EthereumTransactionCommon::new(
            type_,
            chain_id,
            nonce,
            gas_price,
            gas_price,
            gas_limit,
            to,
            value,
            data,
            vec![],
            None,
        )
    }

    // When updating the dummy txs, you can use the `resign` function to get the updated
    // signatures (see bottom of test module)
    fn dummy_eth_caller() -> H160 {
        H160::from_str("f95abdf6ede4c3703e0e9453771fbee8592d31e9").unwrap()
    }

    fn sign_transaction(tx: EthereumTransactionCommon) -> EthereumTransactionCommon {
        let private_key =
            "e922354a3e5902b5ac474f3ff08a79cff43533826b8f451ae2190b65a9d26158";

        tx.sign_transaction(private_key.to_string()).unwrap()
    }

    fn make_dummy_transaction(
        nonce: u64,
        type_: TransactionType,
    ) -> EthereumTransactionCommon {
        let tx = dummy_eth_gen_transaction(nonce, type_);
        sign_transaction(tx)
    }

    fn dummy_eth_transaction_zero() -> EthereumTransactionCommon {
        // corresponding caller's address is 0xf95abdf6ede4c3703e0e9453771fbee8592d31e9
        // private key 0xe922354a3e5902b5ac474f3ff08a79cff43533826b8f451ae2190b65a9d26158
        let nonce = 0;
        make_dummy_transaction(nonce, TransactionType::Legacy)
    }

    fn dummy_eth_transaction_one() -> EthereumTransactionCommon {
        // corresponding caller's address is 0xf95abdf6ede4c3703e0e9453771fbee8592d31e9
        // private key 0xe922354a3e5902b5ac474f3ff08a79cff43533826b8f451ae2190b65a9d26158
        let nonce = 1;
        make_dummy_transaction(nonce, TransactionType::Legacy)
    }

    fn dummy_eth_transaction_deploy_from_nonce_and_pk(
        nonce: u64,
        private_key: &str,
    ) -> EthereumTransactionCommon {
        let gas_price = U256::from(21000u64);
        // gas limit was estimated using Remix on Shanghai network (256,842)
        // plus a safety margin for gas accounting discrepancies
        let gas_limit = 300_000u64;
        let value = U256::zero();
        // corresponding contract is kernel_benchmark/scripts/benchmarks/contracts/storage.sol
        let data: Vec<u8> = hex::decode("608060405234801561001057600080fd5b5061017f806100206000396000f3fe608060405234801561001057600080fd5b50600436106100415760003560e01c80634e70b1dc1461004657806360fe47b1146100645780636d4ce63c14610080575b600080fd5b61004e61009e565b60405161005b91906100d0565b60405180910390f35b61007e6004803603810190610079919061011c565b6100a4565b005b6100886100ae565b60405161009591906100d0565b60405180910390f35b60005481565b8060008190555050565b60008054905090565b6000819050919050565b6100ca816100b7565b82525050565b60006020820190506100e560008301846100c1565b92915050565b600080fd5b6100f9816100b7565b811461010457600080fd5b50565b600081359050610116816100f0565b92915050565b600060208284031215610132576101316100eb565b5b600061014084828501610107565b9150509291505056fea2646970667358221220ec57e49a647342208a1f5c9b1f2049bf1a27f02e19940819f38929bf67670a5964736f6c63430008120033").unwrap();

        let gas_for_fees =
            crate::fees::gas_for_fees(DUMMY_DA_FEE.into(), gas_price, &data, &[])
                .unwrap();
        let gas_limit = gas_limit + gas_for_fees;

        let tx = EthereumTransactionCommon::new(
            tezos_ethereum::transaction::TransactionType::Legacy,
            Some(DUMMY_CHAIN_ID),
            nonce,
            gas_price,
            gas_price,
            gas_limit,
            None,
            value,
            data,
            vec![],
            None,
        );

        tx.sign_transaction(private_key.to_string()).unwrap()
    }

    fn dummy_eth_transaction_deploy() -> EthereumTransactionCommon {
        // corresponding caller's address is 0xaf1276cbb260bb13deddb4209ae99ae6e497f446
        dummy_eth_transaction_deploy_from_nonce_and_pk(
            0,
            "dcdff53b4f013dbcdc717f89fe3bf4d8b10512aae282b48e01d7530470382701",
        )
    }

    fn store_blueprints<Host: Runtime>(host: &mut Host, blueprints: Vec<Blueprint>) {
        for blueprint in blueprints {
            store_inbox_blueprint(host, blueprint).expect("Should have stored blueprint");
        }
    }

    fn produce_block_with_several_valid_txs<Host: KernelRuntime>(
        host: &mut Host,
        evm_account_storage: &mut EthereumAccountStorage,
    ) {
        let tx_hash_0 = [0; TRANSACTION_HASH_SIZE];
        let tx_hash_1 = [1; TRANSACTION_HASH_SIZE];

        let transactions = vec![
            Transaction {
                tx_hash: tx_hash_0,
                content: Ethereum(dummy_eth_transaction_zero()),
            },
            Transaction {
                tx_hash: tx_hash_1,
                content: Ethereum(dummy_eth_transaction_one()),
            },
        ];

        store_blueprints(host, vec![blueprint(transactions)]);

        let sender = dummy_eth_caller();
        set_balance(
            host,
            evm_account_storage,
            &sender,
            U256::from(10000000000000000000u64),
        );

        produce(
            host,
            DUMMY_CHAIN_ID,
            dummy_block_fees(),
            &mut Configuration::default(),
        )
        .expect("The block production failed.");
    }

    fn assert_current_block_reading_validity<Host: KernelRuntime>(host: &mut Host) {
        match storage::read_current_block(host) {
            Ok(_) => (),
            Err(e) => {
                panic!("Block reading failed: {:?}\n", e)
            }
        }
    }

    #[test]
    // Test if the invalid transactions are producing receipts
    fn test_invalid_transactions_receipt_status() {
        let mut mock_host = MockHost::default();
        let mut internal = MockInternal();
        let mut host = SafeStorage {
            host: &mut mock_host,
            internal: &mut internal,
        };
        crate::storage::store_minimum_base_fee_per_gas(
            &mut host,
            DUMMY_BASE_FEE_PER_GAS.into(),
        )
        .unwrap();

        let tx_hash = [0; TRANSACTION_HASH_SIZE];

        let invalid_tx = Transaction {
            tx_hash,
            content: Ethereum(dummy_eth_transaction_zero()),
        };

        let transactions: Vec<Transaction> = vec![invalid_tx];
        store_blueprints(&mut host, vec![blueprint(transactions)]);

        let mut evm_account_storage = init_account_storage().unwrap();
        let sender = dummy_eth_caller();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            U256::from(30000u64),
        );
        produce(
            &mut host,
            DUMMY_CHAIN_ID,
            dummy_block_fees(),
            &mut Configuration::default(),
        )
        .expect("The block production failed.");

        assert!(
            read_transaction_receipt_status(&mut host, &tx_hash).is_err(),
            "Invalid transaction should not have a receipt"
        );
    }

    #[test]
    // Test if a valid transaction is producing a receipt with a success status
    fn tst_valid_transactions_receipt_status() {
        let mut mock_host = MockHost::default();
        let mut internal = MockInternal();
        let mut host = SafeStorage {
            host: &mut mock_host,
            internal: &mut internal,
        };
        crate::storage::store_minimum_base_fee_per_gas(
            &mut host,
            DUMMY_BASE_FEE_PER_GAS.into(),
        )
        .unwrap();

        let tx_hash = [0; TRANSACTION_HASH_SIZE];

        let valid_tx = Transaction {
            tx_hash,
            content: Ethereum(dummy_eth_transaction_zero()),
        };

        let transactions: Vec<Transaction> = vec![valid_tx];
        store_blueprints(&mut host, vec![blueprint(transactions)]);

        let sender = dummy_eth_caller();
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            U256::from(5000000000000000u64),
        );

        produce(
            &mut host,
            DUMMY_CHAIN_ID,
            dummy_block_fees(),
            &mut Configuration::default(),
        )
        .expect("The block production failed.");

        let status = read_transaction_receipt_status(&mut host, &tx_hash)
            .expect("Should have found receipt");
        assert_eq!(TransactionStatus::Success, status);
    }

    #[test]
    // Test if a valid transaction is producing a receipt with a contract address
    fn test_valid_transactions_receipt_contract_address() {
        let mut mock_host = MockHost::default();
        let mut internal = MockInternal();
        let mut host = SafeStorage {
            host: &mut mock_host,
            internal: &mut internal,
        };
        crate::storage::store_minimum_base_fee_per_gas(
            &mut host,
            DUMMY_BASE_FEE_PER_GAS.into(),
        )
        .unwrap();

        let tx_hash = [0; TRANSACTION_HASH_SIZE];
        let tx = dummy_eth_transaction_deploy();
        assert_eq!(
            H160::from_str("af1276cbb260bb13deddb4209ae99ae6e497f446").unwrap(),
            tx.caller().unwrap()
        );
        let valid_tx = Transaction {
            tx_hash,
            content: Ethereum(dummy_eth_transaction_deploy()),
        };

        let transactions: Vec<Transaction> = vec![valid_tx];
        store_blueprints(&mut host, vec![blueprint(transactions)]);

        let sender = H160::from_str("af1276cbb260bb13deddb4209ae99ae6e497f446").unwrap();
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            U256::from(5000000000000000u64),
        );

        produce(
            &mut host,
            DUMMY_CHAIN_ID,
            dummy_block_fees(),
            &mut Configuration::default(),
        )
        .expect("The block production failed.");
        let receipt = read_transaction_receipt(&mut host, &tx_hash)
            .expect("should have found receipt");
        assert_eq!(TransactionStatus::Success, receipt.status);
        assert_eq!(
            H160::from_str("af1276cbb260bb13deddb4209ae99ae6e497f446").unwrap(),
            receipt.from
        );
        assert_eq!(
            Some(H160::from_str("d9d427235f5746ffd1d5a0d850e77880a94b668f").unwrap()),
            receipt.contract_address
        );
    }

    #[test]
    // Test if several valid transactions can be performed
    fn test_several_valid_transactions() {
        let mut mock_host = MockHost::default();
        let mut internal = MockInternal();
        let mut host = SafeStorage {
            host: &mut mock_host,
            internal: &mut internal,
        };
        crate::storage::store_minimum_base_fee_per_gas(
            &mut host,
            DUMMY_BASE_FEE_PER_GAS.into(),
        )
        .unwrap();

        let mut evm_account_storage = init_account_storage().unwrap();

        produce_block_with_several_valid_txs(&mut host, &mut evm_account_storage);

        let dest_address =
            H160::from_str("423163e58aabec5daa3dd1130b759d24bef0f6ea").unwrap();
        let dest_balance =
            get_balance(&mut host, &mut evm_account_storage, &dest_address);

        assert_eq!(dest_balance, U256::from(1000000000u64))
    }

    #[test]
    // Test if several valid proposals can produce valid blocks
    fn test_several_valid_proposals() {
        let mut mock_host = MockHost::default();
        let mut internal = MockInternal();
        let mut host = SafeStorage {
            host: &mut mock_host,
            internal: &mut internal,
        };
        crate::storage::store_minimum_base_fee_per_gas(
            &mut host,
            DUMMY_BASE_FEE_PER_GAS.into(),
        )
        .unwrap();

        let tx_hash_0 = [0; TRANSACTION_HASH_SIZE];
        let tx_hash_1 = [1; TRANSACTION_HASH_SIZE];

        let transaction_0 = vec![Transaction {
            tx_hash: tx_hash_0,
            content: Ethereum(dummy_eth_transaction_zero()),
        }];

        let transaction_1 = vec![Transaction {
            tx_hash: tx_hash_1,
            content: Ethereum(dummy_eth_transaction_one()),
        }];

        store_blueprints(
            &mut host,
            vec![blueprint(transaction_0), blueprint(transaction_1)],
        );

        let sender = dummy_eth_caller();
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            U256::from(10000000000000000000u64),
        );

        produce(
            &mut host,
            DUMMY_CHAIN_ID,
            dummy_block_fees(),
            &mut Configuration::default(),
        )
        .expect("The block production failed.");

        let dest_address =
            H160::from_str("423163e58aabec5daa3dd1130b759d24bef0f6ea").unwrap();
        let dest_balance =
            get_balance(&mut host, &mut evm_account_storage, &dest_address);

        assert_eq!(dest_balance, U256::from(1000000000u64))
    }

    #[test]
    // Test transfers gas consumption consistency
    fn test_cumulative_transfers_gas_consumption() {
        let mut mock_host = MockHost::default();
        let mut internal = MockInternal();
        let mut host = SafeStorage {
            host: &mut mock_host,
            internal: &mut internal,
        };

        let base_gas = U256::from(21000);
        crate::storage::store_minimum_base_fee_per_gas(&mut host, base_gas).unwrap();
        let gas_for_fees = U256::from(7479012);

        let tx_hash_0 = [0; TRANSACTION_HASH_SIZE];
        let tx_hash_1 = [1; TRANSACTION_HASH_SIZE];

        let transactions = vec![
            Transaction {
                tx_hash: tx_hash_0,
                content: Ethereum(dummy_eth_transaction_zero()),
            },
            Transaction {
                tx_hash: tx_hash_1,
                content: Ethereum(dummy_eth_transaction_one()),
            },
        ];

        store_blueprints(&mut host, vec![blueprint(transactions)]);

        let sender = dummy_eth_caller();
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            U256::from(10000000000000000000u64),
        );

        produce(
            &mut host,
            DUMMY_CHAIN_ID,
            dummy_block_fees(),
            &mut Configuration::default(),
        )
        .expect("The block production failed.");
        let receipt0 = read_transaction_receipt(&mut host, &tx_hash_0)
            .expect("should have found receipt");
        let receipt1 = read_transaction_receipt(&mut host, &tx_hash_1)
            .expect("should have found receipt");

        assert_eq!(receipt0.cumulative_gas_used, base_gas + gas_for_fees);
        assert_eq!(
            receipt1.cumulative_gas_used,
            receipt0.cumulative_gas_used + base_gas + gas_for_fees
        );
    }

    #[test]
    // Test if we're able to read current block (with a filled queue) after
    // a block production
    fn test_read_storage_current_block_after_block_production_with_filled_queue() {
        let mut mock_host = MockHost::default();
        let mut internal = MockInternal();
        let mut host = SafeStorage {
            host: &mut mock_host,
            internal: &mut internal,
        };

        let mut evm_account_storage = init_account_storage().unwrap();

        produce_block_with_several_valid_txs(&mut host, &mut evm_account_storage);

        assert_current_block_reading_validity(&mut host);
    }

    #[test]
    // Test that the same transaction can not be replayed twice
    fn test_replay_attack() {
        let mut mock_host = MockHost::default();
        let mut internal = MockInternal();
        let mut host = SafeStorage {
            host: &mut mock_host,
            internal: &mut internal,
        };
        crate::storage::store_minimum_base_fee_per_gas(
            &mut host,
            DUMMY_BASE_FEE_PER_GAS.into(),
        )
        .unwrap();

        let tx = Transaction {
            tx_hash: [0; TRANSACTION_HASH_SIZE],
            content: Ethereum(dummy_eth_transaction_zero()),
        };

        let transactions = vec![tx.clone(), tx];
        store_blueprints(
            &mut host,
            vec![blueprint(transactions.clone()), blueprint(transactions)],
        );

        let sender = dummy_eth_caller();
        let initial_sender_balance = U256::from(10000000000000000000u64);
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            initial_sender_balance,
        );

        produce(
            &mut host,
            DUMMY_CHAIN_ID,
            dummy_block_fees(),
            &mut Configuration::default(),
        )
        .expect("The block production failed.");

        let dest_address =
            H160::from_str("423163e58aabec5daa3dd1130b759d24bef0f6ea").unwrap();
        let sender_balance = get_balance(&mut host, &mut evm_account_storage, &sender);
        let dest_balance =
            get_balance(&mut host, &mut evm_account_storage, &dest_address);

        let expected_dest_balance = U256::from(500000000u64);
        let expected_gas = 21000;
        let da_fee = crate::fees::da_fee(DUMMY_DA_FEE.into(), &[], &[]);
        let rounding_amount = 39000000;
        let expected_fees = dummy_block_fees().base_fee_per_gas() * expected_gas
            + da_fee
            + rounding_amount;
        let expected_sender_balance =
            initial_sender_balance - expected_dest_balance - expected_fees;

        assert_eq!(dest_balance, expected_dest_balance);
        assert_eq!(sender_balance, expected_sender_balance, "sender balance");
    }

    #[test]
    //Test accounts are indexed at the end of the block production
    fn test_accounts_are_indexed() {
        let mut mock_host = MockHost::default();
        let mut internal = MockInternal();
        let mut host = SafeStorage {
            host: &mut mock_host,
            internal: &mut internal,
        };
        crate::storage::store_minimum_base_fee_per_gas(
            &mut host,
            DUMMY_BASE_FEE_PER_GAS.into(),
        )
        .unwrap();

        let accounts_index = init_account_index().unwrap();

        let tx = Transaction {
            tx_hash: [0; TRANSACTION_HASH_SIZE],
            content: Ethereum(dummy_eth_transaction_zero()),
        };

        let transactions = vec![tx];
        store_blueprints(&mut host, vec![blueprint(transactions)]);

        let indexed_accounts = accounts_index.length(&host).unwrap();

        let sender = dummy_eth_caller();
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            U256::from(5000000000000000u64),
        );
        produce(
            &mut host,
            DUMMY_CHAIN_ID,
            dummy_block_fees(),
            &mut Configuration::default(),
        )
        .expect("The block production failed.");

        let indexed_accounts_after_produce = accounts_index.length(&host).unwrap();

        // The sender and receiver have never been indexed yet, and are indexed
        // whether the transaction succeeds or not.
        assert_eq!(
            indexed_accounts_after_produce,
            indexed_accounts + 2,
            "The new accounts haven't been indexed"
        )
    }

    #[test]
    //Test accounts are indexed at the end of the block production
    fn test_accounts_are_indexed_once() {
        let mut mock_host = MockHost::default();
        let mut internal = MockInternal();
        let mut host = SafeStorage {
            host: &mut mock_host,
            internal: &mut internal,
        };

        let accounts_index = init_account_index().unwrap();

        let tx = Transaction {
            tx_hash: [0; TRANSACTION_HASH_SIZE],
            content: Ethereum(dummy_eth_transaction_zero()),
        };

        let transactions = vec![tx];
        store_blueprints(&mut host, vec![blueprint(transactions.clone())]);

        produce(
            &mut host,
            DUMMY_CHAIN_ID,
            dummy_block_fees(),
            &mut Configuration::default(),
        )
        .expect("The block production failed.");

        let indexed_accounts = accounts_index.length(&host).unwrap();
        // Next blueprint
        store_blueprints(&mut host, vec![blueprint(transactions)]);
        produce(
            &mut host,
            DUMMY_CHAIN_ID,
            dummy_block_fees(),
            &mut Configuration::default(),
        )
        .expect("The block production failed.");

        let indexed_accounts_after_second_produce = accounts_index.length(&host).unwrap();

        // The sender and receiver have never been indexed yet
        assert_eq!(
            indexed_accounts, indexed_accounts_after_second_produce,
            "Accounts have been indexed twice"
        )
    }

    #[test]
    fn test_blocks_and_transactions_are_indexed() {
        let mut mock_host = MockHost::default();
        let mut internal = MockInternal();
        let mut host = SafeStorage {
            host: &mut mock_host,
            internal: &mut internal,
        };
        crate::storage::store_minimum_base_fee_per_gas(
            &mut host,
            DUMMY_BASE_FEE_PER_GAS.into(),
        )
        .unwrap();

        let blocks_index = init_blocks_index().unwrap();
        let transaction_hashes_index = init_transaction_hashes_index().unwrap();

        let tx_hash = [0; TRANSACTION_HASH_SIZE];

        let tx = Transaction {
            tx_hash,
            content: Ethereum(dummy_eth_transaction_zero()),
        };

        let transactions = vec![tx];
        store_blueprints(&mut host, vec![blueprint(transactions)]);

        let number_of_blocks_indexed = blocks_index.length(&host).unwrap();
        let number_of_transactions_indexed =
            transaction_hashes_index.length(&host).unwrap();

        let sender = dummy_eth_caller();
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            U256::from(10000000000000000000u64),
        );
        produce(
            &mut host,
            DUMMY_CHAIN_ID,
            dummy_block_fees(),
            &mut Configuration::default(),
        )
        .expect("The block production failed.");

        let new_number_of_blocks_indexed = blocks_index.length(&host).unwrap();
        let new_number_of_transactions_indexed =
            transaction_hashes_index.length(&host).unwrap();

        let current_block_hash = storage::read_current_block(&mut host)
            .unwrap()
            .hash
            .as_bytes()
            .to_vec();

        assert_eq!(number_of_blocks_indexed + 1, new_number_of_blocks_indexed);
        assert_eq!(
            number_of_transactions_indexed + 1,
            new_number_of_transactions_indexed
        );

        assert_eq!(
            Ok(current_block_hash),
            blocks_index.get_value(&host, new_number_of_blocks_indexed - 1)
        );

        let last_indexed_transaction =
            transaction_hashes_index.length(&host).unwrap() - 1;

        assert_eq!(
            Ok(tx_hash.to_vec()),
            transaction_hashes_index.get_value(&host, last_indexed_transaction)
        );
    }

    fn first_block<MockHost: Runtime>(host: &mut MockHost) -> BlockConstants {
        let timestamp = current_timestamp(host);
        let timestamp = U256::from(timestamp.as_u64());
        let chain_id = retrieve_chain_id(host);
        let block_fees = retrieve_block_fees(host);
        assert!(chain_id.is_ok(), "chain_id should be defined");
        assert!(block_fees.is_ok(), "block fees should be defined");
        BlockConstants::first_block(timestamp, chain_id.unwrap(), block_fees.unwrap())
    }

    #[test]
    fn test_stop_computation() {
        // init host
        let mut mock_host = MockHost::default();
        let mut internal = MockInternal();
        let mut host = SafeStorage {
            host: &mut mock_host,
            internal: &mut internal,
        };

        let block_constants = first_block(&mut host);
        let precompiles = precompiles::precompile_set();
        let mut accounts_index = init_account_index().unwrap();

        //provision sender account
        let sender = H160::from_str("af1276cbb260bb13deddb4209ae99ae6e497f446").unwrap();
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            U256::from(10000000000000000000u64),
        );

        // tx is valid because correct nonce and account provisionned
        let valid_tx = Transaction {
            tx_hash: [0; TRANSACTION_HASH_SIZE],
            content: TransactionContent::Ethereum(dummy_eth_transaction_zero()),
        };
        let transactions = vec![valid_tx.clone()].into();

        // init block in progress
        let mut block_in_progress =
            BlockInProgress::new(U256::from(1), U256::from(1), transactions);
        // block is almost full wrt ticks
        block_in_progress.estimated_ticks = tick_model::constants::MAX_TICKS - 1000;

        let data_length = valid_tx.data_size();
        let ticks_for_invalid = tick_model::ticks_of_invalid_transaction(data_length);

        // act
        compute(
            &mut host,
            &mut block_in_progress,
            &block_constants,
            &precompiles,
            &mut evm_account_storage,
            &mut accounts_index,
            true,
        )
        .expect("Should have computed block");

        // assert

        // block in progress should not have registered any gas or ticks
        assert_eq!(
            block_in_progress.cumulative_gas,
            U256::from(0),
            "should not have consumed any gas"
        );
        assert_eq!(
            block_in_progress.estimated_ticks,
            tick_model::constants::MAX_TICKS - 1000 + ticks_for_invalid,
            "should not have consumed any tick"
        );

        // the transaction should not have been processed
        let dest_address =
            H160::from_str("423163e58aabec5daa3dd1130b759d24bef0f6ea").unwrap();
        let sender_balance = get_balance(&mut host, &mut evm_account_storage, &sender);
        let dest_balance =
            get_balance(&mut host, &mut evm_account_storage, &dest_address);
        assert_eq!(sender_balance, U256::from(10000000000000000000u64));
        assert_eq!(dest_balance, U256::from(0u64))
    }

    #[test]
    fn invalid_transaction_should_bump_nonce() {
        let mut mock_host = MockHost::default();
        let mut internal = MockInternal();
        let mut host = SafeStorage {
            host: &mut mock_host,
            internal: &mut internal,
        };

        let mut evm_account_storage = init_account_storage().unwrap();

        let caller =
            address_from_str("f95abdf6ede4c3703e0e9453771fbee8592d31e9").unwrap();

        // Get the balance before the transaction, i.e. 0.
        let caller_account = evm_account_storage
            .get_or_create(&host, &account_path(&caller).unwrap())
            .unwrap();
        let default_nonce = caller_account.nonce(&host).unwrap();
        assert_eq!(default_nonce, 0, "default nonce should be 0");

        let tx = dummy_eth_transaction_zero();
        // Ensures the caller has enough balance to pay for the fees, but not
        // the transaction itself, otherwise the transaction will not even be
        // taken into account.
        let fees = U256::from(21000) * tx.gas_limit_with_fees();
        set_balance(&mut host, &mut evm_account_storage, &caller, fees);

        // Prepare a invalid transaction, i.e. with not enough funds.
        let tx_hash = [0; TRANSACTION_HASH_SIZE];
        let transaction = Transaction {
            tx_hash,
            content: Ethereum(tx),
        };
        store_blueprints(&mut host, vec![blueprint(vec![transaction])]);

        // Apply the transaction
        produce(
            &mut host,
            DUMMY_CHAIN_ID,
            dummy_block_fees(),
            &mut Configuration::default(),
        )
        .expect("The block production failed.");
        assert!(
            read_transaction_receipt(&mut host, &tx_hash).is_err(),
            "Transaction is invalid, so should not have a receipt"
        );

        // Nonce should not have been bumped
        let nonce = caller_account.nonce(&host).unwrap();
        assert_eq!(nonce, default_nonce, "nonce should not have been bumped");
    }

    /// A blueprint that should produce 1 block with an invalid transaction
    fn almost_empty_blueprint() -> Blueprint {
        let tx_hash = [0; TRANSACTION_HASH_SIZE];

        // transaction should be invalid
        let tx = Transaction {
            tx_hash,
            content: Ethereum(dummy_eth_transaction_one()),
        };

        let transactions = vec![tx];

        blueprint(transactions)
    }

    fn check_current_block_number<Host: Runtime>(host: &mut Host, nb: usize) {
        let current_nb = storage::read_current_block_number(host)
            .expect("Should have manage to check block number");
        assert_eq!(current_nb, U256::from(nb), "Incorrect block number");
    }

    #[test]
    fn test_first_blocks() {
        let mut mock_host = MockHost::default();
        let mut internal = MockInternal();
        let mut host = SafeStorage {
            host: &mut mock_host,
            internal: &mut internal,
        };

        // first block should be 0
        let blueprint = almost_empty_blueprint();
        store_inbox_blueprint(&mut host, blueprint).expect("Should store a blueprint");
        produce(
            &mut host,
            DUMMY_CHAIN_ID,
            dummy_block_fees(),
            &mut Configuration::default(),
        )
        .expect("Empty block should have been produced");
        check_current_block_number(&mut host, 0);

        // second block
        let blueprint = almost_empty_blueprint();
        store_inbox_blueprint(&mut host, blueprint).expect("Should store a blueprint");
        produce(
            &mut host,
            DUMMY_CHAIN_ID,
            dummy_block_fees(),
            &mut Configuration::default(),
        )
        .expect("Empty block should have been produced");
        check_current_block_number(&mut host, 1);

        // third block
        let blueprint = almost_empty_blueprint();
        store_inbox_blueprint(&mut host, blueprint).expect("Should store a blueprint");
        produce(
            &mut host,
            DUMMY_CHAIN_ID,
            dummy_block_fees(),
            &mut Configuration::default(),
        )
        .expect("Empty block should have been produced");
        check_current_block_number(&mut host, 2);
    }

    fn hash_from_nonce(nonce: u64) -> TransactionHash {
        let nonce = u64::to_le_bytes(nonce);
        let mut hash = [0; 32];
        hash[..8].copy_from_slice(&nonce);
        hash
    }

    fn is_marked_for_reboot(host: &impl SmartRollupCore) -> bool {
        const REBOOT_PATH: RefPath = RefPath::assert_from(b"/kernel/env/reboot");
        host.store_read_all(&REBOOT_PATH).is_ok()
    }

    fn assert_marked_for_reboot(host: &impl SmartRollupCore) {
        assert!(
            is_marked_for_reboot(host),
            "The kernel should have been marked for reboot"
        );
    }

    const CREATE_LOOP_DATA: &str = "608060405234801561001057600080fd5b506101d0806100206000396000f3fe608060405234801561001057600080fd5b506004361061002b5760003560e01c80630b7d796e14610030575b600080fd5b61004a600480360381019061004591906100c2565b61004c565b005b60005b81811015610083576001600080828254610069919061011e565b92505081905550808061007b90610152565b91505061004f565b5050565b600080fd5b6000819050919050565b61009f8161008c565b81146100aa57600080fd5b50565b6000813590506100bc81610096565b92915050565b6000602082840312156100d8576100d7610087565b5b60006100e6848285016100ad565b91505092915050565b7f4e487b7100000000000000000000000000000000000000000000000000000000600052601160045260246000fd5b60006101298261008c565b91506101348361008c565b925082820190508082111561014c5761014b6100ef565b5b92915050565b600061015d8261008c565b91507fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff820361018f5761018e6100ef565b5b60018201905091905056fea26469706673582212200cd6584173dbec22eba4ce6cc7cc4e702e00e018d340f84fc0ff197faf980ad264736f6c63430008150033";

    const LOOP_1300: &str =
        "0b7d796e0000000000000000000000000000000000000000000000000000000000000514";

    const LOOP_4600: &str =
        "0b7d796e00000000000000000000000000000000000000000000000000000000000011f8";

    const LOOP_5800: &str =
        "0b7d796e00000000000000000000000000000000000000000000000000000000000016a8";

    const TEST_SK: &str =
        "84e147b8bc36d99cc6b1676318a0635d8febc9f02897b0563ad27358589ee502";

    const TEST_ADDR: &str = "f0affc80a5f69f4a9a3ee01a640873b6ba53e539";

    fn create_and_sign_transaction(
        data: &str,
        nonce: u64,
        gas_limit: u64,
        to: Option<H160>,
        secret_key: &str,
    ) -> EthereumTransactionCommon {
        let data = hex::decode(data).unwrap();

        let gas_for_fees = crate::fees::gas_for_fees(
            DUMMY_DA_FEE.into(),
            DUMMY_BASE_FEE_PER_GAS.into(),
            &data,
            &[],
        )
        .unwrap();

        let unsigned_tx = EthereumTransactionCommon::new(
            TransactionType::Eip1559,
            Some(DUMMY_CHAIN_ID),
            nonce,
            U256::from(DUMMY_BASE_FEE_PER_GAS),
            U256::from(DUMMY_BASE_FEE_PER_GAS),
            gas_limit + gas_for_fees,
            to,
            U256::zero(),
            data,
            vec![],
            None,
        );
        unsigned_tx
            .sign_transaction(String::from(secret_key))
            .unwrap()
    }

    fn wrap_transaction(nonce: u64, tx: EthereumTransactionCommon) -> Transaction {
        Transaction {
            tx_hash: hash_from_nonce(nonce),
            content: TransactionContent::Ethereum(tx),
        }
    }

    #[test]
    fn test_reboot_many_tx_one_proposal() {
        // init host
        let mut mock_host = MockHost::default();
        let mut internal = MockInternal();
        let mut host = SafeStorage {
            host: &mut mock_host,
            internal: &mut internal,
        };
        crate::storage::store_minimum_base_fee_per_gas(
            &mut host,
            DUMMY_BASE_FEE_PER_GAS.into(),
        )
        .unwrap();

        // sanity check: no current block
        assert!(
            storage::read_current_block_number(&host).is_err(),
            "Should not have found current block number"
        );

        //provision sender account
        let sender = H160::from_str(TEST_ADDR).unwrap();
        let sender_initial_balance = U256::from(10000000000000000000u64);
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            sender_initial_balance,
        );

        // These transactions are generated with the loop.sol contract, which are:
        // - create the contract
        // - call `loop(1200)`
        // - call `loop(4600)`
        let create_transaction =
            create_and_sign_transaction(CREATE_LOOP_DATA, 0, 3_000_000, None, TEST_SK);
        let loop_addr: H160 = evm_execution::handler::create_address_legacy(&sender, &0);
        let loop_1200_tx =
            create_and_sign_transaction(LOOP_1300, 1, 900_000, Some(loop_addr), TEST_SK);
        let loop_4600_tx = create_and_sign_transaction(
            LOOP_4600,
            2,
            2_600_000,
            Some(loop_addr),
            TEST_SK,
        );

        let proposals = vec![
            wrap_transaction(0, create_transaction),
            wrap_transaction(1, loop_1200_tx),
            wrap_transaction(2, loop_4600_tx),
        ];

        store_blueprints(&mut host, vec![blueprint(proposals)]);

        host.reboot_left().expect("should be some reboot left");

        produce(
            &mut host,
            DUMMY_CHAIN_ID,
            dummy_block_fees(),
            &mut Configuration::default(),
        )
        .expect("Should have produced");

        // test no new block
        assert!(
            storage::read_current_block_number(&host).is_err(),
            "Should not have found current block number"
        );

        // test reboot is set
        assert_marked_for_reboot(host.host)
    }

    #[test]
    fn test_reboot_many_tx_many_proposal() {
        // init host
        let mut mock_host = MockHost::default();
        let mut internal = MockInternal();
        let mut host = SafeStorage {
            host: &mut mock_host,
            internal: &mut internal,
        };

        crate::storage::store_minimum_base_fee_per_gas(
            &mut host,
            DUMMY_BASE_FEE_PER_GAS.into(),
        )
        .unwrap();

        // sanity check: no current block
        assert!(
            storage::read_current_block_number(&host).is_err(),
            "Should not have found current block number"
        );
        //provision sender account
        let sender = H160::from_str(TEST_ADDR).unwrap();
        let sender_initial_balance = U256::from(10000000000000000000u64);
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            sender_initial_balance,
        );

        // These transactions are generated with the loop.sol contract, which are:
        // - create the contract
        // - call `loop(1200)`
        // - call `loop(4600)`
        let create_transaction =
            create_and_sign_transaction(CREATE_LOOP_DATA, 0, 3_000_000, None, TEST_SK);
        let loop_addr: H160 = evm_execution::handler::create_address_legacy(&sender, &0);
        let loop_1200_tx =
            create_and_sign_transaction(LOOP_1300, 1, 900_000, Some(loop_addr), TEST_SK);
        let loop_4600_tx = create_and_sign_transaction(
            LOOP_4600,
            2,
            2_600_000,
            Some(loop_addr),
            TEST_SK,
        );

        let proposals = vec![
            blueprint(vec![wrap_transaction(0, create_transaction)]),
            blueprint(vec![
                wrap_transaction(1, loop_1200_tx),
                wrap_transaction(2, loop_4600_tx),
            ]),
        ];

        store_blueprints(&mut host, proposals);

        produce(
            &mut host,
            DUMMY_CHAIN_ID,
            dummy_block_fees(),
            &mut Configuration::default(),
        )
        .expect("Should have produced");

        // test no new block
        assert_eq!(
            storage::read_current_block_number(&host)
                .expect("should have found a block number"),
            U256::zero(),
            "There should have been one block registered"
        );

        // test reboot is set
        assert_marked_for_reboot(host.host);

        let bip = read_block_in_progress(&host)
            .expect("Should be able to read the block in progress")
            .expect("The reboot context should have a block in progress");

        assert!(
            bip.queue_length() > 0,
            "There should be some transactions left"
        );

        let _next_blueprint =
            read_next_blueprint(&mut host, &mut Configuration::default())
                .expect("The next blueprint should be available");
    }

    #[test]
    fn test_transaction_pre_eip155() {
        // This test injects a presigned transaction defined by
        // https://github.com/mds1/multicall#new-deployments, which is a well
        // known contract on multiple EVM chains. The transaction has been
        // signed without a chain id, so that it can be reproduced on multiple
        // networks (and the contract address is the same on any chain).
        //
        // The purpose of this test is to check the kernel accepts such a
        // transaction, with the expected hash and the expected contract
        // address.

        // init host
        let mut mock_host = MockHost::default();
        let mut internal = MockInternal();
        let mut host = SafeStorage {
            host: &mut mock_host,
            internal: &mut internal,
        };

        // see
        // https://basescan.org/tx/0x07471adfe8f4ec553c1199f495be97fc8be8e0626ae307281c22534460184ed1
        // for example, as the transaction has the same hash on every EVM Chain.
        let expected_tx_hash = hex::decode(
            "07471adfe8f4ec553c1199f495be97fc8be8e0626ae307281c22534460184ed1",
        )
        .unwrap();

        // Extracted from https://github.com/mds1/multicall#new-deployments
        let signed_transaction = hex::decode("f90f538085174876e800830f42408080b90f00608060405234801561001057600080fd5b50610ee0806100206000396000f3fe6080604052600436106100f35760003560e01c80634d2301cc1161008a578063a8b0574e11610059578063a8b0574e1461025a578063bce38bd714610275578063c3077fa914610288578063ee82ac5e1461029b57600080fd5b80634d2301cc146101ec57806372425d9d1461022157806382ad56cb1461023457806386d516e81461024757600080fd5b80633408e470116100c65780633408e47014610191578063399542e9146101a45780633e64a696146101c657806342cbb15c146101d957600080fd5b80630f28c97d146100f8578063174dea711461011a578063252dba421461013a57806327e86d6e1461015b575b600080fd5b34801561010457600080fd5b50425b6040519081526020015b60405180910390f35b61012d610128366004610a85565b6102ba565b6040516101119190610bbe565b61014d610148366004610a85565b6104ef565b604051610111929190610bd8565b34801561016757600080fd5b50437fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0140610107565b34801561019d57600080fd5b5046610107565b6101b76101b2366004610c60565b610690565b60405161011193929190610cba565b3480156101d257600080fd5b5048610107565b3480156101e557600080fd5b5043610107565b3480156101f857600080fd5b50610107610207366004610ce2565b73ffffffffffffffffffffffffffffffffffffffff163190565b34801561022d57600080fd5b5044610107565b61012d610242366004610a85565b6106ab565b34801561025357600080fd5b5045610107565b34801561026657600080fd5b50604051418152602001610111565b61012d610283366004610c60565b61085a565b6101b7610296366004610a85565b610a1a565b3480156102a757600080fd5b506101076102b6366004610d18565b4090565b60606000828067ffffffffffffffff8111156102d8576102d8610d31565b60405190808252806020026020018201604052801561031e57816020015b6040805180820190915260008152606060208201528152602001906001900390816102f65790505b5092503660005b8281101561047757600085828151811061034157610341610d60565b6020026020010151905087878381811061035d5761035d610d60565b905060200281019061036f9190610d8f565b6040810135958601959093506103886020850185610ce2565b73ffffffffffffffffffffffffffffffffffffffff16816103ac6060870187610dcd565b6040516103ba929190610e32565b60006040518083038185875af1925050503d80600081146103f7576040519150601f19603f3d011682016040523d82523d6000602084013e6103fc565b606091505b50602080850191909152901515808452908501351761046d577f08c379a000000000000000000000000000000000000000000000000000000000600052602060045260176024527f4d756c746963616c6c333a2063616c6c206661696c656400000000000000000060445260846000fd5b5050600101610325565b508234146104e6576040517f08c379a000000000000000000000000000000000000000000000000000000000815260206004820152601a60248201527f4d756c746963616c6c333a2076616c7565206d69736d6174636800000000000060448201526064015b60405180910390fd5b50505092915050565b436060828067ffffffffffffffff81111561050c5761050c610d31565b60405190808252806020026020018201604052801561053f57816020015b606081526020019060019003908161052a5790505b5091503660005b8281101561068657600087878381811061056257610562610d60565b90506020028101906105749190610e42565b92506105836020840184610ce2565b73ffffffffffffffffffffffffffffffffffffffff166105a66020850185610dcd565b6040516105b4929190610e32565b6000604051808303816000865af19150503d80600081146105f1576040519150601f19603f3d011682016040523d82523d6000602084013e6105f6565b606091505b5086848151811061060957610609610d60565b602090810291909101015290508061067d576040517f08c379a000000000000000000000000000000000000000000000000000000000815260206004820152601760248201527f4d756c746963616c6c333a2063616c6c206661696c656400000000000000000060448201526064016104dd565b50600101610546565b5050509250929050565b43804060606106a086868661085a565b905093509350939050565b6060818067ffffffffffffffff8111156106c7576106c7610d31565b60405190808252806020026020018201604052801561070d57816020015b6040805180820190915260008152606060208201528152602001906001900390816106e55790505b5091503660005b828110156104e657600084828151811061073057610730610d60565b6020026020010151905086868381811061074c5761074c610d60565b905060200281019061075e9190610e76565b925061076d6020840184610ce2565b73ffffffffffffffffffffffffffffffffffffffff166107906040850185610dcd565b60405161079e929190610e32565b6000604051808303816000865af19150503d80600081146107db576040519150601f19603f3d011682016040523d82523d6000602084013e6107e0565b606091505b506020808401919091529015158083529084013517610851577f08c379a000000000000000000000000000000000000000000000000000000000600052602060045260176024527f4d756c746963616c6c333a2063616c6c206661696c656400000000000000000060445260646000fd5b50600101610714565b6060818067ffffffffffffffff81111561087657610876610d31565b6040519080825280602002602001820160405280156108bc57816020015b6040805180820190915260008152606060208201528152602001906001900390816108945790505b5091503660005b82811015610a105760008482815181106108df576108df610d60565b602002602001015190508686838181106108fb576108fb610d60565b905060200281019061090d9190610e42565b925061091c6020840184610ce2565b73ffffffffffffffffffffffffffffffffffffffff1661093f6020850185610dcd565b60405161094d929190610e32565b6000604051808303816000865af19150503d806000811461098a576040519150601f19603f3d011682016040523d82523d6000602084013e61098f565b606091505b506020830152151581528715610a07578051610a07576040517f08c379a000000000000000000000000000000000000000000000000000000000815260206004820152601760248201527f4d756c746963616c6c333a2063616c6c206661696c656400000000000000000060448201526064016104dd565b506001016108c3565b5050509392505050565b6000806060610a2b60018686610690565b919790965090945092505050565b60008083601f840112610a4b57600080fd5b50813567ffffffffffffffff811115610a6357600080fd5b6020830191508360208260051b8501011115610a7e57600080fd5b9250929050565b60008060208385031215610a9857600080fd5b823567ffffffffffffffff811115610aaf57600080fd5b610abb85828601610a39565b90969095509350505050565b6000815180845260005b81811015610aed57602081850181015186830182015201610ad1565b81811115610aff576000602083870101525b50601f017fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe0169290920160200192915050565b600082825180855260208086019550808260051b84010181860160005b84811015610bb1578583037fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe001895281518051151584528401516040858501819052610b9d81860183610ac7565b9a86019a9450505090830190600101610b4f565b5090979650505050505050565b602081526000610bd16020830184610b32565b9392505050565b600060408201848352602060408185015281855180845260608601915060608160051b870101935082870160005b82811015610c52577fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffa0888703018452610c40868351610ac7565b95509284019290840190600101610c06565b509398975050505050505050565b600080600060408486031215610c7557600080fd5b83358015158114610c8557600080fd5b9250602084013567ffffffffffffffff811115610ca157600080fd5b610cad86828701610a39565b9497909650939450505050565b838152826020820152606060408201526000610cd96060830184610b32565b95945050505050565b600060208284031215610cf457600080fd5b813573ffffffffffffffffffffffffffffffffffffffff81168114610bd157600080fd5b600060208284031215610d2a57600080fd5b5035919050565b7f4e487b7100000000000000000000000000000000000000000000000000000000600052604160045260246000fd5b7f4e487b7100000000000000000000000000000000000000000000000000000000600052603260045260246000fd5b600082357fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff81833603018112610dc357600080fd5b9190910192915050565b60008083357fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe1843603018112610e0257600080fd5b83018035915067ffffffffffffffff821115610e1d57600080fd5b602001915036819003821315610a7e57600080fd5b8183823760009101908152919050565b600082357fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc1833603018112610dc357600080fd5b600082357fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffa1833603018112610dc357600080fdfea2646970667358221220bb2b5c71a328032f97c676ae39a1ec2148d3e5d6f73d95e9b17910152d61f16264736f6c634300080c00331ca0edce47092c0f398cebf3ffc267f05c8e7076e3b89445e0fe50f6332273d4569ba01b0b9d000e19b24c5869b0fc3b22b0d6fa47cd63316875cbbd577d76e6fde086").unwrap();

        let transaction = EthereumTransactionCommon::from_bytes(&signed_transaction)
            .expect("The MultiCall3 transaction shouldn't be unparsable");

        println!("Transaction: {:?}", transaction);

        let mut tx_hash = [0; TRANSACTION_HASH_SIZE];
        tx_hash.copy_from_slice(&expected_tx_hash);

        let tx = Transaction {
            tx_hash,
            content: Ethereum(transaction),
        };

        let transactions: Vec<Transaction> = vec![tx];

        store_blueprints(&mut host, vec![blueprint(transactions)]);

        let sender = H160::from_str("05f32b3cc3888453ff71b01135b34ff8e41263f2").unwrap();
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            U256::from(500000000000000000u64),
        );

        produce(
            &mut host,
            DUMMY_CHAIN_ID,
            dummy_block_fees(),
            &mut Configuration::default(),
        )
        .expect("The block production failed.");

        // See address at https://www.multicall3.com/ on in the github repository linked above
        let expected_created_contract =
            H160::from_str("0xcA11bde05977b3631167028862bE2a173976CA11").unwrap();

        let receipt = read_transaction_receipt(&mut host, &tx_hash)
            .expect("Should have found receipt");
        assert_eq!(TransactionStatus::Success, receipt.status);
        assert_eq!(Some(expected_created_contract), receipt.contract_address);
    }

    #[test]
    fn test_non_retriable_transaction_are_marked_as_failed() {
        // init host
        let mut mock_host = MockHost::default();
        let mut internal = MockInternal();
        let mut host = SafeStorage {
            host: &mut mock_host,
            internal: &mut internal,
        };
        crate::storage::store_minimum_base_fee_per_gas(
            &mut host,
            DUMMY_BASE_FEE_PER_GAS.into(),
        )
        .unwrap();

        // sanity check: no current block
        assert!(
            storage::read_current_block_number(&host).is_err(),
            "Should not have found current block number"
        );

        //provision sender account
        let sender = H160::from_str(TEST_ADDR).unwrap();
        let sender_initial_balance = U256::from(10000000000000000000u64);
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            sender_initial_balance,
        );

        // These transactions are generated with the loop.sol contract, which are:
        // - create the contract
        // - call `loop(1200)`
        // - call `loop(5800)`
        let create_transaction =
            create_and_sign_transaction(CREATE_LOOP_DATA, 0, 3_000_000, None, TEST_SK);
        let loop_addr: H160 = evm_execution::handler::create_address_legacy(&sender, &0);
        let loop_5800_tx = create_and_sign_transaction(
            LOOP_5800,
            1,
            4_000_000,
            Some(loop_addr),
            TEST_SK,
        );

        let proposals_first_reboot = vec![wrap_transaction(0, create_transaction)];

        store_blueprints(&mut host, vec![blueprint(proposals_first_reboot)]);

        produce(
            &mut host,
            DUMMY_CHAIN_ID,
            dummy_block_fees(),
            &mut Configuration::default(),
        )
        .expect("Should have produced");

        assert!(
            storage::read_current_block_number(&host).is_ok(),
            "Should have found a block"
        );

        // We start a new proposal, calling produce again simulates a reboot.

        let proposals_second_reboot = vec![wrap_transaction(2, loop_5800_tx)];

        store_blueprints(&mut host, vec![blueprint(proposals_second_reboot)]);

        produce(
            &mut host,
            DUMMY_CHAIN_ID,
            dummy_block_fees(),
            &mut Configuration::default(),
        )
        .expect("Should have produced");

        let block = read_current_block(&mut host).expect("Should have found a block");
        let failed_loop_hash = block
            .transactions
            .first()
            .expect("There should have been a transaction");
        let failed_loop_status =
            storage::read_transaction_receipt_status(&mut host, failed_loop_hash)
                .expect("There should have been a receipt");

        assert_eq!(
            failed_loop_status,
            TransactionStatus::Failure,
            "The transaction should have failed"
        )
    }

    // Comment out `ignore` when resigning the dummy transactions
    #[test]
    #[ignore = "Only run when re-signing required"]
    fn resign() {
        println!("Dummy eth transactions");
        // corresponding caller's address is 0xf95abdf6ede4c3703e0e9453771fbee8592d31e9
        let private_key =
            "e922354a3e5902b5ac474f3ff08a79cff43533826b8f451ae2190b65a9d26158";

        let zero = dummy_eth_transaction_zero();
        let one = dummy_eth_transaction_one();

        let zero_resigned = zero.sign_transaction(private_key.to_string()).unwrap();
        let one_resigned = one.sign_transaction(private_key.to_string()).unwrap();

        assert_eq!(zero, zero_resigned);
        assert_eq!(one, one_resigned);
    }

    #[test]
    // Test if a valid transaction is producing a receipt with a success status
    fn test_type_propagation() {
        let mut mock_host = MockHost::default();
        let mut internal = MockInternal();
        let mut host = SafeStorage {
            host: &mut mock_host,
            internal: &mut internal,
        };
        crate::storage::store_minimum_base_fee_per_gas(
            &mut host,
            DUMMY_BASE_FEE_PER_GAS.into(),
        )
        .unwrap();

        let tx_hash = [0; TRANSACTION_HASH_SIZE];
        let tx_hash_eip1559 = [1; TRANSACTION_HASH_SIZE];
        let tx_hash_eip2930 = [2; TRANSACTION_HASH_SIZE];

        let valid_tx = Transaction {
            tx_hash,
            content: Ethereum(make_dummy_transaction(0, TransactionType::Legacy)),
        };

        let valid_tx_eip1559 = Transaction {
            tx_hash: tx_hash_eip1559,
            content: Ethereum(make_dummy_transaction(1, TransactionType::Eip1559)),
        };

        let valid_tx_eip2930 = Transaction {
            tx_hash: tx_hash_eip2930,
            content: Ethereum(make_dummy_transaction(2, TransactionType::Eip2930)),
        };

        let transactions: Vec<Transaction> =
            vec![valid_tx, valid_tx_eip1559, valid_tx_eip2930];
        store_blueprints(&mut host, vec![blueprint(transactions)]);

        let sender = dummy_eth_caller();
        let mut evm_account_storage = init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            U256::from(5000000000000000u64),
        );

        produce(
            &mut host,
            DUMMY_CHAIN_ID,
            dummy_block_fees(),
            &mut Configuration::default(),
        )
        .expect("The block production failed.");

        let receipt = read_transaction_receipt(&mut host, &tx_hash)
            .expect("Should have found receipt");
        assert_eq!(receipt.type_, TransactionType::Legacy);

        let receipt_eip1559 = read_transaction_receipt(&mut host, &tx_hash_eip1559)
            .expect("Should have found receipt");
        assert_eq!(receipt_eip1559.type_, TransactionType::Eip1559);

        let receipt_eip2930 = read_transaction_receipt(&mut host, &tx_hash_eip2930)
            .expect("Should have found receipt");
        assert_eq!(receipt_eip2930.type_, TransactionType::Eip2930);
    }
}
