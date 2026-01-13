// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use crate::configuration::{fetch_configuration, Configuration};
use crate::error::Error;
use crate::error::UpgradeProcessError::Fallback;
use crate::migration::storage_migration;
use crate::stage_one::fetch_blueprints;
use crate::storage::{read_sequencer_pool_address, PRIVATE_FLAG_PATH};
use anyhow::Context;
use delayed_inbox::DelayedInbox;
use evm_execution::Config;
use fallback_upgrade::fallback_backup_kernel;
use inbox::StageOneStatus;
use migration::MigrationStatus;
use primitive_types::U256;
use reveal_storage::{is_revealed_storage, reveal_storage};
use storage::{
    read_chain_id, read_da_fee, read_kernel_version, read_minimum_base_fee_per_gas,
    read_tracer_input, store_chain_id, store_da_fee, store_kernel_version,
    store_minimum_base_fee_per_gas, store_storage_version, STORAGE_VERSION,
    STORAGE_VERSION_PATH,
};
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_evm_logging::{log, Level::*, Verbosity};
use tezos_evm_runtime::internal_runtime::InternalRuntime;
use tezos_evm_runtime::runtime::{KernelHost, Runtime};
use tezos_evm_runtime::safe_storage::WORLD_STATE_PATH;
use tezos_smart_rollup::michelson::MichelsonUnit;
use tezos_smart_rollup::outbox::{
    OutboxMessage, OutboxMessageWhitelistUpdate, OUTBOX_QUEUE,
};
use tezos_smart_rollup_encoding::public_key::PublicKey;
use tezos_smart_rollup_host::runtime::ValueType;

mod apply;
mod block;
mod block_in_progress;
mod block_storage;
mod blueprint;
mod blueprint_storage;
mod bridge;
mod configuration;
mod dal;
mod dal_slot_import_signal;
mod delayed_inbox;
mod error;
mod event;
pub mod evm_node_entrypoint;
mod fallback_upgrade;
mod fees;
mod gas_price;
mod inbox;
mod linked_list;
mod migration;
mod parsing;
mod reveal_storage;
mod sequencer_blueprint;
mod simulation;
mod stage_one;
mod storage;
mod tick_model;
mod upgrade;

extern crate alloc;

/// The chain id will need to be unique when the EVM rollup is deployed in
/// production.
pub const CHAIN_ID: u32 = 1337;

/// The configuration for the EVM execution.
const CONFIG: Config = Config {
    // The current implementation doesn't support Shanghai call stack limit of 256.
    // We need to set a lower limit until we have switched to a head-based
    // recursive calls.
    //
    // TODO: When this limitation is removed, some evm evaluation tests needs
    // to be reactivated. As well as tests `call_too_deep_not_revert` and
    // `multiple_call_all_the_way_to_1024` in the evm execution crate.
    call_stack_limit: 256,
    ..Config::shanghai()
};

const KERNEL_VERSION: &str = "604663095ad8d9f537a7035821bc78112c3b865b";

fn switch_to_public_rollup<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    if let Some(ValueType::Value) = host.store_has(&PRIVATE_FLAG_PATH)? {
        log!(
            host,
            Info,
            "Submitting outbox message to make the rollup public."
        );
        let whitelist_update: OutboxMessage<_> =
            OutboxMessage::<MichelsonUnit>::WhitelistUpdate(
                OutboxMessageWhitelistUpdate { whitelist: None },
            );
        OUTBOX_QUEUE.queue_message(host, whitelist_update)?;
        OUTBOX_QUEUE.flush_queue(host);
        host.store_delete_value(&PRIVATE_FLAG_PATH)
            .map_err(Error::from)
    } else {
        Ok(())
    }
}

pub fn stage_zero<Host: Runtime>(host: &mut Host) -> Result<MigrationStatus, Error> {
    log!(host, Debug, "Entering stage zero.");
    init_storage_versioning(host)?;
    switch_to_public_rollup(host)?;
    storage_migration(host)
}

// DO NOT RENAME: function name is used during benchmark
// Never inlined when the kernel is compiled for benchmarks, to ensure the
// function is visible in the profiling results.
#[cfg_attr(feature = "benchmark", inline(never))]
pub fn stage_one<Host: Runtime>(
    host: &mut Host,
    smart_rollup_address: [u8; 20],
    configuration: &mut Configuration,
) -> Result<StageOneStatus, anyhow::Error> {
    log!(host, Debug, "Entering stage one.");
    log!(host, Debug, "Configuration: {}", configuration);

    fetch_blueprints(host, smart_rollup_address, configuration)
}

fn set_kernel_version<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    match read_kernel_version(host) {
        Ok(kernel_version) => {
            if kernel_version != KERNEL_VERSION {
                store_kernel_version(host, KERNEL_VERSION)?
            };
            Ok(())
        }
        Err(_) => store_kernel_version(host, KERNEL_VERSION),
    }
}

fn init_storage_versioning<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    match host.store_read(&STORAGE_VERSION_PATH, 0, 0) {
        Ok(_) => Ok(()),
        Err(_) => store_storage_version(host, STORAGE_VERSION),
    }
}

fn retrieve_chain_id<Host: Runtime>(host: &mut Host) -> Result<U256, Error> {
    match read_chain_id(host) {
        Ok(chain_id) => Ok(chain_id),
        Err(_) => {
            let chain_id = U256::from(CHAIN_ID);
            store_chain_id(host, chain_id)?;
            Ok(chain_id)
        }
    }
}

fn retrieve_minimum_base_fee_per_gas<Host: Runtime>(
    host: &mut Host,
) -> Result<U256, Error> {
    match read_minimum_base_fee_per_gas(host) {
        Ok(minimum_base_fee_per_gas) => Ok(minimum_base_fee_per_gas),
        Err(_) => {
            let minimum_base_fee_per_gas = crate::fees::MINIMUM_BASE_FEE_PER_GAS.into();
            store_minimum_base_fee_per_gas(host, minimum_base_fee_per_gas)?;
            Ok(minimum_base_fee_per_gas)
        }
    }
}

#[cfg(test)]
fn retrieve_base_fee_per_gas<Host: Runtime>(
    host: &mut Host,
    minimum_base_fee_per_gas: U256,
) -> U256 {
    match block_storage::read_current(host) {
        Ok(current_block) => {
            let current_base_fee_per_gas = current_block.base_fee_per_gas;
            if current_base_fee_per_gas < minimum_base_fee_per_gas {
                minimum_base_fee_per_gas
            } else {
                current_base_fee_per_gas
            }
        }
        Err(_) => minimum_base_fee_per_gas,
    }
}

fn retrieve_da_fee<Host: Runtime>(host: &mut Host) -> Result<U256, Error> {
    match read_da_fee(host) {
        Ok(da_fee) => Ok(da_fee),
        Err(_) => {
            let da_fee = U256::from(fees::DA_FEE_PER_BYTE);
            store_da_fee(host, da_fee)?;
            Ok(da_fee)
        }
    }
}

#[cfg(test)]
fn retrieve_block_fees<Host: Runtime>(
    host: &mut Host,
) -> Result<tezos_ethereum::block::BlockFees, Error> {
    let minimum_base_fee_per_gas = retrieve_minimum_base_fee_per_gas(host)?;
    let base_fee_per_gas = retrieve_base_fee_per_gas(host, minimum_base_fee_per_gas);
    let da_fee = retrieve_da_fee(host)?;
    let block_fees = tezos_ethereum::block::BlockFees::new(
        minimum_base_fee_per_gas,
        base_fee_per_gas,
        da_fee,
    );

    Ok(block_fees)
}

pub fn main<Host: Runtime>(host: &mut Host) -> Result<(), anyhow::Error> {
    let chain_id = retrieve_chain_id(host).context("Failed to retrieve chain id")?;

    // We always start by doing the migration if needed.
    match stage_zero(host) {
        Ok(MigrationStatus::None) => {
            // No migration in progress. However as we want to have the kernel
            // version written in the storage, we check for its existence
            // at every kernel run.
            // The alternative is to enforce every new kernels use the
            // installer configuration to initialize this value.
            set_kernel_version(host)?;
        }
        // If the migration is still in progress or was finished, we abort the
        // current kernel run.
        Ok(MigrationStatus::InProgress) => {
            host.mark_for_reboot()?;
            return Ok(());
        }
        Ok(MigrationStatus::Done) => {
            // If a migrtion was finished, we update the kernel version
            // in the storage.
            set_kernel_version(host)?;
            host.mark_for_reboot()?;
            let configuration = fetch_configuration(host);
            log!(
                host,
                Info,
                "Configuration after migration: {}",
                configuration
            );
            return Ok(());
        }
        Err(Error::UpgradeError(Fallback)) => {
            // If the migration failed we backup to the previous kernel
            // and force a reboot to reload the kernel.
            fallback_backup_kernel(host)?;
            host.mark_for_reboot()?;
            return Ok(());
        }
        Err(err) => return Err(err.into()),
    };

    // In the very worst case, we want to be able to upgrade the kernel at
    // any time. The kernel upgrades are retrieved from the inbox, therefore
    // we need be able to to always reach the inbox and the kernel upgrade
    // message.
    //
    // Therefore, the code between here and block production is allowed to
    // fail. It should already not be the case, but we do not want to
    // take the risk.

    // Fetch kernel metadata:

    // 1. Fetch the smart rollup address via the host function, it cannot fail.
    let smart_rollup_address = host.reveal_metadata().raw_rollup_address;
    // 2. Fetch the per mode configuration of the kernel. Returns the default
    //    configuration if it fails.
    let mut configuration = fetch_configuration(host);
    let sequencer_pool_address = read_sequencer_pool_address(host);

    // Run the stage one, this is a no-op if the inbox was already consumed
    // by another kernel run. This ensures that if the migration does not
    // consume all reboots. At least one reboot will be used to consume the
    // inbox.
    if let StageOneStatus::Reboot =
        stage_one(host, smart_rollup_address, &mut configuration)
            .context("Failed during stage 1")?
    {
        host.mark_for_reboot()?;
        return Ok(());
    };

    let trace_input = read_tracer_input(host)?;

    // Start processing blueprints
    #[cfg(not(feature = "benchmark-bypass-stage2"))]
    {
        log!(host, Debug, "Entering stage two.");
        if let block::ComputationResult::RebootNeeded = block::produce(
            host,
            chain_id,
            &mut configuration,
            sequencer_pool_address,
            trace_input,
        )
        .context("Failed during stage 2")?
        {
            host.mark_for_reboot()?;
        }
    }

    #[cfg(feature = "benchmark-bypass-stage2")]
    {
        log!(host, Benchmarking, "Shortcircuiting computation");
        return Ok(());
    }
    Ok(())
}

pub fn kernel_loop<Host: tezos_smart_rollup_host::runtime::Runtime + InternalRuntime>(
    host: &mut Host,
) {
    // In order to setup the temporary directory, we need to move something
    // from /evm to /tmp, so /evm must be non empty, this only happen
    // at the first run.

    // The kernel host is initialized as soon as possible. `kernel_loop`
    // shouldn't be called in tests as it won't use `MockInternal` for the
    // internal runtime.
    let mut host: KernelHost<Host, &mut Host> = KernelHost::init(host);

    let reboot_counter = host
        .host
        .reboot_left()
        .expect("The kernel failed to get the number of reboot left");
    if reboot_counter == 1000 {
        tezos_smart_rollup_debug::debug_msg!(
            host,
            "------------------ Kernel Invocation ------------------\n"
        )
    }

    let world_state_subkeys = host
        .host
        .store_count_subkeys(&WORLD_STATE_PATH)
        .expect("The kernel failed to read the number of /evm/world_state subkeys");

    if world_state_subkeys == 0 {
        host.host
            .store_write(&WORLD_STATE_PATH, "Un festival de GADT".as_bytes(), 0)
            .unwrap();
    }

    if is_revealed_storage(&host) {
        reveal_storage(
            &mut host,
            option_env!("EVM_SEQUENCER").map(|s| {
                PublicKey::from_b58check(s).expect("Failed parsing EVM_SEQUENCER")
            }),
            option_env!("EVM_ADMIN").map(|s| {
                ContractKt1Hash::from_base58_check(s).expect("Failed parsing EVM_ADMIN")
            }),
        );
    }

    match main(&mut host) {
        Ok(()) => (),
        Err(err) => {
            log!(&mut host, Fatal, "The kernel produced an error: {:?}", err);
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use crate::block_storage;
    use crate::blueprint_storage::store_inbox_blueprint_by_number;
    use crate::configuration::{Configuration, Limits};
    use crate::fees;
    use crate::main;
    use crate::parsing::RollupType;
    use crate::storage::{
        read_transaction_receipt_status, store_chain_id, ENABLE_FA_BRIDGE,
    };
    use crate::{
        blueprint::Blueprint,
        inbox::{Transaction, TransactionContent},
        upgrade::KernelUpgrade,
    };
    use evm_execution::account_storage::{self, EthereumAccountStorage};
    use evm_execution::fa_bridge::deposit::{ticket_hash, FaDeposit};
    use evm_execution::fa_bridge::test_utils::{
        convert_h160, convert_u256, dummy_ticket, kernel_wrapper, ticket_balance_add,
        ticket_id, SolCall,
    };
    use evm_execution::handler::RouterInterface;
    use evm_execution::precompiles::FA_BRIDGE_PRECOMPILE_ADDRESS;
    use evm_execution::utilities::{bigint_to_u256, keccak256_hash};
    use evm_execution::NATIVE_TOKEN_TICKETER_PATH;
    use pretty_assertions::assert_eq;
    use primitive_types::{H160, U256};
    use tezos_crypto_rs::hash::ContractKt1Hash;
    use tezos_data_encoding::nom::NomReader;
    use tezos_ethereum::block::BlockFees;
    use tezos_ethereum::transaction::TransactionStatus;
    use tezos_ethereum::{
        transaction::{TransactionHash, TransactionType},
        tx_common::EthereumTransactionCommon,
    };
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_evm_runtime::safe_storage::SafeStorage;

    use tezos_evm_runtime::runtime::Runtime;
    use tezos_smart_rollup::michelson::ticket::FA2_1Ticket;
    use tezos_smart_rollup::michelson::{
        MichelsonBytes, MichelsonContract, MichelsonNat, MichelsonOption, MichelsonPair,
    };
    use tezos_smart_rollup::outbox::{OutboxMessage, OutboxMessageTransaction};
    use tezos_smart_rollup::types::{Contract, Entrypoint};
    use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
    use tezos_smart_rollup_encoding::inbox::ExternalMessageFrame;
    use tezos_smart_rollup_encoding::smart_rollup::SmartRollupAddress;
    use tezos_smart_rollup_encoding::timestamp::Timestamp;
    use tezos_smart_rollup_host::path::RefPath;
    use tezos_smart_rollup_host::runtime::Runtime as SdkRuntime; // Used to put traits interface in the scope
    use tezos_smart_rollup_mock::TransferMetadata;

    const DUMMY_CHAIN_ID: U256 = U256::one();
    const DUMMY_BASE_FEE_PER_GAS: u64 = 12345u64;
    const DUMMY_DA_FEE: u64 = 2_000_000_000_000u64;

    fn dummy_block_fees() -> BlockFees {
        BlockFees::new(
            DUMMY_BASE_FEE_PER_GAS.into(),
            U256::from(DUMMY_BASE_FEE_PER_GAS),
            DUMMY_DA_FEE.into(),
        )
    }

    fn set_balance<Host: Runtime>(
        host: &mut Host,
        evm_account_storage: &mut EthereumAccountStorage,
        address: &H160,
        balance: U256,
    ) {
        let mut account = evm_account_storage
            .get_or_create(host, &account_storage::account_path(address).unwrap())
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

    fn hash_from_nonce(nonce: u64) -> TransactionHash {
        let nonce = u64::to_le_bytes(nonce);
        let mut hash = [0; 32];
        hash[..8].copy_from_slice(&nonce);
        hash
    }

    fn wrap_transaction(nonce: u64, tx: EthereumTransactionCommon) -> Transaction {
        Transaction {
            tx_hash: hash_from_nonce(nonce),
            content: TransactionContent::Ethereum(tx),
        }
    }

    fn blueprint(transactions: Vec<Transaction>) -> Blueprint {
        Blueprint {
            transactions,
            timestamp: Timestamp::from(0i64),
        }
    }

    const CREATE_LOOP_DATA: &str = "608060405234801561001057600080fd5b506101d0806100206000396000f3fe608060405234801561001057600080fd5b506004361061002b5760003560e01c80630b7d796e14610030575b600080fd5b61004a600480360381019061004591906100c2565b61004c565b005b60005b81811015610083576001600080828254610069919061011e565b92505081905550808061007b90610152565b91505061004f565b5050565b600080fd5b6000819050919050565b61009f8161008c565b81146100aa57600080fd5b50565b6000813590506100bc81610096565b92915050565b6000602082840312156100d8576100d7610087565b5b60006100e6848285016100ad565b91505092915050565b7f4e487b7100000000000000000000000000000000000000000000000000000000600052601160045260246000fd5b60006101298261008c565b91506101348361008c565b925082820190508082111561014c5761014b6100ef565b5b92915050565b600061015d8261008c565b91507fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff820361018f5761018e6100ef565b5b60018201905091905056fea26469706673582212200cd6584173dbec22eba4ce6cc7cc4e702e00e018d340f84fc0ff197faf980ad264736f6c63430008150033";

    const LOOP_1300: &str =
        "0b7d796e0000000000000000000000000000000000000000000000000000000000000514";

    const LOOP_4600: &str =
        "0b7d796e00000000000000000000000000000000000000000000000000000000000011f8";

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

        let gas_price = U256::from(DUMMY_BASE_FEE_PER_GAS);
        let gas_for_fees =
            crate::fees::gas_for_fees(DUMMY_DA_FEE.into(), gas_price, &data, &[])
                .unwrap();

        let unsigned_tx = EthereumTransactionCommon::new(
            TransactionType::Eip1559,
            Some(DUMMY_CHAIN_ID),
            nonce,
            gas_price,
            gas_price,
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

    #[test]
    fn test_reboot_during_block_production() {
        // init host
        let mut host = MockKernelHost::default();

        crate::storage::store_minimum_base_fee_per_gas(
            &mut host,
            DUMMY_BASE_FEE_PER_GAS.into(),
        )
        .unwrap();

        // sanity check: no current block
        assert!(
            block_storage::read_current_number(&host).is_err(),
            "Should not have found current block number"
        );

        //provision sender account
        let sender = H160::from_str(TEST_ADDR).unwrap();
        let sender_initial_balance = U256::from(10000000000000000000u64);
        let mut evm_account_storage = account_storage::init_account_storage().unwrap();
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
        let loop_addr: H160 =
            evm_execution::utilities::create_address_legacy(&sender, &0);
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
        // Store blueprints
        for (i, blueprint) in proposals.into_iter().enumerate() {
            store_inbox_blueprint_by_number(&mut host, blueprint, U256::from(i))
                .expect("Should have stored blueprint");
        }
        // the upgrade mechanism should not start otherwise it will fail
        let broken_kernel_upgrade = KernelUpgrade {
            preimage_hash: [0u8; PREIMAGE_HASH_SIZE],
            activation_timestamp: Timestamp::from(1_000_000i64),
        };
        crate::upgrade::store_kernel_upgrade(&mut host, &broken_kernel_upgrade)
            .expect("Should be able to store kernel upgrade");

        let block_fees = dummy_block_fees();

        // Set the tick limit to 11bn ticks - 2bn, which is the old limit minus the safety margin.
        let limits = Limits {
            maximum_allowed_ticks: 9_000_000_000,
            ..Limits::default()
        };

        let mut configuration = Configuration {
            limits,
            ..Configuration::default()
        };

        crate::storage::store_minimum_base_fee_per_gas(
            &mut host,
            block_fees.minimum_base_fee_per_gas(),
        )
        .unwrap();
        crate::storage::store_da_fee(&mut host, block_fees.da_fee_per_byte()).unwrap();

        // If the upgrade is started, it should raise an error
        let computation_result = crate::block::produce(
            &mut host,
            DUMMY_CHAIN_ID,
            &mut configuration,
            None,
            None,
        )
        .expect("Should have produced");

        // test there is a new block
        assert_eq!(
            block_storage::read_current_number(&host)
                .expect("should have found a block number"),
            U256::zero(),
            "There should have been a block registered"
        );

        // test reboot is set
        matches!(
            computation_result,
            crate::block::ComputationResult::RebootNeeded
        );
    }

    #[test]
    fn load_block_fees_new() {
        // Arrange
        let mut host = MockKernelHost::default();

        // Act
        let result = crate::retrieve_block_fees(&mut host);

        // Assert
        let expected = BlockFees::new(
            fees::MINIMUM_BASE_FEE_PER_GAS.into(),
            fees::MINIMUM_BASE_FEE_PER_GAS.into(),
            fees::DA_FEE_PER_BYTE.into(),
        );

        assert!(result.is_ok());
        assert_eq!(expected, result.unwrap());
    }

    #[test]
    fn load_min_block_fees() {
        let min_path =
            RefPath::assert_from(b"/evm/world_state/fees/minimum_base_fee_per_gas");

        // Arrange
        let mut host = MockKernelHost::default();

        let min_base_fee = U256::from(17);
        tezos_storage::write_u256_le(&mut host, &min_path, min_base_fee).unwrap();

        // Act
        let result = crate::retrieve_block_fees(&mut host);

        // Assert
        let expected =
            BlockFees::new(min_base_fee, min_base_fee, fees::DA_FEE_PER_BYTE.into());

        assert!(result.is_ok());
        assert_eq!(expected, result.unwrap());
    }

    #[test]
    fn test_xtz_withdrawal_applied() {
        // init host
        let mut host = MockKernelHost::default();
        let mut safe_storage = SafeStorage { host: &mut host };
        safe_storage
            .store_write_all(
                &NATIVE_TOKEN_TICKETER_PATH,
                b"KT1DWVsu4Jtu2ficZ1qtNheGPunm5YVniegT",
            )
            .unwrap();
        store_chain_id(&mut safe_storage, DUMMY_CHAIN_ID).unwrap();

        // run level in order to initialize outbox counter (by SOL message)
        let level = safe_storage.host.host.run_level(|_| ());

        // provision sender account
        let sender = H160::from_str("af1276cbb260bb13deddb4209ae99ae6e497f446").unwrap();
        let sender_initial_balance = U256::from(10000000000000000000u64);
        let mut evm_account_storage = account_storage::init_account_storage().unwrap();
        set_balance(
            &mut safe_storage,
            &mut evm_account_storage,
            &sender,
            sender_initial_balance,
        );

        // cast calldata "withdraw_base58(string)" "tz1RjtZUVeLhADFHDL8UwDZA6vjWWhojpu5w":
        let data = hex::decode(
            "cda4fee2\
             0000000000000000000000000000000000000000000000000000000000000020\
             0000000000000000000000000000000000000000000000000000000000000024\
             747a31526a745a5556654c6841444648444c385577445a4136766a5757686f6a70753577\
             00000000000000000000000000000000000000000000000000000000",
        )
        .unwrap();

        // create and sign precompile call
        let gas_price = U256::from(40000000000u64);
        let to = H160::from_str("ff00000000000000000000000000000000000001").unwrap();
        let tx = EthereumTransactionCommon::new(
            TransactionType::Legacy,
            Some(DUMMY_CHAIN_ID),
            0,
            gas_price,
            gas_price,
            30_000_000,
            Some(to),
            U256::from(1000000000000000000u64),
            data,
            vec![],
            None,
        );

        // corresponding caller's address is 0xaf1276cbb260bb13deddb4209ae99ae6e497f446
        let tx_payload = tx
            .sign_transaction(
                "dcdff53b4f013dbcdc717f89fe3bf4d8b10512aae282b48e01d7530470382701"
                    .to_string(),
            )
            .unwrap()
            .to_bytes();

        let tx_hash = keccak256_hash(&tx_payload);

        // encode as external message and submit to inbox
        let mut contents = Vec::new();
        contents.push(0x00); // simple tx tag
        contents.extend_from_slice(tx_hash.as_bytes());
        contents.extend_from_slice(&tx_payload);

        let message = ExternalMessageFrame::Targetted {
            address: SmartRollupAddress::from_b58check(
                "sr163Lv22CdE8QagCwf48PWDTquk6isQwv57",
            )
            .unwrap(),
            contents,
        };

        safe_storage.host.host.add_external(message);

        // run kernel twice to get to the stage with block creation:
        main(&mut safe_storage).expect("Kernel error");
        main(&mut safe_storage).expect("Kernel error");

        // verify outbox is not empty
        let outbox = safe_storage.host.host.outbox_at(level + 1);
        assert!(!outbox.is_empty());

        // check message contents:
        let message_bytes = &outbox[0];
        let (remaining, decoded_message) =
            OutboxMessage::nom_read(message_bytes.as_slice()).unwrap();
        assert!(remaining.is_empty());

        let ticketer =
            Contract::from_b58check("KT1DWVsu4Jtu2ficZ1qtNheGPunm5YVniegT").unwrap();
        let ticket = FA2_1Ticket::new(
            ticketer.clone(),
            MichelsonPair(0.into(), MichelsonOption(None)),
            1000000u64,
        )
        .unwrap();
        let receiver =
            Contract::from_b58check("tz1RjtZUVeLhADFHDL8UwDZA6vjWWhojpu5w").unwrap();
        let parameters: RouterInterface =
            MichelsonPair(MichelsonContract(receiver), ticket);
        let destination = ticketer;
        let entrypoint = Entrypoint::try_from("burn".to_string()).unwrap();

        let expected_transaction = OutboxMessageTransaction {
            parameters,
            destination,
            entrypoint,
        };
        let expected_message =
            OutboxMessage::AtomicTransactionBatch(vec![expected_transaction].into());

        assert_eq!(expected_message, decoded_message);
    }

    fn send_fa_deposit(enable_fa_bridge: bool) -> Option<TransactionStatus> {
        // init host
        let mut mock_host = MockKernelHost::default();
        let mut safe_storage = SafeStorage {
            host: &mut mock_host,
        };

        // enable FA bridge feature
        if enable_fa_bridge {
            safe_storage
                .store_write_all(&ENABLE_FA_BRIDGE, &[1u8])
                .unwrap();
        }

        // init rollup parameters (legacy optimized forge)
        // type:
        //   (or
        //     (or (pair %deposit (bytes %routing_info)
        //                        (ticket %ticket (pair %content (nat %token_id)
        //                                                       (option %metadata bytes))))
        //         (bytes %b))
        //     (bytes %c))
        // value:
        // {
        //   "deposit": {
        //     "routing_info": "01" * 20 + "02" * 20,
        //     "ticket": (
        //         "KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5",
        //         (1, None),
        //         42
        //     )
        //   }
        // }
        let params = hex::decode(
            "\
            0505050507070a00000028010101010101010101010101010101010101010102\
            0202020202020202020202020202020202020207070a0000001601d496def47a\
            3be89f5d54c6e6bb13cc6645d6e166000707070700010306002a",
        )
        .unwrap();
        let (_, payload) =
            RollupType::nom_read(&params).expect("Failed to decode params");

        let metadata = TransferMetadata::new(
            "KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5",
            "tz1P2Po7YM526ughEsRbY4oR9zaUPDZjxFrb",
        );
        safe_storage.host.host.add_transfer(payload, &metadata);

        // run kernel
        main(&mut safe_storage).expect("Kernel error");
        // QUESTION: looks like to get to the stage with block creation we need to call main twice (maybe check blueprint instead?) [1]
        main(&mut safe_storage).expect("Kernel error");

        // reconstruct ticket
        let ticket = FA2_1Ticket::new(
            Contract::Originated(
                ContractKt1Hash::from_base58_check(
                    "KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5",
                )
                .unwrap(),
            ),
            MichelsonPair::<MichelsonNat, MichelsonOption<MichelsonBytes>>(
                1u32.into(),
                MichelsonOption(None),
            ),
            42i32,
        )
        .expect("Failed to construct ticket");

        // reconstruct deposit
        let deposit = FaDeposit {
            amount: 42.into(),
            proxy: Some(H160([2u8; 20])),
            inbox_level: safe_storage.host.host.level(), // level not yet advanced
            inbox_msg_id: 2,
            receiver: H160([1u8; 20]),
            ticket_hash: ticket_hash(&ticket).unwrap(),
        };
        // smart rollup address as a seed
        let tx_hash = deposit.hash(&[0u8; 20]);

        // read transaction receipt
        read_transaction_receipt_status(&mut safe_storage, &tx_hash.0).ok()
    }

    #[test]
    fn test_fa_deposit_applied_if_feature_enabled() {
        assert_eq!(send_fa_deposit(true), Some(TransactionStatus::Success));
    }

    #[test]
    fn test_fa_deposit_rejected_if_feature_disabled() {
        assert_eq!(send_fa_deposit(false), None);
    }

    fn send_fa_withdrawal(enable_fa_bridge: bool) -> Vec<Vec<u8>> {
        // init host
        let mut mock_host = MockKernelHost::default();
        let mut safe_storage = SafeStorage {
            host: &mut mock_host,
        };

        // enable FA bridge feature
        if enable_fa_bridge {
            safe_storage
                .store_write_all(&ENABLE_FA_BRIDGE, &[1u8])
                .unwrap();
        }

        // run level in order to initialize outbox counter (by SOL message)
        let level = safe_storage.host.host.run_level(|_| ());

        // provision sender account
        let sender = H160::from_str("af1276cbb260bb13deddb4209ae99ae6e497f446").unwrap();
        let sender_initial_balance = U256::from(10000000000000000000u64);
        let mut evm_account_storage = account_storage::init_account_storage().unwrap();
        set_balance(
            &mut safe_storage,
            &mut evm_account_storage,
            &sender,
            sender_initial_balance,
        );

        // construct ticket
        let ticket = dummy_ticket();
        let ticket_hash = ticket_hash(&ticket).unwrap();
        let amount = bigint_to_u256(ticket.amount()).unwrap();

        // patch ticket table
        ticket_balance_add(
            &mut safe_storage,
            &mut evm_account_storage,
            &ticket_hash,
            &sender,
            amount,
        );

        // construct withdraw calldata
        let (ticketer, content) = ticket_id(&ticket);
        let routing_info = hex::decode("0000000000000000000000000000000000000000000001000000000000000000000000000000000000000000").unwrap();

        let data = kernel_wrapper::withdrawCall::new((
            convert_h160(&sender),
            routing_info.into(),
            convert_u256(&amount),
            ticketer.into(),
            content.into(),
        ))
        .abi_encode();

        // create and sign precompile call
        let gas_price = U256::from(40000000000u64);
        let to = FA_BRIDGE_PRECOMPILE_ADDRESS;
        let tx = EthereumTransactionCommon::new(
            TransactionType::Legacy,
            Some(U256::from(1337)),
            0,
            gas_price,
            gas_price,
            10_000_000,
            Some(to),
            U256::zero(),
            data,
            vec![],
            None,
        );

        // corresponding caller's address is 0xaf1276cbb260bb13deddb4209ae99ae6e497f446
        let tx_payload = tx
            .sign_transaction(
                "dcdff53b4f013dbcdc717f89fe3bf4d8b10512aae282b48e01d7530470382701"
                    .to_string(),
            )
            .unwrap()
            .to_bytes();

        let tx_hash = keccak256_hash(&tx_payload);

        // encode as external message and submit to inbox
        let mut contents = Vec::new();
        contents.push(0x00); // simple tx tag
        contents.extend_from_slice(tx_hash.as_bytes());
        contents.extend_from_slice(&tx_payload);

        let message = ExternalMessageFrame::Targetted {
            address: SmartRollupAddress::from_b58check(
                "sr163Lv22CdE8QagCwf48PWDTquk6isQwv57",
            )
            .unwrap(),
            contents,
        };

        safe_storage.host.host.add_external(message);

        // run kernel
        main(&mut safe_storage).expect("Kernel error");
        // QUESTION: looks like to get to the stage with block creation we need to call main twice (maybe check blueprint instead?) [2]
        main(&mut safe_storage).expect("Kernel error");

        safe_storage.host.host.outbox_at(level + 1)
    }

    #[test]
    fn test_fa_withdrawal_applied_if_feature_enabled() {
        // verify outbox is not empty
        assert_eq!(send_fa_withdrawal(true).len(), 1);
    }

    #[test]
    fn test_fa_withdrawal_rejected_if_feature_disabled() {
        // verify outbox is empty
        assert!(send_fa_withdrawal(false).is_empty());
    }
}
