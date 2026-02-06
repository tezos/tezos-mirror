// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023-2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use crate::configuration::{
    fetch_chain_configuration, fetch_configuration, Configuration, CHAIN_ID,
};
use crate::error::Error;
use crate::error::UpgradeProcessError::Fallback;
use crate::migration::storage_migration;
use crate::stage_one::fetch_blueprints;
use crate::storage::{read_sequencer_pool_address, PRIVATE_FLAG_PATH};
use anyhow::Context;
use chains::ETHERLINK_SAFE_STORAGE_ROOT_PATH;
use delayed_inbox::DelayedInbox;
use fallback_upgrade::fallback_backup_kernel;
use inbox::StageOneStatus;
use migration::MigrationStatus;
use primitive_types::U256;
use reveal_storage::{is_revealed_storage, reveal_storage};
use revm_etherlink::precompiles::initializer::init_precompile_bytecodes;
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
use tezos_smart_rollup::entrypoint;
use tezos_smart_rollup::michelson::MichelsonUnit;
use tezos_smart_rollup::outbox::{
    OutboxMessage, OutboxMessageWhitelistUpdate, OUTBOX_QUEUE,
};
use tezos_smart_rollup_encoding::public_key::PublicKey;
use tezos_smart_rollup_host::runtime::ValueType;
use tezos_tracing::trace_kernel;

mod apply;
mod block;
mod block_in_progress;
mod block_storage;
mod blueprint;
mod blueprint_storage;
mod bridge;
mod chains;
mod configuration;
mod dal;
mod dal_slot_import_signal;
mod delayed_inbox;
mod error;
mod event;
mod evm_node_entrypoint;
mod fallback_upgrade;
mod fees;
mod gas_price;
mod inbox;
mod l2block;
mod linked_list;
mod migration;
mod parsing;
pub mod registry_impl;
mod reveal_storage;
mod sequencer_blueprint;
mod simulation;
mod stage_one;
mod storage;
mod sub_block;
mod tick_model;
mod transaction;
mod upgrade;

extern crate alloc;

// This needs to be set to the frozen commit on snapshot time
const KERNEL_VERSION: &str = env!("GIT_HASH");

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

#[trace_kernel]
pub fn stage_zero<Host: Runtime>(host: &mut Host) -> Result<MigrationStatus, Error> {
    log!(host, Debug, "Entering stage zero.");
    init_storage_versioning(host)?;
    switch_to_public_rollup(host)?;
    storage_migration(host)
}

// DO NOT RENAME: function name is used during benchmark
// Never inlined when the kernel is compiled for benchmarks, to ensure the
// function is visible in the profiling results.
#[trace_kernel]
#[cfg_attr(feature = "benchmark", inline(never))]
pub fn stage_one<Host: Runtime>(
    host: &mut Host,
    smart_rollup_address: [u8; 20],
    chain_config: &chains::ChainConfig,
    configuration: &mut Configuration,
) -> Result<StageOneStatus, anyhow::Error> {
    log!(host, Debug, "Entering stage one.");
    log!(host, Debug, "Chain Configuration: {}", chain_config);
    log!(host, Debug, "Configuration: {}", configuration);

    fetch_blueprints(host, smart_rollup_address, chain_config, configuration)
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

fn retrieve_minimum_base_fee_per_gas<Host: Runtime>(host: &mut Host) -> U256 {
    match read_minimum_base_fee_per_gas(host) {
        Ok(minimum_base_fee_per_gas) => minimum_base_fee_per_gas,
        Err(_) => {
            let minimum_base_fee_per_gas = crate::fees::MINIMUM_BASE_FEE_PER_GAS.into();
            if let Err(err) =
                store_minimum_base_fee_per_gas(host, minimum_base_fee_per_gas)
            {
                log!(
                    host,
                    Error,
                    "Can't store the default minimum_base_fee: {:?}",
                    err
                );
            }
            minimum_base_fee_per_gas
        }
    }
}

#[cfg(test)]
fn retrieve_base_fee_per_gas<Host: Runtime>(
    host: &mut Host,
    minimum_base_fee_per_gas: U256,
) -> U256 {
    match block_storage::read_current_etherlink_block(host) {
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
    let minimum_base_fee_per_gas = retrieve_minimum_base_fee_per_gas(host);
    let base_fee_per_gas = retrieve_base_fee_per_gas(host, minimum_base_fee_per_gas);
    let da_fee = retrieve_da_fee(host)?;
    let block_fees = tezos_ethereum::block::BlockFees::new(
        minimum_base_fee_per_gas,
        base_fee_per_gas,
        da_fee,
    );

    Ok(block_fees)
}

pub fn run<Host: Runtime>(host: &mut Host) -> Result<(), anyhow::Error> {
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
            // If a migration was finished, we update the kernel version
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
    let chain_configuration = fetch_chain_configuration(host, chain_id);
    let mut configuration = fetch_configuration(host);
    let sequencer_pool_address = read_sequencer_pool_address(host);

    // Initialize custom precompile
    init_precompile_bytecodes(host).map_err(|_| Error::RevmPrecompileInitError)?;

    // Run the stage one, this is a no-op if the inbox was already consumed
    // by another kernel run. This ensures that if the migration does not
    // consume all reboots. At least one reboot will be used to consume the
    // inbox.
    if let StageOneStatus::Reboot = stage_one(
        host,
        smart_rollup_address,
        &chain_configuration,
        &mut configuration,
    )
    .context("Failed during stage 1")?
    {
        host.mark_for_reboot()?;
        #[cfg(not(target_arch = "riscv64"))]
        return Ok(());
    };

    let trace_input = read_tracer_input(host)?;

    // Start processing blueprints
    #[cfg(not(feature = "benchmark-bypass-stage2"))]
    {
        log!(host, Debug, "Entering stage two.");
        if let block::ComputationResult::RebootNeeded = match chain_configuration {
            chains::ChainConfig::Evm(chain_configuration) => block::produce(
                host,
                &*chain_configuration,
                &mut configuration,
                sequencer_pool_address,
                trace_input,
            ),
            chains::ChainConfig::Michelson(chain_configuration) => block::produce(
                host,
                &chain_configuration,
                &mut configuration,
                sequencer_pool_address,
                trace_input,
            ),
        }
        .context("Failed during stage 2")?
        {
            host.mark_for_reboot()?;
        }
    }

    #[cfg(feature = "benchmark-bypass-stage2")]
    {
        log!(host, Benchmarking, "Shortcircuiting computation");
        #[cfg(not(target_arch = "riscv64"))]
        return Ok(());
    }

    log!(host, Debug, "End of kernel run.");
    Ok(())
}

// `kernel_loop` shouldn't be called in tests, as it won't use `MockInternal` for the
// internal runtime. Use `kernel` instead.
#[entrypoint::main]
pub fn kernel_loop<Host: tezos_smart_rollup_host::runtime::Runtime>(host: &mut Host) {
    kernel(
        host,
        tezos_evm_runtime::internal_runtime::WasmInternalHost(),
    )
}

pub fn kernel<Host, I>(host: &mut Host, internal: I)
where
    Host: tezos_smart_rollup_host::runtime::Runtime,
    I: InternalRuntime,
{
    let mut host: KernelHost<Host, &mut Host, I> = KernelHost::init(host, internal);

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
        .store_count_subkeys(&ETHERLINK_SAFE_STORAGE_ROOT_PATH)
        .expect("The kernel failed to read the number of /evm/world_state subkeys");

    // In order to setup the temporary directory, we need to move something
    // from /evm to /tmp, so /evm must be non empty, this only happen
    // at the first run.
    if world_state_subkeys == 0 {
        host.host
            .store_write(
                &ETHERLINK_SAFE_STORAGE_ROOT_PATH,
                "Un festival de GADT".as_bytes(),
                0,
            )
            .unwrap();
    }

    let tezlink_subkeys = host
        .host
        .store_count_subkeys(&chains::TEZLINK_SAFE_STORAGE_ROOT_PATH)
        .expect("The kernel failed to read the number of /tezlink subkeys");

    if tezlink_subkeys == 0 {
        host.host
            .store_write(
                &chains::TEZLINK_SAFE_STORAGE_ROOT_PATH,
                b"Un carnaval de foncteur",
                0,
            )
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

    match run(&mut host) {
        Ok(()) => (),
        Err(err) => {
            log!(&host, Fatal, "The kernel produced an error: {:?}", err);
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use crate::block_storage::internal_for_tests::read_transaction_receipt_status;
    use crate::fees;
    use crate::parsing::RollupType;
    use crate::run;
    use crate::storage::{store_chain_id, ENABLE_FA_BRIDGE};
    use alloy_primitives::keccak256;
    use alloy_sol_types::sol;
    use pretty_assertions::assert_eq;
    use primitive_types::{H160, U256};
    use revm_etherlink::helpers::legacy::{
        alloy_to_h160, h160_to_alloy, h256_to_alloy, ticket_hash, u256_to_alloy,
        FaDeposit,
    };
    use revm_etherlink::precompiles::constants::{FA_BRIDGE_SOL_ADDR, SYSTEM_SOL_ADDR};
    use revm_etherlink::precompiles::send_outbox_message::RouterInterface;
    use revm_etherlink::storage::world_state_handler::StorageAccount;
    use revm_etherlink::storage::NATIVE_TOKEN_TICKETER_PATH;
    use tezos_crypto_rs::hash::ContractKt1Hash;
    use tezos_data_encoding::enc::BinWriter;
    use tezos_data_encoding::nom::NomReader;
    use tezos_ethereum::block::BlockFees;
    use tezos_ethereum::transaction::TransactionStatus;
    use tezos_ethereum::{
        transaction::TransactionType, tx_common::EthereumTransactionCommon,
    };
    use tezos_evm_runtime::runtime::MockKernelHost;

    use alloy_sol_types::SolCall;
    use tezos_evm_runtime::runtime::Runtime;
    use tezos_protocol::contract::Contract;
    use tezos_smart_rollup::michelson::ticket::FA2_1Ticket;
    use tezos_smart_rollup::michelson::{
        MichelsonBytes, MichelsonContract, MichelsonNat, MichelsonOption, MichelsonPair,
    };
    use tezos_smart_rollup::outbox::{OutboxMessage, OutboxMessageTransaction};
    use tezos_smart_rollup::types::Entrypoint;
    use tezos_smart_rollup_encoding::inbox::ExternalMessageFrame;
    use tezos_smart_rollup_encoding::smart_rollup::SmartRollupAddress;
    use tezos_smart_rollup_host::path::RefPath;
    use tezos_smart_rollup_host::storage::StorageV1;
    use tezos_smart_rollup_mock::TransferMetadata;

    const DUMMY_CHAIN_ID: U256 = U256::one();

    fn set_balance<Host: Runtime>(host: &mut Host, address: &H160, balance: U256) {
        let mut account = StorageAccount::from_address(&h160_to_alloy(address)).unwrap();
        let mut info = account.info(host).unwrap();
        info.balance = u256_to_alloy(&balance);
        account.set_info(host, info).unwrap();
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
    sol!(
        kernel_wrapper,
        "../revm/contracts/predeployed/abi/fa_bridge.abi"
    );

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
        host.store_write_all(
            &NATIVE_TOKEN_TICKETER_PATH,
            b"KT1DWVsu4Jtu2ficZ1qtNheGPunm5YVniegT",
        )
        .unwrap();
        store_chain_id(&mut host, DUMMY_CHAIN_ID).unwrap();

        // run level in order to initialize outbox counter (by SOL message)
        let level = host.host.run_level(|_| ());

        // provision sender account
        let sender = H160::from_str("af1276cbb260bb13deddb4209ae99ae6e497f446").unwrap();
        let sender_initial_balance = U256::from(10000000000000000000u64);
        set_balance(&mut host, &sender, sender_initial_balance);

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

        let tx_hash = keccak256(&tx_payload);

        // encode as external message and submit to inbox
        let mut contents = Vec::new();
        contents.push(0x00); // simple tx tag
        contents.extend_from_slice(tx_hash.as_slice());
        contents.extend_from_slice(&tx_payload);

        let message = ExternalMessageFrame::Targetted {
            address: SmartRollupAddress::from_b58check(
                "sr163Lv22CdE8QagCwf48PWDTquk6isQwv57",
            )
            .unwrap(),
            contents,
        };

        host.host.add_external(message);

        // run kernel twice to get to the stage with block creation:
        run(&mut host).expect("Kernel error");
        run(&mut host).expect("Kernel error");

        // verify outbox is not empty
        let outbox = host.host.outbox_at(level + 1);
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

        // enable FA bridge feature
        if enable_fa_bridge {
            mock_host
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
        mock_host.host.add_transfer(payload, &metadata);

        // run kernel
        run(&mut mock_host).expect("Kernel error");
        // QUESTION: looks like to get to the stage with block creation we need to call main twice (maybe check blueprint instead?) [1]
        run(&mut mock_host).expect("Kernel error");

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
            inbox_level: mock_host.host.level(), // level not yet advanced
            inbox_msg_id: 2,
            receiver: H160([1u8; 20]),
            ticket_hash: ticket_hash(&ticket).unwrap(),
        };
        // smart rollup address as a seed
        let tx_hash = deposit.hash(&[0u8; 20]);

        // read transaction receipt
        read_transaction_receipt_status(&mut mock_host, &tx_hash.0).ok()
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

        // enable FA bridge feature
        if enable_fa_bridge {
            mock_host.store_write(&ENABLE_FA_BRIDGE, &[1u8], 0).unwrap();
        }

        // run level in order to initialize outbox counter (by SOL message)
        let level = mock_host.host.run_level(|_| ());

        // provision sender account
        let sender = H160::from_str("af1276cbb260bb13deddb4209ae99ae6e497f446").unwrap();
        let sender_initial_balance = U256::from(10000000000000000000u64);
        set_balance(&mut mock_host, &sender, sender_initial_balance);

        // construct ticket
        let ticket = dummy_ticket();
        let ticket_hash = h256_to_alloy(&ticket_hash(&ticket).unwrap());
        let (_, bytes) = ticket.amount().to_bytes_le();
        let amount = U256::from_little_endian(&bytes);

        let mut system = StorageAccount::from_address(&SYSTEM_SOL_ADDR).unwrap();

        // patch ticket table
        let ticket_balance = system
            .read_ticket_balance(
                &mock_host,
                &revm::primitives::U256::from_be_slice(ticket_hash.as_ref()),
                &h160_to_alloy(&sender),
            )
            .unwrap();
        system
            .write_ticket_balance(
                &mut mock_host,
                &revm::primitives::U256::from_be_slice(ticket_hash.as_ref()),
                &h160_to_alloy(&sender),
                ticket_balance + u256_to_alloy(&amount),
            )
            .unwrap();

        // construct withdraw calldata
        let (ticketer, content) = ticket_id(&ticket);
        let routing_info = hex::decode("0000000000000000000000000000000000000000000001000000000000000000000000000000000000000000").unwrap();

        let data = kernel_wrapper::withdrawCall::new((
            h160_to_alloy(&sender),
            routing_info.into(),
            u256_to_alloy(&amount),
            ticketer.into(),
            content.into(),
        ))
        .abi_encode();

        // create and sign precompile call
        let gas_price = U256::from(40000000000u64);
        let to = alloy_to_h160(&FA_BRIDGE_SOL_ADDR);
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

        let tx_hash = keccak256(&tx_payload);

        // encode as external message and submit to inbox
        let mut contents = Vec::new();
        contents.push(0x00); // simple tx tag
        contents.extend_from_slice(tx_hash.as_slice());
        contents.extend_from_slice(&tx_payload);

        let message = ExternalMessageFrame::Targetted {
            address: SmartRollupAddress::from_b58check(
                "sr163Lv22CdE8QagCwf48PWDTquk6isQwv57",
            )
            .unwrap(),
            contents,
        };

        mock_host.host.add_external(message);

        // run kernel
        run(&mut mock_host).expect("Kernel error");
        // QUESTION: looks like to get to the stage with block creation we need to call main twice (maybe check blueprint instead?) [2]
        run(&mut mock_host).expect("Kernel error");

        mock_host.host.outbox_at(level + 1)
    }

    #[test]
    fn test_fa_withdrawal_applied_if_feature_enabled() {
        // verify outbox is not empty
        assert_eq!(send_fa_withdrawal(true).len(), 1);
    }
}
