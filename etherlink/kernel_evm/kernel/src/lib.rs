// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use crate::blueprint::Queue;
use crate::error::Error;
use crate::error::UpgradeProcessError::Fallback;
use crate::inbox::KernelUpgrade;
use crate::migration::storage_migration;
use crate::safe_storage::{InternalStorage, KernelRuntime, SafeStorage, TMP_PATH};
use crate::stage_one::fetch;
use crate::storage::{read_smart_rollup_address, store_smart_rollup_address};
use crate::upgrade::upgrade_kernel;
use crate::Error::UpgradeError;
use anyhow::Context;
use block::ComputationResult;
use evm_execution::Config;
use migration::MigrationStatus;
use primitive_types::U256;
use storage::{
    is_sequencer, read_admin, read_base_fee_per_gas, read_chain_id, read_kernel_version,
    read_last_info_per_level_timestamp, read_last_info_per_level_timestamp_stats,
    read_ticketer, store_base_fee_per_gas, store_chain_id, store_kernel_version,
    store_storage_version, STORAGE_VERSION, STORAGE_VERSION_PATH,
};
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup_encoding::timestamp::Timestamp;
use tezos_smart_rollup_entrypoint::kernel_entry;
use tezos_smart_rollup_host::path::{concat, OwnedPath, RefPath};
use tezos_smart_rollup_host::runtime::Runtime;

mod apply;
mod block;
mod block_in_progress;
mod blueprint;
mod blueprint_storage;
mod error;
mod inbox;
mod indexable_storage;
mod linked_list;
mod migration;
mod mock_internal;
mod parsing;
mod safe_storage;
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

// Minimal base fee per gas
pub const BASE_FEE_PER_GAS: u32 = 21_000;

/// The configuration for the EVM execution.
pub const CONFIG: Config = Config::shanghai();

const KERNEL_VERSION: &str = env!("GIT_HASH");

pub fn stage_zero<Host: Runtime>(host: &mut Host) -> Result<MigrationStatus, Error> {
    log!(host, Info, "Entering stage zero.");
    init_storage_versioning(host)?;
    storage_migration(host)
}

/// Returns the current timestamp for the execution. Based on the last
/// info per level read (or default timestamp if it was not set), plus the
/// artifical average block time.
pub fn current_timestamp<Host: Runtime>(host: &mut Host) -> Timestamp {
    let timestamp =
        read_last_info_per_level_timestamp(host).unwrap_or_else(|_| Timestamp::from(0));
    let (numbers, total) =
        read_last_info_per_level_timestamp_stats(host).unwrap_or((1i64, 0i64));
    let average_block_time = total / numbers;
    let seconds = timestamp.i64() + average_block_time;

    Timestamp::from(seconds)
}

pub fn stage_one<Host: Runtime>(
    host: &mut Host,
    smart_rollup_address: [u8; 20],
    ticketer: Option<ContractKt1Hash>,
    admin: Option<ContractKt1Hash>,
    is_sequencer: bool,
) -> Result<Queue, anyhow::Error> {
    log!(host, Info, "Entering stage one.");
    log!(
        host,
        Info,
        "Ticketer is {:?}. Administrator is {:?}",
        ticketer,
        admin
    );

    // TODO: https://gitlab.com/tezos/tezos/-/issues/5873
    // if rebooted, don't fetch inbox
    let queue = fetch(host, smart_rollup_address, ticketer, admin, is_sequencer)?;

    for (i, queue_elt) in queue.proposals.iter().enumerate() {
        match queue_elt {
            blueprint::QueueElement::Blueprint(b) => log!(
                host,
                Info,
                "Blueprint {} contains {} transactions.",
                i,
                b.transactions.len()
            ),
            blueprint::QueueElement::BlockInProgress(bip) => log!(
                host,
                Info,
                "Block in progress {} has {} transactions left.",
                i,
                bip.queue_length()
            ),
        }
    }

    Ok(queue)
}

fn produce_and_upgrade<Host: KernelRuntime>(
    host: &mut Host,
    queue: Queue,
    kernel_upgrade: KernelUpgrade,
    chain_id: U256,
    base_fee_per_gas: U256,
) -> Result<(), anyhow::Error> {
    // Since a kernel upgrade was detected, in case an error is thrown
    // by the block production, we exceptionally "recover" from it and
    // still process the kernel upgrade.
    // In case of a reboot request, the upgrade is delayed.
    match block::produce(host, queue, chain_id, base_fee_per_gas) {
        Ok(ComputationResult::RebootNeeded) => Ok(()),
        Err(e) => {
            log!(
            host,
            Error,
            "{:?} happened during block production but a kernel upgrade was detected.",
            e);
            upgrade(host, kernel_upgrade)
        }
        Ok(ComputationResult::Finished) => upgrade(host, kernel_upgrade),
    }
}

fn upgrade<Host: Runtime>(
    host: &mut Host,
    kernel_upgrade: KernelUpgrade,
) -> Result<(), anyhow::Error> {
    // TODO: #5873
    // reboot before upgrade just in case
    upgrade_kernel(host, kernel_upgrade.preimage_hash).context("Failed to upgrade kernel")
}

pub fn stage_two<Host: KernelRuntime>(
    host: &mut Host,
    queue: Queue,
    chain_id: U256,
    base_fee_per_gas: U256,
) -> Result<(), anyhow::Error> {
    log!(host, Info, "Entering stage two.");
    let kernel_upgrade = queue.kernel_upgrade.clone();
    if let Some(kernel_upgrade) = kernel_upgrade {
        produce_and_upgrade(host, queue, kernel_upgrade, chain_id, base_fee_per_gas)
    } else {
        block::produce(host, queue, chain_id, base_fee_per_gas).map(|_| ())
    }
}

fn retrieve_smart_rollup_address<Host: Runtime>(
    host: &mut Host,
) -> Result<[u8; 20], Error> {
    match read_smart_rollup_address(host) {
        Ok(smart_rollup_address) => Ok(smart_rollup_address),
        Err(_) => {
            let rollup_metadata = Runtime::reveal_metadata(host);
            let address = rollup_metadata.raw_rollup_address;
            store_smart_rollup_address(host, &address)?;
            Ok(address)
        }
    }
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
fn retrieve_base_fee_per_gas<Host: Runtime>(host: &mut Host) -> Result<U256, Error> {
    match read_base_fee_per_gas(host) {
        Ok(base_fee_per_gas) => Ok(base_fee_per_gas),
        Err(_) => {
            let base_fee_per_gas = U256::from(BASE_FEE_PER_GAS);
            store_base_fee_per_gas(host, base_fee_per_gas)?;
            Ok(base_fee_per_gas)
        }
    }
}

pub fn main<Host: KernelRuntime>(host: &mut Host) -> Result<(), anyhow::Error> {
    let chain_id = retrieve_chain_id(host).context("Failed to retrieve chain id")?;
    let queue = if storage::was_rebooted(host)? {
        // kernel was rebooted
        log!(
            host,
            Info,
            "Kernel was rebooted. Reboot left: {}\n",
            host.reboot_left()?
        );
        storage::delete_reboot_flag(host)?;
        log!(host, Info, "Read queue.");
        storage::read_queue(host)?
    } else {
        // first kernel run of the level
        match stage_zero(host)? {
            MigrationStatus::None | MigrationStatus::Done => {
                set_kernel_version(host)?;
                let smart_rollup_address = retrieve_smart_rollup_address(host)
                    .context("Failed to retrieve smart rollup address")?;
                let ticketer = read_ticketer(host);
                let admin = read_admin(host);
                let is_sequencer = is_sequencer(host)?;
                stage_one(host, smart_rollup_address, ticketer, admin, is_sequencer)
                    .context("Failed during stage 1")?
            }
            MigrationStatus::InProgress => return Ok(()),
        }
    };

    let base_fee_per_gas = retrieve_base_fee_per_gas(host)?;
    stage_two(host, queue, chain_id, base_fee_per_gas).context("Failed during stage 2")
}

const EVM_PATH: RefPath = RefPath::assert_from(b"/evm");

const ERRORS_PATH: RefPath = RefPath::assert_from(b"/errors");

fn log_error<Host: Runtime>(
    host: &mut Host,
    err: &anyhow::Error,
) -> Result<(), anyhow::Error> {
    let current_level = storage::read_current_block_number(host).unwrap_or_default();
    let err_msg = format!("Error during block {}: {:?}", current_level, err);

    let nb_errors = host.store_count_subkeys(&ERRORS_PATH)?;
    let raw_error_path: Vec<u8> = format!("/{}", nb_errors + 1).into();
    let error_path = OwnedPath::try_from(raw_error_path)?;
    let error_path = concat(&ERRORS_PATH, &error_path)?;

    host.store_write_all(&error_path, err_msg.as_bytes())?;
    Ok(())
}

pub fn kernel_loop<Host: Runtime>(host: &mut Host) {
    // In order to setup the temporary directory, we need to move something
    // from /evm to /tmp, so /evm must be non empty, this only happen
    // at the first run.
    let evm_subkeys = host
        .store_count_subkeys(&EVM_PATH)
        .expect("The kernel failed to read the number of /evm subkeys");
    if evm_subkeys == 0 {
        host.store_write(&EVM_PATH, "Un festival de GADT".as_bytes(), 0)
            .unwrap();
    }

    host.store_copy(&EVM_PATH, &TMP_PATH)
        .expect("The kernel failed to create the temporary directory");

    let mut internal_storage = InternalStorage();
    let mut host = SafeStorage {
        host,
        internal: &mut internal_storage,
    };
    match main(&mut host) {
        Ok(()) => {
            host.promote_upgrade()
                .expect("Potential kernel upgrade promotion failed");
            host.promote(&EVM_PATH)
                .expect("The kernel failed to promote the temporary directory")
        }
        Err(e) => {
            if let Some(UpgradeError(Fallback)) = e.downcast_ref::<Error>() {
                // All the changes from the failed migration are reverted.
                host.revert()
                    .expect("The kernel failed to delete the temporary directory");
                host.fallback_backup_kernel()
                    .expect("Fallback mechanism failed");
            } else {
                log_error(host.host, &e).expect("The kernel failed to write the error");
                log!(host, Error, "The kernel produced an error: {:?}", e);
                log!(
                    host,
                    Error,
                    "The temporarily modified durable storage is discarded"
                );

                // TODO: https://gitlab.com/tezos/tezos/-/issues/5766
                // If an input is consumed then an error happens, the input
                // will be lost, this cannot happen in production.

                host.revert()
                    .expect("The kernel failed to delete the temporary directory")
            }
        }
    }
}

kernel_entry!(kernel_loop);

#[cfg(test)]
mod tests {
    use std::{ops::Rem, str::FromStr};

    use crate::mock_internal::MockInternal;
    use crate::safe_storage::{KernelRuntime, SafeStorage};
    use crate::{
        blueprint::{Blueprint, Queue, QueueElement},
        inbox::{KernelUpgrade, Transaction, TransactionContent},
        stage_two, storage,
    };
    use evm_execution::account_storage::{self, EthereumAccountStorage};
    use primitive_types::{H160, U256};
    use tezos_ethereum::{
        transaction::{TransactionHash, TransactionType},
        tx_common::EthereumTransactionCommon,
    };
    use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
    use tezos_smart_rollup_encoding::timestamp::Timestamp;
    use tezos_smart_rollup_mock::MockHost;

    const DUMMY_CHAIN_ID: U256 = U256::one();
    const DUMMY_BASE_FEE_PER_GAS: u64 = 21000u64;
    const TOO_MANY_TRANSACTIONS: u64 = 500;

    fn set_balance<Host: KernelRuntime>(
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

    fn address_from_str(s: &str) -> Option<H160> {
        let data = &hex::decode(s).unwrap();
        Some(H160::from_slice(data))
    }

    fn dummy_eth(nonce: u64) -> EthereumTransactionCommon {
        let nonce = U256::from(nonce);
        let gas_price = U256::from(40000000000u64);
        let gas_limit = 21000;
        let value = U256::from(1);
        let to = address_from_str("423163e58aabec5daa3dd1130b759d24bef0f6ea");
        let tx = EthereumTransactionCommon {
            type_: TransactionType::Legacy,
            chain_id: U256::one(),
            nonce,
            max_fee_per_gas: gas_price,
            max_priority_fee_per_gas: gas_price,
            gas_limit,
            to,
            value,
            data: vec![],
            access_list: vec![],
            signature: None,
        };

        // corresponding caller's address is 0xaf1276cbb260bb13deddb4209ae99ae6e497f446
        tx.sign_transaction(
            "dcdff53b4f013dbcdc717f89fe3bf4d8b10512aae282b48e01d7530470382701"
                .to_string(),
        )
        .unwrap()
    }

    fn hash_from_nonce(nonce: u64) -> TransactionHash {
        let nonce = u64::to_le_bytes(nonce);
        let mut hash = [0; 32];
        hash[..8].copy_from_slice(&nonce);
        hash
    }

    fn dummy_transaction(nonce: u64) -> Transaction {
        Transaction {
            tx_hash: hash_from_nonce(nonce),
            content: TransactionContent::Ethereum(dummy_eth(nonce)),
        }
    }

    fn blueprint(transactions: Vec<Transaction>) -> QueueElement {
        QueueElement::Blueprint(Blueprint {
            transactions,
            timestamp: Timestamp::from(0i64),
        })
    }

    #[test]
    fn test_reboot_during_block_production() {
        // init host
        let mut mock_host = MockHost::default();
        let mut internal = MockInternal();
        let mut host = SafeStorage {
            host: &mut mock_host,
            internal: &mut internal,
        };

        // sanity check: no current block
        assert!(
            storage::read_current_block_number(&host).is_err(),
            "Should not have found current block number"
        );

        //provision sender account
        let sender = H160::from_str("af1276cbb260bb13deddb4209ae99ae6e497f446").unwrap();
        let sender_initial_balance = U256::from(10000000000000000000u64);
        let mut evm_account_storage = account_storage::init_account_storage().unwrap();
        set_balance(
            &mut host,
            &mut evm_account_storage,
            &sender,
            sender_initial_balance,
        );

        let mut transactions = vec![];
        let mut proposals = vec![];
        for n in 0..TOO_MANY_TRANSACTIONS {
            transactions.push(dummy_transaction(n));
            if n.rem(80) == 0 {
                proposals.push(blueprint(transactions));
                transactions = vec![];
            }
        }
        // the upgrade mechanism should not start otherwise it will fail
        let broken_kernel_upgrade = KernelUpgrade {
            preimage_hash: [0u8; PREIMAGE_HASH_SIZE],
        };
        let queue = Queue {
            proposals,
            kernel_upgrade: Some(broken_kernel_upgrade),
        };

        // If the upgrade is started, it should raise an error
        stage_two(
            &mut host,
            queue,
            DUMMY_CHAIN_ID,
            DUMMY_BASE_FEE_PER_GAS.into(),
        )
        .expect("Should have produced");

        // test there is a new block
        assert!(
            storage::read_current_block_number(&host)
                .expect("should have found a block number")
                > U256::zero(),
            "There should have been multiple blocks registered"
        );

        // test reboot is set
        assert!(
            storage::was_rebooted(&mut host).expect("Should have found flag"),
            "Flag should be set"
        );
    }
}
