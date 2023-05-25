// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::U256;
use storage::{
    read_chain_id, read_last_info_per_level_timestamp,
    read_last_info_per_level_timestamp_stats, store_chain_id, store_kernel_upgrade_nonce,
};
use tezos_ethereum::block::L2Block;
use tezos_smart_rollup_debug::debug_msg;
use tezos_smart_rollup_encoding::timestamp::Timestamp;
use tezos_smart_rollup_entrypoint::kernel_entry;
use tezos_smart_rollup_host::path::{concat, OwnedPath, RefPath};
use tezos_smart_rollup_host::runtime::Runtime;

use crate::inbox::KernelUpgrade;
use crate::safe_storage::{SafeStorage, TMP_PATH};

use crate::blueprint::{fetch, Queue};
use crate::error::Error;
use crate::storage::{read_smart_rollup_address, store_smart_rollup_address};
use crate::upgrade::upgrade_kernel;

mod apply;
mod block;
mod blueprint;
mod error;
mod genesis;
mod inbox;
mod parsing;
mod safe_storage;
mod simulation;
mod storage;
mod upgrade;

/// The chain id will need to be unique when the EVM rollup is deployed in
/// production.
pub const CHAIN_ID: u32 = 1337;

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
    chain_id: U256,
) -> Result<Queue, Error> {
    let queue = fetch(host, smart_rollup_address, chain_id)?;

    for (i, blueprint) in queue.proposals.iter().enumerate() {
        debug_msg!(
            host,
            "Blueprint {} contains {} transactions.\n",
            i,
            blueprint.transactions.len()
        );
    }

    Ok(queue)
}

fn produce_and_upgrade<Host: Runtime>(
    host: &mut Host,
    queue: Queue,
    kernel_upgrade: KernelUpgrade,
) -> Result<(), Error> {
    // Since a kernel upgrade was detected, in case an error is thrown
    // by the block production, we exceptionally "recover" from it and
    // still process the kernel upgrade.
    if let Err(e) = block::produce(host, queue) {
        debug_msg!(
                host,
                "Error {:?} happened during block production but a kernel upgrade was detected.\n",
                e
            );
    }
    let upgrade_status = upgrade_kernel(host, kernel_upgrade.preimage_hash);
    if upgrade_status.is_ok() {
        let kernel_upgrade_nonce = u32::from_le_bytes(kernel_upgrade.nonce);
        store_kernel_upgrade_nonce(host, kernel_upgrade_nonce)?;
    }
    upgrade_status
}

pub fn stage_two<Host: Runtime>(host: &mut Host, queue: Queue) -> Result<(), Error> {
    debug_msg!(host, "Stage two\n");
    let kernel_upgrade = queue.kernel_upgrade.clone();
    if let Some(kernel_upgrade) = kernel_upgrade {
        produce_and_upgrade(host, queue, kernel_upgrade)
    } else {
        block::produce(host, queue)
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

fn genesis_initialisation<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    let block_path = storage::block_path(U256::zero())?;
    match Runtime::store_has(host, &block_path) {
        Ok(Some(_)) => Ok(()),
        _ => {
            store_kernel_upgrade_nonce(host, 1)?;
            genesis::init_block(host)
        }
    }
}

pub fn main<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    let smart_rollup_address = retrieve_smart_rollup_address(host)?;
    let chain_id = retrieve_chain_id(host)?;
    genesis_initialisation(host)?;

    let queue = stage_one(host, smart_rollup_address, chain_id)?;

    stage_two(host, queue)
}

const EVM_PATH: RefPath = RefPath::assert_from(b"/evm");

const ERRORS_PATH: RefPath = RefPath::assert_from(b"/errors");

fn log_error<Host: Runtime>(host: &mut Host, err: &Error) -> Result<(), Error> {
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

    let mut host = SafeStorage(host);
    match main(&mut host) {
        Ok(()) => {
            host.promote_upgrade()
                .expect("Potential kernel upgrade promotion failed");
            host.promote(&EVM_PATH)
                .expect("The kernel failed to promote the temporary directory")
        }
        Err(e) => {
            log_error(host.0, &e).expect("The kernel failed to write the error");
            debug_msg!(host, "The kernel produced an error: {:?}\n", e);
            debug_msg!(
                host,
                "The temporarily modified durable storage is discarded\n"
            );

            // TODO: https://gitlab.com/tezos/tezos/-/issues/5766
            // If an input is consumed then an error happens, the input
            // will be lost, this cannot happen in production.

            host.revert()
                .expect("The kernel failed to delete the temporary directory")
        }
    }
}

kernel_entry!(kernel_loop);
