// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use primitive_types::U256;
use storage::{read_chain_id, store_chain_id};
use tezos_ethereum::block::L2Block;
use tezos_smart_rollup_debug::debug_msg;
use tezos_smart_rollup_entrypoint::kernel_entry;
use tezos_smart_rollup_host::path::RefPath;
use tezos_smart_rollup_host::runtime::Runtime;

use crate::safe_storage::{SafeStorage, TMP_PATH};

use crate::blueprint::{fetch, Queue};
use crate::error::Error;
use crate::storage::{read_smart_rollup_address, store_smart_rollup_address};

mod block;
mod blueprint;
mod error;
mod genesis;
mod inbox;
mod parsing;
mod safe_storage;
mod simulation;
mod storage;

/// The chain id will need to be unique when the EVM rollup is deployed in
/// production.
pub const CHAIN_ID: u32 = 1337;

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

pub fn stage_two<Host: Runtime>(host: &mut Host, queue: Queue) -> Result<(), Error> {
    debug_msg!(host, "Stage two\n");
    block::produce(host, queue)
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
        _ => genesis::init_block(host),
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
        Ok(()) => host
            .promote(&EVM_PATH)
            .expect("The kernel failed to promote the temporary directory"),
        Err(e) => {
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
