// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use block::L2Block;
use tezos_smart_rollup_debug::debug_msg;
use tezos_smart_rollup_entrypoint::kernel_entry;
use tezos_smart_rollup_host::runtime::Runtime;

use crate::blueprint::{fetch, Queue};
use crate::error::Error;
use crate::storage::{read_smart_rollup_address, store_smart_rollup_address};

mod block;
mod blueprint;
mod error;
mod genesis;
mod helpers;
mod inbox;
mod storage;

pub fn stage_one<Host: Runtime>(
    host: &mut Host,
    smart_rollup_address: [u8; 20],
) -> Queue {
    let queue = fetch(host, smart_rollup_address);

    for (i, blueprint) in queue.proposals.iter().enumerate() {
        debug_msg!(
            host,
            "Blueprint {} contains {} transactions.\n",
            i,
            blueprint.transactions.len()
        );
    }

    queue
}

pub fn stage_two<Host: Runtime>(host: &mut Host, queue: Queue) -> Result<(), Error> {
    block::produce(host, queue)?;

    if let Ok(L2Block {
        number,
        hash,
        transactions,
        ..
    }) = storage::read_current_block(host)
    {
        debug_msg!(
            host,
            "Block {} at number {} contains {} transaction(s).\n",
            String::from_utf8(hash.to_vec()).expect("INVALID HASH"),
            number,
            transactions.len()
        )
    }

    Ok(())
}

fn retrieve_smart_rollup_address<Host: Runtime>(
    host: &mut Host,
) -> Result<[u8; 20], Error> {
    match read_smart_rollup_address(host) {
        Ok(smart_rollup_address) => Ok(smart_rollup_address),
        Err(_) => {
            let rollup_metadata = Runtime::reveal_metadata(host)?;
            let address = rollup_metadata.raw_rollup_address;
            store_smart_rollup_address(host, &address)?;
            Ok(address)
        }
    }
}

fn genesis_initialisation<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    let block_path = storage::block_path(0)?;
    match Runtime::store_has(host, &block_path) {
        Ok(Some(_)) => Ok(()),
        _ => genesis::init_block(host),
    }
}

pub fn main<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    let smart_rollup_address = retrieve_smart_rollup_address(host)?;
    genesis_initialisation(host)?;

    let queue = stage_one(host, smart_rollup_address);

    stage_two(host, queue)
}

pub fn kernel_loop<Host: Runtime>(host: &mut Host) {
    match main(host) {
        Ok(()) => (),
        Err(e) => panic!("Kernel loop failed: {:?}", e),
    }
}

kernel_entry!(kernel_loop);
