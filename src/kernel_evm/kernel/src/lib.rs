// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use primitive_types::U256;
use tezos_ethereum::block::L2Block;
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
mod inbox;
mod parsing;
mod simulation;
mod storage;

pub fn stage_one<Host: Runtime>(
    host: &mut Host,
    smart_rollup_address: [u8; 20],
) -> Result<Queue, Error> {
    let queue = fetch(host, smart_rollup_address)?;

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

fn genesis_initialisation<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    let block_path = storage::block_path(U256::zero())?;
    match Runtime::store_has(host, &block_path) {
        Ok(Some(_)) => Ok(()),
        _ => genesis::init_block(host),
    }
}

pub fn main<Host: Runtime>(host: &mut Host) -> Result<(), Error> {
    let smart_rollup_address = retrieve_smart_rollup_address(host)?;
    genesis_initialisation(host)?;

    let queue = stage_one(host, smart_rollup_address)?;

    stage_two(host, queue)
}

pub fn kernel_loop<Host: Runtime>(host: &mut Host) {
    match main(host) {
        Ok(()) => (),
        Err(e) => panic!("Kernel loop failed: {:?}", e),
    }
}

kernel_entry!(kernel_loop);
