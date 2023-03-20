// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use block::L2Block;
use host::rollup_core::RawRollupCore;
use host::runtime::Runtime;

use debug::debug_msg;
use kernel::kernel_entry;

use crate::blueprint::{fetch, Queue};
use crate::storage::{read_smart_rollup_address, store_smart_rollup_address};

mod block;
mod blueprint;
mod error;
mod genesis;
mod helpers;
mod inbox;
mod storage;

pub fn stage_one<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    smart_rollup_address: [u8; 20],
) -> Queue {
    let queue = fetch(host, smart_rollup_address);

    for (i, blueprint) in queue.proposals.iter().enumerate() {
        debug_msg!(host; "Blueprint {} contains {} transactions.\n", i, blueprint.transactions.len());
    }

    queue
}

pub fn stage_two<Host: Runtime + RawRollupCore>(host: &mut Host, queue: Queue) {
    block::produce(host, queue);

    if let Ok(L2Block {
        number,
        hash,
        transactions,
        ..
    }) = storage::read_current_block(host)
    {
        debug_msg!(host; "Block {} at number {} contains {} transaction(s).\n",
            String::from_utf8(hash.to_vec()).expect("INVALID HASH"),
            number,
            transactions.len()
        )
    }
}

fn retrieve_smart_rollup_address<Host: Runtime + RawRollupCore>(host: &mut Host) -> [u8; 20] {
    match read_smart_rollup_address(host) {
        Ok(smart_rollup_address) => smart_rollup_address,
        Err(_) => {
            let address = Runtime::reveal_metadata(host).unwrap().raw_rollup_address;
            store_smart_rollup_address(host, address).unwrap();
            address
        }
    }
}

fn genesis_initialisation<Host: Runtime + RawRollupCore>(host: &mut Host) {
    let block_path = storage::block_path(0).unwrap();
    match Runtime::store_has(host, &block_path) {
        Ok(Some(_)) => (),
        _ => genesis::init_block(host).unwrap_or_else(|_| {
            panic!("Error while initializing block genesis: stopping the daemon.")
        }),
    }
}

pub fn main<Host: Runtime + RawRollupCore>(host: &mut Host) {
    let smart_rollup_address = retrieve_smart_rollup_address(host);

    genesis_initialisation(host);

    let queue = stage_one(host, smart_rollup_address);

    stage_two(host, queue)
}

kernel_entry!(main);
