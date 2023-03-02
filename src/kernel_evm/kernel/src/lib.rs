// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use block::L2Block;
use host::rollup_core::RawRollupCore;
use host::runtime::Runtime;

use debug::debug_msg;
use kernel::kernel_entry;

use crate::blueprint::{fetch, Queue};
use crate::error::Error;
use crate::storage::store_account;
use tezos_ethereum::account::Account;
use tezos_ethereum::wei::{from_eth, Wei};

mod block;
mod blueprint;
mod error;
mod inbox;
mod storage;

pub fn stage_one<Host: Runtime + RawRollupCore>(host: &mut Host) -> Queue {
    let queue = fetch(host);

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
            String::from_utf8(hash).expect("INVALID HASH"),
            number,
            transactions.len()
        )
    }
}

pub fn init_mock_account<Host: Runtime + RawRollupCore>(host: &mut Host) -> Result<(), Error> {
    let hash = ("6471a723296395cf1dcc568941affd7a390f94ce").to_ascii_lowercase();

    let balance: Wei = from_eth(9999);

    let mock_account = Account::default_account(Vec::from(hash), balance);

    store_account(host, mock_account)
}

pub fn main<Host: Runtime + RawRollupCore>(host: &mut Host) {
    match init_mock_account(host) {
        Ok(()) => (),
        Err(_) => panic!("The account should be writable"),
    }

    let queue = stage_one(host);

    stage_two(host, queue)
}

kernel_entry!(main);
