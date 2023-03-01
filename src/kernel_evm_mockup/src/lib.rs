// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use host::rollup_core::RawRollupCore;
use host::runtime::Runtime;

use debug::debug_msg;
use kernel::kernel_entry;

use crate::account::Account;
use crate::blueprint::{fetch, Queue};
use crate::error::Error;
use crate::storage::store_account;
use crate::wei::{from_eth, Wei};

mod account;
mod block;
mod blueprint;
mod error;
mod eth_gen;
mod inbox;
mod storage;
mod wei;

pub fn stage_one<Host: Runtime + RawRollupCore>(host: &mut Host) -> Queue {
    fetch(host)
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

    // Stage 1.
    let Queue { proposals } = stage_one(host);
    for (i, blueprint) in proposals.iter().enumerate() {
        debug_msg!(host; "Blueprint {} contains {} transactions.\n", i, blueprint.transactions.len());
    }
}

kernel_entry!(main);
