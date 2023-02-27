// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use host::rollup_core::RawRollupCore;
use host::runtime::Runtime;

use kernel::kernel_entry;

use crate::account::Account;
use crate::blueprint::Queue;
use crate::error::Error;
use crate::inbox::Inbox;
use crate::storage::store_account;
use crate::wei::{from_eth, Wei};

mod account;
mod blueprint;
mod error;
mod inbox;
mod storage;
mod wei;

pub fn stage_one<Host: Runtime + RawRollupCore>(host: &mut Host, max_bytes: usize, queue: Queue) {
    if let Ok(messages) = Inbox::read_input(host, max_bytes) {
        Queue::add(queue, messages)
    }
}

pub fn init_mock_account<Host: Runtime + RawRollupCore>(host: &mut Host) -> Result<(), Error> {
    let hash = ("6471a723296395cf1dcc568941affd7a390f94ce").to_ascii_lowercase();

    let balance: Wei = from_eth(1000);

    let mock_account = Account::default_account(Vec::from(hash), balance);

    store_account(host, mock_account)
}

pub fn main<Host: Runtime + RawRollupCore>(host: &mut Host) {
    match init_mock_account(host) {
        Ok(()) => (),
        Err(_) => panic!("The account should be writable"),
    }
}

kernel_entry!(main);
