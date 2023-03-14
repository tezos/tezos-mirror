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
use crate::storage::{read_smart_rollup_address, store_account, store_smart_rollup_address};
use tezos_ethereum::account::Account;
use tezos_ethereum::wei::{from_eth, Wei};

mod block;
mod blueprint;
mod error;
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

pub fn init_mock_account<Host: Runtime + RawRollupCore>(host: &mut Host) -> Result<(), Error> {
    let hash = (b"6ce4d79d4E77402e1ef3417Fdda433aA744C6e1c").to_ascii_lowercase();
    let path = storage::account_path(&hash)?;

    if !storage::has_account(host, &path)? {
        let balance: Wei = from_eth(9999);

        let mock_account = Account::default_account(balance);

        store_account(host, mock_account, &hash)?
    };
    Ok(())
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

pub fn main<Host: Runtime + RawRollupCore>(host: &mut Host) {
    let smart_rollup_address = retrieve_smart_rollup_address(host);

    match init_mock_account(host) {
        Ok(()) => (),
        Err(_) => debug_msg!(host; "Failed to write the mocked up account"),
    }

    let queue = stage_one(host, smart_rollup_address);

    stage_two(host, queue)
}

kernel_entry!(main);

#[cfg(test)]
mod tests {
    use super::*;

    use mock_runtime::host::MockHost;

    #[test]
    // Test the mock account can be written in the durable storage.
    fn test_init_mock_account() {
        let mut host = MockHost::default();

        match init_mock_account(&mut host) {
            Ok(()) => (),
            Err(_) => panic!("The account should be writable"),
        }
    }
}
