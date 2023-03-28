// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::inbox::{read_input, Transaction};
use tezos_smart_rollup_host::runtime::Runtime;

/// The blueprint of a block is a list of transactions.
pub struct Blueprint {
    pub transactions: Vec<Transaction>,
}

#[derive(Default)]
pub struct Queue {
    // In our case, to make it simple and straightforward it will be
    // an array of pendings transactions even though it'll be only a
    // singleton for our needs.
    pub proposals: Vec<Blueprint>,
}

impl Queue {
    pub fn new() -> Queue {
        Queue {
            proposals: Vec::new(),
        }
    }

    pub fn add(queue: &mut Queue, transactions: Vec<Transaction>) {
        queue.proposals.push(Blueprint { transactions })
    }
}

// Conditionally push a transaction in the blueprint.
fn push(blueprint: &mut Blueprint, transaction: Option<Transaction>) {
    if let Some(transaction) = transaction {
        blueprint.transactions.push(transaction);
    }
}

fn fetch_transactions<Host: Runtime>(
    host: &mut Host,
    blueprint: &mut Blueprint,
    smart_rollup_address: [u8; 20],
) {
    if let Ok(transaction) = read_input(host, smart_rollup_address) {
        push(blueprint, transaction);
        fetch_transactions(host, blueprint, smart_rollup_address)
    }
}

pub fn fetch<Host: Runtime>(host: &mut Host, smart_rollup_address: [u8; 20]) -> Queue {
    let mut blueprint = Blueprint {
        transactions: Vec::new(),
    };
    fetch_transactions(host, &mut blueprint, smart_rollup_address);
    Queue {
        proposals: vec![blueprint],
    }
}
