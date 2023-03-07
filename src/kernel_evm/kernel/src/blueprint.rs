// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::inbox::{read_input, Transaction};
use host::rollup_core::RawRollupCore;
use host::runtime::Runtime;

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

fn fetch_transactions<Host: Runtime + RawRollupCore>(host: &mut Host, blueprint: &mut Blueprint) {
    if let Ok(transaction) = read_input(host, 4096) {
        push(blueprint, transaction);
        fetch_transactions(host, blueprint)
    }
}

pub fn fetch<Host: Runtime + RawRollupCore>(host: &mut Host) -> Queue {
    let mut blueprint = Blueprint {
        transactions: Vec::new(),
    };
    fetch_transactions(host, &mut blueprint);
    Queue {
        proposals: vec![blueprint],
    }
}
