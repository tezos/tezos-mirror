// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

#![allow(dead_code)]

use host::input::Message;
use host::rollup_core::RawRollupCore;
use host::runtime::Runtime;

pub struct Transaction {
    pub level: i32,
    pub tx: Vec<u8>,
}

pub struct EthereumTransaction {
    pub transactions: Vec<Transaction>,
}

pub enum Messages {
    // This enum structure is intended to be extended with Internal at
    // some point.
    External(EthereumTransaction),
}

pub enum Error {
    ReadInputError,
}

impl Messages {
    pub fn new(transaction: Transaction) -> Messages {
        // messages is actually a tx singleton for now
        let eth_tx = EthereumTransaction {
            transactions: vec![transaction],
        };
        Messages::External(eth_tx)
    }

    pub fn of_raw_input(input: Message) -> Result<Messages, Error> {
        let tx = Message::as_ref(&input).to_vec();
        let transaction = Transaction {
            level: input.level,
            tx,
        };
        Ok(Messages::new(transaction))
    }
}

pub struct Inbox {
    pub messages: Messages,
}

impl Inbox {
    pub fn read_input<Host: Runtime + RawRollupCore>(
        host: &mut Host,
        max_bytes: usize,
    ) -> Result<Messages, Error> {
        match Runtime::read_input(host, max_bytes) {
            Ok(Some(input)) => Messages::of_raw_input(input),
            _ => Err(Error::ReadInputError),
        }
    }
}
