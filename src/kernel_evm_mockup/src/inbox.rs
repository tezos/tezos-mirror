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

pub enum Error {
    ReadInputError,
}

impl Transaction {
    pub fn of_raw_input(input: Message) -> Self {
        let tx = Message::as_ref(&input).to_vec();
        Transaction {
            level: input.level,
            tx,
        }
    }
}

pub fn read_input<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    max_bytes: usize,
) -> Result<Transaction, Error> {
    match Runtime::read_input(host, max_bytes) {
        Ok(Some(input)) => Ok(Transaction::of_raw_input(input)),
        _ => Err(Error::ReadInputError),
    }
}
