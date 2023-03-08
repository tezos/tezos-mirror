// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

#![allow(dead_code)]

use host::input::Message;
use host::rollup_core::RawRollupCore;
use host::runtime::Runtime;

use tezos_ethereum::eth_gen::RawTransaction;

pub struct Transaction {
    pub level: i32,
    pub tx: RawTransaction,
}

pub enum Error {
    ReadInputError,
}

impl Transaction {
    fn ensures(cond: bool) -> Option<()> {
        if cond {
            Some(())
        } else {
            None
        }
    }

    pub fn parse(input: Message, smart_rollup_address: [u8; 20]) -> Option<Self> {
        let bytes = Message::as_ref(&input);
        let (input_tag, remaining) = bytes.split_first()?;
        // External messages starts with the tag 1, they are the only
        // messages we consider.
        Self::ensures(*input_tag == 1)?;
        // Next 20 bytes is the targeted smart rollup address.
        let (target_smart_rollup_address, remaining) = remaining.split_at(20);
        Self::ensures(target_smart_rollup_address == smart_rollup_address)?;

        Some(Transaction {
            level: input.level,
            tx: remaining.to_vec(),
        })
    }

    pub fn to_raw_transaction(&self) -> RawTransaction {
        self.tx.clone()
    }
}

pub fn read_input<Host: Runtime + RawRollupCore>(
    host: &mut Host,
    max_bytes: usize,
    smart_rollup_address: [u8; 20],
) -> Result<Option<Transaction>, Error> {
    match Runtime::read_input(host, max_bytes) {
        Ok(Some(input)) => Ok(Transaction::parse(input, smart_rollup_address)),
        _ => Err(Error::ReadInputError),
    }
}
