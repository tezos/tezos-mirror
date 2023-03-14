// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

#![allow(dead_code)]

use host::input::Message;
use host::rollup_core::RawRollupCore;
use host::runtime::Runtime;

use crate::helpers::ensures;

use tezos_ethereum::transaction::{RawTransaction, TransactionHash};

pub struct Transaction {
    pub level: i32,
    pub tx_hash: TransactionHash,
    pub tx: RawTransaction,
}

pub enum Error {
    ReadInputError,
}

impl Transaction {
    pub fn parse(input: Message, smart_rollup_address: [u8; 20]) -> Option<Self> {
        let bytes = Message::as_ref(&input);
        let (input_tag, remaining) = bytes.split_first()?;
        // External messages starts with the tag 1, they are the only
        // messages we consider.
        ensures(*input_tag == 1)?;
        // Next 20 bytes is the targeted smart rollup address.
        let (target_smart_rollup_address, remaining) = remaining.split_at(20);
        ensures(target_smart_rollup_address == smart_rollup_address)?;
        // Next 32 bytes is the transaction hash.
        let (tx_hash, remaining) = remaining.split_at(32);
        let tx_hash: TransactionHash = tx_hash.try_into().ok()?;
        // Remaining bytes is the rlp encoded transaction.
        let tx = RawTransaction::decode_from_rlp(remaining).ok()?;

        Some(Transaction {
            level: input.level,
            tx_hash,
            tx,
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
