// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use evm_execution::signatures::EthereumTransactionCommon;
use tezos_smart_rollup_host::input::Message;
use tezos_smart_rollup_host::runtime::Runtime;

use crate::Error;

use tezos_ethereum::transaction::TransactionHash;

pub struct Transaction {
    pub tx_hash: TransactionHash,
    pub tx: EthereumTransactionCommon,
}

pub enum InputResult {
    NoInput,
    SimpleTransaction(Box<Transaction>),
    Unparsable,
}

const SIMPLE_TRANSACTION_TAG: u8 = 0;

impl InputResult {
    fn parse_simple_transaction(bytes: &[u8]) -> Self {
        // Next 32 bytes is the transaction hash.
        let (tx_hash, remaining) = bytes.split_at(32);
        let tx_hash: TransactionHash = match tx_hash.try_into() {
            Ok(tx_hash) => tx_hash,
            Err(_) => return InputResult::Unparsable,
        };
        // Remaining bytes is the rlp encoded transaction.
        let tx: EthereumTransactionCommon = match remaining.try_into() {
            Ok(tx) => tx,
            Err(_) => return InputResult::Unparsable,
        };
        InputResult::SimpleTransaction(Box::new(Transaction { tx_hash, tx }))
    }

    pub fn parse(input: Message, smart_rollup_address: [u8; 20]) -> Self {
        let bytes = Message::as_ref(&input);
        let (input_tag, remaining) = match bytes.split_first() {
            Some(res) => res,
            None => return InputResult::Unparsable,
        };
        // External messages starts with the tag 1, they are the only
        // messages we consider.
        if *input_tag != 1 {
            return InputResult::Unparsable;
        };
        // Next 20 bytes is the targeted smart rollup address.
        let remaining = {
            let (target_smart_rollup_address, remaining) = remaining.split_at(20);

            if target_smart_rollup_address == smart_rollup_address {
                remaining
            } else {
                return InputResult::Unparsable;
            }
        };
        let (transaction_tag, remaining) = match remaining.split_first() {
            Some(res) => res,
            None => return InputResult::Unparsable,
        };
        match *transaction_tag {
            SIMPLE_TRANSACTION_TAG => Self::parse_simple_transaction(remaining),
            _ => InputResult::Unparsable,
        }
    }
}

pub fn read_input<Host: Runtime>(
    host: &mut Host,
    smart_rollup_address: [u8; 20],
) -> Result<InputResult, Error> {
    let input = host.read_input()?;
    match input {
        Some(input) => Ok(InputResult::parse(input, smart_rollup_address)),
        None => Ok(InputResult::NoInput),
    }
}

pub fn read_inbox<Host: Runtime>(
    host: &mut Host,
    smart_rollup_address: [u8; 20],
) -> Result<Vec<Transaction>, Error> {
    let mut res = Vec::new();
    loop {
        match read_input(host, smart_rollup_address)? {
            InputResult::NoInput => return Ok(res),
            InputResult::Unparsable => (),
            InputResult::SimpleTransaction(tx) => res.push(*tx),
        }
    }
}
