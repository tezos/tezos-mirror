// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_ethereum::signatures::EthereumTransactionCommon;
use tezos_smart_rollup_host::input::Message;

use tezos_ethereum::transaction::TransactionHash;

use crate::inbox::Transaction;

#[derive(Debug, PartialEq)]
pub enum Input {
    SimpleTransaction(Box<Transaction>),
    NewChunkedTransaction {
        tx_hash: TransactionHash,
        num_chunks: u16,
    },
    TransactionChunk {
        tx_hash: TransactionHash,
        i: u16,
        data: Vec<u8>,
    },
}

#[derive(Debug, PartialEq)]
pub enum InputResult {
    NoInput,
    Input(Input),
    Unparsable,
}

const SIMPLE_TRANSACTION_TAG: u8 = 0;

const NEW_CHUNKED_TRANSACTION_TAG: u8 = 1;

const TRANSACTION_CHUNK_TAG: u8 = 2;

pub const MAX_SIZE_PER_CHUNK: usize = 4095 // Max input size minus external tag
            - 20 // Smart rollup address size
            - 1  // Transaction chunk tag
            - 2  // Number of chunks (u16)
            - 32; // Transaction hash size

macro_rules! parsable {
    ($expr : expr) => {
        match $expr {
            Some(x) => x,
            None => return InputResult::Unparsable,
        }
    };
}

impl InputResult {
    fn split_at(bytes: &[u8], len: usize) -> Option<(&[u8], &[u8])> {
        let left = bytes.get(0..len)?;
        let right = bytes.get(len..)?;
        Some((left, right))
    }

    fn parse_simple_transaction(bytes: &[u8]) -> Self {
        // Next 32 bytes is the transaction hash.
        let (tx_hash, remaining) = parsable!(Self::split_at(bytes, 32));
        let tx_hash: TransactionHash = parsable!(tx_hash.try_into().ok()); // Remaining bytes is the rlp encoded transaction.
        let tx: EthereumTransactionCommon = parsable!(remaining.try_into().ok());
        InputResult::Input(Input::SimpleTransaction(Box::new(Transaction {
            tx_hash,
            tx,
        })))
    }

    fn parse_new_chunked_transaction(bytes: &[u8]) -> Self {
        // Next 32 bytes is the transaction hash.
        let (tx_hash, remaining) = parsable!(Self::split_at(bytes, 32));
        let tx_hash: TransactionHash = parsable!(tx_hash.try_into().ok());
        // Next 2 bytes is the number of chunks.
        let (num_chunks, remaining) = parsable!(Self::split_at(remaining, 2));
        let num_chunks = u16::from_le_bytes(num_chunks.try_into().unwrap());
        if remaining.is_empty() {
            Self::Input(Input::NewChunkedTransaction {
                tx_hash,
                num_chunks,
            })
        } else {
            Self::Unparsable
        }
    }

    fn parse_transaction_chunk(bytes: &[u8]) -> Self {
        // Next 32 bytes is the transaction hash.
        let (tx_hash, remaining) = parsable!(Self::split_at(bytes, 32));
        let tx_hash: TransactionHash = parsable!(tx_hash.try_into().ok());
        // Next 2 bytes is the index.
        let (i, remaining) = parsable!(Self::split_at(remaining, 2));
        let i = u16::from_le_bytes(i.try_into().unwrap());
        Self::Input(Input::TransactionChunk {
            tx_hash,
            i,
            data: remaining.to_vec(),
        })
    }

    pub fn parse(input: Message, smart_rollup_address: [u8; 20]) -> Self {
        let bytes = Message::as_ref(&input);
        let (input_tag, remaining) = parsable!(bytes.split_first());
        // External messages starts with the tag 1, they are the only
        // messages we consider.
        if *input_tag != 1 {
            return InputResult::Unparsable;
        };
        // Next 20 bytes is the targeted smart rollup address.
        let remaining = {
            let (target_smart_rollup_address, remaining) =
                parsable!(Self::split_at(remaining, 20));

            if target_smart_rollup_address == smart_rollup_address {
                remaining
            } else {
                return InputResult::Unparsable;
            }
        };
        let (transaction_tag, remaining) = parsable!(remaining.split_first());
        match *transaction_tag {
            SIMPLE_TRANSACTION_TAG => Self::parse_simple_transaction(remaining),
            NEW_CHUNKED_TRANSACTION_TAG => Self::parse_new_chunked_transaction(remaining),
            TRANSACTION_CHUNK_TAG => Self::parse_transaction_chunk(remaining),
            _ => InputResult::Unparsable,
        }
    }
}
