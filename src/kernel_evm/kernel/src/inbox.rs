// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT
use tezos_ethereum::signatures::EthereumTransactionCommon;
use tezos_smart_rollup_host::runtime::Runtime;

use crate::parsing::{Input, InputResult, MAX_SIZE_PER_CHUNK};
use crate::Error;

use tezos_ethereum::transaction::TransactionHash;

#[derive(Debug, PartialEq, Clone)]
pub struct Transaction {
    pub tx_hash: TransactionHash,
    pub tx: EthereumTransactionCommon,
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

fn handle_transaction_chunk<Host: Runtime>(
    host: &mut Host,
    tx_hash: TransactionHash,
    i: u16,
    data: Vec<u8>,
) -> Result<Option<Transaction>, Error> {
    // Sanity check to verify that the transaction chunk uses the maximum
    // space capacity allowed.
    let num_chunks = crate::storage::chunked_transaction_num_chunks(host, &tx_hash)?;
    if i != num_chunks - 1 && data.len() < MAX_SIZE_PER_CHUNK {
        crate::storage::remove_chunked_transaction(host, &tx_hash)?;
        return Ok(None);
    };
    // When the transaction is stored in the storage, it returns the full transaction
    // if `data` was the missing chunk.
    if let Some(data) = crate::storage::store_transaction_chunk(host, &tx_hash, i, data)?
    {
        if let Ok(tx) = EthereumTransactionCommon::from_rlp_bytes(&data) {
            return Ok(Some(Transaction { tx_hash, tx }));
        }
    }
    Ok(None)
}

pub fn read_inbox<Host: Runtime>(
    host: &mut Host,
    smart_rollup_address: [u8; 20],
) -> Result<Vec<Transaction>, Error> {
    let mut res = Vec::new();
    loop {
        match read_input(host, smart_rollup_address)? {
            InputResult::NoInput => {
                return Ok(res);
            }
            InputResult::Unparsable => (),

            InputResult::Input(Input::SimpleTransaction(tx)) => res.push(*tx),
            InputResult::Input(Input::NewChunkedTransaction {
                tx_hash,
                num_chunks,
            }) => crate::storage::create_chunked_transaction(host, &tx_hash, num_chunks)?,
            InputResult::Input(Input::TransactionChunk { tx_hash, i, data }) => {
                if let Some(tx) = handle_transaction_chunk(host, tx_hash, i, data)? {
                    res.push(tx)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_data_encoding::types::Bytes;
    use tezos_ethereum::transaction::TRANSACTION_HASH_SIZE;
    use tezos_smart_rollup_host::input::Message;
    use tezos_smart_rollup_mock::MockHost;

    const ZERO_SMART_ROLLUP_ADDRESS: [u8; 20] = [0; 20];

    const ZERO_TX_HASH: TransactionHash = [0; TRANSACTION_HASH_SIZE];

    fn input_to_bytes(smart_rollup_address: [u8; 20], input: Input) -> Vec<u8> {
        let mut buffer = Vec::new();
        // Smart rollup address.
        buffer.extend_from_slice(&smart_rollup_address);
        match input {
            Input::SimpleTransaction(tx) => {
                // Simple transaction tag
                buffer.push(0);
                buffer.extend_from_slice(&tx.tx_hash);
                let mut tx_bytes = tx.tx.into();
                buffer.append(&mut tx_bytes)
            }
            Input::NewChunkedTransaction {
                tx_hash,
                num_chunks,
            } => {
                // New chunked transaction tag
                buffer.push(1);
                buffer.extend_from_slice(&tx_hash);
                buffer.extend_from_slice(&u16::to_le_bytes(num_chunks))
            }
            Input::TransactionChunk { tx_hash, i, data } => {
                // Transaction chunk tag
                buffer.push(2);
                buffer.extend_from_slice(&tx_hash);
                buffer.extend_from_slice(&u16::to_le_bytes(i));
                buffer.extend_from_slice(&data);
            }
        };
        buffer
    }

    fn make_chunked_transactions(tx_hash: TransactionHash, data: Vec<u8>) -> Vec<Input> {
        let mut chunks: Vec<Input> = data
            .chunks(MAX_SIZE_PER_CHUNK)
            .enumerate()
            .map(|(i, bytes)| Input::TransactionChunk {
                tx_hash,
                i: i as u16,
                data: bytes.to_vec(),
            })
            .collect();
        let number_of_chunks = chunks.len() as u16;

        let new_chunked_transaction = Input::NewChunkedTransaction {
            tx_hash,
            num_chunks: number_of_chunks,
        };

        let mut buffer = Vec::new();
        buffer.push(new_chunked_transaction);
        buffer.append(&mut chunks);
        buffer
    }

    #[test]
    fn parse_valid_simple_transaction() {
        let mut host = MockHost::default();

        let tx =
            EthereumTransactionCommon::from_rlp_bytes(&hex::decode("f86d80843b9aca00825208940b52d4d3be5d18a7ab5e4476a2f5382bbf2b38d888016345785d8a000080820a95a0d9ef1298c18c88604e3f08e14907a17dfa81b1dc6b37948abe189d8db5cb8a43a06fc7040a71d71d3cb74bd05ead7046b10668ad255da60391c017eea31555f156").unwrap()).unwrap();
        let input = Input::SimpleTransaction(Box::new(Transaction {
            tx_hash: ZERO_TX_HASH,
            tx: tx.clone(),
        }));

        host.add_external(Bytes::from(input_to_bytes(
            ZERO_SMART_ROLLUP_ADDRESS,
            input,
        )));

        let transactions = read_inbox(&mut host, ZERO_SMART_ROLLUP_ADDRESS).unwrap();
        let expected_transactions = vec![Transaction {
            tx_hash: ZERO_TX_HASH,
            tx,
        }];
        assert_eq!(transactions, expected_transactions);
    }

    #[test]
    fn parse_valid_chunked_transaction() {
        let mut host = MockHost::default();

        let data: Vec<u8> = hex::decode(["f917e180843b9aca0082520894b53dc01974176e5dff2298c5a94343c2585e3c548a021dfe1f5c5363780000b91770".to_string(), "a".repeat(12_000), "820a96a07fd9567a72223bbc8f70bd2b42011339b61044d16b5a2233534db8ca01f3e57aa03ea489c4bb2b2b52f3c1a18966881114767654c9ab61d46b1fbff78a498043c2".to_string()].join("")).unwrap();
        let tx = EthereumTransactionCommon::from_rlp_bytes(&data).unwrap();

        let inputs = make_chunked_transactions(ZERO_TX_HASH, data);

        for input in inputs {
            host.add_external(Bytes::from(input_to_bytes(
                ZERO_SMART_ROLLUP_ADDRESS,
                input,
            )))
        }

        let transactions = read_inbox(&mut host, ZERO_SMART_ROLLUP_ADDRESS).unwrap();
        let expected_transactions = vec![Transaction {
            tx_hash: ZERO_TX_HASH,
            tx,
        }];
        assert_eq!(transactions, expected_transactions);
    }

    #[test]
    fn parse_unparsable_transaction() {
        let message = Message::new(0, 0, vec![1, 9, 32, 58, 59, 30]);
        assert_eq!(
            InputResult::parse(message, ZERO_SMART_ROLLUP_ADDRESS),
            InputResult::Unparsable
        )
    }
}
