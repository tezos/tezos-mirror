// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::error::UpgradeProcessError::{InvalidUpgradeNonce, NoDictator};
use crate::parsing::{
    Input, InputResult, MAX_SIZE_PER_CHUNK, SIGNATURE_HASH_SIZE, UPGRADE_NONCE_SIZE,
};
use crate::simulation;
use crate::storage::{
    get_and_increment_deposit_nonce, read_dictator_key, read_kernel_upgrade_nonce,
    store_last_info_per_level_timestamp,
};
use crate::tick_model;
use crate::upgrade::check_dictator_signature;
use crate::Error;
use primitive_types::{H160, U256};
use rlp::{Decodable, DecoderError, Encodable};
use sha3::{Digest, Keccak256};
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_ethereum::rlp_helpers::{decode_field, decode_tx_hash, next};
use tezos_ethereum::transaction::TransactionHash;
use tezos_ethereum::tx_common::EthereumTransactionCommon;
use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_host::runtime::Runtime;

#[derive(Debug, PartialEq, Clone)]
pub struct Deposit {
    pub amount: U256,
    pub gas_price: U256,
    pub receiver: H160,
}

impl Encodable for Deposit {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(3);
        stream.append(&self.amount);
        stream.append(&self.gas_price);
        stream.append(&self.receiver);
    }
}

impl Decodable for Deposit {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 3 {
            return Err(DecoderError::RlpIncorrectListLen);
        }

        let mut it = decoder.iter();
        let amount: U256 = decode_field(&next(&mut it)?, "amount")?;
        let gas_price: U256 = decode_field(&next(&mut it)?, "gas_price")?;
        let receiver: H160 = decode_field(&next(&mut it)?, "receiver")?;
        Ok(Deposit {
            amount,
            gas_price,
            receiver,
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TransactionContent {
    Ethereum(EthereumTransactionCommon),
    Deposit(Deposit),
}

const ETHEREUM_TX_TAG: u8 = 1;
const DEPOSIT_TX_TAG: u8 = 2;

impl Encodable for TransactionContent {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(2);
        match &self {
            TransactionContent::Ethereum(eth) => {
                stream.append(&ETHEREUM_TX_TAG);
                eth.rlp_append(stream)
            }
            TransactionContent::Deposit(dep) => {
                stream.append(&DEPOSIT_TX_TAG);
                dep.rlp_append(stream)
            }
        }
    }
}

impl Decodable for TransactionContent {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 2 {
            return Err(DecoderError::RlpIncorrectListLen);
        }
        let tag: u8 = decoder.at(0)?.as_val()?;
        let tx = decoder.at(1)?;
        match tag {
            DEPOSIT_TX_TAG => {
                let deposit = Deposit::decode(&tx)?;
                Ok(Self::Deposit(deposit))
            }
            ETHEREUM_TX_TAG => {
                let eth = EthereumTransactionCommon::decode(&tx)?;
                Ok(Self::Ethereum(eth))
            }
            _ => Err(DecoderError::Custom("Unknown transaction tag.")),
        }
    }
}
#[derive(Debug, PartialEq, Clone)]
pub struct Transaction {
    pub tx_hash: TransactionHash,
    pub content: TransactionContent,
}

impl Transaction {
    /// give an approximation of the number of ticks necessary to process the
    /// transaction. Overapproximated using the [gas_limit] and benchmarks
    pub fn estimate_ticks(&self) -> u64 {
        // all details of tick model stay in the same module
        tick_model::estimate_ticks_for_transaction(self)
    }
}

impl Encodable for Transaction {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(2);
        stream.append_iter(self.tx_hash);
        stream.append(&self.content);
    }
}

impl Decodable for Transaction {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, rlp::DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 2 {
            return Err(DecoderError::RlpIncorrectListLen);
        }
        let mut it = decoder.iter();
        let tx_hash: TransactionHash = decode_tx_hash(next(&mut it)?)?;
        let content: TransactionContent =
            decode_field(&next(&mut it)?, "Transaction content")?;
        Ok(Transaction { tx_hash, content })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct KernelUpgrade {
    pub nonce: [u8; UPGRADE_NONCE_SIZE],
    pub preimage_hash: [u8; PREIMAGE_HASH_SIZE],
    pub signature: [u8; SIGNATURE_HASH_SIZE],
}

#[derive(Debug, PartialEq)]
pub struct InboxContent {
    pub kernel_upgrade: Option<KernelUpgrade>,
    pub transactions: Vec<Transaction>,
}

pub fn read_input<Host: Runtime>(
    host: &mut Host,
    smart_rollup_address: [u8; 20],
    ticketer: &Option<ContractKt1Hash>,
) -> Result<InputResult, Error> {
    let input = host.read_input()?;

    match input {
        Some(input) => Ok(InputResult::parse(
            host,
            input,
            smart_rollup_address,
            ticketer,
        )),
        None => Ok(InputResult::NoInput),
    }
}

fn handle_transaction_chunk<Host: Runtime>(
    host: &mut Host,
    tx_hash: TransactionHash,
    i: u16,
    data: Vec<u8>,
) -> Result<Option<Transaction>, Error> {
    // If the number of chunks doesn't exist in the storage, the chunked
    // transaction wasn't created, so the chunk is ignored.
    let num_chunks = match crate::storage::chunked_transaction_num_chunks(host, &tx_hash)
    {
        Ok(x) => x,
        Err(_) => {
            log!(
                host,
                Info,
                "Ignoring chunk {} of unknown transaction {}\n",
                i,
                hex::encode(tx_hash)
            );
            return Ok(None);
        }
    };
    // Checks that the transaction is not out of bounds
    if i >= num_chunks {
        return Ok(None);
    }
    // Sanity check to verify that the transaction chunk uses the maximum
    // space capacity allowed.
    if i != num_chunks - 1 && data.len() < MAX_SIZE_PER_CHUNK {
        crate::storage::remove_chunked_transaction(host, &tx_hash)?;
        return Ok(None);
    };
    // When the transaction is stored in the storage, it returns the full transaction
    // if `data` was the missing chunk.
    if let Some(data) = crate::storage::store_transaction_chunk(host, &tx_hash, i, data)?
    {
        if let Ok(tx) = EthereumTransactionCommon::from_bytes(&data) {
            let content = TransactionContent::Ethereum(tx);
            return Ok(Some(Transaction { tx_hash, content }));
        }
    }
    Ok(None)
}

fn handle_kernel_upgrade<Host: Runtime>(
    host: &mut Host,
    smart_rollup_address: [u8; 20],
    kernel_upgrade: &KernelUpgrade,
) -> Result<(), Error> {
    match read_dictator_key(host) {
        Some(dictator) => {
            let current_kernel_upgrade_nonce = read_kernel_upgrade_nonce(host)?;
            let incoming_nonce = u16::from_le_bytes(kernel_upgrade.nonce);
            if incoming_nonce == current_kernel_upgrade_nonce + 1 {
                check_dictator_signature(
                    kernel_upgrade.signature,
                    smart_rollup_address,
                    kernel_upgrade.nonce,
                    kernel_upgrade.preimage_hash,
                    dictator,
                )
            } else {
                Err(Error::UpgradeError(InvalidUpgradeNonce))
            }
        }
        None => Err(Error::UpgradeError(NoDictator)),
    }
}

fn handle_deposit<Host: Runtime>(
    host: &mut Host,
    deposit: Deposit,
) -> Result<Transaction, Error> {
    let deposit_nonce = get_and_increment_deposit_nonce(host)?;

    let mut buffer_amount = [0; 32];
    deposit.amount.to_little_endian(&mut buffer_amount);
    let mut buffer_gas_price = [0; 32];
    deposit.gas_price.to_little_endian(&mut buffer_gas_price);

    let mut to_hash = vec![];
    to_hash.extend_from_slice(&buffer_amount);
    to_hash.extend_from_slice(&buffer_gas_price);
    to_hash.extend_from_slice(&deposit.receiver.to_fixed_bytes());
    to_hash.extend_from_slice(&deposit_nonce.to_le_bytes());

    let kec = Keccak256::digest(to_hash);
    let tx_hash = kec
        .as_slice()
        .try_into()
        .map_err(|_| Error::InvalidConversion)?;

    Ok(Transaction {
        tx_hash,
        content: TransactionContent::Deposit(deposit),
    })
}

pub fn read_inbox<Host: Runtime>(
    host: &mut Host,
    smart_rollup_address: [u8; 20],
    ticketer: Option<ContractKt1Hash>,
) -> Result<InboxContent, Error> {
    let mut res = InboxContent {
        kernel_upgrade: None,
        transactions: vec![],
    };
    loop {
        match read_input(host, smart_rollup_address, &ticketer)? {
            InputResult::NoInput => {
                return Ok(res);
            }
            InputResult::Unparsable => (),
            InputResult::Input(Input::SimpleTransaction(tx)) => {
                res.transactions.push(*tx)
            }
            InputResult::Input(Input::NewChunkedTransaction {
                tx_hash,
                num_chunks,
            }) => crate::storage::create_chunked_transaction(host, &tx_hash, num_chunks)?,
            InputResult::Input(Input::TransactionChunk { tx_hash, i, data }) => {
                if let Some(tx) = handle_transaction_chunk(host, tx_hash, i, data)? {
                    res.transactions.push(tx)
                }
            }
            InputResult::Input(Input::Upgrade(kernel_upgrade)) => {
                match handle_kernel_upgrade(host, smart_rollup_address, &kernel_upgrade) {
                    Ok(()) => res.kernel_upgrade = Some(kernel_upgrade),
                    Err(e) => log!(
                        host,
                        Error,
                        "Error while processing the kernel upgrade: {:?}",
                        e
                    ),
                }
            }
            InputResult::Input(Input::Simulation) => {
                // kernel enters in simulation mode, reading will be done by the
                // simulation and all the previous and next transactions are
                // discarded.
                simulation::start_simulation_mode(host)?;
                return Ok(InboxContent {
                    kernel_upgrade: None,
                    transactions: vec![],
                });
            }
            InputResult::Input(Input::Info(info)) => {
                store_last_info_per_level_timestamp(host, info.predecessor_timestamp)?;
            }
            InputResult::Input(Input::Deposit(deposit)) => {
                res.transactions.push(handle_deposit(host, deposit)?)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::inbox::TransactionContent::Ethereum;
    use crate::storage::*;
    use libsecp256k1::PublicKey;
    use tezos_crypto_rs::hash::SmartRollupHash;
    use tezos_data_encoding::types::Bytes;
    use tezos_ethereum::transaction::TRANSACTION_HASH_SIZE;
    use tezos_smart_rollup_encoding::inbox::ExternalMessageFrame;
    use tezos_smart_rollup_encoding::smart_rollup::SmartRollupAddress;
    use tezos_smart_rollup_mock::MockHost;

    const SMART_ROLLUP_ADDRESS: [u8; 20] = [
        20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1,
    ];

    const ZERO_TX_HASH: TransactionHash = [0; TRANSACTION_HASH_SIZE];

    fn smart_rollup_address() -> SmartRollupAddress {
        SmartRollupAddress::new(SmartRollupHash(SMART_ROLLUP_ADDRESS.into()))
    }

    fn input_to_bytes(smart_rollup_address: [u8; 20], input: Input) -> Vec<u8> {
        let mut buffer = Vec::new();
        // Targetted framing protocol
        buffer.push(0);
        buffer.extend_from_slice(&smart_rollup_address);
        match input {
            Input::SimpleTransaction(tx) => {
                // Simple transaction tag
                buffer.push(0);
                buffer.extend_from_slice(&tx.tx_hash);
                let mut tx_bytes = match tx.content {
                    Ethereum(tx) => tx.into(),
                    _ => panic!(
                        "Simple transaction can contain only ethereum transactions"
                    ),
                };

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
            Input::Upgrade(KernelUpgrade {
                nonce,
                preimage_hash,
                signature,
            }) => {
                // Kernel upgrade tag
                buffer.push(3);
                buffer.extend_from_slice(&nonce);
                buffer.extend_from_slice(&preimage_hash);
                buffer.extend_from_slice(&signature)
            }
            _ => (),
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

    fn large_transaction() -> (Vec<u8>, EthereumTransactionCommon) {
        let data: Vec<u8> = hex::decode(["f917e180843b9aca0082520894b53dc01974176e5dff2298c5a94343c2585e3c548a021dfe1f5c5363780000b91770".to_string(), "a".repeat(12_000), "820a96a07fd9567a72223bbc8f70bd2b42011339b61044d16b5a2233534db8ca01f3e57aa03ea489c4bb2b2b52f3c1a18966881114767654c9ab61d46b1fbff78a498043c2".to_string()].join("")).unwrap();
        let tx = EthereumTransactionCommon::from_bytes(&data).unwrap();
        (data, tx)
    }

    #[test]
    fn parse_valid_simple_transaction() {
        let mut host = MockHost::default();

        let tx =
            EthereumTransactionCommon::from_bytes(&hex::decode("f86d80843b9aca00825208940b52d4d3be5d18a7ab5e4476a2f5382bbf2b38d888016345785d8a000080820a95a0d9ef1298c18c88604e3f08e14907a17dfa81b1dc6b37948abe189d8db5cb8a43a06fc7040a71d71d3cb74bd05ead7046b10668ad255da60391c017eea31555f156").unwrap()).unwrap();
        let input = Input::SimpleTransaction(Box::new(Transaction {
            tx_hash: ZERO_TX_HASH,
            content: Ethereum(tx.clone()),
        }));

        host.add_external(Bytes::from(input_to_bytes(SMART_ROLLUP_ADDRESS, input)));

        let inbox_content = read_inbox(&mut host, SMART_ROLLUP_ADDRESS, None).unwrap();
        let expected_transactions = vec![Transaction {
            tx_hash: ZERO_TX_HASH,
            content: Ethereum(tx),
        }];
        assert_eq!(inbox_content.transactions, expected_transactions);
    }

    #[test]
    fn parse_valid_chunked_transaction() {
        let address = smart_rollup_address();
        let mut host = MockHost::with_address(&address);

        let (data, tx) = large_transaction();

        let inputs = make_chunked_transactions(ZERO_TX_HASH, data);

        for input in inputs {
            host.add_external(Bytes::from(input_to_bytes(SMART_ROLLUP_ADDRESS, input)))
        }

        let inbox_content = read_inbox(&mut host, SMART_ROLLUP_ADDRESS, None).unwrap();
        let expected_transactions = vec![Transaction {
            tx_hash: ZERO_TX_HASH,
            content: Ethereum(tx),
        }];
        assert_eq!(inbox_content.transactions, expected_transactions);
    }

    #[test]
    fn parse_valid_kernel_upgrade() {
        let mut host = MockHost::default();
        store_kernel_upgrade_nonce(&mut host, 1).unwrap();

        let dictator_bytes = hex::decode(
            "046edc43401193c9321730cdf73e454f68e8aa52e377d001499b0eaa431fa4763102e685fe33851f5f51bd31adb41582bbfb0ad85c1089c0a0b4adc049a271bc01",
        )
        .unwrap();
        let dictator = PublicKey::parse_slice(&dictator_bytes, None).unwrap();
        crate::storage::internal_for_tests::store_dictator_key(&mut host, dictator)
            .unwrap();

        let preimage_hash = hex::decode(
            "004b28109df802cb1885ab29461bc1b410057a9f3a848d122ac7a742351a3a1f4e",
        )
        .unwrap()
        .try_into()
        .unwrap();
        let nonce = 2u16.to_le_bytes();
        let signature = hex::decode("518510c99979b8c9854302d05167beac2a96aeded627e240f30583e1d445f4fc7ba2e83a09c377a0eb46018f3dda049f0b48bd3d10202d997e6e13f9e21d31a6").unwrap().try_into().unwrap();

        let kernel_upgrade = KernelUpgrade {
            nonce,
            preimage_hash,
            signature,
        };
        let input = Input::Upgrade(kernel_upgrade.clone());

        let zero_smart_rollup_address = [0; 20];
        host.add_external(Bytes::from(input_to_bytes(
            zero_smart_rollup_address,
            input,
        )));

        let inbox_content =
            read_inbox(&mut host, zero_smart_rollup_address, None).unwrap();
        let expected_upgrade = Some(kernel_upgrade);
        assert_eq!(inbox_content.kernel_upgrade, expected_upgrade);
    }

    #[test]
    // Assert that trying to create a chunked transaction has no impact. Only
    // the first `NewChunkedTransaction` should be considered.
    fn recreate_chunked_transaction() {
        let mut host = MockHost::default();

        let tx_hash = [0; TRANSACTION_HASH_SIZE];
        let new_chunk1 = Input::NewChunkedTransaction {
            tx_hash,
            num_chunks: 2,
        };
        let new_chunk2 = Input::NewChunkedTransaction {
            tx_hash,
            num_chunks: 42,
        };

        host.add_external(Bytes::from(input_to_bytes(
            SMART_ROLLUP_ADDRESS,
            new_chunk1,
        )));
        host.add_external(Bytes::from(input_to_bytes(
            SMART_ROLLUP_ADDRESS,
            new_chunk2,
        )));

        let _inbox_content = read_inbox(&mut host, SMART_ROLLUP_ADDRESS, None).unwrap();

        let num_chunks = chunked_transaction_num_chunks(&mut host, &tx_hash)
            .expect("The number of chunks should exist");
        // Only the first `NewChunkedTransaction` should be considered.
        assert_eq!(num_chunks, 2);
    }

    #[test]
    // Assert that an out of bound chunk is simply ignored and does
    // not make the kernel fail.
    fn out_of_bound_chunk_is_ignored() {
        let mut host = MockHost::default();

        let (data, _tx) = large_transaction();
        let tx_hash = ZERO_TX_HASH;

        let mut inputs = make_chunked_transactions(tx_hash, data);
        let new_chunk = inputs.remove(0);
        let chunk = inputs.remove(0);

        // Announce a chunked transaction.
        host.add_external(Bytes::from(input_to_bytes(SMART_ROLLUP_ADDRESS, new_chunk)));

        // Give a chunk with an invalid `i`.
        let out_of_bound_i = 42;
        let chunk = match chunk {
            Input::TransactionChunk {
                tx_hash,
                i: _,
                data,
            } => Input::TransactionChunk {
                tx_hash,
                i: out_of_bound_i,
                data,
            },
            _ => panic!("Expected a transaction chunk"),
        };
        host.add_external(Bytes::from(input_to_bytes(SMART_ROLLUP_ADDRESS, chunk)));

        let _inbox_content = read_inbox(&mut host, SMART_ROLLUP_ADDRESS, None).unwrap();

        // The out of bounds chunk should not exist.
        let chunked_transaction_path = chunked_transaction_path(&tx_hash).unwrap();
        let transaction_chunk_path =
            transaction_chunk_path(&chunked_transaction_path, out_of_bound_i).unwrap();
        if read_transaction_chunk_data(&mut host, &transaction_chunk_path).is_ok() {
            panic!("The chunk should not exist in the storage")
        }
    }

    #[test]
    // Assert that an unknown chunk is simply ignored and does
    // not make the kernel fail.
    fn unknown_chunk_is_ignored() {
        let mut host = MockHost::default();

        let (data, _tx) = large_transaction();
        let tx_hash = ZERO_TX_HASH;

        let mut inputs = make_chunked_transactions(tx_hash, data);
        let chunk = inputs.remove(1);

        // Extract the index of the non existing chunked transaction.
        let index = match chunk {
            Input::TransactionChunk { i, .. } => i,
            _ => panic!("Expected a transaction chunk"),
        };

        host.add_external(Bytes::from(input_to_bytes(SMART_ROLLUP_ADDRESS, chunk)));

        let _inbox_content = read_inbox(&mut host, SMART_ROLLUP_ADDRESS, None).unwrap();

        // The unknown chunk should not exist.
        let chunked_transaction_path = chunked_transaction_path(&tx_hash).unwrap();
        let transaction_chunk_path =
            transaction_chunk_path(&chunked_transaction_path, index).unwrap();
        if read_transaction_chunk_data(&mut host, &transaction_chunk_path).is_ok() {
            panic!("The chunk should not exist in the storage")
        }
    }

    #[test]
    // Assert that a transaction is marked as complete only when each chunk
    // is stored in the storage. That is, if a transaction chunk is sent twice,
    // it rewrites the chunk.
    //
    // This serves as a non-regression test, a previous optimization made unwanted
    // behavior for very little gain:
    //
    // Level 0:
    // - New chunk of size 2
    // - Chunk 0
    //
    // Level 1:
    // - New chunk of size 2 (ignored)
    // - Chunk 0
    // |--> Oh great! I have the two chunks for my transaction, it is then complete!
    // - Chunk 1
    // |--> Fails because the chunk is unknown
    fn transaction_is_complete_when_each_chunk_is_stored() {
        let mut host = MockHost::default();

        let (data, tx) = large_transaction();
        let tx_hash = ZERO_TX_HASH;

        let inputs = make_chunked_transactions(tx_hash, data);
        // The test works if there are 3 inputs: new chunked of size 2, first and second
        // chunks.
        assert_eq!(inputs.len(), 3);

        let new_chunk = inputs[0].clone();
        let chunk0 = inputs[1].clone();

        host.add_external(Bytes::from(input_to_bytes(SMART_ROLLUP_ADDRESS, new_chunk)));

        host.add_external(Bytes::from(input_to_bytes(SMART_ROLLUP_ADDRESS, chunk0)));

        let inbox_content = read_inbox(&mut host, SMART_ROLLUP_ADDRESS, None).unwrap();
        assert_eq!(
            inbox_content,
            InboxContent {
                kernel_upgrade: None,
                transactions: vec![]
            }
        );

        // On the next level, try to re-give the chunks, but this time in full:
        for input in inputs {
            host.add_external(Bytes::from(input_to_bytes(SMART_ROLLUP_ADDRESS, input)))
        }
        let inbox_content = read_inbox(&mut host, SMART_ROLLUP_ADDRESS, None).unwrap();

        let expected_transactions = vec![Transaction {
            tx_hash: ZERO_TX_HASH,
            content: Ethereum(tx),
        }];
        assert_eq!(inbox_content.transactions, expected_transactions);
    }

    #[test]
    fn parse_valid_simple_transaction_framed() {
        // Don't use zero-hash for rollup here - as the long string of zeros is still valid under the previous
        // parsing. This won't happen in practice, though
        let address = smart_rollup_address();

        let mut host = MockHost::with_address(&address);

        let tx =
            EthereumTransactionCommon::from_bytes(&hex::decode("f86d80843b9aca00825208940b52d4d3be5d18a7ab5\
e4476a2f5382bbf2b38d888016345785d8a000080820a95a0d9ef1298c18c88604e3f08e14907a17dfa81b1dc6b37948abe189d8db5cb8a43a06\
fc7040a71d71d3cb74bd05ead7046b10668ad255da60391c017eea31555f156").unwrap()).unwrap();

        let input = Input::SimpleTransaction(Box::new(Transaction {
            tx_hash: ZERO_TX_HASH,
            content: Ethereum(tx.clone()),
        }));

        let mut buffer = Vec::new();
        match input {
            Input::SimpleTransaction(tx) => {
                // Simple transaction tag
                buffer.push(0);
                buffer.extend_from_slice(&tx.tx_hash);
                let mut tx_bytes = match tx.content {
                    Ethereum(tx) => tx.into(),
                    _ => panic!(
                        "Simple transaction can contain only ethereum transactions"
                    ),
                };

                buffer.append(&mut tx_bytes)
            }
            _ => unreachable!("Not tested"),
        };

        let framed = ExternalMessageFrame::Targetted {
            address,
            contents: buffer,
        };

        host.add_external(framed);

        let inbox_content = read_inbox(&mut host, SMART_ROLLUP_ADDRESS, None).unwrap();
        let expected_transactions = vec![Transaction {
            tx_hash: ZERO_TX_HASH,
            content: Ethereum(tx),
        }];
        assert_eq!(inbox_content.transactions, expected_transactions);
    }
}
