// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT
use primitive_types::{H160, U256};
use sha3::{Digest, Keccak256};
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_ethereum::signatures::EthereumTransactionCommon;
use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_debug::debug_msg;
use tezos_smart_rollup_host::runtime::Runtime;

use crate::block_in_progress::BlockInProgress;
use crate::parsing::{
    Input, InputResult, MAX_SIZE_PER_CHUNK, SIGNATURE_HASH_SIZE, UPGRADE_NONCE_SIZE,
};
use crate::simulation;
use crate::storage::{
    get_and_increment_deposit_nonce, read_dictator_key, read_kernel_upgrade_nonce,
    store_last_info_per_level_timestamp,
};

use crate::error::UpgradeProcessError::{InvalidUpgradeNonce, NoDictator};
use crate::upgrade::check_dictator_signature;
use crate::Error;

use tezos_ethereum::transaction::TransactionHash;

#[derive(Debug, PartialEq, Clone)]
pub struct Deposit {
    pub amount: U256,
    pub gas_price: U256,
    pub receiver: H160,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TransactionContent {
    Ethereum(EthereumTransactionCommon),
    Deposit(Deposit),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Transaction {
    pub tx_hash: TransactionHash,
    pub content: TransactionContent,
}

impl Transaction {
    pub fn estimate_ticks(&self) -> u64 {
        // all details of tick model stay in the same module
        BlockInProgress::estimate_ticks_for_transaction(self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct KernelUpgrade {
    pub nonce: [u8; UPGRADE_NONCE_SIZE],
    pub preimage_hash: [u8; PREIMAGE_HASH_SIZE],
    pub signature: [u8; SIGNATURE_HASH_SIZE],
}

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
                    Err(e) => debug_msg!(
                        host,
                        "Error while processing the kernel upgrade: {:?}\n",
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
    use libsecp256k1::PublicKey;
    use tezos_data_encoding::types::Bytes;
    use tezos_ethereum::transaction::TRANSACTION_HASH_SIZE;
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

    #[test]
    fn parse_valid_simple_transaction() {
        let mut host = MockHost::default();

        let tx =
            EthereumTransactionCommon::from_rlp_bytes(&hex::decode("f86d80843b9aca00825208940b52d4d3be5d18a7ab5e4476a2f5382bbf2b38d888016345785d8a000080820a95a0d9ef1298c18c88604e3f08e14907a17dfa81b1dc6b37948abe189d8db5cb8a43a06fc7040a71d71d3cb74bd05ead7046b10668ad255da60391c017eea31555f156").unwrap()).unwrap();
        let input = Input::SimpleTransaction(Box::new(Transaction {
            tx_hash: ZERO_TX_HASH,
            content: Ethereum(tx.clone()),
        }));

        host.add_external(Bytes::from(input_to_bytes(
            ZERO_SMART_ROLLUP_ADDRESS,
            input,
        )));

        let inbox_content =
            read_inbox(&mut host, ZERO_SMART_ROLLUP_ADDRESS, None).unwrap();
        let expected_transactions = vec![Transaction {
            tx_hash: ZERO_TX_HASH,
            content: Ethereum(tx),
        }];
        assert_eq!(inbox_content.transactions, expected_transactions);
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

        let inbox_content =
            read_inbox(&mut host, ZERO_SMART_ROLLUP_ADDRESS, None).unwrap();
        let expected_transactions = vec![Transaction {
            tx_hash: ZERO_TX_HASH,
            content: Ethereum(tx),
        }];
        assert_eq!(inbox_content.transactions, expected_transactions);
    }

    #[test]
    fn parse_valid_kernel_upgrade() {
        let mut host = MockHost::default();
        crate::storage::store_kernel_upgrade_nonce(&mut host, 1).unwrap();

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

        host.add_external(Bytes::from(input_to_bytes(
            ZERO_SMART_ROLLUP_ADDRESS,
            input,
        )));

        let inbox_content =
            read_inbox(&mut host, ZERO_SMART_ROLLUP_ADDRESS, None).unwrap();
        let expected_upgrade = Some(kernel_upgrade);
        assert_eq!(inbox_content.kernel_upgrade, expected_upgrade);
    }
}
