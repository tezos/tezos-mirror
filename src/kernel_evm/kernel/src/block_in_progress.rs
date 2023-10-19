// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::apply::{TransactionObjectInfo, TransactionReceiptInfo};
use crate::current_timestamp;
use crate::error::Error;
use crate::error::TransferError::CumulativeGasUsedOverflow;
use crate::inbox::Transaction;
use crate::storage;
use crate::tick_model;
use anyhow::Context;
use primitive_types::{H256, U256};
use rlp::{Decodable, DecoderError, Encodable};
use std::collections::VecDeque;
use tezos_ethereum::block::{BlockConstants, L2Block};
use tezos_ethereum::rlp_helpers::*;
use tezos_ethereum::transaction::{
    TransactionObject, TransactionReceipt, TransactionStatus, TransactionType,
    TRANSACTION_HASH_SIZE,
};
use tezos_ethereum::Bloom;
use tezos_smart_rollup_host::runtime::Runtime;

#[derive(Debug, PartialEq, Clone)]
/// Container for all data needed during block computation
pub struct BlockInProgress {
    /// block number
    pub number: U256,
    /// queue containing the transactions to execute
    tx_queue: VecDeque<Transaction>,
    /// list of transactions executed without issue
    valid_txs: Vec<[u8; TRANSACTION_HASH_SIZE]>,
    /// gas accumulator
    pub cumulative_gas: U256,
    /// index for next transaction
    pub index: u32,
    /// gas price for transactions in the block being created
    pub gas_price: U256,
    /// hash to use for receipt
    /// (computed from number, not the correct way to do it)
    pub hash: H256,
    /// hash of the parent
    pub parent_hash: H256,
    /// Cumulative number of ticks used
    pub estimated_ticks: u64,
    pub logs_bloom: Bloom,
}

impl Encodable for BlockInProgress {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(9);
        stream.append(&self.number);
        append_queue(stream, &self.tx_queue);
        append_txs(stream, &self.valid_txs);
        stream.append(&self.cumulative_gas);
        stream.append(&self.index);
        stream.append(&self.gas_price);
        stream.append(&self.hash);
        stream.append(&self.parent_hash);
        stream.append(&self.logs_bloom);
    }
}

fn append_queue(stream: &mut rlp::RlpStream, queue: &VecDeque<Transaction>) {
    stream.begin_list(queue.len());
    for transaction in queue {
        stream.append(transaction);
    }
}

fn append_txs(stream: &mut rlp::RlpStream, valid_txs: &Vec<[u8; TRANSACTION_HASH_SIZE]>) {
    stream.begin_list(valid_txs.len());
    valid_txs.iter().for_each(|tx| {
        stream.append_iter(*tx);
    })
}

impl Decodable for BlockInProgress {
    fn decode(decoder: &rlp::Rlp<'_>) -> Result<BlockInProgress, rlp::DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 9 {
            return Err(DecoderError::RlpIncorrectListLen);
        }

        let mut it = decoder.iter();
        let number: U256 = decode_field(&next(&mut it)?, "number")?;
        let tx_queue: VecDeque<Transaction> = decode_queue(&next(&mut it)?)?;
        let valid_txs: Vec<[u8; TRANSACTION_HASH_SIZE]> =
            decode_valid_txs(&next(&mut it)?)?;
        let cumulative_gas: U256 = decode_field(&next(&mut it)?, "cumulative_gas")?;
        let index: u32 = decode_field(&next(&mut it)?, "index")?;
        let gas_price: U256 = decode_field(&next(&mut it)?, "gas_price")?;
        let hash: H256 = decode_field(&next(&mut it)?, "hash")?;
        let parent_hash: H256 = decode_field(&next(&mut it)?, "parent_hash")?;
        let logs_bloom: Bloom = decode_field(&next(&mut it)?, "logs_bloom")?;
        let estimated_ticks: u64 = 0;
        let bip = Self {
            number,
            tx_queue,
            valid_txs,
            cumulative_gas,
            index,
            gas_price,
            hash,
            parent_hash,
            estimated_ticks,
            logs_bloom,
        };
        Ok(bip)
    }
}

fn decode_valid_txs(
    decoder: &rlp::Rlp<'_>,
) -> Result<Vec<[u8; TRANSACTION_HASH_SIZE]>, DecoderError> {
    if !decoder.is_list() {
        return Err(DecoderError::RlpExpectedToBeList);
    }
    let mut valid_txs = Vec::with_capacity(decoder.item_count()?);
    for item in decoder.iter() {
        let tx = decode_tx_hash(item)?;
        valid_txs.push(tx);
    }
    Ok(valid_txs)
}

fn decode_queue(decoder: &rlp::Rlp<'_>) -> Result<VecDeque<Transaction>, DecoderError> {
    if !decoder.is_list() {
        return Err(DecoderError::RlpExpectedToBeList);
    }
    let mut queue = VecDeque::with_capacity(decoder.item_count()?);
    for item in decoder.iter() {
        let tx: Transaction = item.as_val()?;
        queue.push_back(tx);
    }
    Ok(queue)
}

impl BlockInProgress {
    pub fn queue_length(&self) -> usize {
        self.tx_queue.len()
    }

    pub fn new_with_ticks(
        number: U256,
        parent_hash: H256,
        gas_price: U256,
        transactions: VecDeque<Transaction>,
        estimated_ticks: u64,
    ) -> Self {
        Self {
            number,
            tx_queue: transactions,
            valid_txs: Vec::new(),
            cumulative_gas: U256::zero(),
            index: 0,
            gas_price,
            // hash is not the true value of the hashed block
            // it should be computed at the end, and not included in the receipt
            // the block is referenced in the storage by the block number anyway
            hash: H256(number.into()),
            parent_hash,
            estimated_ticks,
            logs_bloom: Bloom::default(),
        }
    }

    // constructor of raw structure, used in tests
    #[cfg(test)]
    pub fn new(
        number: U256,
        gas_price: U256,
        transactions: VecDeque<Transaction>,
    ) -> BlockInProgress {
        Self::new_with_ticks(number, H256::zero(), gas_price, transactions, 0u64)
    }

    pub fn from_queue_element(
        proposal: crate::blueprint::QueueElement,
        current_block_number: U256,
        parent_hash: H256,
        constants: &BlockConstants,
        tick_counter: u64,
    ) -> BlockInProgress {
        match proposal {
            crate::blueprint::QueueElement::Blueprint(proposal) => {
                // proposal is turn into a ring to allow poping from the front
                let ring = proposal.transactions.into();
                BlockInProgress::new_with_ticks(
                    current_block_number,
                    parent_hash,
                    constants.gas_price,
                    ring,
                    tick_counter,
                )
            }
            crate::blueprint::QueueElement::BlockInProgress(mut bip) => {
                bip.estimated_ticks = tick_counter;
                *bip
            }
        }
    }

    pub fn register_valid_transaction<Host: Runtime>(
        &mut self,
        transaction: &Transaction,
        object_info: TransactionObjectInfo,
        receipt_info: TransactionReceiptInfo,
        host: &mut Host,
    ) -> Result<(), anyhow::Error> {
        // account for gas
        self.cumulative_gas = self
            .cumulative_gas
            .checked_add(object_info.gas_used)
            .ok_or(Error::Transfer(CumulativeGasUsedOverflow))?;

        // account for ticks
        self.estimated_ticks +=
            tick_model::ticks_of_valid_transaction(transaction, &receipt_info);

        // register transaction as done
        self.valid_txs.push(transaction.tx_hash);
        self.index += 1;

        // make receipt
        let receipt = self.make_receipt(receipt_info);

        // extend BIP's logs bloom
        self.logs_bloom.accrue_bloom(&receipt.logs_bloom);

        // store info
        storage::store_transaction_receipt(host, &receipt)
            .context("Failed to store the receipt")?;
        storage::store_transaction_object(host, &self.make_object(object_info))
            .context("Failed to store the transaction object")?;
        Ok(())
    }

    pub fn account_for_invalid_transaction(&mut self) {
        self.estimated_ticks += tick_model::ticks_of_invalid_transaction();
    }

    pub fn finalize_and_store<Host: Runtime>(
        self,
        host: &mut Host,
    ) -> Result<L2Block, anyhow::Error> {
        let timestamp = current_timestamp(host);
        let new_block = L2Block {
            timestamp,
            gas_used: self.cumulative_gas,
            ..L2Block::new(
                self.number,
                self.valid_txs,
                timestamp,
                self.parent_hash,
                self.logs_bloom,
            )
        };
        storage::store_current_block(host, &new_block)
            .context("Failed to store the current block")?;
        Ok(new_block)
    }

    pub fn pop_tx(&mut self) -> Option<Transaction> {
        self.tx_queue.pop_front()
    }

    pub fn has_tx(&self) -> bool {
        !self.tx_queue.is_empty()
    }

    pub fn would_overflow(&self) -> bool {
        match self.tx_queue.front() {
            Some(transaction) => {
                tick_model::estimate_would_overflow(self.estimated_ticks, transaction)
            }
            None => false, // should not happen, but false is a safe value anyway
        }
    }

    pub fn make_receipt(
        &self,
        receipt_info: TransactionReceiptInfo,
    ) -> TransactionReceipt {
        let TransactionReceiptInfo {
            tx_hash: hash,
            index,
            caller: from,
            to,
            execution_outcome,
            ..
        } = receipt_info;

        let &Self {
            hash: block_hash,
            number: block_number,
            gas_price: effective_gas_price,
            cumulative_gas,
            ..
        } = self;

        match execution_outcome {
            Some(outcome) => TransactionReceipt {
                hash,
                index,
                block_hash,
                block_number,
                from,
                to,
                cumulative_gas_used: cumulative_gas,
                effective_gas_price,
                gas_used: U256::from(outcome.gas_used),
                contract_address: outcome.new_address,
                logs_bloom: TransactionReceipt::logs_to_bloom(&outcome.logs),
                logs: outcome.logs,
                type_: TransactionType::Legacy,
                status: if outcome.is_success {
                    TransactionStatus::Success
                } else {
                    TransactionStatus::Failure
                },
            },
            None => TransactionReceipt {
                hash,
                index,
                block_hash,
                block_number,
                from,
                to,
                cumulative_gas_used: cumulative_gas,
                effective_gas_price,
                gas_used: U256::zero(),
                contract_address: None,
                logs: vec![],
                logs_bloom: Bloom::default(),
                type_: TransactionType::Legacy,
                status: TransactionStatus::Failure,
            },
        }
    }

    pub fn make_object(&self, object_info: TransactionObjectInfo) -> TransactionObject {
        TransactionObject {
            block_hash: self.hash,
            block_number: self.number,
            from: object_info.from,
            gas_used: object_info.gas_used,
            gas_price: object_info.gas_price,
            hash: object_info.hash,
            input: object_info.input,
            nonce: object_info.nonce,
            to: object_info.to,
            index: object_info.index,
            value: object_info.value,
            signature: object_info.signature,
        }
    }
}

#[cfg(test)]
mod tests {

    use super::BlockInProgress;
    use crate::inbox::{Deposit, Transaction, TransactionContent};
    use primitive_types::{H160, H256, U256};
    use rlp::{Decodable, Encodable, Rlp};
    use tezos_ethereum::{
        transaction::{TransactionType, TRANSACTION_HASH_SIZE},
        tx_common::EthereumTransactionCommon,
        tx_signature::TxSignature,
        Bloom,
    };

    fn new_sig_unsafe(v: u64, r: H256, s: H256) -> TxSignature {
        TxSignature::new(U256::from(v), r, s).unwrap()
    }

    fn dummy_etc(i: u8) -> EthereumTransactionCommon {
        EthereumTransactionCommon {
            type_: TransactionType::Legacy,
            chain_id: U256::from(i),
            nonce: U256::from(i),
            max_fee_per_gas: U256::from(i),
            max_priority_fee_per_gas: U256::from(i),
            gas_limit: i.into(),
            to: None,
            value: U256::from(i),
            data: Vec::new(),
            access_list: vec![],
            signature: Some(new_sig_unsafe(
                (36 + i * 2).into(), // need to be consistent with chain_id
                H256::from([i; 32]),
                H256::from([i; 32]),
            )),
        }
    }

    fn dummy_tx_eth(i: u8) -> Transaction {
        Transaction {
            tx_hash: [i; TRANSACTION_HASH_SIZE],
            content: TransactionContent::Ethereum(dummy_etc(i)),
        }
    }

    fn dummy_tx_deposit(i: u8) -> Transaction {
        let deposit = Deposit {
            amount: U256::from(i),
            gas_price: U256::from(i),
            receiver: H160::from([i; 20]),
        };
        Transaction {
            tx_hash: [i; TRANSACTION_HASH_SIZE],
            content: TransactionContent::Deposit(deposit),
        }
    }

    #[test]
    fn test_encode_bip_ethereum() {
        let bip = BlockInProgress {
            number: U256::from(42),
            tx_queue: vec![dummy_tx_eth(1), dummy_tx_eth(8)].into(),
            valid_txs: vec![[2; TRANSACTION_HASH_SIZE], [9; TRANSACTION_HASH_SIZE]],
            cumulative_gas: U256::from(3),
            index: 4,
            gas_price: U256::from(5),
            hash: H256::from([6; 32]),
            parent_hash: H256::from([5; 32]),
            estimated_ticks: 99,
            logs_bloom: Bloom::default(),
        };

        let encoded = bip.rlp_bytes();
        let expected = "f902752af8e6f871a00101010101010101010101010101010101010101010101010101010101010101f84e01b84bf84901010180018026a00101010101010101010101010101010101010101010101010101010101010101a00101010101010101010101010101010101010101010101010101010101010101f871a00808080808080808080808080808080808080808080808080808080808080808f84e01b84bf84908080880088034a00808080808080808080808080808080808080808080808080808080808080808a00808080808080808080808080808080808080808080808080808080808080808f842a00202020202020202020202020202020202020202020202020202020202020202a00909090909090909090909090909090909090909090909090909090909090909030405a00606060606060606060606060606060606060606060606060606060606060606a00505050505050505050505050505050505050505050505050505050505050505b9010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";
        assert_eq!(hex::encode(encoded), expected);

        let bytes = hex::decode(expected).expect("Should be valid hex string");
        let decoder = Rlp::new(&bytes);
        let decoded =
            BlockInProgress::decode(&decoder).expect("Should have decoded data");

        // the estimated ticks are not stored
        let fresh_bip = BlockInProgress {
            estimated_ticks: 0,
            ..bip
        };
        assert_eq!(decoded, fresh_bip);
    }

    #[test]
    fn test_encode_bip_deposit() {
        let bip = BlockInProgress {
            number: U256::from(42),
            tx_queue: vec![dummy_tx_deposit(1), dummy_tx_deposit(8)].into(),
            valid_txs: vec![[2; TRANSACTION_HASH_SIZE], [9; TRANSACTION_HASH_SIZE]],
            cumulative_gas: U256::from(3),
            index: 4,
            gas_price: U256::from(5),
            hash: H256::from([6; 32]),
            parent_hash: H256::from([5; 32]),
            estimated_ticks: 99,
            logs_bloom: Bloom::default(),
        };

        let encoded = bip.rlp_bytes();
        let expected = "f902092af87af83ba00101010101010101010101010101010101010101010101010101010101010101d902d70101940101010101010101010101010101010101010101f83ba00808080808080808080808080808080808080808080808080808080808080808d902d70808940808080808080808080808080808080808080808f842a00202020202020202020202020202020202020202020202020202020202020202a00909090909090909090909090909090909090909090909090909090909090909030405a00606060606060606060606060606060606060606060606060606060606060606a00505050505050505050505050505050505050505050505050505050505050505b9010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";
        assert_eq!(hex::encode(encoded), expected);

        let bytes = hex::decode(expected).expect("Should be valid hex string");
        let decoder = Rlp::new(&bytes);
        let decoded =
            BlockInProgress::decode(&decoder).expect("Should have decoded data");

        // the estimated ticks are not stored
        let fresh_bip = BlockInProgress {
            estimated_ticks: 0,
            ..bip
        };
        assert_eq!(decoded, fresh_bip);
    }

    #[test]
    fn test_encode_bip_mixed() {
        let bip = BlockInProgress {
            number: U256::from(42),
            tx_queue: vec![dummy_tx_eth(1), dummy_tx_deposit(8)].into(),
            valid_txs: vec![[2; TRANSACTION_HASH_SIZE], [9; TRANSACTION_HASH_SIZE]],
            cumulative_gas: U256::from(3),
            index: 4,
            gas_price: U256::from(5),
            hash: H256::from([6; 32]),
            parent_hash: H256::from([5; 32]),
            estimated_ticks: 99,
            logs_bloom: Bloom::default(),
        };

        let encoded = bip.rlp_bytes();
        let expected = "f9023f2af8b0f871a00101010101010101010101010101010101010101010101010101010101010101f84e01b84bf84901010180018026a00101010101010101010101010101010101010101010101010101010101010101a00101010101010101010101010101010101010101010101010101010101010101f83ba00808080808080808080808080808080808080808080808080808080808080808d902d70808940808080808080808080808080808080808080808f842a00202020202020202020202020202020202020202020202020202020202020202a00909090909090909090909090909090909090909090909090909090909090909030405a00606060606060606060606060606060606060606060606060606060606060606a00505050505050505050505050505050505050505050505050505050505050505b9010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";
        assert_eq!(hex::encode(encoded), expected);

        let bytes = hex::decode(expected).expect("Should be valid hex string");
        let decoder = Rlp::new(&bytes);
        let decoded =
            BlockInProgress::decode(&decoder).expect("Should have decoded data");

        // the estimated ticks are not stored
        let fresh_bip = BlockInProgress {
            estimated_ticks: 0,
            ..bip
        };
        assert_eq!(decoded, fresh_bip);
    }
}
