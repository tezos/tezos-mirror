// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::apply::{TransactionObjectInfo, TransactionReceiptInfo};
use crate::block_storage;
use crate::chains::ETHERLINK_SAFE_STORAGE_ROOT_PATH;
use crate::error::Error;
use crate::error::TransferError::CumulativeGasUsedOverflow;
use crate::gas_price::base_fee_per_gas;
use crate::l2block::L2Block;
use crate::tick_model;
use crate::transaction::{Transaction, Transactions, Transactions::EthTxs};
use alloy_consensus::proofs::ordered_trie_root_with_encoder;
use alloy_consensus::EMPTY_ROOT_HASH;
use anyhow::Context;
use primitive_types::{H160, H256, U256};
use revm_etherlink::helpers::legacy::alloy_to_log;
use revm_etherlink::storage::world_state_handler::EVM_ACCOUNTS_PATH;
use rlp::{Decodable, DecoderError, Encodable};
use std::collections::VecDeque;
use tezos_ethereum::block::{BlockConstants, BlockFees, EthBlock};
use tezos_ethereum::rlp_helpers::*;
use tezos_ethereum::transaction::{
    IndexedLog, TransactionHash, TransactionObject, TransactionReceipt,
    TransactionStatus, TRANSACTION_HASH_SIZE,
};
use tezos_ethereum::Bloom;
use tezos_evm_logging::{log, tracing::instrument, Level::*};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_encoding::timestamp::Timestamp;
use tezos_smart_rollup_host::path::RefPath;

#[derive(Debug, PartialEq, Clone)]
/// Container for all data needed during block computation
pub struct EthBlockInProgress {
    /// block number
    pub number: U256,
    /// queue containing the transactions to execute
    tx_queue: VecDeque<Transaction>,
    /// list of transactions executed without issue
    valid_txs: Vec<TransactionHash>,
    pub delayed_txs: Vec<TransactionHash>,
    /// gas accumulator
    pub cumulative_gas: U256,
    /// index for next transaction
    pub index: u32,
    /// hash of the parent
    pub parent_hash: H256,
    /// Cumulative number of ticks used in current kernel run
    pub estimated_ticks_in_run: u64,
    /// Cumulative number of ticks used in the block
    pub estimated_ticks_in_block: u64,
    /// logs bloom filter
    pub logs_bloom: Bloom,
    /// offset for the first log of the next transaction
    pub logs_offset: u64,
    /// Timestamp
    pub timestamp: Timestamp,
    /// The base fee, is adjusted before and after the computation of
    /// the block
    pub base_fee_per_gas: U256,
    /// Unit of gas used to execute transactions
    pub cumulative_execution_gas: U256,
    /// Cumulative receipts
    pub cumulative_receipts: Vec<TransactionReceipt>,
    /// Cumulative transactions objects
    pub cumulative_tx_objects: Vec<TransactionObject>,
}

impl Encodable for EthBlockInProgress {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        let EthBlockInProgress {
            number,
            tx_queue,
            valid_txs,
            delayed_txs,
            cumulative_gas,
            index,
            parent_hash,
            estimated_ticks_in_run: _,
            estimated_ticks_in_block,
            logs_bloom,
            logs_offset,
            timestamp,
            base_fee_per_gas,
            cumulative_execution_gas,
            cumulative_receipts,
            cumulative_tx_objects: cumulative_objects,
        } = self;
        stream.begin_list(15);
        stream.append(number);
        append_queue(stream, tx_queue);
        append_txs(stream, valid_txs);
        append_txs(stream, delayed_txs);
        stream.append(cumulative_gas);
        stream.append(index);
        stream.append(parent_hash);
        stream.append(estimated_ticks_in_block);
        stream.append(logs_bloom);
        stream.append(logs_offset);
        append_timestamp(stream, *timestamp);
        stream.append(base_fee_per_gas);
        stream.append(cumulative_execution_gas);
        append_receipts(stream, cumulative_receipts);
        append_tx_objects(stream, cumulative_objects);
    }
}

fn append_queue(stream: &mut rlp::RlpStream, queue: &VecDeque<Transaction>) {
    stream.begin_list(queue.len());
    for transaction in queue {
        stream.append(transaction);
    }
}

fn append_txs(stream: &mut rlp::RlpStream, valid_txs: &[[u8; TRANSACTION_HASH_SIZE]]) {
    stream.begin_list(valid_txs.len());
    valid_txs.iter().for_each(|tx| {
        stream.append_iter(*tx);
    })
}

fn append_receipts(stream: &mut rlp::RlpStream, receipts: &[TransactionReceipt]) {
    stream.begin_list(receipts.len());
    receipts.iter().for_each(|receipt| {
        stream.append(receipt);
    })
}

fn append_tx_objects(stream: &mut rlp::RlpStream, objects: &[TransactionObject]) {
    stream.begin_list(objects.len());
    objects.iter().for_each(|object| {
        stream.append(object);
    })
}

impl Decodable for EthBlockInProgress {
    fn decode(decoder: &rlp::Rlp<'_>) -> Result<EthBlockInProgress, rlp::DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 15 {
            return Err(DecoderError::RlpIncorrectListLen);
        }

        let mut it = decoder.iter();
        let number: U256 = decode_field(&next(&mut it)?, "number")?;
        let tx_queue: VecDeque<Transaction> = decode_queue(&next(&mut it)?)?;
        let valid_txs: Vec<TransactionHash> = decode_valid_txs(&next(&mut it)?)?;
        let delayed_txs: Vec<TransactionHash> = decode_valid_txs(&next(&mut it)?)?;
        let cumulative_gas: U256 = decode_field(&next(&mut it)?, "cumulative_gas")?;
        let index: u32 = decode_field(&next(&mut it)?, "index")?;
        let parent_hash: H256 = decode_field(&next(&mut it)?, "parent_hash")?;
        let estimated_ticks_in_block: u64 =
            decode_field(&next(&mut it)?, "estimated_ticks_in_block")?;
        let logs_bloom: Bloom = decode_field(&next(&mut it)?, "logs_bloom")?;
        let logs_offset: u64 = decode_field(&next(&mut it)?, "logs_offset")?;
        let timestamp = decode_timestamp(&next(&mut it)?)?;
        let base_fee_per_gas = decode_field(&next(&mut it)?, "base_fee_per_gas")?;
        let cumulative_execution_gas: U256 =
            decode_field(&next(&mut it)?, "cumulative_execution_gas")?;
        let cumulative_receipts: Vec<TransactionReceipt> =
            decode_receipts(&next(&mut it)?)?;
        let cumulative_tx_objects: Vec<TransactionObject> =
            decode_tx_objects(&next(&mut it)?)?;

        let bip = Self {
            number,
            tx_queue,
            valid_txs,
            delayed_txs,
            cumulative_gas,
            index,
            parent_hash,
            estimated_ticks_in_run: 0,
            estimated_ticks_in_block,
            logs_bloom,
            logs_offset,
            timestamp,
            base_fee_per_gas,
            cumulative_execution_gas,
            cumulative_receipts,
            cumulative_tx_objects,
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

fn decode_receipts(
    decoder: &rlp::Rlp<'_>,
) -> Result<Vec<TransactionReceipt>, DecoderError> {
    if !decoder.is_list() {
        return Err(DecoderError::RlpExpectedToBeList);
    }
    let mut receipts = Vec::with_capacity(decoder.item_count()?);
    for item in decoder.iter() {
        let receipt: TransactionReceipt = item.as_val()?;
        receipts.push(receipt);
    }
    Ok(receipts)
}

fn decode_tx_objects(
    decoder: &rlp::Rlp<'_>,
) -> Result<Vec<TransactionObject>, DecoderError> {
    if !decoder.is_list() {
        return Err(DecoderError::RlpExpectedToBeList);
    }
    let mut objects = Vec::with_capacity(decoder.item_count()?);
    for item in decoder.iter() {
        let object: TransactionObject = item.as_val()?;
        objects.push(object);
    }
    Ok(objects)
}

impl EthBlockInProgress {
    pub fn queue_length(&self) -> usize {
        self.tx_queue.len()
    }

    #[allow(clippy::too_many_arguments)]
    pub fn new_with_ticks(
        number: U256,
        parent_hash: H256,
        transactions: VecDeque<Transaction>,
        estimated_ticks_in_run: u64,
        timestamp: Timestamp,
        base_fee_per_gas: U256,
    ) -> Self {
        Self {
            number,
            tx_queue: transactions,
            valid_txs: Vec::new(),
            delayed_txs: Vec::new(),
            cumulative_gas: U256::zero(),
            index: 0,
            parent_hash,
            estimated_ticks_in_block: 0,
            estimated_ticks_in_run,
            logs_bloom: Bloom::default(),
            logs_offset: 0,
            timestamp,
            base_fee_per_gas,
            cumulative_execution_gas: U256::zero(),
            cumulative_receipts: Vec::new(),
            cumulative_tx_objects: Vec::new(),
        }
    }

    // constructor of raw structure, used in tests
    #[cfg(test)]
    pub fn new(
        number: U256,
        transactions: VecDeque<Transaction>,
        base_fee_per_gas: U256,
    ) -> EthBlockInProgress {
        Self::new_with_ticks(
            number,
            H256::zero(),
            transactions,
            0u64,
            Timestamp::from(0i64),
            base_fee_per_gas,
        )
    }

    /// Derive `BlockConstants` based on current block in progress.
    /// Number and timestamp are taken from `self`.
    pub fn constants(
        &self,
        chain_id: U256,
        minimum_base_fee_per_gas: U256,
        da_fee_per_byte: U256,
        gas_limit: u64,
        coinbase: H160,
    ) -> BlockConstants {
        let timestamp = U256::from(self.timestamp.as_u64());
        let block_fees = BlockFees::new(
            minimum_base_fee_per_gas,
            self.base_fee_per_gas,
            da_fee_per_byte,
        );
        BlockConstants {
            number: self.number,
            coinbase,
            timestamp,
            gas_limit,
            block_fees,
            chain_id,
            prevrandao: None,
        }
    }

    pub fn from_blueprint(
        blueprint: crate::blueprint::Blueprint<Transactions>,
        number: U256,
        parent_hash: H256,
        tick_counter: u64,
        base_fee_per_gas: U256,
    ) -> EthBlockInProgress {
        // blueprint is turn into a ring to allow popping from the front
        let ring = match blueprint.transactions {
            EthTxs(transactions) => transactions.into(),
        };
        EthBlockInProgress::new_with_ticks(
            number,
            parent_hash,
            ring,
            tick_counter,
            blueprint.timestamp,
            base_fee_per_gas,
        )
    }

    fn add_gas(&mut self, gas: U256) -> Result<(), Error> {
        self.cumulative_gas = self
            .cumulative_gas
            .checked_add(gas)
            .ok_or(Error::Transfer(CumulativeGasUsedOverflow))?;
        Ok(())
    }

    fn add_ticks(&mut self, ticks: u64) {
        self.estimated_ticks_in_run += ticks;
        self.estimated_ticks_in_block += ticks;
    }

    pub fn register_delayed_transaction(&mut self, hash: TransactionHash) {
        self.delayed_txs.push(hash);
    }

    #[instrument(skip_all)]
    pub fn register_valid_transaction<Host: Runtime>(
        &mut self,
        transaction: &Transaction,
        object_info: TransactionObjectInfo,
        receipt_info: TransactionReceiptInfo,
        ticks_used: u64,
        execution_gas_used: U256,
        host: &mut Host,
    ) -> Result<(), anyhow::Error> {
        // account for gas
        host.add_execution_gas(receipt_info.execution_outcome.result.gas_used());

        self.add_gas(receipt_info.overall_gas_used)?;

        // account for transaction ticks
        self.add_ticks(tick_model::ticks_of_valid_transaction(
            transaction,
            ticks_used,
        ));

        // register transaction as done
        self.valid_txs.push(transaction.tx_hash);
        self.index += 1;

        // make receipt
        let receipt = self.make_receipt(receipt_info);
        let receipt_bloom_size: u64 = tick_model::bloom_size(&receipt.logs).try_into()?;
        log!(host, Benchmarking, "bloom size: {}", receipt_bloom_size);
        // extend BIP's logs bloom
        self.logs_bloom.accrue_bloom(&receipt.logs_bloom);

        self.cumulative_receipts.push(receipt);
        let tx_object = self.make_object(object_info);
        self.cumulative_tx_objects.push(tx_object);

        // keep track of execution gas used
        self.cumulative_execution_gas += execution_gas_used;

        Ok(())
    }

    pub fn account_for_invalid_transaction(&mut self, tx_data_size: u64) {
        self.add_ticks(tick_model::ticks_of_invalid_transaction(tx_data_size));
    }

    fn safe_store_get_hash<Host: Runtime>(
        host: &mut Host,
        path: &RefPath,
    ) -> Result<Vec<u8>, anyhow::Error> {
        match host.store_get_hash(path) {
            Ok(hash) => Ok(hash),
            _ => Ok("00000000000000000000000000000000".into()),
        }
    }

    fn receipts_root(&self) -> Vec<u8> {
        if self.valid_txs.is_empty() {
            EMPTY_ROOT_HASH.to_vec()
        } else {
            // Can't use `calculate_receipt_root` because it use `Encodable2718` which we can't use
            // also because of `bytes::BufMut` which use floats
            ordered_trie_root_with_encoder(&self.cumulative_receipts, |obj, buf| {
                obj.encode_2718(buf)
            })
            .to_vec()
        }
    }

    fn transactions_root(&self) -> Vec<u8> {
        if self.valid_txs.is_empty() {
            EMPTY_ROOT_HASH.to_vec()
        } else {
            // Can't use `calculate_transactions_root` because it use `Encodable2718` which we can't use
            // also because of `bytes::BufMut` which use floats
            ordered_trie_root_with_encoder(&self.cumulative_tx_objects, |obj, buf| {
                obj.encode_2718(buf)
            })
            .to_vec()
        }
    }

    #[cfg_attr(feature = "benchmark", inline(never))]
    pub fn finalize_and_store<Host: Runtime>(
        self,
        host: &mut Host,
        block_constants: &BlockConstants,
    ) -> Result<L2Block, anyhow::Error> {
        let state_root = Self::safe_store_get_hash(host, &EVM_ACCOUNTS_PATH)?;
        let receipts_root = self.receipts_root();
        block_storage::store_current_transactions_receipts(
            host,
            &ETHERLINK_SAFE_STORAGE_ROOT_PATH,
            &self.cumulative_receipts,
        )?;
        let transactions_root = self.transactions_root();
        block_storage::store_current_transactions_objects(
            host,
            &ETHERLINK_SAFE_STORAGE_ROOT_PATH,
            &self.cumulative_tx_objects,
        )?;
        let base_fee_per_gas = base_fee_per_gas(
            host,
            self.timestamp,
            block_constants.block_fees.minimum_base_fee_per_gas(),
        );
        let new_block = EthBlock::new(
            self.number,
            self.valid_txs,
            self.timestamp,
            self.parent_hash,
            self.logs_bloom,
            transactions_root,
            state_root,
            receipts_root,
            self.cumulative_gas,
            block_constants,
            base_fee_per_gas,
        );
        let new_block = L2Block::Etherlink(Box::new(new_block));
        block_storage::store_current(host, &ETHERLINK_SAFE_STORAGE_ROOT_PATH, &new_block)
            .context("Failed to store the current block")?;
        Ok(new_block)
    }

    pub fn pop_tx(&mut self) -> Option<Transaction> {
        self.tx_queue.pop_front()
    }

    pub fn repush_tx(&mut self, tx: Transaction) {
        self.tx_queue.push_front(tx)
    }

    pub fn has_tx(&self) -> bool {
        !self.tx_queue.is_empty()
    }

    pub fn make_receipt(
        &mut self,
        receipt_info: TransactionReceiptInfo,
    ) -> TransactionReceipt {
        let TransactionReceiptInfo {
            tx_hash: hash,
            index,
            caller: from,
            to,
            execution_outcome,
            effective_gas_price,
            type_,
            ..
        } = receipt_info;

        let &mut Self {
            number: block_number,
            cumulative_gas,
            logs_offset,
            ..
        } = self;

        let is_success = execution_outcome.result.is_success();
        let contract_address = execution_outcome
            .result
            .created_address()
            .map(|x| H160(*x.0));
        let log_iter = execution_outcome.result.logs().iter().map(alloy_to_log);
        let logs: Vec<IndexedLog> = log_iter
            .enumerate()
            .map(|(i, log)| IndexedLog {
                log,
                index: i as u64 + logs_offset,
            })
            .collect();
        self.logs_offset += logs.len() as u64;
        TransactionReceipt {
            hash,
            index,
            block_number,
            from,
            to,
            cumulative_gas_used: cumulative_gas,
            effective_gas_price,
            gas_used: receipt_info.overall_gas_used,
            contract_address,
            logs_bloom: TransactionReceipt::logs_to_bloom(&logs),
            logs,
            type_,
            status: if is_success {
                TransactionStatus::Success
            } else {
                TransactionStatus::Failure
            },
        }
    }

    pub fn make_object(&self, object_info: TransactionObjectInfo) -> TransactionObject {
        TransactionObject {
            block_number: self.number,
            from: object_info.from,
            gas_used: object_info.gas,
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

    #[cfg(test)]
    pub fn valid_txs(&self) -> Vec<[u8; TRANSACTION_HASH_SIZE]> {
        self.valid_txs.clone()
    }
}

#[cfg(test)]
mod tests {

    use super::EthBlockInProgress;
    use crate::bridge::Deposit;
    use crate::transaction::{Transaction, TransactionContent};
    use primitive_types::{H160, H256, U256};
    use rlp::{Decodable, Encodable, Rlp};
    use tezos_ethereum::{
        transaction::{TransactionType, TRANSACTION_HASH_SIZE},
        tx_common::EthereumTransactionCommon,
        tx_signature::TxSignature,
        Bloom,
    };
    use tezos_smart_rollup_encoding::timestamp::Timestamp;

    fn new_sig_unsafe(v: u64, r: H256, s: H256) -> TxSignature {
        TxSignature::new(U256::from(v), r, s).unwrap()
    }

    fn dummy_etc(i: u8) -> EthereumTransactionCommon {
        EthereumTransactionCommon::new(
            TransactionType::Legacy,
            Some(U256::from(i)),
            u64::from(i),
            U256::from(i),
            U256::from(i),
            i.into(),
            None,
            U256::from(i),
            Vec::new(),
            vec![],
            None,
            Some(new_sig_unsafe(
                (36 + i * 2).into(), // need to be consistent with chain_id
                H256::from([i; 32]),
                H256::from([i; 32]),
            )),
        )
    }

    fn dummy_tx_eth(i: u8) -> Transaction {
        Transaction {
            tx_hash: [i; TRANSACTION_HASH_SIZE],
            content: TransactionContent::Ethereum(dummy_etc(i)),
        }
    }

    fn dummy_tx_deposit(i: u8) -> Transaction {
        let deposit = Deposit {
            amount: i.into(),
            receiver: crate::bridge::DepositReceiver::Ethereum(H160([i; 20])),
            inbox_level: 1,
            inbox_msg_id: 0,
        };
        Transaction {
            tx_hash: [i; TRANSACTION_HASH_SIZE],
            content: TransactionContent::Deposit(deposit),
        }
    }

    #[test]
    fn test_encode_bip_ethereum() {
        let bip = EthBlockInProgress {
            number: U256::from(42),
            tx_queue: vec![dummy_tx_eth(1), dummy_tx_eth(8)].into(),
            valid_txs: vec![[2; TRANSACTION_HASH_SIZE], [9; TRANSACTION_HASH_SIZE]],
            delayed_txs: vec![],
            cumulative_gas: U256::from(3),
            index: 4,
            parent_hash: H256::from([5; 32]),
            estimated_ticks_in_block: 99,
            estimated_ticks_in_run: 199,
            logs_bloom: Bloom::default(),
            logs_offset: 33,
            timestamp: Timestamp::from(0i64),
            base_fee_per_gas: U256::from(21000u64),
            cumulative_execution_gas: U256::from(1),
            cumulative_receipts: vec![],
            cumulative_tx_objects: vec![],
        };

        let encoded = bip.rlp_bytes();
        let expected = "f902652af8e6f871a00101010101010101010101010101010101010101010101010101010101010101f84e01b84bf84901010180018026a00101010101010101010101010101010101010101010101010101010101010101a00101010101010101010101010101010101010101010101010101010101010101f871a00808080808080808080808080808080808080808080808080808080808080808f84e01b84bf84908080880088034a00808080808080808080808080808080808080808080808080808080808080808a00808080808080808080808080808080808080808080808080808080808080808f842a00202020202020202020202020202020202020202020202020202020202020202a00909090909090909090909090909090909090909090909090909090909090909c00304a0050505050505050505050505050505050505050505050505050505050505050563b90100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002188000000000000000082520801c0c0";

        pretty_assertions::assert_str_eq!(hex::encode(encoded), expected);

        let bytes = hex::decode(expected).expect("Should be valid hex string");
        let decoder = Rlp::new(&bytes);
        let decoded =
            EthBlockInProgress::decode(&decoder).expect("Should have decoded data");

        // the estimated ticks in the current run are not stored
        let fresh_bip = EthBlockInProgress {
            estimated_ticks_in_run: 0,
            ..bip
        };
        assert_eq!(decoded, fresh_bip);
    }

    #[test]
    fn test_encode_bip_deposit() {
        let bip = EthBlockInProgress {
            number: U256::from(42),
            tx_queue: vec![dummy_tx_deposit(1), dummy_tx_deposit(8)].into(),
            valid_txs: vec![[2; TRANSACTION_HASH_SIZE], [9; TRANSACTION_HASH_SIZE]],
            delayed_txs: vec![[2; TRANSACTION_HASH_SIZE]],
            cumulative_gas: U256::from(3),
            index: 4,
            parent_hash: H256::from([5; 32]),
            estimated_ticks_in_block: 99,
            estimated_ticks_in_run: 199,
            logs_bloom: Bloom::default(),
            logs_offset: 0,
            timestamp: Timestamp::from(0i64),
            base_fee_per_gas: U256::from(21000u64),
            cumulative_execution_gas: U256::from(1),
            cumulative_receipts: vec![],
            cumulative_tx_objects: vec![],
        };

        let encoded = bip.rlp_bytes();
        let expected = "f9021c2af87cf83ca00101010101010101010101010101010101010101010101010101010101010101da02d8019401010101010101010101010101010101010101010180f83ca00808080808080808080808080808080808080808080808080808080808080808da02d8089408080808080808080808080808080808080808080180f842a00202020202020202020202020202020202020202020202020202020202020202a00909090909090909090909090909090909090909090909090909090909090909e1a002020202020202020202020202020202020202020202020202020202020202020304a0050505050505050505050505050505050505050505050505050505050505050563b90100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008088000000000000000082520801c0c0";

        pretty_assertions::assert_str_eq!(hex::encode(encoded), expected);

        let bytes = hex::decode(expected).expect("Should be valid hex string");
        let decoder = Rlp::new(&bytes);
        let decoded =
            EthBlockInProgress::decode(&decoder).expect("Should have decoded data");

        // the estimated ticks in the current run are not stored
        let fresh_bip = EthBlockInProgress {
            estimated_ticks_in_run: 0,
            ..bip
        };
        assert_eq!(decoded, fresh_bip);
    }

    #[test]
    fn test_encode_bip_mixed() {
        let bip = EthBlockInProgress {
            number: U256::from(42),
            tx_queue: vec![dummy_tx_eth(1), dummy_tx_deposit(8)].into(),
            valid_txs: vec![[2; TRANSACTION_HASH_SIZE], [9; TRANSACTION_HASH_SIZE]],
            delayed_txs: vec![],
            cumulative_gas: U256::from(3),
            index: 4,
            parent_hash: H256::from([5; 32]),
            estimated_ticks_in_block: 99,
            estimated_ticks_in_run: 199,
            logs_bloom: Bloom::default(),
            logs_offset: 4,
            timestamp: Timestamp::from(0i64),
            base_fee_per_gas: U256::from(21000u64),
            cumulative_execution_gas: U256::from(1),
            cumulative_receipts: vec![],
            cumulative_tx_objects: vec![],
        };

        let encoded = bip.rlp_bytes();
        let expected =
            "f902302af8b1f871a00101010101010101010101010101010101010101010101010101010101010101f84e01b84bf84901010180018026a00101010101010101010101010101010101010101010101010101010101010101a00101010101010101010101010101010101010101010101010101010101010101f83ca00808080808080808080808080808080808080808080808080808080808080808da02d8089408080808080808080808080808080808080808080180f842a00202020202020202020202020202020202020202020202020202020202020202a00909090909090909090909090909090909090909090909090909090909090909c00304a0050505050505050505050505050505050505050505050505050505050505050563b90100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000488000000000000000082520801c0c0";

        pretty_assertions::assert_str_eq!(hex::encode(encoded), expected);

        let bytes = hex::decode(expected).expect("Should be valid hex string");
        let decoder = Rlp::new(&bytes);
        let decoded =
            EthBlockInProgress::decode(&decoder).expect("Should have decoded data");

        // the estimated ticks in the run are not stored
        let fresh_bip = EthBlockInProgress {
            estimated_ticks_in_run: 0,
            ..bip
        };
        assert_eq!(decoded, fresh_bip);
    }
}
