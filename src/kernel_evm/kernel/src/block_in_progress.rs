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
use primitive_types::{H256, U256};
use std::collections::VecDeque;
use tezos_ethereum::block::L2Block;
use tezos_ethereum::transaction::*;
use tezos_smart_rollup_host::runtime::Runtime;

/// Container for all data needed during block computation
pub struct BlockInProgress {
    /// block number
    pub number: U256,
    /// queue containing the transactions to execute
    tx_queue: VecDeque<Transaction>,
    /// list of transactions executed without issue
    valid_txs: Vec<[u8; 32]>,
    /// gas accumulator
    pub cumulative_gas: U256,
    /// index for next transaction
    pub index: u32,
    /// gas price for transactions in the block being created
    pub gas_price: U256,
    /// hash to use for receipt
    /// (computed from number, not the correct way to do it)
    pub hash: H256,
}

impl BlockInProgress {
    pub fn new(
        number: U256,
        gas_price: U256,
        transactions: VecDeque<Transaction>,
    ) -> Result<BlockInProgress, Error> {
        let block_in_progress = BlockInProgress {
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
        };

        Ok(block_in_progress)
    }

    pub fn register_transaction(
        &mut self,
        tx_hash: [u8; 32],
        gas_used: U256,
    ) -> Result<(), Error> {
        self.cumulative_gas = self
            .cumulative_gas
            .checked_add(gas_used)
            .ok_or(Error::Transfer(CumulativeGasUsedOverflow))?;
        self.valid_txs.push(tx_hash);
        self.index += 1;
        Ok(())
    }

    pub fn finalize_and_store<Host: Runtime>(
        self,
        host: &mut Host,
    ) -> Result<L2Block, Error> {
        let timestamp = current_timestamp(host);
        let new_block = L2Block::new(self.number, self.valid_txs, timestamp);
        storage::store_current_block(host, &new_block)?;
        Ok(new_block)
    }

    pub fn pop_tx(&mut self) -> Option<Transaction> {
        self.tx_queue.pop_front()
    }

    pub fn has_tx(&self) -> bool {
        !self.tx_queue.is_empty()
    }

    pub fn make_receipt(
        &self,
        receipt_info: TransactionReceiptInfo,
    ) -> Result<TransactionReceipt, Error> {
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

        let tx_receipt = match execution_outcome {
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
                type_: TransactionType::Legacy,
                status: TransactionStatus::Failure,
            },
        };

        Ok(tx_receipt)
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
            v: object_info.v,
            r: object_info.r,
            s: object_info.s,
        }
    }
}
