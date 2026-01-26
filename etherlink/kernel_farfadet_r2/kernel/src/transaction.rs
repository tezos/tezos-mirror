// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023-2026 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use crate::bridge::Deposit;
use crate::fees::tx_execution_gas_limit;

use primitive_types::{H160, U256};
use revm_etherlink::helpers::legacy::{alloy_to_h160, FaDeposit};
use revm_etherlink::precompiles::constants::{
    FA_BRIDGE_SOL_ADDR, FA_DEPOSIT_QUEUE_GAS_LIMIT, XTZ_DEPOSIT_EXECUTION_COST,
};
use revm_etherlink::Error;
use rlp::{Decodable, DecoderError, Encodable};
use tezos_ethereum::block::BlockFees;
use tezos_ethereum::rlp_helpers::{self, decode_field, decode_tx_hash, next};
use tezos_ethereum::transaction::{TransactionHash, TransactionType};
use tezos_ethereum::tx_common::EthereumTransactionCommon;
use tezos_ethereum::tx_signature::TxSignature;

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Clone)]
pub enum TransactionContent {
    Ethereum(EthereumTransactionCommon),
    Deposit(Deposit),
    EthereumDelayed(EthereumTransactionCommon),
    FaDeposit(FaDeposit),
}

const ETHEREUM_TX_TAG: u8 = 1;
const DEPOSIT_TX_TAG: u8 = 2;
const ETHEREUM_DELAYED_TX_TAG: u8 = 3;
const FA_DEPOSIT_TX_TAG: u8 = 4;

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
            TransactionContent::EthereumDelayed(eth) => {
                stream.append(&ETHEREUM_DELAYED_TX_TAG);
                eth.rlp_append(stream)
            }
            TransactionContent::FaDeposit(fa_dep) => {
                stream.append(&FA_DEPOSIT_TX_TAG);
                fa_dep.rlp_append(stream)
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
            ETHEREUM_DELAYED_TX_TAG => {
                let eth = EthereumTransactionCommon::decode(&tx)?;
                Ok(Self::EthereumDelayed(eth))
            }
            FA_DEPOSIT_TX_TAG => {
                let fa_deposit = FaDeposit::decode(&tx)?;
                Ok(Self::FaDeposit(fa_deposit))
            }
            _ => Err(DecoderError::Custom("Unknown transaction tag.")),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Transactions {
    EthTxs(Vec<Transaction>),
}

impl Transactions {
    pub fn push(&mut self, tx: Transaction) {
        match self {
            Self::EthTxs(transactions) => transactions.push(tx),
        }
    }
}

impl Encodable for Transactions {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        match self {
            Self::EthTxs(transactions) => {
                s.append_list(transactions);
            }
        }
    }
}

impl Decodable for Transactions {
    fn decode(rlp: &rlp::Rlp) -> Result<Self, DecoderError> {
        let transactions = rlp_helpers::decode_list(rlp, "transactions")?;
        Ok(Self::EthTxs(transactions))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Transaction {
    pub tx_hash: TransactionHash,
    pub content: TransactionContent,
}

impl Transaction {
    pub fn data_size(&self) -> u64 {
        match &self.content {
            TransactionContent::Deposit(_) => 0,
            TransactionContent::Ethereum(e) | TransactionContent::EthereumDelayed(e) => {
                // FIXME: probably need to take into account the access list
                e.data.len() as u64
            }
            TransactionContent::FaDeposit(_) => 0,
        }
    }

    pub fn is_delayed(&self) -> bool {
        match &self.content {
            TransactionContent::Deposit(_)
            | TransactionContent::EthereumDelayed(_)
            | TransactionContent::FaDeposit(_) => true,
            TransactionContent::Ethereum(_) => false,
        }
    }

    pub fn execution_gas_limit(&self, fees: &BlockFees) -> Result<u64, Error> {
        match &self.content {
            TransactionContent::Deposit(_) => Ok(XTZ_DEPOSIT_EXECUTION_COST),
            TransactionContent::Ethereum(e) => tx_execution_gas_limit(e, fees, false),
            TransactionContent::EthereumDelayed(e) => {
                tx_execution_gas_limit(e, fees, true)
            }
            TransactionContent::FaDeposit(_) => Ok(FA_DEPOSIT_QUEUE_GAS_LIMIT),
        }
    }

    pub fn to(&self) -> Result<Option<H160>, Error> {
        Ok(match &self.content {
            TransactionContent::Deposit(Deposit { receiver, .. }) => {
                let receiver = receiver.to_h160().map_err(|_| {
                    Error::Custom("Can't convert deposit receiver".to_owned())
                })?;
                Some(receiver)
            }
            TransactionContent::FaDeposit(FaDeposit { .. }) => {
                Some(alloy_to_h160(&FA_BRIDGE_SOL_ADDR))
            }
            TransactionContent::Ethereum(transaction)
            | TransactionContent::EthereumDelayed(transaction) => transaction.to,
        })
    }

    pub fn data(&self) -> Vec<u8> {
        match &self.content {
            TransactionContent::Deposit(_) | TransactionContent::FaDeposit(_) => vec![],
            TransactionContent::Ethereum(transaction)
            | TransactionContent::EthereumDelayed(transaction) => {
                transaction.data.clone()
            }
        }
    }

    pub fn value(&self) -> U256 {
        match &self.content {
            TransactionContent::Deposit(Deposit { amount, .. }) => *amount,
            &TransactionContent::FaDeposit(_) => U256::zero(),
            TransactionContent::Ethereum(transaction)
            | TransactionContent::EthereumDelayed(transaction) => transaction.value,
        }
    }

    pub fn nonce(&self) -> u64 {
        match &self.content {
            TransactionContent::Deposit(_) | TransactionContent::FaDeposit(_) => 0,
            TransactionContent::Ethereum(transaction)
            | TransactionContent::EthereumDelayed(transaction) => transaction.nonce,
        }
    }

    pub fn signature(&self) -> Option<TxSignature> {
        match &self.content {
            TransactionContent::Deposit(_) | TransactionContent::FaDeposit(_) => None,
            TransactionContent::Ethereum(transaction)
            | TransactionContent::EthereumDelayed(transaction) => {
                transaction.signature.clone()
            }
        }
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

impl Transaction {
    pub fn type_(&self) -> TransactionType {
        match &self.content {
            // The deposit is considered arbitrarily as a legacy transaction
            TransactionContent::Deposit(_) | TransactionContent::FaDeposit(_) => {
                TransactionType::Legacy
            }
            TransactionContent::Ethereum(tx)
            | TransactionContent::EthereumDelayed(tx) => tx.type_,
        }
    }
}
