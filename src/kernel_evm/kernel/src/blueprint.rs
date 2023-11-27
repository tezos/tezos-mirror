// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use crate::block_in_progress::BlockInProgress;
use crate::inbox::{read_inbox, KernelUpgrade, Transaction, TransactionContent};
use crate::sequencer_blueprint::SequencerBlueprint;
use crate::tick_model::constants::MAX_TRANSACTION_GAS_LIMIT;
use crate::{current_timestamp, sequencer_blueprint};
use rlp::{Decodable, DecoderError, Encodable};
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_ethereum::rlp_helpers::{self, append_timestamp, decode_timestamp};

use tezos_smart_rollup_encoding::timestamp::Timestamp;
use tezos_smart_rollup_host::runtime::Runtime;

/// The blueprint of a block is a list of transactions.
#[derive(PartialEq, Debug, Clone)]
pub struct Blueprint {
    pub transactions: Vec<Transaction>,
    pub timestamp: Timestamp,
}

impl From<SequencerBlueprint> for Blueprint {
    fn from(seq_blueprint: SequencerBlueprint) -> Self {
        Self {
            transactions: seq_blueprint.transactions,
            timestamp: seq_blueprint.timestamp,
        }
    }
}

impl Encodable for Blueprint {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(2);
        stream.append_list(&self.transactions);
        append_timestamp(stream, self.timestamp);
    }
}

impl Decodable for Blueprint {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 2 {
            return Err(DecoderError::RlpIncorrectListLen);
        }

        let mut it = decoder.iter();
        let transactions =
            rlp_helpers::decode_list(&rlp_helpers::next(&mut it)?, "transactions")?;
        let timestamp = decode_timestamp(&rlp_helpers::next(&mut it)?)?;

        Ok(Blueprint {
            transactions,
            timestamp,
        })
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum QueueElement {
    Blueprint(Blueprint),
    BlockInProgress(Box<BlockInProgress>),
}

const BIP_QUEUEELT_TAG: u8 = 1;
const BLUEPRINT_QUEUEELT_TAG: u8 = 2;

impl Decodable for QueueElement {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, rlp::DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 2 {
            return Err(DecoderError::RlpIncorrectListLen);
        }
        let tag: u8 = decoder.at(0)?.as_val()?;
        let elt = decoder.at(1)?;
        match tag {
            BIP_QUEUEELT_TAG => {
                // block in progress
                let bip = BlockInProgress::decode(&elt)?;
                Ok(Self::BlockInProgress(Box::new(bip)))
            }
            BLUEPRINT_QUEUEELT_TAG => {
                // blueprint
                let bpt = Blueprint::decode(&elt)?;
                Ok(Self::Blueprint(bpt))
            }
            _ => Err(DecoderError::Custom("Unknown queue element tag.")),
        }
    }
}

impl Encodable for QueueElement {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(2);
        match self {
            QueueElement::Blueprint(bpt) => {
                stream.append(&BLUEPRINT_QUEUEELT_TAG);
                bpt.rlp_append(stream)
            }
            QueueElement::BlockInProgress(bip) => {
                stream.append(&BIP_QUEUEELT_TAG);
                bip.rlp_append(stream)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Queue {
    // In our case, to make it simple and straightforward it will be
    // an array of pendings transactions even though it'll be only a
    // singleton for our needs.
    pub proposals: Vec<QueueElement>,
    pub kernel_upgrade: Option<KernelUpgrade>,
}

impl Decodable for Queue {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, rlp::DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 2 {
            return Err(DecoderError::RlpIncorrectListLen);
        }

        let mut it = decoder.iter();
        let proposals: Vec<QueueElement> =
            rlp_helpers::decode_list(&rlp_helpers::next(&mut it)?, "proposals")?;
        let kernel_upgrade: Option<KernelUpgrade> =
            rlp_helpers::decode_option(&rlp_helpers::next(&mut it)?, "kernel_upgrade")?;
        Ok(Queue {
            proposals,
            kernel_upgrade,
        })
    }
}

impl Encodable for Queue {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(2);
        stream.append_list(&self.proposals);
        rlp_helpers::append_option(stream, &self.kernel_upgrade);
    }
}

fn filter_invalid_transactions(transactions: Vec<Transaction>) -> Vec<Transaction> {
    let filter_max_gas_limit = |transaction: &Transaction| match &transaction.content {
        TransactionContent::Deposit(_) => true,
        TransactionContent::Ethereum(transaction) => {
            transaction.gas_limit <= MAX_TRANSACTION_GAS_LIMIT
        }
    };

    transactions
        .into_iter()
        .filter(filter_max_gas_limit)
        .collect()
}

pub fn fetch_inbox_blueprints<Host: Runtime>(
    host: &mut Host,
    smart_rollup_address: [u8; 20],
    ticketer: Option<ContractKt1Hash>,
    admin: Option<ContractKt1Hash>,
) -> Result<Queue, anyhow::Error> {
    let inbox_content = read_inbox(host, smart_rollup_address, ticketer, admin)?;
    let transactions = filter_invalid_transactions(inbox_content.transactions);
    let timestamp = current_timestamp(host);
    let blueprint = QueueElement::Blueprint(Blueprint {
        transactions,
        timestamp,
    });
    Ok(Queue {
        proposals: vec![blueprint],
        kernel_upgrade: inbox_content.kernel_upgrade,
    })
}

fn fetch_sequencer_blueprints<Host: Runtime>(
    host: &mut Host,
) -> Result<Queue, anyhow::Error> {
    let seq_blueprints = sequencer_blueprint::fetch(host)?;
    let proposals = seq_blueprints
        .into_iter()
        .map(|sb| QueueElement::Blueprint(From::from(sb)))
        .collect();
    Ok(Queue {
        proposals,
        kernel_upgrade: None,
    })
}

pub fn fetch<Host: Runtime>(
    host: &mut Host,
    smart_rollup_address: [u8; 20],
    ticketer: Option<ContractKt1Hash>,
    admin: Option<ContractKt1Hash>,
    is_sequencer: bool,
) -> Result<Queue, anyhow::Error> {
    if is_sequencer {
        fetch_sequencer_blueprints(host)
    } else {
        fetch_inbox_blueprints(host, smart_rollup_address, ticketer, admin)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use super::*;
    use crate::inbox::TransactionContent::Ethereum;
    use primitive_types::{H160, H256, U256};
    use rlp::Rlp;
    use tezos_ethereum::{
        transaction::TRANSACTION_HASH_SIZE, tx_common::EthereumTransactionCommon,
    };
    use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;

    fn address_from_str(s: &str) -> Option<H160> {
        let data = &hex::decode(s).unwrap();
        Some(H160::from_slice(data))
    }
    fn tx_(i: u64) -> EthereumTransactionCommon {
        EthereumTransactionCommon {
            type_: tezos_ethereum::transaction::TransactionType::Legacy,
            chain_id: U256::one(),
            nonce: U256::from(i),
            max_priority_fee_per_gas: U256::from(40000000u64),
            max_fee_per_gas: U256::from(40000000u64),
            gas_limit: 21000u64,
            to: address_from_str("423163e58aabec5daa3dd1130b759d24bef0f6ea"),
            value: U256::from(500000000u64),
            data: vec![],
            access_list: vec![],
            signature: None,
        }
    }
    fn tx() -> EthereumTransactionCommon {
        tx_(40000000u64)
    }

    fn dummy_transaction(i: u8) -> Transaction {
        Transaction {
            tx_hash: [i; TRANSACTION_HASH_SIZE],
            content: Ethereum(tx_(i.into())),
        }
    }

    #[test]
    fn test_filter_large_gas_limit() {
        let valid_content = Ethereum(EthereumTransactionCommon {
            gas_limit: MAX_TRANSACTION_GAS_LIMIT,
            ..tx()
        });
        let valid_transaction = Transaction {
            tx_hash: [0; TRANSACTION_HASH_SIZE],
            content: valid_content,
        };

        let invalid_content = Ethereum(EthereumTransactionCommon {
            gas_limit: MAX_TRANSACTION_GAS_LIMIT + 1,
            ..tx()
        });
        let invalid_transaction = Transaction {
            tx_hash: [0; TRANSACTION_HASH_SIZE],
            content: invalid_content,
        };

        let filtered_transactions = filter_invalid_transactions(vec![
            valid_transaction.clone(),
            invalid_transaction,
        ]);
        assert_eq!(vec![valid_transaction], filtered_transactions)
    }

    #[test]
    fn test_encode_queue_elt() {
        let proposal = QueueElement::Blueprint(Blueprint {
            transactions: vec![dummy_transaction(0), dummy_transaction(1)],
            timestamp: Timestamp::from(0i64),
        });

        let encoded = proposal.rlp_bytes();
        let decoder = Rlp::new(&encoded);
        let decoded = QueueElement::decode(&decoder).expect("Should be decodable");
        assert_eq!(decoded, proposal);
    }

    fn dummy_bip(i: usize) -> BlockInProgress {
        BlockInProgress::new_with_ticks(
            U256::from(i),
            H256::zero(),
            U256::zero(),
            VecDeque::new(),
            0,
            Timestamp::from(0i64),
        )
    }

    #[test]
    fn test_encode_queue() {
        let proposal = QueueElement::Blueprint(Blueprint {
            transactions: vec![dummy_transaction(0), dummy_transaction(1)],
            timestamp: Timestamp::from(0i64),
        });

        let proposals = vec![
            QueueElement::BlockInProgress(Box::new(dummy_bip(2))),
            proposal,
        ];
        let kernel_upgrade = Some(KernelUpgrade {
            preimage_hash: [3; PREIMAGE_HASH_SIZE],
        });
        let queue = Queue {
            proposals,
            kernel_upgrade,
        };

        let encoded = queue.rlp_bytes();
        let decoder = Rlp::new(&encoded);
        let decoded = Queue::decode(&decoder).expect("Should be decodable");
        assert_eq!(decoded, queue);
    }
}
