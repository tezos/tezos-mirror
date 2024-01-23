// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>

use crate::{
    inbox::{Deposit, Transaction, TransactionContent},
    linked_list::LinkedList,
};
use anyhow::Result;
use rlp::{Decodable, DecoderError, Encodable};
use tezos_ethereum::{
    rlp_helpers, transaction::TRANSACTION_HASH_SIZE, tx_common::EthereumTransactionCommon,
};
use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup_encoding::timestamp::Timestamp;
use tezos_smart_rollup_host::{path::RefPath, runtime::Runtime};

pub struct DelayedInbox(LinkedList<Hash, DelayedInboxItem>);

pub const DELAYED_INBOX_PATH: RefPath = RefPath::assert_from(b"/delayed-inbox");

// Tag that indicates the delayed transaction is a eth transaction.
pub const DELAYED_TRANSACTION_TAG: u8 = 0x01;

// Tag that indicates the delayed transaction is a deposit.
pub const DELAYED_DEPOSIT_TAG: u8 = 0x02;

/// Hash of a transaction
///
/// It represents the key of the transaction in the delayed inbox.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Hash(pub [u8; TRANSACTION_HASH_SIZE]);

impl Encodable for Hash {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        s.encoder().encode_value(&self.0);
    }
}

impl Decodable for Hash {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, rlp::DecoderError> {
        let hash: Vec<u8> = decoder.as_val()?;
        let hash = hash
            .try_into()
            .map_err(|_| DecoderError::Custom("expected a vec of 32 elements"))?;
        Ok(Hash(hash))
    }
}

impl AsRef<[u8]> for Hash {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

/// Delayed transaction
#[allow(clippy::large_enum_variant)]
#[derive(Clone)]
pub enum DelayedTransaction {
    Ethereum(EthereumTransactionCommon),
    Deposit(Deposit),
}

impl Encodable for DelayedTransaction {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(2);
        match self {
            DelayedTransaction::Ethereum(delayed_tx) => {
                stream.append(&DELAYED_TRANSACTION_TAG);
                stream.append(&delayed_tx.to_bytes());
            }
            DelayedTransaction::Deposit(delayed_deposit) => {
                stream.append(&DELAYED_DEPOSIT_TAG);
                stream.append(delayed_deposit);
            }
        }
    }
}

impl Decodable for DelayedTransaction {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 2 {
            return Err(DecoderError::RlpIncorrectListLen);
        }
        let tag: u8 = decoder.at(0)?.as_val()?;
        let payload = decoder.at(1)?;
        match tag {
            DELAYED_TRANSACTION_TAG => {
                let payload: Vec<u8> = payload.as_val()?;
                let delayed_tx = EthereumTransactionCommon::from_bytes(&payload)?;
                Ok(Self::Ethereum(delayed_tx))
            }
            DELAYED_DEPOSIT_TAG => {
                let deposit = Deposit::decode(&payload)?;
                Ok(DelayedTransaction::Deposit(deposit))
            }
            _ => Err(DecoderError::Custom("unknown tag")),
        }
    }
}

// Elements in the delayed inbox
#[derive(Clone)]
pub struct DelayedInboxItem {
    transaction: DelayedTransaction,
    timestamp: Timestamp,
}

impl Encodable for DelayedInboxItem {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(2);
        stream.append(&self.transaction);
        rlp_helpers::append_timestamp(stream, self.timestamp);
    }
}

impl Decodable for DelayedInboxItem {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != 2 {
            return Err(DecoderError::RlpIncorrectListLen);
        }
        let mut it = decoder.iter();
        let transaction =
            rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "transaction")?;
        let timestamp = rlp_helpers::decode_timestamp(&rlp_helpers::next(&mut it)?)?;
        Ok(Self {
            transaction,
            timestamp,
        })
    }
}

impl DelayedInbox {
    pub fn new<Host: Runtime>(host: &mut Host) -> Result<Self> {
        let linked_list = LinkedList::new(&DELAYED_INBOX_PATH, host)?;
        Ok(Self(linked_list))
    }

    pub fn save_transaction<Host: Runtime>(
        &mut self,
        host: &mut Host,
        tx: Transaction,
        timestamp: Timestamp,
    ) -> Result<()> {
        let Transaction { tx_hash, content } = tx;
        let transaction = match content {
            TransactionContent::Ethereum(tx) => DelayedTransaction::Ethereum(tx),
            TransactionContent::Deposit(deposit) => DelayedTransaction::Deposit(deposit),
        };
        let item = DelayedInboxItem {
            transaction,
            timestamp,
        };
        self.0.push(host, &Hash(tx_hash), &item)?;
        log!(
            host,
            Info,
            "Saved transaction {} in the delayed inbox",
            hex::encode(tx_hash)
        );
        Ok(())
    }

    pub fn find_and_remove_transaction<Host: Runtime>(
        &mut self,
        host: &mut Host,
        tx_hash: Hash,
    ) -> Result<Option<(Transaction, Timestamp)>> {
        log!(
            host,
            Info,
            "Removing transaction {} from the delayed inbox",
            hex::encode(tx_hash)
        );
        let tx = self.0.remove(host, &tx_hash)?.map(
            |DelayedInboxItem {
                 transaction,
                 timestamp,
             }| match transaction {
                DelayedTransaction::Ethereum(tx) => (
                    Transaction {
                        tx_hash: tx_hash.0,
                        content: TransactionContent::Ethereum(tx),
                    },
                    timestamp,
                ),
                DelayedTransaction::Deposit(deposit) => (
                    Transaction {
                        tx_hash: tx_hash.0,
                        content: TransactionContent::Deposit(deposit),
                    },
                    timestamp,
                ),
            },
        );

        Ok(tx)
    }
}

#[cfg(test)]
mod tests {
    use super::DelayedInbox;
    use super::Hash;
    use crate::current_timestamp;
    use crate::inbox::Transaction;
    use primitive_types::{H160, U256};
    use tezos_smart_rollup_encoding::timestamp::Timestamp;

    use crate::inbox::TransactionContent::Ethereum;
    use tezos_ethereum::{
        transaction::TRANSACTION_HASH_SIZE, tx_common::EthereumTransactionCommon,
    };

    use tezos_smart_rollup_mock::MockHost;

    fn address_from_str(s: &str) -> Option<H160> {
        let data = &hex::decode(s).unwrap();
        Some(H160::from_slice(data))
    }

    fn tx_(i: u64) -> EthereumTransactionCommon {
        EthereumTransactionCommon::new(
            tezos_ethereum::transaction::TransactionType::Legacy,
            Some(U256::one()),
            U256::from(i),
            U256::from(40000000u64),
            U256::from(40000000u64),
            21000u64,
            address_from_str("423163e58aabec5daa3dd1130b759d24bef0f6ea"),
            U256::from(500000000u64),
            vec![],
            vec![],
            None,
        )
    }

    fn dummy_transaction(i: u8) -> Transaction {
        Transaction {
            tx_hash: [i; TRANSACTION_HASH_SIZE],
            content: Ethereum(tx_(i.into())),
        }
    }

    #[test]
    fn test_delayed_inbox_roundtrip() {
        let mut host = MockHost::default();
        let mut delayed_inbox =
            DelayedInbox::new(&mut host).expect("Delayed inbox should be created");

        let tx: Transaction = dummy_transaction(0);
        let timestamp: Timestamp = current_timestamp(&mut host);
        delayed_inbox
            .save_transaction(&mut host, tx.clone(), timestamp)
            .expect("Tx should be saved in the delayed inbox");

        let mut delayed_inbox =
            DelayedInbox::new(&mut host).expect("Delayed inbox should exist");

        let read = delayed_inbox
            .find_and_remove_transaction(&mut host, Hash(tx.tx_hash))
            .expect("Reading from the delayed inbox should work")
            .expect("Transaction should be in the delayed inbox");
        assert_eq!((tx, timestamp), read)
    }
}
