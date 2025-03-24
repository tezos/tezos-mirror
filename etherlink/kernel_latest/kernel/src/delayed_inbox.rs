// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>

use crate::{
    bridge::Deposit,
    event::Event,
    linked_list::LinkedList,
    storage::{self, read_last_info_per_level_timestamp},
    transaction::{Transaction, TransactionContent},
};
use anyhow::Result;
use evm_execution::fa_bridge::deposit::FaDeposit;
use rlp::{Decodable, DecoderError, Encodable};
use tezos_ethereum::{
    rlp_helpers,
    transaction::{TransactionHash, TRANSACTION_HASH_SIZE},
    tx_common::EthereumTransactionCommon,
};
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_encoding::timestamp::Timestamp;
use tezos_smart_rollup_host::path::RefPath;
use tezos_storage::read_u16_le_default;

pub struct DelayedInbox(LinkedList<Hash, DelayedInboxItem>);

pub const DELAYED_INBOX_PATH: RefPath = RefPath::assert_from(b"/evm/delayed-inbox");

// Maximum number of transaction included in a blueprint when
// forcing timed-out transactions from the delayed inbox.
pub const DEFAULT_MAX_DELAYED_INBOX_BLUEPRINT_LENGTH: u16 = 1000;

// Path to override the default value.
pub const MAX_DELAYED_INBOX_BLUEPRINT_LENGTH_PATH: RefPath =
    RefPath::assert_from(b"/evm/max_delayed_inbox_blueprint_length");

// Tag that indicates the delayed transaction is a eth transaction.
pub const DELAYED_TRANSACTION_TAG: u8 = 0x01;

// Tag that indicates the delayed transaction is a deposit.
pub const DELAYED_DEPOSIT_TAG: u8 = 0x02;

// Tag that indicates the delayed transaction is a FA deposit.
pub const DELAYED_FA_DEPOSIT_TAG: u8 = 0x03;

/// Hash of a transaction
///
/// It represents the key of the transaction in the delayed inbox.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Hash(pub [u8; TRANSACTION_HASH_SIZE]);

impl From<TransactionHash> for Hash {
    fn from(v: TransactionHash) -> Self {
        Self(v)
    }
}

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
    FaDeposit(FaDeposit),
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
            DelayedTransaction::FaDeposit(delayed_fa_deposit) => {
                stream.append(&DELAYED_FA_DEPOSIT_TAG);
                stream.append(delayed_fa_deposit);
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
            DELAYED_FA_DEPOSIT_TAG => {
                let fa_deposit = FaDeposit::decode(&payload)?;
                Ok(DelayedTransaction::FaDeposit(fa_deposit))
            }
            _ => Err(DecoderError::Custom("unknown tag")),
        }
    }
}

// Elements in the delayed inbox
#[derive(Clone)]
pub struct DelayedInboxItem {
    pub transaction: DelayedTransaction,
    timestamp: Timestamp,
    level: u32,
}

impl DelayedInboxItem {
    fn list_size() -> usize {
        3
    }
}

impl Encodable for DelayedInboxItem {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(Self::list_size());
        stream.append(&self.transaction);
        rlp_helpers::append_timestamp(stream, self.timestamp);
        rlp_helpers::append_u32_le(stream, &self.level);
    }
}

impl Decodable for DelayedInboxItem {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if decoder.item_count()? != Self::list_size() {
            return Err(DecoderError::RlpIncorrectListLen);
        }
        let mut it = decoder.iter();
        let transaction =
            rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "transaction")?;
        let timestamp = rlp_helpers::decode_timestamp(&rlp_helpers::next(&mut it)?)?;
        let level =
            rlp_helpers::decode_field_u32_le(&rlp_helpers::next(&mut it)?, "level")?;
        Ok(Self {
            transaction,
            timestamp,
            level,
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
        level: u32,
    ) -> Result<()> {
        let Transaction { tx_hash, content } = tx.clone();
        let transaction = match content {
            TransactionContent::Ethereum(_) => anyhow::bail!("Non-delayed evm transaction should not be saved to the delayed inbox. {:?}", tx.tx_hash),
            TransactionContent::EthereumDelayed(tx) => DelayedTransaction::Ethereum(tx),
            TransactionContent::Deposit(deposit) => DelayedTransaction::Deposit(deposit),
            TransactionContent::FaDeposit(fa_deposit) => DelayedTransaction::FaDeposit(fa_deposit)
        };
        let item = DelayedInboxItem {
            transaction,
            timestamp,
            level,
        };

        Event::NewDelayedTransaction(Box::new(tx)).store(host)?;

        self.0.push(host, &Hash(tx_hash), &item)?;
        log!(
            host,
            Info,
            "Saved transaction {} in the delayed inbox",
            hex::encode(tx_hash)
        );
        Ok(())
    }

    pub fn transaction_from_delayed(
        tx_hash: Hash,
        delayed: DelayedTransaction,
    ) -> Transaction {
        match delayed {
            DelayedTransaction::Ethereum(tx) => Transaction {
                tx_hash: tx_hash.0,
                content: TransactionContent::EthereumDelayed(tx),
            },
            DelayedTransaction::Deposit(deposit) => Transaction {
                tx_hash: tx_hash.0,
                content: TransactionContent::Deposit(deposit),
            },
            DelayedTransaction::FaDeposit(fa_deposit) => Transaction {
                tx_hash: tx_hash.0,
                content: TransactionContent::FaDeposit(fa_deposit),
            },
        }
    }

    pub fn find_transaction<Host: Runtime>(
        &mut self,
        host: &mut Host,
        tx_hash: Hash,
    ) -> Result<Option<(Transaction, Timestamp)>> {
        let tx = self.0.find(host, &tx_hash)?.map(
            |DelayedInboxItem {
                 transaction,
                 timestamp,
                 level: _,
             }| {
                (
                    Self::transaction_from_delayed(tx_hash, transaction),
                    timestamp,
                )
            },
        );

        Ok(tx)
    }

    // Returns the oldest tx in the delayed inbox (and its hash) if it
    // timed out
    fn first_if_timed_out<Host: Runtime>(
        &mut self,
        host: &mut Host,
        now: Timestamp,
        timeout: u64,
        current_level: u32,
        min_levels: u32,
    ) -> Result<Option<(Hash, DelayedTransaction)>> {
        let to_pop = self.0.first_with_id(host)?.and_then(
            |(
                tx_hash,
                DelayedInboxItem {
                    transaction,
                    timestamp,
                    level,
                },
            )| {
                if now.as_u64() - timestamp.as_u64() >= timeout
                    && current_level - level >= min_levels
                {
                    log!(
                        host,
                        Info,
                        "Delayed transaction {} timed out",
                        hex::encode(tx_hash)
                    );
                    Some((tx_hash, transaction))
                } else {
                    None
                }
            },
        );
        Ok(to_pop)
    }

    #[cfg(test)]
    pub fn is_empty<Host: Runtime>(&self, host: &mut Host) -> Result<bool> {
        let first = self.0.first_with_id(host)?;
        Ok(first.is_none())
    }

    fn pop_first<Host: Runtime>(
        &mut self,
        host: &mut Host,
    ) -> Result<Option<Transaction>> {
        let to_pop = self.0.first_with_id(host)?;
        match to_pop {
            None => Ok(None),
            Some((hash, delayed)) => {
                let _ = self.0.remove(host, &hash)?;
                let transaction =
                    Self::transaction_from_delayed(hash, delayed.transaction);
                Ok(Some(transaction))
            }
        }
    }

    /// Returns whether the oldest tx in the delayed inbox has timed out.
    pub fn first_has_timed_out<Host: Runtime>(
        &mut self,
        host: &mut Host,
    ) -> Result<bool> {
        let now = read_last_info_per_level_timestamp(host)?;
        let timeout = storage::delayed_inbox_timeout(host)?;
        let current_level = storage::read_l1_level(host)?;
        let min_levels = storage::delayed_inbox_min_levels(host)?;
        let popped =
            self.first_if_timed_out(host, now, timeout, current_level, min_levels)?;
        Ok(popped.is_some())
    }

    /// Computes the next vector of timed-out delayed transactions.
    /// If there are no timed-out transactions, None is returned to
    /// signal that we're done.
    /// Note that this function assumes we're on a "timeout" state,
    /// which should be checked before calling it.
    pub fn next_delayed_inbox_blueprint<Host: Runtime>(
        &mut self,
        host: &mut Host,
    ) -> Result<Option<Vec<Transaction>>> {
        let max_delayed_inbox_blueprint_length = read_u16_le_default(
            host,
            &MAX_DELAYED_INBOX_BLUEPRINT_LENGTH_PATH,
            DEFAULT_MAX_DELAYED_INBOX_BLUEPRINT_LENGTH,
        )?;
        let mut popped: Vec<Transaction> = vec![];
        while let Some(tx) = self.pop_first(host)? {
            popped.push(tx);
            // Check if the number of transactions has reached the limit per
            // blueprint
            if popped.len() as u16 >= max_delayed_inbox_blueprint_length {
                break;
            }
        }
        Ok(if popped.is_empty() {
            None
        } else {
            Some(popped)
        })
    }

    /// Deletes a transaction from the delayed inbox. It does not check if
    /// a transaction is removed or not. The only property ensured by the
    /// function is that the transaction is not part of the delayed inbox
    /// after the call.
    pub fn delete<Host: Runtime>(
        &mut self,
        host: &mut Host,
        tx_hash: Hash,
    ) -> Result<()> {
        log!(
            host,
            Info,
            "Removing transaction {} from the delayed inbox",
            hex::encode(tx_hash)
        );
        let _found = self.0.remove(host, &tx_hash)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::DelayedInbox;
    use super::Hash;
    use crate::storage::read_last_info_per_level_timestamp;
    use crate::transaction::Transaction;
    use primitive_types::{H160, U256};
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_encoding::timestamp::Timestamp;

    use crate::transaction::TransactionContent::{Ethereum, EthereumDelayed};
    use tezos_ethereum::{
        transaction::TRANSACTION_HASH_SIZE, tx_common::EthereumTransactionCommon,
    };

    fn address_from_str(s: &str) -> Option<H160> {
        let data = &hex::decode(s).unwrap();
        Some(H160::from_slice(data))
    }

    fn tx_(i: u64) -> EthereumTransactionCommon {
        EthereumTransactionCommon::new(
            tezos_ethereum::transaction::TransactionType::Legacy,
            Some(U256::one()),
            i,
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
            content: EthereumDelayed(tx_(i.into())),
        }
    }

    #[test]
    fn test_delayed_inbox_roundtrip() {
        let mut host = MockKernelHost::default();
        let mut delayed_inbox =
            DelayedInbox::new(&mut host).expect("Delayed inbox should be created");

        let tx: Transaction = dummy_transaction(0);

        let timestamp: Timestamp =
            read_last_info_per_level_timestamp(&host).unwrap_or(Timestamp::from(0));
        delayed_inbox
            .save_transaction(&mut host, tx.clone(), timestamp, 0)
            .expect("Tx should be saved in the delayed inbox");

        let mut delayed_inbox =
            DelayedInbox::new(&mut host).expect("Delayed inbox should exist");

        let read = delayed_inbox
            .find_transaction(&mut host, Hash(tx.tx_hash))
            .expect("Reading from the delayed inbox should work")
            .expect("Transaction should be in the delayed inbox");
        assert_eq!((tx, timestamp), read)
    }

    #[test]
    fn test_delayed_inbox_roundtrip_error_non_delayed() {
        let mut host = MockKernelHost::default();
        let mut delayed_inbox =
            DelayedInbox::new(&mut host).expect("Delayed inbox should be created");

        let tx: Transaction = Transaction {
            tx_hash: [12; TRANSACTION_HASH_SIZE],
            content: Ethereum(tx_(12)),
        };

        let timestamp: Timestamp =
            read_last_info_per_level_timestamp(&host).unwrap_or(Timestamp::from(0));
        let res = delayed_inbox.save_transaction(&mut host, tx, timestamp, 0);

        assert!(res.is_err());
    }
}
