// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>

use crate::{
    bridge::Deposit,
    chains::TezosXTransaction,
    configuration::{CommonConfig, EVM_CHAIN_ID},
    event::Event,
    linked_list::LinkedList,
    storage::{self, read_last_info_per_level_timestamp},
    transaction::{Transaction, TransactionContent},
};
use anyhow::Result;
use primitive_types::{H256, U256};
use revm_etherlink::helpers::legacy::FaDeposit;
use rlp::{Decodable, DecoderError, Encodable};
use tezos_ethereum::{
    rlp_helpers,
    transaction::{TransactionHash, TRANSACTION_HASH_SIZE},
    tx_common::EthereumTransactionCommon,
};
use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup_encoding::timestamp::Timestamp;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_keyspace::extensions::KeySpaceExtNum;
use tezos_smart_rollup_keyspace::{Key, KeySpace};

use tezos_tezlink::operation::Operation;

pub struct DelayedInbox(LinkedList<Hash, DelayedInboxItem>);

/// Relative key of the delayed inbox inside the `/base` keyspace. It resolves
/// to the historical absolute path `/base/delayed-inbox`, so an existing
/// on-chain delayed inbox keeps being read after the upgrade.
pub const DELAYED_INBOX_KEY: Key = Key::from_static(b"/delayed-inbox");

// Maximum number of transaction included in a blueprint when
// forcing timed-out transactions from the delayed inbox.
pub const DEFAULT_MAX_DELAYED_INBOX_BLUEPRINT_LENGTH: u16 = 1000;

// Key to override the default value, inside the `/base` keyspace. Resolves to
// the durable path `/base/max_delayed_inbox_blueprint_length`.
const MAX_DELAYED_INBOX_BLUEPRINT_LENGTH_KEY: Key =
    Key::from_static(b"/max_delayed_inbox_blueprint_length");

/// Temporary floor on the delayed inbox timeout: until
/// [DELAYED_INBOX_TIMEOUT_FLOOR_SUNSET], delayed transactions are only
/// force-included once they are at least two weeks old, regardless of the
/// timeout configured at `/base/delayed_inbox_timeout`. The `--force` flush
/// of the EVM node is unaffected. Once the sunset has passed on mainnet,
/// this constant and the flooring in [DelayedInbox::first_has_timed_out]
/// can be removed.
///
/// [EVM_CHAIN_ID], the sandbox chain the test suites run on, is exempt so
/// tests keep exercising the configured timeout.
pub const DELAYED_INBOX_TIMEOUT_FLOOR: u64 = 14 * 24 * 60 * 60;

/// End of the [DELAYED_INBOX_TIMEOUT_FLOOR] mitigation window:
/// 2026-11-27T00:00:00Z, three months after the mitigation was written.
pub const DELAYED_INBOX_TIMEOUT_FLOOR_SUNSET: u64 = 1_795_737_600;

// Tag that indicates the delayed transaction is a eth transaction.
pub const DELAYED_TRANSACTION_TAG: u8 = 0x01;

// Tag that indicates the delayed transaction is a deposit.
pub const DELAYED_DEPOSIT_TAG: u8 = 0x02;

// Tag that indicates the delayed transaction is a FA deposit.
pub const DELAYED_FA_DEPOSIT_TAG: u8 = 0x03;

// Tag that indicates the delayed transaction is a Tezos operation.
pub const DELAYED_TEZOS_OPERATION_TAG: u8 = 0x04;

/// Hash of a transaction
///
/// It represents the key of the transaction in the delayed inbox.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
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
    Tezos(Operation),
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
            DelayedTransaction::Tezos(op) => {
                stream.append(&DELAYED_TEZOS_OPERATION_TAG);
                stream.append(op);
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
            DELAYED_TEZOS_OPERATION_TAG => {
                let op = Operation::decode(&payload)?;
                Ok(DelayedTransaction::Tezos(op))
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
    /// Construct from an already-live `/base` handle.
    pub fn from_base(base: &impl KeySpace) -> Result<Self> {
        let linked_list = LinkedList::new(&DELAYED_INBOX_KEY, base)?;
        Ok(Self(linked_list))
    }

    pub fn save_transaction<Host>(
        &mut self,
        host: &Host,
        base: &mut impl KeySpace,
        tx: TezosXTransaction,
        timestamp: Timestamp,
        level: u32,
        common: &CommonConfig,
    ) -> Result<()>
    where
        Host: StorageV1,
    {
        match tx {
            TezosXTransaction::Ethereum(tx) => {
                let Transaction { tx_hash, content } = *tx.clone();
                // Validate the branch at delayed-inbox entry; drop foreign/stale branches (cross-instance replay).
                if let TransactionContent::TezosDelayed(op) = &content {
                    if !crate::chains::is_valid_tez_branch(host, &H256(*op.branch))? {
                        log!(
                            Error,
                            "Dropping delayed Tezos operation {}: branch {} is not a recent block of this instance",
                            hex::encode(tx_hash),
                            hex::encode(*op.branch)
                        );
                        return Ok(());
                    }
                }
                let transaction = match content {
            TransactionContent::Ethereum(_) => anyhow::bail!("Non-delayed evm transaction should not be saved to the delayed inbox. {:?}", tx.tx_hash),
            TransactionContent::EthereumDelayed(tx) => DelayedTransaction::Ethereum(tx),
            TransactionContent::Deposit(deposit) => DelayedTransaction::Deposit(deposit),
            TransactionContent::FaDeposit(fa_deposit) => DelayedTransaction::FaDeposit(fa_deposit),
            TransactionContent::TezosDelayed(op) => DelayedTransaction::Tezos(op),
        };
                let item = DelayedInboxItem {
                    transaction,
                    timestamp,
                    level,
                };

                if self.0.push(base, &Hash(tx_hash), &item)? {
                    Event::NewDelayedTransaction(tx).store(base, common)?;
                    log!(
                        Info,
                        "Saved transaction {} in the delayed inbox",
                        hex::encode(tx_hash)
                    );
                } else {
                    log!(
                        Info,
                        "Transaction {} was already present in the delayed inbox",
                        hex::encode(tx_hash)
                    );
                }
                Ok(())
            }
            TezosXTransaction::Tezos(op) => {
                anyhow::bail!("Non-delayed Tezos operation should not be saved to the delayed inbox. {:?}", op.tx_hash)
            }
        }
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
            DelayedTransaction::Tezos(op) => Transaction {
                tx_hash: tx_hash.0,
                content: TransactionContent::TezosDelayed(op),
            },
        }
    }

    pub fn find_transaction(
        &self,
        base: &impl KeySpace,
        tx_hash: Hash,
    ) -> Result<Option<(Transaction, Timestamp)>> {
        let tx = self.0.find(base, &tx_hash)?.map(
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
    fn first_if_timed_out(
        &mut self,
        base: &impl KeySpace,
        now: Timestamp,
        timeout: u64,
        current_level: u32,
        min_levels: u32,
    ) -> Result<Option<(Hash, DelayedTransaction)>> {
        let to_pop = self.0.first_with_id(base)?.and_then(
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
    pub fn is_empty(&self, base: &impl KeySpace) -> Result<bool> {
        let first = self.0.first_with_id(base)?;
        Ok(first.is_none())
    }

    fn pop_first(&mut self, base: &mut impl KeySpace) -> Result<Option<Transaction>> {
        let to_pop = self.0.first_with_id(base)?;
        match to_pop {
            None => Ok(None),
            Some((hash, delayed)) => {
                let _ = self.0.remove(base, &hash)?;
                let transaction =
                    Self::transaction_from_delayed(hash, delayed.transaction);
                Ok(Some(transaction))
            }
        }
    }

    /// Returns whether the oldest tx in the delayed inbox has timed out.
    pub fn first_has_timed_out(
        &mut self,
        base: &impl KeySpace,
        evm_chain_id: U256,
    ) -> Result<bool> {
        // Every read below resolves against the threaded `/base` handle.
        let now = read_last_info_per_level_timestamp(base)?;
        let configured_timeout = storage::delayed_inbox_timeout(base)?;
        // Apply the two-week floor until the sunset, except on the sandbox chain.
        let floored = evm_chain_id != U256::from(EVM_CHAIN_ID)
            && now.as_u64() < DELAYED_INBOX_TIMEOUT_FLOOR_SUNSET;
        let timeout = if floored {
            u64::max(configured_timeout, DELAYED_INBOX_TIMEOUT_FLOOR)
        } else {
            configured_timeout
        };
        let current_level = storage::read_l1_level(base)?;
        let min_levels = storage::delayed_inbox_min_levels(base)?;
        let popped =
            self.first_if_timed_out(base, now, timeout, current_level, min_levels)?;
        Ok(popped.is_some())
    }

    /// Computes the next vector of timed-out delayed transactions.
    /// If there are no timed-out transactions, None is returned to
    /// signal that we're done.
    /// Note that this function assumes we're on a "timeout" state,
    /// which should be checked before calling it.
    pub fn next_delayed_inbox_blueprint(
        &mut self,
        base: &mut impl KeySpace,
    ) -> Result<Option<Vec<Transaction>>> {
        // Read the override plus pop the whole batch against the threaded
        // `/base` handle.
        let max_delayed_inbox_blueprint_length: u16 = base.get_le_or(
            &MAX_DELAYED_INBOX_BLUEPRINT_LENGTH_KEY,
            DEFAULT_MAX_DELAYED_INBOX_BLUEPRINT_LENGTH,
        );
        let mut popped: Vec<Transaction> = vec![];
        while let Some(tx) = self.pop_first(base)? {
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
    pub fn delete(&mut self, base: &mut impl KeySpace, tx_hash: Hash) -> Result<()> {
        log!(
            Info,
            "Removing transaction {} from the delayed inbox",
            hex::encode(tx_hash)
        );
        let _found = self.0.remove(base, &tx_hash)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::DelayedInbox;
    use super::DelayedTransaction;
    use super::Hash;
    use super::DELAYED_INBOX_TIMEOUT_FLOOR_SUNSET;
    use crate::block_storage;
    use crate::chains::make_test_operation;
    use crate::configuration::{CommonConfig, EVM_CHAIN_ID};
    use crate::load_base;
    use crate::storage::read_last_info_per_level_timestamp;
    use crate::transaction::Transaction;
    use chrono::DateTime;
    use primitive_types::{H160, H256, U256};
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_evm_runtime::runtime_keyspaces::{
        RuntimeKeyspaces, TEZ_WORLD_STATE_ROOT_PATH,
    };
    use tezos_smart_rollup_encoding::timestamp::Timestamp;

    use crate::transaction::TransactionContent::{
        Ethereum, EthereumDelayed, TezosDelayed,
    };
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
    fn test_delayed_tezos_operation_rlp_roundtrip() {
        let op = make_test_operation();
        let delayed = DelayedTransaction::Tezos(op);
        let encoded = rlp::encode(&delayed);
        let decoded: DelayedTransaction = rlp::decode(&encoded).unwrap();

        match (delayed, decoded) {
            (DelayedTransaction::Tezos(expected), DelayedTransaction::Tezos(actual)) => {
                assert_eq!(actual, expected)
            }
            _ => panic!("Expected Tezos delayed transaction"),
        }
    }

    #[test]
    fn test_delayed_inbox_roundtrip() {
        let mut host = MockKernelHost::default();
        let mut base = load_base(&mut host).unwrap();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let mut delayed_inbox =
            DelayedInbox::from_base(&base).expect("Delayed inbox should be created");

        let tx: Transaction = dummy_transaction(0);

        let timestamp: Timestamp =
            read_last_info_per_level_timestamp(&base).unwrap_or(Timestamp::from(0));
        delayed_inbox
            .save_transaction(
                rk.host_mut(),
                &mut base,
                tx.clone().into(),
                timestamp,
                0,
                &CommonConfig::default(),
            )
            .expect("Tx should be saved in the delayed inbox");

        let delayed_inbox =
            DelayedInbox::from_base(&base).expect("Delayed inbox should exist");

        let read = delayed_inbox
            .find_transaction(&base, Hash(tx.tx_hash))
            .expect("Reading from the delayed inbox should work")
            .expect("Transaction should be in the delayed inbox");
        assert_eq!((tx, timestamp), read)
    }

    /// Until [super::DELAYED_INBOX_TIMEOUT_FLOOR_SUNSET], the effective
    /// timeout is floored at two weeks on a deployed network even when the
    /// configured (here: default, 12h) timeout is shorter; the floor itself
    /// still fires; and once the sunset has passed the configured timeout
    /// applies again.
    #[test]
    fn test_delayed_inbox_timeout_floored_until_sunset() {
        use super::{DELAYED_INBOX_TIMEOUT_FLOOR, DELAYED_INBOX_TIMEOUT_FLOOR_SUNSET};
        use crate::storage::{store_l1_level, store_last_info_per_level_timestamp};

        // Etherlink mainnet; any chain id but the sandbox one would do.
        let deployed = U256::from(42793);
        let day = 86_400i64;

        // Before the sunset: one day > the 12h default timeout, but the
        // two-week floor must keep the transaction in the inbox.
        let mut host = MockKernelHost::default();
        let mut base = load_base(&mut host).unwrap();
        let mut delayed_inbox = DelayedInbox::from_base(&base).unwrap();
        let tx: Transaction = dummy_transaction(0);
        delayed_inbox
            .save_transaction(
                &host,
                &mut base,
                tx.into(),
                Timestamp::from(0i64),
                0,
                &CommonConfig::default(),
            )
            .unwrap();
        // Satisfy the min-levels side of the condition (default: 720).
        store_l1_level(&mut base, 10_000).unwrap();
        store_last_info_per_level_timestamp(&mut base, Timestamp::from(day)).unwrap();
        assert!(
            !delayed_inbox.first_has_timed_out(&base, deployed).unwrap(),
            "before the sunset, a one-day-old delayed transaction must not \
             time out: the two-week floor applies"
        );

        // The same transaction times out on the sandbox chain using its
        // configured timeout, without waiting for the two-week floor.
        assert!(
            delayed_inbox
                .first_has_timed_out(&base, U256::from(EVM_CHAIN_ID))
                .unwrap(),
            "the sandbox chain is exempt from the floor"
        );

        // Still before the sunset: once the transaction is two weeks old,
        // the floored timeout fires.
        store_last_info_per_level_timestamp(
            &mut base,
            Timestamp::from(DELAYED_INBOX_TIMEOUT_FLOOR as i64),
        )
        .unwrap();
        assert!(
            delayed_inbox.first_has_timed_out(&base, deployed).unwrap(),
            "the two-week floor must still force-include old transactions"
        );

        // After the sunset: the configured (default, 12h) timeout applies
        // again, so a 13h-old transaction times out.
        let mut host = MockKernelHost::default();
        let mut base = load_base(&mut host).unwrap();
        let mut delayed_inbox = DelayedInbox::from_base(&base).unwrap();
        let tx: Transaction = dummy_transaction(1);
        let saved_at = DELAYED_INBOX_TIMEOUT_FLOOR_SUNSET as i64;
        delayed_inbox
            .save_transaction(
                &host,
                &mut base,
                tx.into(),
                Timestamp::from(saved_at),
                0,
                &CommonConfig::default(),
            )
            .unwrap();
        store_l1_level(&mut base, 10_000).unwrap();
        store_last_info_per_level_timestamp(
            &mut base,
            Timestamp::from(saved_at + 13 * 3_600),
        )
        .unwrap();
        assert!(
            delayed_inbox.first_has_timed_out(&base, deployed).unwrap(),
            "after the sunset, the configured 12h timeout must apply again"
        );
    }

    #[test]
    fn test_delayed_inbox_tezos_roundtrip() {
        let mut host = MockKernelHost::default();
        let mut base = load_base(&mut host).unwrap();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let mut delayed_inbox =
            DelayedInbox::from_base(&base).expect("Delayed inbox should be created");

        let op = make_test_operation();
        // Register the branch as live so the entry check accepts the operation.
        block_storage::internal_for_tests::register_live_block(
            rk.host_mut(),
            &TEZ_WORLD_STATE_ROOT_PATH,
            &H256(*op.branch),
        )
        .expect("register live block");
        let tx = Transaction {
            tx_hash: [5; TRANSACTION_HASH_SIZE],
            content: TezosDelayed(op),
        };

        let timestamp =
            read_last_info_per_level_timestamp(&base).unwrap_or(Timestamp::from(0));
        delayed_inbox
            .save_transaction(
                rk.host_mut(),
                &mut base,
                tx.clone().into(),
                timestamp,
                0,
                &CommonConfig::default(),
            )
            .expect("Tezos operation should be saved in the delayed inbox");

        let delayed_inbox =
            DelayedInbox::from_base(&base).expect("Delayed inbox should exist");

        let read = delayed_inbox
            .find_transaction(&base, Hash(tx.tx_hash))
            .expect("Reading from the delayed inbox should work")
            .expect("Transaction should be in the delayed inbox");
        assert_eq!((tx, timestamp), read)
    }

    /// A delayed operation with a stale/foreign branch is dropped at entry, not saved.
    #[test]
    fn test_delayed_inbox_tezos_dropped_on_stale_branch() {
        let mut host = MockKernelHost::default();
        let mut base = load_base(&mut host).unwrap();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let mut delayed_inbox =
            DelayedInbox::from_base(&base).expect("Delayed inbox should be created");

        // A branch neither in live_blocks nor covered by the genesis fallback: must be rejected.
        let mut op = make_test_operation();
        op.branch = tezos_crypto_rs::hash::BlockHash::from([0x11u8; 32]);
        let tx = Transaction {
            tx_hash: [7; TRANSACTION_HASH_SIZE],
            content: TezosDelayed(op),
        };

        let timestamp =
            read_last_info_per_level_timestamp(&base).unwrap_or(Timestamp::from(0));
        // Dropping is not an error: the call succeeds but nothing is stored.
        delayed_inbox
            .save_transaction(
                rk.host_mut(),
                &mut base,
                tx.clone().into(),
                timestamp,
                0,
                &CommonConfig::default(),
            )
            .expect("save_transaction should drop, not fail");

        let delayed_inbox =
            DelayedInbox::from_base(&base).expect("Delayed inbox should exist");
        let read = delayed_inbox
            .find_transaction(&base, Hash(tx.tx_hash))
            .expect("Reading from the delayed inbox should work");
        assert!(
            read.is_none(),
            "operation with a stale branch must not be saved"
        );
    }

    #[test]
    fn test_delayed_inbox_roundtrip_error_non_delayed() {
        let mut host = MockKernelHost::default();
        let mut base = load_base(&mut host).unwrap();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let mut delayed_inbox =
            DelayedInbox::from_base(&base).expect("Delayed inbox should be created");

        let tx: Transaction = Transaction {
            tx_hash: [12; TRANSACTION_HASH_SIZE],
            content: Ethereum(tx_(12)),
        };

        let timestamp: Timestamp =
            read_last_info_per_level_timestamp(&base).unwrap_or(Timestamp::from(0));
        let res = delayed_inbox.save_transaction(
            rk.host_mut(),
            &mut base,
            tx.into(),
            timestamp,
            0,
            &CommonConfig::default(),
        );

        assert!(res.is_err());
    }

    // The max-blueprint-length override is now read through the `/base`
    // keyspace inside `next_delayed_inbox_blueprint`. A value seeded at its
    // historical absolute path must read back through the keyspace key,
    // proving the resolved durable path never moved; an absent key falls back
    // to the default.
    #[test]
    fn max_delayed_inbox_blueprint_length_resolves_to_absolute_path() {
        use tezos_smart_rollup_host::path::RefPath;
        use tezos_smart_rollup_host::storage::StorageV1;
        use tezos_smart_rollup_keyspace::extensions::KeySpaceExtNum;

        let mut host = MockKernelHost::default();
        let base = load_base(&mut host).unwrap();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();

        assert_eq!(
            base.get_le_or(
                &super::MAX_DELAYED_INBOX_BLUEPRINT_LENGTH_KEY,
                super::DEFAULT_MAX_DELAYED_INBOX_BLUEPRINT_LENGTH
            ),
            super::DEFAULT_MAX_DELAYED_INBOX_BLUEPRINT_LENGTH
        );

        rk.host_mut()
            .store_write_all(
                &RefPath::assert_from(b"/base/max_delayed_inbox_blueprint_length"),
                &42u16.to_le_bytes(),
            )
            .unwrap();
        assert_eq!(
            base.get_le_or(
                &super::MAX_DELAYED_INBOX_BLUEPRINT_LENGTH_KEY,
                super::DEFAULT_MAX_DELAYED_INBOX_BLUEPRINT_LENGTH
            ),
            42
        );
    }

    // The sunset is documented as a calendar date but stored as an epoch, so
    // pin the two together.
    #[test]
    fn delayed_inbox_timeout_floor_sunset_date() {
        let expected = DateTime::parse_from_rfc3339("2026-11-27T00:00:00Z").unwrap();

        assert_eq!(
            DELAYED_INBOX_TIMEOUT_FLOOR_SUNSET,
            expected.timestamp() as u64
        );
    }
}
