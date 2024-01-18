// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>

use crate::{
    inbox::{Deposit, Transaction, TransactionContent},
    linked_list::LinkedList,
};
use anyhow::Result;
use rlp::{Decodable, DecoderError, Encodable};
use tezos_ethereum::{
    transaction::TRANSACTION_HASH_SIZE, tx_common::EthereumTransactionCommon,
};
use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup_host::{path::RefPath, runtime::Runtime};

pub struct DelayedInbox(LinkedList<Hash, DelayedTransaction>);

pub const DELAYED_INBOX_PATH: RefPath = RefPath::assert_from(b"/delayed-inbox");

// Tag that indicates the delayed transaction is a eth transaction.
pub const DELAYED_TRANSACTION_TAG: u8 = 0x01;

// Tag that indicates the delayed transaction is a deposit.
pub const DELAYED_DEPOSIT_TAG: u8 = 0x02;

/// Hash of a transaction
///
/// It represents the key of the transaction in the delayed inbox.
#[derive(Clone, Copy)]
pub struct Hash([u8; TRANSACTION_HASH_SIZE]);

impl Encodable for Hash {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        s.append(&self.0.to_vec());
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
/// Later it might be turned into a struct
/// And fields like the timestamp might be added
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

impl DelayedInbox {
    pub fn new<Host: Runtime>(host: &mut Host) -> Result<Self> {
        let linked_list = LinkedList::new(&DELAYED_INBOX_PATH, host)?;
        Ok(Self(linked_list))
    }

    pub fn save_transaction<Host: Runtime>(
        &mut self,
        host: &mut Host,
        tx: Transaction,
    ) -> Result<()> {
        let Transaction { tx_hash, content } = tx;
        let delayed_transaction = match content {
            TransactionContent::Ethereum(tx) => DelayedTransaction::Ethereum(tx),
            TransactionContent::Deposit(deposit) => DelayedTransaction::Deposit(deposit),
        };
        self.0.push(host, &Hash(tx_hash), &delayed_transaction)?;
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
    ) -> Result<Option<Transaction>> {
        log!(
            host,
            Info,
            "Removing transaction {} from the delayed inbox",
            hex::encode(tx_hash)
        );
        let tx = self.0.remove(host, &tx_hash)?.map(|delayed| match delayed {
            DelayedTransaction::Ethereum(tx) => Transaction {
                tx_hash: tx_hash.0,
                content: TransactionContent::Ethereum(tx),
            },
            DelayedTransaction::Deposit(deposit) => Transaction {
                tx_hash: tx_hash.0,
                content: TransactionContent::Deposit(deposit),
            },
        });

        Ok(tx)
    }
}

}
