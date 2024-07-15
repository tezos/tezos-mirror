// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::{
    collections::{HashSet, VecDeque},
    path::Path,
};

use anyhow::Error;
use primitive_types::H256;

/// A stop list keeps track of the transactions already decrypted by the key holder
trait StopList {
    type Config;

    /// Creates a new StopList
    fn new(config: Self::Config) -> Self;

    /// Returns True if the transaction hash is already in the list, false if otherwise
    fn contains(&self, value: H256) -> Result<bool, Error>;

    /// Adds a new transaction hash into the StopList
    fn insert(&mut self, value: H256) -> Result<(), Error>;
}

trait StorageBackend: Sized {
    type Config;

    /// Creates a new storage backend
    fn new(config: Self::Config) -> Result<Self, Error>;

    /// Loads data and config of a storage stored on disk
    fn load(p: &Path) -> Result<Self, Error>;

    /// Adds a new transaction hash into the storage
    fn insert(&mut self, tx_hash: H256) -> Result<(), Error>;

    /// Returns True if the transaction hash is already in the storage, false if otherwise
    fn contains(&self, tx_hash: H256) -> Result<bool, Error>;

    /// Evicts transaction hash from the storage
    fn evict(&mut self) -> Result<(), Error>;

    /// Returns True if the storage is full, false if otherwise
    fn is_full(&self) -> Result<bool, Error>;
}

struct InMemoryStorageBackend {
    record: VecDeque<H256>,
    record_set: HashSet<H256>,
    max_size: usize,
}

impl StorageBackend for InMemoryStorageBackend {
    type Config = usize;

    fn new(max_size: usize) -> Result<Self, Error> {
        Ok(Self {
            record: VecDeque::new(),
            record_set: HashSet::new(),
            max_size: max_size,
        })
    }

    fn load(_p: &Path) -> Result<Self, Error> {
        // TODO: implement backup on disk
        todo!()
    }

    fn insert(&mut self, tx_hash: H256) -> Result<(), Error> {
        self.record.push_back(tx_hash);
        self.record_set.insert(tx_hash);
        Ok(())
    }

    fn contains(&self, tx_hash: H256) -> Result<bool, Error> {
        Ok(self.record_set.contains(&tx_hash))
    }

    fn evict(&mut self) -> Result<(), Error> {
        if let Some(evicted) = self.record.pop_front() {
            self.record_set.remove(&evicted);
        }
        Ok(())
    }

    fn is_full(&self) -> Result<bool, Error> {
        Ok(self.record_set.len() >= self.max_size)
    }
}

#[allow(unused)]
struct TransactionHashStopList<B: StorageBackend> {
    storage_backend: B,
}

impl StopList for TransactionHashStopList<InMemoryStorageBackend> {
    type Config = usize;

    fn new(config: Self::Config) -> Self {
        TransactionHashStopList {
            storage_backend: InMemoryStorageBackend::new(config).unwrap(),
        }
    }

    fn contains(&self, value: H256) -> Result<bool, Error> {
        self.storage_backend.contains(value)
    }

    fn insert(&mut self, value: H256) -> Result<(), Error> {
        if !self.contains(value)? {
            if self.storage_backend.is_full().unwrap_or(false) {
                let _ = self.storage_backend.evict();
            }
            self.storage_backend.insert(value.clone())?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use primitive_types::H256;

    use crate::stoplist::StopList;

    use super::TransactionHashStopList;

    #[test]
    fn test_stop_list() {
        let mut stop_list = TransactionHashStopList::new(2);

        let tx1 = H256::from_low_u64_le(1);
        let tx2 = H256::from_low_u64_le(2);
        let tx3 = H256::from_low_u64_le(3);

        assert!(!stop_list.contains(tx1).unwrap());
        assert!(!stop_list.contains(tx2).unwrap());
        assert!(!stop_list.contains(tx3).unwrap());

        stop_list.insert(tx1).unwrap();
        assert!(stop_list.contains(tx1).unwrap());
        stop_list.insert(tx2).unwrap();
        stop_list.insert(tx3).unwrap();

        assert!(!stop_list.contains(tx1).unwrap());
        assert!(stop_list.contains(tx2).unwrap());
        assert!(stop_list.contains(tx3).unwrap());
    }
}
