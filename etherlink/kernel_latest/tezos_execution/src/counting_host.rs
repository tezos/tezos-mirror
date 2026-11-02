// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>

//! A test host that counts the bytes it hands back from durable storage.
//!
//! Gas cannot tell a metered read from an unmetered one — it is the same
//! either way. The bytes the host moved can.

use std::cell::Cell;
use std::rc::Rc;

use tezos_evm_runtime::runtime::MockKernelHost;
use tezos_smart_rollup::core_unsafe::STORE_HASH_SIZE;
use tezos_smart_rollup::host::{RuntimeError, ValueType};
use tezos_smart_rollup_host::path::Path;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_keyspace::{
    Key, KeySpace, KeySpaceError, KeySpaceLoader, KeySpaceLoaderError, Name,
};

/// Wraps the host every other test uses and tallies the payload bytes each
/// read returns. Everything else is forwarded untouched.
pub struct CountingHost {
    inner: MockKernelHost,
    bytes_read: Rc<Cell<u64>>,
}

impl Default for CountingHost {
    fn default() -> Self {
        let mut inner = MockKernelHost::default();
        inner
            .store_write_all(
                &tezos_evm_runtime::runtime_keyspaces::ETH_ACCOUNTS_ROOT_PATH,
                b"seed",
            )
            .unwrap();
        Self {
            inner,
            bytes_read: Rc::new(Cell::new(0)),
        }
    }
}

impl CountingHost {
    /// A handle on the tally, live for the host's whole lifetime.
    pub fn bytes_read_handle(&self) -> Rc<Cell<u64>> {
        Rc::clone(&self.bytes_read)
    }

    /// Restart the tally, to measure one operation and not the setup too.
    pub fn reset_bytes_read(&self) {
        self.bytes_read.set(0);
    }

    fn record(&self, bytes: usize) {
        self.bytes_read.set(self.bytes_read.get() + bytes as u64);
    }
}

impl StorageV1 for CountingHost {
    fn store_has<T: Path>(&self, path: &T) -> Result<Option<ValueType>, RuntimeError> {
        self.inner.store_has(path)
    }

    fn store_read<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        max_bytes: usize,
    ) -> Result<Vec<u8>, RuntimeError> {
        let bytes = self.inner.store_read(path, from_offset, max_bytes)?;
        self.record(bytes.len());
        Ok(bytes)
    }

    fn store_read_slice<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        buffer: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        let read = self.inner.store_read_slice(path, from_offset, buffer)?;
        self.record(read);
        Ok(read)
    }

    fn store_read_all(&self, path: &impl Path) -> Result<Vec<u8>, RuntimeError> {
        let bytes = self.inner.store_read_all(path)?;
        self.record(bytes.len());
        Ok(bytes)
    }

    fn store_write<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
        at_offset: usize,
    ) -> Result<(), RuntimeError> {
        self.inner.store_write(path, src, at_offset)
    }

    fn store_write_all<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
    ) -> Result<(), RuntimeError> {
        self.inner.store_write_all(path, src)
    }

    fn store_delete<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        self.inner.store_delete(path)
    }

    fn store_delete_value<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        self.inner.store_delete_value(path)
    }

    fn store_count_subkeys<T: Path>(&self, prefix: &T) -> Result<u64, RuntimeError> {
        self.inner.store_count_subkeys(prefix)
    }

    fn store_move(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        self.inner.store_move(from_path, to_path)
    }

    fn store_copy(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        self.inner.store_copy(from_path, to_path)
    }

    fn store_value_size(&self, path: &impl Path) -> Result<usize, RuntimeError> {
        // Not counted: probing moves no payload, which is the point.
        self.inner.store_value_size(path)
    }

    fn store_get_hash(
        &self,
        path: &impl Path,
    ) -> Result<[u8; STORE_HASH_SIZE], RuntimeError> {
        self.inner.store_get_hash(path)
    }
}

// Keyspaces own their storage handles, so their reads bypass StorageV1 on the
// host. Share the tally with every keyspace returned by the loader as well.
pub struct CountingKeySpace {
    inner: <MockKernelHost as KeySpaceLoader>::KeySpace,
    bytes_read: Rc<Cell<u64>>,
}

impl KeySpaceLoader for CountingHost {
    type KeySpace = CountingKeySpace;

    fn load_or_create(
        &mut self,
        name: Name,
    ) -> Result<Self::KeySpace, KeySpaceLoaderError> {
        Ok(CountingKeySpace {
            inner: self.inner.load_or_create(name)?,
            bytes_read: Rc::clone(&self.bytes_read),
        })
    }
}

impl KeySpace for CountingKeySpace {
    fn name(&self) -> &Name {
        self.inner.name()
    }
    fn get(&self, key: &Key) -> Option<Vec<u8>> {
        let bytes = self.inner.get(key)?;
        self.bytes_read
            .set(self.bytes_read.get() + bytes.len() as u64);
        Some(bytes)
    }
    fn read(&self, key: &Key, offset: usize, buffer: &mut [u8]) -> Option<usize> {
        let read = self.inner.read(key, offset, buffer)?;
        self.bytes_read.set(self.bytes_read.get() + read as u64);
        Some(read)
    }
    fn set(&mut self, key: &Key, value: impl AsRef<[u8]>) -> Result<(), KeySpaceError> {
        self.inner.set(key, value)
    }
    fn write(
        &mut self,
        key: &Key,
        offset: usize,
        data: impl AsRef<[u8]>,
    ) -> Result<usize, KeySpaceError> {
        self.inner.write(key, offset, data)
    }
    fn value_length(&self, key: &Key) -> Option<usize> {
        self.inner.value_length(key)
    }
    fn contains(&self, key: &Key) -> bool {
        self.inner.contains(key)
    }
    fn delete(&mut self, key: &Key) -> bool {
        self.inner.delete(key)
    }
    fn clear(&mut self) {
        self.inner.clear()
    }
    fn copy_from(&mut self, other: &Self) {
        self.inner.copy_from(&other.inner)
    }
    fn move_from(&mut self, other: &mut Self) {
        self.inner.move_from(&mut other.inner)
    }
    fn hash(&self) -> Vec<u8> {
        self.inner.hash()
    }
}
