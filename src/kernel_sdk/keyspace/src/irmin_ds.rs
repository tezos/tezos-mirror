// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! [`KeySpace`] implementation backed by [`StorageV1`].
//!
//! [`StorageV1KeySpaceCompat`] wraps a [`StorageV1`] host and a [`Name`] prefix,
//! delegating every operation to the underlying durable storage by
//! concatenating the prefix with the key to form a full irmin path.

use std::cell::Cell;
use std::collections::BTreeMap;
use std::ops::Bound;
use std::rc::Rc;

use hex_literal::hex;
use tezos_smart_rollup_host::{
    path::{concat, OwnedPath},
    runtime::{RuntimeError, ValueType},
    storage::{CoreStorage, StorageV1},
    Error as HostError,
};

use crate::{Key, KeySpace, KeySpaceLoaderError, KeySpaceWriteError, Name};

/// Blake2B hash of an empty irmin tree (`Context.Tree.empty` in OCaml).
const EMPTY_TREE_HASH: [u8; 32] =
    hex!("81e47a19e6b29b0a65b9591762ce5143ed30d0261e5d24a3201752506b20f15c");

/// Errors that can legitimately occur in durable storage operations.
///
/// [`From<RuntimeError>`] exhaustively maps every [`RuntimeError`] variant:
/// those that cannot occur — thanks to [`Name`] and [`Key`] validation,
/// and the absence of outbox/I-O operations — hit `unreachable!` with
/// the formatted error, ensuring both a compile-time check if new variants are added
/// and a readable panic message at runtime if the logic is violated.
#[derive(Debug)]
enum StorageV1ReadError {
    /// The path does not exist in durable storage.
    PathNotFound,
    /// The path exists but points to a subtree, not a value.
    NotAValue,
    /// The read offset is past the end of the stored value.
    InvalidOffset,
}

// TODO: SDK-133 (https://linear.app/tezos/issue/SDK-133) — error classification
// could move to the SDK directly, avoiding this manual mapping in each KeySpace backend.
impl From<RuntimeError> for StorageV1ReadError {
    fn from(e: RuntimeError) -> Self {
        match e {
            RuntimeError::PathNotFound
            | RuntimeError::HostErr(HostError::StoreNotANode) => Self::PathNotFound,
            RuntimeError::HostErr(HostError::StoreNotAValue) => Self::NotAValue,
            // Offset out of bounds: durable.ml raises Out_of_bounds when
            // read offset >= value length, mapped to StoreInvalidAccess.
            RuntimeError::HostErr(HostError::StoreInvalidAccess) => Self::InvalidOffset,
            // Key and Name enforce valid, correctly-sized paths.
            RuntimeError::HostErr(
                HostError::StoreKeyTooLarge | HostError::StoreInvalidKey,
            ) => unreachable!("Key/Name guarantee valid paths: {e:?}"),
            // Keyspace paths are never under /readonly.
            RuntimeError::HostErr(HostError::StoreReadonlyValue) => {
                unreachable!("Keyspace paths are not readonly: {e:?}")
            }
            // KeySpace does not perform outbox operations.
            RuntimeError::HostErr(HostError::FullOutbox) => {
                unreachable!("KeySpace does not use outbox: {e:?}")
            }
            // Prevented by the StorageV1 SDK impl which handles chunking.
            RuntimeError::HostErr(HostError::InputOutputTooLarge) => {
                unreachable!("StorageV1 SDK impl prevents InputOutputTooLarge: {e:?}")
            }
            // StoreListIndexOutOfBounds: KeySpace does not use subkey-list operations.
            RuntimeError::StoreListIndexOutOfBounds => {
                unreachable!("KeySpace does not use list ops: {e:?}")
            }
            // DecodingError: returned by store_get_hash when the hash is not 32 bytes,
            // but the SDK always requests exactly STORE_HASH_SIZE bytes and the host
            // always produces a 32-byte Blake2B hash — unreachable in practice.
            RuntimeError::DecodingError => {
                unreachable!("store_get_hash always returns 32 bytes: {e:?}")
            }
            // Value exceeds the ~2GB durable storage limit.
            RuntimeError::HostErr(HostError::StoreValueSizeExceeded) => {
                unreachable!(
                    "ValueSizeExceeded is only returned on write, not read: {e:?}"
                )
            }
            // Invalid memory access errors — prevented by safe Rust:
            // the SDK never passes raw pointers to host functions.
            RuntimeError::HostErr(
                HostError::GenericInvalidAccess | HostError::MemoryInvalidAccess,
            ) => unreachable!("Safe Rust prevents invalid memory access: {e:?}"),
        }
    }
}

/// Convert a [`RuntimeError`] from a write operation directly into
/// [`KeySpaceWriteError`], panicking on variants that cannot occur.
fn classify_write_error(e: RuntimeError) -> KeySpaceWriteError {
    match e {
        // Value exceeds the ~2GB durable storage limit.
        RuntimeError::HostErr(HostError::StoreValueSizeExceeded) => {
            KeySpaceWriteError::ValueSizeExceeded
        }
        // Offset out of bounds: durable.ml raises Out_of_bounds when
        // write offset > value length, mapped to StoreInvalidAccess.
        RuntimeError::HostErr(HostError::StoreInvalidAccess) => {
            KeySpaceWriteError::InvalidOffset
        }
        RuntimeError::PathNotFound
        | RuntimeError::HostErr(HostError::StoreNotANode | HostError::StoreNotAValue) => {
            unreachable!("Only returned on read, not write; path is created implicitly on write: {e:?}")
        }
        // Key and Name enforce valid, correctly-sized paths.
        RuntimeError::HostErr(
            HostError::StoreKeyTooLarge | HostError::StoreInvalidKey,
        ) => unreachable!("Key/Name guarantee valid paths: {e:?}"),
        // Keyspace paths are never under /readonly.
        RuntimeError::HostErr(HostError::StoreReadonlyValue) => {
            unreachable!("Keyspace paths are not readonly: {e:?}")
        }
        // KeySpace does not perform outbox operations.
        RuntimeError::HostErr(HostError::FullOutbox) => {
            unreachable!("KeySpace does not use outbox: {e:?}")
        }
        // Prevented by the StorageV1 SDK impl which handles chunking.
        RuntimeError::HostErr(HostError::InputOutputTooLarge) => {
            unreachable!("StorageV1 SDK impl prevents InputOutputTooLarge: {e:?}")
        }
        // StoreListIndexOutOfBounds: KeySpace does not use subkey-list operations.
        RuntimeError::StoreListIndexOutOfBounds => {
            unreachable!("KeySpace does not use list ops: {e:?}")
        }
        // DecodingError: only returned by store_get_hash (not a write op),
        // and always returns 32 bytes — unreachable on write path.
        RuntimeError::DecodingError => {
            unreachable!("DecodingError is read-only (store_get_hash): {e:?}")
        }
        // Invalid memory access errors — prevented by safe Rust:
        // the SDK never passes raw pointers to host functions.
        RuntimeError::HostErr(
            HostError::GenericInvalidAccess | HostError::MemoryInvalidAccess,
        ) => unreachable!("Safe Rust prevents invalid memory access: {e:?}"),
    }
}

/// A [`KeySpace`] backed by the irmin durable storage via [`StorageV1`].
///
/// Each instance operates under a fixed [`Name`] prefix: all keys are
/// resolved to `{prefix}{key}` in the durable tree.
///
/// When created via [`IrminKeySpaceRegistry::load_or_create`], the handle carries a guard
/// that releases the name on [`Drop`], allowing it to be loaded again.
pub struct StorageV1KeySpaceCompat<Storage> {
    storage: Storage,
    prefix: Name,
    guard: Rc<Cell<bool>>,
}

impl<Storage> StorageV1KeySpaceCompat<Storage> {
    /// Create a new `StorageV1KeySpaceCompat` operating under the given name prefix.
    pub fn new(storage: Storage, name: Name, guard: Rc<Cell<bool>>) -> Self {
        Self {
            storage,
            prefix: name,
            guard,
        }
    }

    fn full_path(&self, key: &Key) -> OwnedPath {
        // `Key: Path` (irmin-compat), so `key: &Key` is `&impl Path`.
        // Safety: concat only fails if the resulting path is invalid, but since [Name] and [Key]
        // have restricted sizes and contents, the resulting path is guaranteed to be valid.
        concat(&self.prefix, key).expect("Valid irmin-durable path")
    }
}

impl<Storage: StorageV1> KeySpace for StorageV1KeySpaceCompat<Storage> {
    fn name(&self) -> &Name {
        &self.prefix
    }

    fn get(&self, key: &Key) -> Option<Vec<u8>> {
        let path = self.full_path(key);
        self.storage
            .store_read_all(&path)
            .map_err(StorageV1ReadError::from)
            .ok()
    }

    fn read(&self, key: &Key, offset: usize, buffer: &mut [u8]) -> Option<usize> {
        let path = self.full_path(key);
        self.storage
            .store_read_slice(&path, offset, buffer)
            .map_err(StorageV1ReadError::from)
            .ok()
    }

    fn set(
        &mut self,
        key: &Key,
        value: impl AsRef<[u8]>,
    ) -> Result<(), KeySpaceWriteError> {
        let path = self.full_path(key);
        self.storage
            .store_write_all(&path, value.as_ref())
            .map_err(classify_write_error)
    }

    fn write(
        &mut self,
        key: &Key,
        offset: usize,
        data: impl AsRef<[u8]>,
    ) -> Result<usize, KeySpaceWriteError> {
        let path = self.full_path(key);
        self.storage
            .store_write(&path, data.as_ref(), offset)
            .map_err(classify_write_error)
            .map(|()| data.as_ref().len())
    }

    fn value_length(&self, key: &Key) -> Option<usize> {
        let path = self.full_path(key);
        self.storage
            .store_value_size(&path)
            .map_err(StorageV1ReadError::from)
            .ok()
    }

    fn contains(&self, key: &Key) -> bool {
        let res = self
            .storage
            .store_has(&self.full_path(key))
            .map_err(StorageV1ReadError::from);
        match res {
            Ok(Some(ValueType::Value | ValueType::ValueWithSubtree)) => true,
            Ok(None | Some(ValueType::Subtree)) => false,
            Err(_) => false,
        }
    }

    fn delete(&mut self, key: &Key) -> bool {
        let path = self.full_path(key);
        let has_value = self
            .storage
            .store_has(&path)
            .map_err(StorageV1ReadError::from);
        match has_value {
            Ok(Some(ValueType::Value | ValueType::ValueWithSubtree)) => {
                let _ = self.storage.store_delete_value(&path);
                true
            }
            _ => false,
        }
    }

    fn clear(&mut self) {
        let _ = self
            .storage
            .store_delete(&self.prefix)
            .map_err(StorageV1ReadError::from);
    }

    fn copy_from(&mut self, other: &Self) {
        let res = self
            .storage
            .store_copy(&other.prefix, &self.prefix)
            .map_err(StorageV1ReadError::from);
        match res {
            Ok(()) => (),
            Err(StorageV1ReadError::NotAValue) => self.clear(),
            // Copy: `other` was empty, so `self` should be empty too.
            Err(StorageV1ReadError::PathNotFound) => self.clear(),
            Err(StorageV1ReadError::InvalidOffset) => {
                unreachable!("store_copy does not use offsets")
            }
        }
    }

    fn move_from(&mut self, other: &mut Self) {
        let res = self
            .storage
            .store_move(&other.prefix, &self.prefix)
            .map_err(StorageV1ReadError::from);
        match res {
            Ok(()) => (),
            Err(StorageV1ReadError::NotAValue) => self.clear(),
            // Move: `other` was already empty, so `self` should be empty too.
            Err(StorageV1ReadError::PathNotFound) => self.clear(),
            Err(StorageV1ReadError::InvalidOffset) => {
                unreachable!("store_move does not use offsets")
            }
        }
    }

    fn hash(&self) -> Vec<u8> {
        let res = self
            .storage
            .store_get_hash(&self.prefix)
            .map_err(StorageV1ReadError::from);
        match res {
            Ok(hash) => hash.to_vec(),
            Err(StorageV1ReadError::PathNotFound) => EMPTY_TREE_HASH.to_vec(),
            // Read: store_get_hash hashes the subtree, not a value.
            Err(StorageV1ReadError::NotAValue) => {
                unreachable!("store_get_hash hashes the subtree: {res:?}")
            }
            Err(StorageV1ReadError::InvalidOffset) => {
                unreachable!("store_get_hash does not use offsets")
            }
        }
    }
}

impl<Storage> Drop for StorageV1KeySpaceCompat<Storage> {
    fn drop(&mut self) {
        self.guard.set(false);
    }
}

/// The registry of currently-loaded keyspace names backing a [`StorageV1`]
/// [`KeySpaceLoader`](crate::KeySpaceLoader).
///
/// It mints one handle per keyspace via [`CoreStorage::new_storage`] and
/// rejects overlapping names. The set of loaded names is fully encapsulated:
/// callers receive a per-handle drop-guard, never access to the registry
/// itself.
#[derive(Debug, Default)]
pub struct IrminKeySpaceRegistry {
    /// The set of loaded names. Each name is guarded by a flag cleared when
    /// its keyspace handle is dropped, allowing the name to be reloaded.
    loaded: BTreeMap<Name, Rc<Cell<bool>>>,
}

impl IrminKeySpaceRegistry {
    /// Create an empty registry.
    pub fn new() -> Self {
        Self::default()
    }

    /// Load or create a key space with the given `name`, minting its storage
    /// handle from `host`.
    ///
    /// # Safety
    ///
    /// The caller must ensure `self` is the only registry minting keyspaces
    /// from `host`; otherwise the overlap checks can be bypassed and two live
    /// keyspaces may address overlapping paths.
    pub unsafe fn load_or_create<Host: CoreStorage>(
        &mut self,
        host: &mut Host,
        name: Name,
    ) -> Result<StorageV1KeySpaceCompat<Host::Storage>, KeySpaceLoaderError> {
        // Resolve the name against the registry first, taking an owned guard,
        // so the registry borrow is released before minting the handle.
        let guard = self.register(&name)?;
        // SAFETY: the handle is scoped to `name`, a unique, non-overlapping
        // prefix (enforced by `register`), so no two live keyspaces address
        // overlapping paths.
        let storage = unsafe { host.new_storage() };
        Ok(StorageV1KeySpaceCompat::new(storage, name, guard))
    }

    /// Register `name` in the loaded set and return its guard, or an error if
    /// it is already loaded or overlaps a currently-loaded name.
    fn register(&mut self, name: &Name) -> Result<Rc<Cell<bool>>, KeySpaceLoaderError> {
        let loaded = &mut self.loaded;

        // Check for exact duplicate or re-load a dropped keyspace.
        if let Some(guard) = loaded.get(name) {
            if guard.get() {
                return Err(KeySpaceLoaderError::AlreadyLoaded);
            }
            // Previously loaded and dropped — re-load immediately. Overlap
            // checks are unnecessary: the name was already validated, and
            // overlapping names are permanently rejected.
            guard.set(true);
            return Ok(Rc::clone(guard));
        }

        let name_str: &str = name.as_ref();

        // Overlapping descendants: in the sorted map any name starting with
        // `name/` sorts right after `name` (only '-' and '.' are valid path
        // chars below '/').
        for (existing, _) in
            loaded.range::<Name, _>((Bound::Excluded(name), Bound::Unbounded))
        {
            let existing_str: &str = existing.as_ref();
            if !existing_str.starts_with(name_str) {
                break;
            }
            if existing_str.as_bytes()[name_str.len()] == b'/' {
                return Err(KeySpaceLoaderError::Overlapping);
            }
        }

        // Overlapping ancestors: point lookups at each '/' boundary, bounded by
        // the path depth.
        for (i, &b) in name_str.as_bytes().iter().enumerate().skip(1) {
            if b == b'/' {
                if let Ok(ancestor) = name_str[..i].parse::<Name>() {
                    if loaded.contains_key(&ancestor) {
                        return Err(KeySpaceLoaderError::Overlapping);
                    }
                }
            }
        }

        let guard = loaded
            .entry(name.clone())
            .or_insert_with(|| Rc::new(Cell::new(false)));
        guard.set(true);
        Ok(Rc::clone(guard))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_smart_rollup_mock::MockHost;

    fn make_ks(host: MockHost, name: &str) -> StorageV1KeySpaceCompat<MockHost> {
        StorageV1KeySpaceCompat::new(
            host,
            name.parse().unwrap(),
            Rc::new(Cell::new(true)),
        )
    }

    fn key(s: &[u8]) -> Key {
        Key::from_bytes(s).unwrap()
    }

    #[test]
    fn get_returns_none_for_missing_key() {
        let ks = make_ks(MockHost::default(), "/test");
        assert_eq!(ks.get(&key(b"/a")), None);
    }

    #[test]
    fn set_then_get() {
        let mut ks = make_ks(MockHost::default(), "/test");
        ks.set(&key(b"/a"), b"hello").unwrap();
        assert_eq!(ks.get(&key(b"/a")), Some(b"hello".to_vec()));
    }

    #[test]
    fn contains_and_delete() {
        let mut ks = make_ks(MockHost::default(), "/test");
        assert!(!ks.contains(&key(b"/a")));

        ks.set(&key(b"/a"), b"val").unwrap();
        assert!(ks.contains(&key(b"/a")));

        assert!(ks.delete(&key(b"/a")));
        assert!(!ks.contains(&key(b"/a")));
    }

    #[test]
    fn delete_missing_returns_false() {
        let mut ks = make_ks(MockHost::default(), "/test");
        assert!(!ks.delete(&key(b"/nope")));
    }

    #[test]
    fn value_length() {
        let mut ks = make_ks(MockHost::default(), "/test");
        assert_eq!(ks.value_length(&key(b"/a")), None);

        ks.set(&key(b"/a"), b"12345").unwrap();
        assert_eq!(ks.value_length(&key(b"/a")), Some(5));
    }

    #[test]
    fn partial_read() {
        let mut ks = make_ks(MockHost::default(), "/test");
        ks.set(&key(b"/a"), b"hello world").unwrap();

        let mut buf = [0u8; 5];
        let n = ks.read(&key(b"/a"), 6, &mut buf).unwrap();
        assert_eq!(n, 5);
        assert_eq!(&buf[..n], b"world");
    }

    #[test]
    fn read_missing_returns_none() {
        let ks = make_ks(MockHost::default(), "/test");
        let mut buf = [0u8; 10];
        assert_eq!(ks.read(&key(b"/a"), 0, &mut buf), None);
    }

    #[test]
    fn clear_removes_all() {
        let mut ks = make_ks(MockHost::default(), "/test");
        ks.set(&key(b"/a"), b"1").unwrap();
        ks.set(&key(b"/b"), b"2").unwrap();

        ks.clear();
        assert!(!ks.contains(&key(b"/a")));
        assert!(!ks.contains(&key(b"/b")));
    }

    #[test]
    fn write_at_offset() {
        let mut ks = make_ks(MockHost::default(), "/test");
        ks.set(&key(b"/a"), b"hello world").unwrap();
        let n = ks.write(&key(b"/a"), 6, b"rust!").unwrap();
        assert_eq!(n, 5);
        assert_eq!(ks.get(&key(b"/a")), Some(b"hello rust!".to_vec()));
    }

    #[test]
    fn clear_empty_keyspace_does_not_panic() {
        let mut ks = make_ks(MockHost::default(), "/test");
        ks.clear();
    }

    #[test]
    fn delete_after_clear_returns_false() {
        let mut ks = make_ks(MockHost::default(), "/test");
        ks.set(&key(b"/a"), b"val").unwrap();
        ks.clear();
        assert!(!ks.delete(&key(b"/a")));
    }

    #[test]
    fn get_after_clear_returns_none() {
        let mut ks = make_ks(MockHost::default(), "/test");
        ks.set(&key(b"/a"), b"val").unwrap();
        ks.clear();
        assert_eq!(ks.get(&key(b"/a")), None);
    }

    #[test]
    fn hash_returns_bytes() {
        let mut ks = make_ks(MockHost::default(), "/test");
        let empty_hash = ks.hash();
        // Even an empty keyspace has a hash (empty tree hash).
        assert!(!empty_hash.is_empty());

        ks.set(&key(b"/a"), b"val").unwrap();
        let populated_hash = ks.hash();
        assert!(!populated_hash.is_empty());
        assert_ne!(empty_hash, populated_hash);
    }

    #[test]
    fn hash_changes_after_mutation() {
        let mut ks = make_ks(MockHost::default(), "/test");
        ks.set(&key(b"/a"), b"val").unwrap();
        let h1 = ks.hash();

        ks.set(&key(b"/a"), b"other").unwrap();
        let h2 = ks.hash();

        assert_ne!(h1, h2);
    }

    // Large-value tests: these exercise behaviour that depends on
    // store_read_slice chunking (!21564) and proper InputOutputTooLarge
    // handling. They are expected to fail until those changes land.

    #[test]
    fn set_then_get_large_value() {
        let mut ks = make_ks(MockHost::default(), "/test");
        let data: Vec<u8> = (0..5000).map(|i| (i % 256) as u8).collect();
        ks.set(&key(b"/big"), &data).unwrap();

        let got = ks.get(&key(b"/big")).unwrap();
        assert_eq!(got, data);
    }

    #[test]
    fn read_large_value_returns_all_bytes() {
        let mut ks = make_ks(MockHost::default(), "/test");
        let data: Vec<u8> = (0..5000).map(|i| (i % 256) as u8).collect();
        ks.set(&key(b"/big"), &data).unwrap();

        let mut buf = vec![0u8; 5000];
        let n = ks.read(&key(b"/big"), 0, &mut buf).unwrap();
        assert_eq!(n, 5000);
        assert_eq!(&buf[..n], &data[..]);
    }

    #[test]
    fn read_at_offset_past_end_returns_none() {
        let mut ks = make_ks(MockHost::default(), "/test");
        ks.set(&key(b"/a"), b"hello").unwrap();

        let mut buf = [0u8; 10];
        assert_eq!(ks.read(&key(b"/a"), 10, &mut buf), None);
    }

    #[test]
    fn write_at_offset_past_end_returns_error() {
        let mut ks = make_ks(MockHost::default(), "/test");
        ks.set(&key(b"/a"), b"hello").unwrap();

        assert!(ks.write(&key(b"/a"), 100, b"data").is_err());
    }

    #[test]
    fn get_prefix_exact_returns_first_n_bytes_when_value_has_at_least_n() {
        let mut ks = make_ks(MockHost::default(), "/test");
        ks.set(&key(b"/exact"), b"hello").unwrap();
        assert_eq!(ks.get_prefix_exact::<5>(&key(b"/exact")), Some(*b"hello"));

        ks.set(&key(b"/long"), b"hello world").unwrap();
        assert_eq!(ks.get_prefix_exact::<5>(&key(b"/long")), Some(*b"hello"));
    }

    #[test]
    fn get_prefix_exact_returns_none_when_value_too_short_or_key_missing() {
        let mut ks = make_ks(MockHost::default(), "/test");
        ks.set(&key(b"/a"), b"hi").unwrap();

        assert_eq!(ks.get_prefix_exact::<5>(&key(b"/a")), None);
        assert_eq!(ks.get_prefix_exact::<5>(&key(b"/missing")), None);
    }

    #[test]
    fn read_exact_in_returns_n_bytes_from_offset() {
        let mut ks = make_ks(MockHost::default(), "/test");
        ks.set(&key(b"/a"), b"hello world").unwrap();

        assert_eq!(ks.read_exact_in::<5>(&key(b"/a"), 0), Some(*b"hello"));
        assert_eq!(ks.read_exact_in::<5>(&key(b"/a"), 6), Some(*b"world"));
    }

    #[test]
    fn read_exact_in_returns_none_when_not_enough_bytes_or_key_missing() {
        let mut ks = make_ks(MockHost::default(), "/test");
        ks.set(&key(b"/a"), b"hello").unwrap();

        assert_eq!(ks.read_exact_in::<5>(&key(b"/a"), 2), None);
        assert_eq!(ks.read_exact_in::<5>(&key(b"/missing"), 0), None);
    }
}
