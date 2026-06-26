// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! NDS-backend durable storage functionality, integrated in the
//! WASM PVM.

mod wasm_target;

use core::mem::MaybeUninit;

use tezos_smart_rollup_constants::core::NDS_MAX_KEY_SIZE;
use tezos_smart_rollup_constants::core::STORE_HASH_SIZE;

#[cfg(pvm_kind = "wasm")]
pub use wasm_target::WasmNdsHandle;

/// Errors returned by the NDS host functions exposed through [`WasmNds`].
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum NdsError {
    /// NDS is not enabled in the runtime.
    NdsDisabled,
    /// The requested registry resize was rejected (resizes are limited to one
    /// database at a time, up to `i32::MAX` databases).
    ResizeInvalid,
    /// A database index referenced an entry past the end of the registry.
    DatabaseOutOfBounds,
    /// A `buffer` argument exceeded the maximum permitted size.
    InputOutputTooLarge,
    /// The key was not found in the database.
    KeyNotFound,
    /// The requested offset is past the end of the value.
    OffsetTooLarge,
    /// Writing the value would exceed the maximum value size.
    StoreValueSizeExceeded,
}

/// A request to grow or shrink the database registry by a single database.
#[repr(i64)]
pub enum RegistryResizeRequest {
    /// Append one fresh, empty database to the registry.
    Increment = 1,
    /// Drop the trailing database (and its contents) from the registry.
    Decrement = -1,
}

/// A validated NDS key.
///
/// A `Key` is an unsized, transparent wrapper around a byte slice whose
/// length is guaranteed not to exceed [`NDS_MAX_KEY_SIZE`]. Construct one with
/// [`Key::new`].
#[repr(transparent)]
pub struct Key([u8]);

impl Key {
    /// Validate `bytes` as an NDS key.
    ///
    /// Returns [`None`] if `bytes` is longer than [`NDS_MAX_KEY_SIZE`].
    pub const fn new(bytes: &[u8]) -> Option<&Key> {
        if bytes.len() > NDS_MAX_KEY_SIZE {
            return None;
        }

        // SAFETY: `Key` is `#[repr(transparent)]` over `[u8]`, so a `&[u8]` and
        // a `&Key` have identical layout. The length invariant is enforced
        // above.
        Some(unsafe { &*(bytes as *const [u8] as *const Key) })
    }

    /// The key as a byte slice.
    pub fn as_bytes(&self) -> &[u8] {
        &self.0
    }
}

/// NDS-backed durable storage, integrated in the WASM PVM.
///
/// The store is a registry of independently-addressable databases, each a
/// flat key/value map. Database operations are indexed by position in the
/// registry; key/value operations additionally take a validated [`Key`].
pub trait WasmNds {
    /// The number of databases in the registry.
    fn len(&self) -> Result<usize, NdsError>;

    /// Whether the registry is empty.
    fn is_empty(&self) -> Result<bool, NdsError> {
        self.len().map(|size| size == 0)
    }

    /// Grow or shrink the registry by a single database, returning the new
    /// registry size.
    fn resize(&self, request: RegistryResizeRequest) -> Result<usize, NdsError>;

    /// Replace the contents of database `dst` with a copy of database `src`,
    /// leaving `src` unchanged. `dst == src` is a no-op.
    fn copy_db(&self, src: usize, dst: usize) -> Result<(), NdsError>;

    /// Replace the contents of database `dst` with those of `src`, then reset
    /// `src` to empty. `dst == src` is a no-op.
    fn move_db(&self, src: usize, dst: usize) -> Result<(), NdsError>;

    /// Empty the database at `db_index`, dropping every key.
    fn clear_db(&self, db_index: usize) -> Result<(), NdsError>;

    /// The BLAKE3 root hash of the database at `db_index`.
    fn hash_db(&self, db_index: usize) -> Result<[u8; STORE_HASH_SIZE], NdsError>;

    /// Whether `key` exists in database `db_index`.
    fn store_exists(&self, db_index: usize, key: &Key) -> Result<bool, NdsError>;

    /// The length in bytes of the value at `(db_index, key)`.
    fn store_value_size(&self, db_index: usize, key: &Key) -> Result<usize, NdsError>;

    /// Read the value at `(db_index, key)` starting from `offset` into `out`,
    /// returning the number of bytes written.
    fn store_read(
        &self,
        db_index: usize,
        key: &Key,
        offset: usize,
        out: &mut [MaybeUninit<u8>],
    ) -> Result<usize, NdsError>;

    /// Set the value at `(db_index, key)` to `value`, fully replacing any
    /// previous value and creating the key if absent.
    fn store_set(&self, db_index: usize, key: &Key, value: &[u8])
        -> Result<(), NdsError>;

    /// Write `value` into the value at `(db_index, key)` starting at `offset`,
    /// extending the value if the write runs past its end. The key is created
    /// if absent and `offset == 0`.
    fn store_write(
        &self,
        db_index: usize,
        key: &Key,
        offset: usize,
        value: &[u8],
    ) -> Result<(), NdsError>;

    /// Delete `(db_index, key)`. Deleting an absent key is a no-op.
    fn store_delete(&self, db_index: usize, key: &Key) -> Result<(), NdsError>;
}
