// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! A high-level API for the durable storage.
//!
//! A [`KeySpace`] is a flat key-value store which can hold arbitrarily-sized data
//! for a given [`Key`]. Kernels can create and delete [`KeySpace`] instances as required
//! and can generate hashes representing the entire state of a [`KeySpace`].

/// The maximum size that a V2 durable storage key can have.
pub const MAX_STORE_V2_KEY_SIZE: usize = 256;

/// Key creation error
#[derive(Debug)]
pub enum KeyError {
    /// Attempted to create a key larger than 256 bytes
    KeyTooLarge,
}

/// Key used to access data in a [`KeySpace`].
#[repr(transparent)]
pub struct Key([u8]);

impl Key {
    /// Create a key from raw bytes. `key` must not be longer than the allowed maximum size.
    pub fn from_bytes(key: &[u8]) -> Result<&Self, KeyError> {
        // TODO TZX-45: Ensure keys don't result in invalid Irmin paths when implementing
        // `KeySpace` for the Irmin-based durable storage
        if key.len() > MAX_STORE_V2_KEY_SIZE {
            return Err(KeyError::KeyTooLarge);
        };

        // SAFETY: `Key` is `repr(transparent)` over `[u8]`, so `&[u8]` can be safely cast to `&Key`.
        Ok(unsafe { &*(key as *const [u8] as *const Key) })
    }

    /// Create a key from raw bytes. Should only be used to create constant keys.
    ///
    /// Panics if `key` is longer than the allowed maximum size.
    pub const fn from_static(key: &'static [u8]) -> &'static Self {
        assert!(
            key.len() <= MAX_STORE_V2_KEY_SIZE,
            "Key must not be longer than 256 bytes"
        );

        // SAFETY: `Key` is `repr(transparent)` over `[u8]`, so `&[u8]` can be safely cast to `&Key`.
        unsafe { &*(key as *const [u8] as *const Key) }
    }
}

/// A key space in the durable storage
pub trait KeySpace {
    /// Find the key space with the given name. If it does not exist, create a new key space.
    fn load_or_create(name: &str) -> Self;

    /// Read the whole value associated with the key.
    /// Returns `None` if the key does not exist.
    fn get(&self, key: &Key) -> Option<Vec<u8>>;

    /// Read a portion of the value associated with the key and return the number of bytes written.
    /// Returns `None` if the key does not exist.
    fn read(&self, key: &Key, offset: usize, buffer: &mut [u8]) -> Option<usize>;

    /// Write the given value to the key.
    fn set(&mut self, key: &Key, value: impl AsRef<[u8]>);

    /// Write data to the value at the given key starting at the given offset.
    /// Returns the number of bytes written.
    fn write(&self, key: &Key, offset: usize, data: impl AsRef<[u8]>) -> usize;

    /// Retrieve the length of the value associated with the key.
    /// Returns `None` if the key does not exist.
    fn value_length(&self, key: &Key) -> Option<usize>;

    /// Check if the given key exists in the key space.
    fn contains(&self, key: &Key) -> bool;

    /// Remove the key from the key space. Returns false if the key did not exist.
    fn delete(&mut self, key: &Key) -> bool;

    /// Remove all key-value associations from the key space.
    fn clear(&mut self);

    /// Replace the key-value associations in `self` with those of `other`.
    /// The `other` key space is not modified.
    fn copy_from(&mut self, other: &Self);

    /// Replace the key-value associations in `self` with those of `other` and
    /// remove all key-value associations from `other`.
    fn move_from(&mut self, other: &mut Self);

    /// Once all references to this key space are dropped, the key space will
    /// be deleted.
    fn mark_for_deletion(self);

    /// Obtain the hash that represents the current state of the key space. This hash is
    /// sensitive to the key-value associations and the order in which they were added.
    fn hash(&self) -> Vec<u8>;
}
