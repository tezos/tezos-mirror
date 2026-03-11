// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! A high-level API for the durable storage.
//!
//! A [`KeySpace`] is a flat key-value store which can hold arbitrarily-sized data
//! for a given [`Key`]. Kernels can create and delete [`KeySpace`] instances as required
//! and can generate hashes representing the entire state of a [`KeySpace`].

#[cfg(feature = "irmin-compat")]
mod irmin_path_validator;

/// The maximum size that a V2 durable storage key can have.
#[cfg(not(feature = "irmin-compat"))]
pub const MAX_STORE_V2_KEY_SIZE: usize = 256;

/// Key creation error
#[derive(Debug)]
pub enum KeyError {
    /// Attempted to create a key that exceeds the maximum allowed size.
    KeyTooLarge,
    /// Path validation error (e.g. missing leading `/`, invalid bytes, empty step).
    #[cfg(feature = "irmin-compat")]
    PathError(tezos_smart_rollup_host::path::PathError),
}

/// Key used to access data in a [`KeySpace`].
#[repr(transparent)]
pub struct Key([u8]);

impl Key {
    // Fallback: when `irmin-compat` is not enabled, only a size check is performed.
    // The full validation logic lives in `irmin_path_validator::Key::check_bytes`.
    #[cfg(not(feature = "irmin-compat"))]
    const fn check_bytes(bytes: &[u8]) -> Result<(), KeyError> {
        if bytes.len() > MAX_STORE_V2_KEY_SIZE {
            return Err(KeyError::KeyTooLarge);
        }
        Ok(())
    }

    /// Create a key from raw bytes.
    ///
    /// Returns an error if `key` fails validation (see [`Key::check_bytes`] for rules).
    pub fn from_bytes(key: &[u8]) -> Result<&Self, KeyError> {
        Self::check_bytes(key)?;
        // SAFETY: `Key` is `repr(transparent)` over `[u8]`, so `&[u8]` can be safely transmuted to `&Key`.
        Ok(unsafe { std::mem::transmute::<&[u8], &Key>(key) })
    }

    /// Create a key from raw bytes. Should only be used to create constant keys.
    ///
    /// Panics if `key` is an invalid set of bytes.
    pub const fn from_static(key: &'static [u8]) -> &'static Self {
        assert!(Self::check_bytes(key).is_ok(), "Invalid key");
        // SAFETY: `Key` is `repr(transparent)` over `[u8]`, so `&[u8]` can be safely transmuted to `&Key`.
        unsafe { std::mem::transmute(key) }
    }
}

/// Name creation error
#[derive(Debug)]
pub enum NameError {
    /// Attempted to create a name that exceeds the maximum allowed size.
    #[cfg(feature = "irmin-compat")]
    NameTooLong,
    /// Path validation error (e.g. missing leading `/`, trailing `/`, invalid bytes,
    /// or reserved `/readonly` prefix).
    #[cfg(feature = "irmin-compat")]
    PathError(tezos_smart_rollup_host::path::PathError),
}

/// A validated keyspace name, used as a path prefix in durable storage.
#[repr(transparent)]
pub struct Name(str);

impl Name {
    // Fallback: when `irmin-compat` is not enabled, no validation is performed.
    // The actual validation logic lives in `irmin_path_validator::Name::check_bytes`.
    #[cfg(not(feature = "irmin-compat"))]
    const fn check_bytes(_bytes: &[u8]) -> Result<(), NameError> {
        Ok(())
    }

    /// Create a name from a string slice.
    ///
    /// Returns an error if `name` fails validation (see [`Name::check_bytes`] for rules
    /// when the `irmin-compat` feature is enabled).
    pub fn from_slice(name: &str) -> Result<&Self, NameError> {
        Self::check_bytes(name.as_bytes())?;
        // SAFETY: `Name` is `repr(transparent)` over `str`, so `&str` can be safely transmuted to `&Name`.
        Ok(unsafe { std::mem::transmute::<&str, &Name>(name) })
    }

    /// Create a name from a static string. Should only be used to create constant names.
    ///
    /// Panics if the name fails validation (see [`Name::check_bytes`] for rules
    /// when the `irmin-compat` feature is enabled).
    pub const fn from_static_str(name: &'static str) -> &'static Self {
        assert!(Self::check_bytes(name.as_bytes()).is_ok(), "Invalid name");
        // SAFETY: `Name` is `repr(transparent)` over `str`, so `&str` can be safely transmuted to `&Name`.
        unsafe { std::mem::transmute(name) }
    }
}

/// A key space in the durable storage
pub trait KeySpace {
    /// Find the key space with the given name. If it does not exist, create a new key space.
    fn load_or_create(name: &Name) -> Self;

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
