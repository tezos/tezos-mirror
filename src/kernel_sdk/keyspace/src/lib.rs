// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! A high-level API for the durable storage.
//!
//! A [`KeySpace`] is a flat key-value store which can hold arbitrarily-sized data
//! for a given [`Key`]. Kernels can create and delete [`KeySpace`] instances as required
//! and can generate hashes representing the entire state of a [`KeySpace`].

use std::str::FromStr;

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
    /// Path validation error (e.g. missing leading `/`, invalid bytes, empty step,
    /// or reserved `/readonly` prefix).
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
    /// Returns an error if `key` fails validation (see `Key::check_bytes` for rules).
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

impl core::fmt::Debug for Key {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "Key({self})")
    }
}

impl core::fmt::Display for Key {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        // SAFETY: with irmin-compat, Key bytes are valid ASCII path characters.
        // Without irmin-compat, we print hex-encoded bytes directly.
        match core::str::from_utf8(&self.0) {
            Ok(s) => f.write_str(s),
            Err(_) => write!(f, "Key({:02x?})", &self.0),
        }
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
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct Name(String);

impl core::fmt::Display for Name {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str(&self.0)
    }
}

impl Name {
    // Fallback: when `irmin-compat` is not enabled, no validation is performed.
    // The actual validation logic lives in `irmin_path_validator::Name::check_bytes`.
    #[cfg(not(feature = "irmin-compat"))]
    const fn check_bytes(_bytes: &[u8]) -> Result<(), NameError> {
        Ok(())
    }
}

impl AsRef<str> for Name {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl FromStr for Name {
    type Err = NameError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::check_bytes(s.as_bytes())?;
        Ok(Name(s.to_owned()))
    }
}

#[derive(Debug)]
pub enum KeySpaceWriteError {
    /// Attempted to write more than the maximum allowed bytes at a given key.
    ValueSizeExceeded,
    /// The write offset exceeds the current length of the stored value.
    InvalidOffset,
}

/// A key space in the durable storage.
///
/// A `KeySpace` is a flat key-value store. Instances are created via
/// [`KeySpaceLoader::load_or_create`].
pub trait KeySpace {
    /// Read the whole value associated with the key.
    /// Returns `None` if the key does not exist.
    fn get(&self, key: &Key) -> Option<Vec<u8>>;

    /// Read a portion of the value associated with the key and return the number of bytes written.
    /// Returns `None` if the key does not exist.
    fn read(&self, key: &Key, offset: usize, buffer: &mut [u8]) -> Option<usize>;

    /// Write the given value to the key.
    fn set(
        &mut self,
        key: &Key,
        value: impl AsRef<[u8]>,
    ) -> Result<(), KeySpaceWriteError>;

    /// Write data to the value at the given key starting at the given offset.
    /// Returns the number of bytes written.
    fn write(
        &mut self,
        key: &Key,
        offset: usize,
        data: impl AsRef<[u8]>,
    ) -> Result<usize, KeySpaceWriteError>;

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

    /// Obtain the hash that represents the current state of the key space. This hash is
    /// sensitive to the key-value associations and the order in which they were added.
    fn hash(&self) -> Vec<u8>;
}

/// Error returned by [`KeySpaceLoader::load_or_create`].
#[derive(Debug)]
pub enum KeySpaceLoaderError {
    /// A key space whose name overlaps (is a prefix of, or has as prefix) the
    /// requested name is already loaded.
    Overlapping,
    /// A key space with this exact name is already loaded and has not been
    /// dropped yet.
    AlreadyLoaded,
}

/// A loader for [`KeySpace`] instances backed by a specific storage implementation.
///
/// The loader tracks which keyspace names have been used and prevents
/// overlapping names. Callers receive an owned key space handle; when the
/// handle is dropped, the same name can be reloaded, but names that overlap
/// with any previously loaded name are permanently rejected.
pub trait KeySpaceLoader {
    /// The type of key space produced by this loader.
    type KeySpace: KeySpace;

    /// Load or create a key space with the given name.
    ///
    /// Returns an owned key space handle. The caller may hold multiple
    /// key spaces simultaneously as long as their names do not overlap.
    ///
    /// Returns an error if the requested name overlaps with any
    /// previously loaded key space, or if a key space with this exact
    /// name is already loaded.
    fn load_or_create(
        &mut self,
        name: Name,
    ) -> Result<Self::KeySpace, KeySpaceLoaderError>;
}
