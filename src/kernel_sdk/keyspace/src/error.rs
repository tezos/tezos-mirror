// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Errors of a loaded [`KeySpace`] and of its loader.
//!
//! The errors of key and name construction live next to [`Key`] and
//! [`Name`] in the crate root.
//!
//! [`KeySpace`]: crate::KeySpace

use crate::{Key, Name, NameError};

/// An operation at a key of a [`KeySpace`] that failed, and where.
///
/// Every fallible operation of a key space and of its extension traits
/// returns this error.
///
/// [`KeySpace`]: crate::KeySpace
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
#[error("at key {key} of key space {keyspace}: {kind}")]
pub struct KeySpaceError {
    keyspace: Name,
    key: Key,
    kind: ErrorKind,
}

impl KeySpaceError {
    /// Builds the error of a typed read that failed on `kind` at `key` of the
    /// key space named `keyspace`.
    pub fn read(keyspace: &Name, key: &Key, kind: ReadKind) -> Self {
        Self::new(keyspace, key, ErrorKind::Read(kind))
    }

    /// Builds the error of a write that failed on `kind` at `key` of the key
    /// space named `keyspace`.
    pub fn write(keyspace: &Name, key: &Key, kind: WriteKind) -> Self {
        Self::new(keyspace, key, ErrorKind::Write(kind))
    }

    fn new(keyspace: &Name, key: &Key, kind: ErrorKind) -> Self {
        Self {
            keyspace: keyspace.clone(),
            key: key.clone(),
            kind,
        }
    }
}

/// What the operation behind a [`KeySpaceError`] failed on, by family.
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum ErrorKind {
    /// A typed read found bytes that do not decode as the requested type.
    #[error(transparent)]
    Read(#[from] ReadKind),

    /// A write did not reach the storage, or the storage refused it.
    #[error(transparent)]
    Write(#[from] WriteKind),
}

/// What a typed read from a [`KeySpace`] can fail on.
///
/// [`KeySpace`]: crate::KeySpace
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum ReadKind {
    /// The bytes at the key are not the rlp encoding of the requested type.
    #[cfg(feature = "rlp")]
    #[error("value does not decode as rlp: {0}")]
    Rlp(#[from] rlp::DecoderError),

    /// The bytes at the key are not exactly one binary encoding of the
    /// requested type.
    #[cfg(feature = "tezos-encoding")]
    #[error("value does not decode: {0}")]
    Nom(#[from] tezos_data_encoding::nom::error::NomReadExactError),
}

/// What a write to a [`KeySpace`] can fail on.
///
/// [`KeySpace`]: crate::KeySpace
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum WriteKind {
    /// Attempted to write more than the maximum allowed bytes at a given key.
    #[error("value size exceeded the maximum allowed")]
    ValueSizeExceeded,

    /// The write offset exceeds the current length of the stored value.
    #[error("write offset exceeds the current length of the stored value")]
    InvalidOffset,

    /// The value does not encode, so nothing was written.
    #[cfg(feature = "tezos-encoding")]
    #[error("value does not encode: {0}")]
    Encode(#[from] tezos_data_encoding::enc::BinError),
}

/// Error returned by [`KeySpaceLoader::load_or_create`].
///
/// [`KeySpaceLoader::load_or_create`]: crate::KeySpaceLoader::load_or_create
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum KeySpaceLoaderError {
    /// A key space whose name overlaps (is a prefix of, or has as prefix) the
    /// requested name is already loaded. Only meaningful when names form a
    /// hierarchical path, which is why this variant is gated on `irmin-compat`.
    #[cfg(feature = "irmin-compat")]
    #[error("key space name overlaps an already-loaded key space")]
    Overlapping,

    /// A key space with this exact name is already loaded and has not been
    /// dropped yet.
    #[error("a key space with this name is already loaded")]
    AlreadyLoaded,

    /// The requested name is not a valid key space name.
    #[error("invalid key space name: {0}")]
    InvalidName(#[from] NameError),

    /// Invalid/Inconsistent storage detected
    #[cfg(not(feature = "irmin-compat"))]
    #[error("KeySpaceLoader encountered a malformed name mapping for {0}")]
    InconsistentNameMapping(Name),

    /// Only up to `i32::MAX` databases are supported.
    #[cfg(not(feature = "irmin-compat"))]
    #[error("Could not allocated database for the given name - ran out of db indices.")]
    TooManyDatabases,
}
