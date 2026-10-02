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
use core::convert::Infallible;

/// An operation at a key of a [`KeySpace`] that failed, and where.
///
/// Every fallible operation of a key space and of its extension traits
/// returns this error with both parameters [`Infallible`]: no value of that
/// type exists, so the error holds only the failures that the SDK defines.
///
/// A crate outside the SDK that adds its own reads or writes over a key space
/// can report its own failures in this error. It sets `R` to its read
/// failures, `W` to its write failures, or both, and builds them with
/// [`ReadKind::Ext`] or [`WriteKind::Ext`]. [`KeySpaceError::widen`] carries
/// the failures of the SDK into that error. A crate error that holds the
/// extended error in one variant implements `From<KeySpaceError>` with
/// [`KeySpaceError::widen`], so that `?` takes both kinds of failure:
///
/// ```ignore
/// #[derive(Debug, thiserror::Error)]
/// enum Error {
///     #[error(transparent)]
///     KeySpace(#[from] KeySpaceError<MyReadKind>),
/// }
///
/// impl From<KeySpaceError> for Error {
///     fn from(e: KeySpaceError) -> Self {
///         Self::KeySpace(e.widen())
///     }
/// }
/// ```
///
/// [`KeySpace`]: crate::KeySpace
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
#[error("at key {key} of key space {keyspace}: {kind}")]
pub struct KeySpaceError<R = Infallible, W = Infallible> {
    keyspace: Name,
    key: Key,
    kind: ErrorKind<R, W>,
}

impl<R, W> KeySpaceError<R, W> {
    /// Builds the error of a typed read that failed on `kind` at `key` of the
    /// key space named `keyspace`.
    pub fn read(keyspace: &Name, key: &Key, kind: ReadKind<R>) -> Self {
        Self::new(keyspace, key, ErrorKind::Read(kind))
    }

    /// Builds the error of a write that failed on `kind` at `key` of the key
    /// space named `keyspace`.
    pub fn write(keyspace: &Name, key: &Key, kind: WriteKind<W>) -> Self {
        Self::new(keyspace, key, ErrorKind::Write(kind))
    }

    /// Returns what the operation failed on.
    pub fn kind(&self) -> &ErrorKind<R, W> {
        &self.kind
    }

    fn new(keyspace: &Name, key: &Key, kind: ErrorKind<R, W>) -> Self {
        Self {
            keyspace: keyspace.clone(),
            key: key.clone(),
            kind,
        }
    }
}

impl KeySpaceError {
    /// Returns the same failure, at the same key, as an error whose read and
    /// write failures an extension extends.
    pub fn widen<R, W>(self) -> KeySpaceError<R, W> {
        let kind = match self.kind {
            ErrorKind::Read(kind) => ErrorKind::Read(kind.widen()),
            ErrorKind::Write(kind) => ErrorKind::Write(kind.widen()),
        };
        KeySpaceError {
            keyspace: self.keyspace,
            key: self.key,
            kind,
        }
    }
}

/// What the operation behind a [`KeySpaceError`] failed on, by family.
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum ErrorKind<R = Infallible, W = Infallible> {
    /// A typed read found bytes that do not decode as the requested type.
    #[error(transparent)]
    Read(#[from] ReadKind<R>),

    /// A write did not reach the storage, or the storage refused it.
    #[error(transparent)]
    Write(#[from] WriteKind<W>),
}

/// What a typed read from a [`KeySpace`] can fail on.
///
/// [`KeySpace`]: crate::KeySpace
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum ReadKind<E = Infallible> {
    /// The bytes at the key are not the rlp encoding of the requested type.
    #[cfg(feature = "rlp")]
    #[error("value does not decode as rlp: {0}")]
    Rlp(#[from] rlp::DecoderError),

    /// The bytes at the key are not exactly one binary encoding of the
    /// requested type.
    #[cfg(feature = "tezos-encoding")]
    #[error("value does not decode: {0}")]
    Nom(#[from] tezos_data_encoding::nom::error::NomReadExactError),

    /// A read failure that an extension outside the SDK defines.
    #[error(transparent)]
    Ext(E),
}

impl ReadKind {
    fn widen<E>(self) -> ReadKind<E> {
        match self {
            #[cfg(feature = "rlp")]
            Self::Rlp(err) => ReadKind::Rlp(err),
            #[cfg(feature = "tezos-encoding")]
            Self::Nom(err) => ReadKind::Nom(err),
        }
    }
}

/// What a write to a [`KeySpace`] can fail on.
///
/// [`KeySpace`]: crate::KeySpace
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum WriteKind<E = Infallible> {
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

    /// A write failure that an extension outside the SDK defines.
    #[error(transparent)]
    Ext(E),
}

impl WriteKind {
    fn widen<E>(self) -> WriteKind<E> {
        match self {
            Self::ValueSizeExceeded => WriteKind::ValueSizeExceeded,
            Self::InvalidOffset => WriteKind::InvalidOffset,
            #[cfg(feature = "tezos-encoding")]
            Self::Encode(err) => WriteKind::Encode(err),
        }
    }
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
