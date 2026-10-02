// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use num_bigint::{BigUint, TryFromBigIntError};
use rlp::DecoderError;
use tezos_data_encoding::enc::BinError;
use tezos_data_encoding::nom::error::DecodeError;
use tezos_smart_rollup_host::path::PathError;
use tezos_smart_rollup_host::runtime::RuntimeError;
use tezos_smart_rollup_keyspace::{KeyError, KeySpaceError};
use tezosx_types::{KernelStorageError, TezosXRuntimeError};
use thiserror::Error;

/// What a read of this crate can fail on, on top of the read failures of the
/// SDK.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum StorageReadErrorKind {
    /// A bounded read found a value longer than its bound.
    #[error("value of {length} bytes exceeds the bound of {max_bytes} bytes")]
    ValueExceedsBound {
        /// Length of the stored value, in bytes.
        length: usize,
        /// Bound of the read, in bytes.
        max_bytes: usize,
    },

    /// The key holds no value, and the caller requires one.
    #[error("no value at the key")]
    NotFound,
}

#[derive(Error, Debug, Eq, PartialEq)]
pub enum Error {
    #[error(transparent)]
    Path(PathError),
    /// A keyspace key is not valid, as [`KeyError`] describes.
    #[error(transparent)]
    Key(#[from] KeyError),
    /// An operation at a keyspace key failed, as [`KeySpaceError`] describes:
    /// a failure of the SDK, or a [`StorageReadErrorKind`] of this crate.
    #[error(transparent)]
    KeySpace(#[from] KeySpaceError<StorageReadErrorKind>),
    #[error(transparent)]
    Runtime(RuntimeError),
    #[error(transparent)]
    Storage(tezos_smart_rollup_storage::StorageError),
    #[error("Failed to decode: {0}")]
    RlpDecoderError(DecoderError),
    #[error("Storage error: error while reading a value (incorrect size). Expected {expected} but got {actual}")]
    InvalidLoadValue { expected: usize, actual: usize },
    #[error("Storage error: Failed to encode a value with BinWriter: {0}")]
    BinWriteError(String),
    #[error("Storage error: Failed to decode a value with NomReader: {0}")]
    NomReadError(String),
    #[error("Tried casting an Implicit account into an Originated account")]
    ImplicitToOriginated,
    #[error("Tried casting an Originated account into an Implicit account")]
    OriginatedToImplicit,
    #[error("Typechecking error: {0}")]
    TcError(String),
    #[error("BigInt conversion error: {0}")]
    TryFromBigIntError(TryFromBigIntError<BigUint>),
    #[error("Internal invariant violation: {0}")]
    Internal(String),
}

impl From<KeySpaceError> for Error {
    fn from(e: KeySpaceError) -> Self {
        Self::KeySpace(e.widen())
    }
}

impl From<TryFromBigIntError<BigUint>> for Error {
    fn from(e: TryFromBigIntError<BigUint>) -> Self {
        Error::TryFromBigIntError(e)
    }
}

impl From<PathError> for Error {
    fn from(e: PathError) -> Self {
        Self::Path(e)
    }
}
impl From<RuntimeError> for Error {
    fn from(e: RuntimeError) -> Self {
        Self::Runtime(e)
    }
}

impl From<DecoderError> for Error {
    fn from(e: DecoderError) -> Self {
        Self::RlpDecoderError(e)
    }
}

impl From<DecodeError<&[u8]>> for Error {
    fn from(value: DecodeError<&[u8]>) -> Self {
        let msg = format!("{value:?}");
        Self::NomReadError(msg)
    }
}

impl From<BinError> for Error {
    fn from(value: BinError) -> Self {
        let msg = format!("{value}");
        Self::BinWriteError(msg)
    }
}

impl From<Error> for KernelStorageError {
    fn from(e: Error) -> Self {
        KernelStorageError(e.to_string())
    }
}

impl From<Error> for TezosXRuntimeError {
    fn from(e: Error) -> Self {
        TezosXRuntimeError::Storage(KernelStorageError(e.to_string()))
    }
}
