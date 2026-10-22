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
pub use tezosx_types::StorageReadErrorKind;
use tezosx_types::{KernelStorageError, TezosXRuntimeError};
use thiserror::Error;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum Error {
    #[error(transparent)]
    Path(PathError),
    /// A keyspace key is not valid, as [`KeyError`] describes.
    #[error(transparent)]
    Key(#[from] KeyError),
    /// An operation at a keyspace key failed, as [`KeySpaceError`] describes:
    /// a failure of the SDK, or a [`StorageReadErrorKind`].
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
