// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023-2026 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Trilitech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT
use core::str::Utf8Error;
use num_bigint::{BigUint, TryFromBigIntError};
use primitive_types::U256;
use rlp::DecoderError;
use tezos_data_encoding::enc::BinError;
use tezos_ethereum::tx_common::SigError;
use tezos_indexable_storage::IndexableStorageError;
use tezos_smart_rollup_encoding::entrypoint::EntrypointError;
use tezos_smart_rollup_encoding::michelson::ticket::TicketError;
use tezos_smart_rollup_host::path::PathError;
use tezos_smart_rollup_host::runtime::RuntimeError;
use tezos_storage::error::Error as GenStorageError;
use tezos_tezlink::enc_wrappers::BlockNumberOverflowError;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum TransferError {
    #[error("Transaction error: couldn't reconstruct the caller address")]
    InvalidCallerAddress,
    #[error("Transaction error: couldn't verify the signature")]
    InvalidSignature,
    #[error("Transaction error: incorrect nonce, expected {expected} but got {actual}")]
    InvalidNonce { expected: U256, actual: U256 },
    #[error("Transaction error: not enough funds to apply transaction")]
    NotEnoughBalance,
    #[error("Transaction error: cumulative gas overflowed")]
    CumulativeGasUsedOverflow,
    #[error("Transaction error: invalid address format {0}")]
    InvalidAddressFormat(Utf8Error),
    #[error("Transaction error: invalid chain id {expected} but got {actual}")]
    InvalidChainId { expected: U256, actual: U256 },
    #[error("Transaction error: {0}")]
    Custom(&'static str),
}

#[derive(Error, Debug, Eq, PartialEq)]
pub enum StorageError {
    #[error(transparent)]
    Path(PathError),
    #[error(transparent)]
    Runtime(RuntimeError),
    #[error(transparent)]
    Storage(tezos_smart_rollup_storage::StorageError),
    #[error("Storage error: index out of bound")]
    IndexOutOfBounds,
    #[error("Storage error: failed to initialize an account")]
    AccountInitialisation,
    #[error("Storage error: failed to initialize a genesis account")]
    GenesisAccountInitialisation,
    #[error("Storage error: error while reading a value (incorrect size). Expected {expected} but got {actual}")]
    InvalidLoadValue { expected: usize, actual: usize },
    #[error("Storage error: storing the current block hash failed")]
    BlockHashStorageFailed,
}

#[derive(Error, Debug)]
pub enum UpgradeProcessError {
    #[error("Internal upgrade error: {0}")]
    InternalUpgrade(&'static str),
    #[error("Fallback mechanism was triggered")]
    Fallback,
    #[error("Missing dictator key")]
    NoDictator,
}

#[derive(Error, Debug)]
pub enum EncodingError {
    #[error("Invalid ticket")]
    Ticket(TicketError),
    #[error("Invalid entrypoint")]
    Entrypoint(EntrypointError),
    #[error("Invalid serialization")]
    Bin(BinError),
    // BinWriter error comes from the storage crate which need to
    // implement PartialEq + Display which is why we're keeping a
    // String instead of a BinError
    #[error("Storage error: Failed to encode a value with BinWriter: {0}")]
    BinWriterError(String),
}

#[derive(Error, Debug)]
pub enum TezlinkSimulationError {
    #[error("Unexpected end of inbox")]
    UnexpectedEndOfInbox,
    #[error("Unexpected simulation input: {0:?}, expecting either 0x00 to skip signature checking or 0x01 to check signature")]
    UnexpectedSkipSigTag(Vec<u8>),
}

#[derive(Error, Debug)]
#[allow(clippy::enum_variant_names)]
pub enum Error {
    #[error(transparent)]
    Transfer(TransferError),
    #[error(transparent)]
    Storage(StorageError),
    #[error("Invalid conversion")]
    InvalidConversion,
    #[error("Failed to decode: {0}")]
    RlpDecoderError(DecoderError),
    #[error("Storage error: Failed to decode a value with NomReader: {0}")]
    NomReadError(String),
    #[error("Invalid parsing")]
    InvalidParsing,
    #[error(transparent)]
    InvalidRunTransaction(revm_etherlink::Error),
    #[error("Simulation failed: {0}")]
    Simulation(revm_etherlink::Error),
    #[error("Tezlink simulation failed: {0}")]
    TezlinkSimulation(TezlinkSimulationError),
    #[error(transparent)]
    UpgradeError(UpgradeProcessError),
    #[error(transparent)]
    InvalidSignature(SigError),
    #[error("Invalid signature check")]
    InvalidSignatureCheck,
    #[error("Issue during reboot")]
    Reboot,
    #[error(transparent)]
    Encoding(EncodingError),
    #[error("Tried casting an Implicit account into an Originated account")]
    ImplicitToOriginated,
    #[error("Tried casting an Originated account into an Implicit account")]
    OriginatedToImplicit,
    #[error("Error while initializing REVM precompile bytecodes")]
    RevmPrecompileInitError,
    #[error("Overflow occurred during an arithmetic operation")]
    Overflow(String),
    #[error("Typechecking error: {0}")]
    TcError(String),
    #[error("BigInt conversion error: {0}")]
    TryFromBigIntError(TryFromBigIntError<BigUint>),
    #[error("Unexpected error from native bridge: {0}")]
    BridgeError(String),
    #[error(transparent)]
    BlockNumberOverflowError(BlockNumberOverflowError),
}

impl From<revm_etherlink::Error> for Error {
    fn from(err: revm_etherlink::Error) -> Self {
        Error::Simulation(err)
    }
}

impl From<PathError> for StorageError {
    fn from(e: PathError) -> Self {
        Self::Path(e)
    }
}
impl From<RuntimeError> for StorageError {
    fn from(e: RuntimeError) -> Self {
        Self::Runtime(e)
    }
}

impl From<PathError> for Error {
    fn from(e: PathError) -> Self {
        Self::Storage(StorageError::Path(e))
    }
}
impl From<RuntimeError> for Error {
    fn from(e: RuntimeError) -> Self {
        Self::Storage(StorageError::Runtime(e))
    }
}

impl From<TransferError> for Error {
    fn from(e: TransferError) -> Self {
        Self::Transfer(e)
    }
}

impl From<DecoderError> for Error {
    fn from(e: DecoderError) -> Self {
        Self::RlpDecoderError(e)
    }
}

impl From<UpgradeProcessError> for Error {
    fn from(e: UpgradeProcessError) -> Self {
        Self::UpgradeError(e)
    }
}

impl From<StorageError> for Error {
    fn from(e: StorageError) -> Self {
        Self::Storage(e)
    }
}

impl From<tezos_smart_rollup_storage::StorageError> for Error {
    fn from(e: tezos_smart_rollup_storage::StorageError) -> Self {
        Self::Storage(StorageError::Storage(e))
    }
}

impl From<TicketError> for Error {
    fn from(e: TicketError) -> Self {
        Self::Encoding(EncodingError::Ticket(e))
    }
}

impl From<EntrypointError> for Error {
    fn from(e: EntrypointError) -> Self {
        Self::Encoding(EncodingError::Entrypoint(e))
    }
}

impl From<BinError> for Error {
    fn from(e: BinError) -> Self {
        Self::Encoding(EncodingError::Bin(e))
    }
}

impl From<IndexableStorageError> for Error {
    fn from(e: IndexableStorageError) -> Self {
        match e {
            IndexableStorageError::Path(e) => Error::Storage(StorageError::Path(e)),
            IndexableStorageError::Runtime(e) => Error::Storage(StorageError::Runtime(e)),
            IndexableStorageError::Storage(e) => Error::Storage(StorageError::Storage(e)),
            IndexableStorageError::IndexOutOfBounds => {
                Error::Storage(StorageError::IndexOutOfBounds)
            }
            IndexableStorageError::RlpDecoderError(e) => Error::RlpDecoderError(e),
            IndexableStorageError::InvalidLoadValue { expected, actual } => {
                Error::Storage(StorageError::InvalidLoadValue { expected, actual })
            }
            IndexableStorageError::BinWriteError(msg) => {
                Error::Encoding(EncodingError::BinWriterError(msg))
            }
            IndexableStorageError::NomReadError(msg) => Error::NomReadError(msg),
            IndexableStorageError::ImplicitToOriginated => Error::ImplicitToOriginated,
            IndexableStorageError::OriginatedToImplicit => Error::OriginatedToImplicit,
            IndexableStorageError::TcError(msg) => Error::TcError(msg),
            IndexableStorageError::TryFromBigIntError(msg) => {
                Error::TryFromBigIntError(msg)
            }
        }
    }
}

impl From<GenStorageError> for Error {
    fn from(e: GenStorageError) -> Self {
        match e {
            GenStorageError::Path(e) => Error::Storage(StorageError::Path(e)),
            GenStorageError::Runtime(e) => Error::Storage(StorageError::Runtime(e)),
            GenStorageError::Storage(e) => Error::Storage(StorageError::Storage(e)),
            GenStorageError::RlpDecoderError(e) => Error::RlpDecoderError(e),
            GenStorageError::InvalidLoadValue { expected, actual } => {
                Error::Storage(StorageError::InvalidLoadValue { expected, actual })
            }
            GenStorageError::BinWriteError(msg) => {
                Error::Encoding(EncodingError::BinWriterError(msg))
            }
            GenStorageError::NomReadError(msg) => Error::NomReadError(msg),
            GenStorageError::ImplicitToOriginated => Error::ImplicitToOriginated,
            GenStorageError::OriginatedToImplicit => Error::OriginatedToImplicit,
            GenStorageError::TcError(msg) => Error::TcError(msg),
            GenStorageError::TryFromBigIntError(msg) => Error::TryFromBigIntError(msg),
        }
    }
}

impl From<BlockNumberOverflowError> for Error {
    fn from(e: BlockNumberOverflowError) -> Self {
        Self::BlockNumberOverflowError(e)
    }
}
