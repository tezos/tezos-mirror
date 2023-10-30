// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Trilitech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT
use core::str::Utf8Error;
use evm_execution::account_storage::AccountStorageError;
use evm_execution::{DurableStorageError, EthereumError};
use primitive_types::U256;
use rlp::DecoderError;
use tezos_data_encoding::enc::BinError;
use tezos_ethereum::tx_common::SigError;
use tezos_smart_rollup_encoding::entrypoint::EntrypointError;
use tezos_smart_rollup_encoding::michelson::ticket::TicketError;
use tezos_smart_rollup_host::path::PathError;
use tezos_smart_rollup_host::runtime::RuntimeError;
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
}

#[derive(Error, Debug, Eq, PartialEq)]
pub enum StorageError {
    #[error(transparent)]
    Path(PathError),
    #[error(transparent)]
    Runtime(RuntimeError),
    #[error(transparent)]
    Storage(tezos_smart_rollup_storage::StorageError),
    #[error(transparent)]
    AccountStorage(AccountStorageError),
    #[error("Storage error: index out of bound")]
    IndexOutOfBounds,
    #[error("Storage error: failed to initialize an account")]
    AccountInitialisation,
    #[error("Storage error: failed to initialize a genesis account")]
    GenesisAccountInitialisation,
    #[error("Storage error: error while reading a value (incorrect size). Expected {expected} but got {actual}")]
    InvalidLoadValue { expected: usize, actual: usize },
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
}

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Transfer(TransferError),
    #[error(transparent)]
    Storage(StorageError),
    #[error("Invalid conversion")]
    InvalidConversion,
    #[error("Invalid parsing")]
    InvalidParsing,
    #[error(transparent)]
    InvalidRunTransaction(EthereumError),
    #[error("Simulation failed: {0}")]
    Simulation(EthereumError),
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
    fn from(_: DecoderError) -> Self {
        Self::InvalidConversion
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

impl From<DurableStorageError> for Error {
    fn from(e: DurableStorageError) -> Self {
        match e {
            DurableStorageError::PathError(e) => Self::Storage(StorageError::Path(e)),
            DurableStorageError::RuntimeError(e) => {
                Self::Storage(StorageError::Runtime(e))
            }
        }
    }
}

impl From<tezos_smart_rollup_storage::StorageError> for Error {
    fn from(e: tezos_smart_rollup_storage::StorageError) -> Self {
        Self::Storage(StorageError::Storage(e))
    }
}

impl From<AccountStorageError> for Error {
    fn from(e: AccountStorageError) -> Self {
        Self::Storage(StorageError::AccountStorage(e))
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
