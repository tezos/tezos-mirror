// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use tezos_crypto_rs::{base58, CryptoError};

uniffi::setup_scaffolding!();

pub mod entrypoint;
pub mod forge;
pub mod hash;
pub mod keys;
pub mod micheline;
mod types;

#[derive(Debug, uniffi::Error, thiserror::Error)]
#[uniffi(flat_error)]
pub enum Error {
    #[error("Base58check conversion failure: {0:?}")]
    Base58(#[from] base58::FromBase58CheckError),
    #[error("Forging failure: {0:?}")]
    Forge(#[from] forge::ForgingError),
    #[error("Cryptography failure: {0:?}")]
    Crypto(CryptoError),
    #[error("Entrypoint failure: {0:?}")]
    Entrypoint(#[from] entrypoint::EntrypointError),
    #[error("Parsing failure: {0:?}")]
    Parsing(String),
}
