// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use tezos_crypto_rs::{base58, CryptoError};

uniffi::setup_scaffolding!();

pub mod forge;
pub mod keys;

#[derive(Debug, uniffi::Error, thiserror::Error)]
#[uniffi(flat_error)]
pub enum Error {
    #[error("Base58check conversion failure: {0:?}")]
    Base58(#[from] base58::FromBase58CheckError),
    #[error("Forging failure: {0:?}")]
    Forge(#[from] forge::ForgingError),
    #[error("Cryptography failure: {0:?}")]
    Crypto(CryptoError),
}
