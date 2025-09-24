// Copyright (c) SimpleStaking, Viable Systems and Tezedge Contributors
// SPDX-License-Identifier: MIT

//! This module wrapps [`sodiumoxide::crypto::box_::`] stuff,
//! which is used for encrypt/decrypt messages between peers.
//!
//! Terminology:
//!
//! PublicKey - [`CRYPTO_KEY_SIZE`]-bytes
//! SecretKey - [`CRYPTO_KEY_SIZE`]-bytes
//! PrecomputedKey - [`CRYPTO_KEY_SIZE`]-bytes created from PublicKey and SecretKey
//!
//! CryptoboxPublicKeyHash - generated as a hash of [`PublicKey`], for example used as a peer_id

use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::fmt::{self, Debug};

use hex::{FromHex, FromHexError};

use crate::{blake2b::Blake2bError, CryptoError};

use super::hash::CryptoboxPublicKeyHash;

use thiserror::Error;

pub const BOX_ZERO_BYTES: usize = 32;
pub const CRYPTO_KEY_SIZE: usize = 32;

pub trait CryptoKey: Sized {
    fn from_bytes<B: AsRef<[u8]>>(buf: B) -> Result<Self, CryptoError>;
}

#[derive(Debug, Error, PartialEq)]
pub enum PublicKeyError {
    #[error("Blake2b digest error: {0}")]
    Blake2bError(#[from] Blake2bError),
}

fn ensure_crypto_key_bytes<B: AsRef<[u8]>>(buf: B) -> Result<[u8; CRYPTO_KEY_SIZE], CryptoError> {
    let buf = buf.as_ref();

    // check size
    if buf.len() != CRYPTO_KEY_SIZE {
        return Err(CryptoError::InvalidKeySize {
            expected: CRYPTO_KEY_SIZE,
            actual: buf.len(),
        });
    };

    // convert to correct key size
    let mut arr = [0u8; CRYPTO_KEY_SIZE];
    arr.copy_from_slice(buf);
    Ok(arr)
}

/// Convenience wrapper around [`sodiumoxide::crypto::box_::PublicKey`]
#[derive(Serialize, Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct PublicKey(Vec<u8>);

impl PublicKey {
    /// Generates public key hash for public key
    pub fn public_key_hash(&self) -> CryptoboxPublicKeyHash {
        CryptoboxPublicKeyHash::try_from(crate::blake2b::digest_128(self.0.as_ref())).unwrap()
    }
}

impl CryptoKey for PublicKey {
    fn from_bytes<B: AsRef<[u8]>>(buf: B) -> Result<Self, CryptoError> {
        ensure_crypto_key_bytes(buf).map(|key_bytes| PublicKey(key_bytes.to_vec()))
    }
}

impl AsRef<[u8]> for PublicKey {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

impl FromHex for PublicKey {
    type Error = CryptoError;

    fn from_hex<T: AsRef<[u8]>>(hex: T) -> Result<Self, Self::Error> {
        Self::from_bytes(hex::decode(hex)?)
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Eq, PartialEq)]
/// Convenience wrapper around [`sodiumoxide::crypto::box_::SecretKey`]
pub struct SecretKey(Vec<u8>);

impl CryptoKey for SecretKey {
    fn from_bytes<B: AsRef<[u8]>>(buf: B) -> Result<Self, CryptoError> {
        ensure_crypto_key_bytes(buf).map(|key_bytes| SecretKey(key_bytes.to_vec()))
    }
}

impl AsRef<[u8]> for SecretKey {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

impl FromHex for SecretKey {
    type Error = CryptoError;

    fn from_hex<T: AsRef<[u8]>>(hex: T) -> Result<Self, Self::Error> {
        Self::from_bytes(hex::decode(hex)?)
    }
}

/// Generates random keypair: [`PublicKey, SecretKey`] + [`CryptoboxPublicKeyHash`]
///
/// Note: Strange why it is called pair, bud returns triplet :)
pub fn random_keypair() -> Result<(SecretKey, PublicKey, CryptoboxPublicKeyHash), PublicKeyError> {
    // // generate
    // let (pk, sk) = box_::gen_keypair();

    // // wrap it
    // let sk = SecretKey(sk);
    // let pk = PublicKey(pk);

    // // generate public key hash
    // let pkh = pk.public_key_hash()?;

    // // return
    // Ok((sk, pk, pkh))
    todo!()
}

#[derive(Serialize, Deserialize, Eq, PartialEq, Clone)]
/// Convenience wrapper around [`sodiumoxide::crypto::box_::PrecomputedKey`]
pub struct PrecomputedKey(Vec<u8>);

impl PrecomputedKey {
    /// Create `PrecomputedKey` from public key and secret key
    ///
    /// # Arguments
    /// * `pk_as_hex_string` - Hex string representing public key
    /// * `sk_as_hex_string` - Hex string representing secret key
    pub fn precompute(_pk: &PublicKey, _sk: &SecretKey) -> Self {
        todo!()
    }

    pub fn from_bytes(_bytes: [u8; 42]) -> Self {
        todo!()
    }
}

impl Debug for PrecomputedKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "PrecomputedKey(****)")
    }
}

impl From<FromHexError> for CryptoError {
    fn from(e: FromHexError) -> Self {
        CryptoError::InvalidKey {
            reason: format!("{}", e),
        }
    }
}
