// Copyright (c) SimpleStaking, Viable Systems and Tezedge Contributors
// SPDX-CopyrightText: 2023-2024 TriliTech <contact@trili.tech>
// SPDX-CopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT
#![forbid(unsafe_code)]

use thiserror::Error;

#[macro_use]
pub mod blake2b;
pub mod base58;
#[cfg(feature = "bls")]
pub mod bls;
pub mod crypto_box;
#[macro_use]
pub mod hash;
pub mod nonce;
pub mod proof_of_work;
pub mod public_key;
pub mod public_key_hash;
pub mod signature;
use std::sync::Arc;

use ed25519_dalek::SignatureError as Ed25519Error;

#[derive(Debug, Clone)]
pub struct Ed25519ErrorHandle(Arc<ed25519_dalek::SignatureError>);

impl From<Ed25519Error> for Ed25519ErrorHandle {
    fn from(e: Ed25519Error) -> Self {
        Self(Arc::new(e))
    }
}

impl PartialEq for Ed25519ErrorHandle {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Ed25519ErrorHandle {}

#[derive(Debug, PartialEq, Eq, Clone, Error)]
pub enum CryptoError {
    #[error("Invalid crypto key, reason: {reason}")]
    InvalidKey { reason: String },
    #[error("Invalid crypto key size - expected: {expected}, actual: {actual}")]
    InvalidKeySize { expected: usize, actual: usize },
    #[error("Invalid nonce size - expected: {expected}, actual: {actual}")]
    InvalidNonceSize { expected: usize, actual: usize },
    #[error("Failed to decrypt")]
    FailedToDecrypt,
    #[error("Failed to construct public key")]
    InvalidPublicKey,
    #[error("Failed to construct signature")]
    InvalidSignature,
    #[error("Failed to construct message")]
    InvalidMessage,
    #[error("Unsupported algorithm `{0}`")]
    Unsupported(&'static str),
    #[error("Algorithm error: `{0}`")]
    AlgorithmError(String),
    #[error("Ed25519 error: {0:?}")]
    Ed25519(Ed25519ErrorHandle),
    #[error("Incorrect signature type: {0}")]
    SignatureType(#[from] signature::TryFromSignatureError),
}

/// Public key that support hashing.
pub trait PublicKeyWithHash {
    type Hash;

    fn pk_hash(&self) -> Self::Hash;
}

/// Public key that supports signature verification
pub trait PublicKeySignatureVerifier {
    type Signature;
    type Error;

    fn verify_signature(
        &self,
        signature: &Self::Signature,
        msg: &[u8],
    ) -> Result<bool, Self::Error>;
}
