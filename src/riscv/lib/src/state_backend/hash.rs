// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Common type for hashes

use thiserror::Error;

#[derive(Error, Debug)]
pub enum HashError {
    #[error("BLAKE2b hashing error")]
    HashingError(#[from] tezos_crypto_rs::blake2b::Blake2bError),

    #[error("Invalid digest size")]
    InvalidDigestSize,

    #[error("Serialization error: {0}")]
    SerializationError(#[from] bincode::Error),
}

/// Size of digest produced by the underlying hash function
pub const DIGEST_SIZE: usize = 32;

/// A value of type [Hash] indicates that the enclosed array is a digest
/// produced by a preset hash function, currently BLAKE2b. It can be obtained
/// by either hashing data directly or after hashing by converting from
/// a suitably sized byte slice or vector.
#[derive(Clone, Copy, PartialEq, Debug, serde::Serialize, serde::Deserialize)]
pub struct Hash {
    digest: [u8; DIGEST_SIZE],
}

impl Hash {
    /// Hash a slice of bytes
    pub fn blake2b_hash_bytes(bytes: &[u8]) -> Result<Self, HashError> {
        tezos_crypto_rs::blake2b::digest_256(bytes).try_into()
    }

    /// Get the hash of a value that can be serialised by hashing its serialisation
    pub fn blake2b_hash<T: serde::Serialize>(data: T) -> Result<Self, HashError> {
        Self::blake2b_hash_bytes(&bincode::serialize(&data)?)
    }
}

impl TryFrom<&[u8]> for Hash {
    type Error = HashError;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        let digest: [u8; DIGEST_SIZE] =
            value.try_into().map_err(|_| HashError::InvalidDigestSize)?;
        Ok(Hash { digest })
    }
}

impl TryFrom<Vec<u8>> for Hash {
    type Error = HashError;

    fn try_from(value: Vec<u8>) -> Result<Self, Self::Error> {
        Hash::try_from(value.as_ref())
    }
}

impl From<Hash> for [u8; DIGEST_SIZE] {
    fn from(value: Hash) -> Self {
        value.digest
    }
}

impl From<[u8; DIGEST_SIZE]> for Hash {
    fn from(digest: [u8; DIGEST_SIZE]) -> Self {
        Hash { digest }
    }
}

impl AsRef<[u8]> for Hash {
    fn as_ref(&self) -> &[u8] {
        &self.digest
    }
}
