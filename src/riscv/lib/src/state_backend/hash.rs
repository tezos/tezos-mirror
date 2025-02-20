// SPDX-FileCopyrightText: 2024-2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Common type for hashes

use std::num::NonZeroUsize;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum HashError {
    #[error("BLAKE2b hashing error")]
    HashingError(#[from] tezos_crypto_rs::blake2b::Blake2bError),

    #[error("Invalid digest size")]
    InvalidDigestSize,

    #[error("Serialization error: {0}")]
    SerializationError(#[from] bincode::Error),

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),

    #[error("The input buffer was expected to be non-empty")]
    NonEmptyBufferExpected,
}

/// Size of digest produced by the underlying hash function
pub const DIGEST_SIZE: usize = 32;

/// A value of type [struct@Hash] indicates that the enclosed array is a digest
/// produced by a preset hash function, currently BLAKE2b. It can be obtained
/// by either hashing data directly or after hashing by converting from
/// a suitably sized byte slice or vector.
#[derive(
    Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize, Hash, PartialOrd, Ord,
)]
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

    /// Combine multiple [`struct@Hash`] values into a single one.
    ///
    /// The hashes are combined by concatenating them, then hashing the result.
    /// Pre-image resistance is not compromised because the concatenation is not
    /// ambiguous, with hashes having a fixed size ([`DIGEST_SIZE`]).
    pub fn combine(hashes: &[Hash]) -> Result<Hash, HashError> {
        let mut input: Vec<u8> = Vec::with_capacity(DIGEST_SIZE * hashes.len());

        hashes
            .iter()
            .for_each(|h| input.extend_from_slice(h.as_ref()));

        // TODO RV-250: Instead of building the whole input and hashing it,
        // we should use incremental hashing, which isn't currently supported
        // in `tezos_crypto_rs`.
        Hash::blake2b_hash_bytes(&input)
    }
}

impl std::fmt::Display for Hash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        hex::encode(self.digest).fmt(f)
    }
}

impl std::fmt::Debug for Hash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
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

/// Writer which hashes fixed-sized chunks of data and produces the digests.
pub struct HashWriter {
    size: usize,
    buffer: Vec<u8>,
    hashes: Vec<Hash>,
}

impl HashWriter {
    /// Initialise a new writer with the given `size`.
    pub fn new(size: NonZeroUsize) -> Self {
        let size = size.get();
        Self {
            size,
            hashes: Vec::new(),
            buffer: Vec::with_capacity(size),
        }
    }

    /// Finalise the writer by hashing any remaining data and returning the vector
    /// of hashes.
    pub fn finalise(mut self) -> Result<Vec<Hash>, HashError> {
        if !self.buffer.is_empty() {
            self.flush_buffer()?;
        }
        Ok(self.hashes)
    }

    /// Hash the contents of the buffer.
    fn flush_buffer(&mut self) -> Result<(), HashError> {
        let hash = Hash::blake2b_hash_bytes(&self.buffer)?;
        self.hashes.push(hash);
        self.buffer.clear();
        Ok(())
    }
}

impl std::io::Write for HashWriter {
    fn write(&mut self, mut buf: &[u8]) -> std::io::Result<usize> {
        let consumed = buf.len();

        while !buf.is_empty() {
            let rem_buffer_len = self.size - self.buffer.len();
            let new_buf_len = std::cmp::min(rem_buffer_len, buf.len());

            let new_buf = &buf[..new_buf_len];
            buf = &buf[new_buf_len..];
            self.buffer.extend_from_slice(new_buf);

            // If the buffer has been completely filled, flush it.
            if rem_buffer_len == new_buf_len {
                self.flush_buffer().map_err(std::io::Error::other)?;
            }
        }
        Ok(consumed)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

/// Compute the Merkle hash of a vector of leaf hashes by building a Merkle tree
/// with the given `arity`. The last node in every level might have
/// a smaller arity.
///
/// # Panics
/// Panics if `arity < 2`.
pub(crate) fn build_custom_merkle_hash(
    arity: usize,
    mut nodes: Vec<Hash>,
) -> Result<Hash, HashError> {
    assert!(arity >= 2, "Arity must be at least 2");

    if nodes.is_empty() {
        return Err(HashError::NonEmptyBufferExpected);
    }

    let mut next_level = Vec::with_capacity(nodes.len().div_ceil(arity));

    while nodes.len() > 1 {
        // Group the nodes into chunks of size `arity` and hash each chunk.
        for chunk in nodes.chunks(arity) {
            next_level.push(Hash::combine(chunk)?)
        }

        std::mem::swap(&mut nodes, &mut next_level);
        next_level.truncate(0);
    }

    Ok(nodes[0])
}
