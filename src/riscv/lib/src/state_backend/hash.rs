// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
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

    #[error("The input buffer was expected to be non-empty")]
    NonEmptyBufferExpected,
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

impl std::fmt::Display for Hash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        hex::encode(self.digest).fmt(f)
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

pub trait RootHashable {
    /// Build the root hash corresponding to the Merkle tree described by the
    /// layout of the data.
    fn hash(&self) -> Result<Hash, HashError>;
}

impl<T: RootHashable> RootHashable for &T {
    fn hash(&self) -> Result<Hash, HashError> {
        T::hash(self)
    }
}

impl RootHashable for Hash {
    fn hash(&self) -> Result<Hash, HashError> {
        Ok(*self)
    }
}

impl RootHashable for () {
    fn hash(&self) -> Result<Hash, HashError> {
        Hash::blake2b_hash(())
    }
}

impl<T: RootHashable> RootHashable for [T] {
    fn hash(&self) -> Result<Hash, HashError> {
        let mut hashes: Vec<u8> = Vec::with_capacity(DIGEST_SIZE * self.len());

        self.iter().try_for_each(|e| {
            hashes.extend_from_slice(e.hash()?.as_ref());
            Ok::<(), HashError>(())
        })?;

        // TODO RV-250: Instead of building the whole input and hashing it,
        // we should use incremental hashing, which isn't currently supported
        // in `tezos_crypto_rs`.
        Hash::blake2b_hash_bytes(&hashes)
    }
}

impl<T: RootHashable> RootHashable for Vec<T> {
    fn hash(&self) -> Result<Hash, HashError> {
        let values: &[T] = self.as_ref();
        values.hash()
    }
}

impl<T: RootHashable, const N: usize> RootHashable for [T; N] {
    fn hash(&self) -> Result<Hash, HashError> {
        self.as_ref().hash()
    }
}

macro_rules! impl_roothash_for_tuple {
    ($($name:ident),*) => {
        impl<$($name: RootHashable),*> RootHashable for ($($name,)*) {
            fn hash(&self) -> Result<Hash, HashError> {
                #[allow(non_snake_case)]
                let ($($name,)*) = self;
                let hashes: Result<Vec<Hash>, HashError> = [$($name.hash(),)*].into_iter().collect();
                hashes?.hash()
            }
        }
    }
}

impl_roothash_for_tuple!(A, B);
impl_roothash_for_tuple!(A, B, C);
impl_roothash_for_tuple!(A, B, C, D);
impl_roothash_for_tuple!(A, B, C, D, E);
impl_roothash_for_tuple!(A, B, C, D, E, F);

/// Writer which hashes fixed-sized chunks of data and produces the digests.
pub struct HashWriter {
    size: usize,
    buffer: Vec<u8>,
    hashes: Vec<Hash>,
}

impl HashWriter {
    /// Initialise a new writer with the given `size`.
    ///
    /// # Panics
    /// Panics if `size == 0`.
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
        // TODO RV-250: Instead of building the whole input and hashing it,
        // we should use incremental hashing, which isn't currently supported
        // in `tezos_crypto_rs`.
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
            next_level.push(chunk.hash()?)
        }

        std::mem::swap(&mut nodes, &mut next_level);
        next_level.truncate(0);
    }

    Ok(nodes[0])
}
