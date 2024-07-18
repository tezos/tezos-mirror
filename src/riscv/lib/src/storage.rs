// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use bincode::{deserialize, serialize};
use std::{
    io::{self, Write},
    path::{Path, PathBuf},
};
use thiserror::Error;

pub const DIGEST_SIZE: usize = 32;
const CHUNK_SIZE: usize = 4096;

#[derive(Error, Debug)]
pub enum StorageError {
    #[error("IO error: {0}")]
    IoError(#[from] io::Error),

    #[error("Serialization error: {0}")]
    CommitSerializationError(#[from] bincode::Error),

    #[error("Invalid repo")]
    InvalidRepo,

    #[error("BLAKE2b hashing error")]
    HashError(#[from] tezos_crypto_rs::blake2b::Blake2bError),

    #[error("Data for hash {0} not found")]
    NotFound(String),

    #[error("Commited chunk {0} not found")]
    ChunkNotFound(String),
}

pub type Hash = [u8; DIGEST_SIZE];

#[derive(Debug, PartialEq)]
pub struct Store {
    path: Box<Path>,
}

impl Store {
    /// Initialise a store. Either create a new directory if `path` does not
    /// exist or initialise in an existing directory.
    /// Throws `StorageError::InvalidRepo` if `path` is a file.
    pub fn init(path: impl AsRef<Path>) -> Result<Self, StorageError> {
        let path = path.as_ref().to_path_buf();
        if !path.exists() {
            std::fs::create_dir(&path)?;
        } else if path.metadata()?.is_file() {
            return Err(StorageError::InvalidRepo);
        }
        Ok(Store {
            path: path.into_boxed_path(),
        })
    }

    fn file_name_of_hash(hash: &Hash) -> String {
        hex::encode(hash)
    }

    fn path_of_hash(&self, hash: &Hash) -> PathBuf {
        self.path.join(Self::file_name_of_hash(hash))
    }

    fn write_data_if_new(&self, file_name: PathBuf, data: &[u8]) -> Result<(), StorageError> {
        match std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(file_name)
        {
            Ok(mut f) => f.write_all(data)?,
            Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => (),
            Err(e) => return Err(StorageError::IoError(e)),
        }
        Ok(())
    }

    /// Store data and return its hash. The data is written to disk only if
    /// previously unseen.
    pub fn store(&self, data: &[u8]) -> Result<Hash, StorageError> {
        // This is safe to unwrap because `digest_256` always returns
        // a `DIGEST_SIZE`-long `Vec<u8>`.
        let hash: Hash = tezos_crypto_rs::blake2b::digest_256(data)?
            .try_into()
            .unwrap();
        let file_name = self.path_of_hash(&hash);
        self.write_data_if_new(file_name, data)?;
        Ok(hash)
    }

    /// Load data corresponding to `hash`, if found.
    pub fn load(&self, hash: &Hash) -> Result<Vec<u8>, StorageError> {
        let file_name = self.path_of_hash(hash);
        std::fs::read(file_name).map_err(|e| {
            if e.kind() == io::ErrorKind::NotFound {
                StorageError::NotFound(hex::encode(hash))
            } else {
                StorageError::IoError(e)
            }
        })
    }

    /// Copy the data corresponding to `hash` to `path`.
    pub fn copy(&self, hash: &Hash, path: impl AsRef<Path>) -> Result<(), StorageError> {
        let source_path = self.path_of_hash(hash);
        let target_path = path.as_ref().join(Self::file_name_of_hash(hash));
        std::fs::copy(source_path, target_path)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub struct Repo {
    backend: Store,
}

impl Repo {
    /// Load or create new repo at `path`.
    pub fn load(path: impl AsRef<Path>) -> Result<Repo, StorageError> {
        Ok(Repo {
            backend: Store::init(path)?,
        })
    }

    pub fn close(self) {}

    /// Create a new commit for `bytes` and  return the commit id.
    pub fn commit(&mut self, bytes: &[u8]) -> Result<Hash, StorageError> {
        let mut commit = Vec::with_capacity(bytes.len().div_ceil(CHUNK_SIZE) * DIGEST_SIZE);

        for chunk in bytes.chunks(CHUNK_SIZE) {
            let chunk_hash = self.backend.store(chunk)?;
            commit.push(chunk_hash);
        }

        // A commit contains the list of all chunks needed to reconstruct `data`.
        let commit_bytes = serialize(&commit)?;
        self.backend.store(&commit_bytes)
    }

    /// Checkout the bytes committed under `id`, if the commit exists.
    pub fn checkout(&self, id: &Hash) -> Result<Vec<u8>, StorageError> {
        let bytes = self.backend.load(id)?;
        let commit: Vec<Hash> = deserialize(&bytes)?;
        let mut bytes = Vec::new();
        for hash in commit {
            let mut chunk = self.backend.load(&hash).map_err(|e| {
                if let StorageError::NotFound(hash) = e {
                    StorageError::ChunkNotFound(hash)
                } else {
                    e
                }
            })?;
            bytes.append(&mut chunk);
        }
        Ok(bytes)
    }

    /// A snapshot is a new repo to which only `id` has been committed.
    pub fn export_snapshot(&self, id: &Hash, path: impl AsRef<Path>) -> Result<(), StorageError> {
        // Only export a snapshot to a new or empty directory
        let path = path.as_ref();
        if !path.exists() || path.read_dir()?.next().is_none() {
            std::fs::create_dir_all(path)?;
        } else {
            return Err(StorageError::InvalidRepo);
        };
        let bytes = self.backend.load(id)?;
        let commit: Vec<Hash> = deserialize(&bytes)?;
        for chunk in commit {
            self.backend.copy(&chunk, path)?;
        }
        self.backend.copy(id, path)?;
        Ok(())
    }
}
