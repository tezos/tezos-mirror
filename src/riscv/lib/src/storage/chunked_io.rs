// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::cmp;
use std::collections::VecDeque;
use std::io;
use std::io::Cursor;

use super::CHUNK_SIZE;
use super::Hash;
use super::StorageError;
use super::Store;
use super::binary;

/// Simple writer that stores data in chunks of size [`CHUNK_SIZE`]
pub struct ChunkWriter<'a> {
    store: &'a mut Store,
    hashes: Vec<Hash>,
    buffer: Vec<u8>,
}

impl<'a> ChunkWriter<'a> {
    /// Create a new writer that writes the chunks to the given [`Store`].
    pub fn new(store: &'a mut Store) -> Self {
        Self {
            store,
            hashes: Vec::new(),
            buffer: Vec::with_capacity(CHUNK_SIZE),
        }
    }

    /// Finalise the writer by flushing any remaining chunks-in-progress to the store and returning
    /// the list of identifiers for each chunk that was written.
    pub fn finalise(mut self) -> Result<Vec<Hash>, StorageError> {
        if !self.buffer.is_empty() {
            self.flush_buffer()?;
        }

        Ok(self.hashes)
    }

    /// Write a chunk to the store.
    fn flush_buffer(&mut self) -> Result<(), StorageError> {
        let chunk_hash = self.store.store(self.buffer.as_slice())?;
        self.hashes.push(chunk_hash);
        self.buffer.clear();
        Ok(())
    }
}

impl io::Write for ChunkWriter<'_> {
    fn write(&mut self, mut data: &[u8]) -> io::Result<usize> {
        let ret = data.len();

        while !data.is_empty() {
            let rem_buffer_len = CHUNK_SIZE - self.buffer.len();
            let new_data_len = cmp::min(rem_buffer_len, data.len());

            // Take the data from the input.
            let new_data = &data[..new_data_len];
            data = &data[new_data_len..];
            self.buffer.extend_from_slice(new_data);

            // If the buffer has been completely filled, flush it.
            if rem_buffer_len == new_data_len {
                self.flush_buffer().map_err(io::Error::other)?;
            }
        }

        Ok(ret)
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

/// Just like [`ChunkWriter`], but for reading.
pub struct ChunkedReader<'a> {
    store: &'a Store,
    hashes: VecDeque<Hash>,
    buffer: Cursor<Vec<u8>>,
}

impl<'a> ChunkedReader<'a> {
    /// Create a new reader that pulls the chunks from the given [`Store`].
    pub fn new(store: &'a Store, hash: &Hash) -> Result<Self, StorageError> {
        let raw_hashes = store.load(hash)?;
        let hashes = binary::deserialise(raw_hashes.as_slice())?;
        Ok(Self {
            store,
            hashes,
            buffer: Cursor::new(Vec::with_capacity(CHUNK_SIZE)),
        })
    }

    /// Start reading the next chunk.
    fn next_chunk(&mut self) -> Result<(), StorageError> {
        let Some(hash) = self.hashes.pop_front() else {
            return Ok(());
        };

        self.buffer = Cursor::new(self.store.load(&hash)?);

        Ok(())
    }
}

impl io::Read for ChunkedReader<'_> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        if self.buffer.position() as usize >= self.buffer.get_ref().len() {
            self.next_chunk().map_err(io::Error::other)?;
        }

        self.buffer.read(buf)
    }
}
