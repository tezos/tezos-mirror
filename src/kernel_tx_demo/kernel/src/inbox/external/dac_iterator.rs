// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Iterator-like structure for iterating through the contents of a dac message.

use tezos_data_encoding::{
    enc::{BinError, BinWriter},
    nom::NomReader,
};
use tezos_smart_rollup_core::MAX_FILE_CHUNK_SIZE;
use tezos_smart_rollup_host::path::OwnedPath;
use tezos_smart_rollup_host::path::PathError;
use tezos_smart_rollup_host::runtime::RuntimeError;
use tezos_smart_rollup_host::storage::StorageV1;

use super::v1::verifiable::VerifiableOperation;

/// Errors that can be returned while traversing the list of [VerifiableOperation].
#[derive(Debug)]
pub enum IteratorStateError {
    /// Reading the dac payload from the underlying host fails
    HostStoreReadDacPayloadError(RuntimeError),
    /// Reading the iterator state from the underlying host store fails
    HostStoreReadIteratorStateError(RuntimeError),
    /// Writing the iterator state to the underlying host store fails,
    HostStoreWriteIteratorStateError(RuntimeError),
    /// Parsing an element of type [VerifiableOperation] failed.
    DecodeError,
    /// Encoding the iterator data fails
    EncodeIteratorStateError(BinError),
    /// Error when constructing the Dac payload path
    DacPayloadPathError(PathError),
    /// Error when constructing the Dac iterator path
    IteratorStatePathError(PathError),
}

/// Resume-able iterator over the contents of a DAC message.
///
/// `IteratorState` can be used to iterate over elements of type [VerifiableOperation].
/// persisted in the store. The data structure itself can be persisted to and loaded from the store,
/// which makes it possible to traverse a list of `VerifiableOperation` across multiple
/// instances of `kernel_run`.
///
/// To traverse the list of [VerifiableOperation], we use a technique inspired from double buffering
/// used in compilers. Roughly speaking, we maintain two contiguous buffers of the same size in memory,
/// and a pointer to where to start parsing the next [VerifiableOperation] from in the in-memory buffer.
/// Once the first half of the buffer has been consumed, the second half is copied onto the first half,
/// and the next portion of dac message to read, of size at most half the in-memory buffer size,
/// is loaded onto the second half of the buffer.
#[derive(Debug, NomReader, BinWriter)]
pub struct IteratorState {
    /// A in-memory buffer containing the portion of the dac payload from which new
    /// VerifiableOperation are parsed.
    #[encoding(dynamic, list)]
    buffer: Vec<u8>,
    /// The offset in the buffer where to start parsing the next [VerifiableOperation].
    /// It is stored as a 32 bits unsigned integer.
    buffer_offset: u32,
    /// The offset in the durable store from where to start loading the next portion of DAC payload.
    next_offset: Option<u32>,
    /// The total size of the Dac message payload
    total_length: u32,
    /// The message index that the DAC iterator state/message payload are stored
    idx: u32,
}

impl IteratorState {
    /// Initializes the iterator. [VerifiableOperation]s are retrieved from the
    /// input data, which is persisted on the host's store.
    pub fn new<Host: StorageV1>(
        host: &mut Host,
        idx: u32,
    ) -> Result<IteratorState, IteratorStateError> {
        let half_buffer_size = MAX_FILE_CHUNK_SIZE;
        let dac_payload_path = Self::dac_payload_path(idx)?;
        let total_length = host
            .store_value_size(&dac_payload_path)
            .map_err(IteratorStateError::HostStoreReadDacPayloadError)?;
        let buffer_offset = 4; //skip prefix field containing list length

        let bytes_to_read_first_half = std::cmp::min(half_buffer_size, total_length);
        let mut buffer = vec![0; 2 * half_buffer_size];

        // Read up to MAX_FILE_CHUNK_SIZE bytes from the DAC payload.
        // If the DAC payload consists of less than MAX_FILE_CHUNK_SIZE bytes, read the whole
        // DAC payload. The result is stored in the first half of the iterator buffer.
        let read_bytes_left_half = host
            .store_read_slice(
                &dac_payload_path,
                0,
                &mut buffer[0..bytes_to_read_first_half],
            )
            .map_err(IteratorStateError::HostStoreReadDacPayloadError)?;

        // If the DAC payload consists of strictly more than MAX_CHUNK_SIZE bytes,
        // read the next up to MAX_FILE_CHUNK_SIZE bytes from it, and store the content in the
        // right hand side of the iterator buffer.
        let should_read_second_half = total_length > half_buffer_size;

        let read_bytes_right_half = if should_read_second_half {
            host.store_read_slice(
                &dac_payload_path,
                half_buffer_size,
                &mut buffer[half_buffer_size..2 * half_buffer_size],
            )
            .map_err(IteratorStateError::HostStoreReadDacPayloadError)?
        } else {
            0
        };

        // To avoid keeping trailing zeroes in the iterator buffer, truncate its length to
        // the amount of bytes read.
        let read_bytes = read_bytes_left_half + read_bytes_right_half;
        buffer.truncate(read_bytes);

        // Determine the index from where to start reading the next chunk of DAC payload, if any.
        let next_offset = if read_bytes < total_length {
            Some(read_bytes as u32)
        } else {
            None
        };

        Ok(IteratorState {
            buffer,
            buffer_offset,
            next_offset,
            total_length: total_length as u32,
            idx,
        })
    }

    /// Removes the state of the iterator from the host store.
    pub fn clear<Host: StorageV1>(host: &mut Host, idx: u32) -> Result<(), IteratorStateError> {
        let iterator_state_path = Self::dac_iterator_state_path(idx)?;
        host.store_delete_value(&iterator_state_path)
            .map_err(IteratorStateError::HostStoreWriteIteratorStateError)
    }

    fn update_buffer<Host: StorageV1>(
        &mut self,
        host: &mut Host,
    ) -> Result<(), IteratorStateError> {
        // This function requires the following preconditions:
        // 1. self.buffer_offset >= MAX_FILE_CHUNK_SIZE
        let half_buffer_size = MAX_FILE_CHUNK_SIZE;
        let buffer_offset = self.buffer_offset as usize;
        let total_length = self.total_length as usize;
        assert!(buffer_offset >= half_buffer_size);
        // next_offset is set to None only in the case that the whole dac payload
        // has been read.
        let next_offset = self
            .next_offset
            .expect("update buffer must not be called when no futher data is available")
            as usize;
        // Because we are assuming that self.next_offset is defined,
        // then there is a portion of dac_payload left to parse.
        // This means that all the reads of a dac_payload slice
        // managed to read the whole self.half_buffer_size bytes from the dac payload.
        // As a consequence, the following condition is met:
        // self.buffer.len() = 2 * self.buffer_half_size
        assert_eq!(self.buffer.len(), 2 * half_buffer_size);
        //copy the right half size of the buffer into the left hand size.
        // this call is safe because of the guaranteed condition
        // self.buffer.len() = 2 * self.buffer_half_size
        self.buffer
            .copy_within(half_buffer_size..2 * half_buffer_size, 0);
        // by precondition 1, self.buffer_offset >= half_buffer_size,
        // hence after this line is executed we are guaranteed that self.buffer_offset >= 0
        self.buffer_offset = (buffer_offset - half_buffer_size) as u32;

        let rhs = &mut self.buffer[half_buffer_size..2 * half_buffer_size];

        let dac_payload_path = Self::dac_payload_path(self.idx)?;
        let read_bytes = host
            .store_read_slice(&dac_payload_path, next_offset, rhs)
            .map_err(IteratorStateError::HostStoreReadDacPayloadError)?;

        // Update next offset and set it to none if the whole dac payload has been read
        self.next_offset = Some(next_offset + read_bytes)
            .filter(|offset| offset < &total_length)
            .map(|offset| offset as u32);

        if read_bytes < half_buffer_size {
            // this call is safe because exactly read_bytes bytes have
            // been written into the second half  of self.buffer.
            self.buffer.truncate(half_buffer_size + read_bytes);
        }
        Ok(())
    }

    fn has_finished_reading(&self) -> bool {
        // the whole buffer has been consumed, and there is no data to be read from
        // the dac payload stored on disk
        self.buffer_offset == self.buffer.len() as u32 && self.next_offset.is_none()
    }

    fn buffer_pointer_is_in_right_hand_side(&self) -> bool {
        self.buffer_offset >= MAX_FILE_CHUNK_SIZE as u32
    }

    fn can_keep_reading_from_store(&self) -> bool {
        self.next_offset.is_some()
    }

    /// Parse and return the next [VerifiableOperation] from the iterator.
    /// Unlike standard iterators, this function can return a [Result]
    /// where an Error of type [IteratorStateError] is returned
    /// when reading the next element fails.
    pub fn next<Host: StorageV1>(
        &mut self,
        host: &mut Host,
    ) -> Result<Option<VerifiableOperation<'_>>, IteratorStateError> {
        while self.buffer_pointer_is_in_right_hand_side() && self.can_keep_reading_from_store() {
            // because of the condition in the while outer loop, we know that the
            // buffer_offset points to the right hand side of the buffer,
            // which is required as a precondition of self.update_buffer
            self.update_buffer(host)?;
        }
        if self.has_finished_reading() {
            // Remove the iterator state from the host store.
            IteratorState::clear(host, self.idx)?;
            return Ok(None);
        }
        let buffer_offset = self.buffer_offset as usize;
        let next_element_buf = &self.buffer[buffer_offset..self.buffer.len()];
        let (remaining, verifiable_operation) = VerifiableOperation::parse(next_element_buf)
            .map_err(|_| IteratorStateError::DecodeError)?;
        // Bump buffer_offset to match remaining portion of buffer.
        self.buffer_offset = (self.buffer.len() - remaining.len()) as u32;
        Ok(Some(verifiable_operation))
    }

    /// Stores the iterator data to disk.
    ///
    /// Note that only iterating through a single DAC payload is supported at one time.
    /// Attempting to iterate over multiple DAC payloads simultaneously will likely cause issues.
    pub fn persist<Host: StorageV1>(self, host: &mut Host) -> Result<(), IteratorStateError> {
        let mut iterator_state_buf = Vec::with_capacity(self.buffer.len() + 100);
        self.bin_write(&mut iterator_state_buf)
            .map_err(IteratorStateError::EncodeIteratorStateError)?;

        let state_path = Self::dac_iterator_state_path(self.idx)?;
        host.store_write_all(&state_path, &iterator_state_buf)
            .map_err(IteratorStateError::HostStoreWriteIteratorStateError)
    }

    fn dac_iterator_state_path(idx: u32) -> Result<OwnedPath, IteratorStateError> {
        crate::transactions::store::dac_iterator_state_path(idx)
            .map_err(IteratorStateError::IteratorStatePathError)
    }

    fn dac_payload_path(idx: u32) -> Result<OwnedPath, IteratorStateError> {
        crate::transactions::store::dac_payload_path(idx)
            .map_err(IteratorStateError::DacPayloadPathError)
    }

    /// Loads the state of the iterator from disk. If no iterator state is present in the host store,
    /// a new one is created that can be used to traverse the whole DAC payload
    pub fn load<Host: StorageV1>(host: &mut Host, idx: u32) -> Result<Self, IteratorStateError> {
        let dac_iterator_state_path = Self::dac_iterator_state_path(idx)?;
        if let Ok(None) = host.store_has(&dac_iterator_state_path) {
            IteratorState::new(host, idx)
        } else {
            let serialized_iterator_state = host
                .store_read_all(&dac_iterator_state_path)
                .map_err(IteratorStateError::HostStoreReadIteratorStateError)?;
            let (_, iterator_state) = NomReader::nom_read(&serialized_iterator_state)
                .map_err(|_| IteratorStateError::DecodeError)?;
            Ok(iterator_state)
        }
    }
}

#[cfg(test)]
mod tests {

    use proptest::collection;

    use proptest::prelude::*;
    use tezos_data_encoding::enc::BinWriter;
    use tezos_data_encoding::nom::NomReader;
    use tezos_smart_rollup_host::storage::StorageV1;
    use tezos_smart_rollup_mock::MockHost;

    use crate::inbox::dac_iterator::IteratorState;
    use crate::inbox::v1::sendable::Batch;
    use crate::inbox::v1::Operation;
    use crate::transactions::store::dac_payload_path;

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(1))]

        /// This test checks the case where the DAC payload is less than half the buffer size (2 KB)
        /// Assuming an upper bound of 150 bytes per transactions, at most 10 transactions can fit
        /// a half-buffer.
        #[test]
        fn dac_iterator_traverse_small(
            operations_with_signatures in collection::vec(Operation::arb_with_signer(), 0..10),
        ) {
            let mut host = MockHost::default();

            let original_operations: Vec<Operation> = operations_with_signatures.iter().map(|(operation, _ )| {

                let mut buf = Vec::new();
                operation.bin_write(&mut buf).unwrap();
                let (_, op) = Operation::nom_read(&buf).unwrap();
                op
            }).collect();

            let batch = Batch::new(operations_with_signatures);
            let mut dac_preimage = Vec::new();
            batch.bin_write(&mut dac_preimage).unwrap();

            let dac_payload_path = dac_payload_path(0).unwrap();
            host.store_write_all(&dac_payload_path, &dac_preimage).unwrap();

            let mut iterator = IteratorState::new(&mut host, 0).unwrap();
            let mut original_operations_iterator = original_operations.into_iter();
            while let Some(next_verifiable_operation) = iterator.next(&mut host).unwrap() {
                let original_operation = original_operations_iterator.next().unwrap();
                assert_eq!(original_operation, next_verifiable_operation.operation)
            };
            assert_eq!(original_operations_iterator.next(), None);
        }

        #[test]
        /// This test checks that the Dac iterator behaves as expected, when the iterator buffer
        /// needs to be updated continuously. Assuming a lower bound of 90 bytes per operation,
        /// and a half-buffer of 2KB, we need at least 45 transactions to ensure that the half
        /// buffer is updated at least once. Therefore, this test uses a batch of 50 to 200
        /// transactions.
        fn dac_iterator_traverse_large(
            operations_with_signatures in collection::vec(Operation::arb_with_signer(), 50..200),
        ) {
            let mut host = MockHost::default();

            let original_operations: Vec<Operation> = operations_with_signatures.iter().map(|(operation, _ )| {

                let mut buf = Vec::new();
                operation.bin_write(&mut buf).unwrap();
                let (_, op) = Operation::nom_read(&buf).unwrap();
                op
            }).collect();

            let batch = Batch::new(operations_with_signatures);
            let mut dac_preimage = Vec::new();
            batch.bin_write(&mut dac_preimage).unwrap();

            let dac_payload_path = dac_payload_path(1).unwrap();
            host.store_write_all(&dac_payload_path, &dac_preimage).unwrap();

            let mut iterator = IteratorState::new(&mut host, 1).unwrap();
            let mut original_operations_iterator = original_operations.into_iter();
            while let Some(next_verifiable_operation) = iterator.next(&mut host).unwrap() {
                let original_operation = original_operations_iterator.next().unwrap();
                assert_eq!(original_operation, next_verifiable_operation.operation)
            };
            assert_eq!(original_operations_iterator.next(), None);
        }

        #[test]
        fn dac_iterator_persist(
            operations_with_signatures in collection::vec(Operation::arb_with_signer(), 50..200),
            stop_at_operation in (0..200usize),
        ) {
            let mut host = MockHost::default();

             let original_operations: Vec<Operation> = operations_with_signatures.iter().map(|(operation, _ )| {
                let mut buf = Vec::new();
                operation.bin_write(&mut buf).unwrap();
                let (_, op) = Operation::nom_read(&buf).unwrap();
                op
            }).collect();

            let batch = Batch::new(operations_with_signatures);
            let mut dac_preimage = Vec::new();
            batch.bin_write(&mut dac_preimage).unwrap();

            let dac_payload_path = dac_payload_path(2).unwrap();
            host.store_write_all(&dac_payload_path, &dac_preimage).unwrap();

            let mut iterator = IteratorState::load(&mut host, 2).unwrap();
            let mut iterator_operations = 0;

            let mut original_operations_iterator = original_operations.into_iter();

             while let Some(next_verifiable_operation)  = iterator.next(&mut host).unwrap() {
                let original_operation = original_operations_iterator.next().unwrap();
                assert_eq!(original_operation, next_verifiable_operation.operation);

                iterator_operations += 1;
                if iterator_operations >= stop_at_operation { break };
            };

            iterator.persist(&mut host).unwrap();
            let mut rest_iterator = IteratorState::load(&mut host, 2).unwrap();
            while let Some(next_verifiable_operation)  = rest_iterator.next(&mut host).unwrap() {
                let original_operation = original_operations_iterator.next().unwrap();
                assert_eq!(original_operation, next_verifiable_operation.operation);
            };
            assert!(matches!(rest_iterator.next(&mut host), Ok(None)));
            assert_eq!(original_operations_iterator.next(), None);
        }

        #[test]
        fn dac_iterator_traverse_twice(
            operations in collection::vec(Operation::arb_with_signer(), 50..200),
        ) {
            let mut host = MockHost::default();

            let batch = Batch::new(operations);
            let mut dac_preimage = Vec::new();
            batch.bin_write(&mut dac_preimage).unwrap();

            let dac_payload_path = dac_payload_path(3).unwrap();
            host.store_write_all(&dac_payload_path, &dac_preimage).unwrap();

            let mut verifiable_operations = Vec::new();

            let mut iterator = IteratorState::load(&mut host, 3).unwrap();

             while let Some(next_operation)  = iterator.next(&mut host).unwrap() {
                // we cannot push the next operation to verifiable_operations directly,
                // as this contains the reference to the slice of parsed content in the
                // iterator buffer. Instead, we push a string containing a representation
                // of the verifiable operation that has been parsed. This is quite hacky
                // but it works for the purposes of testing.
                let as_string = format!("{next_operation:?}");
                verifiable_operations.push(as_string);
            };

            // We have traversed the whole DAC payload, if we reload the iterator
            // it will be re-created from scratch.

            let mut rest_iterator =  IteratorState::load(&mut host, 3).unwrap();
            let mut verifiable_operations_iter = verifiable_operations.into_iter();
            while let Some(next_operation)  = rest_iterator.next(&mut host).unwrap() {
                let as_string = format!("{next_operation:?}");
                if let Some(verifiable_operation) = verifiable_operations_iter.next() {
                assert_eq!(verifiable_operation, as_string);
                } else {
                    panic!("The second iteration of the dac iterator has returned more operations than expected");
                }
            };

            assert_eq!(verifiable_operations_iter.next(), None);
        }
    }

    /// This test checks the case where the DAC payload is the empty list of transactions
    #[test]
    fn dac_iterator_traverse_zero() {
        let operations = Vec::new();
        let mut host = MockHost::default();

        let batch = Batch::new(operations);
        let mut dac_preimage = Vec::new();
        batch.bin_write(&mut dac_preimage).unwrap();

        let dac_payload_path = dac_payload_path(5).unwrap();
        host.store_write_all(&dac_payload_path, &dac_preimage)
            .unwrap();

        let mut iterator = IteratorState::new(&mut host, 5).unwrap();
        let next_operation = iterator.next(&mut host).unwrap();
        assert_eq!(next_operation, None);
    }
}
