// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2022-2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Contains an implementation of [SmartRollupCore] suitable for running the
//! kernel standalone for experiements and testing purposes. Used when
//! _not_ compiling to **wasm**.

use crate::state::{HostState, NextInput};
use crate::{DefaultSink, MockHost};
use core::{
    cell::RefCell,
    cmp::min,
    mem::size_of,
    ptr,
    slice::{from_raw_parts, from_raw_parts_mut},
};
use tezos_smart_rollup_core::smart_rollup_core::{ReadInputMessageInfo, SmartRollupCore};
use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_host::dal_parameters::DAL_PARAMETERS_SIZE;
use tezos_smart_rollup_host::metadata::METADATA_SIZE;
use tezos_smart_rollup_host::Error;

impl From<HostState> for MockHost {
    fn from(state: HostState) -> Self {
        Self {
            info: super::info_for_level(state.curr_level as i32),
            state: RefCell::new(state),
            debug_log: Box::new(RefCell::new(DefaultSink)),
            keep_going: true,
        }
    }
}

impl AsMut<HostState> for MockHost {
    fn as_mut(&mut self) -> &mut HostState {
        self.state.get_mut()
    }
}

unsafe fn reveal_dal_parameters(
    host: &MockHost,
    destination_addr: *mut u8,
    max_bytes: usize,
) -> i32 {
    let params: [u8; DAL_PARAMETERS_SIZE] =
        host.state.borrow().get_dal_parameters().into();
    let len = min(max_bytes, params.len());
    let slice = from_raw_parts_mut(destination_addr, len);
    slice.copy_from_slice(&params[..len]);
    len as i32
}

unsafe fn reveal_dal_page(
    host: &MockHost,
    published_level: i32,
    slot_index: u8,
    page_index: i16,
    destination_addr: *mut u8,
    max_bytes: usize,
) -> i32 {
    let state = host.state.borrow();
    let params = state.get_dal_parameters();
    // If the asked level is not old enough, the DAL slot
    // cannot yet be attested. We return 0 in this case to
    // mimic the host function.
    if published_level as u64 + params.attestation_lag > host.level() as u64 {
        return 0;
    }
    let page_size = params.page_size as usize;
    let len = min(max_bytes, page_size);
    let start = ((page_index as u64) * params.page_size) as usize;
    let Some(slot) = state.get_dal_slot(published_level, slot_index) else {
        return 0;
    };
    let slice = from_raw_parts_mut(destination_addr, len);
    slice.copy_from_slice(&slot[start..start + len]);
    len as i32
}

unsafe impl SmartRollupCore for MockHost {
    unsafe fn read_input(
        &self,
        message_info: *mut ReadInputMessageInfo,
        dst: *mut u8,
        max_bytes: usize,
    ) -> i32 {
        if let Some(NextInput { level, id, payload }) =
            self.state.borrow_mut().handle_read_input(max_bytes)
        {
            let input_message_info = ReadInputMessageInfo {
                level: level as i32,
                id: id as i32,
            };
            ptr::write(message_info, input_message_info);

            // safe as payload.len() <= max_bytes
            let slice = from_raw_parts_mut(dst, payload.len());
            slice.copy_from_slice(payload.as_slice());

            payload.len().try_into().unwrap()
        } else {
            0_i32
        }
    }

    unsafe fn write_debug(&self, src: *const u8, num_bytes: usize) {
        let debug_out = from_raw_parts(src, num_bytes);

        self.debug_log.borrow_mut().write_all(debug_out).unwrap();
    }

    unsafe fn write_output(&self, src: *const u8, num_bytes: usize) -> i32 {
        let output = from_raw_parts(src, num_bytes).to_vec();

        self.state
            .borrow_mut()
            .handle_write_output(output)
            .map(|_| 0)
            .unwrap_or_else(Error::code)
    }

    unsafe fn store_has(&self, path: *const u8, len: usize) -> i32 {
        self.state.borrow().store.store_has(path, len)
    }

    unsafe fn store_read(
        &self,
        path: *const u8,
        len: usize,
        offset: usize,
        dst: *mut u8,
        max_bytes: usize,
    ) -> i32 {
        self.state
            .borrow()
            .store
            .store_read(path, len, offset, dst, max_bytes)
    }

    unsafe fn store_write(
        &self,
        path: *const u8,
        len: usize,
        offset: usize,
        src: *const u8,
        num_bytes: usize,
    ) -> i32 {
        self.state
            .borrow_mut()
            .store
            .store_write(path, len, offset, src, num_bytes)
    }

    unsafe fn store_delete(&self, path: *const u8, len: usize) -> i32 {
        self.state.borrow_mut().store.store_delete(path, len)
    }

    unsafe fn store_delete_value(&self, path: *const u8, len: usize) -> i32 {
        self.state.borrow_mut().store.store_delete_value(path, len)
    }

    unsafe fn store_list_size(&self, path: *const u8, len: usize) -> i64 {
        self.state.borrow().store.store_list_size(path, len)
    }

    unsafe fn store_move(
        &self,
        from_path: *const u8,
        from_path_len: usize,
        to_path: *const u8,
        to_path_len: usize,
    ) -> i32 {
        self.state.borrow_mut().store.store_move(
            from_path,
            from_path_len,
            to_path,
            to_path_len,
        )
    }

    unsafe fn store_copy(
        &self,
        from_path: *const u8,
        from_path_len: usize,
        to_path: *const u8,
        to_path_len: usize,
    ) -> i32 {
        self.state.borrow_mut().store.store_copy(
            from_path,
            from_path_len,
            to_path,
            to_path_len,
        )
    }

    unsafe fn reveal_preimage(
        &self,
        hash_addr: *const u8,
        hash_len: usize,
        destination_addr: *mut u8,
        max_bytes: usize,
    ) -> i32 {
        // only `Reveal_hash` supported for now.
        let hash = from_raw_parts(hash_addr, hash_len)
            .try_into()
            .unwrap_or_else(|_| panic!("Hash is not {PREIMAGE_HASH_SIZE} bytes"));

        let bytes = self
            .state
            .borrow()
            .handle_reveal_preimage(&hash, max_bytes)
            .to_vec();

        assert!(bytes.len() <= max_bytes);

        let slice = from_raw_parts_mut(destination_addr, bytes.len());
        slice.copy_from_slice(bytes.as_slice());

        bytes.len().try_into().unwrap()
    }

    unsafe fn store_value_size(&self, path: *const u8, path_len: usize) -> i32 {
        self.state.borrow().store.store_value_size(path, path_len)
    }

    unsafe fn reveal_metadata(&self, destination_addr: *mut u8, max_bytes: usize) -> i32 {
        let metadata: [u8; METADATA_SIZE] =
            self.state.borrow().get_metadata().clone().into();
        let len = min(max_bytes, metadata.len());
        let slice = from_raw_parts_mut(destination_addr, len);
        slice.copy_from_slice(&metadata[..len]);
        len as i32
    }

    unsafe fn reveal(
        &self,
        payload_addr: *const u8,
        payload_len: usize,
        destination_addr: *mut u8,
        max_bytes: usize,
    ) -> i32 {
        let payload: &[u8] = from_raw_parts(payload_addr, payload_len);
        if payload.is_empty() {
            return 0;
        }
        let (tag, payload) = payload.split_at(size_of::<u8>());
        match tag[0] {
            0 => {
                // Reveal_raw_data
                self.reveal_preimage(
                    payload_addr.add(1),
                    payload_len.saturating_sub(1),
                    destination_addr,
                    max_bytes,
                )
            }
            1 => {
                // Reveal_metadata
                self.reveal_metadata(destination_addr, max_bytes)
            }
            2 => {
                // Reveal_dal_page

                const PAYLOAD_SIZE: usize =
                    size_of::<i32>() + size_of::<u8>() + size_of::<i16>();
                if payload.len() < PAYLOAD_SIZE {
                    return 0;
                }

                let (published_level, remaining) = payload.split_at(size_of::<i32>());
                let (slot_index, remaining) = remaining.split_at(size_of::<u8>());
                let (page_index, _) = remaining.split_at(size_of::<i16>());

                let published_level =
                    i32::from_be_bytes(published_level.try_into().unwrap());
                let slot_index = slot_index[0];
                let page_index = i16::from_be_bytes(page_index.try_into().unwrap());

                reveal_dal_page(
                    self,
                    published_level,
                    slot_index,
                    page_index,
                    destination_addr,
                    max_bytes,
                )
            }
            3 => {
                // Reveal_dal_parameters
                reveal_dal_parameters(self, destination_addr, max_bytes)
            }
            tag => unimplemented!(
                "The `reveal` host function is not yet mocked for tag {}.",
                tag
            ),
        }
    }
}

#[cfg(test)]
mod tests {

    use std::iter::repeat_with;

    use super::MockHost;

    use crate::state::HostState;
    use tezos_smart_rollup_core::{MAX_FILE_CHUNK_SIZE, MAX_INPUT_MESSAGE_SIZE};
    use tezos_smart_rollup_host::input::Message;
    use tezos_smart_rollup_host::{
        metadata::RollupMetadata, path::RefPath, reveal::HostReveal,
        runtime::RuntimeError, storage::StorageV1, wasm::WasmHost,
    };

    #[test]
    fn test_read_input_message() {
        // Arrange
        let mut mock_host = MockHost::default();

        mock_host
            .as_mut()
            .add_input(vec![5; MAX_INPUT_MESSAGE_SIZE / 2]);

        // Act
        let _ = mock_host.read_input(); // sol
        let _ = mock_host.read_input(); // info per level
        let result = mock_host.read_input();

        // Assert
        let expected = Ok(Some(Message::new(
            mock_host.level(),
            2,
            vec![5; MAX_INPUT_MESSAGE_SIZE / 2],
        )));

        assert_eq!(expected, result);
    }

    #[test]
    fn test_reveal_preimage() {
        // Arrange
        let mut state = HostState::default();

        let data = vec![b'a'; 3 * 1024];

        let hash = state.set_preimage(data);

        let mock_host = MockHost::from(state);

        let mut buffer = [0; 300];
        // Act
        let _result = mock_host.reveal_preimage(&hash, &mut buffer);

        // Assert

        assert_eq!(buffer, [b'a'; 300]);
    }

    #[test]
    fn test_reveal_metadata() {
        // Arrange

        let metadata_bytes = [
            // sr1 as 20 bytes
            b'M', 165, 28, b']', 231, 161, 205, 212, 148, 193, b'[', b'S', 129, b'^', 31,
            170, b'L', 26, 150, 202, // origination level as 4 bytes
            0, 0, 0, 42,
        ];

        let expected_metadata = RollupMetadata::from(metadata_bytes);
        let state = HostState::default_with_metadata(expected_metadata.clone());
        let mock_host = MockHost::from(state); // Act

        // Act
        let result = mock_host.reveal_metadata();

        // Assert
        assert_eq!(expected_metadata, result);
    }

    #[test]
    fn read_value_slice_not_found() {
        let mock = MockHost::default();
        const PATH: RefPath<'static> = RefPath::assert_from(b"/some/path");
        let mut buffer = [0_u8; 16];

        assert_eq!(
            mock.store_read_slice(&PATH, 0, &mut buffer),
            Err(RuntimeError::HostErr(
                tezos_smart_rollup_host::Error::StoreNotAValue
            ))
        );
    }

    #[test]
    fn read_value_slice_partial_buffer_fill() {
        let mut mock = MockHost::default();
        const PATH: RefPath<'static> = RefPath::assert_from(b"/some/path");
        let value = [1_u8; 8];
        let mut buffer = [0_u8; 16];

        mock.store_write(&PATH, &value, 0)
            .expect("Could not write value to store");

        assert_eq!(mock.store_read_slice(&PATH, 0, &mut buffer), Ok(8_usize));

        assert_eq!(buffer, [1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0]);
    }

    #[test]
    fn read_value_slice_complete_buffer_fill() {
        let mut mock = MockHost::default();
        const PATH: RefPath<'static> = RefPath::assert_from(b"/some/path");
        let value = [1_u8; 16];
        let mut buffer = [0_u8; 16];

        mock.store_write(&PATH, &value, 0)
            .expect("Could not write value to store");

        assert_eq!(mock.store_read_slice(&PATH, 0, &mut buffer), Ok(16_usize));

        assert_eq!(buffer, [1_u8; 16]);
    }

    #[test]
    fn test_store_write() {
        let mut state = HostState::default();
        let size = 256_i32;
        let data = vec![b'a'; size as usize];
        let path = b"/a/b";

        state.store.handle_store_write(path, 0, &data).unwrap();
        let value_size = state.store.handle_store_value_size(path).unwrap();

        assert_eq!(size, value_size)
    }

    #[test]
    fn store_read_and_write_all() {
        let mut mock = MockHost::default();
        const PATH: RefPath = RefPath::assert_from(b"/path/value");

        let value: Vec<u8> = (0..MAX_FILE_CHUNK_SIZE * 2 + 100)
            .map(|v| (v % 100).try_into().unwrap())
            .collect();

        StorageV1::store_write_all(&mut mock, &PATH, &value)
            .expect("Could not write value to store");

        let value_in_durable = StorageV1::store_read_all(&mock, &PATH)
            .expect("Could not read the value from the store");

        let size = mock.store_value_size(&PATH);

        assert_eq!(Ok(value.len()), size);
        assert_eq!(value_in_durable, value);
    }

    #[test]
    fn store_write_all_delete_previous_value() {
        let mut mock = MockHost::default();
        const PATH: RefPath = RefPath::assert_from(b"/path/value");

        // First write a value of size ~4.2KB
        let initial_value: Vec<u8> = (0..MAX_FILE_CHUNK_SIZE * 2 + 100)
            .map(|v| (v % 100).try_into().unwrap())
            .collect();

        StorageV1::store_write_all(&mut mock, &PATH, &initial_value)
            .expect("Could not write value to store");
        let initial_size = mock.store_value_size(&PATH).expect("Could not read size");
        let initial_value_in_store = StorageV1::store_read_all(&mock, &PATH)
            .expect("Could not read the value from the store");

        // Then write a new value of size ~2.1 KB
        let smaller_value: Vec<u8> = (0..MAX_FILE_CHUNK_SIZE + 100)
            .map(|v| (v % 50).try_into().unwrap())
            .collect();

        StorageV1::store_write_all(&mut mock, &PATH, &smaller_value)
            .expect("Could not write value to store");
        let new_size = mock.store_value_size(&PATH).expect("Could not read size");
        let new_value_in_store = StorageV1::store_read_all(&mock, &PATH)
            .expect("Could not read the value from the store");

        // The size of the value in the storage should have been shrinked, and
        // the new value read from the storage shouldn't be equal to the initial
        // one.
        assert!(new_size < initial_size);
        assert_ne!(new_value_in_store, initial_value_in_store);
        assert_eq!(new_value_in_store, smaller_value);
    }

    #[test]
    fn dal_slot_are_padded_with_zeroes() {
        let mut mock = MockHost::default();
        let data = vec![b'v'; 100];
        let published_level = (mock.level()) as i32;
        let slot_index = 4;
        mock.set_dal_slot(published_level, slot_index, &data);

        let dal_parameters = mock.reveal_dal_parameters();

        // First bump the level by attestation_lag so that the slot is old enough to be attested
        let attestation_lag = dal_parameters.attestation_lag as usize;
        let () = repeat_with(|| mock.bump_level())
            .take(attestation_lag + 1)
            .collect();

        // Read the slot completely
        let slot_size = dal_parameters.slot_size as usize;
        let page_size = dal_parameters.page_size as usize;
        let mut slot = vec![0; slot_size];
        let number_of_pages = (slot_size / page_size) as i16;
        let mut offset = 0;
        for page_index in 0..number_of_pages {
            let page_len = mock
                .reveal_dal_page(
                    published_level,
                    slot_index,
                    page_index,
                    &mut slot[offset..(offset + page_size)],
                )
                .expect("Page is expected to be readable");
            offset += page_len;
            assert_eq!(page_len, page_size)
        }

        let data_in_slot = &slot[..100];
        let padding = &slot[100..];

        assert_eq!(slot.len(), slot_size);
        assert_eq!(&data, data_in_slot);
        assert!(padding.iter().all(|b| *b == 0));
    }

    fn publish_and_fetch_slot(
        mock: &mut MockHost,
        published_level: i32,
        page_buffer: &mut [u8],
    ) -> usize {
        let data = vec![b'v'; 100];
        let slot_index = 4;
        assert!(
            published_level > 0,
            "Cannot publish a slot at a negative level"
        );
        mock.set_dal_slot(published_level, slot_index, &data);

        // The slot is in an attestable state, so we can read its content
        mock.reveal_dal_page(published_level, slot_index, 0, page_buffer)
            .expect("Reveal of attested slot shouldn't fail")
    }

    fn assert_unattestable_dal_slot(mock: &mut MockHost, published_level: i32) {
        let dal_parameters = mock.reveal_dal_parameters();
        let page_size = dal_parameters.page_size as usize;
        let mut page_buffer = vec![0; page_size];
        let page_len = publish_and_fetch_slot(mock, published_level, &mut page_buffer);

        assert_eq!(page_len, 0);
        assert!(page_buffer.iter().all(|b| *b == 0));
    }

    fn assert_attestable_dal_slot(mock: &mut MockHost, published_level: i32) {
        let dal_parameters = mock.reveal_dal_parameters();
        let page_size = dal_parameters.page_size as usize;
        let mut page_buffer = vec![0; page_size];
        let page_len = publish_and_fetch_slot(mock, published_level, &mut page_buffer);

        assert_eq!(page_len, page_size);
        assert!(page_buffer[..100].iter().all(|b| *b == b'v'));
    }

    fn bump_levels(mock: &mut MockHost, levels: u32) {
        repeat_with(|| mock.bump_level())
            .take(levels as usize)
            .collect()
    }

    #[test]
    fn unattestable_dal_slots_are_empty() {
        let mut mock = MockHost::default();
        // We'll use the same slot index and data all along the test, as they
        // are not relevant..

        let dal_parameters = mock.reveal_dal_parameters();
        let attestation_lag = dal_parameters.attestation_lag as u32;

        // Let's first test with a DAL slot published before the current level
        // with the attestation lag exceeded.
        let published_level = (mock.level() - attestation_lag) as i32;
        assert_attestable_dal_slot(&mut mock, published_level);

        // Then let's test with a DAL slot where the attestation lag hasn't been
        // exceeded yet.
        let published_level = (mock.level() - (attestation_lag - 1)) as i32;
        assert_unattestable_dal_slot(&mut mock, published_level);

        // Now, let's move forward to the level where it should be attestable
        bump_levels(&mut mock, attestation_lag - 1);
        assert_attestable_dal_slot(&mut mock, published_level);

        // The DAL slot will now be published in the future
        let published_level = (mock.level() + 1) as i32;
        assert_unattestable_dal_slot(&mut mock, published_level);

        // Now, let's move at the same level, it shouldn't be attestable
        bump_levels(&mut mock, 1);
        assert_unattestable_dal_slot(&mut mock, published_level);

        // Finally, let's move forward of `attestation_lag` levels, the DAL slot
        // should now be available
        bump_levels(&mut mock, attestation_lag);
        assert_attestable_dal_slot(&mut mock, published_level);
    }
}
