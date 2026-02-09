// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2022-2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Mock runtime state & state transitions

use crypto::hash::SmartRollupHash;
use tezos_smart_rollup_core::{
    MAX_INPUT_MESSAGE_SIZE, MAX_OUTPUT_SIZE, PREIMAGE_HASH_SIZE,
};
use tezos_smart_rollup_host::{
    dal_parameters::RollupDalParameters, metadata::RollupMetadata, Error,
};

pub(crate) mod in_memory_store;
pub(crate) mod store;

pub use self::in_memory_store::InMemoryStore;

const MAX_OUTPUTS_PER_LEVEL: usize = 100;

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct NextInput {
    pub level: u32,
    pub id: u32,
    pub payload: Vec<u8>,
}

/// The mock `HostState` used by the *mock runtime*, contains the *store* and *debug_log*.
#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct HostState {
    /// Key-value store of runtime state.
    pub store: InMemoryStore,
    pub metadata: RollupMetadata,
    pub dal_parameters: RollupDalParameters,
    // Inbox metadata
    pub(crate) curr_level: u32,
    pub(crate) curr_input_id: usize,
    pub(crate) input: Vec<Vec<u8>>,
}

impl Default for HostState {
    fn default() -> Self {
        let store = InMemoryStore::default();
        let raw_rollup_address: [u8; 20] =
            SmartRollupHash::from_base58_check("sr1V6huFSUBUujzubUCg9nNXqpzfG9t4XD1h")
                .unwrap()
                .into();

        let metadata = RollupMetadata {
            raw_rollup_address,
            origination_level: crate::NAIROBI_ACTIVATION_LEVEL,
        };

        let dal_parameters = RollupDalParameters {
            number_of_slots: 16,
            attestation_lag: 8,
            slot_size: 126944,
            page_size: 3967,
        };
        Self {
            store,
            metadata,
            dal_parameters,
            curr_level: crate::NAIROBI_ACTIVATION_LEVEL,
            curr_input_id: 0,
            input: vec![],
        }
    }
}

impl HostState {
    /// Default with metadata set
    pub fn default_with_metadata(metadata: RollupMetadata) -> Self {
        Self {
            metadata,
            ..HostState::default()
        }
    }

    pub(crate) fn handle_write_output(&mut self, output: Vec<u8>) -> Result<(), Error> {
        if output.len() > MAX_OUTPUT_SIZE {
            return Err(Error::InputOutputTooLarge);
        }

        if self.store.0.outbox_at(self.curr_level).len() < MAX_OUTPUTS_PER_LEVEL {
            self.store.0.outbox_insert(self.curr_level, output);
            Ok(())
        } else {
            Err(Error::FullOutbox)
        }
    }

    pub(crate) fn add_input(&mut self, input: Vec<u8>) {
        if input.len() > MAX_INPUT_MESSAGE_SIZE {
            panic!(
                "input message too big -size:{} -max:{}",
                input.len(),
                MAX_INPUT_MESSAGE_SIZE
            );
        }

        self.input.push(input);
    }

    pub(crate) fn handle_read_input(&mut self, max_bytes: usize) -> Option<NextInput> {
        let next = self
            .input
            .get(self.curr_input_id)
            .map(|m| (m, usize::min(max_bytes, m.len())))
            .map(|(m, max)| m[0..max].to_vec())
            .map(|payload| NextInput {
                level: self.curr_level,
                id: self.curr_input_id as u32,
                payload,
            });

        self.curr_input_id += 1;

        next
    }

    pub(crate) fn handle_reveal_preimage(
        &self,
        hash: &[u8; PREIMAGE_HASH_SIZE],
        max_bytes: usize,
    ) -> &[u8] {
        let preimage = self.store.0.retrieve_preimage(hash);
        if preimage.len() < max_bytes {
            preimage
        } else {
            &preimage[0..max_bytes]
        }
    }

    pub(crate) fn set_preimage(&mut self, preimage: Vec<u8>) -> [u8; PREIMAGE_HASH_SIZE] {
        self.store.0.add_preimage(preimage)
    }

    pub(crate) fn get_metadata(&self) -> &RollupMetadata {
        &self.metadata
    }

    pub(crate) fn get_dal_parameters(&self) -> &RollupDalParameters {
        &self.dal_parameters
    }

    #[allow(dead_code)]
    pub(crate) fn set_dal_parameters(&mut self, params: RollupDalParameters) {
        self.dal_parameters = params
    }

    pub(crate) fn get_dal_slot(
        &self,
        published_level: i32,
        slot_index: u8,
    ) -> Option<&Vec<u8>> {
        self.store.0.retrieve_dal_slot(published_level, slot_index)
    }

    pub(crate) fn set_dal_slot(
        &mut self,
        published_level: i32,
        slot_index: u8,
        data: Vec<u8>,
    ) {
        self.store.0.set_dal_slot(published_level, slot_index, data)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use tezos_smart_rollup_core::{
        VALUE_TYPE_NONE, VALUE_TYPE_SUBTREE, VALUE_TYPE_VALUE,
    };

    #[test]
    fn read_next_inputs() {
        // Arrange
        let mut state = HostState::default();

        // Act
        state.add_input(vec![2; MAX_INPUT_MESSAGE_SIZE / 2]);
        state.add_input(vec![0; MAX_INPUT_MESSAGE_SIZE / 8]);

        // Assert

        assert_eq!(
            Some(NextInput {
                level: state.curr_level,
                id: 0,
                payload: vec![2; MAX_INPUT_MESSAGE_SIZE / 2]
            }),
            state.handle_read_input(MAX_INPUT_MESSAGE_SIZE)
        );

        assert_eq!(
            Some(NextInput {
                level: state.curr_level,
                id: 1,
                payload: vec![0; MAX_INPUT_MESSAGE_SIZE / 8]
            }),
            state.handle_read_input(MAX_INPUT_MESSAGE_SIZE)
        );

        // We've run out of input for this level
        assert_eq!(None, state.handle_read_input(MAX_INPUT_MESSAGE_SIZE));
    }

    #[test]
    fn store_write_new_path() {
        // Arrange
        let mut state = HostState::default();
        let path: &[u8] = b"/test/path";

        let written = vec![1, 2, 3, 4];

        // Act
        state.store.handle_store_write(path, 0, &written).unwrap();

        // Assert
        assert_eq!(
            Ok(VALUE_TYPE_VALUE),
            state.store.handle_store_has(path),
            "Path previously written to"
        );
        assert_eq!(Ok(written), state.store.handle_store_read(path, 0, 4096));
    }

    #[test]
    fn store_write_extend_path() {
        // Arrange
        let mut state = HostState::default();
        let path: &[u8] = b"/test/path";

        let written = vec![1, 2, 3, 4];
        state.store.handle_store_write(path, 0, &written).unwrap();

        // Act
        state.store.handle_store_write(path, 4, &written).unwrap();

        // Assert
        let expected = vec![1, 2, 3, 4, 1, 2, 3, 4];
        assert_eq!(
            Ok(VALUE_TYPE_VALUE),
            state.store.handle_store_has(path),
            "Path previously written to"
        );
        assert_eq!(Ok(expected), state.store.handle_store_read(path, 0, 4096));
    }

    #[test]
    fn store_write_extend_from_within() {
        // Arrange
        let mut state = HostState::default();
        let path: &[u8] = b"/test/path";

        let written = vec![1, 2, 3, 4];
        state.store.handle_store_write(path, 0, &written).unwrap();

        // Act
        state.store.handle_store_write(path, 2, &written).unwrap();

        // Assert
        let expected = vec![1, 2, 1, 2, 3, 4];
        assert_eq!(
            Ok(VALUE_TYPE_VALUE),
            state.store.handle_store_has(path),
            "Path previously written to"
        );
        assert_eq!(Ok(expected), state.store.handle_store_read(path, 0, 4096));
    }

    #[test]
    fn store_write_fully_within() {
        // Arrange
        let mut state = HostState::default();
        let path: &[u8] = b"/test/path";

        state
            .store
            .handle_store_write(path, 0, &[1, 2, 3, 4])
            .unwrap();

        // Act
        state.store.handle_store_write(path, 1, &[9, 10]).unwrap();

        // Assert
        let expected = vec![1, 9, 10, 4];
        assert_eq!(
            Ok(VALUE_TYPE_VALUE),
            state.store.handle_store_has(path),
            "Path previously written to"
        );
        assert_eq!(Ok(expected), state.store.handle_store_read(path, 0, 4096));
    }

    #[test]
    fn test_store_delete() {
        // Arrange
        let mut state = HostState::default();
        let prefix = "/a/long/prefix";

        state
            .store
            .handle_store_write(prefix.as_bytes(), 0, &[])
            .unwrap();

        for i in 0..10 {
            // subkey of prefix
            let subkey = format!("{prefix}/{i}");
            // not subkey as not a sub-path
            let almost_subkey = format!("{prefix}{i}");
            // completely different prefix
            let not_subkey = format!("/different/prefix/{i}");

            state
                .store
                .handle_store_write(subkey.as_bytes(), 0, &[])
                .unwrap();
            state
                .store
                .handle_store_write(almost_subkey.as_bytes(), 0, &[])
                .unwrap();
            state
                .store
                .handle_store_write(not_subkey.as_bytes(), 0, &[])
                .unwrap();
        }

        // Act
        state.store.handle_store_delete(prefix.as_bytes()).unwrap();

        // Assert
        assert_eq!(
            Ok(VALUE_TYPE_NONE),
            state.store.handle_store_has(prefix.as_bytes())
        );

        assert_eq!(
            Err(Error::StoreNotANode),
            state.store.handle_store_list_size(prefix.as_bytes())
        );
    }

    #[test]
    fn test_store_delete_value() {
        // Arrange
        let mut state = HostState::default();
        let prefix = "/a/long/prefix";

        state
            .store
            .handle_store_write(prefix.as_bytes(), 0, &[])
            .unwrap();

        for i in 0..10 {
            // subkey of prefix
            let subkey = format!("{prefix}/{i}");
            // not subkey as not a sub-path
            let almost_subkey = format!("{prefix}{i}");
            // completely different prefix
            let not_subkey = format!("/different/prefix/{i}");

            state
                .store
                .handle_store_write(subkey.as_bytes(), 0, &[])
                .unwrap();
            state
                .store
                .handle_store_write(almost_subkey.as_bytes(), 0, &[])
                .unwrap();
            state
                .store
                .handle_store_write(not_subkey.as_bytes(), 0, &[])
                .unwrap();
        }

        // Act
        state
            .store
            .handle_store_delete_value(prefix.as_bytes())
            .unwrap();

        // Assert
        assert_eq!(
            Ok(VALUE_TYPE_SUBTREE),
            state.store.handle_store_has(prefix.as_bytes())
        );

        assert_eq!(
            Ok(10),
            state.store.handle_store_list_size(prefix.as_bytes())
        );
    }

    #[test]
    fn store_list_size() {
        // Arrange
        let mut state = HostState::default();
        let prefix = "/a/long/prefix";

        for i in 0..10 {
            // subkey of prefix
            let subkey = format!("{prefix}/{i}");
            // not subkey as not a sub-path
            let almost_subkey = format!("{prefix}{i}");
            // completely different prefix
            let not_subkey = format!("/different/prefix/{i}");

            state
                .store
                .handle_store_write(subkey.as_bytes(), 0, &[])
                .unwrap();
            state
                .store
                .handle_store_write(almost_subkey.as_bytes(), 0, &[])
                .unwrap();
            state
                .store
                .handle_store_write(not_subkey.as_bytes(), 0, &[])
                .unwrap();
        }

        // Act
        let result = state
            .store
            .handle_store_list_size(prefix.as_bytes())
            .unwrap();

        // Assert
        assert_eq!(10, result, "Expected 10 subkeys of prefix");

        // Value is included in the count
        state
            .store
            .handle_store_write(prefix.as_bytes(), 0, &[1, 2, 3])
            .unwrap();
        let with_value = state
            .store
            .handle_store_list_size(prefix.as_bytes())
            .unwrap();
        assert_eq!(11, with_value, "Expected 10 subkeys of prefix, plus value");
    }

    #[test]
    fn store_move() {
        // Arrange
        let mut state = HostState::default();
        state.store.handle_store_write(b"/a/b", 0, b"ab").unwrap();
        state
            .store
            .handle_store_write(b"/a/b/c", 0, b"abc")
            .unwrap();
        state
            .store
            .handle_store_write(b"/a/b/c/z", 0, b"abcz")
            .unwrap();
        state
            .store
            .handle_store_write(b"/a/b/d", 0, b"abd")
            .unwrap();
        state.store.handle_store_write(b"/a/bc", 0, b"abc").unwrap();

        // Act
        state.store.handle_store_move(b"/a/b", b"/a/b/c").unwrap();

        // Assert
        assert_eq!(
            Ok(b"ab".to_vec()),
            state.store.handle_store_read(b"/a/b/c", 0, 4096)
        );
        assert_eq!(
            Ok(b"abc".to_vec()),
            state.store.handle_store_read(b"/a/b/c/c", 0, 4096)
        );
        assert_eq!(
            b"abd".to_vec(),
            state.store.handle_store_read(b"/a/b/c/d", 0, 4096).unwrap()
        );
        assert_eq!(
            b"abc".to_vec(),
            state.store.handle_store_read(b"/a/bc", 0, 4096).unwrap()
        );
        assert_eq!(
            Ok(VALUE_TYPE_SUBTREE),
            state.store.handle_store_has(b"/a/b")
        );
        assert_eq!(
            Ok(VALUE_TYPE_NONE),
            state.store.handle_store_has(b"/a/b/c/z")
        );
    }

    #[test]
    fn store_copy() {
        // Arrange
        let mut state = HostState::default();
        state.store.handle_store_write(b"/a/b", 0, b"ab").unwrap();
        state
            .store
            .handle_store_write(b"/a/b/c", 0, b"abc")
            .unwrap();
        state
            .store
            .handle_store_write(b"/a/b/c/z", 0, b"abcz")
            .unwrap();
        state
            .store
            .handle_store_write(b"/a/b/d", 0, b"abd")
            .unwrap();
        state.store.handle_store_write(b"/a/bc", 0, b"abc").unwrap();

        // Act
        state.store.handle_store_copy(b"/a/b", b"/a/b/c").unwrap();

        // Assert
        assert_eq!(
            b"ab".to_vec(),
            state.store.handle_store_read(b"/a/b/c", 0, 4096).unwrap()
        );
        assert_eq!(
            b"abc".to_vec(),
            state.store.handle_store_read(b"/a/b/c/c", 0, 4096).unwrap()
        );
        assert_eq!(
            b"abd".to_vec(),
            state.store.handle_store_read(b"/a/b/c/d", 0, 4096).unwrap()
        );
        assert_eq!(
            b"abc".to_vec(),
            state.store.handle_store_read(b"/a/bc", 0, 4096).unwrap()
        );
        assert_eq!(
            b"ab".to_vec(),
            state.store.handle_store_read(b"/a/b", 0, 4096).unwrap()
        );
        assert_eq!(
            Ok(VALUE_TYPE_NONE),
            state.store.handle_store_has(b"/a/b/c/z")
        );
    }

    #[test]
    fn simple_store_copy() {
        // Arrange
        let mut state = HostState::default();
        state.store.handle_store_write(b"/a/b", 0, b"xxx").unwrap();

        // Act
        state.store.handle_store_copy(b"/a", b"/c").unwrap();

        // Assert
        assert_eq!(
            b"xxx".to_vec(),
            state.store.handle_store_read(b"/a/b", 0, 4096).unwrap()
        );
        assert_eq!(
            b"xxx".to_vec(),
            state.store.handle_store_read(b"/c/b", 0, 4096).unwrap()
        );
    }

    #[test]
    fn store_value_size() {
        let mut state = HostState::default();
        let size = 256_i32;
        let data = vec![b'a'; size as usize];
        let path = b"/a/b";
        state.store.handle_store_write(path, 0, &data).unwrap();

        let value_size = state.store.handle_store_value_size(path);

        assert_eq!(Ok(size), value_size);
    }

    #[test]
    fn read_from_readonly() {
        let mut state = HostState::default();
        let path = "/readonly/kernel/env/reboot_count";
        state.store.0.set_value(path, 4i32.to_le_bytes().to_vec());
        let read_back = state
            .store
            .handle_store_read(path.as_bytes(), 0, 4)
            .unwrap();
        assert_eq!(read_back, [4, 0, 0, 0]);
    }

    #[test]
    fn set_and_get_dal_slot() {
        let mut state = HostState::default();
        let data = vec![b'z'; 512];
        let published_level = 10;
        let slot_index = 4;
        state.set_dal_slot(published_level, slot_index, data.clone());
        let data_in_slot = state
            .get_dal_slot(published_level, slot_index)
            .expect("Slot should contain data");
        assert_eq!(&data, data_in_slot);
    }
}
