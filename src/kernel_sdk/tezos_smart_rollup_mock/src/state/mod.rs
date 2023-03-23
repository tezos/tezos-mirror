// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2022 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Mock runtime state & state transitions

use crypto::hash::SmartRollupHash;
use tezos_smart_rollup_core::{
    MAX_FILE_CHUNK_SIZE, MAX_INPUT_MESSAGE_SIZE, MAX_OUTPUT_SIZE, PREIMAGE_HASH_SIZE,
};
use tezos_smart_rollup_host::{metadata::RollupMetadata, path::RefPath, Error};

pub(crate) mod store;

use self::store::Store;

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
    pub store: Store,
    pub metadata: RollupMetadata,
    // Inbox metadata
    pub(crate) curr_level: u32,
    pub(crate) curr_input_id: usize,
    pub(crate) input: Vec<Vec<u8>>,
}

impl Default for HostState {
    fn default() -> Self {
        let store = Store::default();
        let raw_rollup_address: [u8; 20] =
            SmartRollupHash::from_base58_check("sr1V6huFSUBUujzubUCg9nNXqpzfG9t4XD1h")
                .unwrap()
                .0
                .try_into()
                .unwrap();

        let metadata = RollupMetadata {
            raw_rollup_address,
            origination_level: crate::MUMBAI_ACTIVATION_LEVEL,
        };
        Self {
            store,
            metadata,
            curr_level: crate::MUMBAI_ACTIVATION_LEVEL,
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

        if self.store.outbox_at(self.curr_level).len() < MAX_OUTPUTS_PER_LEVEL {
            self.store.outbox_insert(self.curr_level, output);
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
        let preimage = self.store.retrieve_preimage(hash);
        if preimage.len() < max_bytes {
            preimage
        } else {
            &preimage[0..max_bytes]
        }
    }

    pub(crate) fn set_preimage(&mut self, preimage: Vec<u8>) -> [u8; PREIMAGE_HASH_SIZE] {
        self.store.add_preimage(preimage)
    }

    pub(crate) fn get_metadata(&self) -> &RollupMetadata {
        &self.metadata
    }

    pub(crate) fn handle_store_has(&self, raw_path: &[u8]) -> Result<i32, Error> {
        let path = validate_path(raw_path)?;

        let has_value = self.store.has_entry(&path);
        let has_subvalue = self.handle_store_list_size(raw_path).unwrap_or_default()
            > i64::from(has_value);

        let result = match (has_value, has_subvalue) {
            (false, false) => tezos_smart_rollup_core::VALUE_TYPE_NONE,
            (true, false) => tezos_smart_rollup_core::VALUE_TYPE_VALUE,
            (false, true) => tezos_smart_rollup_core::VALUE_TYPE_SUBTREE,
            (true, true) => tezos_smart_rollup_core::VALUE_TYPE_VALUE_WITH_SUBTREE,
        };

        Ok(result)
    }

    pub(crate) fn handle_store_read(
        &self,
        path: &[u8],
        offset: usize,
        max_bytes: usize,
    ) -> Result<Vec<u8>, Error> {
        let path = validate_path(path)?;

        if !self.store.has_entry(&path) {
            return Err(Error::StoreNotAValue);
        }

        let bytes: Vec<u8> = self.store.get_value(&path);
        if offset > bytes.len() {
            return Err(Error::StoreInvalidAccess);
        }

        let num_bytes = usize::min(
            MAX_FILE_CHUNK_SIZE,
            usize::min(max_bytes, bytes.len() - offset),
        );
        let mut value = Vec::with_capacity(num_bytes);

        value.extend_from_slice(&bytes[offset..(offset + num_bytes)]);
        Ok(value)
    }

    pub(crate) fn handle_store_write(
        &mut self,
        path: &[u8],
        offset: usize,
        bytes: &[u8],
    ) -> Result<(), Error> {
        if bytes.len() > MAX_FILE_CHUNK_SIZE {
            return Err(Error::InputOutputTooLarge);
        }

        let path = validate_path(path)?;

        let mut value: Vec<u8> = match self.store.maybe_get_value(&path) {
            Some(value) => value,
            // No value, so only valid offset is zero (ie writing a new value).
            None => Vec::with_capacity(bytes.len()),
        };

        if offset > value.len() {
            return Err(Error::StoreInvalidAccess);
        } else if offset < value.len() && (offset + bytes.len()) <= value.len() {
            let _ = value
                .splice(offset..(offset + bytes.len()), bytes.iter().copied())
                .collect::<Vec<_>>();
        } else {
            value.truncate(offset);
            value.extend_from_slice(bytes);
        };

        self.store.set_value(&path, value);

        Ok(())
    }

    pub(crate) fn handle_store_delete(&mut self, prefix: &[u8]) -> Result<(), Error> {
        let durable_prefix = validate_path(prefix)?;

        self.store.node_delete(&durable_prefix);
        Ok(())
    }

    pub(crate) fn handle_store_list_size(&self, prefix: &[u8]) -> Result<i64, Error> {
        let prefix = validate_path(prefix)?;
        self.store
            .node_from_path(&prefix)
            .map(|n| n.inner.len() as i64)
            .ok_or(Error::StoreNotANode)
    }

    pub(crate) fn handle_store_list_get(
        &self,
        prefix: &[u8],
        index: i64,
    ) -> Result<String, Error> {
        let prefix = validate_path(prefix)?;
        self.store
            .node_from_path(&prefix)
            .and_then(|n| n.inner.keys().nth(index as usize))
            .ok_or(Error::StoreInvalidSubkeyIndex)
            .cloned()
    }

    pub(crate) fn handle_store_move(
        &mut self,
        from_path: &[u8],
        to_path: &[u8],
    ) -> Result<(), Error> {
        let from_durable_prefix = validate_path(from_path)?;
        let to_durable_prefix = validate_path(to_path)?;

        let from_node = self
            .store
            .node_from_path(&from_durable_prefix)
            .ok_or(Error::StoreNotANode)?
            .clone();

        self.store.node_delete(&from_durable_prefix);
        self.store.node_insert(&to_durable_prefix, from_node);

        Ok(())
    }

    pub(crate) fn handle_store_copy(
        &mut self,
        from_path: &[u8],
        to_path: &[u8],
    ) -> Result<(), Error> {
        let from_durable_prefix = validate_path(from_path)?;
        let to_durable_prefix = validate_path(to_path)?;

        let from_node = self
            .store
            .node_from_path(&from_durable_prefix)
            .ok_or(Error::StoreNotANode)?
            .clone();

        self.store.node_insert(&to_durable_prefix, from_node);

        Ok(())
    }

    pub(crate) fn handle_store_value_size(&self, path: &[u8]) -> Result<i32, Error> {
        let path = validate_path(path)?;
        if !self.store.has_entry(&path) {
            return Err(Error::StoreNotAValue);
        }
        let value: Vec<u8> = self.store.get_value(&path);

        Ok(value.len() as i32)
    }
}

fn validate_path(s: &[u8]) -> Result<String, Error> {
    use tezos_smart_rollup_host::path::PathError;

    match RefPath::try_from(s) {
        Err(PathError::PathTooLong) => Err(Error::StoreKeyTooLarge),
        Err(_) => Err(Error::StoreInvalidKey),
        Ok(_) => {
            // SAFETY: a valid path is valid UTF-8
            Ok(unsafe { String::from_utf8_unchecked(s.to_vec()) })
        }
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
        state.handle_store_write(path, 0, &written).unwrap();

        // Assert
        assert_eq!(
            Ok(VALUE_TYPE_VALUE),
            state.handle_store_has(path),
            "Path previously written to"
        );
        assert_eq!(Ok(written), state.handle_store_read(path, 0, 4096));
    }

    #[test]
    fn store_write_extend_path() {
        // Arrange
        let mut state = HostState::default();
        let path: &[u8] = b"/test/path";

        let written = vec![1, 2, 3, 4];
        state.handle_store_write(path, 0, &written).unwrap();

        // Act
        state.handle_store_write(path, 4, &written).unwrap();

        // Assert
        let expected = vec![1, 2, 3, 4, 1, 2, 3, 4];
        assert_eq!(
            Ok(VALUE_TYPE_VALUE),
            state.handle_store_has(path),
            "Path previously written to"
        );
        assert_eq!(Ok(expected), state.handle_store_read(path, 0, 4096));
    }

    #[test]
    fn store_write_extend_from_within() {
        // Arrange
        let mut state = HostState::default();
        let path: &[u8] = b"/test/path";

        let written = vec![1, 2, 3, 4];
        state.handle_store_write(path, 0, &written).unwrap();

        // Act
        state.handle_store_write(path, 2, &written).unwrap();

        // Assert
        let expected = vec![1, 2, 1, 2, 3, 4];
        assert_eq!(
            Ok(VALUE_TYPE_VALUE),
            state.handle_store_has(path),
            "Path previously written to"
        );
        assert_eq!(Ok(expected), state.handle_store_read(path, 0, 4096));
    }

    #[test]
    fn store_write_fully_within() {
        // Arrange
        let mut state = HostState::default();
        let path: &[u8] = b"/test/path";

        state.handle_store_write(path, 0, &[1, 2, 3, 4]).unwrap();

        // Act
        state.handle_store_write(path, 1, &[9, 10]).unwrap();

        // Assert
        let expected = vec![1, 9, 10, 4];
        assert_eq!(
            Ok(VALUE_TYPE_VALUE),
            state.handle_store_has(path),
            "Path previously written to"
        );
        assert_eq!(Ok(expected), state.handle_store_read(path, 0, 4096));
    }

    #[test]
    fn test_store_delete() {
        // Arrange
        let mut state = HostState::default();
        let prefix = "/a/long/prefix";

        state.handle_store_write(prefix.as_bytes(), 0, &[]).unwrap();

        for i in 0..10 {
            // subkey of prefix
            let subkey = format!("{}/{}", prefix, i);
            // not subkey as not a sub-path
            let almost_subkey = format!("{}{}", prefix, i);
            // completely different prefix
            let not_subkey = format!("/different/prefix/{}", i);

            state.handle_store_write(subkey.as_bytes(), 0, &[]).unwrap();
            state
                .handle_store_write(almost_subkey.as_bytes(), 0, &[])
                .unwrap();
            state
                .handle_store_write(not_subkey.as_bytes(), 0, &[])
                .unwrap();
        }

        // Act
        state.handle_store_delete(prefix.as_bytes()).unwrap();

        // Assert
        assert_eq!(
            Ok(VALUE_TYPE_NONE),
            state.handle_store_has(prefix.as_bytes())
        );

        assert_eq!(
            Err(Error::StoreNotANode),
            state.handle_store_list_size(prefix.as_bytes())
        );
    }

    #[test]
    fn store_list_size() {
        // Arrange
        let mut state = HostState::default();
        let prefix = "/a/long/prefix";

        for i in 0..10 {
            // subkey of prefix
            let subkey = format!("{}/{}", prefix, i);
            // not subkey as not a sub-path
            let almost_subkey = format!("{}{}", prefix, i);
            // completely different prefix
            let not_subkey = format!("/different/prefix/{}", i);

            state.handle_store_write(subkey.as_bytes(), 0, &[]).unwrap();
            state
                .handle_store_write(almost_subkey.as_bytes(), 0, &[])
                .unwrap();
            state
                .handle_store_write(not_subkey.as_bytes(), 0, &[])
                .unwrap();
        }

        // Act
        let result = state.handle_store_list_size(prefix.as_bytes()).unwrap();

        // Assert
        assert_eq!(10, result, "Expected 10 subkeys of prefix");
    }

    #[test]
    fn store_list_get() {
        // Arrange
        let mut state = HostState::default();
        let prefix = "/a/long/prefix";

        for i in 0..10 {
            // subkey of prefix
            let subkey = format!("{}/{}", prefix, i);
            // not subkey as not a sub-path
            let almost_subkey = format!("{}{}", prefix, i);
            // completely different prefix
            let not_subkey = format!("/different/prefix/{}", i);

            state.handle_store_write(subkey.as_bytes(), 0, &[]).unwrap();
            state
                .handle_store_write(almost_subkey.as_bytes(), 0, &[])
                .unwrap();
            state
                .handle_store_write(not_subkey.as_bytes(), 0, &[])
                .unwrap();
        }

        // Act
        let mut results = Vec::new();
        for i in 0..10 {
            results.push(
                state
                    .handle_store_list_get(prefix.as_bytes(), i)
                    .expect("Subkey exists"),
            );
        }
        results.sort();

        // Assert
        let expected = (0..10).map(|i| i.to_string()).collect::<Vec<_>>();
        assert_eq!(results, expected, "Expected stable store_list_get");
    }

    #[test]
    fn store_move() {
        // Arrange
        let mut state = HostState::default();
        state.handle_store_write(b"/a/b", 0, b"ab").unwrap();
        state.handle_store_write(b"/a/b/c", 0, b"abc").unwrap();
        state.handle_store_write(b"/a/b/c/z", 0, b"abcz").unwrap();
        state.handle_store_write(b"/a/b/d", 0, b"abd").unwrap();
        state.handle_store_write(b"/a/bc", 0, b"abc").unwrap();

        // Act
        state.handle_store_move(b"/a/b", b"/a/b/c").unwrap();

        // Assert
        assert_eq!(
            Ok(b"ab".to_vec()),
            state.handle_store_read(b"/a/b/c", 0, 4096)
        );
        assert_eq!(
            Ok(b"abc".to_vec()),
            state.handle_store_read(b"/a/b/c/c", 0, 4096)
        );
        assert_eq!(
            b"abd".to_vec(),
            state.handle_store_read(b"/a/b/c/d", 0, 4096).unwrap()
        );
        assert_eq!(
            b"abc".to_vec(),
            state.handle_store_read(b"/a/bc", 0, 4096).unwrap()
        );
        assert_eq!(Ok(VALUE_TYPE_SUBTREE), state.handle_store_has(b"/a/b"));
        assert_eq!(Ok(VALUE_TYPE_NONE), state.handle_store_has(b"/a/b/c/z"));
    }

    #[test]
    fn store_copy() {
        // Arrange
        let mut state = HostState::default();
        state.handle_store_write(b"/a/b", 0, b"ab").unwrap();
        state.handle_store_write(b"/a/b/c", 0, b"abc").unwrap();
        state.handle_store_write(b"/a/b/c/z", 0, b"abcz").unwrap();
        state.handle_store_write(b"/a/b/d", 0, b"abd").unwrap();
        state.handle_store_write(b"/a/bc", 0, b"abc").unwrap();

        // Act
        state.handle_store_copy(b"/a/b", b"/a/b/c").unwrap();

        // Assert
        assert_eq!(
            b"ab".to_vec(),
            state.handle_store_read(b"/a/b/c", 0, 4096).unwrap()
        );
        assert_eq!(
            b"abc".to_vec(),
            state.handle_store_read(b"/a/b/c/c", 0, 4096).unwrap()
        );
        assert_eq!(
            b"abd".to_vec(),
            state.handle_store_read(b"/a/b/c/d", 0, 4096).unwrap()
        );
        assert_eq!(
            b"abc".to_vec(),
            state.handle_store_read(b"/a/bc", 0, 4096).unwrap()
        );
        assert_eq!(
            b"ab".to_vec(),
            state.handle_store_read(b"/a/b", 0, 4096).unwrap()
        );
        assert_eq!(Ok(VALUE_TYPE_NONE), state.handle_store_has(b"/a/b/c/z"));
    }

    #[test]
    fn simple_store_copy() {
        // Arrange
        let mut state = HostState::default();
        state.handle_store_write(b"/a/b", 0, b"xxx").unwrap();

        // Act
        state.handle_store_copy(b"/a", b"/c").unwrap();

        // Assert
        assert_eq!(
            b"xxx".to_vec(),
            state.handle_store_read(b"/a/b", 0, 4096).unwrap()
        );
        assert_eq!(
            b"xxx".to_vec(),
            state.handle_store_read(b"/c/b", 0, 4096).unwrap()
        );
    }

    #[test]
    fn store_value_size() {
        let mut state = HostState::default();
        let size = 256_i32;
        let data = vec![b'a'; size as usize];
        let path = b"/a/b";
        state.handle_store_write(path, 0, &data).unwrap();

        let value_size = state.handle_store_value_size(path);

        assert_eq!(Ok(size), value_size);
    }
}
