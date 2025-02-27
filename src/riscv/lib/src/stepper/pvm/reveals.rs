// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::collections::HashMap;
use std::fs;
use std::path::Path;

use tezos_crypto_rs::blake2b::digest_256;
use tezos_smart_rollup_constants::core::METADATA_LENGTH;
use tezos_smart_rollup_constants::core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_constants::core::ROLLUP_ADDRESS_LENGTH;

/// Data structure that maps reveal request to reveal response
#[derive(Clone)]
pub struct RevealRequestResponseMap {
    map: HashMap<Box<[u8]>, Box<[u8]>>,
    preimages_dir: Option<Box<Path>>,
}

impl RevealRequestResponseMap {
    /// Construct the mapping from reveal request to reveal response
    pub fn new(
        rollup_address: [u8; 20],
        origination_level: u32,
        preimages_dir: Option<Box<Path>>,
    ) -> Self {
        let mut reveal_request_response_map = Self {
            map: HashMap::new(),
            preimages_dir,
        };

        // Entry for responding to reveal_metadata request
        reveal_request_response_map.add_metadata(rollup_address, origination_level);

        reveal_request_response_map
    }

    /// Construct an entry of RevealRequestResponseMap with static response
    pub fn add_static(&mut self, request: impl Into<Box<[u8]>>, response: impl Into<Box<[u8]>>) {
        let response_value: Box<[u8]> = response.into();
        self.map.insert(request.into(), response_value);
    }

    /// Get response function for the given request
    pub fn get_response(&self, request: &[u8]) -> Option<Box<[u8]>> {
        self.map
            .get(request)
            .cloned()
            .or_else(|| self.load_response_from_disk(request))
    }

    fn load_response_from_disk(&self, request: &[u8]) -> Option<Box<[u8]>> {
        let preimages_dir = self.preimages_dir.as_ref()?;

        // Expecting the first byte to be the tag for preimage hash
        if request.first() != Some(&0) {
            return None;
        }

        let preimage_hash = &request[1..];
        let hex_name = hex::encode(preimage_hash);
        let file_path = preimages_dir.join(hex_name);

        if !file_path.is_file() {
            return None;
        }

        let content = fs::read(file_path).ok()?;
        if check_preimage_hash(&content, preimage_hash) {
            Some(content.into_boxed_slice())
        } else {
            None
        }
    }

    fn add_metadata(
        &mut self,
        rollup_address: [u8; ROLLUP_ADDRESS_LENGTH],
        origination_level: u32,
    ) {
        let mut metadata_response_buffer = [0u8; METADATA_LENGTH];
        metadata_response_buffer[..ROLLUP_ADDRESS_LENGTH].copy_from_slice(&rollup_address);
        metadata_response_buffer[ROLLUP_ADDRESS_LENGTH..]
            .copy_from_slice(&origination_level.to_be_bytes());
        self.add_static([1u8], metadata_response_buffer);
    }
}

fn check_preimage_hash(preimage: &[u8], preimage_hash: &[u8]) -> bool {
    // Only blake2b (`preimage_hash[0] == 0`) is supported for now
    if preimage_hash.len() != PREIMAGE_HASH_SIZE || preimage_hash[0] != 0 {
        return false;
    }

    let computed_hash = digest_256(preimage);
    preimage_hash[1..] == computed_hash
}
