// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::{collections::HashMap, sync::Arc};

use tezos_smart_rollup_constants::core::{METADATA_LENGTH, ROLLUP_ADDRESS_LENGTH};

type ResponseFn = Arc<dyn Fn() -> Result<Box<[u8]>, std::io::Error>>;

/// Data structure that maps reveal request to reveal response
#[derive(Clone, Default)]
pub struct RevealRequestResponseMap {
    map: HashMap<Box<[u8]>, ResponseFn>,
}

impl RevealRequestResponseMap {
    // TODO RV-458: Sandbox can load pre-images and provide them when it receives reveal request
    // Currently, one sample record with dummy data is added to the map for testing purpose
    /// Construct the mapping from reveal request to reveal response
    pub fn new(rollup_address: [u8; 20], origination_level: u32) -> Self {
        let mut reveal_request_response_map = RevealRequestResponseMap::default();

        // Entry for returning dummy data to generic reveal request
        reveal_request_response_map.add_static(
            [
                0, 1, 10, 20, 30, 40, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                1, 1, 1, 1, 100, 120, 140, 160,
            ],
            [
                150, 160, 170, 180, 190, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 50, 60, 70, 80, 90,
            ],
        );

        // Entry for responding to reveal_metadata request
        let mut metadata_response_buffer = [0u8; METADATA_LENGTH];
        metadata_response_buffer[..ROLLUP_ADDRESS_LENGTH].copy_from_slice(&rollup_address);
        metadata_response_buffer[ROLLUP_ADDRESS_LENGTH..]
            .copy_from_slice(&origination_level.to_be_bytes());
        reveal_request_response_map.add_static([1u8], metadata_response_buffer);

        reveal_request_response_map
    }

    /// Construct an entry of RevealRequestResponseMap with response that requires loading
    #[allow(dead_code)]
    pub fn add_handler(
        &mut self,
        request: impl Into<Box<[u8]>>,
        response: impl Fn() -> Result<Box<[u8]>, std::io::Error> + 'static,
    ) {
        self.map.insert(request.into(), Arc::new(response));
    }

    /// Construct an entry of RevealRequestResponseMap with static response
    pub fn add_static(&mut self, request: impl Into<Box<[u8]>>, response: impl Into<Box<[u8]>>) {
        let response_value: Box<[u8]> = response.into();
        self.add_handler(request, move || Ok(response_value.clone()));
    }

    /// Get response function for the given request
    pub fn get_response(&self, request: &[u8]) -> Option<ResponseFn> {
        self.map.get(request).cloned()
    }
}
