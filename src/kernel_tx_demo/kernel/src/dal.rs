// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Utility functions for handling DAL slots.
//!
//! This is currently **experimental** and should be used with caution.
#![cfg(feature = "dal")]

extern crate alloc;
extern crate tezos_crypto_rs as crypto;

use tezos_smart_rollup_host::path::OwnedPath;

#[cfg(feature = "debug")]
use tezos_smart_rollup_debug::debug_msg;

use tezos_smart_rollup_host::runtime::Runtime;

#[allow(dead_code)]
pub(crate) fn store_dal_slot(
    host: &mut impl Runtime,
    published_level: i32,
    num_pages: usize,
    page_size: usize,
    slot_index: u8,
    path_to_store: OwnedPath,
) {
    let mut buffer = vec![0u8; page_size];
    for page_index in 0..(num_pages as i16) {
        let result = host.reveal_dal_page(published_level, slot_index, page_index, &mut buffer);
        match result {
            Ok(size) => {
                #[cfg(feature = "debug")]
                debug_msg!(host, "Revealed dal page with size {size}\n");
                if size == 0 {
                    return;
                }
                host.store_write(&path_to_store, &buffer, (page_index as usize) * page_size)
                    .map_err(|_| "Error writing to storage".to_string())
                    .unwrap_or_default();
            }
            Err(_e) => {
                #[cfg(feature = "debug")]
                debug_msg!(host, "Failed to reveal dal page. Error: {:?}", _e);
                // Stop fetching pages on error
                return;
            }
        }
    }
}
