// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Library for encodings related to the Tezos SCORU system.
#![cfg_attr(not(feature = "alloc"), no_std)]
#![deny(missing_docs)]
#![deny(rustdoc::broken_intra_doc_links)]
#![forbid(unsafe_code)]

#[cfg(feature = "alloc")]
extern crate alloc;
#[cfg(feature = "crypto")]
extern crate tezos_crypto_rs as crypto;
extern crate tezos_smart_rollup_host as host;

#[cfg(feature = "crypto")]
pub mod contract;
pub mod dac;
#[cfg(feature = "alloc")]
pub mod entrypoint;
#[cfg(feature = "alloc")]
pub mod inbox;
#[cfg(feature = "alloc")]
pub mod michelson;
#[cfg(feature = "alloc")]
pub mod outbox;
#[cfg(feature = "crypto")]
pub mod public_key_hash;

#[cfg(feature = "crypto")]
pub mod smart_rollup;
pub mod timestamp;

#[cfg(feature = "testing")]
pub mod testing;
