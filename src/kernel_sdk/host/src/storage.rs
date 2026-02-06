// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Hosts will usually provide storage functionality, of a certain flavour.
//!
//! There are two durable storage systems; one backed by irmin (currently integrated in the WASM pvm)
//! and a new kind that will be integrated into both the WASM and RISC-V pvms.
//!
//! We therefore specify irmin-flavoured durable storage as [`StorageV1`], and the new durable storage
//! as `StorageV2`.
// TODO (TZX-38): define `StorageV2` trait.

mod v1;

pub use v1::StorageV1;
