// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2022-2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

#![doc = include_str!("../README.md")]
#![cfg_attr(not(feature = "mock-core-trait"), no_std)]
#![deny(rustdoc::broken_intra_doc_links)]

pub mod rollup_host;
pub mod smart_rollup_core;

#[cfg(pvm_kind = "wasm")]
#[path = "wasm_target.rs"]
#[doc(hidden)]
// The WebAssembly target implementation is exposed because the panic hook needs access to
// `write_debug`.
pub mod target_impl;

#[cfg(pvm_kind = "riscv")]
#[path = "riscv_target.rs"]
#[doc(hidden)]
mod target_impl;

#[cfg(pvm_kind = "none")]
#[path = "fallback_target.rs"]
#[doc(hidden)]
mod target_impl;

pub use smart_rollup_core::SmartRollupCore;
pub use tezos_smart_rollup_constants::core::*;
