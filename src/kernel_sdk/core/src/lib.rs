// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2022-2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

#![doc = include_str!("../README.md")]
#![cfg_attr(not(feature = "mock-core-trait"), no_std)]
#![deny(rustdoc::broken_intra_doc_links)]

pub mod rollup_host;
pub mod smart_rollup_core;

#[cfg(target_arch = "wasm32")]
#[path = "wasm_target.rs"]
#[doc(hidden)]
// The WebAssembly target implementation is exposed because the panic hook needs access to
// `write_debug`.
pub mod target_impl;

#[cfg(all(target_arch = "riscv64", target_os = "hermit", feature = "proto-alpha"))]
#[path = "riscv_hermit_target.rs"]
#[doc(hidden)]
mod target_impl;

#[cfg(not(any(
    target_arch = "wasm32",
    all(target_arch = "riscv64", target_os = "hermit", feature = "proto-alpha")
)))]
#[path = "fallback_target.rs"]
#[doc(hidden)]
mod target_impl;

pub use smart_rollup_core::SmartRollupCore;
pub use tezos_smart_rollup_constants::core::*;
