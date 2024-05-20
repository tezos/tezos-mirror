// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2022-2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

#![doc = include_str!("../README.md")]
#![cfg_attr(not(any(test, feature = "testing")), no_std)]
#![deny(rustdoc::broken_intra_doc_links)]

pub mod rollup_host;
pub mod smart_rollup_core;

pub use smart_rollup_core::SmartRollupCore;

pub use tezos_smart_rollup_constants::core::*;
