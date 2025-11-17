// SPDX-FileCopyrightText: 2023-2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Definitions relating to Layer-1 accounts, which the kernel may interact with.

#[cfg(feature = "testing")]
pub use tezos_protocol::contract::testing;

pub use tezos_protocol::contract::Contract;
