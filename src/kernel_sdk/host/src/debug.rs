// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Debug capability for smart rollup kernels.
//!
//! Debug output is written statically — on WASM via the host import, on all other
//! targets via `eprintln!`. Use the [`debug_msg!`] and [`debug_str!`] macros from
//! `tezos_smart_rollup_debug` rather than interacting with this module directly.
//!
//! [`debug_msg!`]: tezos_smart_rollup_debug::debug_msg
//! [`debug_str!`]: tezos_smart_rollup_debug::debug_str
