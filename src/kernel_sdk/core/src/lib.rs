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

/// The maximum size of input that can be read in one go from a slot message.
pub const MAX_INPUT_SLOT_DATA_CHUNK_SIZE: usize = 4096;

/// The maximum size of input that can be read in one go from a Layer 1 message.
pub const MAX_INPUT_MESSAGE_SIZE: usize = 4096;

/// The maximum size that may be written to `output` in one go.
pub const MAX_OUTPUT_SIZE: usize = 4096;

/// The maximum size that may be written to, or read from, disk in one go.
pub const MAX_FILE_CHUNK_SIZE: usize = 2048;

/// The size of a preimage *Reveal_hash* hash in bytes.
pub const PREIMAGE_HASH_SIZE: usize = 33;

/// The store key submitted as an argument of a host function exceeds the
/// authorized limit.
pub const STORE_KEY_TOO_LARGE: i32 = -1;

/// The store key submitted as an argument of a host function cannot be parsed.
pub const STORE_INVALID_KEY: i32 = -2;

/// The contents (if any) of the store under the key submitted as an argument of
/// a host function is not a value.
pub const STORE_NOT_A_VALUE: i32 = -3;

/// An access in a value of the durable storage has failed, supposedly out of
/// bounds of a value.
pub const STORE_INVALID_ACCESS: i32 = -4;

/// Writing a value has exceeded 2^31 bytes.
pub const STORE_VALUE_SIZE_EXCEEDED: i32 = -5;

/// An address is out of bound of the memory.
pub const MEMORY_INVALID_ACCESS: i32 = -6;

/// The input or output submitted as an argument of a host function exceeds the
/// authorized limit.
pub const INPUT_OUTPUT_TOO_LARGE: i32 = -7;

/// Generic error code for unexpected errors.
pub const GENERIC_INVALID_ACCESS: i32 = -8;

/// A value cannot be modified if it is readonly.
pub const STORE_READONLY_VALUE: i32 = -9;

/// There is no tree at key. It has no value, nor any subtrees.
pub const STORE_NOT_A_NODE: i32 = -10;

/// The outbox is full an cannot accept new messages at this level.
pub const FULL_OUTBOX: i32 = -11;

/// None ValueType discriminant.
pub const VALUE_TYPE_NONE: i32 = 0;

/// Value ValueType discriminant, for a simple value in the store.
pub const VALUE_TYPE_VALUE: i32 = 1;

/// Subtree ValueType discriminant, for a subtree node in the store.
pub const VALUE_TYPE_SUBTREE: i32 = 2;

/// Value with subtree ValueType discriminant, for a value carrying a subtree node in the store.
pub const VALUE_TYPE_VALUE_WITH_SUBTREE: i32 = 3;
