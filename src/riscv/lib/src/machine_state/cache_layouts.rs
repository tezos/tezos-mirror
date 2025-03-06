// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! This module provides a wrapper for controlling the sizes of various caches
//! (currently just the block cache).
//!
//! In future, this may be expanded to other compile-time-sized types that form
//! part of the machine state (e.g. main memory).

use super::block_cache;
use super::block_cache::BlockCacheLayout;

/// Configuration bucket for the size of caches.
pub enum Sizes<const BLOCK_CACHE_BITS: usize, const BLOCK_CACHE_SIZE: usize> {}

/// Wrapping trait for a bucket containing layouts for various caches used by the machine state.
pub trait CacheLayouts {
    /// Layout for the block cache - controlling the number of entries.
    type BlockCacheLayout: BlockCacheLayout;
}

impl<const BLOCK_CACHE_BITS: usize, const BLOCK_CACHE_SIZE: usize> CacheLayouts
    for Sizes<BLOCK_CACHE_BITS, BLOCK_CACHE_SIZE>
{
    type BlockCacheLayout = block_cache::Layout<BLOCK_CACHE_BITS, BLOCK_CACHE_SIZE>;
}

/// The default configuration of cache layouts.
pub type DefaultCacheLayouts =
    Sizes<{ block_cache::DEFAULT_CACHE_BITS }, { block_cache::DEFAULT_CACHE_SIZE }>;

/// The default configuration of cache layouts for tests.
pub type TestCacheLayouts =
    Sizes<{ block_cache::TEST_CACHE_BITS }, { block_cache::TEST_CACHE_SIZE }>;
