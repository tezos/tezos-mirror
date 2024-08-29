// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! This module contains a cache for the address translation of virtual addresses to physical ones.
//!
//! The cache is structured into two levels:
//!
//! 1. Cache by access type
//! 2. Cache by virtual page buckets
//!
//! [`TranslationCache`] `-( access_type: AccessType )>` [`AccessCache`] `-( virt_page: u64 )>` [`Cached`]
//!
//! This means for any lookup, the first level is resolved using the given access type. The second
//! level is resolved using the virtual page number of the given virtual address.
//!
//! After that, the cache entry [`Cached`] is probed for validity. If it is valid, the physical
//! page number in the cache entry is combined with the offset of the virtual address to obtain the
//! complete physical address.
//!
//! Cache invalidation is achieved in two ways:
//!
//! 1. Either store an invalid virtual page address in the cache entry (e.g. `!0`), or
//! 2. Increment the fence counter at the [`AccessCache`] level.
//!
//! A cache entry is only valid when its fence counter matches that of the containing cache level.
//!
//! For example a cache entry `c: Cached` is valid if `c.fence_counter == a.fence_counter` for an `a:
//! AccessCache` that contains the respective cache entry.
//!
//! To avoid invalid-to-valid transitions when bumping the fence counter, all cache entries need to
//! be invalidated before the fence counter inhabits a previously used value. The cache can't
//! invalidate the cache entries all at once because that would result in a very large proof for
//! a single invalidation step. Therefore, we do it gradually.

use super::{AccessType, PAGE_OFFSET_WIDTH};
use crate::{
    bits::ones,
    cache_utils::FenceCounter,
    machine_state::{bus::Address, csregisters::CSRRepr, mode::Mode},
    state_backend::{AllocatedOf, Atom, Cell, Manager, ManagerBase, Many},
};
use strum::EnumCount;

/// Bit mask to obtain the offset into a page
const OFFSET_MASK: u64 = ones(PAGE_OFFSET_WIDTH as u64);

/// Bit mask to obtain the page number
const PAGE_MASK: u64 = !OFFSET_MASK;

/// Compute the index of the cache entry for a given virtual page.
#[inline(always)]
const fn virtual_page_index(virt_page: u64) -> usize {
    ((virt_page >> PAGE_OFFSET_WIDTH) as usize) & PAGES_MASK
}

/// Layout of a cached address translation item
type CachedLayout = (
    Atom<u8>,
    Atom<FenceCounter>,
    Atom<CSRRepr>,
    Atom<Address>,
    Atom<Address>,
);

/// Cached address translation
struct Cached<M: ManagerBase> {
    /// Privilege mode
    mode: Cell<u8, M>,

    /// SATP value
    satp: Cell<CSRRepr, M>,

    /// Virtual page number
    virt_page: Cell<Address, M>,

    /// Physical page number
    phys_page: Cell<Address, M>,

    /// Counter to invalidate the cache entry
    fence_counter: Cell<FenceCounter, M>,
}

impl<M: Manager> Cached<M> {
    /// Bind the allocated cells.
    fn bind(space: AllocatedOf<CachedLayout, M>) -> Self {
        Self {
            mode: space.0,
            fence_counter: space.1,
            satp: space.2,
            virt_page: space.3,
            phys_page: space.4,
        }
    }

    /// Reset the underlying storage.
    pub fn reset(&mut self) {
        self.fence_counter.write(FenceCounter::INITIAL);
        self.mode.write(!0);
        self.satp.write(!0);
        self.virt_page.write(!0);
        self.phys_page.write(!0);
    }

    /// Invalidate the cache entry.
    #[inline]
    fn invalidate(&mut self) {
        self.virt_page.write(!0);
    }

    /// Update the cache entry.
    fn cache_translation(
        &mut self,
        satp: CSRRepr,
        mode: Mode,
        virt_page: Address,
        phys_page: Address,
        fence_counter: FenceCounter,
    ) {
        self.satp.write(satp);
        self.mode.write(mode as u8);
        self.virt_page.write(virt_page);
        self.phys_page.write(phys_page);
        self.fence_counter.write(fence_counter);
    }

    /// Probe the cache entry for validity.
    fn try_translate(
        &self,
        satp: CSRRepr,
        mode: Mode,
        virt_page: Address,
        virt_offset: Address,
        fence_counter: FenceCounter,
    ) -> Option<u64> {
        if self.virt_page.read() == virt_page
            && self.satp.read() == satp
            && self.mode.read() == mode as u8
            && self.fence_counter.read() == fence_counter
        {
            Some(self.phys_page.read() | virt_offset)
        } else {
            None
        }
    }
}

/// Caching 65,536 pages of 4 KiB each is 768 MiB of memory cached. The cache state size for this
/// chosen configuration is 6 MiB.
const PAGES_BITS: usize = 16;
const PAGES: usize = 1 << PAGES_BITS;
const PAGES_MASK: usize = PAGES - 1;

/// Layout of the cache that caches address translations for a single access type
type AccessCacheLayout = (Atom<FenceCounter>, Many<CachedLayout, PAGES>);

/// Address translation cache for a single access type
struct AccessCache<M: ManagerBase> {
    fence_counter: Cell<FenceCounter, M>,
    entries: Box<[Cached<M>; PAGES]>,
}

impl<M: Manager> AccessCache<M> {
    /// Bind the address translation cache to the allocated items.
    fn bind(space: AllocatedOf<AccessCacheLayout, M>) -> Self {
        Self {
            fence_counter: space.0,
            entries: space
                .1
                .into_iter()
                .map(Cached::bind)
                .collect::<Vec<_>>()
                .into_boxed_slice()
                .try_into()
                .map_err(|_| "mismatching vector lengths for address translation cache pages")
                .unwrap(),
        }
    }

    /// Reset the underlying storage.
    fn reset(&mut self) {
        self.fence_counter.write(FenceCounter::INITIAL);
        self.entries.iter_mut().for_each(Cached::reset);
    }

    /// Invalidate the entire cache.
    #[inline]
    fn invalidate(&mut self) {
        let fence_counter = self.fence_counter.read();
        self.fence_counter.write(fence_counter.next());

        // We try to reset every invalid cache entry before the fence counter wraps to avoid
        // false-positive cache hits.
        //
        // In other words, once a cache entry has been populated, in the absence of any
        // modifications to it, you can call [`invalidate`] [`FenceCounter::MAX`] times and the
        // cache entry will be incorrectly considered valid again. We want to protect againt this
        // by invalidating the cache entry before the fence counter reaches the value with which
        // the cache entry was populated.
        //
        // We want this invalidate method to be fast and have a low proof-size foot print.
        // Therefore invalidating all entries at the same time is not an option.
        let virt_page_index = fence_counter.0 as usize & PAGES_MASK;
        self.entries[virt_page_index].invalidate();
    }

    /// Attempt to translate a virtual address.
    #[inline]
    fn try_translate(&self, satp: CSRRepr, mode: Mode, virt_addr: Address) -> Option<Address> {
        let virt_page = virt_addr & PAGE_MASK;
        let virt_page_index = virtual_page_index(virt_page);

        self.entries[virt_page_index].try_translate(
            satp,
            mode,
            virt_page,
            virt_addr & OFFSET_MASK,
            self.fence_counter.read(),
        )
    }

    /// Populate the cache entry for a given address translation.
    #[inline]
    fn cache_translation(
        &mut self,
        satp: CSRRepr,
        mode: Mode,
        virt_addr: Address,
        phys_addr: Address,
    ) {
        let virt_page = virt_addr & PAGE_MASK;
        let virt_page_index = virtual_page_index(virt_page);

        self.entries[virt_page_index].cache_translation(
            satp,
            mode,
            virt_page,
            phys_addr & PAGE_MASK,
            self.fence_counter.read(),
        );
    }
}

/// Number of access types
const NUM_ACCESS_TYPES: usize = AccessType::COUNT;

/// Compute the index in the cache buckets for the given access type.
#[inline(always)]
const fn access_type_index(access_type: AccessType) -> usize {
    match access_type {
        AccessType::Instruction => 0,
        AccessType::Load => 1,
        AccessType::Store => 2,
    }
}

/// Layout of the address translation cache
pub type TranslationCacheLayout = [AccessCacheLayout; NUM_ACCESS_TYPES];

/// Address translation cache
pub struct TranslationCache<M: ManagerBase> {
    /// Cache entries
    entries: [AccessCache<M>; NUM_ACCESS_TYPES],
}

impl<M: Manager> TranslationCache<M> {
    /// Bind the allocated cells.
    pub fn bind(space: AllocatedOf<TranslationCacheLayout, M>) -> Self {
        Self {
            entries: space.map(AccessCache::bind),
        }
    }

    /// Reset the underlying state.
    pub fn reset(&mut self) {
        self.entries.iter_mut().for_each(AccessCache::reset);
    }

    /// Invalidate the cache.
    #[inline]
    pub fn invalidate(&mut self, access_types: impl IntoIterator<Item = AccessType>) {
        for access_type in access_types {
            let access_type_index = access_type_index(access_type);
            self.entries[access_type_index].invalidate();
        }
    }

    /// Probe the cache for an already translated address.
    #[inline]
    pub fn try_translate(
        &self,
        mode: Mode,
        satp: CSRRepr,
        access_type: AccessType,
        virt_addr: Address,
    ) -> Option<Address> {
        let access_type_index = access_type_index(access_type);
        self.entries[access_type_index].try_translate(satp, mode, virt_addr)
    }

    /// Update the cache entry for a given virtual address translation.
    #[inline]
    pub fn cache_translation(
        &mut self,
        mode: Mode,
        satp: CSRRepr,
        access_type: AccessType,
        virt_addr: Address,
        phys_addr: Address,
    ) {
        let access_type_index = access_type_index(access_type);
        self.entries[access_type_index].cache_translation(satp, mode, virt_addr, phys_addr);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pages_integrity() {
        assert_eq!(PAGES_MASK.trailing_ones() as usize, PAGES_BITS);
        assert_eq!(usize::MAX & PAGES_MASK, PAGES - 1);
    }

    #[test]
    fn test_fence_counter_wrapping_protection() {
        let invalidation_count_until_wrapping = FenceCounter::MAX.0 as usize;
        let cache_entries = PAGES * NUM_ACCESS_TYPES;

        assert!(
            invalidation_count_until_wrapping > cache_entries,
            "The fence counter does a full cycle before all cache entries could be validated!"
        );
    }
}
