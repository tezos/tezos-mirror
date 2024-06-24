// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::PAGE_OFFSET_WIDTH;
use crate::{
    bits::ones,
    machine_state::{bus::Address, csregisters::CSRRepr, mode::Mode},
};

const OFFSET_MASK: u64 = ones(PAGE_OFFSET_WIDTH as u64);
const PAGE_MASK: u64 = !OFFSET_MASK;

// possible improvement idea: Cache decisions
// Asana link: https://app.asana.com/0/1206655199123740/1207545117103727/f

/// Addresss translation cache specific for instruction fetch.
/// Other access types may need other inputs.
pub struct InstructionFetchTranslationCache {
    // Inputs determining if page translation is cached.
    satp: CSRRepr,
    mode: Mode,
    virt_page: Address,
    // translation result
    phys_page: Address,
}

impl InstructionFetchTranslationCache {
    /// The newly created state is an invalid state,
    /// meaning there will be no cache hits when the cache is in this state.
    #[allow(clippy::new_without_default)]
    #[inline(always)]
    pub fn new() -> Self {
        // Having virt_page = !0 is enough to generate an invalid translation
        // since a virtual page has the last 12 bits zeroed.
        Self {
            satp: !0,
            mode: Mode::Machine,
            virt_page: !0,
            phys_page: !0,
        }
    }

    /// Determine if for the given parameters the translation cache is hit.
    /// If yes, return the cached translated address as `Some(translated_addr)`,
    /// otherwise, for a miss return [`None`].
    #[inline(always)]
    pub fn try_translate(
        &self,
        current_mode: Mode,
        current_satp: CSRRepr,
        raw_pc: Address,
    ) -> Option<Address> {
        let page = raw_pc & PAGE_MASK;
        if self.virt_page == page && self.satp == current_satp && self.mode == current_mode {
            Some(self.phys_page + (raw_pc & OFFSET_MASK))
        } else {
            None
        }
    }

    /// Update the cache with the given parameters and translated page.
    #[inline(always)]
    pub fn update_cache(
        &mut self,
        current_mode: Mode,
        current_satp: CSRRepr,
        raw_pc: Address,
        translation: Address,
    ) {
        self.mode = current_mode;
        self.satp = current_satp;
        self.virt_page = raw_pc & PAGE_MASK;
        self.phys_page = translation & PAGE_MASK;
    }

    /// Invalidate cache
    #[inline(always)]
    pub fn invalidate(&mut self) {
        // this will invalidate any possible cache since for translating an address
        // the virtual page is a multiple of PAGE_SIZE, thus having last 12 bits zeroed.
        self.virt_page = !0;
    }
}
