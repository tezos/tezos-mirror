// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::PAGE_OFFSET_WIDTH;
use crate::{
    bits::ones,
    machine_state::{bus::Address, csregisters::CSRRepr, mode::Mode},
    state_backend::{AllocatedOf, Atom, Cell, CellRead, CellWrite, Manager},
};

const OFFSET_MASK: u64 = ones(PAGE_OFFSET_WIDTH as u64);
const PAGE_MASK: u64 = !OFFSET_MASK;

pub type InstructionFetchTransCacheLayout = (Atom<CSRRepr>, Atom<u8>, Atom<Address>, Atom<Address>);

// possible improvement idea: Cache decisions
// Asana link: https://app.asana.com/0/1206655199123740/1207545117103727/f

/// Addresss translation cache specific for instruction fetch.
/// Other access types may need other inputs.
pub struct InstructionFetchTranslationCache<M: Manager> {
    // Inputs determining if page translation is cached.
    satp: Cell<CSRRepr, M>,
    // We use u8 to store the Mode because we never have to read the cell as a Mode. Therefore we
    // can always write Mode as u8.
    mode: Cell<u8, M>,
    virt_page: Cell<Address, M>,
    // translation result
    phys_page: Cell<Address, M>,
}

impl<M: Manager> InstructionFetchTranslationCache<M> {
    /// Bind the cache state to the given space.
    pub fn bind(space: AllocatedOf<InstructionFetchTransCacheLayout, M>) -> Self {
        // Having virt_page = !0 is enough to generate an invalid translation
        // since a virtual page has the last 12 bits zeroed.
        Self {
            satp: space.0,
            mode: space.1,
            virt_page: space.2,
            phys_page: space.3,
        }
    }

    /// Reset the state of the cache.
    pub fn reset(&mut self) {
        // These values guarantee that the cache is empty (i.e. no cache hit is possible initially).
        self.satp.write(!0);
        self.mode.write(!0);
        self.virt_page.write(!0);
        self.phys_page.write(!0);
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
        if self.virt_page.read() == page
            && self.satp.read() == current_satp
            && self.mode.read() == current_mode as u8
        {
            Some(self.phys_page.read() + (raw_pc & OFFSET_MASK))
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
        self.mode.write(current_mode as u8);
        self.satp.write(current_satp);
        self.virt_page.write(raw_pc & PAGE_MASK);
        self.phys_page.write(translation & PAGE_MASK);
    }

    /// Invalidate cache
    #[inline(always)]
    pub fn invalidate(&mut self) {
        // this will invalidate any possible cache since for translating an address
        // the virtual page is a multiple of PAGE_SIZE, thus having last 12 bits zeroed.
        self.virt_page.write(!0);
    }
}
