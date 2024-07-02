// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::HartState;
use crate::{
    machine_state::{csregisters::CSRRepr, mode::Mode},
    state_backend::{self as backend},
};

/// Implements the Cache behaviour for HartState
impl<M: backend::Manager> HartState<M> {
    /// Update the cache with the current mode and possible interrupts
    /// and set the cache as valid.
    pub(super) fn update_possible_interrupts_cache(
        &mut self,
        current_mode: Mode,
        possible_interrupts: CSRRepr,
    ) {
        self.csregisters
            .interrupt_cache
            .update(current_mode, possible_interrupts);
    }

    /// Return [`Some()`] if for the given parameters the cache is hit.
    pub(super) fn get_possible_interrupts_cache(&self, mode: Mode) -> Option<CSRRepr> {
        self.csregisters.interrupt_cache.get(mode)
    }
}

/// Cache of the possible interrupts, which depends on the mode.
/// Cache is valid iff it contains Some(Cache)
#[derive(Debug, Clone, Copy)]
pub struct PossibleInterruptsCache(Option<Cache>);

impl PossibleInterruptsCache {
    const EMPTY: Self = Self(None);

    fn new(mode: Mode, possible_interrupts: CSRRepr) -> Self {
        Self(Some(Cache::new(mode, possible_interrupts)))
    }

    pub(super) fn get(&self, mode: Mode) -> Option<CSRRepr> {
        self.0.and_then(|cache| cache.get_possible_interrupts(mode))
    }

    pub(super) fn update(&mut self, mode: Mode, possible_interrupts: CSRRepr) {
        *self = Self::new(mode, possible_interrupts);
    }

    #[inline(always)]
    pub fn invalidate(&mut self) {
        *self = Self::EMPTY;
    }
}

impl Default for PossibleInterruptsCache {
    fn default() -> Self {
        Self::EMPTY
    }
}

#[derive(Copy, Clone, Debug)]
struct Cache {
    mode: Mode,
    possible_interrupts: CSRRepr,
}

impl Cache {
    fn new(mode: Mode, possible_interrupts: CSRRepr) -> Self {
        Self {
            mode,
            possible_interrupts,
        }
    }

    fn get_possible_interrupts(self, mode: Mode) -> Option<CSRRepr> {
        if self.mode == mode {
            Some(self.possible_interrupts)
        } else {
            None
        }
    }
}
