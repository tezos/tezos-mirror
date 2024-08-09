// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::HartState;
use crate::{
    machine_state::{csregisters::CSRRepr, mode::Mode},
    state_backend::{self as backend},
    traps::Interrupt,
};

/// Implements the Cache behaviour for HartState
impl<M: backend::Manager> HartState<M> {
    /// Update the interrupt cache. Updates are to the entire cache (mode, possible, pending), cannot be partial.
    pub(super) fn update_interrupts_cache(
        &mut self,
        current_mode: Mode,
        possible_interrupts: CSRRepr,
        pending_interrupt: Option<Interrupt>,
    ) {
        self.csregisters.interrupt_cache.update(
            current_mode,
            possible_interrupts,
            pending_interrupt.into(),
        );
    }

    /// For the given mode, get the interrupts from the cache.
    pub(super) fn get_interrupts_cache(&self, mode: Mode) -> InterruptsCacheResult {
        self.csregisters.interrupt_cache.get(mode)
    }
}

/// Result of the InterruptsCache lookup: Miss, PossibleInterrupts (Partial), PendingInterrupt (Full)
pub enum InterruptsCacheResult {
    /// Total miss: neither possible or pending interrupt
    Miss,
    /// If the pending interrupt was invalidated, but possible interrupt mask was not
    /// return the possible interrupts mask as an intermediate cache hit
    PossibleInterrupts(CSRRepr),
    /// Full cache hit: The pending interrupt.
    PendingInterrupt(Option<Interrupt>),
}

/// Cache of the possible interrupts, which depends on the mode.
/// Cache is valid iff it contains Some(Cache), and invalidates on changes to mstatus
#[derive(Debug, Clone, Copy, Default)]
pub struct InterruptsCache(Option<Cache>);

impl InterruptsCache {
    /// For the given mode, get the interrupts from the cache.
    /// Returns (`Miss` | `PossibleInterrupts` (Partial) | `PendingInterrupt` (Full))    
    fn get(&self, mode: Mode) -> InterruptsCacheResult {
        match self.0 {
            Some(cache) => cache.get_interrupts(mode),
            None => InterruptsCacheResult::Miss,
        }
    }

    /// Update the interrupt cache. Updates are to the entire cache, cannot be partial.
    fn update(
        &mut self,
        mode: Mode,
        possible_interrupts: CSRRepr,
        pending_interrupt: PendingInterrupt,
    ) {
        if let Some(cache) = &mut self.0 {
            cache.mode = mode;
            cache.possible_interrupts = possible_interrupts;
            cache.pending_interrupt = Some(pending_interrupt);
        } else {
            self.0 = Some(Cache::new(mode, possible_interrupts, pending_interrupt));
        }
    }

    /// Invalidate the entire cache, making it empty.
    #[inline(always)]
    pub fn invalidate(&mut self) {
        self.0 = None;
    }

    /// Invalidate the pending interrupt, but keep the possible interrupts.
    pub fn invalidate_by_mip(&mut self) {
        if let Some(mut cache) = self.0 {
            cache.clear_pending_interrupts();
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct Cache {
    // If PossibleInterruptsCache is populated, the (mode, possible_interrupts) pair is valid
    mode: Mode,
    /// Cached possible interrupts. returned iff mode matches but pending interrupt is None.
    possible_interrupts: CSRRepr,

    /// Cached pending interrupt.
    // invalidates on changes to mip
    pending_interrupt: Option<PendingInterrupt>,
}

impl Cache {
    fn new(mode: Mode, possible_interrupts: CSRRepr, pending_interrupt: PendingInterrupt) -> Self {
        Self {
            mode,
            possible_interrupts,
            pending_interrupt: Some(pending_interrupt),
        }
    }

    fn clear_pending_interrupts(&mut self) {
        self.pending_interrupt = None;
    }

    fn get_interrupts(self, mode: Mode) -> InterruptsCacheResult {
        if self.mode == mode {
            match self.pending_interrupt {
                Some(result) => InterruptsCacheResult::PendingInterrupt(result.into()),
                None => InterruptsCacheResult::PossibleInterrupts(self.possible_interrupts),
            }
        } else {
            InterruptsCacheResult::Miss
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum PendingInterrupt {
    None,
    Some(Interrupt),
}

impl From<Option<Interrupt>> for PendingInterrupt {
    fn from(option: Option<Interrupt>) -> Self {
        match option {
            None => Self::None,
            Some(interrupt) => Self::Some(interrupt),
        }
    }
}

impl From<PendingInterrupt> for Option<Interrupt> {
    fn from(pending: PendingInterrupt) -> Self {
        match pending {
            PendingInterrupt::None => None,
            PendingInterrupt::Some(interrupt) => Some(interrupt),
        }
    }
}
