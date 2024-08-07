// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::CSRegisters;
use crate::state_backend::ManagerBase;

/// Type representing CSR side effects. (on read/write/replace)
/// Effects to be handled by function [`handle_csr_effect`].
#[derive(PartialEq, Debug, Copy, Clone)]
pub enum CSREffect {
    InvalidateTranslationCacheXIE,
    InvalidateTranslationCacheXIP,
    None,
}

#[inline(always)]
pub fn handle_csr_effect(state: &mut CSRegisters<impl ManagerBase>, effect: CSREffect) {
    match effect {
        CSREffect::InvalidateTranslationCacheXIE => state.interrupt_cache.invalidate(),
        CSREffect::InvalidateTranslationCacheXIP => state.interrupt_cache.invalidate_by_mip(),
        CSREffect::None => (),
    }
}
