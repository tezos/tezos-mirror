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
}

macro_rules! create_effect_getter {
    ($struct_name:ident, $effect:expr) => {
        pub struct $struct_name;

        impl $crate::state_backend::EffectGetter for $struct_name {
            type Effect = CSREffect;

            const EFFECT: Option<Self::Effect> = $effect;
        }
    };
}

create_effect_getter!(NoEffect, None);
create_effect_getter!(MipEffect, Some(CSREffect::InvalidateTranslationCacheXIP));

#[inline(always)]
pub fn handle_csr_effect(state: &mut CSRegisters<impl ManagerBase>, effect: Option<CSREffect>) {
    if let Some(effect) = effect {
        match effect {
            CSREffect::InvalidateTranslationCacheXIE => state.interrupt_cache.invalidate(),
            CSREffect::InvalidateTranslationCacheXIP => state.interrupt_cache.invalidate_by_mip(),
        }
    }
}
