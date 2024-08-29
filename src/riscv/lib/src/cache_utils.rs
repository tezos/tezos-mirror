// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::state_backend::Elem;

/// Integer to keep track of the fence counter
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FenceCounter(pub u32);

impl FenceCounter {
    /// Initial fence counter
    pub const INITIAL: Self = Self(0);

    /// Maximum fence counter value
    #[cfg(test)]
    pub const MAX: Self = Self(u32::MAX);

    /// Increment the fence counter.
    #[inline]
    pub fn next(self) -> Self {
        Self(self.0.wrapping_add(1))
    }
}

impl Elem for FenceCounter {
    #[inline(always)]
    fn store(&mut self, source: &Self) {
        self.0.store(&source.0);
    }

    #[inline(always)]
    fn to_stored_in_place(&mut self) {
        self.0.to_stored_in_place();
    }

    #[inline(always)]
    fn from_stored_in_place(&mut self) {
        self.0.from_stored_in_place();
    }

    #[inline(always)]
    fn from_stored(source: &Self) -> Self {
        Self(u32::from_stored(&source.0))
    }
}
