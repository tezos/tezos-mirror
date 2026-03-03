// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

/// Trait for types which support fallible cloning
pub trait TryClone: Sized {
    type Error;

    fn try_clone(&self) -> Result<Self, Self::Error>;
}
