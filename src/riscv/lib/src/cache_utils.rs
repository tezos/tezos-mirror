// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::convert::Infallible;

use crate::{
    parser::{
        instruction::{Instr, InstrCacheable},
        parse,
    },
    state_backend::Elem,
};

/// Integer to keep track of the fence counter
#[derive(
    Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize,
)]
pub struct FenceCounter(pub u32);

impl FenceCounter {
    /// Initial fence counter
    pub const INITIAL: Self = Self(0);

    /// Maximum fence counter value
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

/// Unparsed instruction used for storing cached instructions in the state.
///
/// Compressed instructions are represented as the lower-16 bits of the u32, with upper-16 bits
/// set to zero.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[repr(transparent)]
pub struct Unparsed(pub u32);

impl Elem for Unparsed {
    #[inline(always)]
    fn store(&mut self, source: &Self) {
        self.0.store(&source.0)
    }

    #[inline(always)]
    fn to_stored_in_place(&mut self) {
        self.0.to_stored_in_place()
    }

    #[inline(always)]
    fn from_stored_in_place(&mut self) {
        self.0.from_stored_in_place()
    }

    #[inline(always)]
    fn from_stored(source: &Self) -> Self {
        Self(u32::from_stored(&source.0))
    }
}

impl From<Unparsed> for (InstrCacheable, Unparsed) {
    fn from(unparsed: Unparsed) -> Self {
        let bytes = unparsed.0;
        let upper = bytes as u16;

        let instr = parse(upper, || {
            Result::<u16, Infallible>::Ok((bytes >> 16) as u16)
        })
        .unwrap();

        match instr {
            Instr::Cacheable(i) => (i, unparsed),
            // As written, this code path is unreachable.
            // We can convert it into a static requirement by allowing
            // errors on bind, instead
            //
            // TODO RV-221: on bind, we should error if an instruction's
            //      bytes correspond to an Uncacheable instruction, rather
            //      than returning an 'Unknown' instruction.
            Instr::Uncacheable(_) => (InstrCacheable::Unknown { instr: bytes }, unparsed),
        }
    }
}

impl From<(InstrCacheable, Unparsed)> for Unparsed {
    fn from((_, unparsed): (InstrCacheable, Unparsed)) -> Self {
        unparsed
    }
}
