// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{Cell, EnrichedValue, ManagerBase};
use crate::state_backend::owned_backend::Owned;
use std::collections::BTreeMap;

/// Proof verification backend
pub struct Verifier;

impl ManagerBase for Verifier {
    type Region<E: 'static, const LEN: usize> = Region<E, LEN>;

    type DynRegion<const LEN: usize> = DynRegion<LEN>;

    type EnrichedCell<V: EnrichedValue> = EnrichedCell<V>;

    type ManagerRoot = Self;
}

/// Verifier region
pub enum Region<E: 'static, const LEN: usize> {
    Absent,
    Present([E; LEN]),
}

/// Verifier dynamic region
pub struct DynRegion<const LEN: usize> {
    _pages: BTreeMap<usize, Box<[u8]>>,
}

impl<const LEN: usize> DynRegion<LEN> {
    /// Construct a verifier dynamic region using the given known pages.
    pub fn from_pages(pages: BTreeMap<usize, Box<[u8]>>) -> Self {
        DynRegion { _pages: pages }
    }
}

/// Verifier enriched cell
pub enum EnrichedCell<V: EnrichedValue> {
    Absent,
    Present(V::E),
}

impl<E> Cell<E, Verifier> {
    /// Construct an absent verifier cell.
    pub const fn absent() -> Self {
        Cell::bind(Region::Absent)
    }

    /// Construct a verifier cell with a value.
    pub fn from_owned(cell: Cell<E, Owned>) -> Self {
        let values = cell.into_region();
        let region = Region::Present(values);
        Cell::bind(region)
    }
}
