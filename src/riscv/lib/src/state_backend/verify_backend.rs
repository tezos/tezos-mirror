// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{hash::Hash, proof_backend::tree::Tree, Cell, EnrichedValue, ManagerBase};
use crate::state_backend::owned_backend::Owned;

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
    Blinded(Hash),
    Present([E; LEN]),
}

/// Termination point (not necessarily a leaf) in the dynamic region tree
pub enum DynRegionTerm {
    /// Sub-tree is blinded
    Blinded(Hash),

    /// Leaf in the memory tree
    Present(Box<[u8]>),
}

/// Verifier dynamic region
pub enum DynRegion<const LEN: usize> {
    Absent,
    Present(Tree<DynRegionTerm>),
}

/// Verifier enriched cell
pub enum EnrichedCell<V: EnrichedValue> {
    Absent,
    Blinded(Hash),
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
