// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{EnrichedValue, ManagerBase};

/// Transformation from a manager `I` to another manager
pub trait FnManager<I: ManagerBase> {
    /// Resulting manager type
    type Output: ManagerBase;

    /// Transform the region of manager `I` to one of manager `O`.
    fn map_region<E: 'static, const LEN: usize>(
        input: I::Region<E, LEN>,
    ) -> <Self::Output as ManagerBase>::Region<E, LEN>;

    /// Transform the dynamic region of manager `I` to one of manager `O`.
    fn map_dyn_region<const LEN: usize>(
        input: I::DynRegion<LEN>,
    ) -> <Self::Output as ManagerBase>::DynRegion<LEN>;

    /// Transform the enriched cell of manager `I` to one of manager `O`.
    fn map_enriched_cell<V: EnrichedValue>(
        input: I::EnrichedCell<V>,
    ) -> <Self::Output as ManagerBase>::EnrichedCell<V>;
}

/// Identity transformation for [`FnManager`]
pub enum FnManagerIdent {}

impl<M: ManagerBase> FnManager<M> for FnManagerIdent {
    type Output = M;

    fn map_region<E: 'static, const LEN: usize>(input: M::Region<E, LEN>) -> M::Region<E, LEN> {
        input
    }

    fn map_dyn_region<const LEN: usize>(input: M::DynRegion<LEN>) -> M::DynRegion<LEN> {
        input
    }

    fn map_enriched_cell<V: EnrichedValue>(
        input: <M as ManagerBase>::EnrichedCell<V>,
    ) -> <M as ManagerBase>::EnrichedCell<V> {
        input
    }
}
