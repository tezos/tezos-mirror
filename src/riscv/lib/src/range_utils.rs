// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::ops::Bound;

/// Shift the bound towards zero by the given amount without wrapping.
/// Unbounded limits are unchanged.
#[inline(always)]
pub fn bound_saturating_sub(bound: Bound<usize>, shift: usize) -> Bound<usize> {
    match bound {
        Bound::Included(x) => Bound::Included(x.saturating_sub(shift)),
        Bound::Excluded(x) => Bound::Excluded(x.saturating_sub(shift)),
        Bound::Unbounded => Bound::Unbounded,
    }
}

/// Is `num` below the bound?
#[inline(always)]
pub fn less_than_bound(num: usize, bound: Bound<usize>) -> bool {
    match bound {
        Bound::Included(x) => num < x,
        Bound::Excluded(x) => num < x.saturating_sub(1),
        Bound::Unbounded => true,
    }
}

/// Turn a bound into a fixed number.
///
/// `Unbounded` is replaced by `usize::MAX`. When using this function additional
/// logic should be supplied to recheck the original bound.
#[inline(always)]
pub fn unwrap_bound(bound: Bound<usize>) -> usize {
    match bound {
        Bound::Included(x) => x,
        Bound::Excluded(x) => x.saturating_sub(1),
        Bound::Unbounded => usize::MAX,
    }
}
