// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::ops::{Bound, RangeBounds};

/// Get the smallest entry in range.
#[inline(always)]
pub fn range_min(range: &impl RangeBounds<usize>) -> usize {
    match range.start_bound() {
        Bound::Included(&x) => x,
        Bound::Excluded(&x) => x.saturating_add(1),
        Bound::Unbounded => 0,
    }
}

/// Get the largest entry in range.
#[inline(always)]
pub fn range_max(range: &impl RangeBounds<usize>) -> usize {
    match range.end_bound() {
        Bound::Included(&x) => x,
        Bound::Excluded(&x) => x.saturating_sub(1),
        Bound::Unbounded => usize::MAX,
    }
}

/// Reduce lower and upper bounds by the given shift amount. Both bounds will be
/// clamped to 0 and won't wrap.
#[inline(always)]
pub fn range_bounds_saturating_sub(
    range: &impl RangeBounds<usize>,
    shift: usize,
) -> impl RangeBounds<usize> {
    let min = range_min(range);
    let max = range_max(range);
    min.saturating_sub(shift)..=max.saturating_sub(shift)
}

/// Does the range cover no numbers larger than 0?
#[inline(always)]
pub fn is_null_range(range: &impl RangeBounds<usize>) -> bool {
    range_max(range) < 1
}
