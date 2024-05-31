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
) -> (Bound<usize>, Bound<usize>) {
    let start = range.start_bound();
    let end = range.end_bound();

    // Construct new start and end bounds
    let new_start = match start {
        Bound::Included(&s) => Bound::Included(s.saturating_sub(shift)),
        Bound::Excluded(&s) => Bound::Excluded(s.saturating_sub(shift)),
        Bound::Unbounded => Bound::Unbounded,
    };
    let new_end = match end {
        Bound::Included(&e) => Bound::Included(e.saturating_sub(shift)),
        Bound::Excluded(&e) => Bound::Excluded(e.saturating_sub(shift)),
        Bound::Unbounded => Bound::Unbounded,
    };
    // return the new range with new_start and new_end, which impl RangeBounds<usize>
    (new_start, new_end)
}

/// Does the range cover no numbers larger than 0?
#[inline(always)]
pub fn is_null_range(range: &impl RangeBounds<usize>) -> bool {
    range_max(range) < 1
}

#[cfg(test)]
mod test {
    use super::*;
    use std::ops::{Bound, RangeInclusive};

    fn range_shift_eq(
        range: &impl RangeBounds<usize>,
        shift: usize,
        expected: (Bound<usize>, Bound<usize>),
    ) {
        let res = range_bounds_saturating_sub(range, shift);
        assert_eq!(expected, res);
    }

    #[test]
    fn test_range_bounds_saturating_sub_full_0() {
        range_shift_eq(&(..), 0, (Bound::Unbounded, Bound::Unbounded));
    }

    #[test]
    fn test_range_bounds_saturating_sub_full_1() {
        range_shift_eq(&(..), 5, (Bound::Unbounded, Bound::Unbounded));
    }

    #[test]
    fn test_range_bounds_saturating_sub_full_2() {
        range_shift_eq(
            &(Bound::<usize>::Unbounded, Bound::<usize>::Unbounded),
            0,
            (Bound::<usize>::Unbounded, Bound::<usize>::Unbounded),
        );
    }

    #[test]
    fn test_range_bounds_saturating_sub_full_3() {
        range_shift_eq(
            &(Bound::<usize>::Unbounded, Bound::<usize>::Unbounded),
            usize::MAX,
            (Bound::<usize>::Unbounded, Bound::<usize>::Unbounded),
        );
    }

    #[test]
    fn test_range_bounds_saturating_sub_inclusive_00() {
        range_shift_eq(&(0..=10), 0, (Bound::Included(0), Bound::Included(10)));
    }

    #[test]
    fn test_range_bounds_saturating_sub_inclusive_01() {
        range_shift_eq(
            &RangeInclusive::new(0, 10),
            0,
            (Bound::Included(0), Bound::Included(10)),
        );
    }

    #[test]
    fn test_range_bounds_saturating_sub_inclusive_02() {
        range_shift_eq(
            &RangeInclusive::new(5, 10),
            2,
            (Bound::Included(3), Bound::Included(8)),
        );
    }

    #[test]
    fn test_range_bounds_saturating_sub_inclusive_03() {
        range_shift_eq(
            &RangeInclusive::new(3, 10),
            10,
            (Bound::Included(0), Bound::Included(0)),
        );
    }

    #[test]
    fn test_range_bounds_saturating_sub_inclusive_04() {
        range_shift_eq(
            &RangeInclusive::new(0, 10),
            15,
            (Bound::Included(0), Bound::Included(0)),
        );
    }

    #[test]
    fn test_range_bounds_saturating_sub_inclusive_05() {
        range_shift_eq(
            &RangeInclusive::new(0, 10),
            usize::MAX,
            (Bound::Included(0), Bound::Included(0)),
        );
    }

    #[test]
    fn test_range_bounds_saturating_sub_inclusive_06() {
        range_shift_eq(
            &RangeInclusive::new(usize::MAX, usize::MAX),
            0,
            (Bound::Included(usize::MAX), Bound::Included(usize::MAX)),
        );
    }

    #[test]
    fn test_range_bounds_saturating_sub_inclusive_07() {
        range_shift_eq(
            &RangeInclusive::new(usize::MAX, usize::MAX),
            1,
            (
                Bound::Included(usize::MAX - 1),
                Bound::Included(usize::MAX - 1),
            ),
        );
    }

    #[test]
    fn test_range_bounds_saturating_sub_inclusive_08() {
        range_shift_eq(
            &RangeInclusive::new(usize::MAX, usize::MAX - 1),
            3,
            (
                Bound::Included(usize::MAX - 3),
                Bound::Included(usize::MAX - 4),
            ),
        );
    }

    #[test]
    fn test_range_bounds_saturating_sub_inclusive_09() {
        range_shift_eq(
            &(Bound::Included(0), Bound::Included(10)),
            0,
            (Bound::Included(0), Bound::Included(10)),
        );
    }

    #[test]
    fn test_range_bounds_saturating_sub_inclusive_10() {
        range_shift_eq(
            &(Bound::Included(5), Bound::Included(10)),
            2,
            (Bound::Included(3), Bound::Included(8)),
        );
    }

    #[test]
    fn test_range_bounds_saturating_sub_inclusive_11() {
        range_shift_eq(
            &(Bound::Included(0), Bound::Included(10)),
            usize::MAX,
            (Bound::Included(0), Bound::Included(0)),
        );
    }

    #[test]
    fn test_range_bounds_saturating_sub_inclusive_12() {
        range_shift_eq(
            &(Bound::Included(usize::MAX), Bound::Included(usize::MAX)),
            1,
            (
                Bound::Included(usize::MAX - 1),
                Bound::Included(usize::MAX - 1),
            ),
        );
    }

    #[test]
    fn test_range_bounds_saturating_sub_exclusive_0() {
        range_shift_eq(
            &(Bound::Excluded(0), Bound::Excluded(10)),
            5,
            (Bound::Excluded(0), Bound::Excluded(5)),
        );
    }

    #[test]
    fn test_range_bounds_saturating_sub_exclusive_1() {
        range_shift_eq(
            &(Bound::Excluded(3), Bound::Excluded(10)),
            10,
            (Bound::Excluded(0), Bound::Excluded(0)),
        );
    }

    #[test]
    fn test_range_bounds_saturating_sub_exclusive_2() {
        range_shift_eq(
            &(Bound::Excluded(7), Bound::Excluded(10)),
            5,
            (Bound::Excluded(2), Bound::Excluded(5)),
        );
    }

    #[test]
    fn test_range_bounds_saturating_sub_mixed_0() {
        range_shift_eq(
            &(Bound::Included(0), Bound::Excluded(10)),
            0,
            (Bound::Included(0), Bound::Excluded(10)),
        );
    }

    #[test]
    fn test_range_bounds_saturating_sub_mixed_1() {
        range_shift_eq(
            &(Bound::Included(0), Bound::Excluded(10)),
            5,
            (Bound::Included(0), Bound::Excluded(5)),
        );
    }
}
