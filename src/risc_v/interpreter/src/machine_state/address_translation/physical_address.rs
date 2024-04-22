// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of `Sv39`/`Sv48`/`Sv57` physical addresses.
//!
//! See sections 5.4, 5.5, 5.6

// Allow unused setters & getters & constants
#![allow(dead_code)]
// Allow non snake case for setters & getters & constants
#![allow(non_snake_case)]

use super::PAGE_OFFSET_WIDTH;
use crate::{
    create_field,
    machine_state::{bus::Address, csregisters::satp::SvLength},
};
use std::ops::RangeInclusive;
use twiddle::Twiddle;

/// Obtain `PPN[index]` from a PPN field specified by `sv_length` Standard.
///
/// The page offset is ignored when computing the ranges.
/// (i.e., the PPN field starts at bit 0).
///
/// Example: `raw_range_PPN[0] = 8..=0`. This is because [`Twiddle`] expects the range in reverse
pub(super) fn get_raw_ppn_i_range(
    sv_length: &SvLength,
    index: usize,
) -> Option<RangeInclusive<usize>> {
    use SvLength::*;
    let bit_range = match (index, sv_length) {
        (0, Sv39 | Sv48 | Sv57) => 0..=8,
        (1, Sv39 | Sv48 | Sv57) => 9..=17,
        (2, Sv39) => 18..=43,
        (2, Sv48 | Sv57) => 18..=26,
        (3, Sv48) => 27..=43,
        (3, Sv57) => 27..=35,
        (4, Sv57) => 36..=43,
        _ => return None,
    };

    Some(*bit_range.end()..=*bit_range.start())
}

// The PPN_i fields are not created with the macro `create_field!` because it is useful
// to be able to call the i-th physical page number by an argument `i`.
create_field!(PAGE_OFFSET, u64, 0, PAGE_OFFSET_WIDTH as u64);

/// Get `p_addr.PPN[index]` from a physical address specified by `sv_length` Standard.
pub fn get_PPN_IDX(p_addr: Address, sv_length: &SvLength, index: usize) -> Option<Address> {
    let bit_range = get_raw_ppn_i_range(sv_length, index)?;
    let (start, end) = (
        bit_range.start() + PAGE_OFFSET_WIDTH,
        bit_range.end() + PAGE_OFFSET_WIDTH,
    );

    Some(p_addr.bits(start..=end))
}

/// Set `p_addr.PPN[index] = ppn_value` specified by `sv_length` Standard.
pub fn set_PPN_IDX(
    p_addr: Address,
    sv_length: &SvLength,
    index: usize,
    ppn_value: Address,
) -> Option<Address> {
    let bit_range = get_raw_ppn_i_range(sv_length, index)?;
    let (start, end) = (
        bit_range.start() + PAGE_OFFSET_WIDTH,
        bit_range.end() + PAGE_OFFSET_WIDTH,
    );

    Some(p_addr.replace(start..=end, ppn_value))
}

#[cfg(test)]
mod tests {
    use crate::machine_state::csregisters::{satp::SvLength, CSRRepr};
    use proptest::proptest;

    #[test]
    pub fn test_physical_address() {
        proptest!(|(
            over_3 in 3..usize::MAX-10,
            ppn_0 in 0_u64..(1 << 9),
            ppn_1 in 0_u64..(1 << 9),
            ppn_2 in 0_u64..(1 << 9),
            ppn_3 in 0_u64..(1 << 9),
            ppn_4 in 0_u64..(1 << 8),
            offset in 0_u64..(1 << 12),
        )| {
            use crate::machine_state::address_translation::physical_address as pa;

            let run_tests = |sv_length, ppn_vals: Vec<CSRRepr>, args| {
                let mut addr = pa::set_PAGE_OFFSET(0u64, offset);
                for (idx, &ppn_idx) in ppn_vals.iter().enumerate() {
                    addr = pa::set_PPN_IDX(addr, sv_length, idx, ppn_idx).unwrap();
                }
                for (idx, res) in args {
                    assert_eq!(pa::get_PPN_IDX(addr, sv_length, idx), res);
                }
                assert_eq!(pa::get_PAGE_OFFSET(addr), offset);
            };

            run_tests(&SvLength::Sv39,
                [ppn_0, ppn_1, ppn_2 | ppn_4 << 18].to_vec(),
            [
                (over_3, None),
                (0, Some(ppn_0)),
                (1, Some(ppn_1)),
                (2, Some(ppn_2 | ppn_4 << 18)),
            ].to_vec(),
            );

            run_tests(&SvLength::Sv48,
                [ppn_0, ppn_1, ppn_2, ppn_3 | ppn_4 << 9].to_vec(),
            [
                (over_3 + 1, None),
                (0, Some(ppn_0)),
                (1, Some(ppn_1)),
                (2, Some(ppn_2)),
                (3, Some(ppn_3 | ppn_4 << 9)),
            ].to_vec());

            run_tests(&SvLength::Sv57, [
                ppn_0, ppn_1, ppn_2, ppn_3, ppn_4].to_vec(),
            [
                (over_3 + 2, None),
                (0, Some(ppn_0)),
                (1, Some(ppn_1)),
                (2, Some(ppn_2)),
                (3, Some(ppn_3)),
                (4, Some(ppn_4)),
            ].to_vec());

        })
    }
}
