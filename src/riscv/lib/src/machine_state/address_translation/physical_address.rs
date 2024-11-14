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
    bits::{ones, u64},
    machine_state::{bus::Address, csregisters::satp::SvLength},
};

/// Obtain `PPN[index]` from a PPN field specified by `sv_length` Standard.
///
/// The page offset is ignored when computing the ranges.
/// (i.e., the PPN field starts at bit 0).
///
/// Example: `raw_range_PPN[0] = 8..=0`. This is because [`Twiddle`] expects the range in reverse
#[inline(always)]
pub(super) fn get_raw_ppn_i_range(sv_length: SvLength, index: usize) -> Option<(usize, usize)> {
    // We use a jump table instead of a match statement to improve performance.
    const JUMP_TABLE: [[Option<(usize, usize)>; 5]; 3] = [
        // Sv39
        [Some((8, 0)), Some((17, 9)), Some((43, 18)), None, None],
        // Sv48
        [
            Some((8, 0)),
            Some((17, 9)),
            Some((26, 18)),
            Some((43, 27)),
            None,
        ],
        // Sv57
        [
            Some((8, 0)),
            Some((17, 9)),
            Some((26, 18)),
            Some((35, 27)),
            Some((43, 36)),
        ],
    ];

    if index > 4 {
        return None;
    }

    JUMP_TABLE[sv_length as usize][index]
}

// Get the page offset of a physical address.
pub const fn get_page_offset(addr: u64) -> u64 {
    addr & ones(PAGE_OFFSET_WIDTH as u64)
}

// Get the page offset of a physical address.
pub fn set_page_offset(addr: u64, page_offset: u64) -> u64 {
    let mask = ones(PAGE_OFFSET_WIDTH as u64);
    addr & !mask | page_offset & mask
}

/// Get `p_addr.PPN[index]` from a physical address specified by `sv_length` Standard.
pub fn get_ppn_idx(p_addr: Address, sv_length: SvLength, index: usize) -> Option<Address> {
    let bit_range = get_raw_ppn_i_range(sv_length, index)?;
    Some(u64::bits_subset(
        p_addr,
        bit_range.0 + PAGE_OFFSET_WIDTH,
        bit_range.1 + PAGE_OFFSET_WIDTH,
    ))
}

/// Set `p_addr.PPN[index] = ppn_value` specified by `sv_length` Standard.
pub fn set_ppn_idx(
    p_addr: Address,
    sv_length: SvLength,
    index: usize,
    ppn_value: Address,
) -> Option<Address> {
    let bit_range = get_raw_ppn_i_range(sv_length, index)?;
    Some(u64::replace_subset(
        p_addr,
        bit_range.0 + PAGE_OFFSET_WIDTH,
        bit_range.1 + PAGE_OFFSET_WIDTH,
        ppn_value,
    ))
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
                let mut addr = pa::set_page_offset(0u64, offset);
                for (idx, &ppn_idx) in ppn_vals.iter().enumerate() {
                    addr = pa::set_ppn_idx(addr, sv_length, idx, ppn_idx).unwrap();
                }
                for (idx, res) in args {
                    assert_eq!(pa::get_ppn_idx(addr, sv_length, idx), res);
                }
                assert_eq!(pa::get_page_offset(addr), offset);
            };

            run_tests(SvLength::Sv39,
                [ppn_0, ppn_1, ppn_2 | ppn_4 << 18].to_vec(),
            [
                (over_3, None),
                (0, Some(ppn_0)),
                (1, Some(ppn_1)),
                (2, Some(ppn_2 | ppn_4 << 18)),
            ].to_vec(),
            );

            run_tests(SvLength::Sv48,
                [ppn_0, ppn_1, ppn_2, ppn_3 | ppn_4 << 9].to_vec(),
            [
                (over_3 + 1, None),
                (0, Some(ppn_0)),
                (1, Some(ppn_1)),
                (2, Some(ppn_2)),
                (3, Some(ppn_3 | ppn_4 << 9)),
            ].to_vec());

            run_tests(SvLength::Sv57, [
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
