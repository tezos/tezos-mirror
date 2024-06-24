// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

// Allow unused setters & getters & constants
#![allow(dead_code)]
// Allow non snake case for setters & getters & constants
#![allow(non_snake_case)]

//! Implementation of `Sv39`/`Sv48`/`Sv57` virtual addresses.
//!
//! See sections 5.4, 5.5, 5.6

use super::PAGE_OFFSET_WIDTH;
use crate::{
    bits::{ones, u64},
    machine_state::{
        bus::Address,
        csregisters::{satp::SvLength, CSRRepr},
    },
};

/// Obtain `VPN[index]` from a VPN field specified by `sv_length` Standard.
///
/// The VPN ranges are indexed from 0. (Ignores the page offset field)
/// e.g. `raw_range_VPN[0] = 8..=0` ([`Twiddle`] expects reversed bit ranges)
fn get_raw_vpn_i_range(sv_length: &SvLength, index: usize) -> Option<(usize, usize)> {
    use SvLength::*;
    let bit_range = match (index, sv_length) {
        (0, Sv39 | Sv48 | Sv57) => (8, 0),
        (1, Sv39 | Sv48 | Sv57) => (17, 9),
        (2, Sv39 | Sv48 | Sv57) => (26, 18),
        (3, Sv48 | Sv57) => (35, 27),
        (4, Sv57) => (44, 36),
        _ => return None,
    };
    Some(bit_range)
}

// Get the page offset of a virtual address.
pub const fn get_page_offset(addr: u64) -> u64 {
    addr & ones(PAGE_OFFSET_WIDTH as u64)
}

/// Obtain VPN[index] from a virtual address specified by `sv_length` Standard.
pub fn get_vpn_idx(v_addr: Address, sv_length: &SvLength, index: usize) -> Option<CSRRepr> {
    let bit_range = get_raw_vpn_i_range(sv_length, index)?;
    Some(u64::bits_subset(
        v_addr,
        bit_range.0 + PAGE_OFFSET_WIDTH,
        bit_range.1 + PAGE_OFFSET_WIDTH,
    ))
}

#[cfg(test)]
mod tests {
    use crate::machine_state::{address_translation::virtual_address, csregisters::satp::SvLength};
    use proptest::proptest;

    #[test]
    pub fn test_virtual_address() {
        proptest!(|(
            over_3 in 3..usize::MAX-10,
            vpn_0 in 0_u64..(1 << 9),
            vpn_1 in 0_u64..(1 << 9),
            vpn_2 in 0_u64..(1 << 9),
            vpn_3 in 0_u64..(1 << 9),
            vpn_4 in 0_u64..(1 << 8),
            offset in 0_u64..(1 << 12),
        )| {
            let ppn_parts = vpn_0 | vpn_1 << 9 | vpn_2 << 18 | vpn_3 << 27 | vpn_4 << 36;
            let vaddr = offset | ppn_parts << 12;

            let run_tests = |sv_length, args| {
                for (idx, res) in args {
                    assert_eq!(virtual_address::get_vpn_idx(vaddr, sv_length, idx), res);
                }
            };

            run_tests(&SvLength::Sv39, [
                (over_3, None),
                (0, Some(vpn_0)),
                (1, Some(vpn_1)),
                (2, Some(vpn_2)),
            ].to_vec());

            // Sv48
            run_tests(&SvLength::Sv48, [
                (over_3 + 1, None),
                (0, Some(vpn_0)),
                (1, Some(vpn_1)),
                (2, Some(vpn_2)),
                (3, Some(vpn_3)),
            ].to_vec());

            // Sv57
            run_tests(&SvLength::Sv57, [
                (over_3 + 2, None),
                (0, Some(vpn_0)),
                (1, Some(vpn_1)),
                (2, Some(vpn_2)),
                (3, Some(vpn_3)),
                (4, Some(vpn_4)),
            ].to_vec());
        })
    }
}
