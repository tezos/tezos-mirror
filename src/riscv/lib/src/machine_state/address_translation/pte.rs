// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Implementation of `Sv39`/`Sv48`/`Sv57` Page Table Entries (PTE).
//!
//! See sections 5.4, 5.5, 5.6

// Allow unused setters & getters & constants
#![allow(dead_code)]
// Allow non snake case for setters & getters & constants
#![allow(non_snake_case)]

use super::physical_address;
use crate::{
    bits::{ones, u64, Bits64, ConstantBits},
    csr,
    machine_state::csregisters::{satp::SvLength, CSRRepr},
};

/// Structure representing the raw bits of a PPN field.
///
/// E.g. `PPN[0] = raw_bits[8:0]`
#[derive(Copy, Clone, Debug)]
pub struct PPNField {
    raw_bits: u64,
}

impl PPNField {
    /// Obtain `PPN[index]` from a PPN field of a page table entry.
    pub fn ppn_i(&self, sv_length: SvLength, index: usize) -> Option<CSRRepr> {
        let (start, end) = physical_address::get_raw_ppn_i_range(sv_length, index)?;
        Some(u64::bits_subset(self.raw_bits, start, end))
    }
}

impl Bits64 for PPNField {
    const WIDTH: usize = 44;

    fn from_bits(value: u64) -> Self {
        PPNField {
            raw_bits: value & ones(Self::WIDTH as u64),
        }
    }

    fn to_bits(&self) -> u64 {
        self.raw_bits & ones(Self::WIDTH as u64)
    }
}

csr! {
   pub struct PageTableEntry {
       V: bool,
       R: bool,
       W: bool,
       X: bool,
       U: bool,
       G: bool,
       A: bool,
       D: bool,
       RSW: ConstantBits<2>,
       PPN: PPNField,
       WPRI2: ConstantBits<7>,
       PBMT: ConstantBits<2>,
       N: ConstantBits<1>,
   }
}

#[cfg(test)]
mod tests {
    use crate::{
        bits::Bits64,
        machine_state::{address_translation::pte::PageTableEntry, csregisters::satp::SvLength},
    };
    use proptest::{prop_assert_eq, proptest};

    #[test]
    pub fn test_pte_fields() {
        proptest!(|(
            flag_R in 0_u64..2,
            flag_X in 0_u64..2,
            flag_W in 0_u64..2,
            flag_D in 0_u64..2,
            flag_U in 0_u64..2,
            flag_G in 0_u64..2,
            flag_A in 0_u64..2,
            flag_V in 0_u64..2,
            over_3 in 3..usize::MAX-10,
            ppn_0 in 0_u64..(1 << 9),
            ppn_1 in 0_u64..(1 << 9),
            ppn_2 in 0_u64..(1 << 9),
            ppn_3 in 0_u64..(1 << 9),
            ppn_4 in 0_u64..(1 << 8),
        )| {
            let ppn_parts = ppn_0 | ppn_1 << 9 | ppn_2 << 18 | ppn_3 << 27 | ppn_4 << 36;
            let flags = flag_V | flag_R << 1 | flag_W << 2 | flag_X << 3 | flag_U << 4 | flag_G << 5 | flag_A << 6 | flag_D << 7;
            let pte = flags | ppn_parts << 10;
            let pte = PageTableEntry::from_bits(pte);

            // Sv39
            let ppn = pte.ppn();
            prop_assert_eq!(ppn.ppn_i(SvLength::Sv39, over_3), None);
            prop_assert_eq!(ppn.ppn_i(SvLength::Sv39, 0), Some(ppn_0));
            prop_assert_eq!(ppn.ppn_i(SvLength::Sv39, 1), Some(ppn_1));
            prop_assert_eq!(ppn.ppn_i(SvLength::Sv39, 2), Some(ppn_2 | ppn_3 << 9 | ppn_4 << 18));

            // Sv48
            prop_assert_eq!(ppn.ppn_i(SvLength::Sv48, over_3 + 1), None);
            prop_assert_eq!(ppn.ppn_i(SvLength::Sv48, 0), Some(ppn_0));
            prop_assert_eq!(ppn.ppn_i(SvLength::Sv48, 1), Some(ppn_1));
            prop_assert_eq!(ppn.ppn_i(SvLength::Sv48, 2), Some(ppn_2));
            prop_assert_eq!(ppn.ppn_i(SvLength::Sv48, 3), Some(ppn_3 | ppn_4 << 9));

            // Sv57
            prop_assert_eq!(ppn.ppn_i(SvLength::Sv57, over_3 + 2), None);
            prop_assert_eq!(ppn.ppn_i(SvLength::Sv57, 0), Some(ppn_0));
            prop_assert_eq!(ppn.ppn_i(SvLength::Sv57, 1), Some(ppn_1));
            prop_assert_eq!(ppn.ppn_i(SvLength::Sv57, 2), Some(ppn_2));
            prop_assert_eq!(ppn.ppn_i(SvLength::Sv57, 3), Some(ppn_3));
            prop_assert_eq!(ppn.ppn_i(SvLength::Sv57, 4), Some(ppn_4));

            // Flags
            prop_assert_eq!(pte.r(), flag_R != 0);
            prop_assert_eq!(pte.x(), flag_X != 0);
            prop_assert_eq!(pte.w(), flag_W != 0);
            prop_assert_eq!(pte.d(), flag_D != 0);
            prop_assert_eq!(pte.u(), flag_U != 0);
            prop_assert_eq!(pte.g(), flag_G != 0);
            prop_assert_eq!(pte.a(), flag_A != 0);
            prop_assert_eq!(pte.v(), flag_V != 0);
        })
    }
}
