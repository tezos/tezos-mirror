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
    bits::Bits64,
    create_field,
    machine_state::csregisters::{satp::SvLength, CSRRepr},
};
use twiddle::Twiddle;

/// Structure representing the raw bits of a PPN field.
///
/// E.g. `PPN[0] = raw_bits[8:0]`
pub struct PPNField {
    raw_bits: u64,
}

impl PPNField {
    /// Obtain `PPN[index]` from a PPN field of a page table entry.
    pub fn get_ppn_i(&self, sv_length: &SvLength, index: usize) -> Option<CSRRepr> {
        let bit_range = physical_address::get_raw_ppn_i_range(sv_length, index)?;
        Some(self.raw_bits.bits(bit_range))
    }
}

impl Bits64 for PPNField {
    const WIDTH: usize = 44;

    fn from_bits(value: u64) -> Self {
        PPNField { raw_bits: value }
    }

    fn to_bits(&self) -> u64 {
        self.raw_bits
    }
}

create_field!(FLAG_V, bool, 0, 1);
create_field!(FLAG_R, bool, 1, 1);
create_field!(FLAG_W, bool, 2, 1);
create_field!(FLAG_X, bool, 3, 1);
create_field!(FLAG_U, bool, 4, 1);
create_field!(FLAG_G, bool, 5, 1);
create_field!(FLAG_A, bool, 6, 1);
create_field!(FLAG_D, bool, 7, 1);
// 8 - 9 bits RSW field is reserved for now by the spec
create_field!(PPN, PPNField, 10, 44);
// 54 - 60 bits are reserved
// 61 - 62 bits PBMT field, since Svpbmt not implemented, it is reserved
// bit 63, N bit usually used by Svnapot extension, currently not implemented

#[cfg(test)]
mod tests {
    use proptest::{prop_assert_eq, proptest};

    use crate::machine_state::{address_translation::pte, csregisters::satp::SvLength};

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

            // Sv39
            let ppn = pte::get_PPN(pte);
            prop_assert_eq!(ppn.get_ppn_i(&SvLength::Sv39, over_3), None);
            prop_assert_eq!(ppn.get_ppn_i(&SvLength::Sv39, 0), Some(ppn_0));
            prop_assert_eq!(ppn.get_ppn_i(&SvLength::Sv39, 1), Some(ppn_1));
            prop_assert_eq!(ppn.get_ppn_i(&SvLength::Sv39, 2), Some(ppn_2 | ppn_3 << 9 | ppn_4 << 18));

            // Sv48
            prop_assert_eq!(ppn.get_ppn_i(&SvLength::Sv48, over_3 + 1), None);
            prop_assert_eq!(ppn.get_ppn_i(&SvLength::Sv48, 0), Some(ppn_0));
            prop_assert_eq!(ppn.get_ppn_i(&SvLength::Sv48, 1), Some(ppn_1));
            prop_assert_eq!(ppn.get_ppn_i(&SvLength::Sv48, 2), Some(ppn_2));
            prop_assert_eq!(ppn.get_ppn_i(&SvLength::Sv48, 3), Some(ppn_3 | ppn_4 << 9));

            // Sv57
            prop_assert_eq!(ppn.get_ppn_i(&SvLength::Sv57, over_3 + 2), None);
            prop_assert_eq!(ppn.get_ppn_i(&SvLength::Sv57, 0), Some(ppn_0));
            prop_assert_eq!(ppn.get_ppn_i(&SvLength::Sv57, 1), Some(ppn_1));
            prop_assert_eq!(ppn.get_ppn_i(&SvLength::Sv57, 2), Some(ppn_2));
            prop_assert_eq!(ppn.get_ppn_i(&SvLength::Sv57, 3), Some(ppn_3));
            prop_assert_eq!(ppn.get_ppn_i(&SvLength::Sv57, 4), Some(ppn_4));

            // Flags
            prop_assert_eq!(pte::get_FLAG_R(pte), flag_R != 0);
            prop_assert_eq!(pte::get_FLAG_X(pte), flag_X != 0);
            prop_assert_eq!(pte::get_FLAG_W(pte), flag_W != 0);
            prop_assert_eq!(pte::get_FLAG_D(pte), flag_D != 0);
            prop_assert_eq!(pte::get_FLAG_U(pte), flag_U != 0);
            prop_assert_eq!(pte::get_FLAG_G(pte), flag_G != 0);
            prop_assert_eq!(pte::get_FLAG_A(pte), flag_A != 0);
            prop_assert_eq!(pte::get_FLAG_V(pte), flag_V != 0);
        })
    }
}
