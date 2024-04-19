// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

// Allow unused setters & getters & constants
#![allow(dead_code)]
// Allow non snake case for setters & getters & constants
#![allow(non_snake_case)]

use super::CSRRepr;
use crate::{bits::Bits64, create_field};

// allowed `MODE` for `satp` register.
// Section 4.1.11
/// `satp.MODE = satp[63:60]`
const SATP_MODE_OFFSET: u64 = 60;
const MODE_BARE: CSRRepr = 0;
const MODE_SV39: CSRRepr = 8;
const MODE_SV48: CSRRepr = 9;
const MODE_SV57: CSRRepr = 10;

/// Default value is BARE mode, (all fields of SATP are zero.)
pub const DEFAULT_VALUE: CSRRepr = MODE_BARE << SATP_MODE_OFFSET;

/// Which flavour of the address virtualization is used.
///
/// `SvXY` represents a virtualization where the virtual address is `XY` bits wide.
#[derive(PartialEq, Debug)]
pub enum SvLength {
    Sv39,
    Sv48,
    Sv57,
}

/// `MODE` field of the `satp` register. See table 5.4
#[derive(PartialEq, Debug)]
pub enum TranslationAlgorithm {
    Bare,
    Sv(SvLength),
}

impl TranslationAlgorithm {
    pub const fn enc(&self) -> CSRRepr {
        match self {
            Self::Bare => MODE_BARE,
            Self::Sv(SvLength::Sv39) => MODE_SV39,
            Self::Sv(SvLength::Sv48) => MODE_SV48,
            Self::Sv(SvLength::Sv57) => MODE_SV57,
        }
    }
}

/// `None` represents that the value of SATP.mode is reserved or
/// that we do not care about it / is irrelevant.
impl Bits64 for Option<TranslationAlgorithm> {
    const WIDTH: usize = 4;

    fn from_bits(value: u64) -> Self {
        use SvLength::*;
        use TranslationAlgorithm::*;

        match value & 0b1111 {
            MODE_BARE => Some(Bare),
            MODE_SV39 => Some(Sv(Sv39)),
            MODE_SV48 => Some(Sv(Sv48)),
            MODE_SV57 => Some(Sv(Sv57)),
            1..=7 | 11..=15 => None,
            16.. => unreachable!(),
        }
    }

    fn to_bits(&self) -> u64 {
        match self {
            None => 0,
            Some(algorithm) => algorithm.enc(),
        }
    }
}

create_field!(MODE, Option<TranslationAlgorithm>, SATP_MODE_OFFSET, 4);
create_field!(ASID, u64, 44, 16);
create_field!(PPN, u64, 0, 44);

#[cfg(test)]
mod tests {
    use crate::{
        bits::Bits64,
        machine_state::csregisters::satp::{
            get_ASID, get_MODE, get_PPN, set_MODE, SvLength, TranslationAlgorithm,
        },
    };

    #[test]
    fn test_satp_fields() {
        let field = u64::from_bits(0xF0F0_0BC0_AAAA_DEAD);
        assert_eq!(field.to_bits(), 0xF0F0_0BC0_AAAA_DEAD);

        let field = <Option<TranslationAlgorithm>>::from_bits(0x0000);
        assert_eq!(field, Some(TranslationAlgorithm::Bare));

        // This `FieldValue` looks at only at the 4 least significant bits
        let field = <Option<TranslationAlgorithm>>::from_bits(0xFFFF_FFF0);
        assert_eq!(field, Some(TranslationAlgorithm::Bare));

        let field = <Option<TranslationAlgorithm>>::from_bits(0x0002);
        assert_eq!(field, None);

        let field = <Option<TranslationAlgorithm>>::from_bits(0x0008);
        assert_eq!(field, Some(TranslationAlgorithm::Sv(SvLength::Sv39)));

        let field = <Option<TranslationAlgorithm>>::from_bits(0x0009);
        assert_eq!(field, Some(TranslationAlgorithm::Sv(SvLength::Sv48)));

        let field = <Option<TranslationAlgorithm>>::from_bits(0x000A);
        assert_eq!(field, Some(TranslationAlgorithm::Sv(SvLength::Sv57)));

        let field = <Option<TranslationAlgorithm>>::from_bits(0x000B);
        assert_eq!(field, None);
    }

    #[test]
    fn test_satp_rw() {
        let satp = 8u64 << 60 | 0xD07 << 44 | 0xABC_DEAD_0BAD;
        let mode = get_MODE(satp);
        let asid = get_ASID(satp);
        let ppn = get_PPN(satp);
        assert_eq!(mode, Some(TranslationAlgorithm::Sv(SvLength::Sv39)));
        assert_eq!(asid.to_bits(), 0xD07);
        assert_eq!(ppn.to_bits(), 0xABC_DEAD_0BAD);

        let satp = set_MODE(satp, Some(TranslationAlgorithm::Bare));
        let mode = get_MODE(satp);
        let asid = get_ASID(satp);
        let ppn = get_PPN(satp);
        assert_eq!(mode, Some(TranslationAlgorithm::Bare));
        assert_eq!(asid.to_bits(), 0xD07);
        assert_eq!(ppn.to_bits(), 0xABC_DEAD_0BAD);
    }
}
