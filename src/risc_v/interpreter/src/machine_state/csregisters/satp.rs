// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

// Allow unused setters & getters & constants
#![allow(dead_code)]
// Allow non snake case for setters & getters & constants
#![allow(non_snake_case)]

use super::CSRRepr;
use crate::{
    bits::{Bits64, FixedWidthBits},
    csr,
};

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

/// `Err` represents that the value of SATP.mode is reserved or
/// that we do not care about it / is irrelevant.
impl Bits64 for Result<TranslationAlgorithm, u64> {
    const WIDTH: usize = 4;

    fn from_bits(value: u64) -> Self {
        use SvLength::*;
        use TranslationAlgorithm::*;

        match value & 0b1111 {
            MODE_BARE => Ok(Bare),
            MODE_SV39 => Ok(Sv(Sv39)),
            MODE_SV48 => Ok(Sv(Sv48)),
            MODE_SV57 => Ok(Sv(Sv57)),
            value @ (1..=7 | 11..=15) => {
                // We need to retain invalid/unknown values so we can re-encode
                // them correctly later. In other words we want to be loss-less.
                Err(value)
            }
            16.. => unreachable!(),
        }
    }

    fn to_bits(&self) -> u64 {
        match self {
            Err(code) => *code,
            Ok(algorithm) => algorithm.enc(),
        }
    }
}

csr! {
    pub struct Satp {
        PPN: FixedWidthBits<44>,
        ASID: FixedWidthBits<16>,
        MODE: Result<TranslationAlgorithm, u64>,
    }
}

impl Default for Satp {
    fn default() -> Self {
        Self::from_bits(DEFAULT_VALUE)
    }
}

impl Satp {
    /// Normalise the SATP value.
    pub fn normalise(self) -> Option<Self> {
        match self.mode() {
            Err(_) => None,
            Ok(TranslationAlgorithm::Bare) => {
                // When no address translation algo is selected, the other fields
                // have no meaning and shall therefore be reset.
                Some(
                    self.with_ppn(FixedWidthBits::from_bits(0))
                        .with_asid(FixedWidthBits::from_bits(0)),
                )
            }
            Ok(TranslationAlgorithm::Sv(_)) => Some(self),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        bits::Bits64,
        machine_state::csregisters::satp::{Satp, SvLength, TranslationAlgorithm},
    };

    #[test]
    fn test_satp_fields() {
        let field = u64::from_bits(0xF0F0_0BC0_AAAA_DEAD);
        assert_eq!(field.to_bits(), 0xF0F0_0BC0_AAAA_DEAD);

        type AlgoField = Result<TranslationAlgorithm, u64>;

        let field = <AlgoField>::from_bits(0x0000);
        assert_eq!(field, Ok(TranslationAlgorithm::Bare));

        // This `FieldValue` looks at only at the 4 least significant bits
        let field = <AlgoField>::from_bits(0xFFFF_FFF0);
        assert_eq!(field, Ok(TranslationAlgorithm::Bare));

        let field = <AlgoField>::from_bits(0x0002);
        assert_eq!(field, Err(0x2));

        let field = <AlgoField>::from_bits(0x0008);
        assert_eq!(field, Ok(TranslationAlgorithm::Sv(SvLength::Sv39)));

        let field = <AlgoField>::from_bits(0x0009);
        assert_eq!(field, Ok(TranslationAlgorithm::Sv(SvLength::Sv48)));

        let field = <AlgoField>::from_bits(0x000A);
        assert_eq!(field, Ok(TranslationAlgorithm::Sv(SvLength::Sv57)));

        let field = <AlgoField>::from_bits(0x000B);
        assert_eq!(field, Err(0xB));
    }

    #[test]
    fn test_satp_rw() {
        let satp = Satp::from_bits(8u64 << 60 | 0xD07 << 44 | 0xABC_DEAD_0BAD);
        let mode = satp.mode();
        let asid = satp.asid();
        let ppn = satp.ppn();
        assert_eq!(mode, Ok(TranslationAlgorithm::Sv(SvLength::Sv39)));
        assert_eq!(asid.to_bits(), 0xD07);
        assert_eq!(ppn.to_bits(), 0xABC_DEAD_0BAD);

        let satp = satp.with_mode(Ok(TranslationAlgorithm::Bare));
        let mode = satp.mode();
        let asid = satp.asid();
        let ppn = satp.ppn();
        assert_eq!(mode, Ok(TranslationAlgorithm::Bare));
        assert_eq!(asid.to_bits(), 0xD07);
        assert_eq!(ppn.to_bits(), 0xABC_DEAD_0BAD);
    }
}
