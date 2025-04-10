// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

// Allow unused setters & getters & constants
#![allow(dead_code)]
// Allow non snake case for setters & getters & constants
#![allow(non_snake_case)]

use super::CSRRepr;
use crate::bits::Bits64;
use crate::bits::FixedWidthBits;
use crate::csr;

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
#[derive(PartialEq, Debug, Clone, Copy)]
#[repr(usize)]
pub enum SvLength {
    Sv39 = 0,
    Sv48 = 1,
    Sv57 = 2,
}

/// `MODE` field of the `satp` register. See table 5.4
#[derive(PartialEq, Debug, Copy, Clone)]
#[repr(u8)]
pub enum TranslationAlgorithm {
    Bare = 0,
    Sv39 = 8,
    Sv48 = 9,
    Sv57 = 10,
}

impl TranslationAlgorithm {
    pub const fn enc(&self) -> CSRRepr {
        *self as u8 as u64
    }
}

/// `Err` represents that the value of SATP.mode is reserved or
/// that we do not care about it / is irrelevant.
impl Bits64 for TranslationAlgorithm {
    const WIDTH: usize = 4;

    fn from_bits(value: u64) -> Self {
        use TranslationAlgorithm::*;

        match value & 0b1111 {
            MODE_BARE => Bare,
            MODE_SV39 => Sv39,
            MODE_SV48 => Sv48,
            MODE_SV57 => Sv57,
            // The satp.mode is a WARL field.
            // This allows us to treat any illegal value as
            // a legal one of our choice or to throw an exception.
            // We are choosing to treat illegal values as `Bare` mode.
            // Note: Reading an illegal value is only be possible if the raw memory is
            // tampered with as the `reset` method and any write also leaves a legal value.
            _ => Bare,
        }
    }

    fn to_bits(&self) -> u64 {
        self.enc()
    }
}

csr! {
    pub struct Satp {
        PPN: FixedWidthBits<44>,
        ASID: FixedWidthBits<16>,
        MODE: TranslationAlgorithm,
    }
}

impl Default for Satp {
    fn default() -> Self {
        Self::from_bits(DEFAULT_VALUE)
    }
}

impl Satp {
    /// Normalise the SATP value.
    pub fn normalise(self) -> Self {
        use TranslationAlgorithm::*;
        match self.mode() {
            Bare => {
                // When no address translation algo is selected, the other fields
                // have no meaning and shall therefore be reset.
                self.with_ppn(FixedWidthBits::from_bits(0))
                    .with_asid(FixedWidthBits::from_bits(0))
            }
            Sv39 | Sv48 | Sv57 => self,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::bits::Bits64;
    use crate::machine_state::csregisters::satp::Satp;
    use crate::machine_state::csregisters::satp::TranslationAlgorithm;

    #[test]
    fn test_satp_fields() {
        let field = u64::from_bits(0xF0F0_0BC0_AAAA_DEAD);
        assert_eq!(field.to_bits(), 0xF0F0_0BC0_AAAA_DEAD);

        type AlgoField = TranslationAlgorithm;

        let field = <AlgoField>::from_bits(0x0000);
        assert_eq!(field, TranslationAlgorithm::Bare);

        // This `FieldValue` looks at only at the 4 least significant bits
        let field = <AlgoField>::from_bits(0xFFFF_FFF0);
        assert_eq!(field, TranslationAlgorithm::Bare);

        let field = <AlgoField>::from_bits(0x0002);
        assert_eq!(field, TranslationAlgorithm::Bare);

        let field = <AlgoField>::from_bits(0x0008);
        assert_eq!(field, TranslationAlgorithm::Sv39);

        let field = <AlgoField>::from_bits(0x0009);
        assert_eq!(field, TranslationAlgorithm::Sv48);

        let field = <AlgoField>::from_bits(0x000A);
        assert_eq!(field, TranslationAlgorithm::Sv57);

        let field = <AlgoField>::from_bits(0x000B);
        assert_eq!(field, TranslationAlgorithm::Bare);
    }

    #[test]
    fn test_satp_rw() {
        let satp = Satp::from_bits((8u64 << 60) | (0xD07 << 44) | 0xABC_DEAD_0BAD);
        let mode = satp.mode();
        let asid = satp.asid();
        let ppn = satp.ppn();
        assert_eq!(mode, TranslationAlgorithm::Sv39);
        assert_eq!(asid.to_bits(), 0xD07);
        assert_eq!(ppn.to_bits(), 0xABC_DEAD_0BAD);

        let satp = satp.with_mode(TranslationAlgorithm::Bare);
        let mode = satp.mode();
        let asid = satp.asid();
        let ppn = satp.ppn();
        assert_eq!(mode, TranslationAlgorithm::Bare);
        assert_eq!(asid.to_bits(), 0xD07);
        assert_eq!(ppn.to_bits(), 0xABC_DEAD_0BAD);
    }
}
