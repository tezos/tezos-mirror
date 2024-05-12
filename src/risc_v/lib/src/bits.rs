// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::fmt;
use twiddle::Twiddle;

/// Implementations can be converted to and from a binary [u64] representation
pub trait Bits64 {
    const WIDTH: usize;

    /// Convert from the [u64] binary representation.
    fn from_bits(value: u64) -> Self;

    /// Serialise to the [u64] binary representation.
    fn to_bits(&self) -> u64;
}

impl Bits64 for bool {
    const WIDTH: usize = 1;

    #[inline(always)]
    fn from_bits(value: u64) -> Self {
        value.bit(0)
    }

    #[inline(always)]
    fn to_bits(&self) -> u64 {
        if *self {
            1
        } else {
            0
        }
    }
}

macro_rules! bits64_builtin {
    ( $t:ty ) => {
        impl Bits64 for $t {
            const WIDTH: usize = { <$t>::BITS as usize };

            #[inline(always)]
            fn from_bits(value: u64) -> Self {
                value as $t
            }

            #[inline(always)]
            fn to_bits(&self) -> u64 {
                *self as u64
            }
        }
    };
}

bits64_builtin!(u8);
bits64_builtin!(u16);
bits64_builtin!(u32);
bits64_builtin!(u64);

bits64_builtin!(i8);
bits64_builtin!(i16);
bits64_builtin!(i32);
bits64_builtin!(i64);

/// Helper type for [Bits64] that always inhabits a default value of `WIDTH` bits
#[derive(Copy, Clone)]
pub struct ConstantBits<const WIDTH: usize, const VALUE: u64 = 0>;

impl<const WIDTH: usize, const VALUE: u64> Bits64 for ConstantBits<WIDTH, VALUE> {
    const WIDTH: usize = WIDTH;

    fn from_bits(_value: u64) -> Self {
        Self
    }

    fn to_bits(&self) -> u64 {
        VALUE
    }
}

impl<const WIDTH: usize, const VALUE: u64> fmt::Debug for ConstantBits<WIDTH, VALUE> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "0b{VALUE:b}/0x{VALUE:x}")
    }
}

/// Like [u64] but limited in width
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FixedWidthBits<const WIDTH: usize>(u64);

impl<const WIDTH: usize> Bits64 for FixedWidthBits<WIDTH> {
    const WIDTH: usize = WIDTH;

    fn from_bits(value: u64) -> Self {
        Self(value.bits((WIDTH - 1)..=0))
    }

    fn to_bits(&self) -> u64 {
        self.0.bits((WIDTH - 1)..=0)
    }
}
