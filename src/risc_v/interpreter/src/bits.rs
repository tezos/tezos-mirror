// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

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
