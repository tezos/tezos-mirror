// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::fmt;

macro_rules! bits_builtin {
    ($t:tt, $size:expr) => {
        #[allow(clippy::allow_attributes, reason = "As the macro generates methods regardless of whether they're used or not, we need to be able to silence some warnings")]
        pub mod $t {
            /// Creates a bitmask from a range of bits for a strict subset of a value.
            /// Panics if the range covers the whole value.
            #[inline(always)]
            pub const fn mask_subset(start: usize, end: usize) -> $t {
                debug_assert!(start < $size);
                debug_assert!(end <= start);
                debug_assert!(!(start == $size - 1 && end == 0));
                let top = 1 << (1 + start - end);
                (top - 1) << end
            }

            /// Creates a bitmask from a range of bits.
            #[allow(dead_code)]
            #[inline(always)]
            pub const fn mask(start: usize, end: usize) -> $t {
                if start == $size - 1 && end == 0 {
                    <$t>::MAX
                } else {
                    mask_subset(start, end)
                }
            }

            /// Returns the [bit] of `v` as a boolean.
            #[inline(always)]
            pub const fn bit(v: $t, bit: usize) -> bool {
                ((v >> bit) & 1) != 0
            }

            /// Returns a strict subset of bits of `v`.
            /// Panics if the range covers the whole value.
            #[inline(always)]
            pub const fn bits_subset(v: $t, start: usize, end: usize) -> $t {
                (v & mask_subset(start, end)) >> end
            }

            /// Sets [bit] in `v` to `value`.
            #[allow(dead_code)]
            #[inline(always)]
            pub const fn set_bit(v: $t, bit: usize, value: bool) -> $t {
                let mask = 1 << bit;
                (v & !mask) | (if value { 1 } else { 0 } << bit)
            }

            /// Replaces a strict subset of bits of `v` with `bits`.
            /// If `bits` is larger than the range, the highest bits will be truncated.
            /// Panics if the range covers the whole value.
            #[allow(dead_code)]
            #[inline(always)]
            pub const fn replace_subset(v: $t, start: usize, end: usize, bits: $t) -> $t {
                let mask = mask_subset(start, end);
                (v & !mask) | ((bits << end) & mask)
            }
        }
    };
}

bits_builtin!(u16, 16);
bits_builtin!(u64, 64);

/// Implementations can be converted to and from a binary [prim@u64] representation
pub trait Bits64 {
    const WIDTH: usize;

    /// Convert from the [prim@u64] binary representation.
    fn from_bits(value: u64) -> Self;

    /// Serialise to the [prim@u64] binary representation.
    fn to_bits(&self) -> u64;
}

impl Bits64 for bool {
    const WIDTH: usize = 1;

    #[inline(always)]
    fn from_bits(value: u64) -> Self {
        u64::bit(value, 0)
    }

    #[inline(always)]
    fn to_bits(&self) -> u64 {
        if *self { 1 } else { 0 }
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

/// Like [prim@u64] but limited in width
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FixedWidthBits<const WIDTH: usize>(u64);

impl<const WIDTH: usize> Bits64 for FixedWidthBits<WIDTH> {
    const WIDTH: usize = WIDTH;

    fn from_bits(value: u64) -> Self {
        Self(u64::bits_subset(value, WIDTH - 1, 0))
    }

    fn to_bits(&self) -> u64 {
        u64::bits_subset(self.0, WIDTH - 1, 0)
    }
}

/// Get the bitmask formed of `n` ones.
pub const fn ones(n: u64) -> u64 {
    // This function should not panic
    let sh_amt = 64_u64.saturating_sub(n);
    match n {
        0 => 0,
        _ => !0 >> sh_amt,
    }
}

#[cfg(test)]
mod tests {
    use crate::bits::u16;

    #[test]
    fn mask_middle() {
        assert_eq!(u16::mask_subset(4, 2), 0b0000_0000_0001_1100);
    }

    #[test]
    fn mask_top() {
        assert_eq!(u16::mask_subset(15, 11), 0b1111_1000_0000_0000);
    }

    #[test]
    fn mask_bottom() {
        assert_eq!(u16::mask_subset(2, 0), 0b0000_0000_0000_0111);
    }

    #[test]
    fn mask_full() {
        assert_eq!(u16::mask(15, 0), 0b1111_1111_1111_1111);
    }

    #[test]
    #[cfg(debug_assertions)]
    #[should_panic(expected = "assertion failed")]
    fn mask_reversed() {
        u16::mask_subset(2, 4);
    }

    #[test]
    #[cfg(debug_assertions)]
    #[should_panic(expected = "assertion failed")]
    fn mask_overflow() {
        u16::mask_subset(99, 2);
    }

    #[test]
    fn bit() {
        let bytes: u16 = 0b0000_0010_1001_0001;

        let mut bits = [false; 16];
        for (i, bit) in bits.iter_mut().enumerate() {
            *bit = u16::bit(bytes, 15 - i);
        }

        assert_eq!(bits, [
            false, false, false, false, false, false, true, false, true, false, false, true, false,
            false, false, true
        ]);
    }

    #[test]
    fn bits_middle() {
        assert_eq!(
            u16::bits_subset(0b0010_1110_1001_0011, 10, 3),
            0b0000_0000_1101_0010
        );
    }

    #[test]
    fn bits_top() {
        assert_eq!(
            u16::bits_subset(0b1110_0011_0011_1111, 15, 12),
            0b0000_0000_0000_1110
        );
    }

    #[test]
    fn bits_bottom() {
        assert_eq!(
            u16::bits_subset(0b0111_1011_1000_0110, 6, 0),
            0b0000_0000_0000_0110
        );
    }

    #[test]
    fn unset_false_bit() {
        assert_eq!(
            u16::set_bit(0b1100_1010_0111_1000, 13, false),
            0b1100_1010_0111_1000
        );
    }

    #[test]
    fn unset_true_bit() {
        assert_eq!(
            u16::set_bit(0b1100_1010_0111_1000, 14, false),
            0b1000_1010_0111_1000
        );
    }

    #[test]
    fn set_false_bit() {
        assert_eq!(
            u16::set_bit(0b1100_1010_0111_1000, 1, true),
            0b1100_1010_0111_1010
        );
    }

    #[test]
    fn set_true_bit() {
        assert_eq!(
            u16::set_bit(0b1100_1010_0111_1000, 3, true),
            0b1100_1010_0111_1000
        );
    }

    #[test]
    fn replace_middle() {
        assert_eq!(
            u16::replace_subset(0b0111_0010_1100_1101, 11, 5, 0b011_0011),
            0b0111_0110_0110_1101
        );
    }

    #[test]
    fn replace_top() {
        assert_eq!(
            u16::replace_subset(0b0011_1100_0101_0110, 15, 10, 0b11_0101),
            0b1101_0100_0101_0110
        );
    }

    #[test]
    fn replace_bottom() {
        assert_eq!(
            u16::replace_subset(0b1111_1001_0100_1100, 7, 0, 0b1110_1110),
            0b1111_1001_1110_1110
        );
    }

    #[test]
    fn replace_overlong() {
        assert_eq!(
            u16::replace_subset(0b0000_0000_0000_0000, 7, 4, 0b1111_1111_1111),
            0b0000_0000_1111_0000
        );
    }
}
