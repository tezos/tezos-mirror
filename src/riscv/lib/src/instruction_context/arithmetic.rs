// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Arithmetic operations required in the [`ICB`], including implementations for interpreted mode.

use super::ICB;
use super::Shift;
use crate::machine_state::registers::XValue;
use crate::machine_state::registers::XValue32;

/// Trait for arithmetic operations on **XValues** used in the [`ICB`].
pub trait Arithmetic<I: ICB + ?Sized>: Copy {
    /// Perform a wrapping add of two **XValues**, returning the new value.
    ///
    /// This behaves identically for both signed & unsigned values.
    fn add(self, other: Self, icb: &mut I) -> Self;

    /// Perform a wrapping sub of two **XValues**, returning the new value.
    ///
    /// This behaves identically for both signed & unsigned values.
    fn sub(self, other: Self, icb: &mut I) -> Self;

    /// Perform a bitwise and of two **XValues**, returning the new value.
    fn and(self, other: Self, icb: &mut I) -> Self;

    /// Perform a bitwise or of two **XValues**, returning the new value.
    fn or(self, other: Self, icb: &mut I) -> Self;

    /// Perform a bitwise xor of two **XValues**, returning the new value.
    fn xor(self, other: Self, icb: &mut I) -> Self;

    /// Perform a bitwise multiplication of two **XValues**, returning the new value.
    ///
    /// This behaves identically for both signed & unsigned values.
    fn mul(self, other: Self, icb: &mut I) -> Self;

    /// Negate the value of the **XValue**.
    fn negate(self, icb: &mut I) -> Self;

    /// Perform a shift of the **XValue** as determined by the given [`Shift`].
    fn shift(self, shift: Shift, amount: Self, icb: &mut I) -> Self;
}

impl<I: ICB> Arithmetic<I> for XValue {
    fn add(self, other: Self, _: &mut I) -> Self {
        self.wrapping_add(other)
    }

    fn sub(self, other: Self, _: &mut I) -> Self {
        self.wrapping_sub(other)
    }

    fn and(self, other: Self, _: &mut I) -> Self {
        self & other
    }

    fn or(self, other: Self, _: &mut I) -> Self {
        self | other
    }

    fn xor(self, other: Self, _: &mut I) -> Self {
        self ^ other
    }

    fn mul(self, other: Self, _: &mut I) -> Self {
        self.wrapping_mul(other)
    }

    fn negate(self, _: &mut I) -> Self {
        0_u64.wrapping_sub(self)
    }

    fn shift(self, shift: Shift, amount: Self, _: &mut I) -> Self {
        match shift {
            Shift::Left => self << amount,
            Shift::RightUnsigned => self >> amount,
            Shift::RightSigned => (self as i64 >> amount) as XValue,
        }
    }
}

impl<I: ICB> Arithmetic<I> for XValue32 {
    fn add(self, other: Self, _: &mut I) -> Self {
        self.wrapping_add(other)
    }

    fn sub(self, other: Self, _: &mut I) -> Self {
        self.wrapping_sub(other)
    }

    fn and(self, other: Self, _: &mut I) -> Self {
        self & other
    }

    fn or(self, other: Self, _: &mut I) -> Self {
        self | other
    }

    fn xor(self, other: Self, _: &mut I) -> Self {
        self ^ other
    }

    fn mul(self, other: Self, _: &mut I) -> Self {
        self.wrapping_mul(other)
    }

    fn negate(self, _: &mut I) -> Self {
        0_u32.wrapping_sub(self)
    }

    fn shift(self, shift: Shift, amount: Self, _: &mut I) -> Self {
        match shift {
            Shift::Left => self << amount,
            Shift::RightUnsigned => self >> amount,
            Shift::RightSigned => (self as i32 >> amount) as XValue32,
        }
    }
}
