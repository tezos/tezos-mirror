// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Comparison operations required in the [`ICB`], including implementations for interpreted mode.

use super::ICB;
use super::Predicate;
use crate::machine_state::MachineCoreState;
use crate::machine_state::memory::MemoryConfig;
use crate::state_backend::ManagerReadWrite;

/// Trait for comparison operations on **XValues** used in the [`ICB`].
pub trait Comparable<I: ICB + ?Sized> {
    /// Compare two values, given the operation to compare them with.
    fn compare(self, other: Self, predicate: Predicate, icb: &mut I) -> I::Bool;
}

impl<MC: MemoryConfig, M: ManagerReadWrite> Comparable<MachineCoreState<MC, M>>
    for <MachineCoreState<MC, M> as ICB>::XValue
{
    #[inline(always)]
    fn compare(
        self,
        other: Self,
        predicate: Predicate,
        _: &mut MachineCoreState<MC, M>,
    ) -> <MachineCoreState<MC, M> as ICB>::Bool {
        match predicate {
            Predicate::Equal => self == other,
            Predicate::NotEqual => self != other,
            Predicate::LessThanSigned => (self as i64) < (other as i64),
            Predicate::LessThanUnsigned => self < other,
            Predicate::LessThanOrEqualSigned => (self as i64) <= (other as i64),
            Predicate::GreaterThanSigned => (self as i64) > (other as i64),
            Predicate::GreaterThanOrEqualSigned => (self as i64) >= (other as i64),
            Predicate::GreaterThanOrEqualUnsigned => self >= other,
        }
    }
}

impl<MC: MemoryConfig, M: ManagerReadWrite> Comparable<MachineCoreState<MC, M>>
    for <MachineCoreState<MC, M> as ICB>::XValue32
{
    #[inline(always)]
    fn compare(
        self,
        other: Self,
        predicate: Predicate,
        _: &mut MachineCoreState<MC, M>,
    ) -> <MachineCoreState<MC, M> as ICB>::Bool {
        match predicate {
            Predicate::Equal => self == other,
            Predicate::NotEqual => self != other,
            Predicate::LessThanSigned => (self as i32) < (other as i32),
            Predicate::LessThanUnsigned => self < other,
            Predicate::LessThanOrEqualSigned => (self as i32) <= (other as i32),
            Predicate::GreaterThanSigned => (self as i32) > (other as i32),
            Predicate::GreaterThanOrEqualSigned => (self as i32) >= (other as i32),
            Predicate::GreaterThanOrEqualUnsigned => self >= other,
        }
    }
}
