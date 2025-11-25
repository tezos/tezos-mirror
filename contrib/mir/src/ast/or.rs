// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! General-purpose sum type and some utilities. Used to represent Michelson `or
//! 'a 'b` type.

/// A simple binary sum type, corresponding to the Michelson `Or` type. It can
/// also be used as a general-purpose sum type where [Result] doesn't quite fit.
#[derive(Debug, Clone, Eq, PartialOrd, Ord, PartialEq)]
pub enum Or<L, R> {
    /// "Left" variant of the sum.
    Left(L),
    /// "Right" variant of the sum.
    Right(R),
}

impl<T> Or<T, T> {
    /// For an [Or] that has the same type in both branches, transform a value
    /// inside the [Or] using a supplied function.
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Or<U, U> {
        match self {
            Self::Left(x) => Or::Left(f(x)),
            Self::Right(x) => Or::Right(f(x)),
        }
    }
}

impl<T, U> Or<T, U> {
    /// Depending on whether the [Or] contains [Or::Left] or [Or::Right], use
    /// correspondingly either `f` or `g` to transform the value inside.
    pub fn bimap<V, W>(self, f: impl FnOnce(T) -> V, g: impl FnOnce(U) -> W) -> Or<V, W> {
        match self {
            Self::Left(x) => Or::Left(f(x)),
            Self::Right(x) => Or::Right(g(x)),
        }
    }
}
