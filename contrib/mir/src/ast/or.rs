/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

#[derive(Debug, Clone, Eq, PartialOrd, Ord, PartialEq)]
pub enum Or<L, R> {
    Left(L),
    Right(R),
}

impl<T> Or<T, T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Or<U, U> {
        match self {
            Self::Left(x) => Or::Left(f(x)),
            Self::Right(x) => Or::Right(f(x)),
        }
    }
}

impl<T, U> Or<T, U> {
    pub fn bimap<V, W>(self, f: impl FnOnce(T) -> V, g: impl FnOnce(U) -> W) -> Or<V, W> {
        match self {
            Self::Left(x) => Or::Left(f(x)),
            Self::Right(x) => Or::Right(g(x)),
        }
    }
}
