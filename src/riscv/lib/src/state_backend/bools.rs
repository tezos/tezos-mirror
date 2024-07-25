// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{AllocatedOf, Atom, Cell, CellRead, CellWrite, Manager};

pub type BoolCellLayout = Atom<u8>;

/// Cell representing an enumeration type
pub struct BoolCell<M: Manager> {
    bool: Cell<u8, M>,
}

impl<M> BoolCell<M>
where
    M: Manager,
{
    pub fn bind(space: AllocatedOf<BoolCellLayout, M>) -> Self {
        Self { bool: space }
    }
}

impl<M: Manager> CellRead for BoolCell<M> {
    type Value = bool;

    #[inline(always)]
    fn read(&self) -> Self::Value {
        self.bool.read() != 0
    }
}

impl<M: Manager> CellWrite for BoolCell<M> {
    #[inline(always)]
    fn write(&mut self, value: Self::Value) {
        self.bool.write(value as u8)
    }

    #[inline(always)]
    fn replace(&mut self, value: Self::Value) -> Self::Value {
        self.bool.replace(value as u8) != 0
    }
}
