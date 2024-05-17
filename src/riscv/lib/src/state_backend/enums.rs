// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{AllocatedOf, Atom, Cell, Elem, Manager};
use std::marker::PhantomData;

/// Cell representing an enumeration type
pub struct EnumCell<T, R: Elem, M: Manager> {
    cell: Cell<R, M>,
    _pd: PhantomData<T>,
}

/// Layout of an enum cell
pub type EnumCellLayout<R> = Atom<R>;

impl<T, R, M> EnumCell<T, R, M>
where
    T: Into<R>,
    R: Elem,
    M: Manager,
{
    /// Bind the enum cell to the allocated space.
    pub fn bind(space: AllocatedOf<EnumCellLayout<R>, M>) -> Self {
        Self {
            cell: Cell::bind(space),
            _pd: PhantomData,
        }
    }

    /// Reset the enum cell by writing the default value of `T`.
    pub fn reset(&mut self)
    where
        T: Default,
    {
        self.write(T::default());
    }

    /// Write a value to the enum cell.
    pub fn write(&mut self, value: T) {
        self.cell.write(value.into());
    }

    /// Read the value from the enum cell.
    pub fn read(&self) -> T
    where
        T: From<R>,
    {
        T::from(self.cell.read())
    }

    /// Read the value from the enum cell. If the value can't be converted from
    /// its underlying representation, return the default value for `T`.
    pub fn read_default(&self) -> T
    where
        T: TryFrom<R> + Default,
    {
        T::try_from(self.cell.read()).unwrap_or(T::default())
    }
}
