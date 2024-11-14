// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{
    AllocatedOf, Atom, Cell, CellBase, CellRead, CellReadWrite, CellWrite, Elem, ManagerBase,
    ManagerRead, ManagerReadWrite, ManagerWrite, Ref,
};
use std::marker::PhantomData;

/// Cell representing an enumeration type
pub struct EnumCell<T, R: Elem, M: ManagerBase> {
    cell: Cell<R, M>,
    _pd: PhantomData<T>,
}

/// Layout of an enum cell
pub type EnumCellLayout<R> = Atom<R>;

impl<T, R, M> EnumCell<T, R, M>
where
    T: From<R> + Default,
    R: From<T> + Elem,
    M: ManagerBase,
{
    /// Bind the enum cell to the allocated space.
    pub fn bind(space: AllocatedOf<EnumCellLayout<R>, M>) -> Self {
        Self {
            cell: space,
            _pd: PhantomData,
        }
    }

    /// Obtain a structure with references to the bound regions of this type.
    pub fn struct_ref(&self) -> AllocatedOf<EnumCellLayout<R>, Ref<'_, M>> {
        self.cell.struct_ref()
    }

    /// Reset the enum cell by writing the default value of `T`.
    pub fn reset(&mut self)
    where
        M: ManagerWrite,
    {
        self.write(T::default());
    }

    /// Write a value to the enum cell.
    pub fn write(&mut self, value: T)
    where
        M: ManagerWrite,
    {
        self.cell.write(value.into());
    }

    /// Replace a value in the enum cell, returning the old value.
    pub fn replace(&mut self, value: T) -> T
    where
        M: ManagerReadWrite,
    {
        T::from(self.cell.replace(value.into()))
    }

    /// Read the value from the enum cell.
    pub fn read(&self) -> T
    where
        M: ManagerRead,
    {
        T::from(self.cell.read())
    }
}

impl<T, R, M> CellBase for EnumCell<T, R, M>
where
    R: Elem,
    M: ManagerBase,
{
    type Value = T;
}

impl<T, R, M> CellRead for EnumCell<T, R, M>
where
    T: From<R> + Default,
    R: From<T> + Elem,
    M: ManagerRead,
{
    fn read(&self) -> Self::Value {
        EnumCell::read(self)
    }
}

impl<T, R, M> CellWrite for EnumCell<T, R, M>
where
    T: From<R> + Default,
    R: From<T> + Elem,
    M: ManagerWrite,
{
    fn write(&mut self, value: Self::Value) {
        EnumCell::write(self, value)
    }
}

impl<T, R, M> CellReadWrite for EnumCell<T, R, M>
where
    T: From<R> + Default,
    R: From<T> + Elem,
    M: ManagerReadWrite,
{
    #[inline(always)]
    fn replace(&mut self, value: Self::Value) -> Self::Value {
        EnumCell::replace(self, value)
    }
}
