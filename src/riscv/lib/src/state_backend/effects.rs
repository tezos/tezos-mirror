// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::marker::PhantomData;

use super::{
    AllocatedOf, Atom, Cell, ManagerBase, ManagerClone, ManagerRead, ManagerReadWrite,
    ManagerWrite, Ref,
};

/// XXX: Workaround trait for not having enum variants as const-generics
pub trait EffectGetter {
    /// Effect type returned
    type Effect;

    /// Effect on write/replace operations.
    const EFFECT: Option<Self::Effect>;
}

pub type EffectCellLayout<T> = Atom<T>;

pub struct EffectCell<T: 'static, EG, M: ManagerBase> {
    inner: Cell<T, M>,
    _pd: PhantomData<EG>,
}

impl<T: 'static, EG, M: ManagerBase> EffectCell<T, EG, M> {
    pub fn bind(space: AllocatedOf<EffectCellLayout<T>, M>) -> Self {
        Self {
            inner: space,
            _pd: PhantomData,
        }
    }

    /// Obtain a structure with references to the bound regions of this type.
    pub fn struct_ref(&self) -> AllocatedOf<EffectCellLayout<T>, Ref<'_, M>> {
        self.inner.struct_ref()
    }
}

impl<T: Copy, EG: EffectGetter, M: ManagerBase> EffectCell<T, EG, M> {
    #[inline(always)]
    pub fn read(&self) -> T
    where
        M: ManagerRead,
    {
        self.inner.read()
    }

    #[inline(always)]
    #[must_use = "CSR Effect must be handled. Use 'handle_csr_effect()'"]
    pub fn write(&mut self, value: T) -> Option<EG::Effect>
    where
        M: ManagerWrite,
    {
        self.inner.write(value);
        EG::EFFECT
    }

    #[inline(always)]
    #[must_use = "CSR Effect must be handled. Use 'handle_csr_effect()'"]
    pub fn replace(&mut self, value: T) -> (T, Option<EG::Effect>)
    where
        M: ManagerReadWrite,
    {
        (self.inner.replace(value), EG::EFFECT)
    }
}

impl<T: Copy, EG, M: ManagerClone> Clone for EffectCell<T, EG, M> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            _pd: PhantomData,
        }
    }
}
