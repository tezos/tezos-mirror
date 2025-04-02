// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::marker::PhantomData;

use super::AllocatedOf;
use super::Atom;
use super::Cell;
use super::FnManager;
use super::ManagerAlloc;
use super::ManagerBase;
use super::ManagerClone;
use super::ManagerRead;
use super::ManagerReadWrite;
use super::ManagerWrite;
use super::Ref;
use crate::default::ConstDefault;
use crate::state::NewState;

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

impl<T: ConstDefault, EG, M: ManagerBase> EffectCell<T, EG, M> {
    pub fn bind(space: AllocatedOf<EffectCellLayout<T>, M>) -> Self {
        Self {
            inner: space,
            _pd: PhantomData,
        }
    }

    /// Given a manager morphism `f : &M -> N`, return the layout's allocated structure containing
    /// the constituents of `N` that were produced from the constituents of `&M`.
    pub fn struct_ref<'a, F: FnManager<Ref<'a, M>>>(
        &'a self,
    ) -> AllocatedOf<EffectCellLayout<T>, F::Output> {
        self.inner.struct_ref::<F>()
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

impl<T: ConstDefault, EG, M: ManagerBase> NewState<M> for EffectCell<T, EG, M> {
    fn new(manager: &mut M) -> Self
    where
        M: ManagerAlloc,
    {
        Self {
            inner: Cell::new(manager),
            _pd: PhantomData,
        }
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
