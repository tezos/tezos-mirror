// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Defines [`SafePointer`] and some methods on `SafePointer<MutableState<T>>` and
//! `SafePointer<ImmutableState<T>>`.
//!
//! [`SafePointer`] is a wrapper around [`ocaml::Pointer`] which only exposes the ImmutableState part of
//! the API (the `AsRef<T>` implementation). We use this together with interior mutability (see
//! [`super::move_semantics::MutableState`]) to enforce thread-safety across the Rust -> OCaml
//! interface.

use super::move_semantics::ImmutableState;
use super::move_semantics::MutableState;

#[derive(ocaml::ToValue, ocaml::FromValue)]
pub struct SafePointer<T>(ocaml::Pointer<T>);

impl<T: ocaml::Custom> From<T> for SafePointer<T> {
    fn from(t: T) -> Self {
        SafePointer(t.into())
    }
}

impl<T> AsRef<T> for SafePointer<T> {
    fn as_ref(&self) -> &T {
        self.0.as_ref()
    }
}

impl<T> SafePointer<ImmutableState<T>> {
    pub fn apply_ro<R>(&self, f: impl FnOnce(&T) -> R) -> R {
        self.as_ref().apply_ro(f)
    }

    pub fn apply_imm<R>(self, f: impl FnOnce(&mut T) -> R) -> (Self, R)
    where
        T: Clone,
        ImmutableState<T>: ocaml::Custom,
    {
        let v = self.as_ref().share();
        let (new_v, result) = v.apply(f);
        (new_v.into(), result)
    }
}

impl<T> SafePointer<MutableState<T>> {
    pub fn apply_ro<R>(&self, f: impl FnOnce(&T) -> R) -> R {
        self.as_ref().apply_ro(f)
    }

    pub fn apply_mut<R>(&self, f: impl FnOnce(&mut T) -> R) -> R
    where
        T: Clone,
    {
        self.as_ref().apply(f)
    }
}
