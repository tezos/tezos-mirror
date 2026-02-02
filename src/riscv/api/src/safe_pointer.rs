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

use std::ops::Deref;

use super::move_semantics::ImmutableState;

#[derive(ocaml::ToValue, ocaml::FromValue)]
pub struct SafePointer<T>(ocaml::Pointer<T>);

impl<T: ocaml::Custom> From<T> for SafePointer<T> {
    fn from(t: T) -> Self {
        SafePointer(t.into())
    }
}

impl<T> Deref for SafePointer<T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.0.as_ref()
    }
}

impl<T> SafePointer<ImmutableState<T>> {
    pub fn apply_imm<R>(self, f: impl FnOnce(&mut T) -> R) -> (Self, R)
    where
        T: Clone,
        ImmutableState<T>: ocaml::Custom,
    {
        let v = self.share();
        let (new_v, result) = v.apply(f);
        (new_v.into(), result)
    }
}
