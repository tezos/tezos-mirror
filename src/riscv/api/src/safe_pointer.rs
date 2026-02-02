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

/// A version of [`ocaml::Pointer`] which does not allow mutable references to the underlying data.
#[derive(ocaml::ToValue, ocaml::FromValue)]
pub struct SafePointer<T: Sync>(ocaml::Pointer<T>);

impl<T: ocaml::Custom + Sync + Send> From<T> for SafePointer<T> {
    fn from(t: T) -> Self {
        SafePointer(t.into())
    }
}

impl<T: Sync> Deref for SafePointer<T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.0.as_ref()
    }
}

impl<T> SafePointer<ImmutableState<T>>
where
    T: Clone,
    ImmutableState<T>: ocaml::Custom + Sync + Send,
{
    /// In the case that we have an `ImmutableState` and a function that requires a mutable
    /// reference, we first make a copy of the state to apply the function to.
    pub fn apply_imm<R>(&self, f: impl FnOnce(&mut T) -> R) -> (Self, R) {
        let (new_v, result) = self.apply(f);
        (new_v.into(), result)
    }
}
