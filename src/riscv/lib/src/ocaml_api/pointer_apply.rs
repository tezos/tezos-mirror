// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Defines extension traits for [`ocaml::Pointer`] holding either [`super::ImmutableState<T>`] or a [`super::MutableState<T>`]

use super::move_semantics::ImmutableState;

ocaml::custom!(ImmutableState<T>);

pub trait ImmutableApply<T>
where
    Self: Sized,
{
    /// Apply a mutable function on a [`ImmutableState<T>`] held under an [`ocaml::Pointer`].
    /// It may perform a deep copy if the reference is not unique.
    fn apply<R>(self, f: impl FnOnce(&mut T) -> R) -> (Self, R);

    /// Apply an immutable function on a [`ImmutableState<T>`] held under an [`ocaml::Pointer`].
    fn apply_ro<R>(&self, f: impl FnOnce(&T) -> R) -> R;
}

impl<T> ImmutableApply<T> for ocaml::Pointer<ImmutableState<T>>
where
    T: Clone,
{
    fn apply<R>(self, f: impl FnOnce(&mut T) -> R) -> (Self, R) {
        // XXX: When ocaml::Pointer will add an API similar to Arc::get_mut or Arc::into_inner
        // it will be used here to potentially avoid unnecessary copies.
        let (new_state, result) = self.as_ref().to_owned().apply(f);
        (new_state.into(), result)
    }

    fn apply_ro<R>(&self, f: impl FnOnce(&T) -> R) -> R {
        self.as_ref().apply_ro(f)
    }
}
