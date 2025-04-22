// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Defines extension traits for [`ocaml::Pointer`] holding either [`super::ImmutableState<T>`] or a [`super::MutableState<T>`]

use super::move_semantics::ImmutableState;
use super::move_semantics::MutableState;

ocaml::custom!(ImmutableState<T>);
ocaml::custom!(MutableState<T>);

/// Introduces `apply_ro` for applying a read-only function over a state.
pub trait ApplyReadOnly<T> {
    /// Applies the read-only function `f` over a state of type `T`.
    fn apply_ro<R>(&self, f: impl FnOnce(&T) -> R) -> R;
}

impl<T> ApplyReadOnly<T> for ocaml::Pointer<ImmutableState<T>> {
    fn apply_ro<R>(&self, f: impl FnOnce(&T) -> R) -> R {
        self.as_ref().apply_ro(f)
    }
}

impl<T> ApplyReadOnly<T> for ocaml::Pointer<MutableState<T>>
where
    T: Clone,
{
    fn apply_ro<R>(&self, f: impl FnOnce(&T) -> R) -> R {
        self.as_ref().apply_ro(f)
    }
}

/// Apply a mutable function on a [`ImmutableState<T>`] held under an [`ocaml::Pointer`].
/// It may perform a deep copy if the reference is not unique.
pub fn apply_imm<R, T: Clone>(
    ptr: ocaml::Pointer<ImmutableState<T>>,
    f: impl FnOnce(&mut T) -> R,
) -> (ocaml::Pointer<ImmutableState<T>>, R) {
    let (new_state, result) = ptr.as_ref().to_owned().apply(f);
    (new_state.into(), result)
}

/// Apply a mutable function on a [`MutableState<T>`] held under an [`ocaml::Pointer`].
/// It may perform a deep copy if the reference is not unique.
pub fn apply_mut<R, T: Clone>(
    mut ptr: ocaml::Pointer<MutableState<T>>,
    f: impl FnOnce(&mut T) -> R,
) -> R {
    ptr.as_mut().apply(f)
}
