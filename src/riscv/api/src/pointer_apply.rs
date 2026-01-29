// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Defines extension traits for [`ocaml::Pointer`] holding either [`super::ImmutableState<T>`] or a [`super::MutableState<T>`]

use ocaml::Pointer;

use super::move_semantics::ImmutableState;
use super::move_semantics::MutableState;

/// Introduces `apply_ro` for applying a read-only function over a state.
pub trait ApplyReadOnly<T> {
    /// Applies the read-only function `f` over a state of type `T`.
    fn apply_ro<R>(&self, f: impl FnOnce(&T) -> R) -> R;
}

impl<T> ApplyReadOnly<T> for Pointer<ImmutableState<T>> {
    fn apply_ro<R>(&self, f: impl FnOnce(&T) -> R) -> R {
        self.as_ref().apply_ro(f)
    }
}

impl<T> ApplyReadOnly<T> for Pointer<MutableState<T>>
where
    T: Clone,
{
    fn apply_ro<R>(&self, f: impl FnOnce(&T) -> R) -> R {
        self.as_ref().apply_ro(f)
    }
}

/// Apply a mutable function on a [`ImmutableState<T>`] held under an [`Pointer`].
/// It may perform a deep copy if the reference is not unique.
pub fn apply_imm<R, T: Clone>(
    ptr: Pointer<ImmutableState<T>>,
    f: impl FnOnce(&mut T) -> R,
) -> (Pointer<ImmutableState<T>>, R)
where
    ImmutableState<T>: ocaml::Custom,
{
    let (new_state, result) = ptr.as_ref().share().apply(f);
    let ptr = Pointer::alloc_custom(new_state);
    (ptr, result)
}

/// Apply a mutable function on a [`MutableState<T>`] held under an [`Pointer`].
/// It may perform a deep copy if the reference is not unique.
pub fn apply_mut<R, T: Clone>(mut ptr: Pointer<MutableState<T>>, f: impl FnOnce(&mut T) -> R) -> R {
    ptr.as_mut().apply(f)
}
