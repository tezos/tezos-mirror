// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! # Move semantics aware state types
//!
//! Mutable and Immutable wrappers over a generic type `T`.
//! Exposes a mutable & immutable API to an underlying state when interfacing with OCaml

use std::ops::Deref;
use std::ops::DerefMut;
use std::sync::Arc;
use std::sync::RwLock;

#[derive(Clone)]
pub struct ImmutableState<T>(Arc<T>);

impl<T> ImmutableState<T> {
    pub fn new(state: T) -> Self {
        ImmutableState(Arc::new(state))
    }
}

impl<T> From<MutableState<T>> for ImmutableState<T> {
    /// Very cheap conversion from [`MutableState<T>`] to [`ImmutableState<T>`]
    fn from(value: MutableState<T>) -> Self {
        match value {
            MutableState::Owned(state) => {
                let inner = state
                    .into_inner()
                    .expect("MutableState RwLock should not be poisoned.");
                ImmutableState::new(inner)
            }
            MutableState::Borrowed(arc_state) => ImmutableState(arc_state),
        }
    }
}

/// Simple [`ImmutableState`]. Functions that modify state can have signature `ImmutableState -> ImmutableState`
impl<T> ImmutableState<T> {
    /// Create a mutable state from an immutable state
    #[inline]
    pub fn to_mut_state(&self) -> MutableState<T> {
        MutableState::Borrowed(self.0.clone())
    }

    /// Apply a read-only function `f` over the underlying state. Never clones data.
    #[inline]
    pub fn apply_ro<R>(&self, f: impl FnOnce(&T) -> R) -> R {
        f(&self.0)
    }

    /// Apply a mutable function `f` over the underlying state. Clones the old data to the new object.
    #[inline]
    #[must_use = "ImmutableState::apply returns new state"]
    pub fn apply<R>(self, f: impl FnOnce(&mut T) -> R) -> (Self, R)
    where
        T: Clone,
    {
        let mut mut_state: MutableState<T> = self.into();
        let result = mut_state.apply(f);
        let imm_state = mut_state.into();
        (imm_state, result)
    }
}

/// [`MutableState`] can hold a state of type `T` and have it borrowed or owned.
/// Mutable functions never return a new reference, as the mutation occurs in place.
///
/// Lifecycle of a [`MutableState`] is:
/// 1. Created from an [`ImmutableState`] becoming [`MutableState::Borrowed`].
/// 2. Transform to [`MutableState::Owned`] when a mutable operation is performed on [`MutableState::Borrowed`].
///
/// or
///
/// 1. Created from an underlying state `T` directly with constructor [`MutableState::Owned`]
pub enum MutableState<T> {
    /// Owned variant of the state. When a mutating function is applied, the state is NOT copied, saving memory.
    Owned(RwLock<T>),
    /// Borrowed variant of the state.
    Borrowed(Arc<T>),
}

impl<T> From<ImmutableState<T>> for MutableState<T> {
    /// Very cheap conversion from [`ImmutableState<T>`] to [`MutableState<T>`]
    fn from(value: ImmutableState<T>) -> Self {
        MutableState::Borrowed(value.0)
    }
}

impl<T> MutableState<T> {
    pub fn new(state: T) -> Self {
        MutableState::Owned(RwLock::new(state))
    }

    /// Create an immutable state from a mutable state
    #[inline]
    pub fn to_imm_state(&self) -> ImmutableState<T>
    where
        T: Clone,
    {
        match self {
            MutableState::Owned(state) => {
                let read_guard = state
                    .try_read()
                    .expect("Should not be competing for read access to MutableState.");
                ImmutableState::new(read_guard.clone())
            }
            MutableState::Borrowed(arc_state) => ImmutableState(arc_state.clone()),
        }
    }

    /// Apply a read-only function `f` over the underlying state. Never clones data.
    #[inline]
    pub fn apply_ro<R>(&self, f: impl FnOnce(&T) -> R) -> R {
        match self {
            MutableState::Owned(state) => {
                let read_guard = state.try_read().expect(
                    "Should not attempt read access to a MutableState that is being written to.",
                );
                f(read_guard.deref())
            }
            MutableState::Borrowed(state) => f(state),
        }
    }

    /// Apply a mutable function `f` mutating the state in place. May perform a copy if
    /// the underlying state is [`MutableState::Borrowed`].
    #[inline]
    pub fn apply<R>(&mut self, f: impl FnOnce(&mut T) -> R) -> R
    where
        T: Clone,
    {
        let (new, res) = match self {
            MutableState::Owned(state) => {
                let mut write_guard = state
                    .try_write()
                    .expect("Should not be competing for write access to MutableState.");
                return f(write_guard.deref_mut());
            }
            MutableState::Borrowed(arc_state) => {
                // We don't know how many references there are to the Arc state because OCaml
                // aliases the reference without invoking "clone" on the Rust side.
                let mut state = arc_state.as_ref().clone();
                let res = f(&mut state);
                (Self::new(state), res)
            }
        };
        *self = new;
        res
    }
}
