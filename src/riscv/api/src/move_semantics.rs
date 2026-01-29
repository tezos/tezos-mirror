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

use parking_lot::RwLock;

pub struct ImmutableState<T>(Arc<T>);

impl<T> From<MutableState<T>> for ImmutableState<T> {
    /// Very cheap conversion from [`MutableState<T>`] to [`ImmutableState<T>`]
    fn from(value: MutableState<T>) -> Self {
        match value.0.into_inner() {
            MutableInner::Owned(t) => ImmutableState::new(t),
            MutableInner::Borrowed(arc) => ImmutableState(arc),
        }
    }
}

/// Simple [`ImmutableState`]. Functions that modify state can have signature `ImmutableState ->
/// ImmutableState`
impl<T> ImmutableState<T> {
    pub fn new(state: T) -> Self {
        Self(Arc::new(state))
    }

    /// Cheaply creates a new `ImmutableState` using `Arc::clone` to get a new reference. Underlying
    /// data is not copied.
    pub fn share(&self) -> Self {
        Self(Arc::clone(&self.0))
    }

    /// Create a mutable state from an ImmutableState state
    #[inline]
    pub fn to_mut_state(&self) -> MutableState<T> {
        MutableState::borrowed(Arc::clone(&self.0))
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
        let mut_state: MutableState<T> = self.into();
        let result = mut_state.apply(f);
        let imm_state = mut_state.into();
        (imm_state, result)
    }
}

/// [`MutableState`] can hold a state of type `T` and have it borrowed or owned.
/// Mutable functions never return a new reference, as the mutation occurs in place.
///
/// Lifecycle of a [`MutableState`] is:
/// 1. Created from an [`ImmutableState`] becoming a borrowed [`MutableState`].
/// 2. Transform to an owned [`MutableState`] when a mutable operation is performed.
///
/// or
///
/// 1. Created from an underlying state `T` directly with constructor [`MutableState::owned`]
pub struct MutableState<T>(RwLock<MutableInner<T>>);

enum MutableInner<T> {
    /// Owned variant of the state. When a mutating function is applied, the state is NOT copied, saving memory.
    Owned(T),
    /// Borrowed variant of the state.
    Borrowed(Arc<T>),
}

impl<T> From<ImmutableState<T>> for MutableState<T> {
    /// Very cheap conversion from [`ImmutableState<T>`] to [`MutableState<T>`]
    fn from(value: ImmutableState<T>) -> Self {
        MutableState::borrowed(value.0)
    }
}

impl<T> MutableState<T> {
    /// Construct a `MutableState<T>` from an owned `T`.
    pub fn owned(t: T) -> Self {
        MutableState(RwLock::new(MutableInner::Owned(t)))
    }

    /// Construct a `MutableState<T>` from an `Arc<T>` without cloning the underlying data.
    pub fn borrowed(arc: Arc<T>) -> Self {
        MutableState(RwLock::new(MutableInner::Borrowed(arc)))
    }

    /// Create an ImmutableState from a MutableState. This will make a copy if the underlying data has been modified.
    #[inline]
    pub fn to_imm_state(&self) -> ImmutableState<T>
    where
        T: Clone,
    {
        let guard = self.0.try_read().expect(
            "MutableState::to_imm_state shouldn't try to read a MutableState that is being written to.",
        );
        match guard.deref() {
            MutableInner::Owned(t) => ImmutableState::new(t.clone()),
            MutableInner::Borrowed(arc) => ImmutableState(Arc::clone(arc)),
        }
    }

    /// Apply a read-only function `f` over the underlying state. Never clones data.
    #[inline]
    pub fn apply_ro<R>(&self, f: impl FnOnce(&T) -> R) -> R {
        let guard = self.0.try_read().expect(
            "MutableState::apply_ro shouldn't try to read a MutableState that is being written to.",
        );
        match guard.deref() {
            MutableInner::Owned(t) => f(t),
            MutableInner::Borrowed(arc) => f(arc),
        }
    }

    /// Apply a mutable function `f` mutating the state in place. May perform a copy if
    /// the underlying state is a borrowed [`MutableState`].
    #[inline]
    pub fn apply<R>(&self, f: impl FnOnce(&mut T) -> R) -> R
    where
        T: Clone,
    {
        let mut guard = self.0.try_write().expect(
            "MutableState::apply should not be competing for write access to MutableState.",
        );
        match guard.deref_mut() {
            MutableInner::Owned(t) => f(t),
            inner => {
                let (t, res) = if let MutableInner::Borrowed(arc) = inner {
                    let mut t = arc.as_ref().clone();
                    let res = f(&mut t);
                    (t, res)
                } else {
                    unreachable!()
                };
                *inner = MutableInner::Owned(t);
                res
            }
        }
    }
}
