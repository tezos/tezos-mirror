// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
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

use crate::try_clone::TryClone;

/// Trait to allow rust types to self-configure the 'resource markers' passed to
/// OCaml's `caml_alloc_custom` function.
///
/// These control how many of a particular type can be allocated before OCaml will
/// start to be proactive about GC-ing them.
///
/// The `USED` and `MAX` are defaulted to the values chosen by [`ocaml::Custom`] trait.
/// The resources they refer to are of 'arbitrary unit' - the importance is the
/// comparison between `USED` and `MAX`.
pub trait CustomGcResource {
    /// Custom type name when under immutable semantics
    const IMMUTABLE_NAME: &'static str;

    /// Custom type name when under mutable semantics
    const MUTABLE_NAME: &'static str;

    /// How many resources an immutable allocation of this type should consume.
    const IMMUTABLE_USED: usize = 0;

    /// The maximum number of resources immutable allocations of the type may consume
    /// before GC kicks in more heavily.
    const IMMUTABLE_MAX: usize = 1;

    /// How many resources an immutable allocation of this type should consume.
    const MUTABLE_USED: usize = 0;

    /// The maximum number of resources mutable allocations of the type may consume
    /// before GC kicks in more heavily.
    const MUTABLE_MAX: usize = 1;
}

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

    /// Create a mutable state from an ImmutableState state
    #[inline]
    pub fn to_mut_state(&self) -> MutableState<T> {
        MutableState::borrowed(Arc::clone(&self.0))
    }

    /// Apply a read-only function `f` over the underlying state. Never clones data.
    #[inline]
    pub fn apply_ro<U, R>(&self, f: impl FnOnce(&U) -> R) -> R
    where
        T: Deref<Target = U>,
    {
        f(&self.0)
    }

    /// Apply a mutable function `f` over the underlying state. Clones the old data to the new object.
    #[inline]
    #[must_use = "ImmutableState::apply returns new state"]
    pub fn apply<U, R>(&self, f: impl FnOnce(&mut U) -> R) -> Result<(Self, R), T::Error>
    where
        T: TryClone + DerefMut<Target = U>,
    {
        let mut t = self.0.as_ref().try_clone()?;
        let result = f(&mut t);
        Ok((ImmutableState::new(t), result))
    }
}

impl<T: CustomGcResource> ocaml::Custom for ImmutableState<T> {
    const NAME: &'static str = T::IMMUTABLE_NAME;

    const OPS: ocaml::custom::CustomOps = ocaml::custom::CustomOps {
        identifier: Self::NAME.as_ptr() as *const ocaml::sys::Char,
        ..ocaml::custom::CustomOps {
            finalize: Some(Self::finalize),
            ..ocaml::custom::DEFAULT_CUSTOM_OPS
        }
    };

    const USED: usize = T::IMMUTABLE_USED;
    const MAX: usize = T::IMMUTABLE_MAX;
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
///
/// Note on thread-safety: this type provide interior mutability using a `RwLock`. From a Rust POV
/// the implementation will seem rather strange because we use `try_read().expect(...)` and
/// `try_write().expect(...)`. This makes sense because the end user of this type is an OCaml
/// program calling an FFI; we wish to ensure thread-safety but we don't want to block as it could
/// cause the OCaml RT to hang.
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
    pub fn to_imm_state(&self) -> Result<ImmutableState<T>, T::Error>
    where
        T: TryClone,
    {
        let guard = self.0.try_read().expect(
            "Shouldn't try to read a MutableState that is being written to. See `move_semantics::MutableState`",
        );
        match guard.deref() {
            MutableInner::Owned(t) => Ok(ImmutableState::new(t.try_clone()?)),
            MutableInner::Borrowed(arc) => Ok(ImmutableState(Arc::clone(arc))),
        }
    }

    /// Apply a read-only function `f` over the underlying state. Never clones data.
    #[inline]
    pub fn apply_ro<U, R>(&self, f: impl FnOnce(&U) -> R) -> R
    where
        T: Deref<Target = U>,
    {
        let guard = self.0.try_read().expect(
            "Shouldn't try to read a MutableState that is being written to. See `move_semantics::MutableState`",
        );
        match guard.deref() {
            MutableInner::Owned(t) => f(t),
            MutableInner::Borrowed(arc) => f(arc),
        }
    }

    /// Apply a mutable function `f` mutating the state in place. May perform a copy if
    /// the underlying state is a borrowed [`MutableState`].
    #[inline]
    pub fn apply<U, R>(&self, f: impl FnOnce(&mut U) -> R) -> Result<R, T::Error>
    where
        T: TryClone + DerefMut<Target = U>,
    {
        let mut guard = self
            .0
            .try_write()
            .expect("Shouldn't be competing for write access. See `move_semantics::MutableState`");
        let inner = guard.deref_mut();
        match inner {
            MutableInner::Owned(t) => Ok(f(t)),
            MutableInner::Borrowed(arc) => {
                let mut t = arc.as_ref().try_clone()?;
                let res = f(&mut t);

                *inner = MutableInner::Owned(t);
                Ok(res)
            }
        }
    }
}

impl<T: CustomGcResource> ocaml::Custom for MutableState<T> {
    const NAME: &'static str = T::MUTABLE_NAME;

    const OPS: ocaml::custom::CustomOps = ocaml::custom::CustomOps {
        identifier: Self::NAME.as_ptr() as *const ocaml::sys::Char,
        ..ocaml::custom::CustomOps {
            finalize: Some(Self::finalize),
            ..ocaml::custom::DEFAULT_CUSTOM_OPS
        }
    };

    const USED: usize = T::MUTABLE_USED;
    const MAX: usize = T::MUTABLE_MAX;
}
