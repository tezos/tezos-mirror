// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::array;

use crate::array_utils::boxed_from_fn;
use crate::state_backend::ManagerAlloc;
use crate::state_backend::ManagerBase;

/// Methods for creating a new state without additional information
pub trait NewState<M: ManagerBase> {
    /// Create a new state.
    fn new(manager: &mut M) -> Self
    where
        M: ManagerAlloc;
}

impl<T: NewState<M>, const LEN: usize, M: ManagerBase> NewState<M> for [T; LEN] {
    fn new(manager: &mut M) -> Self
    where
        M: ManagerAlloc,
    {
        array::from_fn(|_| T::new(manager))
    }
}

// We cannot compose the implementations of `NewState` for `[T; LEN]` and `Box<_>`. Doing so would
// allocate potentially large arrays on the stack and only then move them to the heap using `Box`.
// This results in potential stack overflow for large arrays.
//
// To avoid this, we implement a combined version `Box<[T; LEN]>` that does not allocate on the
// stack. This makes usage of `NewState::new` safe everywhere as it doesn't sneakily compose
// implementations that would result in stack overflows.
//
// This comes with a small trade-off, we loose out on the `Box<_>` implementation of `NewState`.
// However, this is not a problem since we can always use `Box::new` independently.
impl<T: NewState<M>, const LEN: usize, M: ManagerBase> NewState<M> for Box<[T; LEN]> {
    fn new(manager: &mut M) -> Self
    where
        M: ManagerAlloc,
    {
        boxed_from_fn(|| T::new(manager))
    }
}
