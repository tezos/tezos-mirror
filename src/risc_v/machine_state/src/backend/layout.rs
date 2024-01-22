// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::alloc::{Choreographer, Location, Placed};
use std::{array, marker::PhantomData};

/// Structural description of a state type
pub trait Layout {
    /// Representation of the locations in the state backend
    type Placed;

    /// Arrange the locations for atoms of this layout using a [Choreographer].
    fn place_with(alloc: &mut Choreographer) -> Self::Placed;

    /// See [`Choreographer::place`].
    fn placed() -> Placed<Self::Placed> {
        Choreographer::place::<Self>()
    }

    /// Representation of the allocated regions in the state backend
    type Allocated<B: super::Manager>;

    /// Allocate regions in the given state backend.
    fn allocate<B: super::Manager>(backend: &mut B, placed: Self::Placed) -> Self::Allocated<B>;
}

/// `L::Placed`
pub type PlacedOf<L> = <L as Layout>::Placed;

/// `L::Allocated`
pub type AllocatedOf<L, M> = <L as Layout>::Allocated<M>;

/// Layout for a single value
#[repr(transparent)]
pub struct Atom<T> {
    _pd: PhantomData<T>,
}

impl<T: super::Elem> Layout for Atom<T> {
    type Placed = Location<T>;

    fn place_with(alloc: &mut Choreographer) -> Self::Placed {
        alloc.alloc()
    }

    type Allocated<B: super::Manager> = super::Cell<T, B>;

    fn allocate<B: super::Manager>(backend: &mut B, placed: Self::Placed) -> Self::Allocated<B> {
        backend.allocate_cell(placed)
    }
}

/// Layout for a fixed number of values
#[repr(transparent)]
pub struct Array<T, const LEN: usize> {
    _pd: PhantomData<T>,
}

impl<T: super::Elem, const LEN: usize> Layout for Array<T, LEN> {
    type Placed = Location<[T; LEN]>;

    fn place_with(alloc: &mut Choreographer) -> Self::Placed {
        alloc.alloc()
    }

    type Allocated<B: super::Manager> = B::Region<T, LEN>;

    fn allocate<B: super::Manager>(backend: &mut B, placed: Self::Placed) -> Self::Allocated<B> {
        backend.allocate_region(placed)
    }
}

impl<A, B> Layout for (A, B)
where
    A: Layout,
    B: Layout,
{
    type Placed = (A::Placed, B::Placed);

    fn place_with(alloc: &mut Choreographer) -> Self::Placed {
        (A::place_with(alloc), B::place_with(alloc))
    }

    type Allocated<Back: super::Manager> = (A::Allocated<Back>, B::Allocated<Back>);

    fn allocate<Back: super::Manager>(
        backend: &mut Back,
        placed: Self::Placed,
    ) -> Self::Allocated<Back> {
        (
            A::allocate(backend, placed.0),
            B::allocate(backend, placed.1),
        )
    }
}

impl<A, B, C> Layout for (A, B, C)
where
    A: Layout,
    B: Layout,
    C: Layout,
{
    type Placed = (A::Placed, B::Placed, C::Placed);

    fn place_with(alloc: &mut Choreographer) -> Self::Placed {
        (
            A::place_with(alloc),
            B::place_with(alloc),
            C::place_with(alloc),
        )
    }

    type Allocated<Back: super::Manager> =
        (A::Allocated<Back>, B::Allocated<Back>, C::Allocated<Back>);

    fn allocate<Back: super::Manager>(
        backend: &mut Back,
        placed: Self::Placed,
    ) -> Self::Allocated<Back> {
        (
            A::allocate(backend, placed.0),
            B::allocate(backend, placed.1),
            C::allocate(backend, placed.2),
        )
    }
}

impl<A, B, C, D> Layout for (A, B, C, D)
where
    A: Layout,
    B: Layout,
    C: Layout,
    D: Layout,
{
    type Placed = (A::Placed, B::Placed, C::Placed, D::Placed);

    fn place_with(alloc: &mut Choreographer) -> Self::Placed {
        (
            A::place_with(alloc),
            B::place_with(alloc),
            C::place_with(alloc),
            D::place_with(alloc),
        )
    }

    type Allocated<Back: super::Manager> = (
        A::Allocated<Back>,
        B::Allocated<Back>,
        C::Allocated<Back>,
        D::Allocated<Back>,
    );

    fn allocate<Back: super::Manager>(
        backend: &mut Back,
        placed: Self::Placed,
    ) -> Self::Allocated<Back> {
        (
            A::allocate(backend, placed.0),
            B::allocate(backend, placed.1),
            C::allocate(backend, placed.2),
            D::allocate(backend, placed.3),
        )
    }
}

impl<T, const LEN: usize> Layout for [T; LEN]
where
    T: Layout,
{
    type Placed = [T::Placed; LEN];

    fn place_with(alloc: &mut Choreographer) -> Self::Placed {
        array::from_fn(|_| T::place_with(alloc))
    }

    type Allocated<B: super::Manager> = [T::Allocated<B>; LEN];

    fn allocate<B: super::Manager>(backend: &mut B, placed: Self::Placed) -> Self::Allocated<B> {
        placed.map(|placed| T::allocate(backend, placed))
    }
}
