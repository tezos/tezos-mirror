// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{
    alloc::{Choreographer, Location, Placed},
    Cell,
};
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
        let loc = placed.as_array();
        let region = backend.allocate_region(loc);
        Cell::bind(region)
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

/// Usage: Provide a struct with each field holding a layout.
///
/// ```
/// use octez_riscv::*;
/// use octez_riscv::state_backend::*;
/// use octez_riscv::machine_state::csregisters::CSRRepr;
/// use octez_riscv::machine_state::mode::Mode;
/// use octez_riscv::struct_layout;
///
/// struct_layout!(
///     pub struct ExampleLayout {
///         satp_ppn: Atom<CSRRepr>,
///         mode: EnumCellLayout<u8>,
///         cached: BoolCellLayout,
///     }
/// );
/// ```
#[macro_export]
macro_rules! struct_layout {
    ($vis:vis struct $layout_t:ident {
        $($field_name:ident: $cell_repr:ty),+
        $( , )?
    }) => {
        paste::paste! {

            $vis struct $layout_t {
                $( [<_$field_name>]: $cell_repr ),+
            }

            pub struct [<$layout_t Placed>] {
                $( $field_name: PlacedOf<$cell_repr> ),+
            }

            pub struct [<$layout_t Allocated>]<M: Manager>
            {
                $( $field_name: AllocatedOf<$cell_repr, M> ),+
            }

            impl Layout for $layout_t {
                type Placed = [<$layout_t Placed>];
                type Allocated<Back: Manager> = [<$layout_t Allocated>]<Back>;

                fn place_with(alloc: &mut Choreographer) -> Self::Placed {
                    [<$layout_t Placed>] {
                        $($field_name: alloc.alloc()),+
                    }
                }

                fn allocate<B: $crate::state_backend::Manager>(
                    backend: &mut B,
                    placed: Self::Placed
                ) -> Self::Allocated<B> {
                    Self::Allocated {
                        $($field_name: <$cell_repr as Layout>::allocate(
                            backend, placed.$field_name
                        )),+
                    }
                }
            }
        }
    };
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

impl<A, B, C, D, E> Layout for (A, B, C, D, E)
where
    A: Layout,
    B: Layout,
    C: Layout,
    D: Layout,
    E: Layout,
{
    type Placed = (A::Placed, B::Placed, C::Placed, D::Placed, E::Placed);

    fn place_with(alloc: &mut Choreographer) -> Self::Placed {
        (
            A::place_with(alloc),
            B::place_with(alloc),
            C::place_with(alloc),
            D::place_with(alloc),
            E::place_with(alloc),
        )
    }

    type Allocated<Back: super::Manager> = (
        A::Allocated<Back>,
        B::Allocated<Back>,
        C::Allocated<Back>,
        D::Allocated<Back>,
        E::Allocated<Back>,
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
            E::allocate(backend, placed.4),
        )
    }
}

impl<A, B, C, D, E, F> Layout for (A, B, C, D, E, F)
where
    A: Layout,
    B: Layout,
    C: Layout,
    D: Layout,
    E: Layout,
    F: Layout,
{
    type Placed = (
        A::Placed,
        B::Placed,
        C::Placed,
        D::Placed,
        E::Placed,
        F::Placed,
    );

    fn place_with(alloc: &mut Choreographer) -> Self::Placed {
        (
            A::place_with(alloc),
            B::place_with(alloc),
            C::place_with(alloc),
            D::place_with(alloc),
            E::place_with(alloc),
            F::place_with(alloc),
        )
    }

    type Allocated<Back: super::Manager> = (
        A::Allocated<Back>,
        B::Allocated<Back>,
        C::Allocated<Back>,
        D::Allocated<Back>,
        E::Allocated<Back>,
        F::Allocated<Back>,
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
            E::allocate(backend, placed.4),
            F::allocate(backend, placed.5),
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

/// This [`Layout`] is identical to [`[T; LEN]`] but it allows you to choose a very high `LEN`.
pub struct Many<T: Layout, const LEN: usize> {
    positions: Vec<T::Placed>,
}

impl<T, const LEN: usize> Layout for Many<T, LEN>
where
    T: Layout,
{
    type Placed = Self;

    fn place_with(alloc: &mut Choreographer) -> Self::Placed {
        let mut positions = Vec::<T::Placed>::with_capacity(LEN);
        positions.resize_with(LEN, || T::place_with(alloc));
        Self { positions }
    }

    type Allocated<M: super::Manager> = Vec<T::Allocated<M>>;

    fn allocate<M: super::Manager>(backend: &mut M, placed: Self::Placed) -> Self::Allocated<M> {
        placed
            .positions
            .into_iter()
            .map(|placed| T::allocate(backend, placed))
            .collect::<Vec<_>>()
    }
}
