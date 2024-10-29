// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{Cell, Cells};
use crate::default::ConstDefault;
use std::marker::PhantomData;

/// Structural description of a state type
pub trait Layout {
    /// Representation of the allocated regions in the state backend
    type Allocated<M: super::ManagerBase>;

    /// Allocate regions in the given state backend.
    fn allocate<M: super::ManagerAlloc>(backend: &mut M) -> Self::Allocated<M>;
}

impl Layout for () {
    type Allocated<M: super::ManagerBase> = ();

    fn allocate<M: super::ManagerAlloc>(_backend: &mut M) -> Self::Allocated<M> {}
}

/// `L::Allocated`
pub type AllocatedOf<L, M> = <L as Layout>::Allocated<M>;

/// Layout for a single value
#[repr(transparent)]
pub struct Atom<T> {
    _pd: PhantomData<T>,
}

impl<T: ConstDefault + 'static> Layout for Atom<T> {
    type Allocated<M: super::ManagerBase> = super::Cell<T, M>;

    fn allocate<M: super::ManagerAlloc>(backend: &mut M) -> Self::Allocated<M> {
        let region = backend.allocate_region([T::DEFAULT; 1]);
        Cell::bind(region)
    }
}

/// Layout for a fixed number of values
#[repr(transparent)]
pub struct Array<T, const LEN: usize> {
    _pd: PhantomData<T>,
}

impl<T: 'static, const LEN: usize> Layout for Array<T, LEN>
where
    [T; LEN]: ConstDefault,
{
    type Allocated<M: super::ManagerBase> = Cells<T, LEN, M>;

    fn allocate<M: super::ManagerAlloc>(backend: &mut M) -> Self::Allocated<M> {
        let region = backend.allocate_region(<[T; LEN]>::DEFAULT);
        Cells::bind(region)
    }
}

/// Usage: Provide a struct with each field holding a layout.
///
/// ```
/// use octez_riscv::state_backend::*;
/// use octez_riscv::machine_state::csregisters::CSRRepr;
/// use octez_riscv::struct_layout;
///
/// struct_layout!(
///     pub struct ExampleLayout {
///         satp_ppn: Atom<CSRRepr>,
///         mode: Atom<u8>,
///         cached: Atom<bool>,
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
            #[derive(serde::Deserialize, serde::Serialize, PartialEq, Eq)]
            $vis struct [<$layout_t F>]<
                $(
                    [<$field_name:upper>]
                ),+
            > {
                $(
                    $field_name: [<$field_name:upper>]
                ),+
            }

            $vis type $layout_t = [<$layout_t F>]<
                $(
                    $cell_repr
                ),+
            >;

            impl $crate::state_backend::Layout for $layout_t {
                type Allocated<M: $crate::state_backend::ManagerBase> = [<$layout_t F>]<
                    $(
                        AllocatedOf<$cell_repr, M>
                    ),+
                >;

                fn allocate<M: $crate::state_backend::ManagerAlloc>(
                    backend: &mut M,
                ) -> Self::Allocated<M> {
                    Self::Allocated {
                        $($field_name: <$cell_repr as $crate::state_backend::Layout>::allocate(
                            backend
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
    type Allocated<M: super::ManagerBase> = (A::Allocated<M>, B::Allocated<M>);

    fn allocate<M: super::ManagerAlloc>(backend: &mut M) -> Self::Allocated<M> {
        (A::allocate(backend), B::allocate(backend))
    }
}

impl<A, B, C> Layout for (A, B, C)
where
    A: Layout,
    B: Layout,
    C: Layout,
{
    type Allocated<M: super::ManagerBase> = (A::Allocated<M>, B::Allocated<M>, C::Allocated<M>);

    fn allocate<M: super::ManagerAlloc>(backend: &mut M) -> Self::Allocated<M> {
        (
            A::allocate(backend),
            B::allocate(backend),
            C::allocate(backend),
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
    type Allocated<M: super::ManagerBase> = (
        A::Allocated<M>,
        B::Allocated<M>,
        C::Allocated<M>,
        D::Allocated<M>,
    );

    fn allocate<M: super::ManagerAlloc>(backend: &mut M) -> Self::Allocated<M> {
        (
            A::allocate(backend),
            B::allocate(backend),
            C::allocate(backend),
            D::allocate(backend),
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
    type Allocated<M: super::ManagerBase> = (
        A::Allocated<M>,
        B::Allocated<M>,
        C::Allocated<M>,
        D::Allocated<M>,
        E::Allocated<M>,
    );

    fn allocate<M: super::ManagerAlloc>(backend: &mut M) -> Self::Allocated<M> {
        (
            A::allocate(backend),
            B::allocate(backend),
            C::allocate(backend),
            D::allocate(backend),
            E::allocate(backend),
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
    type Allocated<M: super::ManagerBase> = (
        A::Allocated<M>,
        B::Allocated<M>,
        C::Allocated<M>,
        D::Allocated<M>,
        E::Allocated<M>,
        F::Allocated<M>,
    );

    fn allocate<M: super::ManagerAlloc>(backend: &mut M) -> Self::Allocated<M> {
        (
            A::allocate(backend),
            B::allocate(backend),
            C::allocate(backend),
            D::allocate(backend),
            E::allocate(backend),
            F::allocate(backend),
        )
    }
}

impl<T, const LEN: usize> Layout for [T; LEN]
where
    T: Layout,
{
    type Allocated<M: super::ManagerBase> = [T::Allocated<M>; LEN];

    fn allocate<M: super::ManagerAlloc>(backend: &mut M) -> Self::Allocated<M> {
        std::array::from_fn(|_| T::allocate(backend))
    }
}

/// This [`Layout`] is identical to [`[T; LEN]`] but it allows you to choose a very high `LEN`.
pub struct Many<T: Layout, const LEN: usize>(PhantomData<[T; LEN]>);

impl<T, const LEN: usize> Layout for Many<T, LEN>
where
    T: Layout,
{
    type Allocated<M: super::ManagerBase> = Vec<T::Allocated<M>>;

    fn allocate<M: super::ManagerAlloc>(backend: &mut M) -> Self::Allocated<M> {
        let mut space = Vec::with_capacity(LEN);
        space.resize_with(LEN, || T::allocate(backend));
        space
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{backend_test, default::ConstDefault};

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    struct MyFoo(u64);

    impl ConstDefault for MyFoo {
        const DEFAULT: Self = MyFoo(42);
    }

    // Test that the Atom layout initialises the underlying Cell correctly.
    backend_test!(test_cell_init, F, {
        assert_eq!(F::allocate::<Atom<MyFoo>>().read(), MyFoo::DEFAULT);
    });

    // Test that the Array layout initialises the underlying Cells correctly.
    backend_test!(test_cells_init, F, {
        assert_eq!(
            F::allocate::<Array<MyFoo, 1337>>().read_all(),
            [MyFoo::DEFAULT; 1337]
        );
    });
}
