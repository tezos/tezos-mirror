// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024-2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

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
        super::Cell::bind(region)
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
    type Allocated<M: super::ManagerBase> = super::Cells<T, LEN, M>;

    fn allocate<M: super::ManagerAlloc>(backend: &mut M) -> Self::Allocated<M> {
        let region = backend.allocate_region(<[T; LEN]>::DEFAULT);
        super::Cells::bind(region)
    }
}

/// Layout for a fixed number of bytes, readable as types implementing [`super::elems::Elem`].
pub struct DynArray<const LEN: usize> {}

impl<const LEN: usize> Layout for DynArray<LEN> {
    type Allocated<M: super::ManagerBase> = super::DynCells<LEN, M>;

    fn allocate<M: super::ManagerAlloc>(backend: &mut M) -> Self::Allocated<M> {
        let region = backend.allocate_dyn_region();
        super::DynCells::bind(region)
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
        $($field_vis:vis $field_name:ident: $cell_repr:ty),+
        $( , )?
    }) => {
        paste::paste! {
            #[derive(serde::Deserialize, serde::Serialize, Debug, Clone, PartialEq, Eq)]
            $vis struct [<$layout_t F>]<
                $(
                    [<$field_name:upper>]
                ),+
            > {
                $(
                    $field_vis $field_name: [<$field_name:upper>]
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

            use $crate::state_backend::proof_backend::merkle::{
                AccessInfo, AccessInfoAggregatable, MerkleTree,
            };

            impl <
                $(
                    [<$field_name:upper>]: AccessInfoAggregatable + serde::Serialize
                ),+
            > AccessInfoAggregatable for [<$layout_t F>]<
                $(
                    [<$field_name:upper>]
                ),+
            > {
                fn aggregate_access_info(&self) -> AccessInfo {
                    let children = [
                        $(
                            self.$field_name.aggregate_access_info()
                        ),+
                    ];
                    AccessInfo::fold(&children)
                }
            }

            impl $crate::state_backend::CommitmentLayout for $layout_t {
                fn state_hash<M: $crate::state_backend::ManagerRead + $crate::state_backend::ManagerSerialise>(state: AllocatedOf<Self, M>
                ) -> Result<$crate::storage::Hash, $crate::storage::HashError> {
                    $crate::storage::Hash::blake2b_hash(state)
                }
            }

            impl $crate::state_backend::ProofLayout for $layout_t {
                fn to_merkle_tree(
                    state: $crate::state_backend::RefProofGenOwnedAlloc<Self>,
                ) -> Result<$crate::state_backend::proof_backend::merkle::MerkleTree, $crate::storage::HashError> {
                    let serialised = $crate::storage::binary::serialise(&state)?;
                    MerkleTree::make_merkle_leaf(serialised, state.aggregate_access_info())
                }

                fn from_proof(
                    proof: $crate::state_backend::ProofTree,
                ) -> Result<Self::Allocated<$crate::state_backend::verify_backend::Verifier>, $crate::state_backend::FromProofError> {
                    if let $crate::state_backend::ProofTree::Present(proof) = proof {
                        match proof {
                            $crate::state_backend::proof_backend::tree::Tree::Leaf(_) => {
                                Err($crate::state_backend::FromProofError::UnexpectedLeaf)
                            }

                            $crate::state_backend::proof_backend::tree::Tree::Node(branches) => {
                                let mut branches = branches.iter();

                                let expected_branches = 0 $(
                                    + { let $field_name = 1; $field_name }
                                )+;
                                let successful_branches = 0;

                                $(
                                    let $field_name = branches.next().ok_or($crate::state_backend::FromProofError::BadNumberOfBranches {
                                        got: successful_branches,
                                        expected: expected_branches,
                                    })?;
                                    let $field_name = <$cell_repr as $crate::state_backend::ProofLayout>::from_proof($crate::state_backend::ProofTree::Present($field_name))?;
                                    let successful_branches = successful_branches + 1;
                                )+

                                if branches.last().is_some() {
                                    // There were more branches, this is bad.
                                    return Err($crate::state_backend::FromProofError::BadNumberOfBranches {
                                        got: successful_branches + 1,
                                        expected: expected_branches,
                                    });
                                }

                                Ok(
                                    Self::Allocated {
                                        $(
                                            $field_name
                                        ),+
                                    }
                                )
                            }
                        }
                    } else {
                        Ok(
                            Self::Allocated {
                                $(
                                    $field_name: <$cell_repr as $crate::state_backend::ProofLayout>::from_proof($crate::state_backend::ProofTree::Absent)?
                                ),+
                            }
                        )
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
