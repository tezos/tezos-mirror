// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024-2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::marker::PhantomData;

use crate::default::ConstDefault;

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
/// struct_layout! {
///     pub struct ExampleLayout {
///         satp_ppn: Atom<CSRRepr>,
///         mode: Atom<u8>,
///         cached: Atom<bool>,
///     }
/// }
/// ```
#[macro_export]
macro_rules! struct_layout {
    (
        $vis:vis struct $layout_t:ident $(< $($param:ident),+ >)? {
            $($field_vis:vis $field_name:ident: $cell_repr:ty),+
            $(,)?
        }
    ) => {
        paste::paste! {
            #[derive(serde::Deserialize, serde::Serialize, Debug, Clone, PartialEq, Eq)]
            $vis struct [<$layout_t F>]<
                $(
                    [<$field_name:camel>]
                ),+
            > {
                $(
                    $field_vis $field_name: [<$field_name:camel>]
                ),+
            }

            $vis type $layout_t $(< $($param),+ >)? = [<$layout_t F>]<
                $(
                    $cell_repr
                ),+
            >;

            impl <
                $(
                    [<$field_name:camel>]: $crate::state_backend::Layout
                ),+
            > $crate::state_backend::Layout for [<$layout_t F>]<
                $(
                    [<$field_name:camel>]
                ),+
            > {
                type Allocated<M: $crate::state_backend::ManagerBase> = [<$layout_t F>]<
                    $(
                        <[<$field_name:camel>] as $crate::state_backend::Layout>::Allocated<M>
                    ),+
                >;

                #[inline]
                fn allocate<M: $crate::state_backend::ManagerAlloc>(
                    backend: &mut M,
                ) -> Self::Allocated<M> {
                    Self::Allocated {
                        $($field_name: <[<$field_name:camel>] as $crate::state_backend::Layout>::allocate(
                            backend
                        )),+
                    }
                }
            }

            impl <
                $(
                    [<$field_name:camel>]: $crate::state_backend::proof_backend::merkle::AccessInfoAggregatable + serde::Serialize
                ),+
            > $crate::state_backend::proof_backend::merkle::AccessInfoAggregatable for [<$layout_t F>]<
                $(
                    [<$field_name:camel>]
                ),+
            > {
                #[inline]
                fn aggregate_access_info(&self) -> bool {
                    let children = [
                        $(
                            self.$field_name.aggregate_access_info()
                        ),+
                    ];
                    children.iter().any(|&x| x)
                }
            }

            impl <
                $(
                    [<$field_name:camel>]: $crate::state_backend::CommitmentLayout
                ),+
            > $crate::state_backend::CommitmentLayout for [<$layout_t F>]<
                $(
                    [<$field_name:camel>]
                ),+
            > {
                #[inline]
                fn state_hash<M: $crate::state_backend::ManagerRead + $crate::state_backend::ManagerSerialise>(
                    state: AllocatedOf<Self, M>
                ) -> Result<$crate::storage::Hash, $crate::storage::HashError> {
                    $crate::storage::Hash::combine(&[
                        $(
                            [<$field_name:camel>]::state_hash(state.$field_name)?
                        ),+
                    ])
                }
            }

            impl <
                $(
                    [<$field_name:camel>]: $crate::state_backend::ProofLayout
                ),+
            > $crate::state_backend::ProofLayout for [<$layout_t F>]<
                $(
                    [<$field_name:camel>]
                ),+
            > {
                #[inline]
                fn to_merkle_tree(
                    state: $crate::state_backend::RefProofGenOwnedAlloc<Self>,
                ) -> Result<$crate::state_backend::proof_backend::merkle::MerkleTree, $crate::storage::HashError> {
                    $crate::state_backend::proof_backend::merkle::MerkleTree::make_merkle_node(
                        vec![
                            $(
                                [<$field_name:camel>]::to_merkle_tree(state.$field_name)?
                            ),+
                        ]
                    )
                }

                #[inline]
                fn from_proof(
                    proof: $crate::state_backend::ProofTree,
                ) -> Result<Self::Allocated<$crate::state_backend::verify_backend::Verifier>, $crate::state_backend::FromProofError> {
                    let [ $($field_name),+ ] = *proof.into_branches()?;
                    Ok(Self::Allocated {
                        $(
                            $field_name: [<$field_name:camel>]::from_proof($field_name)?
                        ),+
                    })
                }

                fn partial_state_hash(
                    _state: $crate::state_backend::RefVerifierAlloc<Self>,
                    _proof: $crate::state_backend::ProofTree,
                ) -> Result<$crate::storage::Hash, $crate::state_backend::PartialHashError> {
                    // TODO RV-503: Compute the final state hash of a Verifier-bound struct layout
                    // This does not currently limit obtaining final hashes of Verifier states
                    // because the only struct layout in the PVM state, `MStatusLayout`,
                    // is handled by the implementation of `partial_state_hash` for `CSRValuesLayout`.
                    todo!("Hashing of partial generic struct layouts not yet supported")
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
    use crate::backend_test;
    use crate::default::ConstDefault;
    use crate::state_backend::CommitmentLayout;
    use crate::state_backend::FnManagerIdent;
    use crate::state_backend::ProofLayout;
    use crate::state_backend::ProofPart;
    use crate::state_backend::owned_backend::Owned;
    use crate::state_backend::proof_backend::ProofWrapper;
    use crate::state_backend::verify_backend::handle_stepper_panics;

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

    #[test]
    fn test_struct_layout() {
        struct_layout! {
            pub struct Foo {
                bar: Atom<u64>,
                qux: Array<u8, 64>,
            }
        }

        fn inner(bar: u64, qux: [u8; 64]) {
            let mut foo = Owned::allocate::<Foo>();

            foo.bar.write(bar);
            foo.qux.write_all(&qux);

            // Obtain the state hash
            let refs = FooF {
                bar: foo.bar.struct_ref::<FnManagerIdent>(),
                qux: foo.qux.struct_ref::<FnManagerIdent>(),
            };
            let hash = Foo::state_hash(refs).unwrap();

            // Obtain the Merkle tree
            let mut proof_foo = FooF {
                bar: foo.bar.struct_ref::<ProofWrapper>(),
                qux: foo.qux.struct_ref::<ProofWrapper>(),
            };
            let proof_foo_refs = FooF {
                bar: proof_foo.bar.struct_ref::<FnManagerIdent>(),
                qux: proof_foo.qux.struct_ref::<FnManagerIdent>(),
            };

            let tree = Foo::to_merkle_tree(proof_foo_refs).unwrap();
            let tree_root_hash = tree.root_hash();
            assert_eq!(hash, tree_root_hash);

            // Modify the values so they appear in the proof
            proof_foo.bar.write(bar.wrapping_add(1));
            proof_foo.qux.write_all(&qux.map(|x| x.wrapping_add(1)));

            // Obtain the Merkle tree, again, to make sure nothing changed
            let proof_foo_refs = FooF {
                bar: proof_foo.bar.struct_ref::<FnManagerIdent>(),
                qux: proof_foo.qux.struct_ref::<FnManagerIdent>(),
            };

            let tree = Foo::to_merkle_tree(proof_foo_refs).unwrap();
            let tree_root_hash = tree.root_hash();
            assert_eq!(hash, tree_root_hash);

            // Produce a proof
            let proof = tree.to_merkle_proof().unwrap();
            let proof_hash = proof.root_hash().unwrap();
            assert_eq!(hash, proof_hash);

            // Verify the proof
            handle_stepper_panics(|| {
                let verify_foo = Foo::from_proof(ProofPart::Present(&proof)).unwrap();
                assert_eq!(bar, verify_foo.bar.read());
                assert_eq!(qux, verify_foo.qux.read_all().as_slice());
            })
            .unwrap();
        }

        proptest::proptest!(|(bar: u64, qux: [u8; 64])| {
            inner(bar, qux);
        });
    }
}
