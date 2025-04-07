// SPDX-FileCopyrightText: 2023-2025 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024-2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Generic state backends
//!
//! # Layouts
//!
//! [Layouts] are structural descriptions of state types. We use types to
//! describe [Layouts] as statically as possible. This shall help us in writing
//! tests that ensure we don't accidentally change the layout of the machine
//! state.
//!
//! ## Example
//!
//! Consider the following state.
//!
//! ```
//! struct MyState {
//!     foo: u64,
//!     bar: [u8; 1024],
//!     qux: u16,
//! }
//! ```
//!
//! We can describe the layout of this state using the following type.
//!
//! ```
//! use octez_riscv::state_backend::{Atom, Array};
//!
//! type MyStateLayout = (
//!     Atom<u64>,
//!     Array<u8, 1024>,
//!     Atom<u16>,
//! );
//! ```
//!
//! # Managers
//!
//! Different backends have different capabilities and they are described as `Manager<Capability>`.
//! Some of these capabilities are:
//! - [ManagerBase]
//! - [ManagerAlloc]
//! - [ManagerRead]
//! - [ManagerWrite]
//! - [ManagerReadWrite]
//!
//! # Backends
//!
//! Backends are ZST implementing these traits.
//! The main difference between them is the top-level functionality it provides
//! and management of the underlying state memory.
//!
//! These backends can be:
//!
//! - [Owned]
//!     Backend which has the full state allocated in memory. It can execute one step
//!     or multiple steps at a time faster.
//! - [Verifier]
//!     Backend capable of partially allocating a state and verify a given proof.
//!     Needs to be light on memory usage since it runs in the protocol.
//! - [ProofGen]
//!     Backend capable of generating a proof for running one step.
//! - [Ref]
//!     Helper backend to wrap another backend through a reference to it.
//!
//! [Layouts]: layout::Layout
//! [Owned]: owned_backend::Owned
//! [Verifier]: verify_backend::Verifier
//! [ProofGen]: proof_backend::ProofGen

mod commitment_layout;
mod effects;
mod elems;
pub mod hash;
mod layout;
pub mod owned_backend;
pub mod proof_backend;
mod proof_layout;
mod region;
mod trans;
pub mod verify_backend;

pub use commitment_layout::*;
pub use effects::*;
pub use elems::*;
pub use layout::*;
pub use proof_layout::*;
pub use region::*;
pub use trans::*;

/// An enriched value may be stored in a [`ManagerBase::EnrichedCell`].
///
/// This allows a value to have an additional, derived, value attached - that may be expensive
/// to derive lazily.
///
/// This derived value does not form part of any stored state/commitments.
pub trait EnrichedValue: 'static {
    type E: 'static;
    type D<M: ManagerBase>;
}

/// Specifies that there exists a path to derive `V::D` from `&V::E`,
/// for a given manager.
pub trait EnrichedValueLinked<M: ManagerBase>: EnrichedValue {
    /// Construct the derived value from the stored value, maps to
    /// the `From` trait by default.
    fn derive(v: &Self::E) -> Self::D<M>;
}

impl<Value, M: ManagerBase> EnrichedValueLinked<M> for Value
where
    Value: EnrichedValue,
    Value::D<M>: for<'a> From<&'a Value::E>,
{
    fn derive(v: &Self::E) -> Self::D<M> {
        v.into()
    }
}

/// Manager of the state backend storage
pub trait ManagerBase: Sized {
    /// Region that has been allocated in the state storage
    type Region<E: 'static, const LEN: usize>;

    /// Dynamic region represents a fixed-sized byte vector that has been allocated in the state storage
    type DynRegion<const LEN: usize>;

    /// An [enriched] value may have a derived value attached.
    ///
    /// [enriched]: EnrichedValue
    type EnrichedCell<V: EnrichedValue>;

    /// The root manager may either be itself, or occassionally the manager that this manager
    /// wraps.
    ///
    /// For example, the [`Ref`] backend is often use to wrap the [`Owned`] backend to gain access
    /// to its regions. In this case, the root manager would be the owned backend.
    ///
    /// [`Owned`]: owned_backend::Owned
    type ManagerRoot: ManagerBase;
}

/// Manager with allocation capabilities
///
/// Any `ManagerAlloc` inherently has read & write capabilities,
/// since the manager creates the values on the first allocation.
pub trait ManagerAlloc: 'static + ManagerReadWrite {
    /// Allocate a region in the state storage.
    fn allocate_region<E, const LEN: usize>(
        &mut self,
        init_value: [E; LEN],
    ) -> Self::Region<E, LEN>;

    /// Allocate a dynamic region in the state storage.
    fn allocate_dyn_region<const LEN: usize>(&mut self) -> Self::DynRegion<LEN>;

    /// Allocate an enriched cell.
    fn allocate_enriched_cell<V>(&mut self, init_value: V::E) -> Self::EnrichedCell<V>
    where
        V: EnrichedValueLinked<Self>;
}

/// Manager with read capabilities
pub trait ManagerRead: ManagerBase {
    /// Read an element in the region.
    fn region_read<E: Copy, const LEN: usize>(region: &Self::Region<E, LEN>, index: usize) -> E;

    /// Obtain a reference to an element in the region.
    fn region_ref<E: 'static, const LEN: usize>(region: &Self::Region<E, LEN>, index: usize) -> &E;

    /// Read all elements in the region.
    fn region_read_all<E: Copy, const LEN: usize>(region: &Self::Region<E, LEN>) -> Vec<E>;

    /// Read an element in the region. `address` is in bytes.
    fn dyn_region_read<E: Elem, const LEN: usize>(
        region: &Self::DynRegion<LEN>,
        address: usize,
    ) -> E;

    /// Read elements from the region. `address` is in bytes.
    fn dyn_region_read_all<E: Elem, const LEN: usize>(
        region: &Self::DynRegion<LEN>,
        address: usize,
        values: &mut [E],
    );

    /// Read the value contained in the enriched cell.
    fn enriched_cell_read_stored<V>(cell: &Self::EnrichedCell<V>) -> V::E
    where
        V: EnrichedValue,
        V::E: Copy;

    /// Read the derived value of the enriched cell.
    fn enriched_cell_read_derived<V>(cell: &Self::EnrichedCell<V>) -> V::D<Self::ManagerRoot>
    where
        V: EnrichedValueLinked<Self::ManagerRoot>,
        V::D<Self::ManagerRoot>: Copy;

    /// Obtain a reference to the value contained in the enriched cell.
    fn enriched_cell_ref_stored<V>(cell: &Self::EnrichedCell<V>) -> &V::E
    where
        V: EnrichedValue;
}

/// Manager with write capabilities
pub trait ManagerWrite: ManagerBase<ManagerRoot = Self> {
    /// Update an element in the region.
    fn region_write<E: 'static, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        index: usize,
        value: E,
    );

    /// Update all elements in the region.
    fn region_write_all<E: Copy, const LEN: usize>(region: &mut Self::Region<E, LEN>, value: &[E]);

    /// Update an element in the region. `address` is in bytes.
    fn dyn_region_write<E: Elem, const LEN: usize>(
        region: &mut Self::DynRegion<LEN>,
        address: usize,
        value: E,
    );

    /// Update multiple elements in the region. `address` is in bytes.
    fn dyn_region_write_all<E: Elem, const LEN: usize>(
        region: &mut Self::DynRegion<LEN>,
        address: usize,
        values: &[E],
    );

    /// Update the value contained in an enriched cell. The derived value will be recalculated.
    fn enriched_cell_write<V>(cell: &mut Self::EnrichedCell<V>, value: V::E)
    where
        V: EnrichedValueLinked<Self>;
}

/// Manager with capabilities that require both read and write
pub trait ManagerReadWrite: ManagerRead + ManagerWrite {
    /// Update the element in the region and return the previous value.
    fn region_replace<E: Copy, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        index: usize,
        value: E,
    ) -> E;
}

/// Manager with the ability to serialise regions
pub trait ManagerSerialise: ManagerRead {
    /// Serialise the contents of the region.
    fn serialise_region<E: serde::Serialize, const LEN: usize, S: serde::Serializer>(
        region: &Self::Region<E, LEN>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>;

    /// Serialise the contents of the dynamic region.
    fn serialise_dyn_region<const LEN: usize, S: serde::Serializer>(
        region: &Self::DynRegion<LEN>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>;

    /// Serialise the contents of the enriched cell.
    fn serialise_enriched_cell<V, S>(
        cell: &Self::EnrichedCell<V>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        V: EnrichedValue,
        V::E: serde::Serialize,
        S: serde::Serializer;
}

/// Manager with the ability to deserialise regions
pub trait ManagerDeserialise: ManagerBase {
    /// Deserialise a region.
    fn deserialise_region<
        'de,
        E: serde::Deserialize<'de>,
        const LEN: usize,
        D: serde::Deserializer<'de>,
    >(
        deserializer: D,
    ) -> Result<Self::Region<E, LEN>, D::Error>;

    /// Deserialise the dyanmic region.
    fn deserialise_dyn_region<'de, const LEN: usize, D: serde::Deserializer<'de>>(
        deserializer: D,
    ) -> Result<Self::DynRegion<LEN>, D::Error>;

    /// Deserialise an enriched cell.
    fn deserialise_enriched_cell<'de, V, D: serde::Deserializer<'de>>(
        deserializer: D,
    ) -> Result<Self::EnrichedCell<V>, D::Error>
    where
        V: EnrichedValueLinked<Self>,
        V::E: serde::Deserialize<'de>;
}

/// Manager with the ability to clone regions
pub trait ManagerClone: ManagerBase {
    /// Clone the region.
    fn clone_region<E: Copy, const LEN: usize>(
        region: &Self::Region<E, LEN>,
    ) -> Self::Region<E, LEN>;

    /// Clone the dynamic region.
    fn clone_dyn_region<const LEN: usize>(region: &Self::DynRegion<LEN>) -> Self::DynRegion<LEN>;

    /// Clone the enriched cell.
    fn clone_enriched_cell<V>(cell: &Self::EnrichedCell<V>) -> Self::EnrichedCell<V>
    where
        V: EnrichedValue,
        V::E: Copy,
        V::D<Self>: Copy;
}

/// Manager wrapper around `M` whose regions are immutable references to regions of `M`
pub struct Ref<'backend, M>(std::marker::PhantomData<fn(&'backend M)>);

impl<'backend, M: ManagerBase> ManagerBase for Ref<'backend, M> {
    type Region<E: 'static, const LEN: usize> = &'backend M::Region<E, LEN>;

    type DynRegion<const LEN: usize> = &'backend M::DynRegion<LEN>;

    type EnrichedCell<V: EnrichedValue> = &'backend M::EnrichedCell<V>;

    type ManagerRoot = M::ManagerRoot;
}

impl<M: ManagerSerialise> ManagerSerialise for Ref<'_, M> {
    fn serialise_region<E: serde::Serialize, const LEN: usize, S: serde::Serializer>(
        region: &Self::Region<E, LEN>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        M::serialise_region(region, serializer)
    }

    fn serialise_dyn_region<const LEN: usize, S: serde::Serializer>(
        region: &Self::DynRegion<LEN>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        M::serialise_dyn_region(region, serializer)
    }

    fn serialise_enriched_cell<V: EnrichedValue, S: serde::Serializer>(
        cell: &Self::EnrichedCell<V>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        V::E: serde::Serialize,
    {
        M::serialise_enriched_cell(cell, serializer)
    }
}

impl<M: ManagerRead> ManagerRead for Ref<'_, M> {
    fn region_read<E: Copy, const LEN: usize>(region: &Self::Region<E, LEN>, index: usize) -> E {
        M::region_read(region, index)
    }

    fn region_ref<E: 'static, const LEN: usize>(region: &Self::Region<E, LEN>, index: usize) -> &E {
        M::region_ref(region, index)
    }

    fn region_read_all<E: Copy, const LEN: usize>(region: &Self::Region<E, LEN>) -> Vec<E> {
        M::region_read_all(region)
    }

    fn dyn_region_read<E: Elem, const LEN: usize>(
        region: &Self::DynRegion<LEN>,
        address: usize,
    ) -> E {
        M::dyn_region_read(region, address)
    }

    fn dyn_region_read_all<E: Elem, const LEN: usize>(
        region: &Self::DynRegion<LEN>,
        address: usize,
        values: &mut [E],
    ) {
        M::dyn_region_read_all(region, address, values)
    }

    fn enriched_cell_read_stored<V>(cell: &Self::EnrichedCell<V>) -> V::E
    where
        V: EnrichedValue,
        V::E: Copy,
    {
        M::enriched_cell_read_stored(cell)
    }

    fn enriched_cell_ref_stored<V>(cell: &Self::EnrichedCell<V>) -> &V::E
    where
        V: EnrichedValue,
    {
        M::enriched_cell_ref_stored(cell)
    }

    fn enriched_cell_read_derived<V: EnrichedValueLinked<Self::ManagerRoot>>(
        cell: &Self::EnrichedCell<V>,
    ) -> V::D<Self::ManagerRoot>
    where
        V::D<Self::ManagerRoot>: Copy,
    {
        M::enriched_cell_read_derived(cell)
    }
}

/// Alias for the allocated structure with references to regions of
/// the [`owned_backend::Owned`] backend
pub type RefOwnedAlloc<'a, L> = AllocatedOf<L, Ref<'a, owned_backend::Owned>>;

/// Alias for the allocated structure with references to a proof-generating backend
pub type RefProofGenOwnedAlloc<'a, 'b, L> =
    AllocatedOf<L, Ref<'a, proof_backend::ProofGen<Ref<'b, owned_backend::Owned>>>>;

#[cfg(test)]
pub(crate) mod test_helpers {
    use super::{
        AllocatedOf, Layout, ManagerClone, ManagerDeserialise, ManagerReadWrite, ManagerSerialise,
    };
    use crate::jit::state_access::JitStateAccess;

    /// Generate a test against all test backends.
    #[macro_export]
    macro_rules! backend_test {
        ( $(#[$m:meta])* $name:ident, $fac_name:ident, $expr:block ) => {
            $(#[$m])*
            #[test]
            fn $name() {
                fn inner<$fac_name: $crate::state_backend::test_helpers::TestBackendFactory>() {
                    $expr
                }

                inner::<$crate::state_backend::owned_backend::test_helpers::OwnedTestBackendFactory>();
            }
        };
    }

    /// This lets you construct backends for any layout.
    pub trait TestBackendFactory {
        /// Manager used in testing
        type Manager: ManagerReadWrite
            + ManagerSerialise
            + ManagerDeserialise
            + ManagerClone
            + JitStateAccess;

        /// Allocate using the test backend manager.
        fn allocate<L: Layout>() -> AllocatedOf<L, Self::Manager>;
    }

    /// Copy the allocated space by serialising and deserialising it.
    pub fn copy_via_serde<L, N, M>(refs: &AllocatedOf<L, N>) -> AllocatedOf<L, M>
    where
        L: Layout,
        N: ManagerSerialise,
        AllocatedOf<L, N>: serde::Serialize,
        M: ManagerDeserialise,
        AllocatedOf<L, M>: serde::de::DeserializeOwned,
    {
        let data = crate::storage::binary::serialise(refs).unwrap();
        crate::storage::binary::deserialise(&data).unwrap()
    }

    /// Assert that two values are different. If they differ, offer a command to run that shows the
    /// structural differences between the values.
    pub fn assert_eq_struct<T>(lhs: &T, rhs: &T)
    where
        T: serde::Serialize + PartialEq,
    {
        if lhs != rhs {
            let (file_lhs, path_lhs) = tempfile::NamedTempFile::new().unwrap().keep().unwrap();
            serde_json::to_writer_pretty(file_lhs, lhs).unwrap();
            eprintln!("Lhs is located at {}", path_lhs.display());

            let (file_rhs, path_rhs) = tempfile::NamedTempFile::new().unwrap().keep().unwrap();
            serde_json::to_writer_pretty(file_rhs, rhs).unwrap();
            eprintln!("Rhs is located at {}", path_rhs.display());

            eprintln!("Run the following to diff them:");
            eprintln!("jd {} {}", path_lhs.display(), path_rhs.display());

            panic!("Assertion failed: values are different");
        }
    }
}

#[cfg(test)]
pub mod tests {
    use self::owned_backend::Owned;
    use super::*;
    use crate::backend_test;

    /// Run `f` twice against two different randomised backends and see if the
    /// resulting backend state is the same afterwards.
    pub fn test_determinism<L, T>(_f: T)
    where
        L: Layout,
        T: Fn(AllocatedOf<L, Owned>),
    {
        // TODO: RV-46: This test will be re-introduced but customised for initialisation testing.
    }

    /// Given a `State<M: Manager>`, optionally its `StateLayout`,
    /// a [`TestBackendFactory`] type, a `backend` created with `create_backend!` macro,
    /// create the location and return the created `State<M>`.
    #[macro_export]
    macro_rules! create_state {
        // For an extra generic in the state (MachineState for example)
        ($State:tt, $StateLayout:ty, $Factory:ty $(, $ExtraGenerics:ty)* $(, || $arg: expr)*) => {
            {
                let new_state =
                    $State::<
                        $($ExtraGenerics,)*
                        <$Factory as $crate::state_backend::test_helpers::TestBackendFactory>::Manager,
                    >::bind(
                        <$Factory as $crate::state_backend::test_helpers::TestBackendFactory>::allocate::<$StateLayout>(),
                        $($arg,)*
                    );

                new_state
            }
        };

        ($State:tt, $Factory:ty) => {
            create_state!($State, paste::paste!([<$State Layout>]), $Factory)
        };
    }

    backend_test!(test_example, F, {
        struct Example<M: ManagerBase> {
            first: Cell<u64, M>,
            second: Cells<u32, 4, M>,
        }

        type ExampleLayout = (Atom<u64>, Array<u32, 4>);

        impl<M: ManagerBase> Example<M> {
            fn bind(space: AllocatedOf<ExampleLayout, M>) -> Self {
                Example {
                    first: space.0,
                    second: space.1,
                }
            }
        }

        let first_value: u64 = rand::random();
        let second_value: [u32; 4] = rand::random();

        let mut instance = create_state!(Example, ExampleLayout, F);

        instance.first.write(first_value);
        assert_eq!(instance.first.read(), first_value);

        instance.second.write_all(&second_value);
        assert_eq!(instance.second.read_all(), second_value);

        let first_value_read = u64::from_le_bytes(
            bincode::serialize(&instance.first.struct_ref::<FnManagerIdent>())
                .unwrap()
                .try_into()
                .unwrap(),
        );
        assert_eq!(first_value_read, first_value);

        let second_value_read = unsafe {
            let data = bincode::serialize(&instance.second.struct_ref::<FnManagerIdent>()).unwrap();
            data.as_ptr().cast::<[u32; 4]>().read().map(u32::from_le)
        };
        assert_eq!(second_value_read, second_value);
    });
}
