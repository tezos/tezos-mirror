// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
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
//! # [Locations] placement through a [Choreographer]
//!
//! Once a [Layout] has been defined, the [Choreographer] can be used to
//! generate static offsets into the backend storage in the form of [Locations].
//! All offsets shall be generated in a deterministic way.
//!
//! All offsets when added to the state storage root address shall also build
//! correctly aligned addresses as long as the state backend storage has been
//! aligned with the requirements requested in [Placed].
//!
//! # Allocation of [Regions] using a [Manager]
//!
//! A [Manager], given [Locations], assigns [Regions] in the backend. Those
//! [Regions] are then used by the state type to manipulate the backend storage
//! where the state ultimately exists.
//!
//! [Regions]: Region
//! [Layouts]: Layout
//! [Locations]: Location

mod alloc;
mod effects;
mod elems;
mod enums;
mod layout;
pub mod memory_backend;
pub mod owned_backend;
mod region;

pub use alloc::*;
pub use effects::*;
pub use elems::*;
pub use enums::*;
pub use layout::*;
pub use region::*;

/// Manager of the state backend storage
pub trait ManagerBase {
    /// Region that has been allocated in the state storage
    type Region<E: Elem, const LEN: usize>;

    /// Dynamic region represents a fixed-sized byte vector that has been allocated in the state storage
    type DynRegion<const LEN: usize>;
}

/// Manager with allocation capabilities
pub trait ManagerAlloc: ManagerBase {
    /// Allocate a region in the state storage.
    fn allocate_region<E: Elem, const LEN: usize>(
        &mut self,
        loc: Location<[E; LEN]>,
    ) -> Self::Region<E, LEN>;

    /// Allocate a dynamic region in the state storage.
    fn allocate_dyn_region<const LEN: usize>(
        &mut self,
        loc: Location<[u8; LEN]>,
    ) -> Self::DynRegion<LEN>;
}

/// Manager with read capabilities
pub trait ManagerRead: ManagerBase {
    /// Read an element in the region.
    fn region_read<E: Elem, const LEN: usize>(region: &Self::Region<E, LEN>, index: usize) -> E;

    /// Read all elements in the region.
    fn region_read_all<E: Elem, const LEN: usize>(region: &Self::Region<E, LEN>) -> Vec<E>;

    /// Read `buffer.len()` elements from the region, starting at `offset`.
    fn region_read_some<E: Elem, const LEN: usize>(
        region: &Self::Region<E, LEN>,
        offset: usize,
        buffer: &mut [E],
    );

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
}

/// Manager with write capabilities
pub trait ManagerWrite: ManagerBase {
    /// Update an element in the region.
    fn region_write<E: Elem, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        index: usize,
        value: E,
    );

    /// Update all elements in the region.
    fn region_write_all<E: Elem, const LEN: usize>(region: &mut Self::Region<E, LEN>, value: &[E]);

    /// Update a subset of elements in the region starting at `index`.
    fn region_write_some<E: Elem, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        index: usize,
        buffer: &[E],
    );

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
}

/// Manager with capabilities that require both read and write
pub trait ManagerReadWrite: ManagerRead + ManagerWrite {
    /// Update the element in the region and return the previous value.
    fn region_replace<E: Elem, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        index: usize,
        value: E,
    ) -> E;
}

/// State backend with manager
pub trait BackendManagement {
    /// Backend manager
    type Manager<'backend>: ManagerReadWrite;

    /// Backend manager for readonly operations
    type ManagerRO<'backend>: ManagerRead;
}

/// State backend storage
pub trait Backend: BackendManagement + Sized {
    /// Structural representation of the states that this backend supports
    type Layout: layout::Layout;

    /// Allocate regions for the given layout placement.
    fn allocate(
        &mut self,
        placed: PlacedOf<Self::Layout>,
    ) -> AllocatedOf<Self::Layout, Self::Manager<'_>>;

    /// Allocate regions for the given layout placement.
    fn allocate_ro(
        &self,
        placed: PlacedOf<Self::Layout>,
    ) -> AllocatedOf<Self::Layout, Self::ManagerRO<'_>>;
}

/// Like [Backend], but with all its state accessible.
pub trait BackendFull: Backend {
    /// Obtain a reference to the storage that backs the given location.
    fn region<E: Elem, const LEN: usize>(&self, loc: &Location<[E; LEN]>) -> &[u8];

    /// Obtain a mutable reference to the storage that backs the given location.
    fn region_mut<E: Elem, const LEN: usize>(&mut self, loc: &Location<[E; LEN]>) -> &mut [u8];
}

/// Manager wrapper around `M` whose regions are immutable references to regions of `M`
pub struct Ref<'backend, M>(std::marker::PhantomData<&'backend M>);

impl<'backend, M: ManagerBase> ManagerBase for Ref<'backend, M> {
    type Region<E: Elem, const LEN: usize> = &'backend M::Region<E, LEN>;

    type DynRegion<const LEN: usize> = &'backend M::DynRegion<LEN>;
}

pub mod test_helpers {
    use super::{BackendFull, Layout};
    use std::fmt;

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

                inner::<$crate::state_backend::memory_backend::test_helpers::InMemoryBackendFactory>();
            }
        };
    }

    /// This lets you construct backends for any layout.
    pub trait TestBackendFactory {
        type Backend<L: Layout>: BackendFull<Layout = L> + Clone + fmt::Debug + Eq;

        /// Construct a backend for the given layout `L`.
        fn new<L: Layout>() -> Self::Backend<L>;
    }
}

#[cfg(test)]
pub mod tests {
    use super::{test_helpers::TestBackendFactory, *};
    use crate::backend_test;
    use rand::Fill;
    use std::marker::PhantomData;

    /// Fill the backend with random data.
    pub fn randomise_backend<B: BackendFull>(backend: &mut B) {
        struct Randomiser<'a, B> {
            backend: &'a mut B,
        }

        impl<'a, B> ManagerBase for Randomiser<'a, B> {
            type Region<E: Elem, const LEN: usize> = DummyRegion<E>;

            type DynRegion<const LEN: usize> = DummyRegion<u8>;
        }

        impl<'a, B: BackendFull> ManagerAlloc for Randomiser<'a, B> {
            fn allocate_region<E: Elem, const LEN: usize>(
                &mut self,
                loc: Location<[E; LEN]>,
            ) -> Self::Region<E, LEN> {
                let region = self.backend.region_mut(&loc);
                region.try_fill(&mut rand::thread_rng()).unwrap();
                DummyRegion(PhantomData)
            }

            fn allocate_dyn_region<const LEN: usize>(
                &mut self,
                loc: Location<[u8; LEN]>,
            ) -> Self::DynRegion<LEN> {
                self.allocate_region(loc)
            }
        }

        B::Layout::allocate(
            &mut Randomiser { backend },
            B::Layout::placed().into_location(),
        );
    }

    /// Construct the manager for a given backend lifetime `'a`, a test backend
    /// factory `F` and a specific layout `L`.
    pub type ManagerFor<'a, F, L> =
        <<F as TestBackendFactory>::Backend<L> as BackendManagement>::Manager<'a>;

    /// Dummy region that does nothing
    struct DummyRegion<E>(PhantomData<E>);

    /// Run `f` twice against two different randomised backends and see if the
    /// resulting backend state is the same afterwards.
    pub fn test_determinism<F: TestBackendFactory, L: Layout, T>(f: T)
    where
        T: Fn(AllocatedOf<L, ManagerFor<'_, F, L>>),
    {
        let mut backend1 = crate::create_backend!(L, F);
        randomise_backend(&mut backend1);

        let mut backend2 = crate::create_backend!(L, F);
        randomise_backend(&mut backend2);

        // Run the procedure against both backends.
        f(backend1.allocate(L::placed().into_location()));
        f(backend2.allocate(L::placed().into_location()));

        assert_eq!(backend1, backend2);
    }

    /// Given a `StateLayout` and a [`TestBackendFactory`] type,
    /// create the backend for that layout.
    #[macro_export]
    macro_rules! create_backend {
        ($StateLayout:ty, $Factory:ty) => {
            <$Factory as $crate::state_backend::test_helpers::TestBackendFactory>::new::<$StateLayout>()
        };
    }

    /// Given a `State<M: Manager>`, optionally its `StateLayout`,
    /// a [`TestBackendFactory`] type, a `backend` created with `create_backend!` macro,
    /// create the location and return the created `State<M>`.
    #[macro_export]
    macro_rules! create_state {
        // For an extra generic in the state (MachineState for example)
        ($State:tt, $StateLayout:ty, $Factory:ty, $backend:ident $(, $ExtraGenerics:ty)*) => {
            {
                use $crate::state_backend::{Backend, BackendManagement, Layout};
                let loc = <$StateLayout>::placed().into_location();
                let new_state =
                    $State::<
                        $($ExtraGenerics,)*
                        <<$Factory as $crate::state_backend::test_helpers::TestBackendFactory>::Backend<$StateLayout> as BackendManagement>::Manager<'_>
                    >::bind(
                        $backend.allocate(loc),
                    );

                new_state
            }
        };

        ($State:tt, $StateLayout:ty, $Factory:ty, $backend:ident) => {
            {
                use $crate::state_backend::{Backend, BackendManagement, Layout};
                let loc = <$StateLayout>::placed().into_location();
                let new_state =
                    $State::<<<$Factory as $crate::state_backend::test_helpers::TestBackendFactory>::Backend<$StateLayout> as BackendManagement>::Manager<'_>>::bind(
                        $backend.allocate(loc),
                    );

                new_state
            }
        };

        ($State:tt, $Factory:ty, $backend:ident) => {
            create_state!($State, paste::paste!([<$State Layout>]), $Factory, $backend)
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

        let mut backend = create_backend!(ExampleLayout, F);

        let (first_loc, second_loc) = ExampleLayout::placed().into_location();
        let first_loc = first_loc.as_array();

        let first_offset = first_loc.offset();
        assert_eq!(first_offset, 0);

        let second_offset = second_loc.offset();
        assert_eq!(second_offset, 8);

        let first_value: u64 = rand::random();
        let second_value: [u32; 4] = rand::random();

        {
            let mut instance =
                Example::bind(backend.allocate(ExampleLayout::placed().into_location()));

            instance.first.write(first_value);
            assert_eq!(instance.first.read(), first_value);

            instance.second.write_all(&second_value);
            assert_eq!(instance.second.read_all(), second_value);
        }

        let first_value_read = u64::from_le_bytes(backend.region(&first_loc).try_into().unwrap());
        assert_eq!(first_value_read, first_value);

        let second_value_read = unsafe {
            backend
                .region(&second_loc)
                .as_ptr()
                .cast::<[u32; 4]>()
                .read()
                .map(u32::from_le)
        };
        assert_eq!(second_value_read, second_value);
    });
}
