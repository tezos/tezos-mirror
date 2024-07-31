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

pub mod memory_backend;

mod layout;
pub use layout::*;

mod alloc;
pub use alloc::*;

mod region;
pub use region::*;

mod enums;
pub use enums::*;

mod bools;
pub use bools::*;

mod elems;
pub use elems::*;

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

/// Manager with allocation, read, write and replace capabilities
// TODO: RV-157: Remove this trait and use the individual traits instead. Also rename ManagerBase
// back to Manager.
pub trait Manager: ManagerAlloc + ManagerWrite + ManagerRead + ManagerReadWrite {}

impl<All: ManagerAlloc + ManagerWrite + ManagerRead + ManagerReadWrite> Manager for All {}

/// State backend with manager
pub trait BackendManagement {
    /// Backend manager
    type Manager<'backend>: Manager;

    /// Backend manager for readonly operations
    type ManagerRO<'backend>: Manager;
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

    /// Read bytes from the backing storage.
    fn read(&self, index: usize, buffer: &mut [u8]);

    /// Write bytes to the backing storage.
    fn write(&mut self, index: usize, buffer: &[u8]);
}

pub mod test_helpers {
    use super::{Backend, Layout};

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
        type Backend<L: Layout>: Backend<Layout = L> + Clone;

        /// Construct a backend for the given layout `L`.
        fn new<L: Layout>() -> Self::Backend<L>;
    }
}

#[cfg(test)]
pub mod tests {
    use super::{test_helpers::TestBackendFactory, *};
    use crate::backend_test;
    use rand::{Fill, Rng};
    use std::{collections::VecDeque, marker::PhantomData};

    /// Fill the backend with random data.
    pub fn randomise_backend<B: Backend>(backend: &mut B) {
        let len = B::Layout::placed().size();
        let mut rand_data = vec![0u8; len];

        rand_data
            .as_mut_slice()
            .try_fill(&mut rand::thread_rng())
            .unwrap();

        backend.write(0, rand_data.as_slice());
    }

    /// Read the entire backend data.
    pub fn read_backend<B: Backend>(backend: &B) -> Vec<u8> {
        let len = B::Layout::placed().size();
        let mut data = vec![0u8; len];
        backend.read(0, data.as_mut_slice());
        data
    }

    /// Construct the manager for a given backend lifetime `'a`, a test backend
    /// factory `F` and a specific layout `L`.
    pub type ManagerFor<'a, F, L> =
        <<F as TestBackendFactory>::Backend<L> as BackendManagement>::Manager<'a>;

    /// Dummy region that does nothing
    struct DummyRegion<E>(PhantomData<E>);

    /// A tracing [Manager] that only keeps track of a layout's locations.
    struct TraceManager {
        pub regions: VecDeque<(usize, usize)>,
    }

    impl TraceManager {
        /// Trace the locations of `L`.
        fn trace<L: Layout>(placed: L::Placed) -> VecDeque<(usize, usize)> {
            let mut mgr = Self {
                regions: VecDeque::new(),
            };
            L::allocate(&mut mgr, placed);
            mgr.regions
        }
    }

    impl ManagerBase for TraceManager {
        type Region<E: Elem, const LEN: usize> = DummyRegion<E>;

        type DynRegion<const LEN: usize> = DummyRegion<u8>;
    }

    impl ManagerAlloc for TraceManager {
        fn allocate_region<E: Elem, const LEN: usize>(
            &mut self,
            loc: Location<[E; LEN]>,
        ) -> Self::Region<E, LEN> {
            self.regions.push_back((loc.offset(), loc.size()));
            DummyRegion(PhantomData)
        }

        fn allocate_dyn_region<const LEN: usize>(
            &mut self,
            loc: Location<[u8; LEN]>,
        ) -> Self::DynRegion<LEN> {
            self.allocate_region::<u8, LEN>(loc)
        }
    }

    // TODO: RV-157: Remove this implementation
    impl ManagerRead for TraceManager {
        fn region_read<E: Elem, const LEN: usize>(
            _region: &Self::Region<E, LEN>,
            _index: usize,
        ) -> E {
            unimplemented!()
        }

        fn region_read_all<E: Elem, const LEN: usize>(_region: &Self::Region<E, LEN>) -> Vec<E> {
            unimplemented!()
        }

        fn region_read_some<E: Elem, const LEN: usize>(
            _region: &Self::Region<E, LEN>,
            _offset: usize,
            _buffer: &mut [E],
        ) {
            unimplemented!()
        }

        fn dyn_region_read<E: Elem, const LEN: usize>(
            _region: &Self::DynRegion<LEN>,
            _address: usize,
        ) -> E {
            unimplemented!()
        }

        fn dyn_region_read_all<E: Elem, const LEN: usize>(
            _region: &Self::DynRegion<LEN>,
            _address: usize,
            _values: &mut [E],
        ) {
            unimplemented!()
        }
    }

    // TODO: RV-157: Remove this implementation
    impl ManagerWrite for TraceManager {
        fn region_write<E: Elem, const LEN: usize>(
            _region: &mut Self::Region<E, LEN>,
            _index: usize,
            _value: E,
        ) {
            unimplemented!()
        }

        fn region_write_all<E: Elem, const LEN: usize>(
            _region: &mut Self::Region<E, LEN>,
            _value: &[E],
        ) {
            unimplemented!()
        }

        fn region_write_some<E: Elem, const LEN: usize>(
            _region: &mut Self::Region<E, LEN>,
            _index: usize,
            _buffer: &[E],
        ) {
            unimplemented!()
        }

        fn dyn_region_write<E: Elem, const LEN: usize>(
            _region: &mut Self::DynRegion<LEN>,
            _address: usize,
            _value: E,
        ) {
            unimplemented!()
        }

        fn dyn_region_write_all<E: Elem, const LEN: usize>(
            _region: &mut Self::DynRegion<LEN>,
            _address: usize,
            _values: &[E],
        ) {
            unimplemented!()
        }
    }

    // TODO: RV-157: Remove this implementation
    impl ManagerReadWrite for TraceManager {
        fn region_replace<E: Elem, const LEN: usize>(
            _region: &mut Self::Region<E, LEN>,
            _index: usize,
            _value: E,
        ) -> E {
            unimplemented!()
        }
    }

    /// Run `f` twice against two different randomised backends and see if the
    /// resulting backend state is the same afterwards.
    pub fn test_determinism<F: TestBackendFactory, L: Layout, T>(f: T)
    where
        T: Fn(AllocatedOf<L, ManagerFor<'_, F, L>>),
    {
        let mut backend1 = crate::create_backend!(L, F);
        randomise_backend(&mut backend1);

        let mut backend2 = crate::create_backend!(L, F);

        // Ensure both backends start off sufficiently different.
        let mut data = read_backend(&backend1);

        let mut rng = rand::thread_rng();
        for reff in data.iter_mut() {
            let val = *reff;
            *reff = loop {
                let select: u8 = rng.gen();
                if val != select {
                    break select;
                }
            };
        }

        backend2.write(0, &data);

        // Trace the location allocation process in order to find where in the
        // backend storage we have placed regions.
        let locs = TraceManager::trace::<L>(L::placed().into_location());

        // Run the procedure against both backends.
        f(backend1.allocate(L::placed().into_location()));
        f(backend2.allocate(L::placed().into_location()));

        // Loop over the locations that have regions allocated to them.
        // If we were to compare the entire backend storage, we would also
        // include regions that the states can't write to (e.g. padding in front
        // of alignment).
        for (offset, size) in locs {
            let mut buffer1 = vec![0u8; size];
            Backend::read(&backend1, offset, &mut buffer1);

            let mut buffer2 = vec![0u8; size];
            Backend::read(&backend2, offset, &mut buffer2);

            assert_eq!(
                buffer1, buffer2,
                "Location {offset}+{size} is different: {buffer1:?} != {buffer2:?}"
            );
        }
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
        ($State:tt, $StateLayout:ty, $Factory:ty, $backend:ident, $ExtraGeneric:ty) => {
            {
                use $crate::state_backend::{Backend, BackendManagement, Layout};
                let loc = <$StateLayout>::placed().into_location();
                let new_state =
                    $State::<
                        $ExtraGeneric,
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
        struct Example<M: Manager> {
            first: Cell<u64, M>,
            second: Cells<u32, 4, M>,
        }

        type ExampleLayout = (Atom<u64>, Array<u32, 4>);

        impl<M: Manager> Example<M> {
            fn bind(space: AllocatedOf<ExampleLayout, M>) -> Self {
                Example {
                    first: space.0,
                    second: space.1,
                }
            }
        }

        let mut backend = create_backend!(ExampleLayout, F);

        let placed = ExampleLayout::placed().into_location();

        let first_offset = placed.0.offset();
        assert_eq!(first_offset, 0);

        let second_offset = placed.1.offset();
        assert_eq!(second_offset, 8);

        let first_value: u64 = rand::random();
        let second_value: [u32; 4] = rand::random();

        {
            let mut instance = Example::bind(backend.allocate(placed));

            instance.first.write(first_value);
            assert_eq!(instance.first.read(), first_value);

            instance.second.write_all(&second_value);
            assert_eq!(instance.second.read_all(), second_value);
        }

        let mut first_value_read = 0u64;
        backend.read(first_offset, unsafe {
            &mut *(&mut first_value_read as *mut u64 as *mut [u8; 8])
        });
        assert_eq!(u64::from_le(first_value_read), first_value);

        let mut second_value_read = [0u32; 4];
        backend.read(second_offset, unsafe {
            &mut *(&mut second_value_read as *mut [u32; 4] as *mut [u8; 16])
        });
        assert_eq!(second_value_read.map(u32::from_le), second_value);
    });
}
