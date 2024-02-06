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
//! use risc_v_interpreter::machine_state::backend::{Atom, Array};
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

mod layout;
pub use layout::*;

mod alloc;
pub use alloc::*;

mod region;
pub use region::*;

/// Elements that may be stored using a [Backend]
pub trait Elem: Copy + 'static {
    /// Copy from `source` and convert to stored representation.
    fn store(&mut self, source: &Self);

    /// Convert to stored representation in place.
    fn to_stored_in_place(&mut self);

    /// Convert from stored representation in place.
    // The naming of this function trips Clippy.
    #[allow(clippy::wrong_self_convention)]
    fn from_stored_in_place(&mut self);

    /// Read a value from its stored representation.
    fn from_stored(source: &Self) -> Self;
}

macro_rules! impl_elem_prim {
    ( $x:ty ) => {
        impl Elem for $x {
            #[inline(always)]
            fn store(&mut self, source: &Self) {
                *self = source.to_le();
            }

            #[inline(always)]
            fn to_stored_in_place(&mut self) {
                *self = self.to_le();
            }

            #[inline(always)]
            fn from_stored_in_place(&mut self) {
                *self = Self::from_le(*self);
            }

            #[inline(always)]
            fn from_stored(source: &Self) -> Self {
                Self::from_le(*source)
            }
        }
    };
}

impl_elem_prim!(u8);
impl_elem_prim!(i8);
impl_elem_prim!(u16);
impl_elem_prim!(i16);
impl_elem_prim!(u32);
impl_elem_prim!(i32);
impl_elem_prim!(u64);
impl_elem_prim!(i64);
impl_elem_prim!(u128);
impl_elem_prim!(i128);

impl<E: Elem, const LEN: usize> Elem for [E; LEN] {
    #[inline(always)]
    fn store(&mut self, source: &Self) {
        self.copy_from_slice(source);

        // NOTE: This loop may be eliminated if [to_stored_in_place] is a no-op.
        for elem in self {
            elem.to_stored_in_place();
        }
    }

    #[inline(always)]
    fn to_stored_in_place(&mut self) {
        // NOTE: This loop may be eliminated if [to_stored_in_place] is a no-op.
        for elem in self {
            elem.to_stored_in_place();
        }
    }

    #[inline(always)]
    fn from_stored_in_place(&mut self) {
        // NOTE: This loop may be eliminated if [from_stored_in_place] is a no-op.
        for elem in self {
            elem.from_stored_in_place();
        }
    }

    #[inline(always)]
    fn from_stored(source: &Self) -> Self {
        let mut new = *source;

        // NOTE: This loop may be eliminated if [from_stored_in_place] is a no-op.
        for elem in new.iter_mut() {
            elem.from_stored_in_place();
        }

        new
    }
}

/// Manager of the state backend storage
pub trait Manager {
    /// Region that has been allocated in the state storage
    type Region<E: Elem, const LEN: usize>: Region<E>;

    /// Allocate a region in the state storage.
    fn allocate_region<E: Elem, const LEN: usize>(
        &mut self,
        loc: Location<[E; LEN]>,
    ) -> Self::Region<E, LEN>;

    /// Like [`Self::Region`] but all element accesses are "volatile"
    type VolatileRegion<E: Elem, const LEN: usize>: VolatileRegion<E>;

    /// Allocate a volatile region in the state storage.
    fn allocate_volatile_region<E: Elem, const LEN: usize>(
        &mut self,
        loc: Volatile<Location<[E; LEN]>>,
    ) -> Self::VolatileRegion<E, LEN>;

    /// Allocate a cell in the state storage.
    #[inline]
    fn allocate_cell<E: Elem>(&mut self, loc: Location<E>) -> Cell<E, Self> {
        Cell {
            region: self.allocate_region(loc.as_array()),
        }
    }
}

/// State backend with manager
pub trait BackendManagement {
    /// Backend manager
    type Manager<'backend>: Manager;
}

/// State backend storage
// XXX: Rust doesn't like [BackendManagement::Manager] in this trait therefore
// it has been moved to the super trait. This seems to be related to the
// lifetime parameter.
pub trait Backend: BackendManagement + Sized {
    /// Structural representation of the states that this backend supports
    type Layout: layout::Layout;

    /// Allocate regions for the given layout placement.
    fn allocate(
        &mut self,
        placed: PlacedOf<Self::Layout>,
    ) -> AllocatedOf<Self::Layout, Self::Manager<'_>>;

    /// Read bytes from the backing storage.
    fn read(&self, index: usize, buffer: &mut [u8]);

    /// Write bytes to the backing storage.
    fn write(&mut self, index: usize, buffer: &[u8]);
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::machine_state::{bus, interpreter, mode, registers};
    use rand::{Fill, Rng};
    use std::collections::VecDeque;

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
    struct DummyRegion;

    impl<E: Elem> Region<E> for DummyRegion {
        fn read(&self, _index: usize) -> E {
            unimplemented!()
        }

        fn read_all(&self) -> Vec<E> {
            unimplemented!()
        }

        fn read_some(&self, _offset: usize, _buffer: &mut [E]) {
            unimplemented!()
        }

        fn write(&mut self, _index: usize, _value: E) {
            unimplemented!()
        }

        fn write_all(&mut self, _value: &[E]) {
            unimplemented!()
        }

        fn write_some(&mut self, _index: usize, _buffer: &[E]) {
            unimplemented!()
        }

        fn replace(&mut self, _index: usize, _value: E) -> E {
            unimplemented!()
        }
    }

    impl<E: Elem> VolatileRegion<E> for DummyRegion {
        fn read(&self, _index: usize) -> E {
            unimplemented!()
        }

        fn write(&mut self, _index: usize, _value: E) {
            unimplemented!()
        }
    }

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

    impl Manager for TraceManager {
        type Region<E: Elem, const LEN: usize> = DummyRegion;

        fn allocate_region<E: Elem, const LEN: usize>(
            &mut self,
            loc: Location<[E; LEN]>,
        ) -> Self::Region<E, LEN> {
            self.regions.push_back((loc.offset(), loc.size()));
            DummyRegion
        }

        type VolatileRegion<E: Elem, const LEN: usize> = DummyRegion;

        fn allocate_volatile_region<E: Elem, const LEN: usize>(
            &mut self,
            loc: Volatile<Location<[E; LEN]>>,
        ) -> Self::VolatileRegion<E, LEN> {
            self.regions.push_back((loc.offset(), loc.size()));
            DummyRegion
        }

        fn allocate_cell<E: Elem>(&mut self, loc: Location<E>) -> Cell<E, Self> {
            self.regions.push_back((loc.offset(), loc.size()));
            Cell {
                region: DummyRegion,
            }
        }
    }

    /// Run `f` twice against two different randomised backends and see if the
    /// resulting backend state is the same afterwards.
    pub fn test_determinism<F: TestBackendFactory, L: Layout, T>(f: T)
    where
        T: Fn(AllocatedOf<L, ManagerFor<'_, F, L>>),
    {
        let mut backend1 = F::new::<L>();
        randomise_backend(&mut backend1);

        let mut backend2 = F::new::<L>();

        // Ensure both backends start off sufficiently different.
        let data1 = read_backend(&backend1);
        let mut data2 = vec![0; data1.len()];

        let mut rng = rand::thread_rng();
        for (offset, val) in data1.iter().copied().enumerate() {
            data2[offset] = loop {
                let select: u8 = rng.gen();
                if val != select {
                    break select;
                }
            };
        }

        backend2.write(0, &data2);

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
            backend1.read(offset, &mut buffer1);

            let mut buffer2 = vec![0u8; size];
            backend2.read(offset, &mut buffer2);

            assert_eq!(
                buffer1, buffer2,
                "Location {offset}+{size} is different: {buffer1:?} != {buffer2:?}"
            );
        }
    }

    /// This lets you construct backends for any layout.
    pub trait TestBackendFactory {
        type Backend<L: Layout>: Backend<Layout = L>;

        /// Construct a backend for the given layout `L`.
        fn new<L: Layout>() -> Self::Backend<L>;
    }

    /// Given a `StateLayout` and a [`TestBackendFactory`] type,
    /// create the backend for that layout.
    #[macro_export]
    macro_rules! create_backend {
        ($StateLayout:ty, $Factory:ty) => {
            <$Factory>::new::<$StateLayout>()
        };
    }

    /// Given a `State<M: Manager>`, optionally its `StateLayout`,
    /// a [`TestBackendFactory`] type, a `backend` created with `create_backend!` macro,
    /// create the location and return the created `State<M>`.
    #[macro_export]
    macro_rules! create_state {
        ($State:tt, $StateLayout:ty, $Factory:ty, $backend:ident) => {
            {
                use $crate::machine_state::backend::{Backend, BackendManagement, Layout};
                let loc = <$StateLayout>::placed().into_location();
                let new_state =
                    $State::<<<$Factory>::Backend<$StateLayout> as BackendManagement>::Manager<'_>>::bind(
                        $backend.allocate(loc),
                    );

                new_state
            }
        };

        ($State:tt, $Factory:ty, $backend:ident) => {
            create_state!($State, paste::paste!([<$State Layout>]), $Factory, $backend)
        };
    }

    pub fn test_backend<F: TestBackendFactory>() {
        region::tests::test_backend::<F>();
        registers::tests::test_backend::<F>();
        bus::tests::test_backend::<F>();
        mode::tests::test_mode::<F>();
        interpreter::tests::test::<F>();
        test_example::<F>();
        crate::machine_state::tests::test_backend::<F>();
    }

    fn test_example<F: TestBackendFactory>() {
        struct Example<M: Manager> {
            first: Cell<u64, M>,
            second: M::Region<u32, 4>,
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

        let mut backend = F::new::<ExampleLayout>();
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
    }
}
