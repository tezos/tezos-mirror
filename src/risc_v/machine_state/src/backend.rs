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
//! use risc_v_machine_state::backend::{Atom, Array};
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
        let mut new = source.clone();

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
    fn allocate<'backend>(
        &'backend mut self,
        placed: PlacedOf<Self::Layout>,
    ) -> AllocatedOf<Self::Layout, Self::Manager<'backend>>;

    /// Read bytes from the backing storage.
    fn read(&self, index: usize, buffer: &mut [u8]);

    /// Write bytes to the backing storage.
    fn write(&mut self, index: usize, buffer: &[u8]);
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::{bus, registers};

    /// This lets you construct backends for any layout.
    pub trait TestBackendFactory {
        type Backend<L: Layout>: Backend<Layout = L>;

        /// Construct a backend for the given layout `L`.
        fn make<L: Layout>(&mut self) -> Self::Backend<L>;
    }

    pub fn test_backend(factory: &mut impl TestBackendFactory) {
        region::tests::test_backend(factory);
        registers::tests::test_backend(factory);
        bus::main_memory::tests::test_backend(factory);
        test_example(factory);
    }

    fn test_example(factory: &mut impl TestBackendFactory) {
        struct Example<M: Manager> {
            first: Cell<u64, M>,
            second: M::Region<u32, 4>,
        }

        type ExampleLayout = (Atom<u64>, Array<u32, 4>);

        impl<M: Manager> Example<M> {
            fn new_in(space: AllocatedOf<ExampleLayout, M>) -> Self {
                Example {
                    first: space.0,
                    second: space.1,
                }
            }
        }

        let mut backend = factory.make::<ExampleLayout>();
        let placed = ExampleLayout::placed().into_location();

        let first_offset = placed.0.offset();
        assert_eq!(first_offset, 0);

        let second_offset = placed.1.offset();
        assert_eq!(second_offset, 8);

        let first_value: u64 = rand::random();
        let second_value: [u32; 4] = rand::random();

        {
            let mut instance = Example::new_in(backend.allocate(placed));

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
