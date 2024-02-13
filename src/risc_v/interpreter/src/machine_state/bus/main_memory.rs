// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::state_backend::{self as backend, DynRegion};
use std::mem;

use super::Addressable;

/// Configuration object for memory size
pub enum Sizes<const BYTES: usize> {}

/// Generates a variant of [Sizes] with all length parameters instantiated.
macro_rules! gen_memory_layout {
    ($name:ident = $size_in_g:literal GiB) => {
        pub type $name =
            crate::machine_state::bus::main_memory::Sizes<{ $size_in_g * 1024 * 1024 * 1024 }>;
    };

    ($name:ident = $size_in_m:literal MiB) => {
        pub type $name =
            crate::machine_state::bus::main_memory::Sizes<{ $size_in_m * 1024 * 1024 }>;
    };

    ($name:ident = $size_in_k:literal KiB) => {
        pub type $name = crate::machine_state::bus::main_memory::Sizes<{ $size_in_k * 1024 }>;
    };
}

gen_memory_layout!(M1G = 1 GiB);
gen_memory_layout!(M4G = 4 GiB);

/// Main memory layout, i.e. specifies how much memory there is
// XXX: We can't associate these region types directly with [Sizes] because
// inherent associated types are unstable. Hence we must go through a dummy
// trait.
pub trait MainMemoryLayout: backend::Layout {
    type Data<M: backend::Manager>: backend::DynRegion;

    const BYTES: usize;

    fn refl<M: backend::Manager>(space: backend::AllocatedOf<Self, M>) -> MainMemory<Self, M>;

    fn offset(loc: &backend::PlacedOf<Self>) -> usize;
}

impl<const BYTES: usize> MainMemoryLayout for Sizes<BYTES> {
    type Data<M: backend::Manager> = M::DynRegion<BYTES>;

    const BYTES: usize = BYTES;

    fn refl<M: backend::Manager>(space: backend::AllocatedOf<Self, M>) -> MainMemory<Self, M> {
        space
    }

    fn offset(loc: &backend::PlacedOf<Self>) -> usize {
        loc.offset()
    }
}

impl<const BYTES: usize> backend::Layout for Sizes<BYTES> {
    type Placed = backend::Location<[u8; BYTES]>;

    fn place_with(alloc: &mut backend::Choreographer) -> Self::Placed {
        backend::Array::<u8, BYTES>::place_with(alloc)
    }

    type Allocated<M: backend::Manager> = MainMemory<Self, M>;

    fn allocate<M: backend::Manager>(backend: &mut M, placed: Self::Placed) -> Self::Allocated<M> {
        let data = backend.allocate_dyn_region(placed);
        MainMemory { data }
    }
}

/// Main memory state for the given layout
pub struct MainMemory<L: MainMemoryLayout + ?Sized, M: backend::Manager> {
    pub data: L::Data<M>,
}

impl<L: MainMemoryLayout, M: backend::Manager> MainMemory<L, M> {
    /// Bind the main memory state to the given allocated space.
    pub fn bind(space: backend::AllocatedOf<L, M>) -> Self {
        L::refl(space)
    }

    /// Reset to the initial state.
    pub fn reset(&mut self) {
        for i in 0..L::BYTES {
            self.data.write(i, 0u8);
        }
    }
}

impl<E: backend::Elem, L: MainMemoryLayout, M: backend::Manager> Addressable<E>
    for MainMemory<L, M>
{
    #[inline(always)]
    fn read(&self, addr: super::Address) -> Result<E, super::OutOfBounds> {
        if addr as usize + mem::size_of::<E>() > L::BYTES {
            return Err(super::OutOfBounds);
        }

        Ok(self.data.read(addr as usize))
    }

    fn write(&mut self, addr: super::Address, value: E) -> Result<(), super::OutOfBounds> {
        if addr as usize + mem::size_of::<E>() > L::BYTES {
            return Err(super::OutOfBounds);
        }

        self.data.write(addr as usize, value);

        Ok(())
    }
}

#[cfg(test)]
pub mod tests {
    use crate::{
        backend_test,
        machine_state::{
            backend::{tests::test_determinism, Backend, Layout},
            bus::Addressable,
        },
    };

    gen_memory_layout!(T1K = 1 KiB);

    backend_test!(test_endianess, F, {
        let mut backend = F::new::<T1K>();
        let mut memory = backend.allocate(T1K::placed().into_location());

        memory.write(0, 0x1122334455667788u64).unwrap();

        macro_rules! check_address {
            ($ty:ty, $addr:expr, $value:expr) => {
                assert_eq!(Addressable::<$ty>::read(&memory, $addr), Ok($value));
            };
        }

        check_address!(u64, 0, 0x1122334455667788);

        check_address!(u32, 0, 0x55667788);
        check_address!(u32, 4, 0x11223344);

        check_address!(u16, 0, 0x7788);
        check_address!(u16, 2, 0x5566);
        check_address!(u16, 4, 0x3344);
        check_address!(u16, 6, 0x1122);

        check_address!(u8, 0, 0x88);
        check_address!(u8, 1, 0x77);
        check_address!(u8, 2, 0x66);
        check_address!(u8, 3, 0x55);
        check_address!(u8, 4, 0x44);
        check_address!(u8, 5, 0x33);
        check_address!(u8, 6, 0x22);
        check_address!(u8, 7, 0x11);
    });

    backend_test!(test_reset, F, {
        test_determinism::<F, T1K, _>(|mut memory| {
            memory.reset();
        });
    });
}
