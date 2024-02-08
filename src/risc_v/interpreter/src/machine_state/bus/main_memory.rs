// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::machine_state::backend::{self, VolatileRegion};
use std::mem;

/// Configuration object for memory size
pub enum Sizes<const LEN8: usize, const LEN16: usize, const LEN32: usize, const LEN64: usize> {}

/// Generates a variant of [Sizes] with all length parameters instantiated.
macro_rules! gen_memory_layout {
    ($name:ident = $size_in_g:literal GiB) => {
        pub type $name = crate::machine_state::bus::main_memory::Sizes<
            { $size_in_g * 1024 * 1024 * 1024 },
            { $size_in_g * 1024 * 1024 * 512 },
            { $size_in_g * 1024 * 1024 * 256 },
            { $size_in_g * 1024 * 1024 * 128 },
        >;
    };

    ($name:ident = $size_in_m:literal MiB) => {
        pub type $name = crate::machine_state::bus::main_memory::Sizes<
            { $size_in_m * 1024 * 1024 },
            { $size_in_m * 1024 * 512 },
            { $size_in_m * 1024 * 256 },
            { $size_in_m * 1024 * 128 },
        >;
    };

    ($name:ident = $size_in_k:literal KiB) => {
        pub type $name = crate::machine_state::bus::main_memory::Sizes<
            { $size_in_k * 1024 },
            { $size_in_k * 512 },
            { $size_in_k * 256 },
            { $size_in_k * 128 },
        >;
    };
}

gen_memory_layout!(M1G = 1 GiB);
gen_memory_layout!(M4G = 4 GiB);

/// Main memory layout, i.e. specifies how much memory there is
// XXX: We can't associate these region types directly with [Sizes] because
// inherent associated types are unstable. Hence we must go through a dummy
// trait.
pub trait MainMemoryLayout: backend::Layout {
    type Region8<M: backend::Manager>: backend::VolatileRegion<Elem = u8>;
    type Region16<M: backend::Manager>: backend::VolatileRegion<Elem = u16>;
    type Region32<M: backend::Manager>: backend::VolatileRegion<Elem = u32>;
    type Region64<M: backend::Manager>: backend::VolatileRegion<Elem = u64>;

    const LEN8: usize;
    const LEN16: usize;
    const LEN32: usize;
    const LEN64: usize;

    fn refl<M: backend::Manager>(space: backend::AllocatedOf<Self, M>) -> MainMemory<Self, M>;

    fn offset(loc: &backend::PlacedOf<Self>) -> usize;
}

impl<const LEN8: usize, const LEN16: usize, const LEN32: usize, const LEN64: usize> MainMemoryLayout
    for Sizes<LEN8, LEN16, LEN32, LEN64>
{
    type Region8<M: backend::Manager> = M::VolatileRegion<u8, LEN8>;
    type Region16<M: backend::Manager> = M::VolatileRegion<u16, LEN16>;
    type Region32<M: backend::Manager> = M::VolatileRegion<u32, LEN32>;
    type Region64<M: backend::Manager> = M::VolatileRegion<u64, LEN64>;

    const LEN8: usize = LEN8;
    const LEN16: usize = LEN16;
    const LEN32: usize = LEN32;
    const LEN64: usize = LEN64;

    fn refl<M: backend::Manager>(space: backend::AllocatedOf<Self, M>) -> MainMemory<Self, M> {
        space
    }

    fn offset(loc: &backend::PlacedOf<Self>) -> usize {
        loc.0.offset()
    }
}

impl<const LEN8: usize, const LEN16: usize, const LEN32: usize, const LEN64: usize> backend::Layout
    for Sizes<LEN8, LEN16, LEN32, LEN64>
{
    type Placed = (
        backend::VolatileLocation<[u8; LEN8]>,
        backend::VolatileLocation<[u16; LEN16]>,
        backend::VolatileLocation<[u32; LEN32]>,
        backend::VolatileLocation<[u64; LEN64]>,
    );

    fn place_with(alloc: &mut backend::Choreographer) -> Self::Placed {
        let (loc1, loc_rest) = alloc.overlapping(backend::Array::<u8, LEN8>::place_with, |alloc| {
            alloc.overlapping(backend::Array::<u16, LEN16>::place_with, |alloc| {
                alloc.overlapping(
                    backend::Array::<u32, LEN32>::place_with,
                    backend::Array::<u64, LEN64>::place_with,
                )
            })
        });
        let (loc2, loc_rest) = loc_rest.split();
        let (loc4, loc8) = loc_rest.split();

        (loc1, loc2, loc4, loc8)
    }

    type Allocated<M: backend::Manager> = MainMemory<Self, M>;

    fn allocate<M: backend::Manager>(backend: &mut M, placed: Self::Placed) -> Self::Allocated<M> {
        MainMemory {
            bytes: backend.allocate_volatile_region(placed.0),
            halfwords: backend.allocate_volatile_region(placed.1),
            words: backend.allocate_volatile_region(placed.2),
            doublewords: backend.allocate_volatile_region(placed.3),
        }
    }
}

/// Main memory state for the given layout
pub struct MainMemory<L: MainMemoryLayout + ?Sized, M: backend::Manager> {
    pub bytes: L::Region8<M>,
    pub halfwords: L::Region16<M>,
    pub words: L::Region32<M>,
    pub doublewords: L::Region64<M>,
}

impl<L: MainMemoryLayout, M: backend::Manager> MainMemory<L, M> {
    /// Bind the main memory state to the given allocated space.
    pub fn bind(space: backend::AllocatedOf<L, M>) -> Self {
        L::refl(space)
    }

    /// Reset to the initial state.
    pub fn reset(&mut self) {
        for i in 0..L::LEN64 {
            self.doublewords.write(i, 0u64);
        }
    }
}

macro_rules! impl_volatile_region {
    ($field:ident, $int:ty) => {
        impl<L: MainMemoryLayout, M: backend::Manager> super::Addressable<$int>
            for MainMemory<L, M>
        {
            #[inline(always)]
            fn read(&self, addr: super::Address) -> Result<$int, super::OutOfBounds> {
                if addr as usize >= L::LEN8 {
                    return Err(super::OutOfBounds);
                }

                // TODO: Check alignment
                let index = addr as usize / mem::size_of::<$int>();
                Ok(backend::VolatileRegion::read(&self.$field, index))
            }

            #[inline(always)]
            fn write(
                &mut self,
                addr: super::Address,
                value: $int,
            ) -> Result<(), super::OutOfBounds> {
                if addr as usize >= L::LEN8 {
                    return Err(super::OutOfBounds);
                }

                // TODO: Check alignment
                let index = addr as usize / mem::size_of::<$int>();
                backend::VolatileRegion::write(&mut self.$field, index, value);

                Ok(())
            }
        }
    };
}

impl_volatile_region!(bytes, u8);
impl_volatile_region!(halfwords, u16);
impl_volatile_region!(words, u32);
impl_volatile_region!(doublewords, u64);

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
