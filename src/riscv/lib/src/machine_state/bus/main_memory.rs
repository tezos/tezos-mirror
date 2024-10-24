// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use super::{Address, AddressableRead, AddressableWrite};
use crate::state_backend::{
    self as backend,
    hash::{Hash, HashError, RootHashable},
    ManagerDeserialise, ManagerSerialise,
};
use serde::{Deserialize, Serialize};
use std::mem;

/// Configuration object for memory size
#[derive(Clone)]
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

gen_memory_layout!(M1K = 1 KiB);
gen_memory_layout!(M8K = 8 KiB);
gen_memory_layout!(M1M = 1 MiB);
gen_memory_layout!(M100M = 100 MiB);
gen_memory_layout!(M1G = 1 GiB);
gen_memory_layout!(M4G = 4 GiB);

/// Main memory layout, i.e. specifies how much memory there is
// XXX: We can't associate these region types directly with [Sizes] because
// inherent associated types are unstable. Hence we must go through a dummy
// trait.
pub trait MainMemoryLayout: backend::Layout {
    type Data<M: backend::ManagerBase>;

    const BYTES: usize;

    fn refl<M: backend::ManagerBase>(space: backend::AllocatedOf<Self, M>) -> MainMemory<Self, M>;

    /// Obtain a reference to the region(s) used for main memory.
    fn data_struct_ref<M: backend::ManagerBase>(
        data: &Self::Data<M>,
    ) -> backend::AllocatedOf<Self, backend::Ref<'_, M>>;

    /// Read an element in the region. `address` is in bytes.
    fn data_read<E: backend::Elem, M: backend::ManagerRead>(
        data: &Self::Data<M>,
        address: usize,
    ) -> E;

    /// Read elements from the region. `address` is in bytes.
    fn data_read_all<E: backend::Elem, M: backend::ManagerRead>(
        data: &Self::Data<M>,
        address: usize,
        values: &mut [E],
    );

    /// Update an element in the region. `address` is in bytes.
    fn data_write<E: backend::Elem, M: backend::ManagerWrite>(
        data: &mut Self::Data<M>,
        address: usize,
        value: E,
    );

    /// Update multiple elements in the region. `address` is in bytes.
    fn data_write_all<E: backend::Elem, M: backend::ManagerWrite>(
        data: &mut Self::Data<M>,
        address: usize,
        values: &[E],
    );

    /// Serialise main memory's dynamic region.
    fn serialise_data<M: ManagerSerialise, S: serde::Serializer>(
        data: &Self::Data<M>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>;

    /// Deserialise main memory's dynamic region.
    fn deserialise_data<'de, M: ManagerDeserialise, D: serde::Deserializer<'de>>(
        deserializer: D,
    ) -> Result<Self::Data<M>, D::Error>;

    /// Clone the dynamic region.
    fn clone_data<M: backend::ManagerClone>(data: &Self::Data<M>) -> Self::Data<M>;

    fn hash_data<M: ManagerSerialise>(data: &Self::Data<M>) -> Result<Hash, HashError>;
}

impl<const BYTES: usize> MainMemoryLayout for Sizes<BYTES> {
    type Data<M: backend::ManagerBase> = backend::DynCells<BYTES, M>;

    const BYTES: usize = BYTES;

    fn refl<M: backend::ManagerBase>(space: backend::AllocatedOf<Self, M>) -> MainMemory<Self, M> {
        space
    }

    fn data_struct_ref<M: backend::ManagerBase>(
        data: &Self::Data<M>,
    ) -> backend::AllocatedOf<Self, backend::Ref<'_, M>> {
        MainMemory {
            data: data.struct_ref(),
        }
    }

    fn data_read<E: backend::Elem, M: backend::ManagerRead>(
        data: &Self::Data<M>,
        address: usize,
    ) -> E {
        data.read(address)
    }

    fn data_read_all<E: backend::Elem, M: backend::ManagerRead>(
        data: &Self::Data<M>,
        address: usize,
        values: &mut [E],
    ) {
        data.read_all(address, values);
    }

    fn data_write<E: backend::Elem, M: backend::ManagerWrite>(
        data: &mut Self::Data<M>,
        address: usize,
        value: E,
    ) {
        data.write(address, value);
    }

    fn data_write_all<E: backend::Elem, M: backend::ManagerWrite>(
        data: &mut Self::Data<M>,
        address: usize,
        values: &[E],
    ) {
        data.write_all(address, values);
    }

    fn serialise_data<M: ManagerSerialise, S: serde::Serializer>(
        data: &Self::Data<M>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        data.serialize(serializer)
    }

    fn deserialise_data<'de, M: ManagerDeserialise, D: serde::Deserializer<'de>>(
        deserializer: D,
    ) -> Result<Self::Data<M>, D::Error> {
        serde::Deserialize::deserialize(deserializer)
    }

    fn clone_data<M: backend::ManagerClone>(data: &Self::Data<M>) -> Self::Data<M> {
        data.clone()
    }

    fn hash_data<M: ManagerSerialise>(data: &Self::Data<M>) -> Result<Hash, HashError> {
        data.hash()
    }
}

impl<const BYTES: usize> backend::Layout for Sizes<BYTES> {
    type Allocated<M: backend::ManagerBase> = MainMemory<Self, M>;

    fn allocate<M: backend::ManagerAlloc>(backend: &mut M) -> Self::Allocated<M> {
        let data = backend.allocate_dyn_region();
        let data = backend::DynCells::bind(data);
        MainMemory { data }
    }
}

/// Main memory state for the given layout
pub struct MainMemory<L: MainMemoryLayout + ?Sized, M: backend::ManagerBase> {
    pub data: L::Data<M>,
}

impl<L: MainMemoryLayout, M: backend::ManagerBase> MainMemory<L, M> {
    /// Bind the main memory state to the given allocated space.
    pub fn bind(space: backend::AllocatedOf<L, M>) -> Self {
        L::refl(space)
    }

    /// Obtain a structure with references to the bound regions of this type.
    pub fn struct_ref(&self) -> backend::AllocatedOf<L, backend::Ref<'_, M>> {
        L::data_struct_ref(&self.data)
    }

    /// Reset to the initial state.
    pub fn reset(&mut self)
    where
        M: backend::ManagerWrite,
    {
        for i in 0..L::BYTES {
            L::data_write(&mut self.data, i, 0u8);
        }
    }
}

impl<E: backend::Elem, L: MainMemoryLayout, M: backend::ManagerRead> AddressableRead<E>
    for MainMemory<L, M>
{
    #[inline(always)]
    fn read(&self, addr: super::Address) -> Result<E, super::OutOfBounds> {
        if addr as usize + mem::size_of::<E>() > L::BYTES {
            return Err(super::OutOfBounds);
        }

        Ok(L::data_read(&self.data, addr as usize))
    }

    #[inline(always)]
    fn read_all(&self, addr: super::Address, values: &mut [E]) -> Result<(), super::OutOfBounds> {
        if addr as usize + mem::size_of_val(values) > L::BYTES {
            return Err(super::OutOfBounds);
        }

        L::data_read_all(&self.data, addr as usize, values);

        Ok(())
    }
}

impl<E: backend::Elem, L: MainMemoryLayout, M: backend::ManagerWrite> AddressableWrite<E>
    for MainMemory<L, M>
{
    fn write(&mut self, addr: super::Address, value: E) -> Result<(), super::OutOfBounds> {
        if addr as usize + mem::size_of::<E>() > L::BYTES {
            return Err(super::OutOfBounds);
        }

        L::data_write(&mut self.data, addr as usize, value);

        Ok(())
    }

    fn write_all(&mut self, addr: Address, values: &[E]) -> Result<(), super::OutOfBounds> {
        let addr = addr as usize;

        if addr + mem::size_of_val(values) > L::BYTES {
            return Err(super::OutOfBounds);
        }

        L::data_write_all(&mut self.data, addr, values);

        Ok(())
    }
}

impl<L: MainMemoryLayout, M: ManagerSerialise> Serialize for MainMemory<L, M> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        L::serialise_data(&self.data, serializer)
    }
}

impl<'de, L, M> Deserialize<'de> for MainMemory<L, M>
where
    L: MainMemoryLayout,
    M: ManagerDeserialise,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let data = L::deserialise_data(deserializer)?;
        Ok(Self { data })
    }
}

impl<L: MainMemoryLayout, M: ManagerSerialise> RootHashable for MainMemory<L, M> {
    fn hash(&self) -> Result<Hash, HashError> {
        L::hash_data(&self.data)
    }
}

impl<L: MainMemoryLayout, M: backend::ManagerClone> Clone for MainMemory<L, M> {
    fn clone(&self) -> Self {
        Self {
            data: L::clone_data(&self.data),
        }
    }
}

impl<L: MainMemoryLayout, M: backend::ManagerRead, N: backend::ManagerRead>
    PartialEq<MainMemory<L, N>> for MainMemory<L, M>
{
    fn eq(&self, other: &MainMemory<L, N>) -> bool {
        (0..L::BYTES)
            .all(|i| L::data_read::<u8, _>(&self.data, i) == L::data_read::<u8, _>(&other.data, i))
    }
}

#[cfg(test)]
pub mod tests {
    use crate::{
        backend_test,
        machine_state::{backend::tests::test_determinism, bus::AddressableWrite},
    };

    gen_memory_layout!(T1K = 1 KiB);

    backend_test!(test_endianess, F, {
        let mut memory = F::allocate::<T1K>();

        memory.write(0, 0x1122334455667788u64).unwrap();

        macro_rules! check_address {
            ($ty:ty, $addr:expr, $value:expr) => {
                assert_eq!(
                    $crate::machine_state::bus::AddressableRead::<$ty>::read(&memory, $addr),
                    Ok($value)
                );
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

    #[test]
    fn test_reset() {
        test_determinism::<T1K, _>(|mut memory| {
            memory.reset();
        });
    }
}
